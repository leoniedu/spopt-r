//! Capacitated Facility Location Problem (CFLP)
//!
//! Minimize total weighted distance subject to facility capacity constraints.
//! Supports either fixed number of facilities (p) or facility opening costs.
//! Supports an optional maximum distance constraint.

use extendr_api::prelude::*;
use highs::{HighsModelStatus, Sense, RowProblem, Col};

/// Solve Capacitated Facility Location Problem
///
/// Two modes:
/// 1. If n_facilities > 0: Select exactly n_facilities (capacitated p-median)
/// 2. If n_facilities = 0 and facility_costs provided: Minimize total cost including fixed costs
pub fn solve(
    cost_matrix: RMatrix<f64>,
    weights: &[f64],
    capacities: &[f64],
    n_facilities: usize,
    facility_costs: Option<&[f64]>,
    max_distance: Option<f64>,
) -> List {
    let n_demand = cost_matrix.nrows();
    let n_fac = cost_matrix.ncols();

    // Validate inputs
    if capacities.len() != n_fac {
        return list!(
            error = "capacities length must equal number of facilities"
        );
    }

    // Check total capacity is sufficient
    let total_demand: f64 = weights.iter().sum();
    let total_capacity: f64 = capacities.iter().sum();

    if total_capacity < total_demand {
        return list!(
            error = format!(
                "Total capacity ({:.2}) is less than total demand ({:.2}). Problem is infeasible.",
                total_capacity, total_demand
            )
        );
    }

    // Compute reachable facilities for each demand point
    let reachable: Vec<Vec<usize>> = (0..n_demand)
        .map(|i| {
            (0..n_fac)
                .filter(|&j| max_distance.map_or(true, |d| cost_matrix[[i, j]] <= d))
                .collect()
        })
        .collect();

    // Create row-based problem
    let mut pb = RowProblem::new();

    // Variables:
    // y[j] = 1 if facility j is selected (binary)
    let y_cols: Vec<Col> = (0..n_fac)
        .map(|j| {
            let obj_coeff = facility_costs.map_or(0.0, |c| c[j]);
            pb.add_integer_column(obj_coeff, 0.0..=1.0)
        })
        .collect();

    // x[i][j] = fraction of demand i served by facility j (continuous, only for reachable pairs)
    // Forward index: demand -> [(facility_idx, Col)]
    let mut x_cols: Vec<Vec<(usize, Col)>> = Vec::with_capacity(n_demand);
    // Reverse index: facility -> [(demand_idx, Col)] for efficient per-facility iteration
    let mut fac_to_demands: Vec<Vec<(usize, Col)>> = vec![Vec::new(); n_fac];

    for i in 0..n_demand {
        let row_cols: Vec<(usize, Col)> = reachable[i].iter()
            .map(|&j| {
                let obj_coeff = weights[i] * cost_matrix[[i, j]];
                let col = pb.add_column(obj_coeff, 0.0..=1.0);
                fac_to_demands[j].push((i, col));
                (j, col)
            })
            .collect();
        x_cols.push(row_cols);
    }

    // Constraint 1: If n_facilities specified: sum_j y[j] = p
    if n_facilities > 0 {
        let terms: Vec<(Col, f64)> = y_cols.iter().map(|&c| (c, 1.0)).collect();
        pb.add_row(n_facilities as f64..=n_facilities as f64, terms);
    }

    // Constraint 2: sum_j x[i][j] = 1 for all i (each demand fully served)
    // R validates that every demand has at least one reachable facility
    for i in 0..n_demand {
        let terms: Vec<(Col, f64)> = x_cols[i].iter().map(|&(_, c)| (c, 1.0)).collect();
        pb.add_row(1.0..=1.0, terms);
    }

    // Constraint 3: x[i][j] <= y[j] for all reachable (i,j)
    for i in 0..n_demand {
        for &(j, x_col) in &x_cols[i] {
            let terms = vec![(x_col, 1.0), (y_cols[j], -1.0)];
            pb.add_row(..=0.0, terms);
        }
    }

    // Constraint 4: CAPACITY: sum_i (weight[i] * x[i][j]) <= capacity[j] * y[j] for all j
    // Uses reverse index for O(n_demand) per facility instead of scanning all pairs
    for j in 0..n_fac {
        let mut terms: Vec<(Col, f64)> = fac_to_demands[j].iter()
            .map(|&(i, col)| (col, weights[i]))
            .collect();
        terms.push((y_cols[j], -capacities[j]));
        pb.add_row(..=0.0, terms);
    }

    // Solve
    let solved = pb.optimise(Sense::Minimise).solve();
    let status = solved.status();
    let status_str = format!("{:?}", status);

    match status {
        HighsModelStatus::Optimal | HighsModelStatus::ModelEmpty => {
            let sol = solved.get_solution();

            // Extract selected facilities (1-based for R)
            let selected: Vec<i32> = y_cols
                .iter()
                .enumerate()
                .filter(|(_, &c)| sol[c] > 0.5)
                .map(|(i, _)| (i + 1) as i32)
                .collect();

            // Extract assignments - return primary assignment (facility serving most of demand i)
            let assignments: Vec<i32> = (0..n_demand)
                .map(|i| {
                    let mut best_j = 0usize;
                    let mut best_val = 0.0;
                    for &(j, col) in &x_cols[i] {
                        let val = sol[col];
                        if val > best_val {
                            best_val = val;
                            best_j = j;
                        }
                    }
                    (best_j + 1) as i32
                })
                .collect();

            // Extract allocation fractions (full n_demand x n_fac matrix, 0.0 for unreachable)
            // Build from reverse index for efficiency
            let mut allocation_fractions: Vec<f64> = vec![0.0; n_demand * n_fac];
            for j in 0..n_fac {
                for &(i, col) in &fac_to_demands[j] {
                    allocation_fractions[i * n_fac + j] = sol[col];
                }
            }

            // Check if any demand is split (allocation < 1 to primary)
            let n_split: i32 = (0..n_demand)
                .filter(|&i| {
                    let assigned_j = (assignments[i] - 1) as usize;
                    allocation_fractions[i * n_fac + assigned_j] < 0.999
                })
                .count() as i32;

            // Compute utilization of each facility using reverse index
            let mut utilizations: Vec<f64> = vec![0.0; n_fac];
            for j in 0..n_fac {
                if sol[y_cols[j]] > 0.5 {
                    let total_assigned: f64 = fac_to_demands[j].iter()
                        .map(|&(i, col)| weights[i] * sol[col])
                        .sum();
                    utilizations[j] = total_assigned / capacities[j];
                }
            }

            // Compute mean distance (weighted)
            let total_weighted_dist: f64 = (0..n_demand)
                .map(|i| {
                    let dist: f64 = x_cols[i].iter()
                        .map(|&(j, col)| sol[col] * cost_matrix[[i, j]])
                        .sum();
                    weights[i] * dist
                })
                .sum();
            let total_weight: f64 = weights.iter().sum();
            let mean_dist = total_weighted_dist / total_weight;

            let n_selected = selected.len() as i32;

            list!(
                selected = selected,
                assignments = assignments,
                allocation_matrix = allocation_fractions,
                n_selected = n_selected,
                n_split_demand = n_split,
                objective = solved.objective_value(),
                mean_distance = mean_dist,
                utilizations = utilizations,
                status = status_str
            )
        }
        _ => {
            list!(
                error = format!("Solver returned non-optimal status: {}", status_str),
                status = status_str
            )
        }
    }
}
