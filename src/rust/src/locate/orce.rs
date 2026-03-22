//! Warehouse Location Problem (ORCE)
//!
//! Minimize transport + facility opening + worker costs.
//! Worker count per facility is an integer decision variable.
//! Each demand point assigned to exactly one facility (no splitting).

use extendr_api::prelude::*;
use highs::{HighsModelStatus, Sense, RowProblem, Col};

/// Solve the ORCE warehouse location problem
///
/// Decision variables:
///   x[i,j] binary  — 1 if demand i assigned to facility j
///   y[j]   binary  — 1 if facility j is open
///   w[j]   integer — number of workers at facility j
///
/// Objective (minimize):
///   Σ_ij cost[i,j] * x[i,j]  +  Σ_j fixed_cost[j] * y[j]  +  Σ_j worker_cost * w[j]
///
/// Constraints:
///   1. Σ_j x[i,j] = 1                                  (assignment)
///   2. x[i,j] ≤ y[j]                                    (linking)
///   3. Σ_i weight[i] * x[i,j] ≤ worker_capacity * w[j] (capacity)
///   4. min_workers * y[j] ≤ w[j]                         (min workers)
///   5. w[j] ≤ max_workers[j] * y[j]                      (max workers)
pub fn solve(
    cost_matrix: RMatrix<f64>,
    weights: &[f64],
    facility_costs: &[f64],
    worker_cost: f64,
    worker_capacity: f64,
    min_workers: i32,
    max_workers: &[i32],
) -> List {
    let n_demand = cost_matrix.nrows();
    let n_fac = cost_matrix.ncols();

    // Validate dimensions
    if weights.len() != n_demand {
        return list!(error = "weights length must equal number of demand points");
    }
    if facility_costs.len() != n_fac {
        return list!(error = "facility_costs length must equal number of facilities");
    }
    if max_workers.len() != n_fac {
        return list!(error = "max_workers length must equal number of facilities");
    }

    let mut pb = RowProblem::new();

    // --- Variables ---

    // y[j] — binary: facility j open (objective: fixed_cost[j])
    let y_cols: Vec<Col> = (0..n_fac)
        .map(|j| pb.add_integer_column(facility_costs[j], 0.0..=1.0))
        .collect();

    // w[j] — integer: workers at facility j (objective: worker_cost)
    let w_cols: Vec<Col> = (0..n_fac)
        .map(|j| pb.add_integer_column(worker_cost, 0.0..=(max_workers[j] as f64)))
        .collect();

    // x[i][j] — binary: demand i assigned to facility j (objective: cost[i,j])
    // Forward index: demand -> [(facility_idx, Col)]
    let mut x_cols: Vec<Vec<(usize, Col)>> = Vec::with_capacity(n_demand);
    // Reverse index: facility -> [(demand_idx, Col)]
    let mut fac_to_demands: Vec<Vec<(usize, Col)>> = vec![Vec::new(); n_fac];

    for i in 0..n_demand {
        let row_cols: Vec<(usize, Col)> = (0..n_fac)
            .map(|j| {
                let col = pb.add_integer_column(cost_matrix[[i, j]], 0.0..=1.0);
                fac_to_demands[j].push((i, col));
                (j, col)
            })
            .collect();
        x_cols.push(row_cols);
    }

    // --- Constraints ---

    // 1. Assignment: Σ_j x[i,j] = 1 for all i
    for i in 0..n_demand {
        let terms: Vec<(Col, f64)> = x_cols[i].iter().map(|&(_, c)| (c, 1.0)).collect();
        pb.add_row(1.0..=1.0, terms);
    }

    // 2. Linking: x[i,j] ≤ y[j] for all i,j
    for i in 0..n_demand {
        for &(j, x_col) in &x_cols[i] {
            let terms = vec![(x_col, 1.0), (y_cols[j], -1.0)];
            pb.add_row(..=0.0, terms);
        }
    }

    // 3. Capacity: Σ_i weight[i] * x[i,j] ≤ worker_capacity * w[j] for all j
    //    Rewritten: Σ_i weight[i] * x[i,j] - worker_capacity * w[j] ≤ 0
    for j in 0..n_fac {
        let mut terms: Vec<(Col, f64)> = fac_to_demands[j]
            .iter()
            .map(|&(i, col)| (col, weights[i]))
            .collect();
        terms.push((w_cols[j], -worker_capacity));
        pb.add_row(..=0.0, terms);
    }

    // 4. Min workers: min_workers * y[j] ≤ w[j] for all j
    //    Rewritten: min_workers * y[j] - w[j] ≤ 0
    let min_w = min_workers as f64;
    for j in 0..n_fac {
        let terms = vec![(y_cols[j], min_w), (w_cols[j], -1.0)];
        pb.add_row(..=0.0, terms);
    }

    // 5. Max workers: w[j] ≤ max_workers[j] * y[j] for all j
    //    Rewritten: w[j] - max_workers[j] * y[j] ≤ 0
    for j in 0..n_fac {
        let terms = vec![(w_cols[j], 1.0), (y_cols[j], -(max_workers[j] as f64))];
        pb.add_row(..=0.0, terms);
    }

    // --- Solve ---

    let solved = pb.optimise(Sense::Minimise).solve();
    let status = solved.status();
    let status_str = format!("{:?}", status);

    match status {
        HighsModelStatus::Optimal | HighsModelStatus::ModelEmpty => {
            let sol = solved.get_solution();

            // Selected facilities (1-based for R)
            let selected: Vec<i32> = y_cols
                .iter()
                .enumerate()
                .filter(|(_, &c)| sol[c] > 0.5)
                .map(|(j, _)| (j + 1) as i32)
                .collect();

            // Assignments (1-based)
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

            // Workers per facility
            let workers: Vec<i32> = w_cols
                .iter()
                .map(|&c| sol[c].round() as i32)
                .collect();

            // Cost decomposition
            let transport_cost: f64 = (0..n_demand)
                .map(|i| {
                    x_cols[i]
                        .iter()
                        .map(|&(j, col)| sol[col] * cost_matrix[[i, j]])
                        .sum::<f64>()
                })
                .sum();

            let facility_cost_total: f64 = y_cols
                .iter()
                .enumerate()
                .map(|(j, &c)| sol[c] * facility_costs[j])
                .sum();

            let worker_cost_total: f64 = w_cols
                .iter()
                .map(|&c| sol[c] * worker_cost)
                .sum();

            // Utilization per facility
            let mut utilizations: Vec<f64> = vec![0.0; n_fac];
            for j in 0..n_fac {
                let w = workers[j];
                if w > 0 {
                    let assigned_weight: f64 = fac_to_demands[j]
                        .iter()
                        .map(|&(i, col)| weights[i] * sol[col])
                        .sum();
                    utilizations[j] = assigned_weight / (w as f64 * worker_capacity);
                }
            }

            // N assigned per facility
            let mut n_assigned: Vec<i32> = vec![0; n_fac];
            for &a in &assignments {
                n_assigned[(a - 1) as usize] += 1;
            }

            let n_selected = selected.len() as i32;

            list!(
                selected = selected,
                assignments = assignments,
                workers = workers,
                n_assigned = n_assigned,
                n_selected = n_selected,
                objective = solved.objective_value(),
                transport_cost = transport_cost,
                facility_cost = facility_cost_total,
                worker_cost_total = worker_cost_total,
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
