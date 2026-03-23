//! Cheapest insertion cost computation for iterative location-routing.
//!
//! Given a square distance matrix over (demand points + facility depots),
//! current facility assignments, and TSP tours per facility, compute the
//! cheapest insertion cost for every (demand, facility) pair.

use super::tsp;

/// Compute cheapest insertion costs for all (demand, facility) pairs.
///
/// # Arguments
/// * `full_matrix` — (n_demand + n_fac) × (n_demand + n_fac) distance matrix.
///   Rows/cols 0..n_demand are demand points; n_demand..n_demand+n_fac are depots.
/// * `assignments` — length n_demand, 0-based facility index for each demand point.
/// * `n_demand` — number of demand points.
/// * `n_fac` — number of facilities.
///
/// # Returns
/// n_demand × n_fac matrix of cheapest insertion costs (row-major).
pub fn compute_insertion_costs(
    full_matrix: &[Vec<f64>],
    assignments: &[usize],
    n_demand: usize,
    n_fac: usize,
) -> Vec<Vec<f64>> {
    let n_total = n_demand + n_fac;
    debug_assert_eq!(full_matrix.len(), n_total);

    // Build per-facility demand lists
    let mut fac_demands: Vec<Vec<usize>> = vec![Vec::new(); n_fac];
    for (i, &j) in assignments.iter().enumerate() {
        fac_demands[j].push(i);
    }

    // Solve TSP per facility and store tours (in full-matrix index space)
    let tours: Vec<Vec<usize>> = (0..n_fac)
        .map(|j| {
            let depot = n_demand + j;
            let assigned = &fac_demands[j];

            if assigned.is_empty() {
                return vec![depot];
            }

            if assigned.len() == 1 {
                return vec![depot, assigned[0], depot];
            }

            // Build sub-matrix for depot + assigned demands
            let mut nodes: Vec<usize> = Vec::with_capacity(assigned.len() + 1);
            nodes.push(depot);
            nodes.extend_from_slice(assigned);

            let sub_n = nodes.len();
            let mut sub_matrix = vec![vec![0.0; sub_n]; sub_n];
            for a in 0..sub_n {
                for b in 0..sub_n {
                    sub_matrix[a][b] = full_matrix[nodes[a]][nodes[b]];
                }
            }

            // Solve TSP: closed tour starting/ending at depot (index 0 in sub-matrix)
            let mut tour = tsp::nearest_neighbor(sub_n, 0, 0, &sub_matrix);

            let symmetric = tsp::is_symmetric(&sub_matrix);
            loop {
                let mut any = false;
                if tsp::two_opt_pass(&mut tour, &sub_matrix, symmetric) {
                    any = true;
                }
                if tsp::or_opt_pass(&mut tour, &sub_matrix) {
                    any = true;
                }
                if !any {
                    break;
                }
            }

            // Map back to full-matrix indices
            tour.iter().map(|&idx| nodes[idx]).collect()
        })
        .collect();

    // Compute insertion costs
    let mut costs = vec![vec![0.0; n_fac]; n_demand];

    for i in 0..n_demand {
        for j in 0..n_fac {
            costs[i][j] = cheapest_insertion_cost(i, &tours[j], &fac_demands[j], full_matrix);
        }
    }

    costs
}

/// Compute the cheapest insertion cost of demand point `i` into facility `j`'s tour.
///
/// If `i` is already in the tour (i.e., assigned to `j`), we compute insertion
/// cost as if `i` were removed first — this ensures consistent penalties across
/// all (i, j) pairs.
fn cheapest_insertion_cost(
    demand_idx: usize,
    tour: &[usize],
    assigned_demands: &[usize],
    matrix: &[Vec<f64>],
) -> f64 {
    // Tour with just the depot — insertion cost is out-and-back
    if tour.len() <= 1 {
        let depot = tour[0];
        return matrix[depot][demand_idx] + matrix[demand_idx][depot];
    }

    let is_assigned = assigned_demands.contains(&demand_idx);

    if is_assigned {
        // Remove demand_idx from tour, then compute insertion cost into shortened tour
        let short_tour: Vec<usize> = tour.iter().copied().filter(|&n| n != demand_idx).collect();

        if short_tour.len() <= 1 {
            // Was the only demand — insertion cost is out-and-back from depot
            let depot = short_tour[0];
            return matrix[depot][demand_idx] + matrix[demand_idx][depot];
        }

        min_insertion_delta(demand_idx, &short_tour, matrix)
    } else {
        min_insertion_delta(demand_idx, tour, matrix)
    }
}

/// Find the minimum cost of inserting `node` into any edge of `tour`.
fn min_insertion_delta(node: usize, tour: &[usize], matrix: &[Vec<f64>]) -> f64 {
    let mut best = f64::MAX;

    for k in 0..tour.len() - 1 {
        let a = tour[k];
        let b = tour[k + 1];
        let delta = matrix[a][node] + matrix[node][b] - matrix[a][b];
        if delta < best {
            best = delta;
        }
    }

    best
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Build a symmetric distance matrix from coordinates.
    fn euclidean_matrix(coords: &[(f64, f64)]) -> Vec<Vec<f64>> {
        let n = coords.len();
        let mut m = vec![vec![0.0; n]; n];
        for i in 0..n {
            for j in 0..n {
                let dx = coords[i].0 - coords[j].0;
                let dy = coords[i].1 - coords[j].1;
                m[i][j] = (dx * dx + dy * dy).sqrt();
            }
        }
        m
    }

    #[test]
    fn empty_facility_returns_out_and_back() {
        // 1 demand point, 2 facilities. Demand assigned to fac 0, fac 1 is empty.
        // coords: demand0=(0,0), depot0=(1,0), depot1=(5,0)
        let coords = vec![(0.0, 0.0), (1.0, 0.0), (5.0, 0.0)];
        let matrix = euclidean_matrix(&coords);

        let assignments = vec![0]; // demand 0 -> fac 0
        let costs = compute_insertion_costs(&matrix, &assignments, 1, 2);

        // Insertion into empty fac 1: d(depot1, demand0) + d(demand0, depot1) = 5 + 5 = 10
        assert!((costs[0][1] - 10.0).abs() < 1e-6);
    }

    #[test]
    fn single_demand_facility() {
        // 2 demand points, 1 facility. Both assigned to fac 0.
        // coords: d0=(0,0), d1=(2,0), depot=(1,0)
        let coords = vec![(0.0, 0.0), (2.0, 0.0), (1.0, 0.0)];
        let matrix = euclidean_matrix(&coords);

        let assignments = vec![0, 0]; // both -> fac 0
        let costs = compute_insertion_costs(&matrix, &assignments, 2, 1);

        // Tour for fac 0 visits d0 and d1. Optimal: depot->d0->d1->depot (cost 1+2+1=4)
        // or depot->d1->d0->depot (cost 1+2+1=4).
        // Insertion cost for d0 (remove d0 from tour first):
        //   Short tour: depot->d1->depot. Insert d0:
        //   - between depot-d1: d(depot,d0)+d(d0,d1)-d(depot,d1) = 1+2-1 = 2
        //   - between d1-depot: d(d1,d0)+d(d0,depot)-d(d1,depot) = 2+1-1 = 2
        //   min = 2
        assert!((costs[0][0] - 2.0).abs() < 1e-6);
    }

    #[test]
    fn triangle_insertion_costs() {
        // 3 demands forming a triangle, 1 depot at center.
        // coords: d0=(0,0), d1=(4,0), d2=(2,3), depot=(2,1)
        let coords = vec![
            (0.0, 0.0), // d0
            (4.0, 0.0), // d1
            (2.0, 3.0), // d2
            (2.0, 1.0), // depot
        ];
        let matrix = euclidean_matrix(&coords);

        // All assigned to fac 0
        let assignments = vec![0, 0, 0];
        let costs = compute_insertion_costs(&matrix, &assignments, 3, 1);

        // All insertion costs should be non-negative
        for i in 0..3 {
            assert!(costs[i][0] >= -1e-10, "insertion cost should be non-negative");
        }
    }

    #[test]
    fn two_facilities_cross_costs() {
        // 4 demands, 2 facilities. Check that insertion into the "wrong" facility
        // is more expensive than insertion into the "right" one.
        // coords: d0=(0,0), d1=(1,0), d2=(10,0), d3=(11,0), depot0=(0.5,0), depot1=(10.5,0)
        let coords = vec![
            (0.0, 0.0),   // d0
            (1.0, 0.0),   // d1
            (10.0, 0.0),  // d2
            (11.0, 0.0),  // d3
            (0.5, 0.0),   // depot0
            (10.5, 0.0),  // depot1
        ];
        let matrix = euclidean_matrix(&coords);

        let assignments = vec![0, 0, 1, 1]; // d0,d1 -> fac0; d2,d3 -> fac1
        let costs = compute_insertion_costs(&matrix, &assignments, 4, 2);

        // d0 should be cheaper to insert into fac0's tour than fac1's
        assert!(
            costs[0][0] < costs[0][1],
            "d0 insertion into fac0 ({}) should be cheaper than fac1 ({})",
            costs[0][0], costs[0][1]
        );

        // d2 should be cheaper to insert into fac1's tour than fac0's
        assert!(
            costs[2][1] < costs[2][0],
            "d2 insertion into fac1 ({}) should be cheaper than fac0 ({})",
            costs[2][1], costs[2][0]
        );
    }
}
