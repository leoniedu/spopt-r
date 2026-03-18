use extendr_api::prelude::*;

const EPS: f64 = 1e-10;

#[derive(Clone)]
struct Schedule {
    arrival: Vec<f64>,
    departure: Vec<f64>,
    total_cost: f64,
}

struct WindowSpec<'a> {
    earliest: &'a [f64],
    latest: &'a [f64],
    service: &'a [f64],
}

/// Compute total cost of a tour/path.
fn tour_cost(tour: &[usize], matrix: &[Vec<f64>]) -> f64 {
    if tour.len() < 2 {
        return 0.0;
    }

    let mut cost = 0.0;
    for i in 0..tour.len() - 1 {
        cost += matrix[tour[i]][tour[i + 1]];
    }
    cost
}

/// Check if a matrix is symmetric (d[i][j] == d[j][i] for all i,j).
pub fn is_symmetric(matrix: &[Vec<f64>]) -> bool {
    let n = matrix.len();
    for i in 0..n {
        for j in i + 1..n {
            if (matrix[i][j] - matrix[j][i]).abs() > EPS {
                return false;
            }
        }
    }
    true
}

fn schedule_tour(tour: &[usize], matrix: &[Vec<f64>], windows: &WindowSpec) -> Option<Schedule> {
    if tour.is_empty() {
        return Some(Schedule {
            arrival: Vec::new(),
            departure: Vec::new(),
            total_cost: 0.0,
        });
    }

    let mut arrival = vec![0.0; tour.len()];
    let mut departure = vec![0.0; tour.len()];

    let start = tour[0];
    let start_time = windows.earliest[start];
    if !start_time.is_finite() || start_time > windows.latest[start] + EPS {
        return None;
    }

    arrival[0] = start_time;
    departure[0] = start_time + windows.service[start];

    let mut total_cost = 0.0;
    for idx in 1..tour.len() {
        let prev = tour[idx - 1];
        let curr = tour[idx];
        let travel = matrix[prev][curr];

        if !travel.is_finite() {
            return None;
        }

        total_cost += travel;
        let earliest_service = (departure[idx - 1] + travel).max(windows.earliest[curr]);
        if !earliest_service.is_finite() || earliest_service > windows.latest[curr] + EPS {
            return None;
        }

        arrival[idx] = earliest_service;
        let service_duration = if idx == tour.len() - 1 && curr == tour[0] {
            0.0
        } else {
            windows.service[curr]
        };
        departure[idx] = earliest_service + service_duration;
    }

    Some(Schedule {
        arrival,
        departure,
        total_cost,
    })
}

/// Build a tour/path using nearest-neighbor heuristic with fixed endpoints.
fn nearest_neighbor(n: usize, start: usize, end: usize, matrix: &[Vec<f64>]) -> Vec<usize> {
    let mut visited = vec![false; n];
    let mut tour = Vec::with_capacity(n + usize::from(end == start));

    tour.push(start);
    visited[start] = true;
    if end != start {
        visited[end] = true;
    }

    let mut current = start;
    let n_to_insert = n - 1 - usize::from(end != start);
    for _ in 0..n_to_insert {
        let mut best_next = None;
        let mut best_cost = f64::MAX;

        for node in 0..n {
            if !visited[node] && matrix[current][node] < best_cost {
                best_cost = matrix[current][node];
                best_next = Some(node);
            }
        }

        let next = best_next.expect("nearest-neighbor could not find an unvisited node");
        tour.push(next);
        visited[next] = true;
        current = next;
    }

    tour.push(end);
    tour
}

fn feasible_insertion(
    n: usize,
    start: usize,
    end: usize,
    matrix: &[Vec<f64>],
    windows: &WindowSpec,
) -> Result<Vec<usize>> {
    let mut tour = vec![start, end];
    let mut unvisited: Vec<usize> = (0..n)
        .filter(|&node| node != start && node != end)
        .collect();

    while !unvisited.is_empty() {
        let mut best_move: Option<(usize, usize, f64)> = None;

        for &node in &unvisited {
            for insert_idx in 1..tour.len() {
                let mut candidate = tour.clone();
                candidate.insert(insert_idx, node);

                if let Some(schedule) = schedule_tour(&candidate, matrix, windows) {
                    match best_move {
                        None => best_move = Some((node, insert_idx, schedule.total_cost)),
                        Some((_, _, best_cost)) if schedule.total_cost < best_cost - EPS => {
                            best_move = Some((node, insert_idx, schedule.total_cost));
                        }
                        _ => {}
                    }
                }
            }
        }

        if let Some((node, insert_idx, _)) = best_move {
            tour.insert(insert_idx, node);
            unvisited.retain(|&candidate| candidate != node);
        } else {
            return Err(Error::Other(
                "Could not construct a feasible route for the supplied time windows.".to_string(),
            ));
        }
    }

    Ok(tour)
}

/// One pass of 2-opt improvement on interior segments with fixed endpoints.
fn two_opt_pass(tour: &mut Vec<usize>, matrix: &[Vec<f64>], symmetric: bool) -> bool {
    let n = tour.len();
    let mut improved = false;

    for i in 1..n - 2 {
        for j in i + 1..n - 1 {
            let delta = if symmetric {
                let old = matrix[tour[i - 1]][tour[i]] + matrix[tour[j]][tour[j + 1]];
                let new = matrix[tour[i - 1]][tour[j]] + matrix[tour[i]][tour[j + 1]];
                new - old
            } else {
                let mut old_cost = matrix[tour[i - 1]][tour[i]] + matrix[tour[j]][tour[j + 1]];
                for k in i..j {
                    old_cost += matrix[tour[k]][tour[k + 1]];
                }
                let mut new_cost = matrix[tour[i - 1]][tour[j]] + matrix[tour[i]][tour[j + 1]];
                for k in i..j {
                    new_cost += matrix[tour[k + 1]][tour[k]];
                }
                new_cost - old_cost
            };

            if delta < -EPS {
                tour[i..=j].reverse();
                improved = true;
            }
        }
    }

    improved
}

fn two_opt_pass_windows(
    tour: &mut Vec<usize>,
    matrix: &[Vec<f64>],
    windows: &WindowSpec,
) -> bool {
    let current_cost = tour_cost(tour, matrix);

    for i in 1..tour.len() - 2 {
        for j in i + 1..tour.len() - 1 {
            let mut candidate = tour.clone();
            candidate[i..=j].reverse();

            if let Some(schedule) = schedule_tour(&candidate, matrix, windows) {
                if schedule.total_cost < current_cost - EPS {
                    *tour = candidate;
                    return true;
                }
            }
        }
    }

    false
}

/// One pass of or-opt: relocate segments of length 1, 2, 3.
fn or_opt_pass(tour: &mut Vec<usize>, matrix: &[Vec<f64>]) -> bool {
    let mut improved = false;
    for seg_len in 1..=3 {
        if or_opt_segment(tour, matrix, seg_len) {
            improved = true;
        }
    }
    improved
}

fn or_opt_segment(tour: &mut Vec<usize>, matrix: &[Vec<f64>], seg_len: usize) -> bool {
    let n = tour.len();
    let mut improved = false;
    let mut start = 1;

    while start + seg_len <= n - 1 {
        let before_seg = tour[start - 1];
        let seg_first = tour[start];
        let seg_last = tour[start + seg_len - 1];
        let after_seg = tour[start + seg_len];

        let removal_saving = matrix[before_seg][seg_first]
            + matrix[seg_last][after_seg]
            - matrix[before_seg][after_seg];

        let mut best_insert: Option<usize> = None;
        let mut best_gain = EPS;

        for insert_after in 0..n - 1 {
            if insert_after >= start.saturating_sub(1) && insert_after < start + seg_len {
                continue;
            }

            let a = tour[insert_after];
            let b = tour[insert_after + 1];
            let insert_cost = matrix[a][seg_first] + matrix[seg_last][b] - matrix[a][b];
            let gain = removal_saving - insert_cost;

            if gain > best_gain {
                best_gain = gain;
                best_insert = Some(insert_after);
            }
        }

        if let Some(insert_pos) = best_insert {
            let segment: Vec<usize> = tour[start..start + seg_len].to_vec();
            tour.drain(start..start + seg_len);

            let adjusted_pos = if insert_pos > start {
                insert_pos - seg_len
            } else {
                insert_pos
            };

            let insert_idx = adjusted_pos + 1;
            for (k, &node) in segment.iter().enumerate() {
                tour.insert(insert_idx + k, node);
            }
            improved = true;
        } else {
            start += 1;
        }
    }

    improved
}

fn or_opt_pass_windows(
    tour: &mut Vec<usize>,
    matrix: &[Vec<f64>],
    windows: &WindowSpec,
) -> bool {
    let current_cost = tour_cost(tour, matrix);
    let n = tour.len();

    for seg_len in 1..=3 {
        if seg_len >= n - 1 {
            continue;
        }

        for start in 1..n - seg_len {
            let segment: Vec<usize> = tour[start..start + seg_len].to_vec();
            let mut base = tour.clone();
            base.drain(start..start + seg_len);

            for insert_idx in 1..base.len() {
                let mut candidate = base.clone();
                for (offset, &node) in segment.iter().enumerate() {
                    candidate.insert(insert_idx + offset, node);
                }

                if candidate == *tour {
                    continue;
                }

                if let Some(schedule) = schedule_tour(&candidate, matrix, windows) {
                    if schedule.total_cost < current_cost - EPS {
                        *tour = candidate;
                        return true;
                    }
                }
            }
        }
    }

    false
}

/// Solve TSP/path-TSP with optional time windows.
pub fn solve(
    cost_matrix: RMatrix<f64>,
    start: usize,
    end: Option<usize>,
    method: &str,
    earliest: Option<&[f64]>,
    latest: Option<&[f64]>,
    service: Option<&[f64]>,
) -> Result<List> {
    let base_n = cost_matrix.nrows();
    let flat = cost_matrix.data();

    let mut matrix = vec![vec![0.0; base_n]; base_n];
    for i in 0..base_n {
        for j in 0..base_n {
            matrix[i][j] = flat[j * base_n + i];
        }
    }

    let mut earliest_vec = vec![0.0; base_n];
    let mut latest_vec = vec![f64::INFINITY; base_n];
    let mut service_vec = vec![0.0; base_n];

    if let Some(values) = earliest {
        earliest_vec.clone_from_slice(values);
    }
    if let Some(values) = latest {
        latest_vec.clone_from_slice(values);
    }
    if let Some(values) = service {
        service_vec.clone_from_slice(values);
    }

    let open_route = end.is_none();
    let effective_end = match end {
        Some(node) => node,
        None => {
            for row in &mut matrix {
                row.push(0.0);
            }
            matrix.push(vec![0.0; base_n + 1]);
            earliest_vec.push(0.0);
            latest_vec.push(f64::INFINITY);
            service_vec.push(0.0);
            base_n
        }
    };

    let windows = WindowSpec {
        earliest: &earliest_vec,
        latest: &latest_vec,
        service: &service_vec,
    };
    let use_windows = earliest.is_some() || latest.is_some() || service.is_some();

    let nn_tour = if use_windows {
        feasible_insertion(matrix.len(), start, effective_end, &matrix, &windows)?
    } else {
        nearest_neighbor(matrix.len(), start, effective_end, &matrix)
    };
    let nn_schedule = schedule_tour(&nn_tour, &matrix, &windows).ok_or_else(|| {
        Error::Other("Failed to build an initial feasible route.".to_string())
    })?;
    let nn_cost = nn_schedule.total_cost;

    let symmetric = is_symmetric(&matrix);

    let (tour, schedule, iterations) = match method {
        "nn" => (nn_tour, nn_schedule, 0u32),
        "2-opt" | "2opt" => {
            let mut tour = nn_tour;
            let mut iterations = 0u32;

            loop {
                let mut any_improved = false;

                if use_windows {
                    if two_opt_pass_windows(&mut tour, &matrix, &windows) {
                        any_improved = true;
                    }
                    if or_opt_pass_windows(&mut tour, &matrix, &windows) {
                        any_improved = true;
                    }
                } else {
                    if two_opt_pass(&mut tour, &matrix, symmetric) {
                        any_improved = true;
                    }
                    if or_opt_pass(&mut tour, &matrix) {
                        any_improved = true;
                    }
                }

                iterations += 1;
                if !any_improved {
                    break;
                }
            }

            let schedule = schedule_tour(&tour, &matrix, &windows).ok_or_else(|| {
                Error::Other("Improvement search produced an infeasible route.".to_string())
            })?;
            (tour, schedule, iterations)
        }
        _ => {
            return Err(Error::Other(format!(
                "Unknown method '{}'. Use 'nn' or '2-opt'.",
                method
            )))
        }
    };

    let improvement = if nn_cost > 0.0 {
        (nn_cost - schedule.total_cost) / nn_cost * 100.0
    } else {
        0.0
    };

    let clean_len = if open_route {
        tour.len() - 1
    } else {
        tour.len()
    };
    let route_tour = &tour[..clean_len];
    let route_arrival = &schedule.arrival[..clean_len];
    let route_departure = &schedule.departure[..clean_len];

    let tour_r: Vec<i32> = route_tour.iter().map(|&node| (node + 1) as i32).collect();

    Ok(list!(
        tour = tour_r,
        total_cost = (schedule.total_cost * 100.0).round() / 100.0,
        nn_cost = (nn_cost * 100.0).round() / 100.0,
        improvement_pct = (improvement * 100.0).round() / 100.0,
        iterations = iterations as i32,
        arrival_time = route_arrival.to_vec(),
        departure_time = route_departure.to_vec()
    ))
}
