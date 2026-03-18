use extendr_api::prelude::*;

/// A single vehicle route: depot -> customers -> depot
#[derive(Clone, Debug)]
struct Route {
    stops: Vec<usize>,  // customer indices (not including depot)
    load: f64,
    cost: f64,
    total_time: f64,    // travel time + service time at non-depot stops
}

impl Route {
    fn new() -> Self {
        Route {
            stops: Vec::new(),
            load: 0.0,
            cost: 0.0,
            total_time: 0.0,
        }
    }

    /// Recompute route cost and total_time from scratch
    fn recompute(&mut self, depot: usize, matrix: &[Vec<f64>], service_times: &[f64]) {
        if self.stops.is_empty() {
            self.cost = 0.0;
            self.total_time = 0.0;
            return;
        }
        let mut c = matrix[depot][self.stops[0]];
        for i in 0..self.stops.len() - 1 {
            c += matrix[self.stops[i]][self.stops[i + 1]];
        }
        c += matrix[*self.stops.last().unwrap()][depot];
        self.cost = c;
        self.total_time = c + self.stops.iter().map(|&s| service_times[s]).sum::<f64>();
    }

    /// Get the full tour including depot at start and end
    fn full_tour(&self, depot: usize) -> Vec<usize> {
        let mut tour = Vec::with_capacity(self.stops.len() + 2);
        tour.push(depot);
        tour.extend_from_slice(&self.stops);
        tour.push(depot);
        tour
    }
}

// ---------------------------------------------------------------------------
// Time-window schedule computation
// ---------------------------------------------------------------------------

/// Compute schedule for a VRP route given time windows.
/// Returns Some((arrivals, departures, cost, total_time)) or None if infeasible.
/// Empty routes return Some with empty vecs, cost=0, total_time=0.
fn schedule_route(
    stops: &[usize],
    depot: usize,
    matrix: &[Vec<f64>],
    service_times: &[f64],
    earliest: &[f64],
    latest: &[f64],
) -> Option<(Vec<f64>, Vec<f64>, f64, f64)> {
    if stops.is_empty() {
        return Some((Vec::new(), Vec::new(), 0.0, 0.0));
    }

    let depot_departure = earliest[depot];
    let mut arrivals = vec![0.0; stops.len()];
    let mut departures = vec![0.0; stops.len()];
    let mut cost = 0.0;

    // First stop
    let travel = matrix[depot][stops[0]];
    cost += travel;
    let arrive = depot_departure + travel;
    let service_start = arrive.max(earliest[stops[0]]);
    if service_start > latest[stops[0]] + 1e-10 {
        return None;
    }
    arrivals[0] = service_start;
    departures[0] = service_start + service_times[stops[0]];

    // Remaining stops
    for i in 1..stops.len() {
        let travel = matrix[stops[i - 1]][stops[i]];
        cost += travel;
        let arrive = departures[i - 1] + travel;
        let service_start = arrive.max(earliest[stops[i]]);
        if service_start > latest[stops[i]] + 1e-10 {
            return None;
        }
        arrivals[i] = service_start;
        departures[i] = service_start + service_times[stops[i]];
    }

    // Return to depot
    let return_travel = matrix[*stops.last().unwrap()][depot];
    cost += return_travel;
    let return_time = departures[stops.len() - 1] + return_travel;
    let total_time = return_time - depot_departure;

    Some((arrivals, departures, cost, total_time))
}

/// Recompute route cost/load and set total_time from schedule.
/// Must only be called when feasibility has already been verified.
fn recompute_with_schedule(
    route: &mut Route,
    depot: usize,
    matrix: &[Vec<f64>],
    service_times: &[f64],
    earliest: &[f64],
    latest: &[f64],
) {
    route.recompute(depot, matrix, service_times);
    if route.stops.is_empty() {
        return;
    }
    if let Some((_, _, _, total_time)) = schedule_route(
        &route.stops, depot, matrix, service_times, earliest, latest,
    ) {
        route.total_time = total_time;
    }
    // If schedule_route returns None here, it's a caller bug -- feasibility
    // should have been checked before calling this function. We keep the
    // recompute() total_time as a safe fallback.
}

/// Check if a route's stops are time-feasible (windows + max_route_time).
/// When windows are not active, checks max_route_time against the non-windowed total_time.
/// When windows are active, uses schedule_route for the authoritative total_time.
/// Returns Some(total_time) if feasible, None if not.
fn check_time_feasibility(
    stops: &[usize],
    depot: usize,
    matrix: &[Vec<f64>],
    service_times: &[f64],
    max_route_time: Option<f64>,
    earliest: Option<&[f64]>,
    latest: Option<&[f64]>,
) -> Option<f64> {
    if let (Some(ew), Some(lw)) = (earliest, latest) {
        // Window-aware: schedule determines feasibility and total_time
        match schedule_route(stops, depot, matrix, service_times, ew, lw) {
            None => return None, // window violated
            Some((_, _, _, total_time)) => {
                if let Some(max_t) = max_route_time {
                    if total_time > max_t + 1e-10 {
                        return None;
                    }
                }
                return Some(total_time);
            }
        }
    }

    // No windows: compute non-windowed total_time
    if stops.is_empty() {
        return Some(0.0);
    }
    let mut cost = matrix[depot][stops[0]];
    for i in 0..stops.len() - 1 {
        cost += matrix[stops[i]][stops[i + 1]];
    }
    cost += matrix[*stops.last().unwrap()][depot];
    let total_time = cost + stops.iter().map(|&s| service_times[s]).sum::<f64>();

    if let Some(max_t) = max_route_time {
        if total_time > max_t + 1e-10 {
            return None;
        }
    }
    Some(total_time)
}

// ---------------------------------------------------------------------------
// Clarke-Wright Savings construction heuristic
// ---------------------------------------------------------------------------

struct Saving {
    i: usize,
    j: usize,
    value: f64,
}

fn clarke_wright(
    n: usize,
    depot: usize,
    matrix: &[Vec<f64>],
    demands: &[f64],
    capacity: f64,
    max_vehicles: Option<usize>,
    service_times: &[f64],
    max_route_time: Option<f64>,
    earliest: Option<&[f64]>,
    latest: Option<&[f64]>,
) -> Vec<Route> {
    let customers: Vec<usize> = (0..n).filter(|&i| i != depot).collect();
    let nc = customers.len();

    // Start: one route per customer
    let mut routes: Vec<Option<Route>> = Vec::with_capacity(nc);
    // Map customer -> route index
    let mut customer_route: Vec<usize> = vec![0; n];

    for (idx, &c) in customers.iter().enumerate() {
        let mut r = Route::new();
        r.stops.push(c);
        r.load = demands[c];
        r.cost = matrix[depot][c] + matrix[c][depot];
        // Set total_time: window-aware if windows active, otherwise travel + service
        if let Some(tt) = check_time_feasibility(&r.stops, depot, matrix, service_times, max_route_time, earliest, latest) {
            r.total_time = tt;
        } else {
            r.total_time = r.cost + service_times[c];
        }
        routes.push(Some(r));
        customer_route[c] = idx;
    }

    // Compute savings: s(i,j) = d(depot,i) + d(depot,j) - d(i,j)
    let mut savings: Vec<Saving> = Vec::with_capacity(nc * nc);
    for &i in &customers {
        for &j in &customers {
            if i < j {
                let s = matrix[depot][i] + matrix[j][depot] - matrix[i][j];
                if s > 0.0 {
                    savings.push(Saving { i, j, value: s });
                }
            }
        }
    }

    // Sort descending by savings value
    savings.sort_by(|a, b| b.value.partial_cmp(&a.value).unwrap());

    // Merge routes
    for saving in &savings {
        let ri = customer_route[saving.i];
        let rj = customer_route[saving.j];

        // Skip if same route or either route already merged away
        if ri == rj {
            continue;
        }
        if routes[ri].is_none() || routes[rj].is_none() {
            continue;
        }

        let route_i = routes[ri].as_ref().unwrap();
        let route_j = routes[rj].as_ref().unwrap();

        // Check capacity
        if route_i.load + route_j.load > capacity {
            continue;
        }

        // Check if max_vehicles constraint would be violated by NOT merging
        // (we always try to merge when possible)

        // i must be the LAST stop in route_i, j must be the FIRST stop in route_j
        let i_is_last = *route_i.stops.last().unwrap() == saving.i;
        let j_is_first = route_j.stops[0] == saving.j;

        // Also try i first in route_i, j last in route_j (reversed merge)
        let i_is_first = route_i.stops[0] == saving.i;
        let j_is_last = *route_j.stops.last().unwrap() == saving.j;

        if i_is_last && j_is_first {
            // Merge: route_i ++ route_j
            let mut merged = routes[ri].take().unwrap();
            let donor = routes[rj].take().unwrap();
            let saved_merged = merged.clone();
            let saved_donor = donor.clone();
            for &c in &donor.stops {
                customer_route[c] = ri;
            }
            merged.stops.extend_from_slice(&donor.stops);
            merged.load += donor.load;
            merged.recompute(depot, matrix, service_times);
            // Check time + window feasibility
            match check_time_feasibility(&merged.stops, depot, matrix, service_times, max_route_time, earliest, latest) {
                None => {
                    // Undo merge
                    for &c in &saved_donor.stops {
                        customer_route[c] = rj;
                    }
                    routes[ri] = Some(saved_merged);
                    routes[rj] = Some(saved_donor);
                    continue;
                }
                Some(tt) => { merged.total_time = tt; }
            }
            routes[ri] = Some(merged);
        } else if j_is_last && i_is_first {
            // Merge: route_j ++ route_i
            let mut merged = routes[rj].take().unwrap();
            let donor = routes[ri].take().unwrap();
            let saved_merged = merged.clone();
            let saved_donor = donor.clone();
            for &c in &donor.stops {
                customer_route[c] = rj;
            }
            merged.stops.extend_from_slice(&donor.stops);
            merged.load += donor.load;
            merged.recompute(depot, matrix, service_times);
            // Check time + window feasibility
            match check_time_feasibility(&merged.stops, depot, matrix, service_times, max_route_time, earliest, latest) {
                None => {
                    // Undo merge
                    for &c in &saved_donor.stops {
                        customer_route[c] = ri;
                    }
                    routes[rj] = Some(saved_merged);
                    routes[ri] = Some(saved_donor);
                    continue;
                }
                Some(tt) => { merged.total_time = tt; }
            }
            routes[rj] = Some(merged);
        }
        // If neither endpoint condition is met, skip this saving
    }

    // Collect non-empty routes
    let mut result: Vec<Route> = routes.into_iter().flatten().collect();

    // If max_vehicles is set and we have too many routes, reduce to target
    if let Some(max_v) = max_vehicles {
        // Phase 1: Try simple route concatenation merges
        while result.len() > max_v {
            let mut best_merge: Option<(usize, usize, f64)> = None;

            for i in 0..result.len() {
                for j in i + 1..result.len() {
                    if result[i].load + result[j].load <= capacity {
                        // Check time + window feasibility of potential merge
                        let mut trial_stops = result[i].stops.clone();
                        trial_stops.extend_from_slice(&result[j].stops);
                        if check_time_feasibility(&trial_stops, depot, matrix, service_times, max_route_time, earliest, latest).is_none() {
                            continue;
                        }
                        let merge_cost = result[i].cost + result[j].cost;
                        if best_merge.is_none()
                            || merge_cost < best_merge.unwrap().2
                        {
                            best_merge = Some((i, j, merge_cost));
                        }
                    }
                }
            }

            if let Some((i, j, _)) = best_merge {
                let donor = result.remove(j);
                result[i].stops.extend_from_slice(&donor.stops);
                result[i].load += donor.load;
                result[i].recompute(depot, matrix, service_times);
                // Update total_time from schedule if windows active
                if let (Some(ew), Some(lw)) = (earliest, latest) {
                    if let Some((_, _, _, tt)) = schedule_route(&result[i].stops, depot, matrix, service_times, ew, lw) {
                        result[i].total_time = tt;
                    }
                }
            } else {
                break;
            }
        }

        // Phase 2: If still too many routes, rebuild from scratch using
        // first-fit-decreasing bin packing into exactly max_v routes,
        // then optimize insertion order for cost.
        if result.len() > max_v {
            // Collect ALL customers from ALL routes
            let mut all_customers: Vec<usize> = Vec::new();
            for route in &result {
                all_customers.extend_from_slice(&route.stops);
            }

            // Sort by demand descending (first-fit-decreasing)
            all_customers.sort_by(|&a, &b| demands[b].partial_cmp(&demands[a]).unwrap());

            // Create exactly max_v empty routes
            let mut new_routes: Vec<Route> = (0..max_v).map(|_| Route::new()).collect();

            // Assign each customer to the route with the most remaining
            // capacity that can still fit it (and satisfies time constraint)
            let mut unassigned: Vec<usize> = Vec::new();
            for &customer in &all_customers {
                let cust_demand = demands[customer];

                // Collect candidate routes sorted by remaining capacity descending
                let mut candidates: Vec<(usize, f64)> = new_routes.iter().enumerate()
                    .filter(|(_, route)| capacity - route.load >= cust_demand)
                    .map(|(ri, route)| (ri, capacity - route.load))
                    .collect();
                candidates.sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap());

                let mut inserted = false;
                for (ri, _) in candidates {
                    // Find best insertion position within this route
                    let rlen = new_routes[ri].stops.len();
                    let mut best_pos = rlen; // default: append
                    let mut best_cost = f64::MAX;

                    for pos in 0..=rlen {
                        let before = if pos == 0 { depot } else { new_routes[ri].stops[pos - 1] };
                        let after = if pos == rlen { depot } else { new_routes[ri].stops[pos] };
                        let cost = matrix[before][customer] + matrix[customer][after]
                            - matrix[before][after];
                        if cost < best_cost {
                            best_cost = cost;
                            best_pos = pos;
                        }
                    }

                    new_routes[ri].stops.insert(best_pos, customer);
                    new_routes[ri].load += cust_demand;
                    new_routes[ri].recompute(depot, matrix, service_times);

                    // Check time + window feasibility; if exceeded, undo and try next route
                    match check_time_feasibility(&new_routes[ri].stops, depot, matrix, service_times, max_route_time, earliest, latest) {
                        None => {
                            new_routes[ri].stops.remove(best_pos);
                            new_routes[ri].load -= cust_demand;
                            new_routes[ri].recompute(depot, matrix, service_times);
                            continue;
                        }
                        Some(tt) => { new_routes[ri].total_time = tt; }
                    }

                    inserted = true;
                    break;
                }
                if !inserted {
                    unassigned.push(customer);
                }
            }

            // If any customers couldn't be assigned, fall back to keeping
            // the pre-FFD routes (which may exceed max_v but are complete).
            // The R wrapper's post-check will catch the n_vehicles overshoot.
            if unassigned.is_empty() {
                new_routes.retain(|r| !r.stops.is_empty());
                result = new_routes;
            }
        }
    }

    result
}

// ---------------------------------------------------------------------------
// Intra-route 2-opt (same as TSP, applied to each route)
// ---------------------------------------------------------------------------

fn intra_route_two_opt(route: &mut Route, depot: usize, matrix: &[Vec<f64>], symmetric: bool, service_times: &[f64]) -> bool {
    if route.stops.len() < 3 {
        return false;
    }

    let mut tour = route.full_tour(depot);
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

            if delta < -1e-10 {
                tour[i..=j].reverse();
                improved = true;
            }
        }
    }

    if improved {
        route.stops = tour[1..tour.len() - 1].to_vec();
        route.recompute(depot, matrix, service_times);
    }

    improved
}

// ---------------------------------------------------------------------------
// Intra-route or-opt: relocate 1, 2, or 3 consecutive stops within a route
// ---------------------------------------------------------------------------

fn intra_route_or_opt(route: &mut Route, depot: usize, matrix: &[Vec<f64>], service_times: &[f64]) -> bool {
    if route.stops.len() < 3 {
        return false;
    }

    let mut tour = route.full_tour(depot);
    let mut improved = false;

    for seg_len in 1..=3usize {
        let mut start = 1;
        while start + seg_len <= tour.len() - 1 {
            let before_seg = tour[start - 1];
            let seg_first = tour[start];
            let seg_last = tour[start + seg_len - 1];
            let after_seg = tour[start + seg_len];

            let removal_saving = matrix[before_seg][seg_first]
                + matrix[seg_last][after_seg]
                - matrix[before_seg][after_seg];

            let mut best_insert: Option<usize> = None;
            let mut best_gain = 1e-10;

            for insert_after in 0..tour.len() - 1 {
                // Skip positions that overlap with the segment
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
    }

    if improved {
        route.stops = tour[1..tour.len() - 1].to_vec();
        route.recompute(depot, matrix, service_times);
    }

    improved
}

// ---------------------------------------------------------------------------
// Window-aware intra-route 2-opt (first-improvement)
// ---------------------------------------------------------------------------

fn intra_route_two_opt_windows(
    route: &mut Route,
    depot: usize,
    matrix: &[Vec<f64>],
    service_times: &[f64],
    earliest: &[f64],
    latest: &[f64],
) -> bool {
    if route.stops.len() < 3 {
        return false;
    }

    let current_cost = route.cost;

    for i in 0..route.stops.len() - 1 {
        for j in i + 1..route.stops.len() {
            let mut candidate = route.stops.clone();
            candidate[i..=j].reverse();

            if let Some((_, _, cost, total_time)) =
                schedule_route(&candidate, depot, matrix, service_times, earliest, latest)
            {
                if cost < current_cost - 1e-10 {
                    route.stops = candidate;
                    route.cost = cost;
                    route.total_time = total_time;
                    return true; // first-improvement
                }
            }
        }
    }

    false
}

// ---------------------------------------------------------------------------
// Window-aware intra-route or-opt (first-improvement)
// ---------------------------------------------------------------------------

fn intra_route_or_opt_windows(
    route: &mut Route,
    depot: usize,
    matrix: &[Vec<f64>],
    service_times: &[f64],
    earliest: &[f64],
    latest: &[f64],
) -> bool {
    if route.stops.len() < 3 {
        return false;
    }

    let current_cost = route.cost;

    for seg_len in 1..=3usize {
        for start in 0..route.stops.len().saturating_sub(seg_len - 1) {
            if start + seg_len > route.stops.len() {
                break;
            }
            let segment: Vec<usize> = route.stops[start..start + seg_len].to_vec();
            let mut base = route.stops.clone();
            base.drain(start..start + seg_len);

            // Try all insertion positions in the remaining stops
            for insert_pos in 0..=base.len() {
                if insert_pos == start {
                    continue; // same position
                }
                let mut candidate = base.clone();
                for (k, &node) in segment.iter().enumerate() {
                    candidate.insert(insert_pos + k, node);
                }

                if let Some((_, _, cost, total_time)) =
                    schedule_route(&candidate, depot, matrix, service_times, earliest, latest)
                {
                    if cost < current_cost - 1e-10 {
                        route.stops = candidate;
                        route.cost = cost;
                        route.total_time = total_time;
                        return true; // first-improvement
                    }
                }
            }
        }
    }

    false
}

// ---------------------------------------------------------------------------
// Inter-route relocate: move one customer from one route to another
// ---------------------------------------------------------------------------

fn inter_route_relocate(
    routes: &mut Vec<Route>,
    depot: usize,
    matrix: &[Vec<f64>],
    demands: &[f64],
    capacity: f64,
    service_times: &[f64],
    max_route_time: Option<f64>,
) -> bool {
    let n_routes = routes.len();
    let mut best_gain = 1e-10;
    let mut best_move: Option<(usize, usize, usize, usize)> = None; // (from_route, from_pos, to_route, to_pos)

    for ri in 0..n_routes {
        for pos in 0..routes[ri].stops.len() {
            let customer = routes[ri].stops[pos];
            let cust_demand = demands[customer];

            // Cost of removing customer from route ri
            let prev = if pos == 0 { depot } else { routes[ri].stops[pos - 1] };
            let next = if pos == routes[ri].stops.len() - 1 {
                depot
            } else {
                routes[ri].stops[pos + 1]
            };
            let removal_saving =
                matrix[prev][customer] + matrix[customer][next] - matrix[prev][next];

            // Try inserting into every other route
            for rj in 0..n_routes {
                if ri == rj {
                    continue;
                }
                if routes[rj].load + cust_demand > capacity {
                    continue;
                }

                // Try every insertion position in route rj
                let rj_len = routes[rj].stops.len();
                for insert_pos in 0..=rj_len {
                    let before = if insert_pos == 0 {
                        depot
                    } else {
                        routes[rj].stops[insert_pos - 1]
                    };
                    let after = if insert_pos == rj_len {
                        depot
                    } else {
                        routes[rj].stops[insert_pos]
                    };

                    let insert_cost =
                        matrix[before][customer] + matrix[customer][after] - matrix[before][after];

                    // Check time feasibility on receiving route
                    if let Some(max_t) = max_route_time {
                        if routes[rj].total_time + insert_cost + service_times[customer] > max_t {
                            continue;
                        }
                    }

                    let gain = removal_saving - insert_cost;
                    if gain > best_gain {
                        best_gain = gain;
                        best_move = Some((ri, pos, rj, insert_pos));
                    }
                }
            }
        }
    }

    if let Some((from_r, from_pos, to_r, to_pos)) = best_move {
        let customer = routes[from_r].stops.remove(from_pos);
        let cust_demand = demands[customer];
        routes[from_r].load -= cust_demand;
        routes[from_r].recompute(depot, matrix, service_times);

        routes[to_r].stops.insert(to_pos, customer);
        routes[to_r].load += cust_demand;
        routes[to_r].recompute(depot, matrix, service_times);

        // Remove empty routes
        routes.retain(|r| !r.stops.is_empty());

        return true;
    }

    false
}

// ---------------------------------------------------------------------------
// Inter-route swap: exchange one customer between two routes
// ---------------------------------------------------------------------------

fn inter_route_swap(
    routes: &mut Vec<Route>,
    depot: usize,
    matrix: &[Vec<f64>],
    demands: &[f64],
    capacity: f64,
    service_times: &[f64],
    max_route_time: Option<f64>,
) -> bool {
    let n_routes = routes.len();
    let mut best_gain = 1e-10;
    let mut best_swap: Option<(usize, usize, usize, usize)> = None;

    for ri in 0..n_routes {
        for pi in 0..routes[ri].stops.len() {
            let ci = routes[ri].stops[pi];

            for rj in ri + 1..n_routes {
                for pj in 0..routes[rj].stops.len() {
                    let cj = routes[rj].stops[pj];

                    // Check capacity feasibility after swap
                    let new_load_i = routes[ri].load - demands[ci] + demands[cj];
                    let new_load_j = routes[rj].load - demands[cj] + demands[ci];
                    if new_load_i > capacity || new_load_j > capacity {
                        continue;
                    }

                    // Compute cost change for route i: replace ci with cj
                    let prev_i = if pi == 0 { depot } else { routes[ri].stops[pi - 1] };
                    let next_i = if pi == routes[ri].stops.len() - 1 {
                        depot
                    } else {
                        routes[ri].stops[pi + 1]
                    };
                    let old_i = matrix[prev_i][ci] + matrix[ci][next_i];
                    let new_i = matrix[prev_i][cj] + matrix[cj][next_i];

                    // Compute cost change for route j: replace cj with ci
                    let prev_j = if pj == 0 { depot } else { routes[rj].stops[pj - 1] };
                    let next_j = if pj == routes[rj].stops.len() - 1 {
                        depot
                    } else {
                        routes[rj].stops[pj + 1]
                    };
                    let old_j = matrix[prev_j][cj] + matrix[cj][next_j];
                    let new_j = matrix[prev_j][ci] + matrix[ci][next_j];

                    // Check time feasibility after swap
                    if let Some(max_t) = max_route_time {
                        let travel_delta_i = new_i - old_i;
                        let service_delta_i = service_times[cj] - service_times[ci];
                        let new_time_i = routes[ri].total_time + travel_delta_i + service_delta_i;
                        if new_time_i > max_t {
                            continue;
                        }
                        let travel_delta_j = new_j - old_j;
                        let service_delta_j = service_times[ci] - service_times[cj];
                        let new_time_j = routes[rj].total_time + travel_delta_j + service_delta_j;
                        if new_time_j > max_t {
                            continue;
                        }
                    }

                    let gain = (old_i + old_j) - (new_i + new_j);
                    if gain > best_gain {
                        best_gain = gain;
                        best_swap = Some((ri, pi, rj, pj));
                    }
                }
            }
        }
    }

    if let Some((ri, pi, rj, pj)) = best_swap {
        let ci = routes[ri].stops[pi];
        let cj = routes[rj].stops[pj];

        routes[ri].stops[pi] = cj;
        routes[rj].stops[pj] = ci;

        routes[ri].load = routes[ri].load - demands[ci] + demands[cj];
        routes[rj].load = routes[rj].load - demands[cj] + demands[ci];

        routes[ri].recompute(depot, matrix, service_times);
        routes[rj].recompute(depot, matrix, service_times);

        return true;
    }

    false
}

// ---------------------------------------------------------------------------
// Window-aware inter-route relocate
// ---------------------------------------------------------------------------

fn inter_route_relocate_windows(
    routes: &mut Vec<Route>,
    depot: usize,
    matrix: &[Vec<f64>],
    demands: &[f64],
    capacity: f64,
    service_times: &[f64],
    max_route_time: Option<f64>,
    earliest: &[f64],
    latest: &[f64],
) -> bool {
    let n_routes = routes.len();
    let mut best_gain = 1e-10;
    let mut best_move: Option<(usize, usize, usize, usize)> = None;

    for ri in 0..n_routes {
        for pos in 0..routes[ri].stops.len() {
            let customer = routes[ri].stops[pos];
            let cust_demand = demands[customer];

            for rj in 0..n_routes {
                if ri == rj { continue; }
                if routes[rj].load + cust_demand > capacity { continue; }

                let rj_len = routes[rj].stops.len();
                for insert_pos in 0..=rj_len {
                    // Build candidate receiving route
                    let mut rj_stops = routes[rj].stops.clone();
                    rj_stops.insert(insert_pos, customer);

                    // Check window feasibility on receiving route
                    let rj_sched = match schedule_route(
                        &rj_stops, depot, matrix, service_times, earliest, latest,
                    ) {
                        Some(s) => s,
                        None => continue,
                    };

                    // Check max_route_time
                    if let Some(max_t) = max_route_time {
                        if rj_sched.3 > max_t + 1e-10 { continue; }
                    }

                    // Build candidate source route (customer removed)
                    let mut ri_stops = routes[ri].stops.clone();
                    ri_stops.remove(pos);

                    let ri_sched = match schedule_route(
                        &ri_stops, depot, matrix, service_times, earliest, latest,
                    ) {
                        Some(s) => s,
                        None => continue,
                    };

                    if let Some(max_t) = max_route_time {
                        if ri_sched.3 > max_t + 1e-10 { continue; }
                    }

                    // Cost gain from actual schedules
                    let old_cost = routes[ri].cost + routes[rj].cost;
                    let new_cost = ri_sched.2 + rj_sched.2;
                    let gain = old_cost - new_cost;

                    if gain > best_gain {
                        best_gain = gain;
                        best_move = Some((ri, pos, rj, insert_pos));
                    }
                }
            }
        }
    }

    if let Some((from_r, from_pos, to_r, to_pos)) = best_move {
        let customer = routes[from_r].stops.remove(from_pos);
        routes[from_r].load -= demands[customer];
        recompute_with_schedule(&mut routes[from_r], depot, matrix, service_times, earliest, latest);

        routes[to_r].stops.insert(to_pos, customer);
        routes[to_r].load += demands[customer];
        recompute_with_schedule(&mut routes[to_r], depot, matrix, service_times, earliest, latest);

        routes.retain(|r| !r.stops.is_empty());
        return true;
    }

    false
}

// ---------------------------------------------------------------------------
// Window-aware inter-route swap
// ---------------------------------------------------------------------------

fn inter_route_swap_windows(
    routes: &mut Vec<Route>,
    depot: usize,
    matrix: &[Vec<f64>],
    demands: &[f64],
    capacity: f64,
    service_times: &[f64],
    max_route_time: Option<f64>,
    earliest: &[f64],
    latest: &[f64],
) -> bool {
    let n_routes = routes.len();
    let mut best_gain = 1e-10;
    let mut best_swap: Option<(usize, usize, usize, usize)> = None;

    for ri in 0..n_routes {
        for pi in 0..routes[ri].stops.len() {
            let ci = routes[ri].stops[pi];

            for rj in ri + 1..n_routes {
                for pj in 0..routes[rj].stops.len() {
                    let cj = routes[rj].stops[pj];

                    // Capacity check
                    let new_load_i = routes[ri].load - demands[ci] + demands[cj];
                    let new_load_j = routes[rj].load - demands[cj] + demands[ci];
                    if new_load_i > capacity || new_load_j > capacity { continue; }

                    // Build candidate routes with swap
                    let mut ri_stops = routes[ri].stops.clone();
                    ri_stops[pi] = cj;
                    let mut rj_stops = routes[rj].stops.clone();
                    rj_stops[pj] = ci;

                    // Check feasibility via schedule
                    let ri_sched = match schedule_route(
                        &ri_stops, depot, matrix, service_times, earliest, latest,
                    ) {
                        Some(s) => s,
                        None => continue,
                    };

                    let rj_sched = match schedule_route(
                        &rj_stops, depot, matrix, service_times, earliest, latest,
                    ) {
                        Some(s) => s,
                        None => continue,
                    };

                    // Check max_route_time
                    if let Some(max_t) = max_route_time {
                        if ri_sched.3 > max_t + 1e-10 || rj_sched.3 > max_t + 1e-10 {
                            continue;
                        }
                    }

                    let old_cost = routes[ri].cost + routes[rj].cost;
                    let new_cost = ri_sched.2 + rj_sched.2;
                    let gain = old_cost - new_cost;

                    if gain > best_gain {
                        best_gain = gain;
                        best_swap = Some((ri, pi, rj, pj));
                    }
                }
            }
        }
    }

    if let Some((ri, pi, rj, pj)) = best_swap {
        let ci = routes[ri].stops[pi];
        let cj = routes[rj].stops[pj];

        routes[ri].stops[pi] = cj;
        routes[rj].stops[pj] = ci;

        routes[ri].load = routes[ri].load - demands[ci] + demands[cj];
        routes[rj].load = routes[rj].load - demands[cj] + demands[ci];

        recompute_with_schedule(&mut routes[ri], depot, matrix, service_times, earliest, latest);
        recompute_with_schedule(&mut routes[rj], depot, matrix, service_times, earliest, latest);

        return true;
    }

    false
}

// ---------------------------------------------------------------------------
// Route-time balancing operators (Phase 3)
// ---------------------------------------------------------------------------

/// Find the index and total_time of the route with the highest total_time
fn max_time_route(routes: &[Route]) -> (usize, f64) {
    let mut max_idx = 0;
    let mut max_t = f64::NEG_INFINITY;
    for (i, r) in routes.iter().enumerate() {
        if r.total_time > max_t {
            max_t = r.total_time;
            max_idx = i;
        }
    }
    (max_idx, max_t)
}

/// Relocate a customer from the max-time route to another route to reduce
/// the global max vehicle time. Uses delta estimates to screen candidates,
/// executes the best move, then checks actual cost against budget.
fn balance_relocate(
    routes: &mut Vec<Route>,
    depot: usize,
    matrix: &[Vec<f64>],
    demands: &[f64],
    capacity: f64,
    service_times: &[f64],
    max_route_time: Option<f64>,
    cost_budget: &mut f64,
) -> bool {
    if routes.len() < 2 {
        return false;
    }
    let (heavy_idx, current_max_time) = max_time_route(routes);

    // Compute max time among routes OTHER than the heavy one
    let other_max_time = routes.iter().enumerate()
        .filter(|&(i, _)| i != heavy_idx)
        .map(|(_, r)| r.total_time)
        .fold(f64::NEG_INFINITY, f64::max);

    let mut best_move: Option<(usize, usize, usize)> = None; // (from_pos, to_route, to_pos)
    let mut best_new_max = current_max_time;
    let mut best_cost_delta = f64::MAX;

    for pos in 0..routes[heavy_idx].stops.len() {
        let customer = routes[heavy_idx].stops[pos];
        let cust_demand = demands[customer];

        // Delta estimate: cost of removing from heavy route
        let prev = if pos == 0 { depot } else { routes[heavy_idx].stops[pos - 1] };
        let next = if pos == routes[heavy_idx].stops.len() - 1 {
            depot
        } else {
            routes[heavy_idx].stops[pos + 1]
        };
        let removal_saving =
            matrix[prev][customer] + matrix[customer][next] - matrix[prev][next];

        // Estimated heavy route time after removal
        let heavy_time_est = routes[heavy_idx].total_time - removal_saving - service_times[customer];

        for rj in 0..routes.len() {
            if rj == heavy_idx { continue; }
            if routes[rj].load + cust_demand > capacity { continue; }

            let rj_len = routes[rj].stops.len();
            for insert_pos in 0..=rj_len {
                let before = if insert_pos == 0 { depot }
                    else { routes[rj].stops[insert_pos - 1] };
                let after = if insert_pos == rj_len { depot }
                    else { routes[rj].stops[insert_pos] };

                let insert_cost =
                    matrix[before][customer] + matrix[customer][after]
                    - matrix[before][after];

                // Estimated receiving route time after insertion
                let rj_time_est = routes[rj].total_time + insert_cost + service_times[customer];

                // Screen: max_route_time feasibility
                if let Some(max_t) = max_route_time {
                    if rj_time_est > max_t { continue; }
                }

                // Estimated new global max time
                let new_max_est = heavy_time_est.max(rj_time_est).max(other_max_time);

                // Must strictly reduce max time
                if new_max_est >= current_max_time - 1e-10 { continue; }

                let cost_delta = insert_cost - removal_saving;

                // Pick best: lowest new max time, then lowest cost delta
                if new_max_est < best_new_max - 1e-10
                    || (new_max_est < best_new_max + 1e-10 && cost_delta < best_cost_delta - 1e-10)
                {
                    best_new_max = new_max_est;
                    best_cost_delta = cost_delta;
                    best_move = Some((pos, rj, insert_pos));
                }
            }
        }
    }

    if let Some((from_pos, to_r, to_pos)) = best_move {
        // Save state for potential undo
        let saved_from = routes[heavy_idx].clone();
        let saved_to = routes[to_r].clone();
        let old_cost: f64 = routes.iter().map(|r| r.cost).sum();

        // Execute the move
        let customer = routes[heavy_idx].stops.remove(from_pos);
        routes[heavy_idx].load -= demands[customer];
        routes[heavy_idx].recompute(depot, matrix, service_times);

        routes[to_r].stops.insert(to_pos, customer);
        routes[to_r].load += demands[customer];
        routes[to_r].recompute(depot, matrix, service_times);

        let new_cost: f64 = routes.iter().map(|r| r.cost).sum();
        let actual_cost_delta = new_cost - old_cost;

        // Budget check: only constrain positive cost increases
        if actual_cost_delta > 0.0 && actual_cost_delta > *cost_budget {
            // Undo
            routes[heavy_idx] = saved_from;
            routes[to_r] = saved_to;
            return false;
        }

        // Deduct positive cost increase from budget
        if actual_cost_delta > 0.0 {
            *cost_budget -= actual_cost_delta;
        }

        // Remove empty routes
        routes.retain(|r| !r.stops.is_empty());
        return true;
    }

    false
}

/// Swap a customer from the max-time route with a customer from another route
/// to reduce the global max vehicle time.
fn balance_swap(
    routes: &mut Vec<Route>,
    depot: usize,
    matrix: &[Vec<f64>],
    demands: &[f64],
    capacity: f64,
    service_times: &[f64],
    max_route_time: Option<f64>,
    cost_budget: &mut f64,
) -> bool {
    if routes.len() < 2 {
        return false;
    }
    let (heavy_idx, current_max_time) = max_time_route(routes);

    let mut best_swap: Option<(usize, usize, usize)> = None; // (pos_heavy, other_route, pos_other)
    let mut best_new_max = current_max_time;
    let mut best_cost_delta = f64::MAX;

    for pi in 0..routes[heavy_idx].stops.len() {
        let ci = routes[heavy_idx].stops[pi];

        for rj in 0..routes.len() {
            if rj == heavy_idx { continue; }

            for pj in 0..routes[rj].stops.len() {
                let cj = routes[rj].stops[pj];

                // Capacity check
                let new_load_heavy = routes[heavy_idx].load - demands[ci] + demands[cj];
                let new_load_j = routes[rj].load - demands[cj] + demands[ci];
                if new_load_heavy > capacity || new_load_j > capacity { continue; }

                // Cost deltas for heavy route
                let prev_i = if pi == 0 { depot } else { routes[heavy_idx].stops[pi - 1] };
                let next_i = if pi == routes[heavy_idx].stops.len() - 1 { depot }
                    else { routes[heavy_idx].stops[pi + 1] };
                let old_i = matrix[prev_i][ci] + matrix[ci][next_i];
                let new_i = matrix[prev_i][cj] + matrix[cj][next_i];

                // Cost deltas for other route
                let prev_j = if pj == 0 { depot } else { routes[rj].stops[pj - 1] };
                let next_j = if pj == routes[rj].stops.len() - 1 { depot }
                    else { routes[rj].stops[pj + 1] };
                let old_j = matrix[prev_j][cj] + matrix[cj][next_j];
                let new_j = matrix[prev_j][ci] + matrix[ci][next_j];

                // Estimated times after swap
                let heavy_time_est = routes[heavy_idx].total_time
                    + (new_i - old_i) + (service_times[cj] - service_times[ci]);
                let rj_time_est = routes[rj].total_time
                    + (new_j - old_j) + (service_times[ci] - service_times[cj]);

                // Screen: max_route_time
                if let Some(max_t) = max_route_time {
                    if heavy_time_est > max_t || rj_time_est > max_t { continue; }
                }

                // Compute max across: heavy (rj excluded from other_max since it changes too)
                let other_max_excl_rj = routes.iter().enumerate()
                    .filter(|&(i, _)| i != heavy_idx && i != rj)
                    .map(|(_, r)| r.total_time)
                    .fold(f64::NEG_INFINITY, f64::max);
                let new_max_est = heavy_time_est.max(rj_time_est).max(other_max_excl_rj);

                // Must strictly reduce max time
                if new_max_est >= current_max_time - 1e-10 { continue; }

                let cost_delta = (new_i + new_j) - (old_i + old_j);

                if new_max_est < best_new_max - 1e-10
                    || (new_max_est < best_new_max + 1e-10 && cost_delta < best_cost_delta - 1e-10)
                {
                    best_new_max = new_max_est;
                    best_cost_delta = cost_delta;
                    best_swap = Some((pi, rj, pj));
                }
            }
        }
    }

    if let Some((pi, rj, pj)) = best_swap {
        // Save state for potential undo
        let saved_heavy = routes[heavy_idx].clone();
        let saved_other = routes[rj].clone();
        let old_cost: f64 = routes.iter().map(|r| r.cost).sum();

        let ci = routes[heavy_idx].stops[pi];
        let cj = routes[rj].stops[pj];

        routes[heavy_idx].stops[pi] = cj;
        routes[rj].stops[pj] = ci;

        routes[heavy_idx].load = routes[heavy_idx].load - demands[ci] + demands[cj];
        routes[rj].load = routes[rj].load - demands[cj] + demands[ci];

        routes[heavy_idx].recompute(depot, matrix, service_times);
        routes[rj].recompute(depot, matrix, service_times);

        let new_cost: f64 = routes.iter().map(|r| r.cost).sum();
        let actual_cost_delta = new_cost - old_cost;

        // Budget check
        if actual_cost_delta > 0.0 && actual_cost_delta > *cost_budget {
            // Undo
            routes[heavy_idx] = saved_heavy;
            routes[rj] = saved_other;
            return false;
        }

        if actual_cost_delta > 0.0 {
            *cost_budget -= actual_cost_delta;
        }

        return true;
    }

    false
}

// ---------------------------------------------------------------------------
// Main solver
// ---------------------------------------------------------------------------

pub fn solve(
    cost_matrix: RMatrix<f64>,
    depot: usize,
    demands: &[f64],
    capacity: f64,
    max_vehicles: Option<usize>,
    method: &str,
    service_times: &[f64],
    max_route_time: Option<f64>,
    balance_time: bool,
    earliest: Option<&[f64]>,
    latest: Option<&[f64]>,
) -> List {
    let n = cost_matrix.nrows();
    let flat = cost_matrix.data();

    // Build 2D matrix from column-major R matrix
    let mut matrix = vec![vec![0.0; n]; n];
    for i in 0..n {
        for j in 0..n {
            matrix[i][j] = flat[j * n + i];
        }
    }

    let use_windows = earliest.is_some() && latest.is_some();

    // Phase 1: Construction via Clarke-Wright savings
    let mut routes = clarke_wright(n, depot, &matrix, demands, capacity, max_vehicles, service_times, max_route_time, earliest, latest);

    // Compute initial total cost
    let initial_cost: f64 = routes.iter().map(|r| r.cost).sum();

    // Detect symmetric matrix for fast 2-opt
    let symmetric = super::tsp::is_symmetric(&matrix);

    let mut iterations: i32 = 0;

    if method != "savings" {
        // Phase 2: Improvement
        loop {
            let mut any_improved = false;

            if use_windows {
                let ew = earliest.unwrap();
                let lw = latest.unwrap();

                // Window-aware intra-route operators
                for route in routes.iter_mut() {
                    if intra_route_two_opt_windows(route, depot, &matrix, service_times, ew, lw) {
                        any_improved = true;
                    }
                    if intra_route_or_opt_windows(route, depot, &matrix, service_times, ew, lw) {
                        any_improved = true;
                    }
                }

                // Window-aware inter-route operators
                if inter_route_relocate_windows(&mut routes, depot, &matrix, demands, capacity,
                                                 service_times, max_route_time, ew, lw) {
                    any_improved = true;
                }
                if inter_route_swap_windows(&mut routes, depot, &matrix, demands, capacity,
                                             service_times, max_route_time, ew, lw) {
                    any_improved = true;
                }
            } else {
                // Standard operators (no windows)
                for route in routes.iter_mut() {
                    if intra_route_two_opt(route, depot, &matrix, symmetric, service_times) {
                        any_improved = true;
                    }
                    if intra_route_or_opt(route, depot, &matrix, service_times) {
                        any_improved = true;
                    }
                }

                if inter_route_relocate(&mut routes, depot, &matrix, demands, capacity, service_times, max_route_time) {
                    any_improved = true;
                }
                if inter_route_swap(&mut routes, depot, &matrix, demands, capacity, service_times, max_route_time) {
                    any_improved = true;
                }
            }

            iterations += 1;

            if !any_improved || iterations > 100 {
                break;
            }
        }
    }

    // Post-check: if max_vehicles was requested and we still exceed it,
    // error rather than returning a solution that violates the contract
    if let Some(max_v) = max_vehicles {
        if routes.len() > max_v {
            if max_route_time.is_some() {
                extendr_api::throw_r_error(format!(
                    "Cannot satisfy max_vehicles={} with max_route_time={:.1}: solver needed {} vehicles.",
                    max_v, max_route_time.unwrap(), routes.len()
                ));
            } else {
                extendr_api::throw_r_error(format!(
                    "Cannot satisfy max_vehicles={}: solver needed {} vehicles to respect capacity.",
                    max_v, routes.len()
                ));
            }
        }
    }

    // Phase 3: Route-time balancing (if requested)
    // Skip balancing when time windows are active
    let mut balance_iterations: i32 = 0;
    if balance_time && !use_windows && method != "savings" && routes.len() > 1 {
        let pre_balance_cost: f64 = routes.iter().map(|r| r.cost).sum();
        let mut cost_budget = pre_balance_cost * 0.02;

        loop {
            let mut any_improved = false;

            for route in routes.iter_mut() {
                intra_route_two_opt(route, depot, &matrix, symmetric, service_times);
                intra_route_or_opt(route, depot, &matrix, service_times);
            }

            if balance_relocate(&mut routes, depot, &matrix, demands, capacity,
                                service_times, max_route_time, &mut cost_budget) {
                any_improved = true;
            }

            if !any_improved {
                if balance_swap(&mut routes, depot, &matrix, demands, capacity,
                                service_times, max_route_time, &mut cost_budget) {
                    any_improved = true;
                }
            }

            if !any_improved {
                break;
            }

            balance_iterations += 1;

            if balance_iterations > 50 {
                break;
            }
        }
    }

    // Post-check: if windows are active, verify every route can be scheduled
    if use_windows {
        let ew = earliest.unwrap();
        let lw = latest.unwrap();
        for (v, route) in routes.iter().enumerate() {
            if schedule_route(&route.stops, depot, &matrix, service_times, ew, lw).is_none() {
                extendr_api::throw_r_error(format!(
                    "Vehicle {} route is infeasible under the supplied time windows. \
                     Check that all customers are individually reachable within their windows.",
                    v + 1
                ));
            }
        }
    }

    let total_cost: f64 = routes.iter().map(|r| r.cost).sum();
    let total_time: f64 = routes.iter().map(|r| r.total_time).sum();
    let improvement = if initial_cost > 0.0 {
        (initial_cost - total_cost) / initial_cost * 100.0
    } else {
        0.0
    };

    // Build per-vehicle output
    let n_vehicles = routes.len() as i32;
    let mut all_vehicle_ids: Vec<i32> = vec![0; n];
    let mut all_visit_orders: Vec<i32> = vec![0; n];
    let mut all_arrivals: Vec<f64> = vec![f64::NAN; n];
    let mut all_departures: Vec<f64> = vec![f64::NAN; n];

    // Per-vehicle stats
    let mut vehicle_costs: Vec<f64> = Vec::new();
    let mut vehicle_loads: Vec<f64> = Vec::new();
    let mut vehicle_stops: Vec<i32> = Vec::new();
    let mut vehicle_times: Vec<f64> = Vec::new();

    for (v, route) in routes.iter().enumerate() {
        let vehicle_id = (v + 1) as i32;
        vehicle_costs.push((route.cost * 100.0).round() / 100.0);
        vehicle_loads.push((route.load * 100.0).round() / 100.0);
        vehicle_stops.push(route.stops.len() as i32);
        vehicle_times.push((route.total_time * 100.0).round() / 100.0);

        // Compute final schedule for arrival/departure output
        if use_windows {
            let ew = earliest.unwrap();
            let lw = latest.unwrap();
            if let Some((arrs, deps, _, _)) = schedule_route(
                &route.stops, depot, &matrix, service_times, ew, lw,
            ) {
                for (idx, &stop) in route.stops.iter().enumerate() {
                    all_arrivals[stop] = (arrs[idx] * 100.0).round() / 100.0;
                    all_departures[stop] = (deps[idx] * 100.0).round() / 100.0;
                }
            }
        }

        for (order, &stop) in route.stops.iter().enumerate() {
            all_vehicle_ids[stop] = vehicle_id;
            all_visit_orders[stop] = (order + 1) as i32;
        }
    }

    list!(
        vehicle = all_vehicle_ids,
        visit_order = all_visit_orders,
        n_vehicles = n_vehicles,
        total_cost = (total_cost * 100.0).round() / 100.0,
        total_time = (total_time * 100.0).round() / 100.0,
        initial_cost = (initial_cost * 100.0).round() / 100.0,
        improvement_pct = (improvement * 100.0).round() / 100.0,
        iterations = iterations,
        balance_iterations = balance_iterations,
        vehicle_costs = vehicle_costs,
        vehicle_loads = vehicle_loads,
        vehicle_stops = vehicle_stops,
        vehicle_times = vehicle_times,
        arrival_times = all_arrivals,
        departure_times = all_departures
    )
}
