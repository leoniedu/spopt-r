test_that("tsp returns correct structure", {
  skip_if_not_installed("sf")

  set.seed(42)
  pts <- sf::st_as_sf(
    data.frame(x = runif(8), y = runif(8)),
    coords = c("x", "y")
  )

  result <- route_tsp(pts, depot = 1)

  expect_s3_class(result, "spopt_tsp")
  expect_s3_class(result, "spopt_route")
  expect_true(".visit_order" %in% names(result))
  expect_true(".tour_position" %in% names(result))

  meta <- attr(result, "spopt")
  expect_equal(meta$algorithm, "tsp")
  expect_equal(meta$n_locations, 8)
  expect_equal(meta$depot, 1)
  expect_true(meta$total_cost > 0)
  expect_true(meta$total_cost <= meta$nn_cost)
  expect_equal(meta$tour[1], 1L)          # starts at depot
  expect_equal(meta$tour[length(meta$tour)], 1L)  # ends at depot
  expect_equal(length(meta$tour), 9)      # n + 1 (depot repeated)
})

test_that("tsp handles asymmetric cost matrices correctly", {
  skip_if_not_installed("sf")

  set.seed(42)
  pts <- sf::st_as_sf(
    data.frame(x = runif(6), y = runif(6)),
    coords = c("x", "y")
  )

  # Build asymmetric matrix: d(i,j) != d(j,i)
  n <- 6
  asym <- matrix(runif(n * n, 5, 50), n, n)
  diag(asym) <- 0

  result <- route_tsp(pts, depot = 1, cost_matrix = asym, method = "2-opt")
  meta <- attr(result, "spopt")

  # Tour should be valid
  expect_equal(meta$tour[1], 1L)
  expect_equal(meta$tour[length(meta$tour)], 1L)

  # Verify reported cost matches actual matrix traversal
  tour <- meta$tour
  actual_cost <- 0
  for (i in seq_len(length(tour) - 1)) {
    actual_cost <- actual_cost + asym[tour[i], tour[i + 1]]
  }
  expect_equal(meta$total_cost, round(actual_cost, 2), tolerance = 0.01)
})

test_that("tsp with pre-computed cost matrix", {
  skip_if_not_installed("sf")

  pts <- sf::st_as_sf(
    data.frame(x = c(0, 1, 1, 0), y = c(0, 0, 1, 1)),
    coords = c("x", "y")
  )

  # Square with known optimal tour
  m <- matrix(c(
    0, 1, 1.41, 1,
    1, 0, 1,    1.41,
    1.41, 1, 0, 1,
    1, 1.41, 1, 0
  ), 4, 4)

  result <- route_tsp(pts, depot = 1, cost_matrix = m)
  meta <- attr(result, "spopt")

  # Optimal tour of a square is perimeter = 4
  expect_equal(meta$total_cost, 4.0, tolerance = 0.01)
})

test_that("tsp supports open routes from a fixed start", {
  skip_if_not_installed("sf")

  pts <- sf::st_as_sf(
    data.frame(x = 0:3, y = 0),
    coords = c("x", "y")
  )

  m <- as.matrix(dist(cbind(0:3, 0)))
  result <- route_tsp(pts, start = 1, end = NULL, cost_matrix = m, method = "nn")
  meta <- attr(result, "spopt")

  expect_equal(meta$route_type, "open")
  expect_equal(meta$tour, 1:4)
  expect_equal(meta$total_cost, 3, tolerance = 0.01)
})

test_that("tsp supports fixed start and end paths", {
  skip_if_not_installed("sf")

  pts <- sf::st_as_sf(
    data.frame(x = 0:3, y = 0),
    coords = c("x", "y")
  )

  m <- as.matrix(dist(cbind(0:3, 0)))
  result <- route_tsp(pts, start = 1, end = 4, cost_matrix = m, method = "nn")
  meta <- attr(result, "spopt")

  expect_equal(meta$route_type, "path")
  expect_equal(meta$tour, 1:4)
  expect_equal(meta$total_cost, 3, tolerance = 0.01)
})

test_that("tsp supports time windows and returns schedule columns", {
  skip_if_not_installed("sf")

  pts <- sf::st_as_sf(
    data.frame(x = 0:3, y = 0),
    coords = c("x", "y")
  )

  m <- as.matrix(dist(cbind(0:3, 0)))
  result <- route_tsp(
    pts,
    start = 1,
    end = 4,
    cost_matrix = m,
    method = "2-opt",
    earliest = c(0, 1, 3, 5),
    latest = c(0, 2, 4, 6),
    service_time = c(0, 1, 1, 0)
  )
  meta <- attr(result, "spopt")

  expect_true(meta$has_time_windows)
  expect_equal(meta$tour, 1:4)
  expect_equal(result$.arrival_time[1:4], c(0, 1, 3, 5), tolerance = 0.01)
  expect_equal(result$.departure_time[1:4], c(0, 2, 4, 5), tolerance = 0.01)
})

test_that("vrp returns correct structure", {
  skip_if_not_installed("sf")

  set.seed(42)
  pts <- sf::st_as_sf(
    data.frame(x = runif(11), y = runif(11), demand = c(0, rpois(10, 10))),
    coords = c("x", "y")
  )

  result <- route_vrp(pts, depot = 1, demand_col = "demand", vehicle_capacity = 30)

  expect_s3_class(result, "spopt_vrp")
  expect_s3_class(result, "spopt_route")
  expect_true(".vehicle" %in% names(result))
  expect_true(".visit_order" %in% names(result))

  meta <- attr(result, "spopt")
  expect_equal(meta$algorithm, "vrp")
  expect_true(meta$n_vehicles >= 1)
  expect_true(meta$total_cost > 0)
  expect_equal(length(meta$vehicle_costs), meta$n_vehicles)
  expect_equal(length(meta$vehicle_loads), meta$n_vehicles)

  # All vehicle loads should be within capacity
  expect_true(all(meta$vehicle_loads <= 30))

  # All non-depot locations should be assigned
  expect_true(all(result$.vehicle[-1] > 0))
})

test_that("vrp satisfies feasible n_vehicles even when heuristic initially overshoots", {
  skip_if_not_installed("sf")

  # Demands: 6,5,3,2,2,2 with capacity=10, n_vehicles=2
  # Valid partition: {6,2,2}=10, {5,3,2}=10
  # Clarke-Wright may not find this via merging — tests the re-insertion fallback
  pts <- sf::st_as_sf(
    data.frame(
      x = c(0, 1, 2, 3, 4, 5, 6),
      y = c(0, 0, 0, 0, 0, 0, 0),
      demand = c(0, 6, 5, 3, 2, 2, 2)
    ),
    coords = c("x", "y")
  )

  result <- route_vrp(pts, depot = 1, demand_col = "demand",
                vehicle_capacity = 10, n_vehicles = 2)

  meta <- attr(result, "spopt")
  expect_equal(meta$n_vehicles, 2)
  expect_true(all(meta$vehicle_loads <= 10))
})

test_that("vrp errors on infeasible n_vehicles (demands 8,8,4 cap=10 n=2)", {
  skip_if_not_installed("sf")

  # 8+8=16>10, 8+4=12>10 — every customer needs its own truck
  # No 2-vehicle solution exists. Solver will either return 3 vehicles
  # or drop a customer — both caught by post-check.
  pts <- sf::st_as_sf(
    data.frame(x = c(0, 1, 2, 3), y = c(0, 0, 0, 0), demand = c(0, 8, 8, 4)),
    coords = c("x", "y")
  )

  expect_error(
    route_vrp(pts, depot = 1, demand_col = "demand", vehicle_capacity = 10, n_vehicles = 2),
    "n_vehicles|infeasible|Cannot satisfy|failed to assign"
  )
})

test_that("vrp errors on infeasible n_vehicles (lower bound check)", {
  skip_if_not_installed("sf")

  # total_demand=21, cap=10, ceiling=3 — requesting 1 is clearly infeasible
  pts <- sf::st_as_sf(
    data.frame(x = c(0, 1, 2, 3), y = c(0, 0, 0, 0), demand = c(0, 7, 7, 7)),
    coords = c("x", "y")
  )

  expect_error(
    route_vrp(pts, depot = 1, demand_col = "demand", vehicle_capacity = 10, n_vehicles = 1),
    "Cannot satisfy"
  )
})

test_that("vrp rejects non-positive n_vehicles", {
  skip_if_not_installed("sf")

  pts <- sf::st_as_sf(
    data.frame(x = c(0, 1, 2), y = c(0, 0, 0), demand = c(0, 5, 5)),
    coords = c("x", "y")
  )

  expect_error(
    route_vrp(pts, depot = 1, demand_col = "demand", vehicle_capacity = 10, n_vehicles = 0),
    "positive integer"
  )

  expect_error(
    route_vrp(pts, depot = 1, demand_col = "demand", vehicle_capacity = 10, n_vehicles = -1),
    "positive integer"
  )
})

test_that("vrp rejects demand exceeding capacity", {
  skip_if_not_installed("sf")

  pts <- sf::st_as_sf(
    data.frame(x = c(0, 1, 2), y = c(0, 0, 0), demand = c(0, 50, 5)),
    coords = c("x", "y")
  )

  expect_error(
    route_vrp(pts, depot = 1, demand_col = "demand", vehicle_capacity = 10),
    "exceeds vehicle capacity"
  )
})

test_that("rust_tsp and rust_vrp validate inputs with clean R errors", {
  m <- matrix(c(
    0, 1, 2,
    1, 0, 1,
    2, 1, 0
  ), 3, 3, byrow = TRUE)

  tsp_err <- tryCatch(
    rust_tsp(m, -1L, 0L, "nn", NULL, NULL, NULL),
    error = identity
  )
  vrp_err <- tryCatch(
    rust_vrp(m, -1L, c(0, 1, 1), 10, 1L, "savings", NULL, NULL, FALSE),
    error = identity
  )

  expect_s3_class(tsp_err, "error")
  expect_s3_class(vrp_err, "error")
  expect_match(conditionMessage(tsp_err), "start index|-1")
  expect_match(conditionMessage(vrp_err), "depot index|-1")
  expect_false(grepl("panicked", conditionMessage(tsp_err), fixed = TRUE))
  expect_false(grepl("panicked", conditionMessage(vrp_err), fixed = TRUE))
})

test_that("rust_vrp validates demands length without panicking", {
  m <- matrix(c(
    0, 1, 2,
    1, 0, 1,
    2, 1, 0
  ), 3, 3, byrow = TRUE)

  err <- tryCatch(
    rust_vrp(m, 0L, c(0, 1), 10, NULL, "savings", NULL, NULL, FALSE),
    error = identity
  )
  expect_s3_class(err, "error")
  expect_match(conditionMessage(err), "demands.*length")
  expect_false(grepl("panicked", conditionMessage(err), fixed = TRUE))
})

test_that("rust_vrp errors on infeasible max_vehicles + max_route_time", {
  # Star pattern: depot at center, 6 customers in different directions
  pts_x <- c(0, 5, -5, 5, -5, 0, 0)
  pts_y <- c(0, 5, 5, -5, -5, 5, -5)
  m <- as.matrix(dist(cbind(pts_x, pts_y)))

  # max_route_time=18 with max_vehicles=2: solver can't fit 6 customers in 2 routes
  err <- tryCatch(
    rust_vrp(m, 0L, c(0, 1, 1, 1, 1, 1, 1), 100, 2L, "2-opt",
             rep(0, 7), 18, FALSE),
    error = identity
  )
  expect_s3_class(err, "error")
  expect_match(conditionMessage(err), "max_vehicles|Cannot satisfy")
  expect_false(grepl("panicked", conditionMessage(err), fixed = TRUE))
})

# ---- VRP time constraints ----

test_that("vrp vehicle_time >= vehicle_cost, equality when service_time is zero", {
  skip_if_not_installed("sf")

  set.seed(42)
  pts <- sf::st_as_sf(
    data.frame(x = runif(11), y = runif(11), demand = c(0, rpois(10, 10))),
    coords = c("x", "y")
  )

  # Without service_time: times should equal costs
  result <- route_vrp(pts, depot = 1, demand_col = "demand", vehicle_capacity = 30)
  meta <- attr(result, "spopt")
  expect_equal(meta$vehicle_times, meta$vehicle_costs)
  expect_equal(meta$total_time, meta$total_cost)

  # With service_time: times >= costs for every route
  result2 <- route_vrp(pts, depot = 1, demand_col = "demand", vehicle_capacity = 30,
                        service_time = c(0, rep(2, 10)))
  meta2 <- attr(result2, "spopt")
  for (i in seq_along(meta2$vehicle_times)) {
    expect_true(meta2$vehicle_times[i] >= meta2$vehicle_costs[i])
  }
})

test_that("vrp service_time without max_route_time increases vehicle_times", {
  skip_if_not_installed("sf")

  pts <- sf::st_as_sf(
    data.frame(x = c(0, 1, 2, 3, 4), y = c(0, 0, 0, 0, 0),
               demand = c(0, 5, 5, 5, 5)),
    coords = c("x", "y")
  )

  result_no_svc <- route_vrp(pts, depot = 1, demand_col = "demand", vehicle_capacity = 100)
  result_svc <- route_vrp(pts, depot = 1, demand_col = "demand", vehicle_capacity = 100,
                           service_time = c(0, 3, 3, 3, 3))

  meta_no <- attr(result_no_svc, "spopt")
  meta_svc <- attr(result_svc, "spopt")

  # Costs should be the same (service doesn't affect travel cost)
  expect_equal(meta_svc$total_cost, meta_no$total_cost)
  # Times should be higher with service
  expect_true(meta_svc$total_time > meta_no$total_time)
})

test_that("vrp max_route_time forces splitting when capacity is loose", {
  skip_if_not_installed("sf")

  # Star pattern: depot at center, 4 customers in different directions
  # Each round trip = 6, but visiting all = ~22 (must detour between customers)
  pts <- sf::st_as_sf(
    data.frame(x = c(0, 3, 0, -3, 0), y = c(0, 0, 3, 0, -3),
               demand = c(0, 1, 1, 1, 1)),
    coords = c("x", "y")
  )
  m <- as.matrix(dist(cbind(c(0, 3, 0, -3, 0), c(0, 0, 3, 0, -3))))

  # max round trip = 6 (all same dist), so max_route_time=10 allows each
  # All 4 in one route costs ~22, way over 10
  result_no_limit <- route_vrp(pts, depot = 1, demand_col = "demand",
                                vehicle_capacity = 100, cost_matrix = m)
  result_limited <- route_vrp(pts, depot = 1, demand_col = "demand",
                               vehicle_capacity = 100, cost_matrix = m,
                               max_route_time = 10)

  meta_no <- attr(result_no_limit, "spopt")
  meta_lim <- attr(result_limited, "spopt")

  expect_equal(meta_no$n_vehicles, 1)
  expect_true(meta_lim$n_vehicles > meta_no$n_vehicles)
  # All limited routes should be within time
  for (t in meta_lim$vehicle_times) {
    expect_true(t <= 10 + 1e-6)
  }
})

test_that("vrp max_route_time + service_time combined pushes over limit", {
  skip_if_not_installed("sf")

  # 2 customers at distance 1 from depot, capacity is loose
  # Without service: round trip each is 2, both in one route = 2 (depot->1->2->depot ≈ 2)
  # With service_time=5 each and max_route_time=10:
  # one route: travel~2 + service=10 = 12 > 10, must split
  pts <- sf::st_as_sf(
    data.frame(x = c(0, 1, -1), y = c(0, 0, 0),
               demand = c(0, 1, 1)),
    coords = c("x", "y")
  )
  m <- as.matrix(dist(cbind(c(0, 1, -1), 0)))

  result <- route_vrp(pts, depot = 1, demand_col = "demand",
                       vehicle_capacity = 100, cost_matrix = m,
                       service_time = c(0, 5, 5), max_route_time = 10)
  meta <- attr(result, "spopt")

  # Should need 2 vehicles since 1 route would be travel(2) + service(10) = 12 > 10
  expect_equal(meta$n_vehicles, 2)
  for (t in meta$vehicle_times) {
    expect_true(t <= 10 + 1e-6)
  }
})

test_that("vrp service_time as column name works", {
  skip_if_not_installed("sf")

  pts <- sf::st_as_sf(
    data.frame(x = c(0, 1, 2, 3), y = c(0, 0, 0, 0),
               demand = c(0, 5, 5, 5), svc = c(0, 3, 3, 3)),
    coords = c("x", "y")
  )

  result <- route_vrp(pts, depot = 1, demand_col = "demand",
                       vehicle_capacity = 100, service_time = "svc")
  meta <- attr(result, "spopt")

  expect_true(meta$has_service_time)
  expect_true(meta$total_time > meta$total_cost)
})

test_that("vrp errors on infeasible max_route_time (single customer too far)", {
  skip_if_not_installed("sf")

  pts <- sf::st_as_sf(
    data.frame(x = c(0, 100, 1), y = c(0, 0, 0),
               demand = c(0, 1, 1)),
    coords = c("x", "y")
  )
  m <- as.matrix(dist(cbind(c(0, 100, 1), 0)))

  expect_error(
    route_vrp(pts, depot = 1, demand_col = "demand",
              vehicle_capacity = 100, cost_matrix = m,
              max_route_time = 5),
    "unreachable"
  )
})

test_that("vrp errors on negative service_time", {
  skip_if_not_installed("sf")

  pts <- sf::st_as_sf(
    data.frame(x = c(0, 1, 2), y = c(0, 0, 0), demand = c(0, 5, 5)),
    coords = c("x", "y")
  )

  expect_error(
    route_vrp(pts, depot = 1, demand_col = "demand",
              vehicle_capacity = 100, service_time = c(0, -1, 3)),
    "non-negative"
  )
})

test_that("vrp n_vehicles + max_route_time infeasibility caught by post-check", {
  skip_if_not_installed("sf")

  # 6 customers in a star pattern, each at distance 5 from depot
  # Each round-trip = 10, so max_route_time=11 allows each individually
  # But n_vehicles=2 means 3 customers per route, which takes ~25+ (star detours)
  # => 3+ vehicles needed but only 2 allowed
  pts <- sf::st_as_sf(
    data.frame(
      x = c(0, 5, -5, 5, -5, 0, 0),
      y = c(0, 5, 5, -5, -5, 5, -5),
      demand = c(0, 1, 1, 1, 1, 1, 1)
    ),
    coords = c("x", "y")
  )
  m <- as.matrix(dist(cbind(c(0, 5, -5, 5, -5, 0, 0), c(0, 5, 5, -5, -5, 5, -5))))

  expect_error(
    route_vrp(pts, depot = 1, demand_col = "demand",
              vehicle_capacity = 100, cost_matrix = m,
              n_vehicles = 2, max_route_time = 18),
    "n_vehicles|Cannot satisfy|failed to assign"
  )
})

# ---- VRP balancing ----

test_that("vrp balance='time' reduces max vehicle time", {
  skip_if_not_installed("sf")

  # Asymmetric problem: cluster of nearby stops + 1 far stop
  # Cost optimizer puts the far stop alone, creating imbalance
  pts <- sf::st_as_sf(
    data.frame(
      x = c(0, 1, 1.5, 2, 2.5, 10),
      y = c(0, 0, 0.5, 0, 0.5, 0),
      demand = c(0, 5, 5, 5, 5, 5)
    ),
    coords = c("x", "y")
  )
  m <- as.matrix(dist(cbind(c(0, 1, 1.5, 2, 2.5, 10), c(0, 0, 0.5, 0, 0.5, 0))))

  result_no <- route_vrp(pts, depot = 1, demand_col = "demand",
                          vehicle_capacity = 15, n_vehicles = 2,
                          cost_matrix = m)
  result_bal <- route_vrp(pts, depot = 1, demand_col = "demand",
                           vehicle_capacity = 15, n_vehicles = 2,
                           cost_matrix = m, balance = "time")

  meta_no <- attr(result_no, "spopt")
  meta_bal <- attr(result_bal, "spopt")

  # Balanced max vehicle time should be <= unbalanced
  expect_true(max(meta_bal$vehicle_times) <= max(meta_no$vehicle_times) + 1e-6)
})

test_that("vrp balance='time' no-regression invariant", {
  skip_if_not_installed("sf")

  set.seed(42)
  pts <- sf::st_as_sf(
    data.frame(x = runif(11), y = runif(11),
               demand = c(0, rpois(10, 8))),
    coords = c("x", "y")
  )

  result_no <- route_vrp(pts, depot = 1, demand_col = "demand",
                          vehicle_capacity = 25,
                          service_time = c(0, rep(2, 10)),
                          max_route_time = 20)
  result_bal <- route_vrp(pts, depot = 1, demand_col = "demand",
                           vehicle_capacity = 25,
                           service_time = c(0, rep(2, 10)),
                           max_route_time = 20,
                           balance = "time")

  meta_no <- attr(result_no, "spopt")
  meta_bal <- attr(result_bal, "spopt")

  # All customers assigned
  expect_true(all(result_bal$.vehicle[-1] > 0))
  # All loads within capacity
  expect_true(all(meta_bal$vehicle_loads <= 25))
  # All times within max_route_time
  for (t in meta_bal$vehicle_times) {
    expect_true(t <= 20 + 1e-6)
  }
  # n_vehicles does not increase
  expect_true(meta_bal$n_vehicles <= meta_no$n_vehicles)
})

test_that("vrp balance='time' works without service_time", {
  skip_if_not_installed("sf")

  set.seed(42)
  pts <- sf::st_as_sf(
    data.frame(x = runif(11), y = runif(11),
               demand = c(0, rpois(10, 10))),
    coords = c("x", "y")
  )

  result <- route_vrp(pts, depot = 1, demand_col = "demand",
                       vehicle_capacity = 30, balance = "time")
  meta <- attr(result, "spopt")

  expect_equal(meta$balance, "time")
  expect_true(is.numeric(meta$balance_iterations))
  # time == cost when no service time
  expect_equal(meta$vehicle_times, meta$vehicle_costs)
})

test_that("vrp balance metadata is populated", {
  skip_if_not_installed("sf")

  set.seed(42)
  pts <- sf::st_as_sf(
    data.frame(x = runif(8), y = runif(8), demand = c(0, rep(5, 7))),
    coords = c("x", "y")
  )

  result <- route_vrp(pts, depot = 1, demand_col = "demand",
                       vehicle_capacity = 20, balance = "time")
  meta <- attr(result, "spopt")

  expect_equal(meta$balance, "time")
  expect_true(is.numeric(meta$balance_iterations))
  expect_true(meta$balance_iterations >= 0)

  # Without balance, balance should be NULL
  result_no <- route_vrp(pts, depot = 1, demand_col = "demand",
                          vehicle_capacity = 20)
  meta_no <- attr(result_no, "spopt")
  expect_null(meta_no$balance)
  expect_equal(meta_no$balance_iterations, 0)
})

test_that("vrp balance='time' cost increase is bounded", {
  skip_if_not_installed("sf")

  set.seed(42)
  pts <- sf::st_as_sf(
    data.frame(x = runif(16), y = runif(16),
               demand = c(0, rpois(15, 8))),
    coords = c("x", "y")
  )

  result_no <- route_vrp(pts, depot = 1, demand_col = "demand",
                          vehicle_capacity = 30)
  result_bal <- route_vrp(pts, depot = 1, demand_col = "demand",
                           vehicle_capacity = 30, balance = "time")

  meta_no <- attr(result_no, "spopt")
  meta_bal <- attr(result_bal, "spopt")

  # Cost should not increase by more than ~2.5% (2% budget + rounding tolerance)
  cost_increase_pct <- (meta_bal$total_cost - meta_no$total_cost) / meta_no$total_cost * 100
  expect_true(cost_increase_pct <= 2.5)
})

# ---- Print / summary methods ----

test_that("print.spopt_tsp returns invisibly with correct class", {
  skip_if_not_installed("sf")

  pts <- sf::st_as_sf(
    data.frame(x = c(0, 1, 1, 0), y = c(0, 0, 1, 1)),
    coords = c("x", "y")
  )

  result <- route_tsp(pts, depot = 1)
  out <- expect_output(ret <- print(result), "TSP route")
  expect_s3_class(ret, "spopt_tsp")
})

test_that("print.spopt_vrp returns invisibly with correct class", {
  skip_if_not_installed("sf")

  pts <- sf::st_as_sf(
    data.frame(x = c(0, 1, 2, 3), y = c(0, 0, 0, 0), demand = c(0, 5, 5, 5)),
    coords = c("x", "y")
  )

  result <- route_vrp(pts, depot = 1, demand_col = "demand", vehicle_capacity = 10)
  out <- expect_output(ret <- print(result), "VRP routes")
  expect_s3_class(ret, "spopt_vrp")
})

test_that("summary.spopt_tsp shows tour sequence", {
  skip_if_not_installed("sf")

  pts <- sf::st_as_sf(
    data.frame(x = c(0, 1, 1, 0), y = c(0, 0, 1, 1)),
    coords = c("x", "y")
  )

  result <- route_tsp(pts, depot = 1)
  expect_output(summary(result), "Tour sequence")
})

test_that("summary.spopt_vrp shows per-vehicle table", {
  skip_if_not_installed("sf")

  pts <- sf::st_as_sf(
    data.frame(x = c(0, 1, 2, 3), y = c(0, 0, 0, 0), demand = c(0, 5, 5, 5)),
    coords = c("x", "y")
  )

  result <- route_vrp(pts, depot = 1, demand_col = "demand", vehicle_capacity = 10)
  expect_output(summary(result), "Per-vehicle summary")
})

test_that("summary.spopt_vrp shows Time column when service_time is set", {
  skip_if_not_installed("sf")

  pts <- sf::st_as_sf(
    data.frame(x = c(0, 1, 2, 3, 4), y = c(0, 0, 0, 0, 0),
               demand = c(0, 5, 5, 5, 5)),
    coords = c("x", "y")
  )

  result <- route_vrp(pts, depot = 1, demand_col = "demand",
                       vehicle_capacity = 100,
                       service_time = c(0, 3, 3, 3, 3))
  expect_output(summary(result), "Time")
})

# ---- VRP or-opt ----

test_that("vrp local search (2-opt + or-opt) improves on savings construction", {
  skip_if_not_installed("sf")

  set.seed(99)
  pts <- sf::st_as_sf(
    data.frame(x = runif(16), y = runif(16),
               demand = c(0, rpois(15, 8))),
    coords = c("x", "y")
  )

  # method="savings" is construction only; method="2-opt" adds
  # intra-route 2-opt + or-opt and inter-route relocate + swap.
  # Can't isolate or-opt from 2-opt via the R API, but we verify
  # the full local search pipeline improves on the construction baseline.
  result_savings <- route_vrp(pts, depot = 1, demand_col = "demand",
                               vehicle_capacity = 30, method = "savings")
  result_opt <- route_vrp(pts, depot = 1, demand_col = "demand",
                           vehicle_capacity = 30, method = "2-opt")

  meta_savings <- attr(result_savings, "spopt")
  meta_opt <- attr(result_opt, "spopt")

  # Local search should be at least as good as savings-only
  expect_true(meta_opt$total_cost <= meta_savings$total_cost + 1e-6)
  # And improvement_pct should be non-negative
  expect_true(meta_opt$improvement_pct >= 0)
})
