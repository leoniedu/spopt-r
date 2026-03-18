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
    rust_vrp(m, -1L, c(0, 1, 1), 10, 1L, "savings", NULL, NULL, FALSE, NULL, NULL),
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
    rust_vrp(m, 0L, c(0, 1), 10, NULL, "savings", NULL, NULL, FALSE, NULL, NULL),
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
             rep(0, 7), 18, FALSE, NULL, NULL),
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

# ---- VRP time windows ----

test_that("vrp basic time windows: arrivals within windows", {
  skip_if_not_installed("sf")

  # Depot at 0, 4 customers in a line
  pts <- sf::st_as_sf(
    data.frame(x = c(0, 1, 2, 3, 4), y = rep(0, 5),
               demand = c(0, 5, 5, 5, 5)),
    coords = c("x", "y")
  )
  m <- as.matrix(dist(cbind(c(0, 1, 2, 3, 4), 0)))

  result <- route_vrp(pts, depot = 1, demand_col = "demand",
                       vehicle_capacity = 100, cost_matrix = m,
                       earliest = c(0, 0, 0, 0, 0),
                       latest = c(100, 10, 10, 10, 10))
  meta <- attr(result, "spopt")

  expect_true(meta$has_time_windows)
  # All arrivals should be within windows
  for (i in 2:5) {
    if (!is.na(result$.arrival_time[i])) {
      expect_true(result$.arrival_time[i] >= 0 - 1e-6)
      expect_true(result$.arrival_time[i] <= 10 + 1e-6)
    }
  }
  # Depot should be NA
  expect_true(is.na(result$.arrival_time[1]))
})

test_that("vrp method=savings with windows respects feasibility on merges", {
  skip_if_not_installed("sf")

  # Depot at 0, customer 2 at x=1 (window 10-12), customer 3 at x=2 (window 0-3)
  # Each individually feasible. But merged route [2,3]:
  # depart depot at 0, travel 1 to cust2, wait until 10, service, depart 10,
  # travel 1 to cust3, arrive 11 > latest 3 -> infeasible
  # So savings-only must keep them on separate routes
  pts <- sf::st_as_sf(
    data.frame(x = c(0, 1, 2), y = rep(0, 3),
               demand = c(0, 1, 1)),
    coords = c("x", "y")
  )
  m <- as.matrix(dist(cbind(c(0, 1, 2), 0)))

  result <- route_vrp(pts, depot = 1, demand_col = "demand",
                       vehicle_capacity = 100, cost_matrix = m,
                       method = "savings",
                       earliest = c(0, 10, 0),
                       latest = c(100, 12, 3))
  meta <- attr(result, "spopt")

  # Must use 2 vehicles because the merge is infeasible
  expect_equal(meta$n_vehicles, 2)
  # All arrivals should be within windows
  for (i in 2:3) {
    if (!is.na(result$.arrival_time[i])) {
      expect_true(result$.arrival_time[i] <= c(12, 3)[i - 1] + 1e-6)
    }
  }
})

test_that("vrp windows + max_route_time catches waiting-induced infeasibility on merge", {
  skip_if_not_installed("sf")

  # Two customers, each individually feasible:
  # Cust 2: dist 1, window 3-10, alone: depart 0, travel 1, wait until 3, return 1 = route_time 4
  # Cust 3: dist 1, window 3-10, alone: depart 0, travel 1, wait until 3, return 1 = route_time 4
  # max_route_time = 5: both individually ok (4 <= 5)
  # Merged [2, 3]: depart 0, travel 1 to cust2, wait until 3, depart 3,
  #   travel 2 to cust3, arrive 5, service_start = max(5, 3) = 5, depart 5,
  #   travel 1 to depot = 6. route_time = 6 > 5.
  # So merge is infeasible due to cumulative waiting.
  pts <- sf::st_as_sf(
    data.frame(x = c(0, 1, -1), y = rep(0, 3),
               demand = c(0, 1, 1)),
    coords = c("x", "y")
  )
  m <- as.matrix(dist(cbind(c(0, 1, -1), 0)))

  result <- route_vrp(pts, depot = 1, demand_col = "demand",
                       vehicle_capacity = 100, cost_matrix = m,
                       earliest = c(0, 3, 3),
                       latest = c(100, 10, 10),
                       max_route_time = 5)
  meta <- attr(result, "spopt")

  # Must use 2 vehicles because merged route exceeds max_route_time with waiting
  expect_equal(meta$n_vehicles, 2)
})

test_that("vrp infeasible time window errors", {
  skip_if_not_installed("sf")

  pts <- sf::st_as_sf(
    data.frame(x = c(0, 10, 1), y = rep(0, 3),
               demand = c(0, 1, 1)),
    coords = c("x", "y")
  )
  m <- as.matrix(dist(cbind(c(0, 10, 1), 0)))

  # Customer 2 at distance 10 but window closes at 5
  expect_error(
    route_vrp(pts, depot = 1, demand_col = "demand",
              vehicle_capacity = 100, cost_matrix = m,
              earliest = c(0, 0, 0),
              latest = c(100, 5, 100)),
    "infeasible"
  )
})

test_that("vrp windows + capacity both respected", {
  skip_if_not_installed("sf")

  pts <- sf::st_as_sf(
    data.frame(x = c(0, 1, 2, 3, 4), y = rep(0, 5),
               demand = c(0, 10, 10, 10, 10)),
    coords = c("x", "y")
  )
  m <- as.matrix(dist(cbind(c(0, 1, 2, 3, 4), 0)))

  result <- route_vrp(pts, depot = 1, demand_col = "demand",
                       vehicle_capacity = 20, cost_matrix = m,
                       earliest = c(0, 0, 0, 0, 0),
                       latest = c(100, 50, 50, 50, 50))
  meta <- attr(result, "spopt")

  # Capacity respected
  expect_true(all(meta$vehicle_loads <= 20))
  # All customers assigned
  expect_true(all(result$.vehicle[-1] > 0))
})

test_that("vrp windows + max_route_time with waiting forces split", {
  skip_if_not_installed("sf")

  # Customer at distance 1, but window doesn't open until t=10
  # Round trip = 2 travel + 10 waiting = 12 total
  # With max_route_time = 15, one route can handle 1 customer + waiting
  # Two customers both needing waiting: 2 travel + 10 wait + 1 travel + 10 wait + 2 return
  # That's way over 15, so must split
  pts <- sf::st_as_sf(
    data.frame(x = c(0, 1, -1), y = rep(0, 3),
               demand = c(0, 1, 1)),
    coords = c("x", "y")
  )
  m <- as.matrix(dist(cbind(c(0, 1, -1), 0)))

  result <- route_vrp(pts, depot = 1, demand_col = "demand",
                       vehicle_capacity = 100, cost_matrix = m,
                       earliest = c(0, 10, 10),
                       latest = c(100, 20, 20),
                       max_route_time = 15)
  meta <- attr(result, "spopt")

  # Should need 2 vehicles because of waiting
  expect_equal(meta$n_vehicles, 2)
})

test_that("vrp waiting-only infeasibility caught by pre-check", {
  skip_if_not_installed("sf")

  # Customer 2 at distance 1 (round trip travel = 2)
  # But window opens at t=10, so waiting = 10
  # Route time = 10 wait + 0 service + 1 return = 12 total
  # max_route_time = 5: travel alone is fine (2), but waiting makes it 12
  # Need 3 locations for VRP (depot + 2 customers)
  pts <- sf::st_as_sf(
    data.frame(x = c(0, 1, 0.5), y = c(0, 0, 0),
               demand = c(0, 1, 1)),
    coords = c("x", "y")
  )
  m <- as.matrix(dist(cbind(c(0, 1, 0.5), 0)))

  expect_error(
    route_vrp(pts, depot = 1, demand_col = "demand",
              vehicle_capacity = 100, cost_matrix = m,
              earliest = c(0, 10, 0),
              latest = c(100, 20, 100),
              max_route_time = 5),
    "unreachable|route time"
  )
})

test_that("vrp windows + balance produces warning and is ignored", {
  skip_if_not_installed("sf")

  pts <- sf::st_as_sf(
    data.frame(x = c(0, 1, 2, 3), y = rep(0, 4),
               demand = c(0, 5, 5, 5)),
    coords = c("x", "y")
  )
  m <- as.matrix(dist(cbind(c(0, 1, 2, 3), 0)))

  expect_warning(
    result <- route_vrp(pts, depot = 1, demand_col = "demand",
                         vehicle_capacity = 100, cost_matrix = m,
                         earliest = c(0, 0, 0, 0),
                         latest = c(100, 50, 50, 50),
                         balance = "time"),
    "balance is ignored"
  )

  meta <- attr(result, "spopt")
  expect_null(meta$balance)
})

test_that("vrp arrival/departure output with windows", {
  skip_if_not_installed("sf")

  pts <- sf::st_as_sf(
    data.frame(x = c(0, 1, 2, 3), y = rep(0, 4),
               demand = c(0, 5, 5, 5)),
    coords = c("x", "y")
  )
  m <- as.matrix(dist(cbind(c(0, 1, 2, 3), 0)))

  result <- route_vrp(pts, depot = 1, demand_col = "demand",
                       vehicle_capacity = 100, cost_matrix = m,
                       earliest = c(0, 0, 0, 0),
                       latest = c(100, 50, 50, 50),
                       service_time = c(0, 2, 2, 2))

  # Columns exist
  expect_true(".arrival_time" %in% names(result))
  expect_true(".departure_time" %in% names(result))
  # Depot is NA
  expect_true(is.na(result$.arrival_time[1]))
  expect_true(is.na(result$.departure_time[1]))
  # Non-depot arrivals are within windows
  for (i in 2:4) {
    expect_true(result$.arrival_time[i] >= 0 - 1e-6)
    expect_true(result$.arrival_time[i] <= 50 + 1e-6)
  }
})

test_that("rust_vrp validates bad earliest/latest input without panicking", {
  m <- matrix(c(
    0, 1, 2,
    1, 0, 1,
    2, 1, 0
  ), 3, 3, byrow = TRUE)

  # Wrong length
  err1 <- tryCatch(
    rust_vrp(m, 0L, c(0, 1, 1), 10, NULL, "savings", NULL, NULL, FALSE,
             c(0, 0), c(100, 100)),
    error = identity
  )
  expect_s3_class(err1, "error")
  expect_match(conditionMessage(err1), "earliest.*length")
  expect_false(grepl("panicked", conditionMessage(err1), fixed = TRUE))

  # earliest > latest
  err2 <- tryCatch(
    rust_vrp(m, 0L, c(0, 1, 1), 10, NULL, "savings", NULL, NULL, FALSE,
             c(0, 10, 0), c(100, 5, 100)),
    error = identity
  )
  expect_s3_class(err2, "error")
  expect_match(conditionMessage(err2), "earliest.*greater")
  expect_false(grepl("panicked", conditionMessage(err2), fixed = TRUE))

  # Individually infeasible customer (window too tight for travel distance)
  err3 <- tryCatch(
    rust_vrp(m, 0L, c(0, 1, 1), 10, NULL, "savings", NULL, NULL, FALSE,
             c(0, 10, 0), c(100, 12, 1)),
    error = identity
  )
  expect_s3_class(err3, "error")
  expect_match(conditionMessage(err3), "infeasible")
  expect_false(grepl("panicked", conditionMessage(err3), fixed = TRUE))
})
