test_that("orce returns correct structure", {
  skip_if_not_installed("sf")
  skip_if_not(is.loaded("wrap__rust_orce"), "Rust compilation required")

  set.seed(42)
  demand <- sf::st_as_sf(
    data.frame(x = runif(20), y = runif(20), workload = rpois(20, 20)),
    coords = c("x", "y")
  )
  facilities <- sf::st_as_sf(
    data.frame(
      x = runif(5), y = runif(5),
      fixed_cost = rep(100, 5),
      max_workers = rep(5L, 5)
    ),
    coords = c("x", "y")
  )

  cost <- distance_matrix(demand, facilities)

  result <- orce(demand, facilities,
    weight_col = "workload", cost_matrix = cost,
    facility_cost_col = "fixed_cost", worker_cost = 50,
    worker_capacity = 100, max_workers_col = "max_workers"
  )

  # Structure
  expect_type(result, "list")
  expect_s3_class(result, "spopt_orce")
  expect_s3_class(result$demand, "sf")
  expect_s3_class(result$facilities, "sf")

  # Demand columns
  expect_true(".facility" %in% names(result$demand))
  expect_true(all(result$demand$.facility >= 1))
  expect_true(all(result$demand$.facility <= nrow(facilities)))

  # Facility columns
  expect_true(".selected" %in% names(result$facilities))
  expect_true(".n_assigned" %in% names(result$facilities))
  expect_true(".workers" %in% names(result$facilities))
  expect_true(".utilization" %in% names(result$facilities))

  # Workers are 0 for closed facilities
  closed <- !result$facilities$.selected
  expect_true(all(result$facilities$.workers[closed] == 0L))

  # Workers >= min_workers for open facilities
  open <- result$facilities$.selected
  expect_true(all(result$facilities$.workers[open] >= 1L))

  # Metadata
  meta <- attr(result, "spopt")
  expect_equal(meta$algorithm, "orce")
  expect_true(meta$n_selected > 0)
  expect_true(meta$objective > 0)
  expect_true(meta$solve_time >= 0)
})

test_that("orce cost decomposition sums to objective", {
  skip_if_not_installed("sf")
  skip_if_not(is.loaded("wrap__rust_orce"), "Rust compilation required")

  set.seed(42)
  demand <- sf::st_as_sf(
    data.frame(x = runif(15), y = runif(15), workload = rpois(15, 20)),
    coords = c("x", "y")
  )
  facilities <- sf::st_as_sf(
    data.frame(
      x = runif(4), y = runif(4),
      fixed_cost = c(100, 200, 150, 300),
      max_workers = c(3L, 4L, 3L, 5L)
    ),
    coords = c("x", "y")
  )

  cost <- distance_matrix(demand, facilities)

  result <- orce(demand, facilities,
    weight_col = "workload", cost_matrix = cost,
    facility_cost_col = "fixed_cost", worker_cost = 50,
    worker_capacity = 100, max_workers_col = "max_workers"
  )

  meta <- attr(result, "spopt")
  cost_sum <- meta$transport_cost + meta$facility_cost + meta$worker_cost_total
  expect_equal(meta$objective, cost_sum, tolerance = 1e-6)
})

test_that("orce errors on infeasible problem", {
  skip_if_not_installed("sf")
  skip_if_not(is.loaded("wrap__rust_orce"), "Rust compilation required")

  demand <- sf::st_as_sf(
    data.frame(x = c(0, 1), y = c(0, 1), workload = c(500, 500)),
    coords = c("x", "y")
  )
  facilities <- sf::st_as_sf(
    data.frame(
      x = 0.5, y = 0.5,
      fixed_cost = 100,
      max_workers = 1L
    ),
    coords = c("x", "y")
  )

  cost <- distance_matrix(demand, facilities)

  # 1 worker * 100 capacity = 100 < 1000 total demand
  expect_error(
    orce(demand, facilities,
      weight_col = "workload", cost_matrix = cost,
      facility_cost_col = "fixed_cost", worker_cost = 50,
      worker_capacity = 100, max_workers_col = "max_workers"
    ),
    "infeasible"
  )
})

test_that("orce validates inputs", {
  skip_if_not_installed("sf")

  demand <- sf::st_as_sf(
    data.frame(x = 1, y = 1, workload = 10),
    coords = c("x", "y")
  )
  facilities <- sf::st_as_sf(
    data.frame(x = 1.1, y = 1.1, fixed_cost = 100, max_workers = 5L),
    coords = c("x", "y")
  )
  cost <- matrix(0.1, nrow = 1, ncol = 1)

  # Missing column
  expect_error(
    orce(demand, facilities, weight_col = "nope", cost_matrix = cost,
         facility_cost_col = "fixed_cost", worker_cost = 50,
         worker_capacity = 100, max_workers_col = "max_workers"),
    "not found"
  )

  # Bad worker_cost
  expect_error(
    orce(demand, facilities, weight_col = "workload", cost_matrix = cost,
         facility_cost_col = "fixed_cost", worker_cost = -1,
         worker_capacity = 100, max_workers_col = "max_workers"),
    "positive"
  )

  # Wrong cost_matrix dimensions
  expect_error(
    orce(demand, facilities, weight_col = "workload",
         cost_matrix = matrix(1, 2, 2),
         facility_cost_col = "fixed_cost", worker_cost = 50,
         worker_capacity = 100, max_workers_col = "max_workers"),
    "matrix"
  )
})

test_that("orce warns on unopenable facilities", {
  skip_if_not_installed("sf")
  skip_if_not(is.loaded("wrap__rust_orce"), "Rust compilation required")

  demand <- sf::st_as_sf(
    data.frame(x = 0.5, y = 0.5, workload = 10),
    coords = c("x", "y")
  )
  facilities <- sf::st_as_sf(
    data.frame(
      x = c(0, 1), y = c(0, 1),
      fixed_cost = c(100, 100),
      max_workers = c(5L, 0L)   # second facility can never open (0 < min_workers=1)
    ),
    coords = c("x", "y")
  )
  cost <- distance_matrix(demand, facilities)

  expect_warning(
    orce(demand, facilities,
      weight_col = "workload", cost_matrix = cost,
      facility_cost_col = "fixed_cost", worker_cost = 50,
      worker_capacity = 100, max_workers_col = "max_workers"
    ),
    "cannot be opened"
  )
})

test_that("orce matches orce package results", {
  skip_if_not_installed("sf")
  skip_if_not_installed("orce")
  skip_if_not(is.loaded("wrap__rust_orce"), "Rust compilation required")

  # This test sets up an identical problem in both spopt::orce() and
  # orce::orce() and verifies they produce the same objective value.
  # The exact setup depends on the orce package API — this is a
  # placeholder that should be filled in with a concrete example
  # once the orce package interface is confirmed.
  skip("TODO: implement comparison test with orce package")
})
