test_that("p_median returns correct structure", {
  skip_if_not_installed("sf")
  skip_if_not(is.loaded("wrap__rust_p_median"), "Rust compilation required")

  set.seed(42)
  demand <- sf::st_as_sf(
    data.frame(x = runif(30), y = runif(30), pop = rpois(30, 100)),
    coords = c("x", "y")
  )
  facilities <- sf::st_as_sf(
    data.frame(x = runif(10), y = runif(10)),
    coords = c("x", "y")
  )

  result <- p_median(demand, facilities, n_facilities = 3, weight_col = "pop")

  expect_type(result, "list")
  expect_s3_class(result$demand, "sf")
  expect_s3_class(result$facilities, "sf")
  expect_true(".facility" %in% names(result$demand))
  expect_true(".selected" %in% names(result$facilities))
  expect_equal(sum(result$facilities$.selected), 3)
})

test_that("p_median assigns all demand to selected facilities", {
  skip_if_not_installed("sf")
  skip_if_not(is.loaded("wrap__rust_p_median"), "Rust compilation required")

  set.seed(42)
  demand <- sf::st_as_sf(
    data.frame(x = runif(20), y = runif(20), pop = rep(1, 20)),
    coords = c("x", "y")
  )
  facilities <- sf::st_as_sf(
    data.frame(x = runif(8), y = runif(8)),
    coords = c("x", "y")
  )

  result <- p_median(demand, facilities, n_facilities = 4, weight_col = "pop")

  # All assignments should be to selected facilities
  selected_ids <- which(result$facilities$.selected)
  expect_true(all(result$demand$.facility %in% selected_ids))
})

test_that("p_median with max_distance works when all demand is reachable", {
  skip_if_not_installed("sf")
  skip_if_not(is.loaded("wrap__rust_p_median"), "Rust compilation required")

  set.seed(42)
  demand <- sf::st_as_sf(
    data.frame(x = runif(20), y = runif(20), pop = rpois(20, 100)),
    coords = c("x", "y")
  )
  facilities <- sf::st_as_sf(
    data.frame(x = runif(8), y = runif(8)),
    coords = c("x", "y")
  )

  # Large max_distance — all demand reachable
  result <- p_median(demand, facilities, n_facilities = 4,
                     weight_col = "pop", max_distance = 2.0)

  expect_equal(sum(result$facilities$.selected), 4)
  selected_ids <- which(result$facilities$.selected)
  expect_true(all(result$demand$.facility %in% selected_ids))
  expect_equal(attr(result, "spopt")$max_distance, 2.0)
})

test_that("p_median with max_distance enforces radius constraint", {
  skip_if_not_installed("sf")
  skip_if_not(is.loaded("wrap__rust_p_median"), "Rust compilation required")

  # Two clusters of demand far apart, with facilities near each cluster
  demand <- sf::st_as_sf(data.frame(
    x = c(0, 0.05, 0.1, 10, 10.05, 10.1),
    y = c(0, 0.05, 0.1, 0, 0.05, 0.1),
    pop = rep(100, 6)
  ), coords = c("x", "y"))
  facilities <- sf::st_as_sf(data.frame(
    x = c(0.05, 10.05),
    y = c(0.05, 0.05)
  ), coords = c("x", "y"))

  result <- p_median(demand, facilities, n_facilities = 2,
                     weight_col = "pop", max_distance = 1.0)

  # Check all assigned distances are within max_distance
  cm <- distance_matrix(demand, facilities)
  for (i in seq_len(nrow(demand))) {
    j <- result$demand$.facility[i]
    expect_lte(cm[i, j], 1.0)
  }
})

test_that("p_median errors when demand has no reachable facility", {
  skip_if_not_installed("sf")
  skip_if_not(is.loaded("wrap__rust_p_median"), "Rust compilation required")

  demand <- sf::st_as_sf(data.frame(
    x = c(0, 0.1, 100),  # third demand far from all facilities
    y = c(0, 0.1, 100),
    pop = c(10, 20, 30)
  ), coords = c("x", "y"))
  facilities <- sf::st_as_sf(data.frame(
    x = c(0.05, 0.15),
    y = c(0.05, 0.15)
  ), coords = c("x", "y"))

  expect_error(
    p_median(demand, facilities, n_facilities = 2,
             weight_col = "pop", max_distance = 1.0),
    "no facility within max_distance"
  )
})

test_that("p_median validates max_distance parameter", {
  skip_if_not_installed("sf")

  demand <- sf::st_as_sf(
    data.frame(x = 1, y = 1, pop = 10),
    coords = c("x", "y")
  )
  facilities <- sf::st_as_sf(
    data.frame(x = 1.1, y = 1.1),
    coords = c("x", "y")
  )

  expect_error(
    p_median(demand, facilities, n_facilities = 1,
             weight_col = "pop", max_distance = -1),
    "single positive number"
  )
  expect_error(
    p_median(demand, facilities, n_facilities = 1,
             weight_col = "pop", max_distance = "abc"),
    "single positive number"
  )
})
