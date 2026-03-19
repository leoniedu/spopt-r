test_that("cflp returns correct structure", {
  skip_if_not_installed("sf")
  skip_if_not(is.loaded("wrap__rust_cflp"), "Rust compilation required")

  set.seed(42)
  demand <- sf::st_as_sf(
    data.frame(x = runif(20), y = runif(20), pop = rpois(20, 100)),
    coords = c("x", "y")
  )
  facilities <- sf::st_as_sf(
    data.frame(x = runif(8), y = runif(8), cap = rep(1000, 8)),
    coords = c("x", "y")
  )

  result <- cflp(demand, facilities, n_facilities = 3,
                 weight_col = "pop", capacity_col = "cap")

  expect_type(result, "list")
  expect_s3_class(result$demand, "sf")
  expect_s3_class(result$facilities, "sf")
  expect_true(".facility" %in% names(result$demand))
  expect_true(".selected" %in% names(result$facilities))
  expect_true(".utilization" %in% names(result$facilities))
  expect_equal(sum(result$facilities$.selected), 3)
})

test_that("cflp with max_distance works when all demand is reachable", {
  skip_if_not_installed("sf")
  skip_if_not(is.loaded("wrap__rust_cflp"), "Rust compilation required")

  set.seed(42)
  demand <- sf::st_as_sf(
    data.frame(x = runif(20), y = runif(20), pop = rpois(20, 100)),
    coords = c("x", "y")
  )
  facilities <- sf::st_as_sf(
    data.frame(x = runif(8), y = runif(8), cap = rep(1000, 8)),
    coords = c("x", "y")
  )

  result <- cflp(demand, facilities, n_facilities = 3,
                 weight_col = "pop", capacity_col = "cap",
                 max_distance = 2.0)

  expect_equal(sum(result$facilities$.selected), 3)
  expect_equal(attr(result, "spopt")$max_distance, 2.0)
})

test_that("cflp errors when demand has no reachable facility", {
  skip_if_not_installed("sf")
  skip_if_not(is.loaded("wrap__rust_cflp"), "Rust compilation required")

  demand <- sf::st_as_sf(data.frame(
    x = c(0, 0.1, 100),
    y = c(0, 0.1, 100),
    pop = c(10, 20, 30)
  ), coords = c("x", "y"))
  facilities <- sf::st_as_sf(data.frame(
    x = c(0.05, 0.15),
    y = c(0.05, 0.15),
    cap = c(500, 500)
  ), coords = c("x", "y"))

  expect_error(
    cflp(demand, facilities, n_facilities = 2,
         weight_col = "pop", capacity_col = "cap",
         max_distance = 1.0),
    "no facility within max_distance"
  )
})

test_that("cflp validates max_distance parameter", {
  skip_if_not_installed("sf")

  demand <- sf::st_as_sf(
    data.frame(x = 1, y = 1, pop = 10),
    coords = c("x", "y")
  )
  facilities <- sf::st_as_sf(
    data.frame(x = 1.1, y = 1.1, cap = 100),
    coords = c("x", "y")
  )

  expect_error(
    cflp(demand, facilities, n_facilities = 1,
         weight_col = "pop", capacity_col = "cap",
         max_distance = -1),
    "single positive number"
  )
})
