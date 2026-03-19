test_that("allocate_zones nearest finds minimum centers", {
  skip_if_not_installed("sf")
  skip_if_not(is.loaded("wrap__rust_lscp"), "Rust compilation required")

  zones <- sf::st_as_sf(data.frame(
    x = c(0, 0.1, 0.2, 100, 100.1, 100.2),
    y = c(0, 0.1, 0.2, 0, 0.1, 0.2)
  ), coords = c("x", "y"))

  result <- allocate_zones(zones, max_distance = 1.0)

  expect_equal(sum(result$zones$.is_center), 2L)
  expect_true(all(result$zones$.distance <= 1.0))
  expect_true(all(result$zones$.center[1:3] %in% 1:3))
  expect_true(all(result$zones$.center[4:6] %in% 4:6))
  expect_equal(attr(result, "spopt")$method, "nearest")
})

test_that("allocate_zones p_median optimizes weighted distance", {
  skip_if_not_installed("sf")
  skip_if_not(is.loaded("wrap__rust_p_median"), "Rust compilation required")

  set.seed(42)
  zones <- sf::st_as_sf(data.frame(
    x = runif(20, 0, 1), y = runif(20, 0, 1),
    pop = c(rep(1000, 5), rep(1, 15))
  ), coords = c("x", "y"))

  md <- 0.5
  r_near <- allocate_zones(zones, max_distance = md)
  r_opt <- allocate_zones(zones, max_distance = md, method = "p_median",
                          weight_col = "pop")

  # Same K
  expect_equal(sum(r_near$zones$.is_center), sum(r_opt$zones$.is_center))

  # p_median should have <= weighted distance
  w <- zones$pop
  expect_lte(sum(w * r_opt$zones$.distance),
             sum(w * r_near$zones$.distance) + 1e-6)
})

test_that("allocate_zones cflp respects capacity", {
  skip_if_not_installed("sf")
  skip_if_not(is.loaded("wrap__rust_cflp"), "Rust compilation required")

  zones <- sf::st_as_sf(data.frame(
    x = c(0, 0.05, 0.1, 0.15),
    y = c(0, 0.05, 0.1, 0.15),
    w = c(10, 10, 10, 10)
  ), coords = c("x", "y"))

  # Capacity 15: can't fit all 40 in one center
  result <- allocate_zones(zones, max_distance = 0.5, method = "cflp",
                           weight_col = "w", capacity = 15)
  expect_true(sum(result$zones$.is_center) >= 3L)
  expect_true(all(result$zones$.distance <= 0.5))
})

test_that("allocate_zones cflp with no weight_col uses unit weights", {
  skip_if_not_installed("sf")
  skip_if_not(is.loaded("wrap__rust_cflp"), "Rust compilation required")

  zones <- sf::st_as_sf(data.frame(
    x = c(0, 0.1, 0.2, 0.3, 100, 100.1, 100.2, 100.3),
    y = c(0, 0.1, 0.2, 0.3, 0, 0.1, 0.2, 0.3)
  ), coords = c("x", "y"))

  # capacity = 2 means max 2 zones per center
  result <- allocate_zones(zones, max_distance = 1.0, method = "cflp",
                           capacity = 2)
  expect_true(max(table(result$zones$.center)) <= 2)
})

test_that("allocate_zones respects partition_col", {
  skip_if_not_installed("sf")
  skip_if_not(is.loaded("wrap__rust_lscp"), "Rust compilation required")

  zones <- sf::st_as_sf(data.frame(
    x = c(0, 0.1, 5, 5.1),
    y = c(0, 0.1, 0, 0.1),
    region = c("A", "A", "B", "B")
  ), coords = c("x", "y"))

  result <- allocate_zones(zones, max_distance = 1.0, partition_col = "region")
  expect_true(all(result$zones$.center[1:2] %in% 1:2))
  expect_true(all(result$zones$.center[3:4] %in% 3:4))
})

test_that("allocate_zones validates inputs", {
  skip_if_not_installed("sf")

  zones <- sf::st_as_sf(data.frame(x = 1, y = 1, pop = 10), coords = c("x", "y"))

  expect_error(allocate_zones(zones, max_distance = -1), "single positive number")
  expect_error(allocate_zones(zones, max_distance = 1, method = "cflp"),
               "capacity.*required")
  expect_error(allocate_zones(zones, max_distance = 1, weight_col = "nope"),
               "not found")
  expect_error(allocate_zones(zones, max_distance = 1, partition_col = "nope"),
               "not found")
})
