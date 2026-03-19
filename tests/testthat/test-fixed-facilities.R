# Shared test data
make_test_data <- function(fixed_logical = NULL, fixed_status = NULL) {
  set.seed(42)
  demand <- sf::st_as_sf(
    data.frame(x = runif(20), y = runif(20), pop = rpois(20, 100)),
    coords = c("x", "y")
  )
  fac_df <- data.frame(x = runif(8), y = runif(8))
  if (!is.null(fixed_logical)) fac_df$fixed <- fixed_logical
  if (!is.null(fixed_status)) fac_df$status <- fixed_status
  facilities <- sf::st_as_sf(fac_df, coords = c("x", "y"))
  list(demand = demand, facilities = facilities)
}

test_that("mclp with fixed_col (logical) includes fixed in selected", {
  skip_if_not_installed("sf")

  d <- make_test_data(fixed_logical = c(TRUE, TRUE, rep(FALSE, 6)))
  result <- mclp(d$demand, d$facilities, service_radius = 0.5,
                  n_facilities = 3, weight_col = "pop",
                  fixed_col = "fixed")

  meta <- attr(result, "spopt")
  selected <- which(result$facilities$.selected)

  expect_true(1 %in% selected)
  expect_true(2 %in% selected)
  expect_equal(meta$n_selected, 3)
  expect_equal(meta$n_fixed, 2)
  expect_true(result$facilities$.fixed[1])
  expect_true(result$facilities$.fixed[2])
  expect_false(result$facilities$.fixed[3])
})

test_that("mclp with fixed_col (required/candidate) works", {
  skip_if_not_installed("sf")

  d <- make_test_data(fixed_status = c("required", "required",
                                        rep("candidate", 6)))
  result <- mclp(d$demand, d$facilities, service_radius = 0.5,
                  n_facilities = 3, weight_col = "pop",
                  fixed_col = "status")

  selected <- which(result$facilities$.selected)
  expect_true(1 %in% selected)
  expect_true(2 %in% selected)
  expect_equal(length(selected), 3)
})

test_that("p_median with fixed_col includes fixed in selected", {
  skip_if_not_installed("sf")

  d <- make_test_data(fixed_logical = c(TRUE, rep(FALSE, 7)))
  result <- p_median(d$demand, d$facilities, n_facilities = 3,
                      weight_col = "pop", fixed_col = "fixed")

  meta <- attr(result, "spopt")
  selected <- which(result$facilities$.selected)

  expect_true(1 %in% selected)
  expect_equal(length(selected), 3)
  expect_equal(meta$n_fixed, 1)
})

test_that("p_center binary_search with fixed_col includes fixed in selected", {
  skip_if_not_installed("sf")

  d <- make_test_data(fixed_logical = c(TRUE, rep(FALSE, 7)))
  result <- p_center(d$demand, d$facilities, n_facilities = 3,
                      method = "binary_search", fixed_col = "fixed")

  selected <- which(result$facilities$.selected)
  expect_true(1 %in% selected)
  expect_equal(length(selected), 3)
  expect_true(result$facilities$.fixed[1])
})

test_that("p_center mip with fixed_col includes fixed in selected", {
  skip_if_not_installed("sf")

  d <- make_test_data(fixed_logical = c(TRUE, rep(FALSE, 7)))
  result <- p_center(d$demand, d$facilities, n_facilities = 3,
                      method = "mip", fixed_col = "fixed")

  selected <- which(result$facilities$.selected)
  expect_true(1 %in% selected)
  expect_equal(length(selected), 3)
  expect_true(result$facilities$.fixed[1])
})

test_that("all facilities fixed (degenerate case) works", {
  skip_if_not_installed("sf")

  d <- make_test_data(fixed_logical = c(TRUE, TRUE, TRUE, rep(FALSE, 5)))
  result <- p_median(d$demand, d$facilities, n_facilities = 3,
                      weight_col = "pop", fixed_col = "fixed")

  meta <- attr(result, "spopt")
  selected <- which(result$facilities$.selected)

  expect_equal(sort(selected), c(1, 2, 3))
  expect_equal(meta$n_fixed, 3)
  expect_true(all(result$facilities$.fixed[1:3]))
})

test_that("fixed_col validation catches errors", {
  skip_if_not_installed("sf")

  d <- make_test_data()

  # Column not found
  expect_error(
    p_median(d$demand, d$facilities, n_facilities = 3,
             weight_col = "pop", fixed_col = "nonexistent"),
    "not found"
  )

  # NA values in column
  d2 <- make_test_data(fixed_logical = c(TRUE, NA, rep(FALSE, 6)))
  expect_error(
    p_median(d2$demand, d2$facilities, n_facilities = 3,
             weight_col = "pop", fixed_col = "fixed"),
    "NA"
  )

  # More fixed than requested
  d3 <- make_test_data(fixed_logical = c(TRUE, TRUE, TRUE, rep(FALSE, 5)))
  expect_error(
    p_median(d3$demand, d3$facilities, n_facilities = 2,
             weight_col = "pop", fixed_col = "fixed"),
    "more fixed"
  )

  # Bad character values
  d4 <- make_test_data(fixed_status = c("required", "open", rep("candidate", 6)))
  expect_error(
    p_median(d4$demand, d4$facilities, n_facilities = 3,
             weight_col = "pop", fixed_col = "status"),
    "required.*candidate"
  )
})

test_that("NULL fixed_col preserves backward compatibility", {
  skip_if_not_installed("sf")

  d <- make_test_data()
  result <- p_median(d$demand, d$facilities, n_facilities = 3,
                      weight_col = "pop", fixed_col = NULL)

  meta <- attr(result, "spopt")
  expect_equal(meta$n_fixed, 0)
  expect_true(all(!result$facilities$.fixed))
})
