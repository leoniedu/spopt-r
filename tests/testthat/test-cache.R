library(sf)

# Helper: small test dataset used across multiple tests
make_zones <- function(n = 8, seed = 42) {
  set.seed(seed)
  sf::st_as_sf(
    data.frame(x = runif(n), y = runif(n), pop = rpois(n, 50)),
    coords = c("x", "y")
  )
}

# ── Default state ────────────────────────────────────────────────────────────

test_that("cache is disabled by default", {
  skip_if_not_installed("memoise")

  info <- spopt_cache_info()
  expect_false(info$enabled)
  expect_null(info$type)
  expect_null(info$versioned_path)
})

test_that("rust_lscp dispatch slot is not memoised by default", {
  skip_if_not_installed("memoise")
  skip_if_not(is.loaded("wrap__rust_lscp"), "Rust compilation required")

  expect_false(memoise::is.memoised(spopt:::spopt_solvers$rust_lscp))
})

# ── Enable in-memory cache ────────────────────────────────────────────────────

test_that("spopt_enable_cache() switches dispatch slots to memoised functions", {
  skip_if_not_installed("memoise")
  skip_if_not(is.loaded("wrap__rust_lscp"), "Rust compilation required")

  on.exit(spopt_disable_cache(), add = TRUE)

  spopt_enable_cache()

  expect_true(spopt_cache_info()$enabled)
  expect_equal(spopt_cache_info()$type, "memory")
  expect_null(spopt_cache_info()$versioned_path)

  expect_true(memoise::is.memoised(spopt:::spopt_solvers$rust_lscp))
  expect_true(memoise::is.memoised(spopt:::spopt_solvers$rust_p_median))
  expect_true(memoise::is.memoised(spopt:::spopt_solvers$rust_cflp))
})

# ── Disable cache ─────────────────────────────────────────────────────────────

test_that("spopt_disable_cache() restores bare functions", {
  skip_if_not_installed("memoise")
  skip_if_not(is.loaded("wrap__rust_lscp"), "Rust compilation required")

  spopt_enable_cache()
  spopt_disable_cache()

  expect_false(spopt_cache_info()$enabled)
  expect_false(memoise::is.memoised(spopt:::spopt_solvers$rust_lscp))
})

# ── Double-enable idempotency ──────────────────────────────────────────────────

test_that("calling spopt_enable_cache() twice does not error or double-wrap", {
  skip_if_not_installed("memoise")
  skip_if_not(is.loaded("wrap__rust_lscp"), "Rust compilation required")

  on.exit(spopt_disable_cache(), add = TRUE)

  expect_no_error(spopt_enable_cache())
  expect_no_error(spopt_enable_cache())

  expect_true(memoise::is.memoised(spopt:::spopt_solvers$rust_lscp))
})

# ── Result identity ────────────────────────────────────────────────────────────

test_that("cached p_median() returns identical result to uncached", {
  skip_if_not_installed("memoise")
  skip_if_not(is.loaded("wrap__rust_p_median"), "Rust compilation required")

  zones <- make_zones(12)
  facilities <- make_zones(6, seed = 99)

  result_uncached <- p_median(zones, facilities, n_facilities = 2, weight_col = "pop")

  on.exit(spopt_disable_cache(), add = TRUE)
  spopt_enable_cache()

  result_cached1 <- p_median(zones, facilities, n_facilities = 2, weight_col = "pop")
  result_cached2 <- p_median(zones, facilities, n_facilities = 2, weight_col = "pop")

  expect_equal(result_cached1$demand$.facility, result_uncached$demand$.facility)
  expect_equal(result_cached2$demand$.facility, result_cached1$demand$.facility)
})

# ── Clear cache ────────────────────────────────────────────────────────────────

test_that("spopt_clear_cache() resets in-memory cache without disabling it", {
  skip_if_not_installed("memoise")
  skip_if_not(is.loaded("wrap__rust_lscp"), "Rust compilation required")

  on.exit(spopt_disable_cache(), add = TRUE)

  spopt_enable_cache()
  expect_true(spopt_cache_info()$enabled)

  spopt_clear_cache()

  expect_true(spopt_cache_info()$enabled)
  expect_equal(spopt_cache_info()$type, "memory")
  expect_true(memoise::is.memoised(spopt:::spopt_solvers$rust_lscp))
})

test_that("spopt_clear_cache() on disabled cache emits message", {
  skip_if_not_installed("memoise")

  spopt_disable_cache()
  expect_message(spopt_clear_cache(), "not currently enabled")
})

# ── Persistent (filesystem) cache ─────────────────────────────────────────────

test_that("spopt_enable_cache(persistent=TRUE) uses versioned filesystem backend", {
  skip_if_not_installed("memoise")
  skip_if_not(is.loaded("wrap__rust_lscp"), "Rust compilation required")

  on.exit(spopt_disable_cache(), add = TRUE)

  spopt_enable_cache(persistent = TRUE)

  info <- spopt_cache_info()
  expect_true(info$enabled)
  expect_equal(info$type, "filesystem")

  # Versioned subdirectory: <R_user_dir("spopt","cache")>/<version>/
  pkg_ver   <- as.character(utils::packageVersion("spopt"))
  base_path <- tools::R_user_dir("spopt", "cache")
  expect_equal(info$versioned_path, file.path(base_path, pkg_ver))
  expect_true(dir.exists(info$versioned_path))
  expect_true(memoise::is.memoised(spopt:::spopt_solvers$rust_lscp))
})

test_that("spopt_clear_cache() removes version subdirs but not base cache dir", {
  skip_if_not_installed("memoise")

  on.exit(spopt_disable_cache(), add = TRUE)

  # Simulate a stale version directory from an old install
  base_path <- tools::R_user_dir("spopt", "cache")
  old_version_dir <- file.path(base_path, "0.0.1")
  dir.create(old_version_dir, recursive = TRUE)

  spopt_enable_cache(persistent = TRUE)
  spopt_clear_cache()

  # Stale version dir gone
  expect_false(dir.exists(old_version_dir))
  # Base cache dir is still there
  expect_true(dir.exists(base_path))
  # Caching still active with a fresh versioned dir
  expect_true(spopt_cache_info()$enabled)
  pkg_ver <- as.character(utils::packageVersion("spopt"))
  expect_true(dir.exists(file.path(base_path, pkg_ver)))
})

# ── allocate_zones integration ────────────────────────────────────────────────

test_that("allocate_zones returns identical results with cache enabled", {
  skip_if_not_installed("memoise")
  skip_if_not(is.loaded("wrap__rust_lscp"), "Rust compilation required")

  zones <- make_zones(10)

  result1 <- allocate_zones(zones, max_distance = 0.5)
  on.exit(spopt_disable_cache(), add = TRUE)
  spopt_enable_cache()

  result2 <- allocate_zones(zones, max_distance = 0.5)
  result3 <- allocate_zones(zones, max_distance = 0.5)

  expect_equal(result2$zones$.center, result1$zones$.center)
  expect_equal(result3$zones$.center, result2$zones$.center)
})
