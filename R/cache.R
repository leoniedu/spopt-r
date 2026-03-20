# Private state environment -------------------------------------------------

.spopt_env <- new.env(parent = emptyenv())
.spopt_env$cache_enabled         <- FALSE
.spopt_env$cache_type            <- NULL  # "memory" or "filesystem"
.spopt_env$versioned_cache_path  <- NULL  # effective path: <R_user_dir>/<version>

# Solver names — single source of truth for the cacheable Rust wrappers.
.solver_names <- c(
  "rust_lscp",
  "rust_p_median",
  "rust_cflp",
  "rust_mclp",
  "rust_p_center",
  "rust_p_dispersion",
  "rust_frlm_greedy"
)

# Solver dispatch table ------------------------------------------------------
# On load, holds bare Rust functions. When cache is enabled, holds memoised
# wrappers. All locate_*.R call sites route through this environment.

spopt_solvers <- new.env(parent = emptyenv())

# Restore bare (unmemoised) Rust functions into the dispatch table.
.restore_bare_solvers <- function() {
  ns <- asNamespace("spopt")
  for (nm in .solver_names) {
    spopt_solvers[[nm]] <- get(nm, envir = ns, inherits = FALSE)
  }
}

# .onLoad: populate dispatch table after the DLL is loaded ------------------

.onLoad <- function(libname, pkgname) {
  .restore_bare_solvers()
}

# Public API -----------------------------------------------------------------

#' Enable solver result caching
#'
#' Wraps the internal Rust solver calls with \pkg{memoise} so that identical
#' solver inputs return cached results immediately. All facility location
#' functions (\code{allocate_zones}, \code{p_median}, \code{cflp}, etc.)
#' benefit transparently.
#'
#' @param persistent Logical. If \code{FALSE} (default), results are cached
#'   in memory for the duration of the R session. If \code{TRUE}, a
#'   filesystem cache is used at the package's standard user-cache directory
#'   (\code{tools::R_user_dir("spopt", "cache")}), with the package version
#'   automatically appended as a subdirectory so that upgrading spopt
#'   invalidates old entries without any manual cleanup.
#'
#' @details
#' **Parallel safety:** The in-memory cache is not shared across forked R
#' processes (e.g. \code{parallel::mclapply}). Each child process gets its own
#' copy and writes back to it independently — there is no corruption, but also
#' no cross-process cache hits. The filesystem cache is safe across separate R
#' processes due to file-based locking.
#'
#' @return Invisibly \code{NULL}.
#' @seealso [spopt_disable_cache()], [spopt_clear_cache()], [spopt_cache_info()]
#' @export
spopt_enable_cache <- function(persistent = FALSE) {
  if (!requireNamespace("memoise", quietly = TRUE)) {
    stop(
      "Package 'memoise' is required for caching. ",
      "Install it with: install.packages('memoise')",
      call. = FALSE
    )
  }

  if (!persistent) {
    cache_obj <- memoise::cache_memory()
    .spopt_env$cache_type           <- "memory"
    .spopt_env$versioned_cache_path <- NULL
  } else {
    base_path      <- tools::R_user_dir("spopt", "cache")
    pkg_ver        <- as.character(utils::packageVersion("spopt"))
    versioned_path <- file.path(base_path, pkg_ver)
    dir.create(versioned_path, recursive = TRUE, showWarnings = FALSE)
    cache_obj <- memoise::cache_filesystem(versioned_path)
    .spopt_env$cache_type           <- "filesystem"
    .spopt_env$versioned_cache_path <- versioned_path
  }

  # Always fetch bare functions from the package namespace to prevent
  # double-wrapping if spopt_enable_cache() is called more than once.
  ns <- asNamespace("spopt")
  for (nm in .solver_names) {
    bare_fn <- get(nm, envir = ns, inherits = FALSE)
    spopt_solvers[[nm]] <- memoise::memoise(bare_fn, cache = cache_obj)
  }

  .spopt_env$cache_enabled <- TRUE
  invisible(NULL)
}

#' Disable solver result caching
#'
#' Restores all solver dispatch slots to the bare (unmemoised) Rust functions,
#' removing any caching overhead.
#'
#' @return Invisibly \code{NULL}.
#' @seealso [spopt_enable_cache()], [spopt_clear_cache()], [spopt_cache_info()]
#' @export
spopt_disable_cache <- function() {
  .restore_bare_solvers()
  .spopt_env$cache_enabled         <- FALSE
  .spopt_env$cache_type            <- NULL
  .spopt_env$versioned_cache_path  <- NULL
  invisible(NULL)
}

#' Clear cached solver results
#'
#' Discards all cached results without changing whether caching is enabled. The
#' next solver call will be computed fresh and re-cached.
#'
#' @details
#' For in-memory caches, re-initialises the cache object. For filesystem
#' caches, removes all version-named subdirectories under the package cache
#' directory (e.g. \code{0.1.1/}) — only directories created by spopt are
#' removed, nothing else is touched.
#'
#' @return Invisibly \code{NULL}.
#' @seealso [spopt_enable_cache()], [spopt_disable_cache()], [spopt_cache_info()]
#' @export
spopt_clear_cache <- function() {
  if (!.spopt_env$cache_enabled) {
    message("Cache is not currently enabled. Use spopt_enable_cache() first.")
    return(invisible(NULL))
  }
  is_persistent <- identical(.spopt_env$cache_type, "filesystem")
  # For filesystem: remove version-named subdirectories only. The base cache
  # directory is never deleted.
  if (is_persistent && !is.null(.spopt_env$versioned_cache_path)) {
    base_path <- dirname(.spopt_env$versioned_cache_path)
    if (dir.exists(base_path)) {
      subdirs <- list.dirs(base_path, recursive = FALSE, full.names = TRUE)
      version_subdirs <- subdirs[grepl("^\\d+\\.\\d+(\\.\\d+)*$", basename(subdirs))]
      for (d in version_subdirs) unlink(d, recursive = TRUE)
    }
  }
  # Re-enable with same settings to recreate the cache object (and directory).
  spopt_enable_cache(persistent = is_persistent)
  invisible(NULL)
}

#' Query the current cache state
#'
#' Returns a named list describing whether caching is active and, if so,
#' which backend is in use.
#'
#' @return A named list with elements:
#'   \describe{
#'     \item{enabled}{Logical. \code{TRUE} if caching is active.}
#'     \item{type}{Character or \code{NULL}. \code{"memory"}, \code{"filesystem"}, or \code{NULL}.}
#'     \item{versioned_path}{Character or \code{NULL}. Effective filesystem path
#'       including the package version subdirectory, or \code{NULL} for in-memory
#'       or disabled caches.}
#'   }
#' @seealso [spopt_enable_cache()], [spopt_disable_cache()], [spopt_clear_cache()]
#' @export
spopt_cache_info <- function() {
  list(
    enabled        = .spopt_env$cache_enabled,
    type           = .spopt_env$cache_type,
    versioned_path = .spopt_env$versioned_cache_path
  )
}
