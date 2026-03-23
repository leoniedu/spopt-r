#' Warehouse Location Problem (ORCE)
#'
#' Solves the warehouse location problem: minimize total transport costs,
#' facility opening costs, and worker costs. The number of facilities to open
#' and the number of workers per facility are determined by the optimizer.
#'
#' @param demand An sf object representing demand points.
#' @param facilities An sf object representing candidate facility locations.
#' @param weight_col Character. Column name in `demand` containing demand
#'   weights (e.g., collection days, workload).
#' @param cost_matrix Numeric matrix (demand x facilities). Pre-computed
#'   transport cost from each demand point to each facility.
#' @param facility_cost_col Character. Column name in `facilities` containing
#'   the fixed cost to open each facility.
#' @param worker_cost Numeric scalar. Cost per worker (salary, training, etc.).
#' @param worker_capacity Numeric scalar. Maximum demand units a single worker
#'   can handle.
#' @param max_workers_col Character. Column name in `facilities` containing the
#'   maximum number of workers each facility can employ.
#' @param min_workers Integer scalar. Minimum number of workers at each open
#'   facility. Default is 1.
#' @param peso_tsp Numeric scalar. Weight for TSP routing penalty. When
#'   greater than 0, an iterative location-routing loop adjusts the cost
#'   matrix using cheapest insertion costs from per-facility TSP tours, which
#'   encourages geographically coherent assignments. Default is 0 (no routing
#'   penalty).
#' @param max_iter_tsp Integer scalar. Maximum number of location-routing
#'   iterations when `peso_tsp > 0`. Default is 10.
#' @param distance_matrix_full Square numeric matrix of dimensions
#'   `(n_demand + n_fac) x (n_demand + n_fac)`. Required when `peso_tsp > 0`.
#'   Rows/columns `1:n_demand` are demand points (same order as `demand`),
#'   rows/columns `(n_demand+1):(n_demand+n_fac)` are facilities (same order
#'   as `facilities`). Typically built as
#'   `distance_matrix(rbind(demand, facilities))`.
#' @param verbose Logical. Print problem dimensions before solving. Default
#'   is FALSE.
#'
#' @return A list with two sf objects:
#'   \itemize{
#'     \item `$demand`: Original demand sf with `.facility` column (1-based
#'       index of the assigned facility)
#'     \item `$facilities`: Original facilities sf with `.selected` (logical),
#'       `.n_assigned` (integer), `.workers` (integer), and `.utilization`
#'       (numeric) columns
#'   }
#'   Metadata is stored in the "spopt" attribute, including:
#'   \itemize{
#'     \item `objective`: Total cost (transport + facility + worker)
#'     \item `transport_cost`: Transport cost component
#'     \item `facility_cost`: Facility opening cost component
#'     \item `worker_cost_total`: Worker cost component
#'     \item `n_selected`: Number of open facilities
#'     \item `solve_time`: Solver runtime in seconds
#'     \item `tsp_iterations`: Number of location-routing iterations (0 when
#'       `peso_tsp = 0`)
#'     \item `tsp_converged`: Logical. TRUE if assignments stabilized before
#'       `max_iter_tsp`
#'     \item `peso_tsp`: The routing penalty weight used
#'   }
#'
#' @details
#' The ORCE model extends the uncapacitated facility location problem by adding
#' worker allocation as integer decision variables. The solver simultaneously
#' decides which facilities to open, how to assign demand, and how many workers
#' to place at each facility.
#'
#' The formulation minimizes:
#' \deqn{\sum_{ij} c_{ij} x_{ij} + \sum_j f_j y_j + \sum_j w_{cost} \cdot w_j}
#'
#' Subject to assignment, linking, capacity, and worker bound constraints. See
#' the package vignette for full details.
#'
#' The `cost_matrix` should contain pre-computed transport costs. Users can
#' build this from distance and duration data using a helper, e.g.:
#' `cost = distance_km / kml * fuel_cost + duration_hours * hourly_cost`
#'
#' ## Iterative location-routing (`peso_tsp > 0`)
#'
#' When `peso_tsp > 0`, the solver iteratively refines assignments by adding
#' a TSP routing penalty to the cost matrix:
#' 1. Solve ORCE with current costs.
#' 2. For each opened facility, solve a TSP tour over its assigned demands.
#' 3. For every (demand, facility) pair, compute the cheapest insertion cost
#'    of adding that demand into the facility's tour.
#' 4. Update: `cost_new = cost_original + peso_tsp * insertion_cost`.
#' 5. Repeat until assignments stabilize or `max_iter_tsp` is reached.
#'
#' This encourages geographically compact clusters without the scalability
#' issues of embedding route variables directly in the MIP.
#'
#' @references
#' Leon, E. et al. (2024). orce: Optimization of Statistical Data Collection
#' Networks. R package. \url{https://github.com/orce-ibge/orce}
#'
#' @examples
#' \dontrun{
#' library(sf)
#'
#' demand <- st_as_sf(data.frame(
#'   x = runif(30), y = runif(30), workload = rpois(30, 20)
#' ), coords = c("x", "y"))
#'
#' facilities <- st_as_sf(data.frame(
#'   x = runif(10), y = runif(10),
#'   fixed_cost = rep(500, 10),
#'   max_workers = rep(5L, 10)
#' ), coords = c("x", "y"))
#'
#' cost <- distance_matrix(demand, facilities)
#'
#' result <- orce(demand, facilities,
#'   weight_col = "workload", cost_matrix = cost,
#'   facility_cost_col = "fixed_cost", worker_cost = 1000,
#'   worker_capacity = 100, max_workers_col = "max_workers"
#' )
#'
#' # With routing penalty
#' all_pts <- st_as_sf(as.data.frame(rbind(
#'   st_coordinates(demand), st_coordinates(facilities)
#' )), coords = c("X", "Y"))
#' dm_full <- distance_matrix(all_pts)
#' result2 <- orce(demand, facilities,
#'   weight_col = "workload", cost_matrix = cost,
#'   facility_cost_col = "fixed_cost", worker_cost = 1000,
#'   worker_capacity = 100, max_workers_col = "max_workers",
#'   peso_tsp = 1, distance_matrix_full = dm_full
#' )
#' }
#'
#' @export
orce <- function(demand,
                 facilities,
                 weight_col,
                 cost_matrix,
                 facility_cost_col,
                 worker_cost,
                 worker_capacity,
                 max_workers_col,
                 min_workers = 1L,
                 peso_tsp = 0,
                 max_iter_tsp = 10L,
                 distance_matrix_full = NULL,
                 verbose = FALSE) {
  # --- Input validation ---
  if (!inherits(demand, "sf")) {
    stop("`demand` must be an sf object", call. = FALSE)
  }
  if (!inherits(facilities, "sf")) {
    stop("`facilities` must be an sf object", call. = FALSE)
  }
  if (!weight_col %in% names(demand)) {
    stop(paste0("Weight column '", weight_col, "' not found in demand"), call. = FALSE)
  }
  if (!facility_cost_col %in% names(facilities)) {
    stop(paste0("Facility cost column '", facility_cost_col, "' not found in facilities"),
         call. = FALSE)
  }
  if (!max_workers_col %in% names(facilities)) {
    stop(paste0("Max workers column '", max_workers_col, "' not found in facilities"),
         call. = FALSE)
  }

  weights <- as.numeric(demand[[weight_col]])
  facility_costs <- as.numeric(facilities[[facility_cost_col]])
  max_workers <- as.integer(facilities[[max_workers_col]])

  if (any(is.na(weights))) {
    stop("Weight column contains NA values", call. = FALSE)
  }
  if (any(is.na(facility_costs))) {
    stop("Facility cost column contains NA values", call. = FALSE)
  }
  if (any(is.na(max_workers))) {
    stop("Max workers column contains NA values", call. = FALSE)
  }

  if (!is.numeric(worker_cost) || length(worker_cost) != 1 || worker_cost <= 0) {
    stop("`worker_cost` must be a single positive number", call. = FALSE)
  }
  if (!is.numeric(worker_capacity) || length(worker_capacity) != 1 || worker_capacity <= 0) {
    stop("`worker_capacity` must be a single positive number", call. = FALSE)
  }
  min_workers <- as.integer(min_workers)
  if (is.na(min_workers) || min_workers < 1L) {
    stop("`min_workers` must be a positive integer", call. = FALSE)
  }

  # TSP parameters validation
  if (!is.numeric(peso_tsp) || length(peso_tsp) != 1 || is.na(peso_tsp) || peso_tsp < 0) {
    stop("`peso_tsp` must be a single non-negative number", call. = FALSE)
  }
  max_iter_tsp <- as.integer(max_iter_tsp)
  if (is.na(max_iter_tsp) || max_iter_tsp < 1L) {
    stop("`max_iter_tsp` must be a positive integer", call. = FALSE)
  }

  # Warn about facilities that can never open
  unopenable <- max_workers < min_workers
  if (any(unopenable)) {
    warning(
      sprintf(
        "%d facility(ies) have max_workers < min_workers and cannot be opened",
        sum(unopenable)
      ),
      call. = FALSE
    )
  }

  # Feasibility check (only count openable facilities)
  openable_capacity <- sum(as.numeric(max_workers[!unopenable])) * worker_capacity
  total_demand <- sum(weights)
  if (openable_capacity < total_demand) {
    stop(sprintf(
      "Total openable capacity (%.2f) is less than total demand (%.2f). Problem is infeasible.",
      openable_capacity, total_demand
    ), call. = FALSE)
  }

  # Cost matrix validation
  n_demand <- nrow(demand)
  n_fac <- nrow(facilities)
  if (!is.matrix(cost_matrix) || nrow(cost_matrix) != n_demand || ncol(cost_matrix) != n_fac) {
    stop(sprintf(
      "`cost_matrix` must be a %d x %d matrix, got %s",
      n_demand, n_fac,
      if (is.matrix(cost_matrix)) paste(dim(cost_matrix), collapse = " x ") else class(cost_matrix)[1]
    ), call. = FALSE)
  }

  cost_matrix <- sanitize_cost_matrix(cost_matrix)

  # Validate distance_matrix_full when TSP is active
  use_tsp <- peso_tsp > 0
  if (use_tsp) {
    n_total <- n_demand + n_fac
    if (is.null(distance_matrix_full)) {
      stop("`distance_matrix_full` is required when `peso_tsp > 0`", call. = FALSE)
    }
    if (!is.matrix(distance_matrix_full) ||
        nrow(distance_matrix_full) != n_total ||
        ncol(distance_matrix_full) != n_total) {
      stop(sprintf(
        "`distance_matrix_full` must be a %d x %d matrix, got %s",
        n_total, n_total,
        if (is.matrix(distance_matrix_full)) {
          paste(dim(distance_matrix_full), collapse = " x ")
        } else {
          class(distance_matrix_full)[1]
        }
      ), call. = FALSE)
    }
    if (any(is.na(distance_matrix_full))) {
      stop("`distance_matrix_full` must not contain NA values", call. = FALSE)
    }
  }

  if (verbose) {
    message(sprintf(
      "ORCE: %d demand points, %d facilities (%d openable), min_workers=%d",
      n_demand, n_fac, sum(!unopenable), min_workers
    ))
    if (use_tsp) {
      message(sprintf("  TSP routing penalty: peso_tsp=%.2f, max_iter=%d", peso_tsp, max_iter_tsp))
    }
  }

  # --- Solve ---
  start_time <- Sys.time()

  if (!use_tsp) {
    # Single ORCE solve (original behavior)
    result <- spopt_solvers$rust_orce(
      cost_matrix,
      weights,
      facility_costs,
      worker_cost,
      worker_capacity,
      min_workers,
      max_workers
    )

    if (!is.null(result$error)) {
      stop(result$error, call. = FALSE)
    }

    tsp_iterations <- 0L
    tsp_converged <- NA
  } else {
    # Iterative location-routing loop
    current_cost_matrix <- cost_matrix
    prev_assignments <- NULL
    tsp_converged <- FALSE
    tsp_iterations <- 0L

    for (iter in seq_len(max_iter_tsp)) {
      result <- spopt_solvers$rust_orce(
        current_cost_matrix,
        weights,
        facility_costs,
        worker_cost,
        worker_capacity,
        min_workers,
        max_workers
      )

      if (!is.null(result$error)) {
        stop(result$error, call. = FALSE)
      }

      tsp_iterations <- iter

      # Check convergence
      if (identical(result$assignments, prev_assignments)) {
        tsp_converged <- TRUE
        if (verbose) {
          message(sprintf("  TSP converged after %d iteration(s)", iter))
        }
        break
      }
      prev_assignments <- result$assignments

      # Compute insertion costs and update cost matrix for next iteration
      if (iter < max_iter_tsp) {
        insertion_costs <- rust_orce_insertion_costs(
          distance_matrix_full,
          result$assignments,
          as.integer(n_demand),
          as.integer(n_fac)
        )
        current_cost_matrix <- cost_matrix + peso_tsp * insertion_costs
      }
    }

    # Recompute true cost decomposition using original cost matrix
    result$transport_cost <- sum(
      cost_matrix[cbind(seq_len(n_demand), result$assignments)]
    )
    result$facility_cost <- sum(facility_costs[result$selected])
    result$worker_cost_total <- sum(result$workers) * worker_cost
    result$objective <- result$transport_cost + result$facility_cost +
      result$worker_cost_total
  }

  end_time <- Sys.time()

  # --- Build output ---
  demand_result <- demand
  facilities_result <- facilities

  demand_result$.facility <- result$assignments

  facilities_result$.selected <- seq_len(n_fac) %in% result$selected
  facilities_result$.n_assigned <- result$n_assigned
  facilities_result$.workers <- result$workers
  facilities_result$.utilization <- result$utilizations

  output <- list(
    demand = demand_result,
    facilities = facilities_result
  )

  metadata <- list(
    algorithm = "orce",
    n_selected = result$n_selected,
    objective = result$objective,
    transport_cost = result$transport_cost,
    facility_cost = result$facility_cost,
    worker_cost_total = result$worker_cost_total,
    solve_time = as.numeric(difftime(end_time, start_time, units = "secs")),
    solver_status = result$status,
    tsp_iterations = tsp_iterations,
    tsp_converged = tsp_converged,
    peso_tsp = peso_tsp
  )

  attr(output, "spopt") <- metadata
  class(output) <- c("spopt_orce", "spopt_locate", "list")

  output
}
