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
#' # Which facilities are open?
#' result$facilities[result$facilities$.selected, ]
#'
#' # Cost breakdown
#' attr(result, "spopt")
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

  if (verbose) {
    message(sprintf(
      "ORCE: %d demand points, %d facilities (%d openable), min_workers=%d",
      n_demand, n_fac, sum(!unopenable), min_workers
    ))
  }

  # --- Solve ---
  start_time <- Sys.time()

  result <- spopt_solvers$rust_orce(
    cost_matrix,
    weights,
    facility_costs,
    worker_cost,
    worker_capacity,
    min_workers,
    max_workers
  )

  end_time <- Sys.time()

  if (!is.null(result$error)) {
    stop(result$error, call. = FALSE)
  }

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
    solver_status = result$status
  )

  attr(output, "spopt") <- metadata
  class(output) <- c("spopt_orce", "spopt_locate", "list")

  output
}
