# Internal helper: TSP-based sequencing of zones and tracts
sequence_zones <- function(zones_result, partition_data, partition_col) {
  if (!requireNamespace("seriation", quietly = TRUE)) {
    stop("Package 'seriation' is required for sequence = TRUE. Install with: install.packages('seriation')",
         call. = FALSE)
  }
  zones_result$.zone_order <- NA_integer_
  zones_result$.tract_order <- NA_integer_

  for (comp_id in unique(zones_result$.component)) {
    comp_mask <- which(zones_result$.component == comp_id)
    zone_ids <- sort(unique(zones_result$.center[comp_mask]))
    n_zones <- length(zone_ids)
    pd <- partition_data[[comp_id]]

    # Identify members per zone (indices into comp_mask)
    zone_members <- lapply(zone_ids, function(zid) {
      which(zones_result$.center[comp_mask] == zid)
    })

    # Zone-level ordering via TSP Hamiltonian path (seriation)
    if (n_zones <= 2) {
      zone_tour <- seq_len(n_zones)
    } else {
      # Inter-zone distance = min pairwise distance between zone members
      zone_dm <- matrix(0, n_zones, n_zones)
      for (a in seq_len(n_zones)) {
        for (b in seq_len(n_zones)) {
          if (a != b) {
            local_a <- match(comp_mask[zone_members[[a]]], pd$idx)
            local_b <- match(comp_mask[zone_members[[b]]], pd$idx)
            zone_dm[a, b] <- min(pd$cm[local_a, local_b])
          }
        }
      }
      o <- seriation::seriate(stats::as.dist(zone_dm), method = "TSP")
      zone_tour <- seriation::get_order(o)
    }

    # Assign zone_order and within-zone TSP + orientation in a single pass
    prev_exit <- NULL
    for (k in seq_along(zone_tour)) {
      z <- zone_tour[k]
      members <- comp_mask[zone_members[[z]]]
      zones_result$.zone_order[members] <- k

      if (length(members) >= 3) {
        local_idx <- match(members, pd$idx)
        intra_cm <- pd$cm[local_idx, local_idx]
        o_intra <- seriation::seriate(stats::as.dist(intra_cm), method = "TSP")
        intra_tour <- seriation::get_order(o_intra)
        ordered_members <- members[intra_tour]
      } else {
        ordered_members <- members
      }

      # Orient: if previous zone exists, check whether reversing brings
      # entry closer to previous zone's exit (surveyzones approach)
      if (!is.null(prev_exit) && length(ordered_members) > 1) {
        first <- ordered_members[1]
        last <- ordered_members[length(ordered_members)]
        local_first <- match(first, pd$idx)
        local_last <- match(last, pd$idx)
        local_prev <- match(prev_exit, pd$idx)
        d_first <- pd$cm[local_prev, local_first]
        d_last <- pd$cm[local_prev, local_last]
        if (d_last < d_first) {
          ordered_members <- rev(ordered_members)
        }
      }

      zones_result$.tract_order[ordered_members] <- seq_along(ordered_members)
      prev_exit <- ordered_members[length(ordered_members)]
    }
  }

  # Build composite zone_id: [partition_]zone_order.tract_order (with leading zeros)
  max_tract <- max(zones_result$.tract_order, na.rm = TRUE)
  tract_width <- nchar(as.character(max_tract))
  tract_fmt <- paste0("%0", tract_width, "d")
  tract_part <- sprintf(tract_fmt, zones_result$.tract_order)
  if (!is.null(partition_col)) {
    zones_result$.zone_id <- paste0(
      zones_result$.partition, "_",
      zones_result$.zone_order, ".",
      tract_part
    )
  } else {
    zones_result$.zone_id <- paste0(zones_result$.zone_order, ".", tract_part)
  }

  zones_result
}

#' Allocate Zones
#'
#' Partitions geographic units into zones, each with a center, such that no
#' unit is farther than `max_distance` from its center. Automatically finds
#' the minimum number of centers needed via LSCP, then assigns units using
#' one of three methods.
#'
#' @param zones An sf object where each feature is both a demand point and
#'   a candidate center.
#' @param max_distance Numeric. Maximum allowable distance from a zone to its
#'   assigned center.
#' @param method Assignment method after K discovery:
#'   \describe{
#'     \item{`"nearest"`}{Assign each zone to the nearest selected center.
#'       Fastest. No weights or capacity.}
#'     \item{`"p_median"`}{Solve a p-median MILP to minimize total weighted
#'       distance. Uses `weight_col` if provided.}
#'     \item{`"cflp"`}{Solve a capacitated facility location MILP. Requires
#'       `capacity`. Increments K beyond the LSCP minimum if needed.}
#'   }
#' @param weight_col Optional character. Column name in `zones` containing
#'   demand weights (e.g., population, workload). If NULL, all weights are 1.
#'   Used by `"p_median"` and `"cflp"` methods.
#' @param capacity Optional numeric. Maximum total weight assignable to a
#'   single center. Only used when `method = "cflp"`. If `weight_col` is NULL,
#'   this is the maximum number of zones per center.
#' @param partition_col Optional character. Column name in `zones` that defines
#'   independent partitions. Each partition is solved separately.
#' @param cost_matrix Optional. Pre-computed square distance matrix (n x n).
#' @param distance_metric Distance metric: "euclidean" (default) or "manhattan".
#'   Ignored if `cost_matrix` is provided.
#' @param sequence Logical. If TRUE, order zones and tracts via TSP for
#'   geographic sequencing. Adds `.zone_order` (visit order of zones) and
#'   `.tract_order` (visit order of tracts within each zone) columns.
#'   Inter-zone distances use minimum pairwise distance between zone members.
#' @param verbose Logical. Print progress messages.
#'
#' @return A list with a single sf object:
#'   \itemize{
#'     \item `$zones`: Original sf with added columns:
#'       \itemize{
#'         \item `.center`: Index of the assigned center zone
#'         \item `.is_center`: TRUE if this zone was selected as a center
#'         \item `.distance`: Distance to assigned center
#'         \item `.component`: Partition ID (sequential integer)
#'       }
#'       When `method = "cflp"`, a `.split` column is added (TRUE if demand
#'       is split across centers). When `sequence = TRUE`, `.zone_order` and
#'       `.tract_order` columns give TSP-based visit ordering.
#'   }
#'   Metadata is stored in the "spopt" attribute.
#'
#' @details
#' All methods use LSCP (Location Set Covering Problem) to find the minimum
#' number of centers K* such that every zone has a center within `max_distance`.
#' They differ in how assignments are made:
#'
#' \describe{
#'   \item{`"nearest"`}{Greedy nearest-center assignment. One MILP solve.
#'     Equivalent to `"p_median"` when all weights are equal.}
#'   \item{`"p_median"`}{Lexicographic: minimize K (LSCP), then minimize
#'     total weighted distance (p-median). Two MILP solves per partition.}
#'   \item{`"cflp"`}{Like `"p_median"` but with capacity constraints.
#'     If LSCP's K* is infeasible under capacity, K is incremented until
#'     a feasible solution is found. Two or more MILP solves per partition.}
#' }
#'
#' @examples
#' \dontrun{
#' library(sf)
#'
#' zones <- st_as_sf(data.frame(
#'   x = c(0, 0.1, 0.2, 10, 10.1, 10.2),
#'   y = c(0, 0.1, 0.2, 0, 0.1, 0.2),
#'   pop = rep(100, 6)
#' ), coords = c("x", "y"))
#'
#' # Fast: LSCP + nearest
#' allocate_zones(zones, max_distance = 1)
#'
#' # Optimal weighted: LSCP + p-median
#' allocate_zones(zones, max_distance = 1, method = "p_median", weight_col = "pop")
#'
#' # Capacitated: max 2 zones per center
#' allocate_zones(zones, max_distance = 1, method = "cflp", capacity = 2)
#' }
#'
#' @seealso [p_median()] and [cflp()] for standard facility location with
#'   separate demand/facilities and user-specified K
#'
#' @export
allocate_zones <- function(zones,
                           max_distance,
                           method = c("nearest", "p_median", "cflp"),
                           weight_col = NULL,
                           capacity = NULL,
                           partition_col = NULL,
                           cost_matrix = NULL,
                           distance_metric = "euclidean",
                           sequence = FALSE,
                           verbose = FALSE) {
  method <- match.arg(method)

  # --- Input validation ---
  if (!inherits(zones, "sf")) {
    stop("`zones` must be an sf object", call. = FALSE)
  }
  if (!is.null(weight_col) && !weight_col %in% names(zones)) {
    stop(paste0("Weight column '", weight_col, "' not found in zones"), call. = FALSE)
  }
  validate_max_distance(max_distance)
  if (method == "cflp") {
    if (is.null(capacity)) {
      stop("`capacity` is required when method = \"cflp\"", call. = FALSE)
    }
    if (!is.numeric(capacity) || length(capacity) != 1 ||
        is.na(capacity) || capacity <= 0) {
      stop("`capacity` must be a single positive number", call. = FALSE)
    }
  }
  if (!is.null(partition_col)) {
    if (!partition_col %in% names(zones)) {
      stop(paste0("Partition column '", partition_col, "' not found in zones"),
           call. = FALSE)
    }
  }

  n <- nrow(zones)
  if (!is.null(weight_col)) {
    all_weights <- as.numeric(zones[[weight_col]])
    if (any(is.na(all_weights))) {
      stop("Weight column contains NA values", call. = FALSE)
    }
  } else {
    all_weights <- rep(1.0, n)
  }

  start_time <- Sys.time()

  # --- Initialize result columns ---
  zones_result <- zones
  zones_result$.center <- NA_integer_
  zones_result$.is_center <- FALSE
  zones_result$.distance <- NA_real_
  zones_result$.component <- NA_integer_
  if (method == "cflp") {
    zones_result$.split <- FALSE
  }

  if (!is.null(partition_col)) {
    zones_result$.partition <- zones[[partition_col]]
  }

  total_objective <- 0
  partition_data <- list()  # store cm + idx per component for sequencing
  total_n_selected <- 0L
  total_n_split <- 0L
  component_counter <- 0L

  # --- Compute full cost matrix if provided ---
  if (!is.null(cost_matrix)) {
    full_cm <- sanitize_cost_matrix(cost_matrix)
  } else {
    full_cm <- NULL
  }

  # --- Determine partitions ---
  if (!is.null(partition_col)) {
    partition_values <- unique(zones[[partition_col]])
  } else {
    partition_values <- list(NULL)
  }

  for (pval in partition_values) {
    if (!is.null(pval)) {
      idx <- which(zones[[partition_col]] == pval)
      if (verbose) message(sprintf("Partition '%s': %d zones", pval, length(idx)))
    } else {
      idx <- seq_len(n)
    }

    # Compute or subset cost matrix
    if (is.null(full_cm)) {
      cm <- distance_matrix(zones[idx, ], zones[idx, ], type = distance_metric)
      cm <- sanitize_cost_matrix(cm)
    } else {
      cm <- full_cm[idx, idx, drop = FALSE]
    }

    weights_part <- all_weights[idx]
    n_part <- length(idx)
    component_counter <- component_counter + 1L

    # Stage 1: LSCP — find minimum K
    lscp_result <- rust_lscp(cm, max_distance)
    k_star <- lscp_result$n_selected

    if (verbose) message(sprintf("  LSCP: K*=%d", k_star))

    if (k_star == 0L) {
      stop(sprintf(
        "LSCP found no feasible covering at max_distance = %g%s",
        max_distance,
        if (!is.null(pval)) sprintf(" in partition '%s'", pval) else ""
      ), call. = FALSE)
    }

    # Stage 2: Assignment
    if (method == "nearest") {
      # Nearest-center assignment using LSCP-selected facilities
      selected_local <- lscp_result$selected
      dists_to_selected <- cm[, selected_local, drop = FALSE]
      nearest_idx <- apply(dists_to_selected, 1, which.min)
      assignments_local <- selected_local[nearest_idx]
      distances_local <- dists_to_selected[cbind(seq_along(idx), nearest_idx)]
      objective_local <- sum(weights_part * distances_local)

    } else if (method == "p_median") {
      result <- rust_p_median(cm, weights_part, as.integer(k_star), max_distance)
      if (length(result$selected) == 0 || is.nan(result$objective)) {
        stop(sprintf("P-median infeasible at K=%d%s",
                     k_star,
                     if (!is.null(pval)) sprintf(" in partition '%s'", pval) else ""),
             call. = FALSE)
      }
      selected_local <- result$selected
      assignments_local <- result$assignments
      distances_local <- cm[cbind(seq_along(idx), assignments_local)]
      objective_local <- result$objective

      if (verbose) {
        message(sprintf("  P-median: K=%d, objective=%.2f", k_star, objective_local))
      }

    } else {
      # method == "cflp"
      capacities_part <- rep(capacity, n_part)
      result <- NULL

      k_lb <- max(k_star, ceiling(sum(weights_part) / capacity))
      for (k in k_lb:n_part) {
        cflp_result <- rust_cflp(cm, weights_part, capacities_part,
                                 as.integer(k), NULL, max_distance)
        if (is.null(cflp_result$error)) {
          result <- cflp_result
          if (verbose) message(sprintf("  CFLP: K=%d feasible (objective=%.2f)",
                                       k, result$objective))
          break
        }
        if (verbose) message(sprintf("  CFLP: K=%d infeasible", k))
      }

      if (is.null(result)) {
        stop(sprintf("No feasible capacitated solution found%s",
                     if (!is.null(pval)) sprintf(" in partition '%s'", pval) else ""),
             call. = FALSE)
      }

      selected_local <- result$selected
      assignments_local <- result$assignments
      distances_local <- cm[cbind(seq_along(idx), assignments_local)]
      objective_local <- result$objective

      # Detect split demand
      alloc <- matrix(result$allocation_matrix, nrow = n_part, ncol = n_part,
                      byrow = TRUE)
      for (i in seq_len(n_part)) {
        if (alloc[i, assignments_local[i]] < 0.999) {
          zones_result$.split[idx[i]] <- TRUE
          total_n_split <- total_n_split + 1L
        }
      }
    }

    # Map back to original indices
    zones_result$.center[idx] <- idx[assignments_local]
    zones_result$.is_center[idx[selected_local]] <- TRUE
    zones_result$.distance[idx] <- distances_local
    zones_result$.component[idx] <- component_counter

    total_objective <- total_objective + objective_local
    total_n_selected <- total_n_selected + length(selected_local)

    # Store data for sequencing
    if (sequence) {
      partition_data[[component_counter]] <- list(cm = cm, idx = idx)
    }
  }

  # --- TSP sequencing ---
  if (sequence) {
    zones_result <- sequence_zones(zones_result, partition_data, partition_col)
  }

  end_time <- Sys.time()

  output <- list(zones = zones_result)

  total_weight <- sum(all_weights)
  metadata <- list(
    algorithm = paste0("allocate_zones/", method),
    method = method,
    max_distance = max_distance,
    n_selected = total_n_selected,
    n_partitions = length(partition_values),
    objective = total_objective,
    mean_distance = total_objective / total_weight,
    partition_col = partition_col,
    solve_time = as.numeric(difftime(end_time, start_time, units = "secs"))
  )
  if (method == "cflp") {
    metadata$capacity <- capacity
    metadata$n_split <- total_n_split
  }

  attr(output, "spopt") <- metadata
  class(output) <- c("spopt_allocate_zones", "spopt_locate", "list")

  output
}
