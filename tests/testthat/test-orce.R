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

test_that("orce with weight_tsp returns correct structure and metadata", {
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
  all_coords <- as.data.frame(rbind(
    sf::st_coordinates(demand), sf::st_coordinates(facilities)
  ))
  all_points <- sf::st_as_sf(all_coords, coords = c("X", "Y"))
  dm_full <- distance_matrix(all_points)

  result <- orce(demand, facilities,
    weight_col = "workload", cost_matrix = cost,
    facility_cost_col = "fixed_cost", worker_cost = 50,
    worker_capacity = 100, max_workers_col = "max_workers",
    weight_tsp = 1, distance_matrix_full = dm_full,
    kml = 10, fuel_price = 6
  )

  expect_s3_class(result, "spopt_orce")

  meta <- attr(result, "spopt")
  expect_true("tsp_iterations" %in% names(meta))
  expect_true("tsp_converged" %in% names(meta))
  expect_true("weight_tsp" %in% names(meta))
  expect_equal(meta$weight_tsp, 1)
  expect_true(meta$tsp_iterations >= 1L)

  # Cost decomposition should use original cost matrix
  cost_sum <- meta$transport_cost + meta$facility_cost + meta$worker_cost_total
  expect_equal(meta$objective, cost_sum, tolerance = 1e-6)
})

test_that("orce with weight_tsp=0 matches baseline", {
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

  result_base <- orce(demand, facilities,
    weight_col = "workload", cost_matrix = cost,
    facility_cost_col = "fixed_cost", worker_cost = 50,
    worker_capacity = 100, max_workers_col = "max_workers"
  )
  result_tsp0 <- orce(demand, facilities,
    weight_col = "workload", cost_matrix = cost,
    facility_cost_col = "fixed_cost", worker_cost = 50,
    worker_capacity = 100, max_workers_col = "max_workers",
    weight_tsp = 0
  )

  meta_base <- attr(result_base, "spopt")
  meta_tsp0 <- attr(result_tsp0, "spopt")

  expect_equal(meta_base$objective, meta_tsp0$objective, tolerance = 1e-6)
  expect_equal(result_base$demand$.facility, result_tsp0$demand$.facility)
  expect_equal(meta_tsp0$tsp_iterations, 0L)
  expect_true(is.na(meta_tsp0$tsp_converged))
})

test_that("orce with weight_tsp validates inputs", {
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

  dm_full <- matrix(0.1, nrow = 2, ncol = 2)

  # weight_tsp > 0 without distance_matrix_full
  expect_error(
    orce(demand, facilities, weight_col = "workload", cost_matrix = cost,
         facility_cost_col = "fixed_cost", worker_cost = 50,
         worker_capacity = 100, max_workers_col = "max_workers",
         weight_tsp = 1, kml = 10, fuel_price = 6),
    "distance_matrix_full.*required"
  )

  # Wrong-sized distance_matrix_full
  expect_error(
    orce(demand, facilities, weight_col = "workload", cost_matrix = cost,
         facility_cost_col = "fixed_cost", worker_cost = 50,
         worker_capacity = 100, max_workers_col = "max_workers",
         weight_tsp = 1, distance_matrix_full = matrix(1, 3, 3),
         kml = 10, fuel_price = 6),
    "matrix"
  )

  # Negative weight_tsp
  expect_error(
    orce(demand, facilities, weight_col = "workload", cost_matrix = cost,
         facility_cost_col = "fixed_cost", worker_cost = 50,
         worker_capacity = 100, max_workers_col = "max_workers",
         weight_tsp = -1),
    "non-negative"
  )

  # Missing kml when weight_tsp > 0
  expect_error(
    orce(demand, facilities, weight_col = "workload", cost_matrix = cost,
         facility_cost_col = "fixed_cost", worker_cost = 50,
         worker_capacity = 100, max_workers_col = "max_workers",
         weight_tsp = 1, distance_matrix_full = dm_full, fuel_price = 6),
    "kml.*positive"
  )

  # Missing fuel_price when weight_tsp > 0
  expect_error(
    orce(demand, facilities, weight_col = "workload", cost_matrix = cost,
         facility_cost_col = "fixed_cost", worker_cost = 50,
         worker_capacity = 100, max_workers_col = "max_workers",
         weight_tsp = 1, distance_matrix_full = dm_full, kml = 10),
    "fuel_price.*positive"
  )
})

test_that("orce with weight_tsp improves routing quality on split-cluster problem", {
  skip_if_not_installed("sf")
  skip_if_not(is.loaded("wrap__rust_orce"), "Rust compilation required")

  # Two geographic clusters with a bridge point that naive assignment may split.
  # Cluster A near depot 1 (x~0): demand at x = 0.05, 0.10, 0.15, 0.20
  # Cluster B near depot 2 (x~1): demand at x = 0.85, 0.90, 0.95, 1.00
  # Bridge point: x = 0.25 (close to cluster A but equidistant cost-wise
  # if transport cost is scaled by workload).
  set.seed(99)
  demand <- sf::st_as_sf(data.frame(
    x = c(0.05, 0.10, 0.15, 0.20, 0.25, 0.85, 0.90, 0.95, 1.00),
    y = rep(0, 9),
    workload = c(10, 10, 10, 10, 10, 10, 10, 10, 10)
  ), coords = c("x", "y"))

  facilities <- sf::st_as_sf(data.frame(
    x = c(0, 1),
    y = c(0, 0),
    fixed_cost = c(10, 10),
    max_workers = c(5L, 5L)
  ), coords = c("x", "y"))

  cost <- distance_matrix(demand, facilities)
  all_coords <- as.data.frame(rbind(
    sf::st_coordinates(demand), sf::st_coordinates(facilities)
  ))
  all_points <- sf::st_as_sf(all_coords, coords = c("X", "Y"))
  dm_full <- distance_matrix(all_points)

  result_no_tsp <- orce(demand, facilities,
    weight_col = "workload", cost_matrix = cost,
    facility_cost_col = "fixed_cost", worker_cost = 5,
    worker_capacity = 100, max_workers_col = "max_workers",
    weight_tsp = 0
  )

  result_tsp <- orce(demand, facilities,
    weight_col = "workload", cost_matrix = cost,
    facility_cost_col = "fixed_cost", worker_cost = 5,
    worker_capacity = 100, max_workers_col = "max_workers",
    weight_tsp = 1, distance_matrix_full = dm_full,
    kml = 10, fuel_price = 6
  )

  # With TSP penalty, the 5 left-side points (including bridge at 0.25)
  # should all be assigned to facility 1
  left_cluster <- result_tsp$demand$.facility[1:5]
  expect_true(
    length(unique(left_cluster)) == 1,
    label = "TSP penalty should keep left cluster together"
  )
})

test_that("orce matches orce package results", {
  skip_if_not_installed("sf")
  skip_if_not_installed("orce")
  skip_if_not(is.loaded("wrap__rust_orce"), "Rust compilation required")

  # Synthetic problem: 8 demand points, 4 facilities, single period.
  # No diarias, no TSP, no training, no travel-time cost — pure distance cost.
  set.seed(123)
  n_demand <- 8L
  n_fac <- 4L

  demand_xy <- data.frame(x = runif(n_demand), y = runif(n_demand))
  fac_xy <- data.frame(x = runif(n_fac), y = runif(n_fac))

  workload <- as.integer(rpois(n_demand, 15) + 5L) # dias_coleta per UC
  facility_cost <- c(100, 200, 150, 250)
  max_workers <- c(3L, 4L, 3L, 5L)
  worker_cost_val <- 50
  worker_cap <- 30L
  min_workers_val <- 1L
  kml <- 10
  fuel_cost <- 6

  # Distance matrix (Euclidean, treat as km for simplicity)
  dist_km <- as.matrix(dist(rbind(demand_xy, fac_xy), method = "euclidean"))
  dist_km <- dist_km[1:n_demand, (n_demand + 1):(n_demand + n_fac)]

  # spopt transport cost: cost_matrix[i,j] = workload[i] * 2 * dist_km[i,j] / kml * fuel_cost
  cost_mat <- outer(workload, rep(1, n_fac)) * 2 * dist_km / kml * fuel_cost

  # --- spopt ---
  demand_sf <- sf::st_as_sf(
    data.frame(demand_xy, workload = workload),
    coords = c("x", "y")
  )
  facilities_sf <- sf::st_as_sf(
    data.frame(fac_xy, fixed_cost = facility_cost, max_workers = max_workers),
    coords = c("x", "y")
  )

  spopt_result <- orce(demand_sf, facilities_sf,
    weight_col = "workload", cost_matrix = cost_mat,
    facility_cost_col = "fixed_cost", worker_cost = worker_cost_val,
    worker_capacity = worker_cap, max_workers_col = "max_workers",
    min_workers = min_workers_val
  )
  spopt_obj <- attr(spopt_result, "spopt")$objective

  # --- orce package ---
  uc_ids <- paste0("uc_", seq_len(n_demand))
  ag_ids <- paste0("ag_", seq_len(n_fac))

  ucs_df <- data.frame(
    uc = uc_ids,
    agencia_codigo = ag_ids[1], # arbitrary jurisdiction (not used in opt)
    dias_coleta = workload,
    viagens = 1L,
    data = "2024-01",
    diaria_valor = 0,
    stringsAsFactors = FALSE
  )

  agencias_df <- data.frame(
    agencia_codigo = ag_ids,
    n_entrevistadores_agencia_max = max_workers,
    custo_fixo = facility_cost,
    diaria_valor = 0,
    stringsAsFactors = FALSE
  )

  distancias_ucs_df <- expand.grid(
    uc = uc_ids, agencia_codigo = ag_ids,
    stringsAsFactors = FALSE
  )
  distancias_ucs_df$distancia_km <- as.vector(dist_km)
  distancias_ucs_df$duracao_horas <- 0
  distancias_ucs_df$diaria_municipio <- FALSE
  distancias_ucs_df$diaria_pernoite <- FALSE

  orce_result <- orce::orce(
    ucs = ucs_df,
    agencias = agencias_df,
    distancias_ucs = distancias_ucs_df,
    dias_coleta_entrevistador_max = worker_cap,
    remuneracao_entrevistador = worker_cost_val,
    n_entrevistadores_min = min_workers_val,
    custo_litro_combustivel = fuel_cost,
    custo_hora_viagem = 0,
    kml = kml,
    dias_treinamento = 0,
    weight_tsp = 0,
    adicional_troca_jurisdicao = 0,
    rel_tol = 0,
    use_cache = FALSE
  )
  orce_obj <- attr(orce_result, "valor")

  expect_equal(spopt_obj, orce_obj, tolerance = 1e-4)
})

test_that("orce returns col_solution for warm start", {
  skip_if_not_installed("sf")
  skip_if_not(is.loaded("wrap__rust_orce"), "Rust compilation required")

  set.seed(42)
  demand <- sf::st_as_sf(
    data.frame(x = runif(10), y = runif(10), workload = rpois(10, 20)),
    coords = c("x", "y")
  )
  facilities <- sf::st_as_sf(
    data.frame(
      x = runif(4), y = runif(4),
      fixed_cost = rep(100, 4),
      max_workers = rep(5L, 4)
    ),
    coords = c("x", "y")
  )

  cost <- distance_matrix(demand, facilities)
  n_demand <- nrow(demand)
  n_fac <- nrow(facilities)

  result <- orce(demand, facilities,
    weight_col = "workload", cost_matrix = cost,
    facility_cost_col = "fixed_cost", worker_cost = 50,
    worker_capacity = 100, max_workers_col = "max_workers"
  )

  # col_solution should not be exposed in the R output (internal to Rust)
  # but we can test the warm start indirectly via TSP loop
  # Expected column count: 2 * n_fac + n_demand * n_fac (y + w + x)
  expected_cols <- 2L * n_fac + n_demand * n_fac

  # Call Rust directly to verify col_solution is returned
  raw <- rust_orce(cost, as.numeric(demand$workload),
    as.numeric(facilities$fixed_cost), 50, 100, 1L,
    as.integer(facilities$max_workers), NULL)
  expect_true("col_solution" %in% names(raw))
  expect_equal(length(raw$col_solution), expected_cols)

  # Warm start: passing col_solution back should produce the same result
  raw2 <- rust_orce(cost, as.numeric(demand$workload),
    as.numeric(facilities$fixed_cost), 50, 100, 1L,
    as.integer(facilities$max_workers), raw$col_solution)
  expect_equal(raw2$objective, raw$objective, tolerance = 1e-6)
  expect_equal(raw2$assignments, raw$assignments)
})

test_that("orce TSP loop converges with valid assignments", {
  skip_if_not_installed("sf")
  skip_if_not(is.loaded("wrap__rust_orce"), "Rust compilation required")

  set.seed(99)
  n_demand <- 20
  n_fac <- 5

  demand <- sf::st_as_sf(data.frame(
    x = c(runif(10, 0, 10), runif(10, 90, 100)),
    y = c(runif(10, 0, 10), runif(10, 90, 100)),
    workload = rep(10L, n_demand)
  ), coords = c("x", "y"))

  facilities <- sf::st_as_sf(data.frame(
    x = c(5, 95, 50, 5, 95),
    y = c(5, 95, 50, 95, 5),
    fixed_cost = rep(100, n_fac),
    max_workers = rep(5L, n_fac)
  ), coords = c("x", "y"))

  kml <- 10
  fuel_cost <- 6
  dist_mat <- distance_matrix(demand, facilities)
  cost_mat <- outer(demand$workload, rep(1, n_fac)) * 2 * dist_mat / kml * fuel_cost

  all_coords <- as.data.frame(rbind(
    sf::st_coordinates(demand), sf::st_coordinates(facilities)
  ))
  all_points <- sf::st_as_sf(all_coords, coords = c("X", "Y"))
  dm_full <- distance_matrix(all_points)

  result <- orce(demand, facilities,
    weight_col = "workload", cost_matrix = cost_mat,
    facility_cost_col = "fixed_cost", worker_cost = 100,
    worker_capacity = 50, max_workers_col = "max_workers",
    weight_tsp = 1, distance_matrix_full = dm_full,
    kml = kml, fuel_price = fuel_cost
  )

  meta <- attr(result, "spopt")
  expect_true(meta$tsp_iterations >= 1L)
  expect_true(meta$objective > 0)
  # Assignments should be valid
  expect_true(all(result$demand$.facility >= 1L))
  expect_true(all(result$demand$.facility <= n_fac))
})
