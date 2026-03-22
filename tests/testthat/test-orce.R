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
    peso_tsp = 0,
    adicional_troca_jurisdicao = 0,
    rel_tol = 0,
    use_cache = FALSE
  )
  orce_obj <- attr(orce_result, "valor")

  expect_equal(spopt_obj, orce_obj, tolerance = 1e-4)
})
