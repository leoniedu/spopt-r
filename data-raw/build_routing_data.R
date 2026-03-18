# Build routing vignette data: delivery stops + travel-time matrix
# Uses r5r with the Tarrant/Parker County OSM extract

options(java.parameters = "-Xmx4G")
library(r5r)
library(sf)
library(dplyr)
library(purrr)
library(tidygeocoder)

# --- 1. Geocode delivery addresses ---
# Real addresses in west Fort Worth, Benbrook, Aledo, and Willow Park
# near the Amazon DDA9 depot at 3700 San Jacinto Dr

addresses <- tibble::tribble(
  ~id, ~address,
  # Depot
  "depot", "3700 San Jacinto Dr, Fort Worth, TX 76116",
  # Ridglea / near depot
  "D01", "6301 Camp Bowie Blvd, Fort Worth, TX 76116",
  "D02", "5800 Lovell Ave, Fort Worth, TX 76107",
  "D03", "4200 Birchman Ave, Fort Worth, TX 76107",
  "D04", "6100 Wester Ave, Fort Worth, TX 76116",
  "D05", "3516 Corto Ave, Fort Worth, TX 76109",
  # Western Hills / southwest
  "D06", "7400 Oakmont Blvd, Fort Worth, TX 76132",
  "D07", "7200 Oakmont Blvd, Fort Worth, TX 76132",
  "D08", "8517 Calmont Ave, Fort Worth, TX 76116",
  "D09", "3905 Las Vegas Trail, Fort Worth, TX 76116",
  "D10", "4701 Westcreek Dr, Fort Worth, TX 76133",
  # Benbrook
  "D11", "1100 Mercedes St, Benbrook, TX 76126",
  "D12", "1000 Winscott Rd, Benbrook, TX 76126",
  "D13", "9200 Westpark Dr, Benbrook, TX 76126",
  "D14", "3921 Benbrook Hwy, Fort Worth, TX 76116",
  "D15", "1201 Sproles Dr, Benbrook, TX 76126",
  # Aledo / Parker County
  "D16", "101 Bailey Ranch Rd, Aledo, TX 76008",
  "D17", "600 FM 5, Aledo, TX 76008",
  "D18", "709 FM 1187, Aledo, TX 76008",
  "D19", "300 Oak St, Aledo, TX 76008",
  "D20", "1001 Bankhead Hwy, Willow Park, TX 76087",
  # Willow Park / Hudson Oaks
  "D21", "100 Crown Pointe Blvd, Willow Park, TX 76087",
  "D22", "3200 Fort Worth Hwy, Hudson Oaks, TX 76087",
  "D23", "4801 Old Granbury Rd, Fort Worth, TX 76133",
  "D24", "8608 Chapin Rd, Fort Worth, TX 76116",
  "D25", "6500 Woodway Dr, Fort Worth, TX 76133"
)

cat("Geocoding", nrow(addresses), "addresses...\n")
geocoded <- addresses |>
  geocode(address, method = "osm", lat = lat, long = lon)

# Check for failures
failed <- geocoded |> filter(is.na(lat))
if (nrow(failed) > 0) {
  cat("Failed to geocode:\n")
  print(failed$address)
}

# Convert to sf
stops_sf <- geocoded |>
  filter(!is.na(lat)) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

cat("Geocoded", nrow(stops_sf), "of", nrow(addresses), "addresses\n")

# --- 2. Build r5r network and compute travel-time matrix ---
data_path <- file.path(getwd(), "data-raw")
r5r_core <- build_network(data_path = data_path)

# Prepare points for r5r
r5r_pts <- stops_sf |>
  st_coordinates() |>
  as_tibble() |>
  rename(lon = X, lat = Y) |>
  mutate(id = stops_sf$id)

# Square matrix: all stops to all stops (including depot)
cat("Computing travel-time matrix...\n")
ttm <- travel_time_matrix(
  r5r_core,
  origins = r5r_pts,
  destinations = r5r_pts,
  mode = "CAR",
  departure_datetime = as.POSIXct("2025-03-15 08:00:00"),
  max_trip_duration = 120,
  progress = TRUE
)

# Reshape to square matrix
# r5r uses our id column for from_id/to_id
id_order <- stops_sf$id

ttm_wide <- ttm |>
  select(from_id, to_id, travel_time_p50) |>
  tidyr::pivot_wider(names_from = to_id, values_from = travel_time_p50)

# Reorder rows and columns to match stops_sf order
ttm_wide <- ttm_wide |>
  arrange(match(from_id, id_order))

ttm_matrix <- ttm_wide |>
  select(-from_id) |>
  select(any_of(id_order)) |>
  as.matrix()

rownames(ttm_matrix) <- ttm_wide$from_id
colnames(ttm_matrix) <- colnames(ttm_matrix)

# Replace NAs with large value
ttm_matrix[is.na(ttm_matrix)] <- 999
diag(ttm_matrix) <- 0

cat("Matrix dimensions:", dim(ttm_matrix), "\n")
cat("Sample (first 5x5):\n")
print(ttm_matrix[1:5, 1:5])

# --- 3. Add package counts for VRP demo ---
# Uniform 3-5 packages per stop with capacity 35 gives a balanced 3-van split
set.seed(2026)
stops_sf <- stops_sf |>
  mutate(
    packages = ifelse(id == "depot", 0L, sample(3L:5L, n(), replace = TRUE))
  )

# --- 4. Solve routes and get road geometries ---
cat("Solving TSP and VRP for route geometries...\n")
devtools::load_all(quiet = TRUE)

tsp_result <- route_tsp(stops_sf, start = 1, cost_matrix = ttm_matrix)
tsp_tour <- attr(tsp_result, "spopt")$tour

vrp_result <- route_vrp(stops_sf, depot = 1, demand_col = "packages",
                        vehicle_capacity = 35, cost_matrix = ttm_matrix)
vrp_meta <- attr(vrp_result, "spopt")

# Get road-snapped route geometries from r5r
get_route_legs <- function(tour_indices, r5r_core) {
  map(seq_len(length(tour_indices) - 1), \(i) {
    detailed_itineraries(
      r5r_core,
      origins = r5r_pts[r5r_pts$id == stops_sf$id[tour_indices[i]], ],
      destinations = r5r_pts[r5r_pts$id == stops_sf$id[tour_indices[i + 1]], ],
      mode = "CAR",
      departure_datetime = as.POSIXct("2025-03-15 08:00:00"),
      shortest_path = TRUE
    )
  }) |>
    bind_rows()
}

cat("Getting TSP route geometries...\n")
tsp_route <- get_route_legs(tsp_tour, r5r_core)

cat("Getting VRP route geometries...\n")
vrp_route <- seq_len(vrp_meta$n_vehicles) |>
  map(\(v) {
    vehicle_stops <- vrp_result |>
      filter(.vehicle == v) |>
      arrange(.visit_order)

    vehicle_tour <- c(1, match(vehicle_stops$id, stops_sf$id), 1)
    get_route_legs(vehicle_tour, r5r_core) |>
      mutate(vehicle = v)
  }) |>
  bind_rows()

stop_r5(r5r_core)

# --- 5. Bundle and save ---
delivery_data <- list(
  stops = stops_sf,
  matrix = ttm_matrix,
  tsp_route = tsp_route,
  vrp_route = vrp_route
)

usethis::use_data(delivery_data, overwrite = TRUE)
cat("Saved delivery_data to data/delivery_data.rda\n")
