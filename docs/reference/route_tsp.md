# Traveling Salesman Problem (TSP)

Solves fixed-start routing problems over an `sf` layer. By default the
route is closed (start and end at the same location), but open routes
and fixed start/end paths are also supported. Optional time windows can
be supplied in the same units as the travel-time matrix.

## Usage

``` r
route_tsp(
  locations,
  depot = 1L,
  start = NULL,
  end = NULL,
  cost_matrix = NULL,
  distance_metric = "euclidean",
  method = "2-opt",
  earliest = NULL,
  latest = NULL,
  service_time = NULL
)
```

## Arguments

- locations:

  An sf object representing locations to visit.

- depot:

  Integer. Backward-compatible alias for `start` when `start` is not
  supplied. Defaults to 1.

- start:

  Integer. Row index of the route start in `locations` (1-based). If
  omitted, `depot` is used.

- end:

  Integer or NULL. Row index of the route end in `locations` (1-based).
  If omitted, defaults to `start` for a closed route. Set to NULL
  explicitly for an open route that may end at any stop.

- cost_matrix:

  Optional. Pre-computed square distance/cost matrix (n x n). If NULL,
  computed from `locations` using `distance_metric`.

- distance_metric:

  Distance metric when computing from geometry: "euclidean" (default) or
  "manhattan".

- method:

  Algorithm: "2-opt" (default, nearest-neighbor + local search) or "nn"
  (nearest-neighbor only).

- earliest:

  Optional earliest arrival/service times. Supply either a numeric
  vector of length `nrow(locations)` or a column name in `locations`.

- latest:

  Optional latest arrival/service times. Must be supplied together with
  `earliest`.

- service_time:

  Optional service duration at each stop. Supply either a numeric vector
  of length `nrow(locations)` or a column name in `locations`.

## Value

An sf object (the input `locations`) with added columns:

- `.visit_order`: Visit sequence (1 = route start)

- `.tour_position`: Position in the returned tour/path

- `.arrival_time`: Arrival/service start time at each visited stop

- `.departure_time`: Departure time after service

Metadata is stored in the "spopt" attribute, including `total_cost`,
`nn_cost`, `improvement_pct`, `tour`, `start`, `end`, `route_type`, and
`solve_time`.

## Details

Supported route variants:

1.  **Closed tour**: `start = 1, end = 1` (default)

2.  **Open route**: `start = 1, end = NULL`

3.  **Fixed path**: `start = 1, end = 5`

Time windows use the same units as `cost_matrix`. When windows are
supplied, the solver constructs a feasible route and only accepts
local-search moves that preserve feasibility.

## See also

[`route_vrp()`](https://walker-data.com/spopt-r/reference/route_vrp.md)
for multi-vehicle routing,
[`distance_matrix()`](https://walker-data.com/spopt-r/reference/distance_matrix.md)
for computing cost matrices
