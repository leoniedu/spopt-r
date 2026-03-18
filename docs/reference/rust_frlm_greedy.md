# Solve FRLM using greedy heuristic

Solve FRLM using greedy heuristic

## Usage

``` r
rust_frlm_greedy(
  n_candidates,
  path_candidates,
  path_offsets,
  path_distances,
  flow_volumes,
  vehicle_range,
  n_facilities
)
```

## Arguments

- n_candidates:

  Number of candidate facility locations

- path_candidates:

  Flat array of candidate indices for each path

- path_offsets:

  Start index for each path in path_candidates

- path_distances:

  Distances to each candidate along paths

- flow_volumes:

  Volume of each flow

- vehicle_range:

  Maximum vehicle range

- n_facilities:

  Number of facilities to place

## Value

List with selected facilities and coverage info
