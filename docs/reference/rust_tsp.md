# Solve Traveling Salesman Problem (TSP)

Solve a closed tour, open route, or fixed-end path over a square
cost/distance matrix using nearest-neighbor construction with optional
2-opt and or-opt local search. Optional time windows and service times
can be supplied for each stop.

## Usage

``` r
rust_tsp(cost_matrix, start, end, method, earliest, latest, service_time)
```

## Arguments

- cost_matrix:

  Square cost/distance matrix (n x n)

- start:

  Start index (0-based)

- end:

  End index (0-based), or NULL for an open route

- method:

  Algorithm: "nn" (nearest-neighbor only) or "2-opt" (with local search)

- earliest:

  Optional earliest service times

- latest:

  Optional latest service times

- service_time:

  Optional service times at each stop

## Value

List with tour (1-based), total_cost, nn_cost, improvement_pct,
iterations, arrival_time, and departure_time
