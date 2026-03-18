# Solve Capacitated Vehicle Routing Problem (CVRP)

Solve Capacitated Vehicle Routing Problem (CVRP)

## Usage

``` r
rust_vrp(
  cost_matrix,
  depot,
  demands,
  capacity,
  max_vehicles,
  method,
  service_times,
  max_route_time,
  balance_time,
  earliest,
  latest
)
```

## Arguments

- cost_matrix:

  Square cost/distance matrix (n x n)

- depot:

  Depot index (0-based)

- demands:

  Demand at each location

- capacity:

  Vehicle capacity

- max_vehicles:

  Maximum number of vehicles (NULL for unlimited)

- method:

  Algorithm: "savings" or "2-opt"

- service_times:

  Optional service time at each stop (NULL for zero)

- max_route_time:

  Optional maximum total time per route (NULL for unlimited)

- balance_time:

  Whether to run route-time balancing phase

- earliest:

  Optional earliest arrival times at each stop

- latest:

  Optional latest arrival times at each stop

## Value

List with vehicle assignments, costs, and route details
