# Solve Capacitated Facility Location Problem (CFLP)

Minimize weighted distance subject to facility capacity constraints.
Supports fixed number of facilities or facility opening costs.

## Usage

``` r
rust_cflp(cost_matrix, weights, capacities, n_facilities, facility_costs)
```

## Arguments

- cost_matrix:

  Cost/distance matrix (demand x facilities)

- weights:

  Demand weights

- capacities:

  Capacity of each facility

- n_facilities:

  Number of facilities to locate (0 if using facility costs)

- facility_costs:

  Optional fixed cost to open each facility

## Value

List with selected facilities, assignments, utilizations
