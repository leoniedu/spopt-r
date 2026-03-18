# Solve AZP regionalization problem

Automatic Zoning Procedure with basic, tabu, and SA variants.

## Usage

``` r
rust_azp(
  attrs,
  n_regions,
  adj_i,
  adj_j,
  method,
  max_iterations,
  tabu_length,
  cooling_rate,
  initial_temperature,
  seed
)
```

## Arguments

- attrs:

  Attribute matrix (n x p)

- n_regions:

  Number of regions to create

- adj_i:

  Row indices of adjacency (0-based)

- adj_j:

  Column indices of adjacency (0-based)

- method:

  "basic", "tabu", or "sa"

- max_iterations:

  Maximum iterations

- tabu_length:

  Tabu list length (for tabu method)

- cooling_rate:

  SA cooling rate (for sa method)

- initial_temperature:

  SA initial temperature (for sa method)

- seed:

  Random seed

## Value

List with labels, n_regions, objective
