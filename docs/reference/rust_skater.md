# Solve SKATER regionalization

Solve SKATER regionalization

## Usage

``` r
rust_skater(attrs, adj_i, adj_j, n_regions, floor_var, floor_value, seed)
```

## Arguments

- attrs:

  Attribute matrix (n x p)

- adj_i:

  Row indices of adjacency

- adj_j:

  Column indices of adjacency

- n_regions:

  Number of regions to create

- floor_var:

  Optional floor variable values

- floor_value:

  Minimum floor value per region

- seed:

  Random seed

## Value

Vector of region labels (1-based)
