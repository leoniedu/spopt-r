# Solve SPENC regionalization problem

Spatially-encouraged spectral clustering.

## Usage

``` r
rust_spenc(attrs, n_regions, adj_i, adj_j, gamma, seed)
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

- gamma:

  RBF kernel parameter

- seed:

  Random seed

## Value

List with labels, n_regions, objective
