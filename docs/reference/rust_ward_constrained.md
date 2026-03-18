# Solve spatially-constrained Ward clustering

Solve spatially-constrained Ward clustering

## Usage

``` r
rust_ward_constrained(attrs, n_regions, adj_i, adj_j)
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

## Value

List with labels, n_regions, objective
