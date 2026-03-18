# Compute minimum spanning tree from adjacency matrix

Compute minimum spanning tree from adjacency matrix

## Usage

``` r
rust_mst(i, j, weights, n)
```

## Arguments

- i:

  Row indices (0-based) of adjacency matrix non-zero entries

- j:

  Column indices (0-based) of adjacency matrix non-zero entries

- weights:

  Edge weights (distances/dissimilarities)

- n:

  Number of nodes

## Value

List with MST edges (from, to, weight)
