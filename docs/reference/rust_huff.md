# Compute Huff Model probabilities

Computes probability surface based on distance decay and attractiveness.
Formula: P_ij = (A_j × D_ij^β) / Σ_k(A_k × D_ik^β)

## Usage

``` r
rust_huff(cost_matrix, attractiveness, distance_exponent, sales_potential)
```

## Arguments

- cost_matrix:

  Cost/distance matrix (demand x stores)

- attractiveness:

  Attractiveness values for each store (pre-computed with exponents)

- distance_exponent:

  Distance decay exponent (typically negative, e.g., -1.5)

- sales_potential:

  Optional sales potential for each demand point

## Value

List with probabilities, market shares, expected sales
