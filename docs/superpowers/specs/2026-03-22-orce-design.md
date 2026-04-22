# ORCE: Warehouse Location Problem for spopt-r

## Overview

Add an `orce()` function to spopt-r that solves the warehouse location problem:
allocate demand points to facilities while minimizing transport costs, facility
opening costs, and worker costs. Worker count per facility is an integer decision
variable in the MIP.

This is a simplified version of the
[orce](https://github.com/orce-ibge/orce) R package, re-implemented with
spopt-r's Rust/HiGHS backend and following spopt-r's English nomenclature,
API conventions, and coding style.

### Simplifications relative to orce

- Single period (no multi-period support)
- No daily allowance (diária) calculation
- No TSP routing for geographic coherence
- No constraint application UI (block/force assignments)
- No daily allowance tiers
- Cost matrix is pre-computed externally (no built-in distance/duration → cost conversion)

## R Function Signature

```r
orce(
  demand,                # sf object: demand points
  facilities,            # sf object: candidate facilities
  weight_col,            # character: demand weight column in demand
  cost_matrix,           # numeric matrix: pre-computed transport cost (demand × facilities)
  facility_cost_col,     # character: fixed cost column in facilities
  worker_cost,           # numeric scalar: cost per worker (salary + training)
  worker_capacity,       # numeric scalar: max demand units one worker handles
  max_workers_col,       # character: per-facility max workers column in facilities
  min_workers = 1L,      # integer scalar: min workers if facility is open
  verbose = FALSE        # logical: print solver progress
)
```

### Key design decisions

- **No `n_facilities` parameter**: the solver always decides how many facilities
  to open based on the cost trade-off (unlike `cflp()` which supports both modes).
- **No `distance_metric`**: cost matrix is always pre-computed. Users who need a
  composite cost (distance + duration) build it externally or via a helper.
- **No `max_distance`**: dropped for v1 simplicity. Users can pre-filter their
  cost matrix if needed.
- **No demand splitting**: each demand point is assigned to exactly one facility
  (binary `x[i,j]`), matching orce's model. In Rust, `x[i,j]` must use
  `add_integer_column()` (not `add_column()` as in cflp which allows continuous
  splitting).
- **Cost matrix is not weight-multiplied**: unlike cflp where the objective
  coefficient is `weight[i] * cost[i,j]`, here `cost[i,j]` is the full
  pre-computed transport cost. The `weight_col` is used only in the capacity
  constraint (constraint 3).
- **`verbose` is R-side only**: prints problem dimensions and summary before
  calling Rust. Not passed to the Rust solver.
- **English throughout**: follows spopt-r convention. Portuguese context appears
  only in vignette examples.

## MIP Formulation

### Decision variables

| Variable | Type    | Description                              |
|----------|---------|------------------------------------------|
| `x[i,j]` | binary  | 1 if demand `i` assigned to facility `j` |
| `y[j]`   | binary  | 1 if facility `j` is open                |
| `w[j]`   | integer | number of workers at facility `j`        |

### Objective (minimize)

```
  Σ_ij  cost[i,j] * x[i,j]          # transport costs
+ Σ_j   fixed_cost[j] * y[j]        # facility opening costs
+ Σ_j   worker_cost * w[j]          # worker costs
```

### Constraints

1. **Assignment**: each demand assigned to exactly one facility
   `Σ_j x[i,j] = 1` for all `i`

2. **Linking**: can only assign to open facilities
   `x[i,j] ≤ y[j]` for all `i, j`

3. **Worker capacity**: total assigned demand ≤ workers × capacity
   `Σ_i weight[i] * x[i,j] ≤ worker_capacity * w[j]` for all `j`

4. **Min workers if open**:
   `min_workers * y[j] ≤ w[j]` for all `j`

5. **Max workers**:
   `w[j] ≤ max_workers[j] * y[j]` for all `j`

Constraint 5 also enforces `w[j] = 0` when facility `j` is closed.

## Return Structure

### Return value

A list with two sf objects:

```r
list(
  demand = demand_sf,
  facilities = facilities_sf
)
```

### Columns added to `demand`

| Column      | Type    | Description                        |
|-------------|---------|------------------------------------|
| `.facility` | integer | Index of assigned facility (1-based) |

### Columns added to `facilities`

| Column        | Type    | Description                              |
|---------------|---------|------------------------------------------|
| `.selected`   | logical | Whether facility is open                 |
| `.n_assigned` | integer | Number of demand points assigned         |
| `.workers`    | integer | Number of workers allocated              |
| `.utilization`| numeric | Assigned weight / (workers × worker_capacity) |

### Metadata (`attr(result, "spopt")`)

| Field              | Type    | Description                         |
|--------------------|---------|-------------------------------------|
| `algorithm`        | character | `"orce"`                          |
| `n_selected`       | integer | Number of open facilities            |
| `objective`        | numeric | Total cost                           |
| `transport_cost`   | numeric | Transport component                  |
| `facility_cost`    | numeric | Fixed cost component                 |
| `worker_cost_total` | numeric | Worker cost component               |
| `solve_time`       | numeric | Seconds                              |
| `solver_status`    | character | HiGHS status string                |

### Class

```r
c("spopt_orce", "spopt_locate", "list")
```

## Rust Module

### File

`src/rust/src/locate/orce.rs`

### Function signature

```rust
pub fn solve(
    cost_matrix: RMatrix<f64>,    // n_demand × n_fac
    weights: &[f64],              // demand weights [n_demand]
    facility_costs: &[f64],       // fixed costs [n_fac]
    worker_cost: f64,             // scalar
    worker_capacity: f64,         // scalar
    min_workers: i32,             // scalar
    max_workers: &[i32],          // per-facility [n_fac]
) -> List
```

### Return list fields

- `selected` — `Vec<i32>` (1-based facility indices)
- `assignments` — `Vec<i32>` (1-based, one per demand point)
- `workers` — `Vec<i32>` (worker count per facility)
- `n_selected` — `i32`
- `objective` — `f64` (total)
- `transport_cost` — `f64`
- `facility_cost` — `f64`
- `worker_cost_total` — `f64`
- `utilizations` — `Vec<f64>` (per facility)
- `status` — `String`
- Or `error` — `String` on failure

### Integration points

- `src/rust/src/locate/mod.rs` — add `pub mod orce;`
- `src/rust/src/lib.rs` — expose `rust_orce` via `extendr_module!`
- `R/extendr-wrappers.R` — auto-generated after `rextendr::document()`
- `R/cache.R` — add `rust_orce` to `spopt_solvers` environment and cacheable list

## File Layout

### New files

| File                              | Purpose                        |
|-----------------------------------|--------------------------------|
| `R/locate_orce.R`                | R wrapper function             |
| `src/rust/src/locate/orce.rs`    | Rust MIP solver                |
| `tests/testthat/test-orce.R`     | Tests                          |
| `vignettes/orce.qmd`            | Vignette                       |

### Modified files

| File                              | Change                         |
|-----------------------------------|--------------------------------|
| `src/rust/src/locate/mod.rs`     | Add `pub mod orce;`            |
| `src/rust/src/lib.rs`            | Expose `rust_orce`             |
| `R/cache.R`                      | Add to solver dispatch + cache |
| `NAMESPACE`                      | Auto-generated, exports `orce` |

## Validation (R wrapper)

Before calling Rust:

- `demand` and `facilities` are sf objects
- `weight_col`, `facility_cost_col`, `max_workers_col` exist in their respective sf objects
- No NAs in any input columns
- `cost_matrix` dimensions match `nrow(demand) × nrow(facilities)`
- `sanitize_cost_matrix()` on cost_matrix (replace NA/Inf with large finite values)
- `worker_cost > 0`, `worker_capacity > 0`, `min_workers >= 1`
- Warn if any facility has `max_workers[j] < min_workers` (facility can never open)
- Feasibility: `sum(max_workers[j] where max_workers[j] >= min_workers) * worker_capacity >= sum(weights)`
  (only count facilities that could actually open)

## Comparison with orce

### Objective components

| Component | orce | spopt-r `orce()` | Status |
|---|---|---|---|
| Transport cost | `Σ transport_cost_i_j[i,j] * x[i,j]` | `Σ cost[i,j] * x[i,j]` | Equivalent |
| Fixed facility cost | `Σ custo_fixo[j] * y[j]` | `Σ fixed_cost[j] * y[j]` | Equivalent |
| Worker salary | `remuneracao * Σ w[j]` (scalar) | `worker_cost * Σ w[j]` (scalar) | Equivalent |
| Per-facility training | `Σ custo_treinamento[j] * w[j]` | — | Dropped; fold into `facility_cost_col` |
| TSP routing penalty | `(fuel * weight_tsp / kml) * Σ dist * route` | — | Dropped by design |

### Constraints

| # | orce | spopt-r `orce()` | Status |
|---|---|---|---|
| 1 | `Σ_j x[i,j] = 1` | `Σ_j x[i,j] = 1` | Same |
| 2 | `x[i,j] ≤ y[j]` | `x[i,j] ≤ y[j]` | Same |
| 3 | `min_workers * y[j] ≤ w[j]` | `min_workers * y[j] ≤ w[j]` | Same |
| 4 | `Σ_i dct(i,j,t) * x[i,j] ≤ w[j] * max_days` (per period t) | `Σ_i weight[i] * x[i,j] ≤ worker_capacity * w[j]` | Single-period equivalent |
| 5 | `w[j] ≤ max_workers[j]` (when finite) | `w[j] ≤ max_workers[j] * y[j]` | Tighter — links to y[j], forces w=0 when closed |
| 6 | `Σ_i diarias[i,j] * x[i,j] ≤ max_diarias * w[j]` | — | Dropped by design |
| 7 | TSP/MTZ subtour elimination | — | Dropped by design |

### Features not implemented in v1

- Multi-period capacity constraints (orce iterates over periods `t = 1:p`)
- Daily allowance (diária) calculation and constraints
- Per-facility training cost (`custo_treinamento_por_entrevistador[j]`)
- TSP routing for geographic coherence (`weight_tsp`, `route[i,k,j]`, MTZ)
- Constraint application (`orce_aplicar_restricoes()`: block/force assignments)
- `n_entrevistadores_tipo` choice between continuous/integer workers (always integer here)

## Tests

`tests/testthat/test-orce.R`:

- Basic solve with known small problem
- Verify all output columns and metadata fields
- Infeasible problem returns error
- Cost decomposition: `objective ≈ transport_cost + facility_cost + worker_cost_total`
- **Comparison with orce package**: set up identical problem using `orce::orce()`
  with ROI/HiGHS backend, verify same facilities selected and same objective
  value (within solver tolerance). Wrapped in `skip_if_not_installed("orce")`.

## Vignette

`vignettes/orce.qmd`:

- Brief intro: warehouse location problem, relation to facility location
- Example with synthetic data
- How to build a cost matrix from distance + duration
- Compare result against `orce::orce()` on the same data
- Cost breakdown visualization
