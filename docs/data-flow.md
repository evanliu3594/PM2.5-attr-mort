# Data flow — inputs, mortality, aggregation and decomposition

Scope: describes v6.4 (2026-11-02). v6.3 replaced the per-group join chain
with the vectorised engine, added the decomposition input cache and the
single-pass aggregation; v6.4 removed the PWE `Uncertainty()` path. Timings and
bit-identity records live in `CHANGELOG.md` and `README.md`, not here.

## 1. Inputs and keys

`read_files()` loads five required files plus an optional `Conc_cf`: `Grid_info`,
`GridPop`, `GridPM25` (real exposure), `GBD_mortality`, `GBD_agestructure`. It
normalises keys to strings and aborts on duplicated or missing keys.

| Join | Keys | Precision | Where it happens |
|------|------|-----------|------------------|
| Grids ↔ Conc ↔ Pop | `x`, `y` | `matchable(dgt_grid)`, default 2 | precision set in `read_files()`; joined in `Mortality()` and `aggregate_range()` |
| Conc ↔ RR lookup | `concentration`, within endpoint × agegroup | `matchable(dgt_conc)`, default 1 | `set_Model()` (build), `Mortality()` (lookup) |
| Domain stats ↔ grid rows | `domain`; `mRate` also `endpoint` × `agegroup`; `AgeStruc` also `agegroup` | values must match between the Grids domain and `MortRate`/`AgeGroup` | `Mortality()` |
| Spatial aggregation | `at` column of Grid_info | string | `aggregate_range()`; `aggregate_sigma()` also takes `pwr_domain` for PWRR |

All join keys are strings (not floats) to avoid floating-point mismatch.
`matchable()` is `round(num, dgt) |> str_c()`. `dgt_conc` is a default, not a
constant: `Mortality()` aborts when the RR table's precision differs from
`.DGT_CONC`.

Only grids with a domain and finite `Pop`, `RR`, `PWRR`, `MortRate` and
`AgeStruc` enter the output; a missing concentration has no RR match and drops
the grid. Concentrations outside the RR range are clamped and reported.

## 2. `Mortality()`

```
 Grids ─┬─ Conc_c ─┬─ pop ──► base_c ──► RR numerator matrix
        │          │                 │
        └─ Conc_r ─┴─ pop ──► base_r ──► RR denominator matrix ──► PWRR tables
                                                     │
                              keep mask ──► one pass over endpoint × agegroup
                                                     │
                                                     ▼
                          x │ y │ endpoint_agegroup_CI │ ...   (wide grid table)
```

1. Build two grid-level base tables by joining `Grids`, the concentration column
   and `pop` on `x`, `y`. `base_c` feeds the numerator, `base_r` feeds the PWRR
   denominator; `Conc_c` defaults to `Conc_r`. Each table keeps `x`, `y`, the
   domain column, one concentration column and `Pop`. No long
   grid × endpoint × agegroup table is built.
2. Build one n_grid × group RR matrix per CI branch: `match(concentration)`
   inside each endpoint × agegroup group.
3. Build the PWRR tables: `PWRR = sum(RR × Pop) / sum(Pop)` per domain ×
   endpoint × agegroup, one table per PWRR branch. `pwr_branch = "branch"`
   (PM2.5 models) uses the matching branch; `pwr_branch = "mean"` (O3/NO2,
   configured in `Data/RR_std_config.json`) reuses the MEAN denominator.
4. Build one shared keep mask over grids: a domain and finite `Pop`, `RR`,
   `PWRR`, `MortRate` and `AgeStruc`.
5. One pass over the endpoint × agegroup groups writes the wide table:
   `Mort = Pop × AgeStruc × MortRate × (RR − 1) / PWRR / 1e5`.
   The zero-PM2.5 baseline is uniform inside a domain × endpoint × agegroup.

Output columns are `x`, `y` and one column per endpoint–agegroup per CI branch:
three for `CI = "RANGE"` (`_MEAN`, `_UP`, `_LOW`), one for a single CI. There is
no Total column.

`debug = TRUE` runs `mortality_join_path()` instead: the pre-v6.3 per-group
dplyr join chain used by `join_report()`. It is the reference/diagnostic path,
not the default.

## 3. Aggregation

`aggregate_range(x, at = "Country", by = "total", write = FALSE)` accepts one
`Mortality()` table or a named list of them. It builds one long table per call,
codes the `at` and scenario × CI keys as integers, sums each group with R-level
`sum()` in long-table row order, and pivots back to one column per scenario × CI.
`by` selects `"total"`, `"endpoint"`, `"agegroup"` or `"all"`. `write = TRUE`
writes each result to `./Results/`.

`aggregate_sigma()` is the only dedicated uncertainty estimator and supports
`by = "total"` only. With a named list it uses the supplied `_UP` / `_LOW`
columns; with a character vector it calls `Mortality_at(CI = "RANGE")`. It
converts the branches to deviations from the MEAN total and, with
`includeConc = TRUE`, adds a ±`Conc_ERR`% concentration perturbation (default
12%) in quadrature:

```
total_UP  = total_MEAN + sqrt(ci_up_cr^2  + ci_up_conc^2)
total_LOW = total_MEAN - sqrt(ci_low_cr^2 + ci_low_conc^2)
```

The `_UP` / `_LOW` columns are branch bounds, not a parametric ±σ: the RR
numerator uses the UP or LOW curve while the PWRR denominator follows the
model's `pwr_branch` setting. `aggregate_range()` sums each branch separately
across endpoint × agegroup, which treats branch deviations as perfectly
correlated. `Uncertainty()` was removed in v6.4; the entry scripts no longer
source it.

## 4. Decomposition

`DrivingFactors.R` calls `aggregate_drivers(RUN24 = TRUE)`. The 24 cumulative
factor sequences share 16 unique factor states. `decomp_input_cache()` builds
the state-independent inputs once: two RR concentration matrices (start/end) and
four PWRR tables (population start/end × concentration start/end), plus the
scenario vectors. `state_grid_cached()` assembles each state from that cache;
`state_summary_cached()` reduces it.

Since v6.3 the serial cached path wins when the engine is available, even with a
kernel plan active. Measured 6.4–7.0 s through the cache, against 12.3 s through
the old per-state dispatch. The kernel path remains only for checkouts without
the engine. The grid-set guard runs once per `aggregate_drivers()` run; the
clamp, orphan and coverage reports print once per run. Output: a 26-sheet
workbook (`1`–`24`, `Summary`, `Sequences`).

## 5. Entry scripts

| Script | Path |
|---|---|
| `HealthBurdenCalc.R` | `Mortality_at(CI = "RANGE")` per scenario → four `aggregate_range()` calls (total, endpoint, agegroup, endpoint+agegroup) → `./Results/` |
| `DrivingFactors.R` | `aggregate_drivers(RUN24 = TRUE)` → `./Results/` |
| `build_instance.R` | raw rasters → instance workbooks and `instance_manifest.json` |
