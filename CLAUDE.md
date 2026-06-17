# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project overview

PM2.5-attr-mort is an R project that estimates PM2.5-attributable mortality across gridded regions. It supports multiple concentration-response (C-R) models — IER, GEMM, MRBRT, O3, and NO2 — and can aggregate results by grid cell, province, region, or country. While the sample data targets China, the framework applies to any region given corresponding population, pollution, and baseline mortality data.

## Development environment

- Open `PM25-attr-mort.Rproj` in RStudio (the project root).
- Tab width: 2 spaces, UTF-8 encoding.
- Application scripts (`HealthBurdenCalc.R`, `DrivingFactors.R`) source the five core modules directly from `./Code/`.

## Architecture

The computation pipeline works in four layers:

1. **Raw data ingestion** (`build_instance.R` at project root) — reads NetCDF PM2.5 data, GeoTIFF population rasters, IHME GBD CSV/ZIP mortality and age-structure files, and Natural Earth shapefiles. Resamples all rasters to a common template grid, matches IHME location names to Natural Earth country names via `llmjoin`, and outputs five standardised Excel instance files under `./Data/`.
2. **C-R lookup table generation** (`Code/build_CR.R`) — takes CRF coefficient CSVs from `Data/CRF_coefficients/` and produces RR lookup tables (MEAN/LOW/UP) for every 0.1 µg/m³ step, written to `Data/RR_index/`.
3. **Core calculation engine** — five modular files (`Code/model.R`, `Code/data.R`, `Code/mortality.R`, `Code/uncertainty.R`, `Code/aggregation.R`), sourced by application scripts. Key functions:
   - `set_Model()` / `tell_Model()` (model.R) — select and name the C-R model. `set_Model()` calls `RR_std()` to build the standardised long-form RR table (`concentration, endpoint, agegroup, CI, RR`) and stores it in the global `.RR_std_tbl`.
   - `RR_std()` (model.R) — builder: reads the RR lookup xlsx and `RR_std_config.json`, returns the standardised long-form table. Called by `set_Model()` once; downstream consumers read `.RR_std_tbl` directly.
   - `read_files()` (data.R) — loads gridded and domain input data into global env (`Grid_info`, `Pop`, `Conc_real`, `Conc_cf`, `MortRate`, `AgeGroup`). `Conc_cf` defaults to `NULL` for absolute-burden mode.
   - `matchable()` (data.R) — round-and-stringify helper used pervasively as a join key normalizer.
   - `normalize_coords()` (data.R) — renames `lon/lat/long/longitude/latitude` variants to canonical `x`/`y`.
   - `getConc_real()`, `getPop()`, `getAgeGroup()`, `getMortRate()` (data.R) — column extractors that select the column matching a given year/scenario name.
   - `Mortality()` / `Mortality_at()` (mortality.R) — the core attribution formula. Joins grid info, concentration, population, `.RR_std_tbl`, mortality rate, and age structure; filters/pivots CI column as needed; computes PWRR per domain; then calculates `Mort = Pop × AgeStruc × MortRate × (RR − 1) / PWRR / 1e5`. Returns a wide table with one column per endpoint–agegroup–CI combination per grid cell. Parameters: `verbose` (truncate domain names in warnings) and `debug` (enable `join_report()` hierarchical NA diagnostics).
   - `join_report()` (mortality.R) — left_join wrapper with hierarchical NA reporting (L1=domain entirely missing, L2=domain/endpoint, L3=specific). When `debug=FALSE`, short-circuits to a plain `left_join()`.
   - `detect_domain()` (mortality.R) — auto-detects the PWRR domain column by matching unique values between MortRate and Grid_info.
   - `Uncertainty()` (uncertainty.R) — analytic uncertainty propagation combining CR uncertainty (RR UP/LOW branches) and optional concentration RMSE uncertainty.
   - `aggregate_range()` (aggregation.R) — RR-substitution CI aggregation: takes `Mortality(CI="RANGE")` output, aggregates by `at` and `by`, keeps MEAN/UP/LOW branches as columns. Returns a named list (one element per `at` value).
   - `aggregate_sigma()` (aggregation.R) — error-propagation CI aggregation: takes `Mortality(CI="MEAN")` output, aggregates by `at` and `by`, derives CI bounds via `Uncertainty()`.
4. **Application scripts** at project root:
   - `HealthBurdenCalc.R` — the main user-facing workflow. Set model, load data, compute gridded mortality for all scenarios, aggregate.
   - `DrivingFactors.R` — decomposition analysis attributing mortality changes between two time periods to Population Growth (PG), Population Aging (PA), Exposure change (EXP), and Other Risk Factors (ORF). Runs all 24 possible step-order permutations and averages them.
   - `Code/experimental/Core_MonteCarlo.R` — Monte Carlo uncertainty analysis using `furrr` for parallel draws. **Warning: extremely time-consuming.** Reimplements the mortality calculation with random RR and concentration draws.

## Key design patterns

- **Global mutable state**: `set_Model()` and `read_files()` write to `globalenv()`. Functions like `getPop()` implicitly read from those globals. Do not refactor to pure functions without updating all callers.
- **`matchable()` join keys**: Numeric join columns (coordinates, concentrations, ages) are rounded to a fixed digit count and converted to strings before joining. This avoids floating-point mismatches. The `dgt_grid` and `dgt_conc` parameters in `read_files()` control the precision.
- **Coordinate type-agnostic matching**: `read_files()` normalises `x`/`y` columns via `across(any_of(c("x","y")), ~ matchable(as.numeric(.x), dgt = dgt_grid))`, so coordinates stored as either numeric or character in the xlsx will be consistently processed.
- **Domain column auto-detection**: `read_files()` accepts `Country`/`country`/`location`/`Location` as domain column variants in MortRate and AgeGroup files, normalising them to `domain` internally.
- **`rlang` tidy-eval**: Column selection in `getConc_real()`, `getPop()`, etc. uses `{at}` and `{{domain}}` tidy-eval syntax. Be careful with quotation/unquotation when adding similar functions.
- **Counterfactual (`Conc_cf`) is optional**: `Conc_cf` defaults to `NULL`. When absent, `Mortality()` falls back to using `Conc_r` for both real and counterfactual concentrations (computes absolute burden, not a scenario delta).
- **`verbose` / `debug` parameters**: `Mortality()` and `Mortality_at()` accept `verbose` (controls truncation of domain names in warnings; `FALSE` = max 6, `TRUE` = all) and `debug` (enables `join_report()` hierarchical NA diagnostics; `FALSE` = fast `left_join`). Both default to `FALSE`.

## CRF parameter files

Located in `Data/CRF_coefficients/`. Each CSV provides per-endpoint/age-group α, β, γ (or θ for GEMM), and tmrel values:

| File | Model | RR formula |
|------|-------|------------|
| `IER2010_parameters.csv` through `IER2017_parameters.csv` | IER | `RR = 1 + α × (1 − exp(−β × max(C − tmrel, 0)^γ))` |
| `GEMM-parameters.csv` | GEMM | `RR = exp(θ × log(1 + z/α) / (1 + exp((μ − z)/γ)))` where `z = max(C − 2.4, 0)` |

The lookup tables in `Data/RR_index/` are pre-built from these coefficients. Regenerate them via `Code/build_CR.R` when coefficients change.

## PWRR methodology in `Mortality()` and decomposition

### Why PWRR normalization (not standard grid-level PAF)

Baseline mortality rates (`MortRate`) are domain-level survey data (national or provincial statistics), not grid-specific rates. The standard grid-level PAF formula `(RR_g − 1) / RR_g` implies:

```
I₀ = MortRate_domain / RR_g    ← varies per grid (physically inconsistent)
```

The PWRR approach gives a uniform zero-PM₂.₅ baseline within each domain:

```
I₀ = MortRate_domain / PWRR_domain    ← same for all grids in the domain

where PWRR = Σ(Pop_g × RR(C_g)) / Σ(Pop_g)
```

The attributable mortality formula becomes:

```
ΔMort_g = Pop_g × AgeStruc × I₀ × (RR_g − 1)
        = Pop_g × AgeStruc × MortRate_domain × (RR_g − 1) / PWRR_domain
```

This is implemented in `Mortality()` (see in-code comments at `Code/mortality.R` for the full derivation).

### PWRR guard logic (three-phase)

`Mortality()` validates PWRR in three phases to distinguish data-coverage gaps from genuine data errors:

- **Phase 1 (hard error)**: Inf PWRR or PWRR < 1 → `stop()`. These indicate corrupted data.
- **Phase 2 (warn + skip)**: NA PWRR → `log_msg(WARN)` and `filter(!na_pwrr)`. Domains with no valid grids after `na.omit` (remote territories without PM2.5 or population data) are excluded automatically.
- **Phase 3 (all-dropped safeguard)**: If all domains were removed in Phase 2 → `stop()`.

### DrivingFactors.R: ORF factor design rationale

The decomposition uses four factors — PG (Population Growth), PA (Population Aging), EXP (Exposure), and ORF (Other Risk Factors) — with the Dietzenbacher 24-permutation averaging method.

The key design question is **why ORF changes both `Conc_r` (→ PWRR) and `MortRate` together**, rather than `MortRate` alone.

**The problem it solves**: `MortRate_domain` is the observed total mortality rate. It already contains deaths caused by PM₂.₅ exposure. When PM₂.₅ concentration changes between two years, the observed `MortRate` change includes both:

- ΔMortRate_healthcare (non-PM₂.₅ drivers: medical care, lifestyle, disease trends)
- ΔMortRate_PM2.5-induced (secondary effect of concentration change on total mortality)

If ORF only changed `MortRate` and left PWRR unchanged, the ΔMortRate_PM2.5-induced component would be double-counted — once through EXP (concentration → RR change) and once through ORF (embedded in MortRate change).

**The PWRR normalization as purification**: Under the modeling assumption `MortRate ≈ I₀ × PWRR`, the PM₂.₅-embedded portion of MortRate is proportional to PWRR. By changing both `MortRate` and `Conc_r` (→ PWRR) together in the ORF step, the two effects approximately cancel in the division:

```
I₀ = MortRate / PWRR
   = (MortRate_healthcare × MortRate_PM2.5) / PWRR
   ≈ MortRate_healthcare × constant
```

The resulting ΔI₀ reflects **only** non-PM₂.₅ baseline mortality changes — which is exactly what "other risk factors" should measure.

### Modeling assumptions and caveats

These are not logic errors, but premises of the decomposition framework that must be understood when interpreting results.

#### 1. `MortRate ≈ I₀ × PWRR` is a modeling assumption, not a data identity

`MortRate` comes from GBD/IHME survey statistics (death registration + model estimation). `PWRR` is computed from PM₂.₅ concentration grids × population weights × epidemiological RR functions (IER/GEMM). They are **independent data sources** — the relationship is not guaranteed to hold exactly in the data. Sources of discrepancy:

- RR_model (IER/GEMM) comes from global meta-analysis and may not precisely represent the domain's true exposure-response relationship
- Gridded concentrations have monitoring/simulation error
- Population weights from LandScan or similar products have their own error

When `PWRR_model` deviates from `PWRR_true`, the estimated `ΔI₀ = MortRate_new/PWRR_new − MortRate_old/PWRR_old` contains systematic bias, which flows into the ORF factor. The magnitude is typically 3%–8% given PWRR in the 1.2–1.8 range, but should be checked via sensitivity analysis (±10% PWRR perturbation) if ORF is a key finding.

#### 2. PG captures both population scaling and population redistribution

`Mortality()` always recomputes `PWRR = weighted.mean(RR, Pop)` with the Pop currently in scope. When PG changes population from start to end year:

```
PG_actual = pure_population_scaling + population_redistribution_effect
```

The redistribution effect arises because:
- Urban grids (high PM₂.₅, high RR) typically grow faster than rural grids (low PM₂.₅, low RR)
- As population weights shift toward high-RR grids, PWRR increases even if concentrations are unchanged
- This compositional effect is captured within PG through the PWRR recalculation

**This is intentional, not a bug.** Fixing PWRR to the start year during the PG step would break the symmetry of the 24-permutation framework — it would impose a fixed causal ordering where population redistribution is forbidden from interacting with other factors. The 24-permutation averaging is specifically designed to **absorb interaction terms without producing a residual**:

```
ΔTotal = PG_avg + PA_avg + EXP_avg + ORF_avg    (no residual term)
```

All factor×factor interactions (PG×PA, PG×EXP, PA×EXP, PG×PWRR, etc.) are distributed across the main effects through the averaging of all 24 replacement sequences. Attempting to "purify" PG by freezing PWRR would create an artificial residual that the framework cannot absorb.

#### 3. Total PM₂.₅ effect is split between EXP and ORF by design

When PM₂.₅ concentration changes, two parameters shift:

| Parameter | Role in formula | Allocated to |
|-----------|----------------|-------------|
| `Conc_c` | RR numerator `(RR − 1)` | EXP |
| `Conc_r` | PWRR denominator | ORF (as part of I₀ purification) |

EXP alone does **not** represent the full health impact of PM₂.₅ change. To obtain the complete concentration-driven mortality change, use the counterfactual scenario feature in `HealthBurdenCalc.R`:

```r
# Direct counterfactual comparison, not decomposition
total_exposure_effect = Mort(Conc_2015) − Mort(Conc_2030)
```

The decomposition is designed to answer "which driving factors explain the observed mortality change," not "what is the total effect of PM₂.₅." EXP and ORF share the concentration-driven component by construction.

#### 4. Age structure and mortality rates are not independent

PA changes `AgeStruc`; ORF changes `MortRate`. In reality, population aging shifts the disease spectrum, which in turn changes age-specific mortality rates. This PA×ORF interaction has no independent term in the 4-factor decomposition — it is distributed between PA and ORF through the 24-permutation average. This preserves additive completeness but means neither PA nor ORF can be interpreted as "pure" effects when the age structure and disease spectrum are shifting simultaneously.

## 聚合设计（`aggregate_range` / `aggregate_sigma`）

### 设计原则
两种聚合函数，对应两种 CI 计算方法：
- `aggregate_range`：RR 替换法。接受 `Mortality(CI="RANGE")` 的三分支输出（MEAN/UP/LOW），
  逐分支聚合后保留为列，写入 xlsx。返回命名列表（每个 `at` 值一个元素），支持 `pluck()` 管道索引。
- `aggregate_sigma`：误差传播法。接受 `Mortality(CI="MEAN")` 的均值输出，
  聚合后通过 `Uncertainty()` 计算 CI 边界。

### `aggregate_range(x, at, by, write)`

内部闭包 `aggregate_one(at_val, by_val)` 负责单个 `at×by` 组合的
pivot → group → bind → spread。调度层将 `at` 展开为列向量列表后用 `lapply` 调用，结果收集为命名列表返回。

- `at = "geo"`  → 展开为 grid (c("x","y")) + 所有 `Grid_info` 中的列名
- `at = "grid"` → 映射为 `at_val = c("x", "y")`
- `by` 限制为 `"total"` / `"endpoint"` / `"agegroup"` / `"all"`
- `by = "all"` → 同时按 endpoint + agegroup 分解（一次调用，两维共同分组）
- `by = NULL`或`"total"` → 仅 Total 列

调度情况示例：
```
at=geo  + by=all       → 对每个 geo 列：endpoint + agegroup（一次调用，两维共同分组）
at=geo  + by=Total     → 对每个 geo 列：Total
at=geo  + by=endpoint  → 对每个 geo 列：endpoint
at=geo  + by=agegroup  → 对每个 geo 列：agegroup

at=grid + by=all       → grid(c("x","y"))：endpoint + agegroup
at=grid + by=Total     → grid(c("x","y"))：Total
at=grid + by=endpoint  → grid(c("x","y"))：endpoint
at=grid + by=agegroup  → grid(c("x","y"))：agegroup

at=X    + by=all       → 对每个 X：endpoint + agegroup
at=X    + by=Y         → X × Y
```

## Common commands

```r
# Generate instance files from raw data — edit CONFIG section first
source('build_instance.R')

# Regenerate RR lookup tables from CRF coefficients:
source('./Code/build_CR.R')

# Main health burden calculation — open HealthBurdenCalc.R in RStudio and run line-by-line.

# Run decomposition analysis:
source('./DrivingFactors.R')
```

## Version history

- v1.0: Initial IER-based calculation.
- v2.0: Added GEMM model support.
- v3.0: Refactored from matrix algebra to tidyverse joins, improving extensibility.
- v4.0: Extended to multi-region/international scale; added O3, NO2, and MRBRT support.
- v5.0: Unified data preprocessing via `build_instance.R` (resample-to-template, llmjoin country matching). Removed fishnet dependency; all datasets aligned by XY coordinates on a common grid. Modular mortality engine with PWRR three-phase guard, `verbose`/`debug` parameters, type-agnostic coordinate normalisation, and optional `Conc_cf`.
