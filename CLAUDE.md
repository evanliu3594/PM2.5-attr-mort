# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project overview

PM2.5-attr-mort is an R project that estimates PM2.5-attributable mortality across gridded regions. It supports multiple concentration-response (C-R) models — IER, GEMM, MRBRT, O3, and NO2 — and can aggregate results by grid cell, province, region, or country. While the sample data targets China, the framework applies to any region given corresponding population, pollution, and baseline mortality data.

## Development environment

- Open `PM25-attr-mort.Rproj` in RStudio (the project root).
- Tab width: 2 spaces, UTF-8 encoding.
- The `.RData` workspace is saved/restored. All R scripts rely on `./Code/Core.R` being sourced first from the project root.

## Architecture

The computation pipeline works in four layers:

1. **Raw data ingestion** (`Code/Rawdata Process.R`) — converts NetCDF PM2.5 data, GeoTIFF population rasters, IHME CSV mortality/age-structure files, and natural-earth shapefiles into the standardized Excel instance files consumed by the main calculation.
2. **C-R lookup table generation** (`Code/Concentration_Response.R`) — takes CRF coefficient CSVs from `Data/CRF_coefficients/` and produces RR lookup tables (MEAN/LOW/UP) for every 0.1 µg/m³ step, written to `Data/RR_index/`.
3. **Core calculation engine** (`Code/Core.R`) — the heart of the project. All other analysis scripts source this file. It provides:
   - `set_Model()` / `tell_Model()` — select and name the C-R model (stored in global `.CR_Model`).
   - `read_files()` — load all input Excel files into global variables (`Grid_info`, `Pop`, `Conc_real`, `Conc_cf`, `MortRate`, `AgeGroup`, `RR_table`). The function auto-selects the correct RR lookup table based on `.CR_Model`.
   - `matchable()` — round-and-stringify helper used pervasively as a join key normalizer.
   - `getConc_real()`, `getPop()`, `getAgeGroup()`, `getMortRate()` — column extractors that select the column matching a given year/scenario name.
   - `RR_std()` — reshape the raw RR lookup table into a long-form `(concentration, endpoint, agegroup, RR)` table tailored to the active C-R model. Each model has different endpoint/age-group sets and fill rules.
   - `Mortality()` / `Mortality_at()` — the core attribution formula. Joins grid info, concentration, population, RR table, mortality rate, and age structure; computes PWRR (population-weighted relative risk) per domain; then calculates `Mort = Pop × AgeStruc × MortRate × (RR − 1) / PWRR / 1e5`. Returns a wide table with one column per endpoint–agegroup combination per grid cell.
   - `Uncertainty()` — analytic uncertainty propagation combining CR uncertainty (HIGH/LOW RR branches) and optional concentration RMSE uncertainty.
   - `Mort_Aggregate()` — aggregates gridded results to Grid/Province/Country/Region level, optionally by endpoint or agegroup, computes uncertainty CIs, and writes Excel output to `./Result/`.
4. **Application scripts** that source `Core.R` and run specific analyses:
   - `Code/HealthBurdenCalc.R` — the main user-facing workflow. Set model, load data, compute gridded mortality for all scenarios, aggregate.
   - `Code/DrivingFactors.R` — decomposition analysis attributing mortality changes between two time periods to Population Growth (PG), Population Aging (PA), Exposure change (EXP), and Other Risk Factors (ORF). Runs all 24 possible step-order permutations and averages them.
   - `Code/Core_MonteCarlo.R` — Monte Carlo uncertainty analysis using `furrr` for parallel draws. **Warning: extremely time-consuming.** Separate from `Core.R` because it reimplements the mortality calculation with random RR and concentration draws rather than wrapping the main pipeline.

## Key design patterns

- **Global mutable state**: `set_Model()` and `read_files()` write to `globalenv()`. Functions like `getPop()` implicitly read from those globals. Do not refactor to pure functions without updating all callers.
- **`matchable()` join keys**: Numeric join columns (coordinates, concentrations, ages) are rounded to a fixed digit count and converted to strings before joining. This avoids floating-point mismatches. The `dgt_grid` and `dgt_conc` parameters in `read_files()` control the precision.
- **`rlang` tidy-eval**: Column selection in `getConc_real()`, `getPop()`, etc. uses `{at}` and `{{domain}}` tidy-eval syntax. Be careful with quotation/unquotation when adding similar functions.
- **Counterfactual (`Conc_cf`) is optional**: When `Conc_cf` file does not exist, it's set to `NULL`, and `Mortality()` falls back to using `Conc_r` for both real and counterfactual concentrations (i.e., it computes the actual burden, not a scenario delta).

## CRF parameter files

Located in `Data/CRF_coefficients/`. Each CSV provides per-endpoint/age-group α, β, γ (or θ for GEMM), and tmrel values:

| File | Model | RR formula |
|------|-------|------------|
| `IER2010_parameters.csv` through `IER2017_parameters.csv` | IER | `RR = 1 + α × (1 − exp(−β × max(C − tmrel, 0)^γ))` |
| `GEMM-parameters.csv` | GEMM | `RR = exp(θ × log(1 + z/α) / (1 + exp((μ − z)/γ)))` where `z = max(C − 2.4, 0)` |

The lookup tables in `Data/RR_index/` are pre-built from these coefficients. Regenerate them via `Concentration_Response.R` when coefficients change.

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

This is implemented in `Mortality()` (see in-code comments at `Code/Core.R:301-320` for the full derivation).

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

## 聚合设计（`aggregate_mort` / `agg_mort`）

### 设计原则
聚合层采用双函数拆分：
- `agg_mort`是一个纯函数：零I/O、零对魔法值的分支、始终使用相同的管道。
- `aggregate_mort`仅进行调度和写入；不进行数据处理。
- 每个`at×by`组合都是一个显式的`agg_mort()`调用——循环仅用于用户提供的、代码时长度未知的列向量（例如`geo_cols`）。

- geo聚合层级包括：grid (c("x","y") 双键)、所有存在于grid_info中的column key（可能包括：x经度，y纬度，Country，Continent，region，province）

### `agg_mort(x, at_val, by_val)` — 纯工作函数

接收原始宽格式死亡率数据，转换为长格式，并为**一个**`at×by`组合进行聚合并写根据write参数判断是否写出到xlsx。没有含调度逻辑。

- `at_val`：在输出中保留的分组列。对于网格级别为`c("x", "y")`；对于经度为`"x"`，对于纬度为`"y"`，对于国家级别为`"Country"`。
- `by_val`：`"Total"`（无额外维度）、`"endpoint"`或`"agegroup"`。这些与`at`一起在`group_by`运算中分组。

```
group_by 键：at_val +（如果by_val != "Total"则为by_val）+ scenario/year + CI
列名：at_val + by_val（by_val == "Total"则没有这一列）+ scenario/year + CI
```

输出列示例：
- `at = "grid"` + `by = "Total"` → `x`、`y`、`base2015_MEAN`、`base2015_UP`、`SSP1_2030_MEAN`...
- `at = "y"` + `by = "endpoint"` → `y`、`endpoint`、`base2015_MEAN`、`SSP1_2030_UP`、`base2015_MEAN`...
- `at = "Country"` + `by = "agegroup"` → `Country`、`agegroup`、`base2015_MEAN`、`SSP1_2030_UP`...

### `aggregate_mort(x, at, by, write)` — 调度函数

一个Given-When-Then路由。展开缩写值并对每个具体组合调用`agg_mort`，将结果收集到命名列表中。并将write参数传输给agg_mort。

- `at = "geo"` → 展开为`c("grid", "x", "y", Country, Province, Region, ...)`
- `at = "grid"` → 映射为`at_val = c("x", "y")`
- `by = "all"` → 同时按 endpoint + agegroup 分解（一次调用，两维共同分组）
- `by = NULL`或`"total"` → 映射为`by_val = "Total"`

调度情况示例（为清晰起见硬编码）：
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
# Main health burden calculation — open HealthBurdenCalc.R in RStudio and run line-by-line.
# The script sources Core.R automatically.

# Regenerate RR lookup tables from CRF coefficients:
source('./Code/Concentration_Response.R')

# Run decomposition analysis:
source('./Code/DrivingFactors.R')
```

## Version history

- v1.0: Initial IER-based calculation.
- v2.0: Added GEMM model support.
- v3.0: Refactored from matrix algebra to tidyverse joins, improving extensibility.
- v4.0: Extended to multi-region/international scale; added O3, NO2, and MRBRT support.
- v5.0 (in progress): Built-in population/mortality data to reduce manual data inputs.
