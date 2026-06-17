# Genre
This project records my up-to-date progress in assessing the PM<sub>2.5</sub> health burden according to PM<sub>2.5</sub> pollutions. The framework is global-scale and applies to any region with corresponding population, pollution, and baseline mortality data.

P.S.: the **PM2.5-attr-mort** refers to the **PM<sub>2.5</sub>-attributable-mortality**

# Supported Researches
1.  XING Z#, **LIU Y**#, CHEPELIEV M, et al. Global food trade can mitigate substantial health burdens attributed to ambient PM2.5 pollution[J]. Nature Food, 2026, 7(3): 223—233.

2. Tang R, Zhao J, **LIU Y**, et al. Air quality and health co-benefits of China's carbon dioxide emissions peaking before 2030[J]. Nature Communications, 2022, 13 (1): 1008.

3. **LIU Y**, Zhu G, Zhao Z, et al. Population aging might have delayed the alleviation of China's PM2.5 health burden[J]. Atmospheric Environment, 2022, 270: 118895.

4. Wang H, He X, Liang X, et al. Health benefits of on-road transportation pollution control programs in China[J]. Proceedings of the National Academy of Sciences, 2020, 117 (41): 25370-25377.

# Usage

## Quick start

1. Clone the repo and open `PM25-attr-mort.Rproj` in RStudio.

2. **Generate instance data** from raw sources (one-time setup):

   Edit the CONFIG section in `build_instance.R`, then source it:

   ```r
   source('build_instance.R')
   ```

   This produces five standardised `.xlsx` files under `./Data/`:
   | Output file | Content |
   |---|---|
   | `Grid_info_instance_*.xlsx` | Grid coordinates (`x`, `y`) with `Country` column |
   | `GridPM25_instance_*.xlsx` | Grid-level annual-average PM<sub>2.5</sub> (µg/m³) per scenario |
   | `GridPop_instance_*.xlsx` | Grid-level population counts per scenario |
   | `GBD_mortality_instance_*.xlsx` | Baseline mortality rate (deaths per 100,000) by country, endpoint, and age group |
   | `GBD_agestructure_instance_*.xlsx` | Proportion of population in each age group by country |

3. Choose a C-R model and run the calculation — open `HealthBurdenCalc.R` and step through:

   ```r
   set_Model('NCD+LRI')     # or '5COD', 'IER', 'MRBRT', 'O3', 'NO2'

   read_files(
     Grids     = './Data/Grid_info_instance_260617.xlsx',
     Pop       = './Data/GridPop_instance_260617.xlsx',
     Conc_real = './Data/GridPM25_instance_260617.xlsx',
     # Conc_cf = './Data/GridPM25_cf_instance_*.xlsx',  # omit for absolute burden
     MortRate  = './Data/GBD_mortality_instance_260617.xlsx',
     AgeGroup  = './Data/GBD_agestructure_instance_260617.xlsx',
     dgt_grid  = 2
   )

   scenarios <- names(Conc_real)[c(-1:-2)]

   # Grid-level mortality with 95% CI (single-pass MEAN + UP + LOW)
   grid_ci <- map(set_names(scenarios), ~ Mortality_at(at = .x, CI = "RANGE"))

   # Aggregate by country and endpoint
   aggregate_range(grid_ci, at = "Country", by = "endpoint", write = TRUE)
   ```

4. **Decomposition analysis** — `source('DrivingFactors.R')` to attribute mortality changes to Population Growth, Population Aging, Exposure change, and Other Risk Factors.

## Instance file format reference

If you prepare instance files manually (without `build_instance.R`), each file must follow the column format below. Coordinate columns accept any case variant of `x`/`lon`/`long`/`longitude` and `y`/`lat`/`latitude`. Domain columns accept `Country`/`country`/`location`/`Location` (auto-normalised to `domain` internally).

| File | Required columns | Content |
|------|-----------------|---------|
| **Grid_info** | `x`, `y`, plus geographic domain columns (e.g. `Country`, `Province`, `Region`) | Coordinate and hierarchical region assignment of each grid cell |
| **GridPop** | `x`, `y`, plus year/scenario columns (e.g. `base2015`, `SSP1-Baseline_2030`) | Grid-level population counts |
| **GridPM25** | `x`, `y`, plus year/scenario columns | Grid-level annual-average PM<sub>2.5</sub> concentration (µg/m³) |
| **GridPM25_cf** _(optional)_ | same as GridPM25 | Counterfactual PM<sub>2.5</sub>; omit `Conc_cf` argument for absolute burden mode |
| **GBD_mortality** | `Country` _(or `domain`)_, `endpoint`, `agegroup`, plus year/scenario columns | Baseline mortality rate (deaths per 100,000) by region, disease, and age group |
| **GBD_agestructure** | `Country` _(or `domain`)_, `agegroup`, plus year/scenario columns | Proportion of population in each age group by region |
| **RR_index** | sheets `MEAN`, `LOW`, `UP`; concentration column (name configurable via `conc_col` in `RR_std_config.json`) + `{endpoint}_{agegroup}` columns (e.g. `copd_25`) | Concentration–response lookup table, auto-selected by `set_Model()`. Regenerate via `Code/build_CR.R`. |

All gridded files must share the same `(x, y)` coordinate system. `read_files()` normalises coordinates to `matchable(dgt_grid)` precision regardless of whether they are stored as numeric or text. Concentration values are matched to the RR lookup table at the precision set by `dgt_conc`.

### Data flow — how sources are joined inside `Mortality()`

```
 ┌─────────────────────────────────────────────────────────────────────────────┐
 │                         INPUT DATA (6 sources)                              │
 ├─────────────────────────┬─────────────────────────┬─────────────────────────┤
 │  Grid_info              │  GridPop                │  GridPM25               │
 │  x │ y │ Country │ ...  │  x │ y │ base2015 │ ... │  x │ y │ base2015 │ ... │
 └────┬────────────────────┴────┬────────────────────┴────┬────────────────────┘
      │                         │                         │
      │  matchable(dgt_grid)    │  matchable(dgt_grid)    │  matchable(dgt_conc)
      │  → "113.25"             │  → "113.25"             │  → "36.8"
      │                         │                         │
      │                         │  getPop(at)             │  getConc_real(at)
      │                         │  select(x, y,           │  select(x, y,
      │                         │         Pop = base2015) │         concentration = base2015)
      │                         │                         │
      ▼                         ▼                         ▼
 ┌──────────────────────────────────────────────────────────────────┐
 │ STEP 1 — Grid-level join  (by = x, y)                            │
 │                                                                  │
 │   Grid_info ─┬─ Conc_r ─── Conc_c                                │
 │              ├─ Pop                                              │
 │              │   ↓  reduce(left_join)                            │
 │              │   ┌───────────────────────────────────────────┐   │
 │              │   │ x │ y │ Country │ concentration │ Pop │ ..│   │
 │              │   └───────────────────────────────────────────┘   │
 └──────────────┼───────────────────────────────────────────────────┘
                │
                │  by = concentration   (both sides: matchable(dgt=1), e.g. "36.8")
                ▼
 ┌───────────────────────────────────────────────────────────────────────┐
 │ STEP 2 — RR match  (by = concentration, endpoint, agegroup)           │
 │                                                                       │
 │   RR_index.xlsx          .RR_std_tbl  (built by set_Model())          │
 │   ┌─────────────┐        ┌──────────────────────────────────────────┐ │
 │   │ MEAN sheet  │        │ concentration │ endpoint │ age │ CI │ RR │ │
 │   │ LOW  sheet  │──────→ │    36.8       │ ncd+lri  │ 25  │ .. │ .. │ │
 │   │ UP   sheet  │RR_std()│    36.8       │ ncd+lri  │ 30  │ .. │ .. │ │
 │   └─────────────┘        └──────────────────────────────────────────┘ │
 │                                                                       │
 │   conc_col config → read from xlsx; output always "concentration"     │
 │   CI column ∈ {MEAN, UP, LOW}; Mortality() pivots/filters as needed   │
 └────────────────────────────┬──────────────────────────────────────────┘
                              │
     PWRR = weighted.mean(RR, Pop)  ←── grouped by domain (Country)
     Mort = Pop × AgeStruc × MortRate × (RR − 1) / PWRR / 1e5
                              │
                              ▼
 ┌────────────────────────────────────────────────────────────────────┐
 │ STEP 3 — Domain join  (by = domain, endpoint, agegroup)            │
 │                                                                    │
 │   GBD_mortality                 GBD_agestructure                   │
 │   getMortRate(at)               getAgeGroup(at)                    │
 │   ┌───────────────────┐         ┌──────────────────────────┐       │
 │   │ domain │ endpoint │         │ domain │ agegroup        │       │
 │   │        │ agegroup │         │        │ AgeStruc=base   │       │
 │   │        │ MortRate │         └───────────┬──────────────┘       │
 │   └────────┬──────────┘                     │                      │
 │            │                                │                      │
 │   by = domain, endpoint,          by = domain, agegroup            │
 │          agegroup                           |                      │
 │            │                                │                      │
 │            ▼                                ▼                      │
 │ ┌───────────────────────────────────────────────────────────────┐  │
 │ │ x │ y │ Country │ conc │ endpoint │ age │ RR │ Pop │ MortRate │  │
 │ │ . │ . │   ...   │  ..  │    ...   │ ... │ .. │ ... │   ...    │  │
 │ └───────────────────────────────────────────────────────────────┘  │
 └────────────────────────────────────────────────────────────────────┘
                              │
                              ▼
 ┌──────────────────────────────────────────────────────────────────┐
 │ OUTPUT — wide-format gridded mortality                           │
 │                                                                  │
 │   x │ y │ copd_25_MEAN │ copd_25_UP │ copd_25_LOW │ ... │ Total  │
 │   ──┼───┼──────────────┼────────────┼─────────────┼─────┼─────── │
 │                                                                  │
 │   Then aggregated by aggregate_range(at, by) → ./Result/         │
 └──────────────────────────────────────────────────────────────────┘
```

**Key matching rules:**

| Join | Keys | Precision | Where it happens |
|------|------|-----------|------------------|
| Grids ↔ Conc ↔ Pop | `x`, `y` | `matchable(dgt_grid)` — default 2 decimal places | `read_files()` |
| Conc ↔ RR lookup | `concentration` | `matchable(dgt_conc)` — default 1 decimal place, string type | `set_Model()` (build), `Mortality()` (join) |
| RR ↔ Mortality rates | `endpoint`, `agegroup` | endpoint = lowercase string, agegroup = `matchable(dgt=0)` | `Mortality()` |
| All ↔ Domain stats | `domain` (e.g. `Country`) | string, must match Grid_info column names | `Mortality()`, `Uncertainty()` |

All join keys are **strings** (not floats) to avoid floating-point mismatch. The `matchable()` helper does `round(dgt) |> as.character()`.

### C-R model configuration

```r
# Built-in models — lookup path and endpoint/agegroup config read from RR_std_config.json
set_Model('IER')         # IER2017
set_Model('NCD+LRI')     # GEMM NCD+LRI (composite endpoint)
set_Model('5COD')        # GEMM 5-COD (individual endpoints)
set_Model('MRBRT')       # MRBRT 2021
set_Model('O3')          # Ozone (COPD only)
set_Model('NO2')         # NO2 (all-cause)

# Custom RR lookup table — auto-generates endpoint/agegroup config and appends to JSON
set_Model("MyModel", path = "./Data/RR_index/My_Lookup.xlsx")
```

All model configuration is driven by `Data/RR_std_config.json`.

### Diagnostic parameters

`Mortality_at()` accepts two optional parameters for debugging:

```r
# Default (production): fast path, compact warnings
Mortality_at(at = "base2015", CI = "RANGE")

# Diagnostic mode: full NA reports + all domain names
Mortality_at(at = "base2015", CI = "RANGE", debug = TRUE, verbose = TRUE)
```

| Parameter | Default | Effect |
|-----------|---------|--------|
| `debug` | `FALSE` | `FALSE` = plain `left_join` (fast); `TRUE` = `join_report()` hierarchical NA diagnostics |
| `verbose` | `FALSE` | `FALSE` = truncate domain names to 6 in warnings; `TRUE` = show all |

# Release notes

## v5.0 (current)
- **Automated data preprocessing**: `build_instance.R` resamples all rasters to a common template grid, matches IHME country names via `llmjoin`, and produces five standardised instance files — replacing the old fishnet-based workflow
- **No fishnet dependency**: grid cells are matched by XY coordinates directly; geographic domains come from `terra::rasterize()` of Natural Earth polygons
- **PWRR three-phase guard**: Inf / <1 → hard error; NA → warn + auto-skip (remote territories); all-dropped → safeguard stop
- **`verbose` / `debug` parameters**: control warning verbosity and join diagnostics; both default to `FALSE` for production use
- **Type-agnostic coordinate matching**: `read_files()` handles both numeric and character `x`/`y` columns uniformly via `as.numeric()` + `any_of()`
- **Optional `Conc_cf`**: `read_files(Conc_cf)` defaults to `NULL`; omit for absolute burden mode
- **RR-substitution CI95**: `Mortality(CI = "RANGE")` computes MEAN/UP/LOW in a single pass
- **JSON-driven CR configuration**: `Data/RR_std_config.json` defines concentration column name, endpoints, age groups, lookup paths, and labels
- **Custom CRF support**: `set_Model("Name", path = "...")` auto-generates config from any lookup table
- **Auto PWRR domain detection**: `detect_domain()` matches mortality data domains against Grid_info columns
- **`aggregate_range()`**: RR-substitution CI aggregation, returns named list for pipe-friendly `pluck()` indexing
- **`aggregate_sigma()`**: error-propagation CI aggregation via `Uncertainty()`
- **Modular code structure**: model / data / mortality / uncertainty / aggregation
- **`cli`-based logging**: `log_msg(INFO/WARN/ERROR, ...)` for coloured terminal output

## v4.0
Extended to international / multi-region scale. Added O3 and NO<sub>2</sub> health burden
calculation. Added MRBRT method CRF as used by GBD 2019.

## v3.0
Refactored from matrix algebra to tidyverse join-based grammar, greatly improving
extensibility for larger-scale models.

## v2.0
Incorporated both GEMM and IER models for PM<sub>2.5</sub> health burden calculation,
using different concentration-response lookup tables.

## v1.0
Initial published version. IER-based PM<sub>2.5</sub> attributable mortality assessment
with refined, easy-to-use functions.
