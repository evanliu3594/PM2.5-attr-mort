# Genre
This project records my up-to-date progress in assessing the PM<sub>2.5</sub> health burden according to PM<sub>2.5</sub> pollutions across China. Theoretically, it also applies to other regions with corresponding population, pollution, and baseline mortality data.

P.S.: the **PM2.5-attr-mort** refers to the **PM<sub>2.5</sub>-attributable-mortality**

# Supported Researches
1.  XING Z#, **LIU Y**#, CHEPELIEV M, et al. Global food trade can mitigate substantial health burdens attributed to ambient PM2.5 pollution[J]. Nature Food, 2026, 7(3): 223—233.

2. Tang R, Zhao J, **LIU Y**, et al. Air quality and health co-benefits of China's carbon dioxide emissions peaking before 2030[J]. Nature Communications, 2022, 13 (1): 1008.

3. **LIU Y**, Zhu G, Zhao Z, et al. Population aging might have delayed the alleviation of China's PM2.5 health burden[J]. Atmospheric Environment, 2022, 270: 118895.

4. Wang H, He X, Liang X, et al. Health benefits of on-road transportation pollution control programs in China[J]. Proceedings of the National Academy of Sciences, 2020, 117 (41): 25370-25377.

# Usage

1. Clone the repo and open `PM25-attr-mort.Rproj` in RStudio.

2. Replace the sample data in `./Data/` with your own. Each file must follow the column format below. Coordinate columns accept any case variant of `x`/`lon`/`long`/`longitude` and `y`/`lat`/`latitude`.

| File | Required columns | Content |
|------|-----------------|---------|
| **GRID_information** | `x`, `y`, plus geographic domain columns (e.g. `Country`, `Province`, `Region`) | Coordinate and hierarchical region assignment of each grid cell |
| **GridPop** | `x`, `y`, plus year/scenario columns (e.g. `base2015`, `SSP1_2030`) | Grid-level population counts |
| **GridPM25** | `x`, `y`, plus year/scenario columns | Grid-level annual-average PM<sub>2.5</sub> concentration (µg/m³) |
| **GridPM25_cf** _(optional)_ | same as GridPM25 | Counterfactual PM<sub>2.5</sub> for policy scenario; if absent, falls back to GridPM25 |
| **GBD_mortality** | `domain`, `endpoint`, `agegroup`, plus year/scenario columns | Baseline mortality rate (deaths per 100,000) by region, disease, and age group |
| **GBD_agestructure** | `domain`, `agegroup`, plus year/scenario columns | Proportion of population in each age group by region |
| **RR_index** | sheets `MEAN`, `LOW`, `UP`; columns `concentration` + `{endpoint}_{agegroup}` (e.g. `copd_25`) | Concentration–response lookup table, auto-selected by `set_Model()`. See `Code/DataPrepare/Concentration_Response.R` to regenerate. |

All gridded files must share the same `(x, y)` precision (controlled by `dgt_grid` in `read_files()`). Concentration values are matched to the RR lookup table at the precision set by `dgt_conc`.

Specify filenames in `read_files()` within `HealthBurdenCalc.R`.

3. In `HealthBurdenCalc.R`, choose the C-R model with `set_Model()` and adjust file paths in `read_files()`.

4. Run line by line to compute grid-level results and aggregate.

# Release notes

## v5.0 (current)
- RR-substitution CI: `Mortality(CI = "RANGE")` computes MEAN/UP/LOW in a single pass
- Auto PWRR domain detection from mortality data and grid information
- Modular code structure: model / data / mortality / uncertainty / aggregation
- `aggregate_mort()` with `at`/`by`/`write` API for one-shot multi-level aggregation
- Unified coordinate handling via `normalize_coords()` (x/lon/long/latitude variants)
- Concentration clamping to CR table boundaries instead of dropping grids
- `Mort_Aggregate()` accepts scenario names directly and auto-computes MEAN internally

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
