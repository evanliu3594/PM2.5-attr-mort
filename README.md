# PM2.5-attr-mort

Grid-level PM<sub>2.5</sub>-attributable mortality for any region with gridded
concentration, population, and baseline mortality data. The framework supports
IER, GEMM (NCD+LRI, 5-COD), GBD MR-BRT (2019/2021/2023), ozone, and
NO<sub>2</sub> concentration–response curves. It estimates PWRR-normalised
attributable deaths with confidence intervals, aggregates by region and
cause/age, and decomposes mortality change into population growth, population
ageing, exposure change, and other risk factors.

## Requirements

R 4.6 or newer. The commands below use the R 4.6.2 install path
`C:\Program Files\R\R-4.6.2\bin\Rscript.exe`. Adjust the path to your R
install. Install the required packages:

```r
install.packages(c("tidyverse", "readxl", "writexl", "jsonlite"))
```

Concurrency is optional. Enabling the kernel needs the `future` package; the
parallel backend also needs `furrr` and, by default, `future.callr`. The default
sequential run needs none of them.

## Repository layout

| Path | Content |
|---|---|
| `Data/` | Sample instance (260617), RR lookup tables, CRF coefficients, raw downloads |
| `Code/` | Calculation modules: model, data, mortality, aggregation, decomposition, kernel |
| `HealthBurdenCalc.R` | Entry script: grid-level mortality and national aggregates |
| `DrivingFactors.R` | Entry script: 24-sequence decomposition |
| `build_instance.R` | One-time builder: raw rasters to instance files |
| `Results/`, `Figures/` | Outputs (gitignored) |
| `tests/` | Local regression tests (gitignored) |
| `handoff/` | Local session handoffs (gitignored) |
| `docs/` | Programme design docs: [data flow](docs/data-flow.md), [superpowers plans](docs/superpowers/plans/) |
| `docs/` | Background notes: [data flow](docs/data-flow.md) |

## Quick start

### 1. Input data

The repository ships a sample instance dated 2026-06-17
(`Data/*_instance_260617.xlsx`). Both entry scripts list the five input paths in
an `# Input data` block; edit those paths to use another instance.

`build_instance.R` rebuilds the instance from raw rasters and writes
`Data/instance_manifest.json`; `resolve_instance_files()` reads that manifest for
scripts that prefer it. File formats and rebuild steps are in
[`Data/README.md`](Data/README.md).

### 2. Health burden

Edit the model and input paths at the top of `HealthBurdenCalc.R`, then run:

```powershell
& "C:\Program Files\R\R-4.6.2\bin\Rscript.exe" HealthBurdenCalc.R
```

The script writes national aggregates to `Results/`. The same calculation can be
driven directly after sourcing the modules used by the entry script:

```r
set_Model("NCD+LRI")

GRIDS_FILE     <- "./Data/Grid_info_instance_260617.xlsx"
POP_FILE       <- "./Data/GridPop_instance_260617.xlsx"
CONC_REAL_FILE <- "./Data/GridPM25_instance_260617.xlsx"
MORT_RATE_FILE <- "./Data/GBD_mortality_instance_260617.xlsx"
AGE_GROUP_FILE <- "./Data/GBD_agestructure_instance_260617.xlsx"

read_files(
  Grids = GRIDS_FILE, Pop = POP_FILE, Conc_real = CONC_REAL_FILE,
  MortRate = MORT_RATE_FILE, AgeGroup = AGE_GROUP_FILE,
  dgt_grid = 2, conc_unit = "ug/m3"
)

scenarios <- get_scenarios()
grid_ci <- map(set_names(scenarios), ~ Mortality_at(at = .x, CI = "RANGE"))
aggregate_range(grid_ci, at = "Country", by = "endpoint", write = TRUE)
```

`grid_ci` is one row per grid with `MEAN`, `UP`, and `LOW` columns per endpoint
and age group. `aggregate_range()` accepts `by = "total"`, `"endpoint"`,
`"agegroup"`, or `"all"`.

### 3. Decomposition

Edit the configuration block in `DrivingFactors.R` (`MODEL`, `START_YEAR`,
`END_YEAR`, `RESULT_AT`, `RUN24`, `WORKERS`), then source or run the script. It
writes 24 sequence sheets plus `Summary` and `Sequences` to `Results/`.

```r
Sys.setenv(PM25_RUN_DRIVERS = "false")   # load data and functions only
source("DrivingFactors.R")

Decomposition(serie = 1, start.y = "base2015", end.y = "SSP1-Baseline_2030")
```

Decomposition functions live in `Code/decomposition.R`.

## Input files

If you prepare instance files manually, each file follows the format below.
Coordinate columns accept any case variant of `x`/`lon`/`long`/`longitude` and
`y`/`lat`/`latitude`. Domain columns accept `Country`/`country`/`location`/
`Location` and are normalised to `domain` internally.

| File | Required columns | Content |
|------|-----------------|---------|
| **Grid_info** | `x`, `y`, plus domain columns (e.g. `Country`, `Province`) | Grid coordinates and region labels |
| **GridPop** | `x`, `y`, plus scenario columns | Grid-level population |
| **GridPM25** | `x`, `y`, plus scenario columns | Annual-average PM<sub>2.5</sub> (µg/m³) |
| **GridPM25_cf** _(optional)_ | same as GridPM25 | Counterfactual PM<sub>2.5</sub>; omit for absolute burden |
| **GBD_mortality** | `domain`, `endpoint`, `agegroup`, plus scenario columns | Baseline mortality rate (deaths per 100,000) |
| **GBD_agestructure** | `domain`, `agegroup`, plus scenario columns | Population age structure |
| **RR_index** | sheets `MEAN`/`LOW`/`UP`; concentration column plus `{endpoint}_{agegroup}` columns | Concentration–response lookup, selected by `set_Model()` |

Gridded files must share one `(x, y)` grid. `read_files()` rounds coordinates to
`dgt_grid` digits and concentrations to the fixed 0.1 grid. Duplicate keys stop
the load. Join rules are in [`docs/data-flow.md`](docs/data-flow.md).

`Data/RR_std_config.json` records the lookup path, endpoints, age groups,
concentration unit, and valid range for every model.

## C-R models

```r
set_Model("IER")           # IER2017
set_Model("NCD+LRI")       # GEMM NCD+LRI (composite endpoint)
set_Model("5COD")          # GEMM 5-COD (individual endpoints)
set_Model("MRBRT")         # GBD 2023 MR-BRT (default alias)
set_Model("MRBRT2023")     # GBD 2023 MR-BRT (explicit)
set_Model("O3")            # GBD 2023 ozone (COPD)
set_Model("NO2")           # GBD 2023 NO2 (asthma)
set_Model("O3_legacy")     # legacy ozone lookup
set_Model("NO2_allcause")  # legacy NO2 all-cause lookup

set_Model("MyModel", path = "./Data/RR_index/My_Lookup.xlsx")  # custom table
```

The concentration grid is fixed at 0.1: `set_Model()` rejects a lookup whose
steps are finer than 0.1, and `read_files()` rounds exposure to the same grid.
Custom tables define their endpoints and age groups and append an entry to
`RR_std_config.json`.

## Diagnostics

| Parameter | Default | Effect |
|---|---|---|
| `debug` | `FALSE` | `FALSE` = vectorised engine; `TRUE` = the pre-v6.3 join path with `join_report()` diagnostics |
| `verbose` | `FALSE` | `FALSE` = truncate domain names to 6; `TRUE` = show all |

```r
Mortality_at("base2015", CI = "RANGE", debug = TRUE, verbose = TRUE)
```

## Uncertainty

The only uncertainty path is `aggregate_sigma()` (and the `_UP` / `_LOW`
columns of `aggregate_range()`). Both take the CR branch bounds from
`Mortality(CI = "RANGE")`: each bound re-evaluates the same estimand under the
UP or LOW RR curve. They are not a parametric ±σ, and the aggregation sums the
branch columns across endpoint × agegroup, which treats those deviations as
perfectly correlated.

The PWE-based `Uncertainty()` was removed in v6.4. PAF at population-weighted
exposure equals the PWRR estimator only when the RR curve is affine in
concentration or the domain exposure is uniform; on the 260617 instance the PWE
central estimate runs +1.8% to +2.1% above the PWRR total.

## Concurrency

`Code/kernel.R` is an optional concurrency layer on the R Future API. The default
is sequential. Enable it for a session with:

```r
source("Code/kernel.R")
kernel_plan(workers = 4)   # callr by default; falls back to multisession
kernel_off()               # restore the sequential plan
```

Only `kernel_plan()` enables parallel dispatch: a future plan installed by
another package (for example `future.apply` or `targets`) leaves
`kernel_active()` `FALSE`, so kernel calls run serially, while
`kernel_workers()` and `kernel_info()` still report that plan. If `future` is
missing, `kernel_plan()` stops; if `furrr` is missing, the backend falls back and
the run stays sequential.

`DrivingFactors.R` reads `PM25_KERNEL_WORKERS` (default `1`); `HealthBurdenCalc.R`
stays sequential. Since v6.3 the decomposition builds its 16 unique states from a
serial input cache, and that cache is faster than dispatch: measured 6.4 s serial
versus 12.3 s with four workers. A plan is still honoured for checkouts without
the engine, but the cached path wins when the engine is present. `Mortality()`
stays sequential.

Measured on the 260617 instance (NCD+LRI, 68,302 grids, quiet window): a single
`Mortality()` call with `validate_keys = FALSE` takes about 0.83 s MEAN and 1.25 s
RANGE (2.3 s and 2.7 s in v6.2); the 24-sequence serial run computes in about
6.4 s and the `DrivingFactors.R` wall clock is about 12 s (39.9–44.2 s in v6.2);
`HealthBurdenCalc.R` takes about 11.5 s wall (17.8 s in v6.2). Outputs are
identical across backends and to the pre-optimisation run (maximum absolute
difference 0). Backends: `callr` (one process per future), `multisession`
(persistent workers), and `sequential`. Mapping calls accept three error
policies: `stop`, `warn`, and `collect`. Implementation notes are in
`Code/kernel.R`; measurements for v6.1 are in `handoff/261027_perf-review.md` and
for v6.3 in `handoff/261101_perf-round2.md`.

## Testing

```powershell
& "C:\Program Files\R\R-4.6.2\bin\Rscript.exe" tests/run_tests.R
```

`tests/` is local only. The suite covers input contracts, aggregation, RR
builders, PWRR branches, decomposition caches, and the concurrency kernel.

## Publications using this framework

1. XING Z#, **LIU Y**#, CHEPELIEV M, et al. Global food trade can mitigate
   substantial health burdens attributed to ambient PM2.5 pollution.
   *Nature Food*, 2026, 7(3): 223–233.
2. Tang R, Zhao J, **LIU Y**, et al. Air quality and health co-benefits of
   China's carbon dioxide emissions peaking before 2030.
   *Nature Communications*, 2022, 13(1): 1008.
3. **LIU Y**, Zhu G, Zhao Z, et al. Population aging might have delayed the
   alleviation of China's PM2.5 health burden.
   *Atmospheric Environment*, 2022, 270: 118895.
4. Wang H, He X, Liang X, et al. Health benefits of on-road transportation
   pollution control programs in China. *PNAS*, 2020, 117(41): 25370–25377.

## Data source and license

- GBD 2023 air pollution exposure estimates:
  [GHDx record](https://ghdx.healthdata.org/record/ihme-data/gbd-2023-air-pollution-exposure-estimates-1990-2023),
  [DOI](https://doi.org/10.6069/K4TW-Q814).
- IHME data are provided under the IHME Free-of-Charge Non-Commercial User
  Agreement.
- Code license: see [`LICENSE`](LICENSE).

## Changelog

Release notes v1.0 to v6.4: [`CHANGELOG.md`](CHANGELOG.md). Data flow and join
rules: [`docs/data-flow.md`](docs/data-flow.md).
