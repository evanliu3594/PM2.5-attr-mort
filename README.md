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

   Set the population raster directory before running. The other raw paths can
   also be overridden with environment variables:

   ```powershell
   $env:PM25_POP_RAW_DIR = "D:\path\to\population\tifs"
   $env:PM25_INSTANCE_SUFFIX = "261026"
   ```

   ```r
   source('build_instance.R')
   ```

   `build_instance.R` blocks mixed source years by default. The current `2030`
   label combines 2030 exposure with 2020 population/mortality and 2019 age
   structure. Set `ALLOW_MIXED_SCENARIO_YEARS <- TRUE` only after the proxy is
   approved; the chosen years are written to `Data/instance_manifest.json`.

   This produces five standardised `.xlsx` files under `./Data/`:
   | Output file | Content |
   |---|---|
   | `Grid_info_instance_*.xlsx` | Grid coordinates (`x`, `y`) with `Country` column |
   | `GridPM25_instance_*.xlsx` | Grid-level annual-average PM<sub>2.5</sub> (µg/m³) per scenario |
   | `GridPop_instance_*.xlsx` | Grid-level population counts per scenario |
   | `GBD_mortality_instance_*.xlsx` | Baseline mortality rate (deaths per 100,000) by country, endpoint, and age group |
   | `GBD_agestructure_instance_*.xlsx` | Proportion of population in each age group by country |

   `build_instance.R` writes `Data/instance_manifest.json` with the active
   instance paths and source-year specification. Entry scripts list the five
   input paths in their own configuration block; edit those paths to use
   another instance. `resolve_instance_files()` still reads the manifest for
   scripts that prefer it. Counterfactual concentration is not generated
   automatically. Supply an approved `Conc_cf` file separately.

3. Choose a C-R model and run the calculation — `HealthBurdenCalc.R` lists the
   input paths in its configuration block and writes national aggregates by
   default:

   ```r
   set_Model('NCD+LRI')     # or '5COD', 'IER', 'MRBRT', 'MRBRT2023', 'O3', 'NO2'

   GRIDS_FILE      <- "./Data/Grid_info_instance_260617.xlsx"
   POP_FILE        <- "./Data/GridPop_instance_260617.xlsx"
   CONC_REAL_FILE  <- "./Data/GridPM25_instance_260617.xlsx"
   CONC_CF_FILE    <- NULL   # optional counterfactual file
   MORT_RATE_FILE  <- "./Data/GBD_mortality_instance_260617.xlsx"
   AGE_GROUP_FILE  <- "./Data/GBD_agestructure_instance_260617.xlsx"

   read_files(
     Grids     = GRIDS_FILE,
     Pop       = POP_FILE,
     Conc_real = CONC_REAL_FILE,
     Conc_cf   = CONC_CF_FILE,
     MortRate  = MORT_RATE_FILE,
     AgeGroup  = AGE_GROUP_FILE,
     dgt_grid  = 2,
     conc_unit = "ug/m3"
   )

   scenarios <- get_scenarios()

   # Grid-level mortality with 95% CI (single-pass MEAN + UP + LOW)
   grid_ci <- map(set_names(scenarios), ~ Mortality_at(at = .x, CI = "RANGE"))

   # Aggregate by country and endpoint
   aggregate_range(grid_ci, at = "Country", by = "endpoint", write = TRUE)
   ```

   Set `WRITE_RESULTS <- FALSE` in `HealthBurdenCalc.R` when stepping through
   without writing. O3/NO2 require ppb concentration and the matching endpoint
   in the mortality table; the entry stops when those inputs are missing.

4. **Decomposition analysis** — `DrivingFactors.R` is the entry script; the decomposition functions live in `Code/decomposition.R`. Edit its configuration block (`MODEL`, `START_YEAR`, `END_YEAR`, `RESULT_AT`, `RUN24`, `WORKERS`), then source the file or run it with Rscript. Set `PM25_RUN_DRIVERS=false` to load data and functions without running the 24 sequences.

## Output layout

- `Results/` stores data result workbooks only. It is gitignored.
- `Figures/` stores figures only. It is gitignored.
- `tests/`, `handoff/`, and `Code/build/` are local working materials and are gitignored.

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
| **RR_index** | sheets `MEAN`, `LOW`, `UP`; concentration column (name configurable via `conc_col` in `RR_std_config.json`) + `{endpoint}_{agegroup}` columns (e.g. `copd_25`) | Concentration–response lookup table, auto-selected by `set_Model()`. Regenerate IER/GEMM via `Code/build/build_CR.R`; build the GBD 2023 MR-BRT table via `Code/build/build_CR_MRBRT2023.R`. |

All gridded files must share the same `(x, y)` coordinate system. `read_files()` normalises coordinates to `matchable(dgt_grid)` precision regardless of whether they are stored as numeric or text. Concentration values are matched to the RR lookup table at the precision set by `dgt_conc`. Duplicate `(x, y)` or domain keys stop the load.

Both entry scripts list the five input paths in their configuration block. Edit those paths to switch instance. `Data/instance_manifest.json` records the active instance and source-year specification; `resolve_instance_files()` reads it for scripts that prefer the manifest. For a manual instance, pass a six-digit suffix explicitly: `resolve_instance_files(suffix = "260617")`.

`RR_std_config.json` also records `conc_unit` and `conc_range` for every model. Pass the matching unit to `read_files(conc_unit = "ug/m3")`; gas models use `"ppb"`. Legacy gas entries remain `"unknown"` until their source unit is verified.

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
     PWRR = weighted.mean(RR, Pop)  ←── grouped by domain × endpoint × agegroup
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
 │   Then aggregated by aggregate_range(at, by) → ./Results/        │
 └──────────────────────────────────────────────────────────────────┘
```

**Key matching rules:**

| Join | Keys | Precision | Where it happens |
|------|------|-----------|------------------|
| Grids ↔ Conc ↔ Pop | `x`, `y` | `matchable(dgt_grid)` — default 2 decimal places | `read_files()` |
| Conc ↔ RR lookup | `concentration` | `matchable(dgt_conc)` — default 1 decimal place, string type | `set_Model()` (build), `Mortality()` (join) |
| RR ↔ Mortality rates | `endpoint`, `agegroup` | endpoint = lowercase string, agegroup = `matchable(dgt=0)` | `Mortality()` |
| All ↔ Domain stats | `domain` (e.g. `Country`) | string, must match Grid_info column names | `Mortality()`, `aggregate_sigma()` |

All join keys are **strings** (not floats) to avoid floating-point mismatch. The `matchable()` helper does `round(dgt) |> as.character()`.

### C-R model configuration

```r
# Built-in models — lookup path and endpoint/agegroup config read from RR_std_config.json
set_Model('IER')         # IER2017
set_Model('NCD+LRI')     # GEMM NCD+LRI (composite endpoint)
set_Model('5COD')        # GEMM 5-COD (individual endpoints)
set_Model('MRBRT')       # MR-BRT GBD 2023 (default alias)
set_Model('MRBRT2023')   # MR-BRT GBD 2023 (explicit)
set_Model('O3')          # GBD 2023 Ozone (COPD only)
set_Model('NO2')         # GBD 2023 NO2 (asthma)
set_Model('O3_legacy')   # legacy O3 lookup (COPD)
set_Model('NO2_allcause') # legacy NO2 all-cause lookup

# Custom RR lookup table — auto-generates endpoint/agegroup config and appends to JSON
set_Model("MyModel", path = "./Data/RR_index/My_Lookup.xlsx")
```

All model configuration is driven by `Data/RR_std_config.json`. The file records the concentration unit and valid range. `set_Model("NCD+LRI", dgt_conc = 2)` builds the standardised table at two decimal places; `read_files()` then requires `dgt_conc = 2`.

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

### Concurrency

`Code/kernel.R` is the project concurrency kernel. It builds on the R Future API
and keeps the historical sequential behaviour by default. Load it before the
calculation and set a plan:

```r
source("Code/kernel.R")

kernel_plan(workers = 4)   # callr; falls back to multisession, then sequential
grid_ci <- map(set_names(scenarios), ~ Mortality_at(at = .x, CI = "RANGE"))
kernel_off()               # restore the sequential plan
```

What runs in parallel:

- `Decomposition()` builds any missing factor state in parallel, then assembles
  the grid-level output from the cached states. One series needs five states.
- `aggregate_drivers()` precomputes the 16 unique states' country summaries in
  parallel, then assembles the 24 sequences from those summaries.

Backends and fallbacks:

| Backend | Engine | Use |
|---------|--------|-----|
| `"callr"` (default) | `future.callr`, one process per future | Production; needs process pipes. Verified in a normal R session (elevated run): output identical to serial, but process startup per future makes one call slower than serial |
| `"multisession"` | persistent R workers | Works where pipes are blocked, such as sandboxed runners |
| `"sequential"` | main process | No parallel execution |

`kernel_plan()` runs one smoke test. When the requested backend cannot start, it
falls back to the next backend with one warning. Set `fallback = FALSE` to stop
instead.

Error strategies for one mapping call:

| `.error` | Behaviour |
|----------|-----------|
| `"stop"` (default) | Run every task, then raise one `kernel_task_error` listing up to three failed keys |
| `"warn"` | Run every task, signal one warning, leave failed slots `NULL` |
| `"collect"` | Return a `kernel_result` with `values`, `errors`, `n_ok`, and timings |

`kernel_info()` reports the backend, worker count, whether the call ran inside a
worker, and the last run's elapsed time, mapping-input payload size, and error
count.

Entry scripts read `PM25_KERNEL_WORKERS` (default `1`, sequential). The plan
stays active for the R session, so call `kernel_off()` to restore the sequential
plan.

Two kernel rules matter for performance:

- A mapping call with one task never starts a future; it runs in the main process.
- `Mortality()` stays sequential. Its endpoint × agegroup loop is only about
  1.8 s of a roughly 6 s call, so parallel group execution was slower than
  serial and the integration was reverted.
- Parallel gains appear at the decomposition level: one `Decomposition()` call
  fell from 23.22 s to 15.39 s with four workers (1.51×), and the 24-sequence
  aggregate fell from about 66 s to about 29 s (2.22×).

`kernel_info()$payload_mb_last` reports only the mapping inputs (`.x`, `.y`,
`.l`). It does not include the exported closure environment.

# Release notes

## v6.0 (current)
- **Explicit input paths**: both entry scripts carry an input-data block with five file paths (`GRIDS_FILE`, `POP_FILE`, `CONC_REAL_FILE`, `MORT_RATE_FILE`, `AGE_GROUP_FILE`; `CONC_CF_FILE` optional). Edit those paths to switch instance. `resolve_instance_files()` remains available for scripts that prefer `Data/instance_manifest.json`.
- **Concurrency kernel**: `Code/kernel.R` adds one Future-based kernel for the project: plan management (`kernel_plan()`, `kernel_off()`, `kernel_probe()`, `kernel_info()`, `kernel_active()`, `kernel_workers()`) and mapping functions (`kernel_map()`, `kernel_map2()`, `kernel_imap()`, `kernel_pmap()`, `kernel_walk()`). Every task runs through `tryCatch()`; the parallel path dispatches `furrr` functions with `do.call()`. The default backend is `future.callr`, followed by `multisession` and sequential fallbacks.
- **Default stays sequential**: entry scripts read `PM25_KERNEL_WORKERS` (default `1`). Serial results are unchanged.
- **Decomposition() states build in parallel**: when a plan is active, missing factor states are computed in parallel and cached; the grid-level output is identical to the serial path. One series (five states) fell from 23.22 s to 15.39 s with four workers (1.51×).
- **DrivingFactors state precompute**: `aggregate_drivers()` summarises the 16 unique states in parallel and assembles the 24 sequences from the summaries. The 24-sequence run fell from about 66 s to about 29 s with four workers (2.22×), and the workbook matches the frozen 261026 run (maximum absolute difference 0).
- **DrivingFactors entry split**: function definitions moved to `Code/decomposition.R`; the root `DrivingFactors.R` is now a 74-line entry script with a configuration block (`MODEL`, `START_YEAR`, `END_YEAR`, `RESULT_AT`, `RUN24`, `WORKERS`). Existing `source('DrivingFactors.R')` and `PM25_RUN_DRIVERS=false` usage still work. End-to-end: 72.49 s sequential vs 33.90 s with four workers (2.14×); sequential, parallel, and the frozen 261026 workbook agree with maximum absolute difference 0.
- **Mortality() stays serial**: group-level dispatch was built and measured, then reverted. The endpoint × agegroup loop is only about 1.8 s of a roughly 6 s call; input key validation and RR/base-table preparation dominate. See `handoff/261027_inner-parallel-diagnosis.md`.
- **Tests**: `tests/run_tests.R` runs eight files with 367 assertions, including kernel contract, decomposition cache, and parallel decomposition equivalence tests. The original 183 assertions are unchanged.
- **Boundaries**: the development sandbox blocks process pipes, so all timing measurements used `multisession`. A separate elevated run verified the `future.callr` backend in a normal R session: both backends return identical output (maximum absolute difference 0), and callr is slower per call. `aggregate_sigma()` is not parallelised.

## v5.9
- **DrivingFactors state cache**: the 24 decomposition sequences share 16 unique factor states. States are cached by sorted factor set and reused across sequences.
- **Country-level summaries**: `aggregate_drivers()` now uses `Decomposition_summary()` and country × Cause_Age state summaries. It no longer rebuilds the 805,695-row grid table for every sequence.
- **Trusted-input fast path**: `Mortality(..., validate_keys = FALSE)` skips repeated input key checks after `read_files()` validation. The default remains `TRUE`.
- **Measured performance**: full NCD+LRI 24-sequence run fell from about 725 seconds to 65.54 seconds, an 11.06× speedup. The workbook remains 26 sheets; maximum absolute difference from the pre-optimization workbook is 1.16×10^-10, with no new NA.
- **Tests**: local tests cover the 16-state count, `validate_keys` equivalence, cached grid-level decomposition, country summaries, and workbook structure.

## v5.8
- **Repository layout**: data results go to `Results/`; figures go to `Figures/`. `tests/`, `handoff/`, and `Code/build/` stay local and are excluded from Git via `.gitignore`.
- **Builder location**: CR builders moved to `Code/build/`. Documentation now uses the new paths.

## v5.7
- **Aggregation correctness**: `aggregate_range()` sums only mortality columns. Numeric Grid_info metadata no longer enters totals, and missing mortality stays `NA` instead of becoming zero.
- **Input contracts**: duplicated grid/domain keys abort. Irrelevant metadata `NA` no longer drops grids. Explicit missing `Conc_cf` paths abort.
- **PWRR consistency**: single `CI = "UP"/"LOW"` calls now use the same PWRR denominator as the corresponding `RANGE` branch, including `pwr_branch = "mean"`.
- **Precision and units**: `dgt_conc` propagates through RR construction and mortality joins. `RR_std_config.json` records `conc_unit` and `conc_range`; `read_files(conc_unit = ...)` checks them.
- **Uncertainty**: `Uncertainty()` is retired. `aggregate_sigma()` now computes MEAN/UP/LOW from grid-level PWRR mortality. It supports `by = "total"`; use `aggregate_range()` for breakdowns.
- **Builders**: `build_CR.R` parses under R 4.6.1. MRBRT2023 uses an explicit endpoint scale map. Builders assert source range, finite values, RR ≥ 1, and CI order.
- **Instance provenance**: `build_instance.R` writes `Data/instance_manifest.json`, caches country mappings, blocks mixed-year scenarios by default, and no longer generates random `Conc_cf` files. At v5.7 entry scripts resolved paths from the manifest; v6.0 lists them explicitly.
- **Tests**: `Rscript tests/run_tests.R` covers aggregation, missing values, duplicate keys, PWRR branch consistency, precision, units, manifest, and builders.

## v5.6
- **GBD 2023 O3/NO2 defaults**: `O3` now uses the GBD 2023 ozone–COPD curve; `NO2` now uses the GBD 2023 NO2–asthma curve. Both lookups are built by `Code/build/build_CR_GBD2023_gas.R`.
- **Legacy retained**: `O3_legacy` and `NO2_allcause` keep the previous lookups and endpoints.
- **Units/ranges**: GBD 2023 O3 and NO2 source concentrations are in ppb. O3 covers 0–50 ppb; NO2 covers 0–100 ppb. Input concentration data must use ppb.
- **NO2 endpoint change**: GBD 2023 NO2 is an asthma curve. The current sample mortality instance has no asthma, so `NO2` can build its RR table but cannot run `Mortality()` on the sample; use `NO2_allcause` for the legacy all-cause endpoint.
- **CI handling**: the GBD 2023 NO2 lower bound falls below RR = 1; the lookup floors it at 1 so the project's RR ≥ 1 and PWRR guard remain valid. The NO2 LOW branch is therefore flat at 1.
- **CI denominator**: O3/NO2 entries use `"pwr_branch": "mean"`; all three branches share the MEAN PWRR. This keeps the very wide NO2 UP curve from producing a non-monotonic CI. PM2.5 models keep branch-specific PWRR.

## v5.5
- **MRBRT default**: `set_Model("MRBRT")` now loads the GBD 2023 lookup (`MRBRT2023`). `MRBRT2021` remains available for legacy runs; `MRBRT2023` remains an explicit alias for the same lookup.
- Output labels for `MRBRT` now use `PM2.5_MRBRT2023`; files produced with the old alias are labeled `PM2.5_MRBRT2021`.

## v5.4
- **Mortality() grouped by endpoint × agegroup**: `Mortality()` now iterates one `{endpoint}-{agegroup}` group at a time. Each group joins at grid level, computes PWRR, and returns one or three value columns. The grid × endpoint × agegroup long table is no longer materialized.
- **Future-ready**: `compute_group()` is side-effect free; the `map2()` call can be replaced by `future_map2()` to parallelize one group per worker. v6.0 measured this path and kept the loop sequential.
- **Validation**: NCD+LRI MEAN/RANGE outputs are bit-identical to the pre-refactor baseline (max abs diff 0). MRBRT2021 RANGE: 53,713 × 287, NA = 0; totals 6,715,235 / 8,204,761 / 4,892,366; 31.8 s.
- **Debug**: per-group `join_report()` diagnostics remain; debug small-subset test 20 × 17, NA = 0.

## v5.3
- **DrivingFactors 24-sequence rewrite**: `DrivingFactors.R` no longer generates permutations with `expand_grid()` + `slice()`. The 24 sequences are an explicit table in the documented order; factor state is one explicit mapping (`PG` → Pop, `PA` → AgeStruc, `EXP` → Conc_c, `ORF` → Conc_r + MortRate).
- **Audit result**: 24 sequences × 5 states were checked with a recording `Mortality()` stub; 0 state mismatches. The old `serie` order did not match the 24-line comment order, so per-serie sheets looked mislabeled; the 24-sequence mean was unaffected when the old code could run.
- **Runtime fixes**: `Mortality()` returns no `Country` column, so the old `Decomposition()` would fail at `pivot_longer(cols = -c(x, y, Country))`. The new code injects the detected domain column. `aggregate_drivers()` now accepts `domain` and writes a `Sequences` sheet for audit.
- **Validation**: real 200-grid China subset, `Decomposition(serie = 1)` → 3,000 × 10, NA = 0. The full 24-sequence run was not executed (120 `Mortality()` calls).

## v5.2
- **PWRR fix**: `PWRR` is now computed per domain × endpoint × agegroup, not as one scalar per domain. The old code averaged RR across diseases and ages, then applied that scalar to every endpoint; this biased each endpoint in opposite directions.
- **CI branches**: `Mortality(CI = "RANGE")` now uses the matching PWRR branch (`MEAN`/`UP`/`LOW`) for each CI. The old code used the MEAN PWRR denominator for all three branches.
- **Impact (sample global, MRBRT2021)**: total MEAN −5.23%, UP −17.9%, LOW +6.34%; COPD −20.9%, DM2 +6.88%, IHD −4.95%, LC +9.56%, LRI +1.98%, Stroke +1.29%. Old results are invalid; rerun downstream.
- **Guard**: NA PWRR now drops only the affected domain × endpoint × agegroup rows; it no longer drops the whole domain.

## v5.1
- **GBD 2023 MR-BRT CRF**: added the `MRBRT2023` lookup, built from the GHDx GBD 2023 air pollution RR archive. IHD, stroke, and LRI curves are updated; the Y2022 COPD, diabetes, and lung cancer curves are carried forward and differ from GBD 2021 by ≤1.1% at 300 µg/m³.
- **Reproducible builder**: `Code/build/build_CR_MRBRT2023.R` extracts the six PM<sub>2.5</sub> mean curves, detects RR vs log(RR), and interpolates to the 0.1 µg/m³ grid.
- **Backward compatible (at v5.1)**: `MRBRT` pointed to the GBD 2021 table; v5.5 changes the default to 2023. `MRBRT2021` remains available for legacy runs.

## v5.0
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
- **`aggregate_sigma()`**: total-mortality CI aggregation (PWRR-consistent since v5.7)
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
