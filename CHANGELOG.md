# Changelog

Version history for PM2.5-attr-mort. Newest first.

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
