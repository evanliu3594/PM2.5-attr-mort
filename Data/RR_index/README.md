# RR Lookup Tables

This directory stores **pre-computed concentration–response (C-R) lookup tables**. Each table maps ambient PM<sub>2.5</sub> concentration (µg/m³) to relative risk (RR) for every disease endpoint × age group combination, at three confidence levels (MEAN, LOW, UP).

These files are the bridge between raw epidemiological coefficients and the mortality calculation: `Mortality()` joins grid-level concentrations against these tables to obtain the RR values needed for the attributable-death formula.

## File inventory

| File | Model | Source |
|------|-------|---------------------|
| `IER2010_Lookup_Table_Build_220601.xlsx` | IER 2010 | `Data/CRF_coefficients/IER2010_parameters.csv` |
| `IER2013_Lookup_Table_Build_220601.xlsx` | IER 2013 | `Data/CRF_coefficients/IER2013_parameters.csv` |
| `IER2015_Lookup_Table_Build_220601.xlsx` | IER 2015 | `Data/CRF_coefficients/IER2015_parameters.csv` |
| `IER2017_Lookup_Table_Build_220601.xlsx` | IER 2017 | `Data/CRF_coefficients/IER2017_parameters.csv` |
| `GEMM_Lookup_Table_Build_220914.xlsx` | GEMM (NCD+LRI & 5COD) | `Data/CRF_coefficients/GEMM-parameters.csv` |
| `MRBRT2019_Lookup_Table_LYF220601.xlsx` | MR-BRT GBD 2019 | External |
| `MRBRT2021_Lookup_Table_LYF250716.xlsx` | MR-BRT GBD 2021 | External |
| `MRBRT2023_Lookup_Table_Build_261026.xlsx` | MR-BRT GBD 2023 | GHDx archive, built by `Code/build_CR_MRBRT2023.R` |
| `O3_Lookup_Table_GBD2023_Build_261026.xlsx` | O<sub>3</sub> GBD 2023 (COPD) | GHDx archive, built by `Code/build_CR_GBD2023_gas.R` |
| `NO2_Lookup_Table_GBD2023_Build_261026.xlsx` | NO<sub>2</sub> GBD 2023 (asthma) | GHDx archive, built by `Code/build_CR_GBD2023_gas.R` |
| `O3_CR_Lookup_Table.xlsx` | O<sub>3</sub> legacy | External |
| `NO2_CR_Lookup_Table.xlsx` | NO<sub>2</sub> legacy all-cause | External |

Files with different `Build_YYMMDD` suffixes are historical versions. The active version for each model is specified in `Data/RR_std_config.json` under the `"lookup"` key.

**Units:** PM<sub>2.5</sub> tables use µg/m³. GBD 2023 O<sub>3</sub> and NO<sub>2</sub> tables use ppb, matching the GBD source curves.

## Internal structure

Each `.xlsx` file contains **three sheets** named `MEAN`, `LOW`, and `UP`, corresponding to the central estimate and 95% confidence bounds of the relative risk.

### Sheet layout (wide format)

```
concentration | copd_25 | copd_30 | ... | ihd_25 | ... | lri_0 | lri_5 | ...
0.0           | 1.0000  | 1.0000  | ... | 1.0000 | ... | 1.000 | 1.000 | ...
0.1           | 1.0005  | 1.0004  | ... | 1.0003 | ... |   ... |   ... | ...
...
300.0         | 3.2140  | 3.1890  | ... | 5.6701 | ... |   ... |   ... | ...
```

- **First column**: concentration values — PM<sub>2.5</sub> in µg/m³, from 0.0 to 300.0 in steps of 0.1 (~3000 rows); GBD 2023 O<sub>3</sub> and NO<sub>2</sub> in ppb. The column name is defined by the `"conc_col"` field in `Data/RR_std_config.json` (default `"concentration"`).
- **Remaining columns**: `{endpoint}_{agegroup}` — one column per disease × age band. For example, `copd_25` means COPD at age 25–29, `lri_0` means lower respiratory infection at age 0–4.

### Column naming convention

Column names follow the pattern `{endpoint}_{agegroup}` where:

| Part | Format | Examples |
|------|--------|----------|
| `endpoint` | Lowercase disease abbreviation | `copd`, `ihd`, `lc`, `lri`, `stroke`, `dm`, `ncd+lri`, `allcause` |
| `agegroup` | Integer lower bound of the 5-year age band | `0`, `5`, `10`, …, `25`, `30`, …, `90`, `95` |

A column named `copd_25` contains RR values for COPD mortality in the 25–29 age group at each concentration level. The age group `25` covers ages ≥25 to <30; `95` covers ages ≥95.

## How these files are consumed

1. `set_Model()` (in `Code/model.R`) calls `RR_std()` with the lookup path recorded in `Data/RR_std_config.json`. `RR_std()` reads all three sheets, pivots the wide table into long format (`concentration × endpoint × agegroup × CI × RR`), fills age-group-specific values from the `ALL` row where needed, and applies endpoint/age-group filtering defined in the config.

2. The result is stored in the global variable `.RR_std_tbl`. Columns: `concentration`, `endpoint`, `agegroup`, `CI` (`"MEAN"`, `"UP"`, or `"LOW"`), `RR`.

3. `Mortality()` and `Uncertainty()` (in `Code/mortality.R` and `Code/uncertainty.R`) read `.RR_std_tbl` directly — they filter or pivot the `CI` column as needed.

## How to generate or replace a lookup table

### From CRF coefficients (built-in models)

Run `Code/build_CR.R`:

```r
source('./Code/build_CR.R')
compute_RR_index("IER2017", write = TRUE)
```

This reads the coefficient CSV, evaluates the RR formula at every concentration step, and writes a new `.xlsx` to this directory. Update `RR_std_config.json` if the filename changes.

### From the GBD 2023 air pollution RR archive

`Code/build_CR_MRBRT2023.R` compiles the six PM<sub>2.5</sub> mortality endpoints from the GHDx GBD 2023 archive:

```r
source('./Code/build_CR_MRBRT2023.R')
```

The script reads `Data/IHME_GBD_2023_AIR_POLLUTION_1990_2021_RR_CURVES.zip`, extracts the six `..._MEAN_...` curves, detects whether each curve is on the RR or log(RR) scale, and interpolates log(RR) linearly to 0–300 µg/m³ at 0.1 steps. It writes `MRBRT2023_Lookup_Table_Build_261026.xlsx` and reminds you to add the config entry. Download the archive from [GHDx](https://ghdx.healthdata.org/record/ihme-data/gbd-2023-air-pollution-exposure-estimates-1990-2023) with an IHME account. The 185 MB archive is excluded from git.

`MRBRT2023` uses the GBD 2023 curves for IHD, stroke, and LRI; the archive's Y2022 COPD, diabetes, and lung cancer files are close to the GBD 2021 curves (≤1.1% difference at 300 µg/m³). The archive also contains dementia, birth weight, and gestational age curves; these are not compiled into the six-endpoint mortality table. `MRBRT` remains an alias for the GBD 2021 table.

### From the GBD 2023 O3/NO2 archive

`Code/build_CR_GBD2023_gas.R` compiles the two gas curves from the same GHDx zip:

```r
source('./Code/build_CR_GBD2023_gas.R')
```

- O<sub>3</sub>: ozone–COPD, 0–50 ppb.
- NO<sub>2</sub>: NO<sub>2</sub>–asthma, 0–100 ppb; the lower confidence bound is floored at RR = 1.

The script interpolates log(RR) to 0.1 ppb steps and writes `O3_Lookup_Table_GBD2023_Build_261026.xlsx` and `NO2_Lookup_Table_GBD2023_Build_261026.xlsx`. Input concentration data must use ppb. The O3/NO2 config entries use `"pwr_branch": "mean"` so all CI branches share the MEAN PWRR; the very wide NO2 upper curve would otherwise produce a non-monotonic CI.

### From a custom RR curve

If you have your own RR function and want to plug it in, you can either:

**Option A — use `set_Model()` with auto-config** (simplest):

```r
# Prepare an xlsx with MEAN / LOW / UP sheets in the wide format above,
# then let set_Model() auto-detect endpoints and age groups:
set_Model("MyModel", path = "./Data/RR_index/My_Lookup.xlsx")
```

**Option B — manually produce the xlsx**:

Create an `.xlsx` file with exactly three sheets (`MEAN`, `LOW`, `UP`), each containing:
- A `concentration` column (0.0–300.0 in steps of 0.1)
- One column per `endpoint_agegroup` combination (e.g., `copd_25`)

Then add an entry to `Data/RR_std_config.json`:

```json
"MyModel": {
  "conc_col": "concentration",
  "label": "PM2.5_MyModel",
  "lookup": "./Data/RR_index/My_Lookup.xlsx",
  "endpoints": [
    { "name": "copd", "ages": [25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95] }
  ]
}
```

If your lookup table uses a different column name for concentration (e.g., `"pm25"`, `"exposure"`), set `"conc_col"` accordingly. The pipeline will use this name when reading and reshaping the table.

### Key constraints

- Concentration values must be **rounded to one decimal place** (matching `dgt_conc = 1` in `read_files()`). The `matchable()` helper is used to ensure string-identical join keys.
- All three sheets must cover the **same concentration range** and **same endpoint × agegroup columns**.
- The `ALL` age group row (if present in the coefficient parameters) is used as a fallback when age-specific values are missing; `RR_std()` fills these via `tidyr::fill()`.

## Relationship to other files

```
Data/CRF_coefficients/*.csv          →  Raw epidemiological parameters
Data/IHME_..._RR_CURVES.zip          →  Raw GBD 2023 RR curves
    ↓ (Code/build_CR.R / Code/build_CR_MRBRT2023.R / Code/build_CR_GBD2023_gas.R)
Data/RR_index/*.xlsx                 ←  You are here
    ↓ (set_Model() → RR_std() → .RR_std_tbl)
Code/*.R                             →  Mortality calculation
    ↓
Result/                              →  Aggregated output
```

The mapping from model name to lookup-table path and endpoint definitions is maintained in `Data/RR_std_config.json`, which serves as the central registry.
