# RR Lookup Tables

This directory stores **pre-computed concentration–response (C-R) lookup tables**. Each table maps ambient PM<sub>2.5</sub> concentration (µg/m³) to relative risk (RR) for every disease endpoint × age group combination, at three confidence levels (MEAN, LOW, UP).

These files are the bridge between raw epidemiological coefficients and the mortality calculation: `Mortality()` joins grid-level concentrations against these tables to obtain the RR values needed for the attributable-death formula.

## File inventory

| File | Model | Source coefficients |
|------|-------|---------------------|
| `IER2010_Lookup_Table_Build_220601.xlsx` | IER 2010 | `Data/CRF_coefficients/IER2010_parameters.csv` |
| `IER2013_Lookup_Table_Build_220601.xlsx` | IER 2013 | `Data/CRF_coefficients/IER2013_parameters.csv` |
| `IER2015_Lookup_Table_Build_220601.xlsx` | IER 2015 | `Data/CRF_coefficients/IER2015_parameters.csv` |
| `IER2017_Lookup_Table_Build_220601.xlsx` | IER 2017 | `Data/CRF_coefficients/IER2017_parameters.csv` |
| `GEMM_Lookup_Table_Build_220914.xlsx` | GEMM (NCD+LRI & 5COD) | `Data/CRF_coefficients/GEMM-parameters.csv` |
| `MRBRT2019_Lookup_Table_LYF220601.xlsx` | MRBRT | External |
| `O3_CR_Lookup_Table.xlsx` | O<sub>3</sub> | External |
| `NO2_CR_Lookup_Table.xlsx` | NO<sub>2</sub> | External |

Files with different `Build_YYMMDD` suffixes are historical versions. The active version for each model is specified in `Data/RR_std_config.json` under the `"lookup"` key.

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

- **First column**: concentration values — PM<sub>2.5</sub> in µg/m³, from 0.0 to 300.0 in steps of 0.1 (~3000 rows). The column name is defined by the `"conc_col"` field in `Data/RR_std_config.json` (default `"concentration"`).
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

Run `Code/DataPrepare/Concentration_Response.R`:

```r
source('./Code/DataPrepare/Concentration_Response.R')
compute_RR_index("IER2017", write = TRUE)
```

This reads the coefficient CSV, evaluates the RR formula at every concentration step, and writes a new `.xlsx` to this directory. Update `RR_std_config.json` if the filename changes.

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
    ↓ (DataPrepare/Concentration_Response.R)
Data/RR_index/*.xlsx                 ←  You are here
    ↓ (set_Model() → RR_std() → .RR_std_tbl)
Code/*.R                             →  Mortality calculation
    ↓
Result/                              →  Aggregated output
```

The mapping from model name to lookup-table path and endpoint definitions is maintained in `Data/RR_std_config.json`, which serves as the central registry.
