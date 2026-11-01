# Data flow — how sources are joined inside `Mortality()`

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
 ┌──────────────────────────────────────────────────────────────────────┐
 │ STEP 1 — Two grid-level base tables  (by = x, y)                     │
 │                                                                      │
 │   base_c = Grid_info left_join(Conc_c) left_join(pop)  RR numerator  │
 │   base_r = Grid_info left_join(Conc_r) left_join(pop)  PWRR          │
 │                                                                      │
 │   Conc_c defaults to Conc_r. Each table keeps the grid keys, the     │
 │   domain column, and one concentration column. The wide grid ×       │
 │   endpoint × agegroup join is not materialised (v5.4).               │
 └──────────────┼───────────────────────────────────────────────────────┘
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
     base_r → PWRR = weighted.mean(RR, Pop)  grouped by domain × endpoint × agegroup
     base_c → Mort = Pop × AgeStruc × MortRate × (RR − 1) / PWRR / 1e5
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
 │   x │ y │ copd_25_MEAN │ copd_25_UP │ copd_25_LOW │ ...          │
 │   ──┼───┼──────────────┼────────────┼─────────────┼─────         │
 │                                                                  │
 │   One column per endpoint–agegroup–branch. No Total column:      │
 │   aggregate_range(at, by = "total") sums the value columns.      │
 │   Totals go to ./Results/ when write = TRUE.                     │
 └──────────────────────────────────────────────────────────────────┘
```

**Concentration roles:**

- `Conc_r` — real exposure. `Mortality()` builds `base_r` from it and computes the PWRR denominator per domain × endpoint × agegroup: `weighted.mean(RR, Pop)`.
- `Conc_c` — concentration passed to the RR lookup for the attributable-death numerator. It defaults to `Conc_r`; `Mortality_at()` keeps that default, so absolute-burden runs use the real concentration in both roles. Pass `Conc_c = getConc_cf(at)` for a counterfactual scenario.
- Both base tables keep one row per grid. `Mortality()` loops endpoint × agegroup (v5.4) and binds the value columns; it never joins the grid table to every endpoint and age group at once.

**Key matching rules:**

| Join | Keys | Precision | Where it happens |
|------|------|-----------|------------------|
| Grids ↔ Conc ↔ Pop | `x`, `y` | `matchable(dgt_grid)` — default 2 decimal places | `read_files()` |
| Conc ↔ RR lookup | `concentration` | `matchable(dgt_conc)` — default 1 decimal place, string type | `set_Model()` (build), `Mortality()` (join) |
| RR ↔ Mortality rates | `endpoint`, `agegroup` | endpoint = lowercase string, agegroup = `matchable(dgt=0)` | `Mortality()` |
| All ↔ Domain stats | `domain` (e.g. `Country`) | string, must match Grid_info column names | `Mortality()`, `aggregate_sigma()` |

All join keys are **strings** (not floats) to avoid floating-point mismatch. The `matchable()` helper does `round(dgt) |> as.character()`.

`RR_std()` normalises its own keys at build time (concentration and agegroup `matchable`, endpoint lower-case) and marks the result `rr_normalized`. `Mortality()` reuses those keys and only re-normalises tables assigned by hand.
