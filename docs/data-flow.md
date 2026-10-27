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
