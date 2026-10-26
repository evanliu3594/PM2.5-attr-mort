This directory stores the basic input data for the calculation: gridded population, PM<sub>2.5</sub> concentration, grid information (x/y coordinates and geographic domains), baseline mortality rates, and population age structure. C-R lookup tables are stored separately in `Data/RR_index/`.

Sample gridded population, PM<sub>2.5</sub> concentration data and corresponding grid information (x/y coordinates and geographic domain columns) are provided to illustrate the input data structure.

`Data/instance_manifest.json` records the active instance paths and the source-year specification used by `build_instance.R`. Entry scripts read this manifest. Raw population rasters are not committed; set `PM25_POP_RAW_DIR` before rebuilding. A rebuild stops when the requested scenario mixes source years unless `ALLOW_MIXED_SCENARIO_YEARS` is explicitly enabled.

`Data/GridPM25_cf_instance_260617.xlsx` is a legacy synthetic counterfactual file. It contains negative concentrations (1,252 in `base2015`, 1,207 in `SSP1-Baseline_2030`). It is not produced by the current builder and is not used by the default decomposition. Do not use it for formal counterfactual analysis until its provenance and non-negativity are resolved.

`IHME_GBD_2023_AIR_POLLUTION_1990_2021_RR_CURVES.zip` is the raw GBD 2023 air pollution RR archive used by `Code/build_CR_MRBRT2023.R`. It is excluded from git; download it from [GHDx](https://ghdx.healthdata.org/record/ihme-data/gbd-2023-air-pollution-exposure-estimates-1990-2023) with an IHME account. The copy used here was downloaded on 2026-10-26: 185,156,657 bytes, SHA256 `e12c276090f4181ed48e3c4d7474572b2c00b8889f1c3c82257042ffc59f2816`.
