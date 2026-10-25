This directory stores the basic input data for the calculation: gridded population, PM<sub>2.5</sub> concentration, grid information (x/y coordinates and geographic domains), baseline mortality rates, and population age structure. C-R lookup tables are stored separately in `Data/RR_index/`.

Sample gridded population, PM<sub>2.5</sub> concentration data and corresponding grid information (x/y coordinates and geographic domain columns) are provided to illustrate the input data structure.

`IHME_GBD_2023_AIR_POLLUTION_1990_2021_RR_CURVES.zip` is the raw GBD 2023 air pollution RR archive used by `Code/build_CR_MRBRT2023.R`. It is excluded from git; download it from [GHDx](https://ghdx.healthdata.org/record/ihme-data/gbd-2023-air-pollution-exposure-estimates-1990-2023) with an IHME account. The copy used here was downloaded on 2026-10-26: 185,156,657 bytes, SHA256 `e12c276090f4181ed48e3c4d7474572b2c00b8889f1c3c82257042ffc59f2816`.
