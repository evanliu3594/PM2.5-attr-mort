# General
This project documents my current progress in evaluating the health burden of PM2.5 exposure globally.

For several years, the project was encapsulated as an R project, necessitating the cloning of the entire project folder each time it was used to ensure consistency in data formats and output locations. This approach posed significant challenges in the flexible application of the project's methodology for calculating attributable mortality.

In the latest version I have released, I have refactored the core code upon which the project depends, enabling it to leverage the latest capabilities of the tidyverse packages. It now exhibits a more object-oriented design and no longer strictly follows the initial grid-based computation model, making it suitable for more flexible data resolutions.

Furthermore, installing a package offers greater flexibility than cloning the entire project. It allows for more adaptable function calls, more versatile update processes, and more flexible data import methods.

P.S.: the **PM2.5-attr-mort** refers to the **PM<sub>2.5</sub>-attributable-mortality**

# Supported Researches
1. Liu Y, Zhu G, Zhao Z, et al. Population aging might have delayed the alleviation of China's PM2.5 health burden[J]. Atmospheric Environment, 2022, 270: 118895.

2. Tang R, Zhao J, Liu Y, et al. Air quality and health co-benefits of China's carbon dioxide emissions peaking before 2030[J]. Nature Communications, 2022, 13 (1): 1008.

3. Wang H, He X, Liang X, et al. Health benefits of on-road transportation pollution control programs in China[J]. Proceedings of the National Academy of Sciences, 2020, 117 (41): 25370-25377.

# Installation

I've yet not considering putting this project into CRAN until it gets more well-functioned, so at present, it can only installed through github.
```
devtools::install_github("evanliu3594/PM2.5-attr-mort")    
library(AttrMort)
```

# Data importing
As I mentioned earlier, the functions in the `AttrMort` package accept a more flexible arrangement of data than before. 
There is no longer a strict requirement for the input data to maintain a grid format of x-y. As long as the field data, population data, dose data, age structure data, and mortality data can be matched and combined according to some key columns, the calculations can be completed.
This package includes some sample data. Please refer to the following for more information.
```{r}
library(tidyverse)
library(AttrMort) 

builtin_data <- list.files(system.file('extdata', package = "AttrMort"), full.names = T)

for (f in builtin_data) {
  nm <- basename(f) %>% str_extract(".+(?=\\.)")
  assign(nm, readxl::read_xlsx(f))
}

head(Grid_info)
```
# Calculation statement
claim the Model to use in the next calculation.

```{R}
set_Model("NCD+LRI")
```

# Calculating Attributable Mortality

```{r}
Mortality(
  field = Grid_info %>% filter(location == "China"),
  dose_real = get_at(Grid_PM25, scenario, "base2015"),
  pop = get_at(Grid_Pop, scenario, "base2015"),
  age = get_at(GBD_agestructure, scenario, "base2015"),
  mort = get_at(GBD_mortrate, scenario, "base2015"),
  lvl = "location"
)
```
# Calculating Aggregated Attributable Mortality with uncertainty

```{r}
Mortality_Aggr(
  field = Grid_info %>% filter(location == "China"),
  dose_real = get_at(Grid_PM25, scenario, "base2015"),
  pop = get_at(Grid_Pop, scenario, "base2015"),
  age = get_at(GBD_agestructure, scenario, "base2015"),
  mort = get_at(GBD_mortrate, scenario, "base2015"),
  doseRSME = 24, 
  lvl = "location", 
  uncertain = T
)
```

# Calculate the driving force of Attributable Mortality

```{r}
Decomposition(
  serie = 1, 
  G = Grid_info %>% filter(location == "China"),
  D = Grid_PM25, P = Grid_Pop, A = GBD_agestructure, M = GBD_mortrate, L = "location", 
  key = scenario, from = "base2015", to = "SSP1-Baseline_2030"
)
```


# Changelog
## 1.0-release 
The Initial published version of the PM25-attr-mort with a series of refined and easy-to-use functions to assess PM<sub>2.5</sub> health burden using the IER model.
## 2.0-release
PM<sub>2.5</sub>-Attr-Mort v2.0 incorporated both GEMM and IER model in calculating the PM<sub>2.5</sub> health burden, by refering to different concentration-responce lookup-table.
## 3.0-release
PM<sub>2.5</sub>-Attr-Mort v3.0 flushes the original linear algebra calculation with a more tidy-R grammar, which greatly improves the extensible of the scales of the model. 
## 4.0-release
PM<sub>2.5</sub>-Attr-Mort v4.0 extends to international or other muiti-region scale calculation, and extends to O3 or NO<sub>2</sub> burden calculation, add MRBRT method CRF used by GBD2019.
## 5.0-release
Announce of the launch of AttrMort package
