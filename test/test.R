

GEMM_Lookup_Table <- GEMM_Lookup_Table %>% map(~.x %>% rename(dose = concentration))

usethis::use_data(GEMM_Lookup_Table, overwrite = TRUE)

IER2017_Lookup_Table %>% map(~.x %>% rename(dose = concentration)) -> IER2017_Lookup_Table

usethis::use_data(IER2017_Lookup_Table, overwrite = TRUE)

IER2015_Lookup_Table %>% map(~.x %>% rename(dose = concentration)) -> IER2015_Lookup_Table

usethis::use_data(IER2015_Lookup_Table, overwrite = TRUE)

IER2013_Lookup_Table %>% map(~.x %>% rename(dose = concentration)) -> IER2013_Lookup_Table

usethis::use_data(IER2013_Lookup_Table, overwrite = TRUE)

IER2010_Lookup_Table %>% map(~.x %>% rename(dose = concentration)) -> IER2010_Lookup_Table

usethis::use_data(IER2010_Lookup_Table, overwrite = TRUE)

MRBRT_Lookup_Table %>% map(~.x %>% rename(dose = concentration)) -> MRBRT_Lookup_Table

usethis::use_data(MRBRT_Lookup_Table, overwrite = TRUE)

NO2_CR_Lookup_Table %>% map(~.x %>% rename(dose = concentration)) -> NO2_CR_Lookup_Table

usethis::use_data(NO2_CR_Lookup_Table, overwrite = TRUE)

O3_CR_Lookup_Table %>% map(~.x %>% rename(dose = concentration)) -> O3_CR_Lookup_Table

usethis::use_data(O3_CR_Lookup_Table, overwrite = TRUE)

for (f in list.files(system.file('extdata', package = "AttrMort"), full.names = T)) {
  nm <- basename(f) %>% str_extract(".+(?=\\.)")
  assign(nm, read_rds(f))
}
{
  field <- Grid_info 
  pop <- get_at(Grid_Pop, scenario, "SSP1-Baseline_2030")
  mort <- 
  age <- get_at(GBD_agestructure, scenario, "SSP1-Baseline_2030") 
  dose_real <- get_at(Grid_PM25, scenario, "SSP1-Baseline_2030")
}

bind <- function(lvl) {
  PWE <- list(field, dose_real, pop) %>% reduce(left_join) %>% group_by(pick(!!lvl)) %>% 
    summarise(dose = weighted.mean(as.numeric(dose), pop, na.rm = T)) %>% 
    ungroup() %>% na.omit()
}

bind("location")

Mortality_Aggr(
  field = field,
  dose_real = dose_real,
  pop = pop,
  age = age,
  mort = mort,
  doseRSME = 6,
  lvl = "location",
  uncertain = T,
  write = F
)
