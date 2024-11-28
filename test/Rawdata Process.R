library(tidyverse)
library(stars);sf_use_s2(F)

# dose_data ----

pm25 <- list.files("./Data/PM25_raw/", pattern = 'nc', full.names = T) %>%
  map_dfr(~ {
    read_ncdf(.x) %>% st_apply(c('lon', 'lat'), mean) %>% as.data.frame %>% na.omit %>%
      mutate(across(lon:lat, \(x) round(x, 2) %>% as.character),
             scenario = str_remove_all(basename(.x), "Annual_pm25_|\\.nc"))
  }) %>% pivot_wider(names_from = 'scenario', values_from = 'mean')

pm25 %>% rename(x = lon, y = lat) %>% 
  write_xlsx(str_glue('./Data/GridPM25_instance_{format(Sys.Date(), "%y%m%d")}.xlsx'))

unzipfiles <- str_subset(
  list.files(
    file.path(Sys.getenv("NJUBOX"), "./0.Data/5.PopData/LandScan_PopDistribution"), 
    pattern = ".zip", full.names = T
  ),
  "2015|2020"
) %>% map(~ unzip(.x, overwrite = F, exdir = "D:/temp/", list = T) %>% pull(Name)) %>% 
  reduce(c)

# POP ----

pop <- list.files('D:/temp/', recursive = T, pattern = '\\d{4}\\.tif', full.names = T) %>% map_dfr(~{
  read_stars(.x) %>% 
    st_warp(st_as_stars(st_bbox(), values = NA, dx = .5, dy = .5), use_gdal = T, method = 'sum') %>% 
    `names<-`('pop') %>% as.data.frame() %>% na.omit %>% mutate(
      across(x:y, \(x) round(x, 2) %>% as.character),
      scenario = str_extract(.x, "\\d{4}"))
}) %>% pivot_wider(names_from = 'scenario', values_from = 'pop')

pop %>% set_names(names(pm25)) %>% rename(x = lon, y = lat) %>% 
  write_xlsx(str_glue('./Data/GridPop_instance_{format(Sys.Date(), "%y%m%d")}.xlsx'))

file.path("D:/temp/", unzipfiles) %>% file.remove()

# Mortrate ----

mortrate <- file.path(
  Sys.getenv("NJUBOX"),
  '0.Data/4.Health Data/IHME/GBD2019/IHME-GBD_2019_DATA-96c44824-1.csv'
) %>% read_csv()

cause.abbr <- c(
  "Tracheal, bronchus, and lung cancer" = 'LC',
  "Lower respiratory infections" = 'LRI',
  "Stroke" = 'STROKE',
  "Ischemic heart disease" = 'IHD',
  "Non-communicable diseases" = 'NCD',
  "Chronic obstructive pulmonary disease" = 'COPD'
)

mortrate %>%
  mutate(
    cause = str_replace_all(cause, !!cause.abbr),
    age = str_extract(age, "\\d+(?=[-+])") %>% replace_na("0"),
    year = as.character(round(year, 0))
  ) %>% na.omit() %>%
  select(
    year,
    location,
    age = age,
    endpoint = cause,
    val
  ) %>% 
  pivot_wider(names_from = 'endpoint', values_from = 'val') %>% 
  mutate(`NCD+LRI` = NCD + LRI) %>% 
  pivot_longer(cols = -c(year:age), names_to = 'endpoint') %>% 
  pivot_wider(names_from = 'year', values_from = 'value') %>% 
  mutate(`SSP1-Baseline_2030` = map_dbl(`2017`, ~ .x + runif(1)/123 )) %>% 
  rename(base2015 = `2017`) %>% 
  pivot_longer(-location:-endpoint, names_to = "scenario", values_to = "mortrate") %>% 
  write_rds(str_glue('./inst/extdata/GBD_mortrate.rds'))

# Age structure ----
age.abbr <- c(
  "Under 5" = '0',
  "5 to 9" = '5',
  "10 to 14" = '10',
  "15 to 19" = '15',
  "20 to 24" = '20',
  "25 to 29" = '25',
  "30 to 34" = '30',
  "35 to 39" = '35',
  "40 to 44" = '40',
  "45 to 49" = '45',
  "50 to 54" = '50',
  "55 to 59" = '55',
  "60 to 64" = '60',
  "65 to 69" = '65',
  "70 to 74" = '70',
  "75 to 79" = '75',
  "80 to 84" = '80',
  "85 to 89" = '85',
  "90 to 94" = '90',
  "95 plus"  = '95'
)

agegroup <- list.files(
  file.path(Sys.getenv("NJUBOX"), '0.Data/4.Health Data/IHME/GBD2019/'), 
  pattern = 'POP', full.names = T
) %>% map(~{
  read_csv(.x) %>% filter(
    sex_name == 'both',
    location_name %in% unique(mortrate$location),
    age_group_name %in% names(age.abbr),
    location_id != 533 
  ) %>% mutate(
    age  = str_replace_all(age_group_name, !!age.abbr)
  ) %>% select(year = year_id, location = location_name, age, pop = val)
}) %>% list_rbind()
               
agegroup %>% 
  group_by(year, location) %>% mutate(prop = prop.table(pop), .keep = 'unused') %>% 
  pivot_wider(names_from = 'year', values_from = 'prop', values_fn = mean) %>% 
  rename(`2017` = `2015`, `SSP1-Baseline_2030` = `2019`) %>% ungroup() %>% 
  pivot_longer(-location:-age, names_to = "scenario", values_to = "prop") %>% 
  write_rds(str_glue('./inst/extdata/GBD_agestructure.rds'))

# grid_info ----
world  <- rnaturalearth::ne_countries(type = "sovereignty", scale = "medium")

f_gbd <- file.path(
  Sys.getenv("NJUBOX"),
  '0.Data/4.Health Data/IHME/GBD2019/IHME-GBD_2019_DATA-96c44824-1.csv'
)

loc_nm <- c(
  "Vietnam" = "Viet Nam",
  "Venezuela" = "Venezuela (Bolivarian Republic of)",
  "Vatican" = "Italy",
  "Micronesia" = "Micronesia (Federated States of)",
  "Marshall Is." = "Marshall Islands",
  "Tanzania" = "United Republic of Tanzania",
  "Taiwan" = "Taiwan (Province of China)",
  "Syria" = "Syrian Arab Republic",
  "eSwatini" = "Eswatini",
  "S. Sudan" = "South Sudan",
  "South Korea" = "Republic of Korea",
  "Somaliland" = "Somalia",
  "Solomon Is." = "Solomon Islands",
  "São Tomé and Principe" = "Sao Tome and Principe",
  "St. Vin. and Gren." = "Saint Vincent and the Grenadines",
  "St. Kitts and Nevis" = "Saint Kitts and Nevis",
  "Russia" = "Russian Federation",
  "North Korea" = "Democratic People's Republic of Korea",
  "W. Sahara" = "Morocco",
  "Moldova" = "Republic of Moldova",
  "Liechtenstein" = "Germany",
  "Laos" = "Lao People's Democratic Republic",
  "Kosovo" = "Serbia",
  "Iran" = "Iran (Islamic Republic of)",
  "Eq. Guinea" = "Equatorial Guinea",
  "Dominican Rep." = "Dominican Republic",
  "N. Cyprus" = "Cyprus",
  "Dem. Rep. Congo" = "Democratic Republic of the Congo",
  "Central African Rep." = "Central African Republic",
  "Brunei" = "Brunei Darussalam",
  "Bosnia and Herz." = "Bosnia and Herzegovina",
  "Bolivia" = "Bolivia (Plurinational State of)",
  "Antigua and Barb." = "Antigua and Barbuda",
  "Siachen Glacier" = "India"
)

unmatched_loc <- left_join(
  # world %>% mutate(location = name),
  world %>% mutate(location = str_replace_all(name, !!loc_nm)),
  read_csv(f_gbd) %>% filter(sex == "Both", age == "<5 years", cause == "Stroke")
) %>% filter(is.na(val))

ggplot(unmatched_loc) + geom_sf() + geom_sf_text(aes(label = name), ) + 
  theme(axis.title = element_blank())

unique(unmatched_loc$name)

"Tanzania" %>% map(~{list(
  world %>% filter(str_detect(name, .x)),
  read_csv(f_gbd) %>% filter(str_detect(location, .x))
)})

# world %>% st_drop_geometry() %>% arrange(region_un, subregion, name) %>% 
#   writexl::write_xlsx("./Data/Country-Scenario mapper.xlsx")

world_grids <- st_as_stars(st_bbox(), dx = .5, dy = .5) %>% st_as_sf() %>% 
  mutate(st_centroid(geometry) %>% st_coordinates() %>% as.data.frame(),
         across(X:Y, ~as.character(round(.x, 2)))) %>% 
  select(-values) %>% rownames_to_column("FID")

gridinfo <- st_intersection(
  world_grids,
  world %>% mutate(location = str_replace_all(name, !!loc_nm)) %>% 
    group_by(location) %>% summarise(geometry=st_union(geometry))
) %>% 
  mutate(shp.area = st_area(geometry)) %>% 
  filter(!is.na(location)) %>% group_by(FID) %>% 
  filter(shp.area == max(shp.area)) %>% ungroup() %>% 
  st_drop_geometry()

gridinfo %>% mutate(across(X:Y, as.numeric)) %>% 
  st_as_stars(coords = c("X","Y")) %>% st_as_sf() %>% st_set_crs(4326) %>% 
  ggplot() + 
  geom_sf(aes(fill = location), color = "gray80") + 
  geom_sf(data = world, fill = NA) +
  theme(legend.position = "none")

gridinfo %>% ungroup %>% rename_with(tolower, X:Y) %>% select(-FID, -shp.area) %>% 
  write_rds(str_glue("./inst/extdata/Grid_info.rds"))
