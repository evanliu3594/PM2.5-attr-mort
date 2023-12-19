library(tidyverse)
library(stars)
sf_use_s2(F)

world <- st_read('./Data/WorldMap/geoBoundariesCGAZ_ADM0.shp')

st_as_stars(st_bbox(), dx = .5, dy = .5) %>% 
  st_join(world, as_points = F) %>% as.data.frame %>% 
  mutate(across(x:y, ~ round(.x, 2) %>% as.character)) %>% 
  filter(!is.na(ISO_CODE)) %>% left_join(read_csv('./Data/WorldMap/ISO_code.csv')) %>% 
  select(x, y, Country = FULL_NAME) %>% 
  write_xlsx('./Data/GRID_information_instance_220628.xlsx')

pm25 <- list.files("./Data/PM25_raw/", pattern = 'nc') %>%
  set_names(paste0("./Data/PM25_raw/", .)) %>%
  imap_dfr( ~ {
    read_ncdf(.y) %>% st_apply(c('lon', 'lat'), mean) %>% as.data.frame %>% na.omit %>% 
      mutate(across(lon:lat, ~ round(.x, 2) %>% as.character),
             scenario = str_remove_all(.x, "Annual_pm25_|\\.nc"))
  }) %>% pivot_wider(names_from = 'scenario', values_from = 'mean')

pm25 %>% rename(x = lon, y = lat) %>% 
  write_xlsx('./Data/GridPM25_instance_220628.xlsx')

pop <- list.files('D:/temp/', recursive = T, pattern = '[^d]\\.tif') %>% 
  set_names(paste0('D:/temp/', .)) %>% imap_dfr(~{
  read_stars(.y) %>% 
    st_warp(st_as_stars(st_bbox(),values = NA, dx = .5, dy = .5), use_gdal = T, method = 'sum') %>% 
    `names<-`('pop') %>% as.data.frame() %>% na.omit %>% mutate(
      across(x:y, ~ round(.x, 2) %>% as.character),
      scenario = str_extract(.x, "\\d{4}"))
}) %>% pivot_wider(names_from = 'scenario', values_from = 'pop')

pop %>% write_xlsx('./Data/GridPop_instance_220628.xlsx')

mortrate <- './Data/IHME-GBD_2019_DATA-2d0164ad-1.csv' %>% read_csv()

cause.abbr <- c(
  "Tracheal, bronchus, and lung cancer" = 'LC',
  "Lower respiratory infections" = 'LRI',
  "Stroke" = 'STROKE',
  "Ischemic heart disease" = 'IHD',
  "Non-communicable diseases" = 'NCD',
  "Chronic obstructive pulmonary disease" = 'COPD'
)

age.abbr <- c(
  "<5 year" = '0',
  "5-9 years" = '5',
  "10-14 years" = '10',
  "15-19 years" = '15',
  "20-24 years" = '20',
  "25-29 years" = '25',
  "30-34 years" = '30',
  "35-39 years" = '35',
  "40-44 years" = '40',
  "45-49 years" = '45',
  "50-54 years" = '50',
  "55-59 years" = '55',
  "60-64 years" = '60',
  "65-69 years" = '65',
  "70-74 years" = '70',
  "75-79 years" = '75',
  "80-84 years" = '80',
  "85-89 years" = '85',
  "90-94 years" = '90',
  "95+ years" = '95'
)

mortrate %>% 
  mutate(cause_name = cause.abbr[cause_name], age_name = age.abbr[age_name], year = as.character(year)) %>% na.omit() %>%
  select(year, domain = location_name, agegroup = age_name, endpoint = cause_name, mortrate = val) %>% 
  pivot_wider(names_from = 'endpoint', values_from = 'mortrate') %>% 
  mutate(`NCD+LRI` = NCD + LRI) %>% 
  pivot_longer(cols = -c(year:agegroup),names_to = 'endpoint', values_to = 'mortrate') %>% 
  pivot_wider(names_from = 'year', values_from = 'mortrate') %>% 
  mutate(`2030` = map_dbl(`2015`, ~ .x * runif(1))) %>% 
  write_xlsx('./Data/GBD_mortality_instance_220628.xlsx')

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


agegroup <- list.files('./Data/POP_raw/', pattern = 'CSV', full.names = T) %>% map_dfr(read_csv) %>% filter(
  sex_name == 'both',
  age_group_name %in% names(age.abbr),
  location_id %in% mortrate$location_id
) %>% mutate(age_group_name  = age.abbr[age_group_name]) %>% 
  select(year = year_id, domain = location_name, agegroup = age_group_name, pop = val)

agegroup %>% group_by(year,domain) %>% mutate(agestruc = prop.table(pop), .keep = 'unused') %>% 
  pivot_wider(names_from = 'year', values_from = 'agestruc') %>% 
  write_xlsx('./Data/GBD_agestructure_instance_220628.xlsx')
