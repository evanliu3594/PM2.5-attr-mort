library(tidyverse)
library(stars); sf_use_s2(F)
require(mapchina)

CN_domain <- st_bbox(c(xmin = 73, xmax = 136, ymin = 6, ymax = 54))

mapchina::china %>% ggplot() + geom_sf(aes(fill = Code_Province))

CN_fishnet <- mapchina::china %>% select(contains('Code')) %>% 
  mutate(across(where(is.character), as.integer)) %>% 
  st_rasterize(
    st_as_stars(CN_domain, dx = .1, dy = .1, values = NA) %>% st_set_crs(4326)
  ) %>% as.data.frame %>% rownames_to_column('FID') %>% na.omit()

FID_info <- c(
  list( CN_fishnet %>% mutate(across(contains('Code'), as.character))),
  c('Province', 'Perfecture', 'County') %>% 
    map(~ mapchina::china %>% st_drop_geometry %>% select(contains(.x)) %>% unique)
) %>% reduce(left_join) %>% 
  select(-contains('Code')) %>% rename_with(~str_remove(.x, 'Name_'), contains('Name')) %>% 
  add_column(Country = '中国', .after = 'y')

FID_info %>% writexl::write_xlsx('./Data/FID_information.xlsx')

china
