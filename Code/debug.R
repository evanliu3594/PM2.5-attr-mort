ISO_Country

ISO_Grid_info$Country %>% unique()

Grid_info$Country %>% unique()

full_ISOlist <- ISO_Country %>% full_join(
  Grid_info$Country %>% unique %>% enframe() %>% rename(ISO_full = value)
) %>% select(-name) %>% full_join(
  ISO_Grid_info$Country %>% unique %>% enframe() %>% rename(ISO = value)
) %>% select(-name) %>% filter(!(is.na(ISO) & is.na(ISO_full))) %>% 
  rownames_to_column("ID") %>% mutate(ID = as.integer(ID))

full_ISOlist

CNTRY.LIST_Grid_info <- Grid_info %>% filter(!is.na(Country)) %>% split(.$Country) %>% map(~{
  cat(.x$Country %>% unique)
  cat("\n")
  .x %>% mutate(Country = 1, across(x:y, as.numeric)) %>% st_as_stars() %>% st_set_crs(4326) %>% 
    st_as_sf(as_points = F, merge = T)
})

Grid_info %>% filter(Country=="Antigua and Barbuda") %>% 
  mutate(Country = 1, across(x:y, as.numeric)) %>% st_as_stars() %>% st_set_crs(4326) %>% 
  st_as_sf(as_points = F, merge = T)

quancheng_map <- Grid_info %>% mutate(across(x:y, as.numeric)) %>% rename(ISO_full = Country) %>% 
  st_as_stars() %>% st_set_crs(4326) %>% st_as_sf(as_points = F) %>% 
  split(.$ISO_full) %>% imap_dfr(~ .x %>% st_union() %>% st_sf %>% mutate(Country = .y))

quancheng_map %>% filter(Country == "Antigua and Barbuda") %>% ggplot()+geom_sf()

jiancheng_map <- ISO_Grid_info %>% mutate(across(x:y, as.numeric)) %>% rename(ISO_full = Country) %>% 
  st_as_stars() %>% st_set_crs(4326) %>% st_as_sf(as_points = F) %>% 
  split(.$ISO_full) %>% imap_dfr(~ .x %>% st_union() %>% st_sf %>% mutate(Country = .y))

jiancheng_map %>% filter(Country == "ARM") %>% ggplot()+geom_sf()

for (I in 1:nrow(full_ISOlist)) {
  ggplot()+
    geom_sf(data = filter(jiancheng_map,Country == full_ISOlist[I,"ISO"]),
            mapping = aes(geometry = geometry), color = "red", alpha = .6) +
    geom_sf(data = filter(quancheng_map,Country == full_ISOlist[I,"ISO_full"]),
            mapping = aes(geometry = geometry), color = "blue", alpha = .6)
  ggsave(str_glue("./Figure/Compare_ISO/{full_ISOlist[I,'ISO']}.{full_ISOlist[I,'ISO_full']}.png"),
         width = 16,height = 12,units = "cm")
} 

.x <- "SSP3-RCP26_2060"

read_files(
  Grids = './Data/Grid_information_iso.csv',
  Pop = './Data/GridPop_SSP_RCP26.csv',
  Conc_real = './Data/GridPM25_SSP_RCP26.csv',
  Conc_cf = './Data/PM_Ctrl.csv',  # PM_cf works only in counter-fact scenario
  MortRate = "./Data/Yang_Future_basemortal_country_SSP_RCP26.csv",
  AgeGroup = './Data/Fnl_SSP_agestructure_country_RCP26.csv'
)

ISO_Grid_info <- Grid_info


ISO_Debug <- Mortality_debug(
  Grids = ISO_Grid_info,
  RR = RR_table$MEAN,
  Conc_r = Conc_real %>% select(x:y, concentration = !!.x),
  Conc_c = NULL,
  pop = Pop %>% select(x:y, Pop = !!.x),
  ag = AgeGroup %>% select(domain, agegroup, AgeStruc = !!.x),
  mRate = mortrate_std(MortRate, .x)
) %>% rename_with(~ str_c(.x,"_ISO"), c("MortRate","AgeStruc"))


read_files(
  Grids = './Data/GRID_information_instance_iso.csv',
  Pop = './Data/GridPop_SSP_RCP26.csv',
  Conc_real = './Data/GridPM25_SSP_RCP26.csv',
  Conc_cf = './Data/PM_Ctrl.csv',  # PM_cf works only in counter-fact scenario
  MortRate = "./Data/Yang_Future_basemortal_country_SSP_RCP26.csv",
  AgeGroup = './Data/Fnl_SSP_agestructure_country_RCP26.csv'
)

Origin_Grid_info <- Grid_info

Origin_Debug <- Mortality_debug(
  Grids = Origin_Grid_info,
  RR = RR_table$MEAN,
  Conc_r = Conc_real %>% select(x:y, concentration = !!.x),
  Conc_c = NULL,
  pop = Pop %>% select(x:y, Pop = !!.x),
  ag = AgeGroup %>% select(domain, agegroup, AgeStruc = !!.x),
  mRate = mortrate_std(MortRate, .x)
) %>% rename_with(~ str_c(.x,"_Origin"), c("MortRate","AgeStruc"))


Cross_validation <- left_join(
  Origin_Debug, ISO_Debug, by = c("x","y","concentration","Pop","endpoint","agegroup")
) %>% mutate(
  across(x:y, as.numeric),
  MortRate.vary = MortRate_ISO / MortRate_Origin,
  AgeStruc.vary = AgeStruc_ISO / AgeStruc_Origin,
  MortRate.vary = case_when(
    MortRate.vary > 1 ~ "1",
    MortRate.vary == 1 ~ "0",
    MortRate.vary < 1 ~ "-1",
    TRUE ~ NA_character_
  ),
  AgeStruc.vary = case_when(
    AgeStruc.vary > 1 ~ "1",
    AgeStruc.vary == 1 ~ "0",
    AgeStruc.vary < 1 ~ "-1",
    TRUE ~ NA_character_
  ),
) %>% select(x, y, endpoint, agegroup, MortRate.vary, AgeStruc.vary) %>%
  st_as_stars(coords = 1:4) %>% st_set_crs(4326)

layers <- expand_grid(
  endpoint = st_get_dimension_values(Cross_validation, which = "endpoint"),
  agegroup = st_get_dimension_values(Cross_validation, which = "agegroup")
) 

walk2(
  .x = layers %>% pull(endpoint),
  .y = layers %>% pull(agegroup),
  ~{
    cat(str_glue("{.x}-{.y}\n\n"))
    m <- Cross_validation["MortRate.vary",,, .x, .y] %>% 
      qtm("MortRate.vary", raster.palette = c("gray77","blue","red"))
    mname <- str_glue("./Figure/examine_MortRate/{.x}_{.y}.png")
    tmap_save(m, filename = mname, width=1600, height=900, asp=0)
    
    a <- Cross_validation["AgeStruc.vary",,, .x, .y] %>% 
      qtm("AgeStruc.vary", raster.palette = c("gray77","blue","red"))
    
    aname <- str_glue("./Figure/examine_AgeStruc/{.x}_{.y}.png")
    tmap_save(a, filename = aname, width=1600, height=900, asp=0)
  })

