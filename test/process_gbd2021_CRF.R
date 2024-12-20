library(tidyverse)

matchable <- \(x, dit) as.character(round(x, dit))

MRBRT2021_dir <- file.path(
  Sys.getenv("NJUBOX"),
  "0.Data/4.Health Data/IHME_GBD_2021_AIR_POLLUTION_1990_2021_RR_CURVES/"
)

# list.files(MRBRT2021_dir, pattern = "MEAN") %>% .[1] %>% 
#   file.path(MRBRT2021_dir, .) %>% read_csv()

MRBRT2021_files <- list.files(MRBRT2021_dir, pattern = "DRAWS")

MRBRT2021_curves <- MRBRT2021_files %>%
  
  str_subset("PM") %>%
  
  set_names(\(x) str_remove_all(x, "IHME_GBD_2021_AIR_POLLUTION_1990_2021_|RR_|_DRAWS_Y2022M01D31.CSV")) %>%
  
  imap(~{

    info <- str_match(.y, "([^_]+)_(.+)") %>% as.vector()

    CR <- read_csv(file.path(MRBRT2021_dir,.x)) %>% select(exposure, matches("draw"))

    exposure_range <- c(min(CR$exposure), max(CR$exposure))
    
    CR_format <- data.frame(
      exposure = matchable(seq(exposure_range[1], exposure_range[2], by = .01), 2)
    ) %>% left_join(CR %>% mutate(exposure = matchable(exposure, 2)))

    CR_test <- CR_format %>% filter(if_any(everything(), is.na))

    if (nrow(CR_test) > 0) {
      CR_format <- CR_format %>% mutate(across(where(is.numeric), zoo::na.approx))
    }

    CR_std <- CR_format %>% 
      filter(
        exposure %in% matchable(seq(min(CR$exposure), max(CR$exposure),by = .1), 1)
      ) %>% 
      mutate(
        summa = pmap(select(., matches("draw")), ~quantile(c(...), probs = c(.025, .5, .975)))
      ) %>% 
      select(-matches("draw")) %>% 
      unnest_wider(summa) %>% 
      mutate(
        risk = info[2],
        cause = info[3] %>% str_replace_all(c(
          BIRTH_WEIGHT = "BW",
          GESTATIONAL_AGE_SHIFT = "GAS",
          DIABETES = "DM2",
          ISCHEMIC_HEART_DISEASE = "IHD",
          LOWER_RESPIRATORY_INFECTIONS = "LRI",
          LUNG_CANCER = "LC"
        )),
        .before = 1
      )

  }) %>% list_rbind()

# MRBRT2021_files <- list.files(MRBRT2021_dir, pattern = "MEAN")
# 
# MRBRT2021_curves <- MRBRT2021_files %>%
#   
#   set_names(~str_remove_all(.x, "IHME_GBD_2021_AIR_POLLUTION_1990_2021_|RR_|_DRAWS_|_MEAN_|Y2022M01D31.CSV")) %>%
#   
#   imap(~{
#     
#     info <- str_match(.y, "([^_]+)_(.+)") %>% as.vector()
#     
#     CR <- read_csv(file.path(MRBRT2021_dir,.x))
#     
#     exposure_range <- seq(min(CR$exposure),max(CR$exposure), by = .01) %>% round(2)
#     
#     CR_format <- data.frame(
#       exposure = sort(c(exposure_range, round(CR$exposure, 2))) %>% .[!duplicated(.)] %>% as.character
#     ) %>% left_join(CR %>% mutate(exposure = as.character(round(exposure, 2))))
#     
#     CR_test <- CR_format %>% filter(if_any(everything(), is.na))
#     
#     if (nrow(CR_test) > 0) {
#       CR_format <- CR_format %>% mutate(across(where(is.numeric), ~zoo::na.approx(.x)))
#     }
#     
    CR_std <- CR_format %>%
      filter(exposure %in% exposure_range) %>%
      mutate(
        risk = info[2],
        cause = info[3] %>% str_replace_all(c(
          BIRTH_WEIGHT = "BW",
          GESTATIONAL_AGE_SHIFT = "GAS",
          DIABETES = "DM2",
          ISCHEMIC_HEART_DISEASE = "IHD",
          LOWER_RESPIRATORY_INFECTIONS = "LRI",
          LUNG_CANCER = "LC"
        )),
        .before = 1
      ) %>% filter(
        exposure %in% c(seq(min(CR$exposure), max(CR$exposure), by = .1) %>% round(1) %>% as.character())
      ) %>% select(!!c("risk","cause","exposure","mean","lower","upper"))
#     
#     return(CR_std)
#     
#   }) %>% list_rbind()

MRBRT2021_curves %>% 
  rename(LOWER = "2.5%", MEAN = "50%", UPPER = "97.5%") %>% 
  filter(risk == "OZONE") %>% select(-risk) %>% 
  mutate(across(c(LOWER, MEAN, UPPER), exp)) %>% 
  pivot_longer(
    -c(cause, exposure), names_to = 'metric', values_to = "RR"
  ) %>% 
  LYFtools::split_by(metric) %>% .[[1]] %>% #-> .x
  map(~{
    RR_tmrel <- .x %>% 
      filter(as.numeric(exposure) >= 29.1 & as.numeric(exposure) <= 35.7) %>% 
      group_by(cause) %>% summarise(RR_tmrel = mean(RR, na.rm = T)) %>% ungroup()
    
    left_join(.x, RR_tmrel) %>% mutate(RR = pmax(RR/RR_tmrel, 1), exposure = as.numeric(exposure)) 
    
    .x %>% mutate(exposure = as.numeric(exposure)) %>% 
      ggplot() + geom_point(aes(x = exposure, y = RR))
  })
  
ggplot() + 
  geom_line(
    data = MRBRT2021_curves %>% 
      rename(LOWER = "2.5%", MEAN = "50%", UPPER = "97.5%") %>% 
      filter(risk == "PM", cause == "LC") %>% 
      mutate(
        age = "ALL",
        # across(c(LOWER, MEAN, UPPER), exp)),
      )
    ,
    aes(x = as.numeric(exposure), y = MEAN, group = cause), color = "red"
  ) + 
  geom_line(
    data = AttrMort::MRBRT2019_Lookup_Table$MEAN %>% 
      pivot_longer(-1, names_to = c("cause", "age"), names_sep = "_", values_to = "RR") %>% 
      filter(str_detect(cause, "LC")),
    aes(x = as.numeric(dose), y = RR, group = cause),
    color = "blue", alpha = .4
  )
  # facet_wrap(~age)

MRBRT2021_curves %>%
  rename(LOW = "2.5%", MEAN = "50%", UP = "97.5%") %>%
  filter(risk == "PM", as.numeric(exposure) <= 300) %>% 
  select(-risk) %>% rename(dose = exposure) %>% 
  mutate(age = "ALL") %>%
  pivot_longer(c("LOW", "MEAN", "UP"),
               names_to = "metric",
               values_to = "RR") %>%
  pivot_wider(
    names_from = c("cause", "age"),
    names_sep = "_",
    values_from = RR
  ) %>%
  LYFtools::split_by(metric) -> MRBRT2021_Lookup_Table

MRBRT2021_Lookup_Table$MEAN %>% 
  pivot_longer(-1, names_to = c("cause","age"), names_sep = "_", values_to = "RR") %>%
  filter(!cause %in% c("BW", "GAS")) %>% 
  ggplot() + geom_line(aes(x = as.numeric(dose), y = RR, group = cause, color = cause))

