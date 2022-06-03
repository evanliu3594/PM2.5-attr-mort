library(tidyverse)

age_group <- c(
  '15-19 years',
  '20-24 years',
  '25-29 years',
  '30-34 years',
  '35-39 years',
  '40-44 years',
  '45-49 years',
  '50-54 years',
  '55-59 years',
  '60-64 years',
  '65-69 years',
  '70-74 years',
  '75-79 years',
  '80-84 years',
  '85-89 years',
  '90-94 years',
  '95+ years',
  '<5 years',
  '5-9 years',
  '10-14 years'
) %>% set_names %>% 
  map_chr( ~ .x %>% str_extract('\\d{1,2}(?=[-+])')) %>% 
  replace_na('0')

end_point <- c(
  "Chronic obstructive pulmonary disease" = 'COPD',
  "Diabetes mellitus type 2" = 'DM',
  "Diarrheal diseases" = "Diarrheal diseases",
  "Neonatal preterm birth" = "Neonatal preterm birth",
  "Neonatal encephalopathy due to birth asphyxia and trauma" = "Neonatal encephalopathy due to birth asphyxia and trauma",
  "Neonatal sepsis and other neonatal infections" = "Neonatal sepsis and other neonatal infections",
  "Hemolytic disease and other neonatal jaundice" = "Hemolytic disease and other neonatal jaundice",
  "Other neonatal disorders" = "Other neonatal disorders",
  "Ischemic heart disease" = 'IHD',
  "Non-communicable diseases" = 'NCD',
  "Tracheal, bronchus, and lung cancer" = 'LC',
  "Lower respiratory infections" = 'LRI',
  "Upper respiratory infections" = 'URI',
  "Otitis media" = "Otitis media",
  "Meningitis" = "Meningitis",
  "Encephalitis" = "Encephalitis",
  "Stroke" = "Stroke"
)

RAW.DATA <- read_csv('Data/RAW/IHME-GBD_2019_DATA-25efb80c-1.csv')

RAW.DATA %>% mutate(.keep = 'unused',
                    agegroup = age_group[age],
                    endpoint = end_point[cause]) %>% 
  arrange(year,endpoint,as.integer(agegroup)) %>% 
  select(year,agegroup,endpoint,mortrate = val) %>% 
  pivot_wider(names_from = endpoint,values_from = mortrate) %>% 
  mutate(`NCD+LRI` = replace_na(NCD,0) + replace_na(LRI,0)) %>% 
  pivot_longer(cols = -c(year,agegroup),names_to = 'endpoint',values_to = 'mortrate') %>% 
  mutate(mortrate = replace_na(mortrate,0)) %>% 
  pivot_wider(names_from = 'agegroup', values_from = 'mortrate') %>% 
  write_excel_csv('./Data/GBD_incidence_China_2000-2019.csv')
