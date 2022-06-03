library(tidyverse)
library(readxl)

matchable <- function(num, dgt = 1) num %>% round(digits = dgt) %>% str_c

FID_info <- readfile$FID %>% read_xlsx %>% 
  mutate(across(where(is.numeric) & FID, matchable, dgt = 0))

Pop <- readfile$Pop %>% read_csv %>% 
  mutate(across(where(is.numeric) & FID, matchable, dgt = 0))

PM_real <- readfile$PM_real %>% read_csv %>%
  mutate(across(where(is.numeric) & FID, matchable, dgt = 0),
         across(-FID, matchable, dgt = 1))

# Specify UNREAL PM2.5 data, used for only counter-fact scenario.

PM_cf <- if (file.exists(readfile$PM_cf)) {
  readfile$PM_cf %>% read_csv %>% 
    mutate(across(where(is.numeric) & FID, matchable, dgt = 0),
           across(-FID, matchable, dgt = 1))
} else NULL

MortRate <- readfile$MortRate %>% read_csv %>%
  pivot_longer(cols = c(-year,-endpoint), names_to = 'agegroup',values_to = 'MortRate') %>% 
  pivot_wider(names_from = 'year', values_from = 'MortRate')

AgeGroup <- readfile$AgeGroup %>% read_csv %>%
  pivot_longer(cols = -`year`, names_to = 'agegroup', values_to = 'AgeStruc') %>%
  pivot_wider(names_from = 'year', values_from = 'AgeStruc') %>%
  mutate(across(-agegroup, prop.table))

RR_table <- expand_grid(excel_sheets(readfile$CR), readfile$CR) %>% deframe %>% 
  imap(~ read_excel(.x, sheet = .y) %>% 
         mutate(across(where(is.numeric) & concentration, matchable, dgt = 1)))
