library(tidyverse)
library(readxl)

fuse_read <- function(filename) {
  if (str_detect(filename,'csv$')) filename %>% read_csv
  else if (str_detect(filename,'xlsx$')) filename %>% read_xlsx
}

matchable <- function(num, dgt = 1) num %>% round(digits = dgt) %>% str_c

FID_info <- readfile$FID %>% fuse_read %>% 
  mutate(across(where(is.numeric) & FID, matchable, dgt = 0))

Pop <- readfile$Pop %>% fuse_read %>% 
  mutate(across(where(is.numeric) & FID, matchable, dgt = 0))

PM_real <- readfile$PM_real %>% fuse_read %>%
  mutate(across(where(is.numeric) & FID, matchable, dgt = 0),
         across(-FID, matchable, dgt = 1))

# Specify UNREAL PM2.5 data, used for only counter-fact scenario.

PM_cf <- if (file.exists(readfile$PM_cf)) {
  readfile$PM_cf %>% fuse_read %>% 
    mutate(across(where(is.numeric) & FID, matchable, dgt = 0),
           across(-FID, matchable, dgt = 1))
} else NULL

MortRate <- readfile$MortRate %>% fuse_read %>%
  pivot_longer(cols = c(-year,-endpoint), names_to = 'agegroup',values_to = 'MortRate') %>% 
  pivot_wider(names_from = 'year', values_from = 'MortRate')

AgeGroup <- readfile$AgeGroup %>% fuse_read %>%
  pivot_longer(cols = -`year`, names_to = 'agegroup', values_to = 'AgeStruc') %>%
  pivot_wider(names_from = 'year', values_from = 'AgeStruc') %>%
  mutate(across(-agegroup, prop.table))

RR_table <- expand_grid(excel_sheets(readfile$CR), readfile$CR) %>% deframe %>% 
  imap(~ read_excel(.x, sheet = .y) %>% 
         mutate(across(where(is.numeric) & concentration, matchable, dgt = 1)))

rm(fuse_read, readfile)