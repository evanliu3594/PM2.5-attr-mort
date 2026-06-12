# PM25 Health Impact Calc Core
# By Yifan LIU 2021/05/15
# Improved By Yifan LIU With Deepseek on 260610
#
# This file sources all functional modules in dependency order.
# Source this from your analysis scripts as before.

library(tidyverse)
library(writexl)
library(readxl)

source('./Code/model.R',        encoding = 'UTF8')
source('./Code/data.R',         encoding = 'UTF8')
source('./Code/mortality.R',    encoding = 'UTF8')
source('./Code/uncertainty.R',  encoding = 'UTF8')
source('./Code/aggregation.R',  encoding = 'UTF8')
