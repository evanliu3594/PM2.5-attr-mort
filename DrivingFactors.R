#====================================================#
# 提取 PM2.5 健康负担的驱动因素                                 #
#====================================================#
# 入口脚本。分解函数位于 Code/decomposition.R。
# 修改配置块后，用 source() 加载本文件或用 Rscript 运行。
# NOTE 261026：PWRR 按 domain × endpoint × agegroup 计算；此日期之前产出的分解
# 结果失效。
# NOTE 261027：启用并发计划时，缺失的 factor state 由 Code/decomposition.R
# 并行构建。默认为串行。

# 核心模块加载 ----

library(tidyverse)
library(writexl)
library(readxl)
source('./Code/model.R', encoding = 'UTF8')
source('./Code/data.R', encoding = 'UTF8')
source('./Code/mortality.R', encoding = 'UTF8')
source('./Code/aggregation.R', encoding = 'UTF8')
source('./Code/kernel.R', encoding = 'UTF8')
source('./Code/decomposition.R', encoding = 'UTF8')

# 配置 ----

# C-R 模型：'IER'、'NCD+LRI'、'5COD'、'MRBRT'、'MRBRT2023'、'O3'、'NO2'
MODEL <- 'NCD+LRI'

# 分解的起止年份与空间单元。
START_YEAR <- 'base2015'
END_YEAR <- 'SSP1-Baseline_2030'
RESULT_AT <- 'Country'

# TRUE 运行全部 24 个序列，FALSE 只运行序列 1。
RUN24 <- TRUE

# FALSE 只加载数据与函数，不运行分解。
RUN_DRIVERS <- tolower(Sys.getenv("PM25_RUN_DRIVERS", unset = "true")) == "true"

# Code/kernel.R 的 worker 数；取 1 保持串行计划。
WORKERS <- as.integer(Sys.getenv("PM25_KERNEL_WORKERS", unset = "1"))

# 输入数据 ----
# 修改这些路径即可指向另一个实例。路径相对于
# 项目根目录。

GRIDS_FILE      <- "./Data/Grid_info_instance_260617.xlsx"
POP_FILE        <- "./Data/GridPop_instance_260617.xlsx"
CONC_REAL_FILE  <- "./Data/GridPM25_instance_260617.xlsx"
MORT_RATE_FILE  <- "./Data/GBD_mortality_instance_260617.xlsx"
AGE_GROUP_FILE  <- "./Data/GBD_agestructure_instance_260617.xlsx"

# C-R 函数设置 ----

set_Model(MODEL)

# 数据读取 ----

read_files(
  Grids = GRIDS_FILE,
  Pop = POP_FILE,
  Conc_real = CONC_REAL_FILE,
  MortRate = MORT_RATE_FILE,
  AgeGroup = AGE_GROUP_FILE,
  conc_unit = "ug/m3"
)

# 并发（可选） ----
# 默认为串行。该计划在本 R 会话内保持生效。
if (!is.na(WORKERS) && WORKERS > 1) {
  kernel_plan(workers = WORKERS)
}

# 运行 ----

if (RUN_DRIVERS) {
  aggregate_drivers(
    RUN24 = RUN24,
    start.year = START_YEAR,
    end.year = END_YEAR,
    domain = RESULT_AT
  )
}