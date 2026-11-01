#======================================================================================#
#      oooooooooo oooo     oooo                                                        #
#      888    888 8888o   888   ooooo    8888888                                       #
#     888oooo88  88 888o8 88  o   o88   8oooooo                                        #
#    888        88  888  88     o8          o88                                        #
#  o888o      o88o  8  o88o o88ooo8 88 88oo88                                          #
#                                                                                      #
#        o        o8     o8                 oooo     oooo                        o8    #
#       888     o888oo o888oo oo oooooo     8888o   888   ooooooo  oo oooooo  o888oo   #
#      8  88    888    888    888     88   88 888o8 88 888     888 888    888 888      #
#     8ooo888  888    888    888          88  888  88 888     888 888        888       #
#   o8o  o888o 888o   888o o888o        o88o  8  o88o  88ooo88  o888o        888o      #
#--------------------------------------------------------------------------------------#
#       -- PM2.5 健康负担综合计算程序 --                                                         #
#                                                                                      #
#  创建人：Yifan LIU，20210505                                                              #
#       更新 210525：用 tidy-R 语法替换矩阵运算                                                    #
#       更新 220602：简化代码                                                                 #
#       * 增加 GEMM 模型支持                                                                 #
#       * 改进不确定性评估，改进结果返回                                                              #
#       更新 260610：增加自定义 CRF 支持                                                         #
#       更新 260618：模块化引擎重构，增加 verbose/debug 参数                                          #
#       更新 261026：PWRR 按 domain × endpoint × agegroup 计算；CI 分支使用                       #
#                      对应 PWRR；旧的单一 domain PWRR 结果失效                                    #
#                                                                                      #
#======================================================================================#

# 安装依赖（只运行一次） ----
# install.packages(c("tidyverse", "jsonlite", "writexl", "readxl"))

# 核心函数加载 ----

library(tidyverse)
library(writexl)
library(readxl)
source('./Code/model.R', encoding = 'UTF8')
source('./Code/data.R', encoding = 'UTF8')
source('./Code/mortality.R', encoding = 'UTF8')
source('./Code/aggregation.R', encoding = 'UTF8')

# C-R 模型设置 ----
# 本段用于选择健康影响计算所用的 C-R 模型。
# 支持的 C-R 函数：'IER'、'NCD+LRI'（GEMM 的一部分）、'5COD'（GEMM 的一部分）、'MRBRT'、'MRBRT2023'、'O3'、'NO2'

# set_Model('MRBRT')       # GBD 2023 默认别名
# set_Model('MRBRT2023')   # GBD 2023 MR-BRT（显式）
set_Model('NCD+LRI')
# set_Model('5COD')
# set_Model('IER')
# set_Model('O3')          # GBD 2023 O3（COPD）
# set_Model('NO2')         # GBD 2023 NO2（asthma）

# 输入数据 ----
# 修改这些路径即可指向另一个实例。路径相对于
# 项目根目录。CONC_CF_FILE 保持 NULL 时，用 Conc_real 计算绝对负担；
# 设定为反事实文件时，执行情景计算。

GRIDS_FILE      <- "./Data/Grid_info_instance_260617.xlsx"
POP_FILE        <- "./Data/GridPop_instance_260617.xlsx"
CONC_REAL_FILE  <- "./Data/GridPM25_instance_260617.xlsx"
CONC_CF_FILE    <- NULL   # 示例："./Data/GridPM25_cf_instance_260617.xlsx"
MORT_RATE_FILE  <- "./Data/GBD_mortality_instance_260617.xlsx"
AGE_GROUP_FILE  <- "./Data/GBD_agestructure_instance_260617.xlsx"

# 数据读取 ----

read_files(
  Grids = GRIDS_FILE,
  Pop = POP_FILE,
  Conc_real = CONC_REAL_FILE,
  Conc_cf = CONC_CF_FILE,
  MortRate = MORT_RATE_FILE,
  AgeGroup = AGE_GROUP_FILE,
  dgt_grid = 2,
  conc_unit = "ug/m3"
)

scenarios <- get_scenarios()

# 格级计算，含 95% CI（一次遍历得到 MEAN + UP + LOW） ----
# read_files() 已校验输入键，因此跳过逐次复查
# （与分解共用同一可信输入路径；v5.9 契约）。
grid_ci <- map(set_names(scenarios), ~ Mortality_at(at = .x, CI = "RANGE", validate_keys = FALSE))
# grid_mean <- map(set_names(scenarios), ~ Mortality_at(at = .x, CI = "MEAN"))
# grid_low <- map(set_names(scenarios), ~ Mortality_at(at = .x, CI = "LOW"))
# grid_up <- map(set_names(scenarios), ~ Mortality_at(at = .x, CI = "UP"))

# 结果输出 ----
# 逐步调试且不写文件时，把 WRITE_RESULTS 设为 FALSE。
WRITE_RESULTS <- TRUE
RESULT_AT <- "Country"
if (WRITE_RESULTS) {
  walk(
    c("total", "endpoint", "agegroup", "all"),
    ~ aggregate_range(
      grid_ci,
      at = RESULT_AT,
      by = .x,
      write = TRUE
    )
  )
}

# 聚合 ----
# 取消注释所需的层级与拆分组合。
# at  = "grid" | "geo" | "x" | "y" | "Country" | Grid_info 中的任意列
# by  = "total"（仅总量）| "all" | "endpoint" | "agegroup"
# write = FALSE | TRUE（写入 ./Results/xxx）

## 格级 ----
# aggregate_range(grid_ci, at = "grid", by = "total", write = FALSE)
# aggregate_range(grid_ci, at = "grid", by = "endpoint", write = FALSE)
# aggregate_range(grid_ci, at = "grid", by = "agegroup", write = FALSE)
# aggregate_range(grid_ci, at = "grid", by = "all", write = FALSE)

## 纬向 / 经向 ----
# aggregate_range(grid_ci, at = "x", by = "total", write = FALSE)
# aggregate_range(grid_ci, at = "x", by = "endpoint", write = FALSE)
# aggregate_range(grid_ci, at = "x", by = "agegroup", write = FALSE)
# aggregate_range(grid_ci, at = "x", by = "all", write = FALSE)

# aggregate_range(grid_ci, at = "y", by = "total", write = FALSE)
# aggregate_range(grid_ci, at = "y", by = "endpoint", write = FALSE)
# aggregate_range(grid_ci, at = "y", by = "agegroup", write = FALSE)
# aggregate_range(grid_ci, at = "y", by = "all", write = FALSE)

## 国家级 ----
# aggregate_range(grid_ci, at = "Country", by = "total", write = FALSE)
# aggregate_range(grid_ci, at = "Country", by = "endpoint", write = FALSE)
# aggregate_range(grid_ci, at = "Country", by = "agegroup", write = FALSE)
# aggregate_range(grid_ci, at = "Country", by = "all", write = FALSE)

## 省级（若有）----
# aggregate_range(grid_ci, at = "Province", by = "total", write = FALSE)
# aggregate_range(grid_ci, at = "Province", by = "endpoint", write = FALSE)
# aggregate_range(grid_ci, at = "Province", by = "agegroup", write = FALSE)
# aggregate_range(grid_ci, at = "Province", by = "all", write = FALSE)

# # 区域级（若有）----
# aggregate_range(grid_ci, at = "Region", by = "total", write = FALSE)
# aggregate_range(grid_ci, at = "Region", by = "endpoint", write = FALSE)
# aggregate_range(grid_ci, at = "Region", by = "agegroup", write = FALSE)
# aggregate_range(grid_ci, at = "Region", by = "all", write = FALSE)

##  一次输出全部地理层级与全部分组字段 ----
# aggregate_range(grid_ci, at = "geo", by = "total", write = FALSE)
# aggregate_range(grid_ci, at = "geo", by = "all", write = FALSE)

# 与 PWRR 一致的不确定性 ====
# aggregate_sigma() 使用传入的 RANGE 分支（grid_ci 带 MEAN/UP/LOW）。
# 传入字符向量时，每个情景各调用一次 Mortality_at(CI = "RANGE")。
# PWRR domain 与聚合列 `at` 不同时，传入 pwr_domain。
# sigma_result <- aggregate_sigma(grid_ci, at = "Country", by = "total", write = FALSE)
