# AGENTS.md — PM2.5-attr-mort（项目总纲）

> 本文件是项目总纲（路标）。用法见 `README.md`；数据流见 `docs/data-flow.md`；版本沿革见 `CHANGELOG.md`；函数说明见 `Code/*.R`；会话记录见 `handoff/`。
> 目录契约：数据结果放 `Results/`，图件放 `Figures/`；`tests/`、`handoff/`、`Code/build/` 保留本地但不入 git（用户 261026 拍板）。

## 0. 目录结构

| 目录 / 文件 | 职责 | 明细位置 |
|---|---|---|
| `Code/` | 计算引擎 | `Code/README.md`（待补）；函数说明见代码注释 |
| `Code/build/` | CR 构建脚本，本地保留，不入 git | `Data/RR_index/README.md` |
| `Code/experimental/` | 遗留实验脚本，未被加载，仅留档备查 | — |
| `Data/` | 原始数据、实例表、CRF 系数、RR 查找表 | `Data/README.md`、`Data/RR_index/README.md` |
| `Results/` | 数据结果表，不入 git | `README.md` |
| `Figures/` | 图件，不入 git | `README.md` |
| `tests/` | 本地回归测试，不入 git | `tests/run_tests.R` |
| `handoff/` | 本地会话交接，不入 git；最新一份 = 当前状态入口 | — |
| `docs/` | 程序设计文档（superpowers 计划与设计记录），随 git 入库 | `docs/README.md` |
| 根脚本 | `build_instance.R`、`HealthBurdenCalc.R`、`DrivingFactors.R` | `README.md` |

`Data/` 内部分层：原始只读（`PM25_raw/`、`IHME_Pop_raw/`、`IHME_MortRate_raw/`）；实例表（`*_instance_260617.xlsx`）；`CRF_coefficients/`；`RR_index/`。

## 1. 研究问题与目标

**意图基线（待用户确认）**：用网格化 PM2.5 浓度、人口、GBD 死亡率与年龄结构，估计 PM2.5 归因死亡；支持 IER、GEMM、MR-BRT、O3、NO2 多种 C-R 模型；可按格、省、区域、国家聚合。示例数据覆盖中国，框架可换区域。成功标准与边界待确认。

**问题**：不同 C-R 模型与浓度情景下，PM2.5 归因死亡是多少，如何分解？

**目标**：① 生成格级归因死亡与置信区间；② 按空间、终点、年龄聚合；③ 分解人口、老龄化、暴露与其他风险因素的贡献。

## 2. 基础原始数据（只读）

- `Data/PM25_raw/`：PM2.5 浓度原始数据。
- `Data/IHME_Pop_raw/`：IHME 人口。
- `Data/IHME_MortRate_raw/`：IHME GBD 死亡率与年龄结构。
- `Data/CRF_coefficients/`：IER/GEMM 参数 CSV。
- `Data/RR_index/`：预构建 RR 查找表。
- `Data/IHME_GBD_2023_…_RR_CURVES.zip`：GBD 2023 RR 原始包（185 MB，不入 git，重下需 IHME 登录）。
- 版本、年份、来源明细见 `Data/README.md` 与 `Data/RR_index/README.md`。

## 3. 研究方案与计算逻辑（概要）

管线：`build_instance.R` 生成实例表 → `Code/build/build_CR.R`、`build_CR_MRBRT2023.R` 或 `build_CR_GBD2023_gas.R` 生成 RR 查找表 → `set_Model()` / `RR_std()` 建 `.RR_std_tbl` → `read_files()` 读数据 → `Mortality()` / `Mortality_at()` 算 PWRR 与归因死亡 → `aggregate_range()` / `aggregate_sigma()` 聚合 → `Code/decomposition.R` 做 24 排列分解。公式与函数契约见 `Code/*.R` 内注释；数据流见 `docs/data-flow.md`。

### 3.1 已拍板口径（用户确认，勿再改）

| # | 口径 | 决定 | 日期 |
|---|---|---|---|
| ① | 目录命名 | 见文首目录契约（`Results/`、`Figures/`、本地目录不入 git） | 261026 |
| ② | GBD 2023 RR 接入 | 下载 GHDx 曲线并编译为项目可用 CR 表 | 261026 |
| ③ | PWRR 归一化 | 按域 × 终点 × 年龄档计算；CI 分支用对应 PWRR | 261026 |
| ④ | DrivingFactors 24 序列 | 按注释顺序明文表；state 规则显式；domain 注入 | 261026 |
| ⑤ | Mortality 计算结构 | 按 endpoint × agegroup 分组遍历；每组 grid 级长表；默认串行 | 261026 |
| ⑥ | MRBRT 默认指向 | `set_Model("MRBRT")` = `MRBRT2023`；`MRBRT2021` 保留 | 261026 |
| ⑦ | O3/NO2 默认 | `O3` = GBD 2023 COPD；`NO2` = GBD 2023 asthma；`O3_legacy` / `NO2_allcause` 保留 | 261026 |
| ⑧ | 并发内核 | 构建块 `future.callr` + `furrr` + `tryCatch` + `do.call`；并发默认关闭 | 261027 |

### 3.2 代理决策（agent 决定，用户未反对）

「取值」即当前契约。决策理由与完整沿革见 `CHANGELOG.md` 与 `handoff/`。

| # | 选择项 | 取值 | 日期 |
|---|---|---|---|
| ① | 新模型命名 | `MRBRT2023` 为显式 2023 别名；`MRBRT2021` 为历史别名 | 261026 |
| ② | MRBRT2023 终点 | copd、dm2、ihd、lc、lri、stroke | 261026 |
| ③ | MRBRT2023 网格 | 0–300 µg/m³，步长 0.1；全年龄 `_ALL` 填充 | 261026 |
| ④ | 源文件尺度 | COPD/DM2/LC 按 RR；IHD/Stroke/LRI 按 log(RR) | 261026 |
| ⑤ | 插值 | log(RR) 线性插值到 0.1 网格 | 261026 |
| ⑥ | 输出文件名 | `MRBRT2023_Lookup_Table_Build_261026.xlsx` | 261026 |
| ⑦ | 原始 zip | 放 `Data/`，加入 `.gitignore` | 261026 |
| ⑧ | RANGE 分支 PWRR | 三分支各用对应 PWRR | 261026 |
| ⑨ | O3/NO2 曲线口径 | GBD 2023；ppb；O3 0–50、NO2 0–100；NO2 为 asthma；NO2 LOW 截到 1；保留 legacy | 261026 |
| ⑩ | O3/NO2 CI 分母 | `pwr_branch: "mean"`；其余模型默认 `"branch"` | 261026 |
| ⑪ | 并发范围 | 内核 `Code/kernel.R`；默认串行；`PM25_KERNEL_WORKERS` 开启；对象为 `Decomposition()` 状态与状态摘要 | 261027 |
| ⑫ | RR 表归一化位置 | `RR_std()` 归一化一次并打 `rr_normalized` 标记；`Mortality()` 据此跳过重复归一化；手工赋值的表仍走原路径 | 261027 |
| ⑬ | v6.2 修复口径 | 覆盖缺口与分解网格集不一致时告警或中止；`aggregate_sigma()` 以传入分支为准；`resolve_instance_files()` 不自动带 `Conc_cf`；仅 `kernel_plan()` 启用并发 | 261101 |

### 3.3 当前缺口（摘要）

- 完整健康负担、真实 O3/NO2 burden 与 0.1° 压测未完成。
- 现有 260617 实例的 2030 列混用 exposure 2030、population 2020、age 2019、mortality 2020。重建默认阻止混合年份，正式分解待用户批准代理或换真 2030 输入。
- NO2 0–14 岁、IER LRI 年龄、legacy 气体单位、超范围 clamp 口径待确认。
- `Code/README.md` 与目录级 `AGENTS.md` 未建。
- 旧健康负担结果失效；`MRBRT2023` 仅完成 `base2015` 评审测试。研究意图未确认；对比图未入库。

## 4. 基本规则（全局）

- **环境**：Windows；R 4.6.2（R 4.6 或更新），`C:\Program Files\R\R-4.6.2\bin\Rscript.exe`；从项目根 `source()` 运行。命令中的 R 路径按本机安装调整。
- **原始数据只读**：任何脚本不得写 `Data/*_raw/` 或原始 zip。
- **目录**：脚本写 `Code/`；实例表与查找表写 `Data/`；结果写 `Results/`；图件写 `Figures/`；`tests/`、`handoff/`、`Code/build/` 本地保留。临时件用完即删；退役件移入 `deprecated/<目录>/` 归档。
- **文档分层**：本文件只写定位、必守、指向（≤9 KB）；文件清单归各目录 `README.md`；细节归专门文件或代码注释；沿革归 `handoff/`。
- **变更必同步**：计算/数据、口径、结论、规划、目标变更后，全项目非归档件同轮同步；未同步项写进当次 handoff。规程用 skill `downstream-sync`。
- **HDD（强制）**：代码改动先提可证伪假设与验证方式，再动手；改完以验证结果收口。机械改动可简化，但须声明“预期不变/预期值”并核对。
- **需求确认（先听后问）**：目标或口径有歧义时先开放问，再回放，最后确认；代理决定必须说出口并登记 3.2。
- **写作**：直白、朴素、准确；先结论后证据；不编造未验证数值。
- **代码注释**：按 `yifan-r-code-style` §3（文件头写说明与版本记录；函数与块顶写输入、步骤、输出和口径）。本项目不是 R 包，不使用 roxygen 标签。
- **交接**：开工建 `handoff/<yyMMdd>_<主题>.md`，收口定稿；`handoff/` 保留本地，不入 git。

## 5. 主要假设与当前约束

1. PWRR 归一化：按域×终点×年龄档计算；域内零 PM2.5 基线一致；归因死亡与 `RR−1` 成正比。
2. MR-BRT 曲线只提供全年龄 RR，按 `_ALL` 填到各年龄档；这不是年龄特异曲线。
3. GBD 2023 与 2021：IHD/Stroke/LRI 更新；COPD/DM2/LC 在 300 µg/m³ 处差 ≤1.1%。
4. IHME 免费非商业许可；原始 zip 不入库，重下需 IHME 账号。
5. 查找表范围随模型不同：IER 0–200、GEMM/MRBRT2021/2023 0–300、MRBRT2019 0–600；`Mortality()` 当前将超范围浓度 clamp 到边界，目标口径待用户确认。
6. GBD 2023 O3/NO2 用 ppb；O3 0–50、NO2 0–100；NO2 LOW 截到 1。legacy 单位未核实。

## 6. 当前进展

GBD 2023 MR-BRT 已接入；v5.2–v6.3 沿革见 `CHANGELOG.md`。v6.3 串行 24 序列 6.4 秒；最新交接：`handoff/261101_perf-round2.md`。

## 7. 联系与来源

- GHDx 记录：https://ghdx.healthdata.org/record/ihme-data/gbd-2023-air-pollution-exposure-estimates-1990-2023
- DOI：https://doi.org/10.6069/K4TW-Q814
- 许可：IHME Free-of-Charge Non-Commercial User Agreement。
