# AGENTS.md — PM2.5-attr-mort（项目总纲）

> 本文件是项目总纲（路标）。详细架构与函数说明见 `README.md` 与 `Code/*.R` 内注释；文件清单见各目录 `README.md`；沿革见 `handoff/`。
> 目录契约：本项目沿用 `Code/`、`Data/`、`Result/`，不迁移到 A 型默认的 `R/`、`data/`、`output/`（用户 261026 拍板）。

## 0. 目录结构

| 目录 / 文件 | 职责 | 明细位置 |
|---|---|---|
| `Code/` | 计算引擎、CR 构建脚本 | `Code/README.md`（待补）；函数说明见 `README.md` 与代码注释 |
| `Data/` | 原始数据、实例表、CRF 系数、RR 查找表 | `Data/README.md`、`Data/RR_index/README.md` |
| `Result/` | 结果表与图件 | `Result/README.md`（待补） |
| `handoff/` | 会话交接记录；最新一份 = 当前状态入口 | — |
| 根脚本 | `build_instance.R`、`HealthBurdenCalc.R`、`DrivingFactors.R` | `README.md` |

`Data/` 内部分层：原始只读（`PM25_raw/`、`IHME_Pop_raw/`、`IHME_MortRate_raw/`）；实例表（`*_instance_260617.xlsx`）；`CRF_coefficients/`；`RR_index/`。原始数据与原始 zip 只读。

## 1. 研究问题与目标

**意图基线（待用户确认）**：用网格化 PM2.5 浓度、人口、GBD 死亡率与年龄结构，估计 PM2.5 归因死亡；支持 IER、GEMM、MR-BRT、O3、NO2 多种 C-R 模型；可按格、省、区域、国家聚合。示例数据覆盖中国，框架可换区域。成功标准、服务对象与边界尚未由用户在本轮确认。

**问题**：不同 C-R 模型与浓度情景下，PM2.5 归因死亡是多少，如何分解？

**目标**：① 生成格级归因死亡与置信区间；② 按空间、终点、年龄聚合；③ 分解人口、老龄化、暴露与其他风险因素的贡献。

## 2. 基础原始数据（只读）

- `Data/PM25_raw/`：PM2.5 浓度原始数据。
- `Data/IHME_Pop_raw/`：IHME 人口。
- `Data/IHME_MortRate_raw/`：IHME GBD 死亡率与年龄结构。
- `Data/CRF_coefficients/`：IER/GEMM 参数 CSV。
- `Data/RR_index/`：预构建 RR 查找表。
- `Data/IHME_GBD_2023_AIR_POLLUTION_1990_2021_RR_CURVES.zip`：GBD 2023 空气污染 RR 原始包，185 MB，不入 git，重下需 IHME 登录。
- 版本、年份、来源明细见 `Data/README.md` 与 `Data/RR_index/README.md`。

## 3. 研究方案与计算逻辑（概要）

管线：`build_instance.R` 生成实例表 → `Code/build_CR.R` 或 `Code/build_CR_MRBRT2023.R` 生成 RR 查找表 → `set_Model()` / `RR_std()` 建 `.RR_std_tbl` → `read_files()` 读数据 → `Mortality()` / `Mortality_at()` 算 PWRR 与归因死亡 → `aggregate_range()` / `aggregate_sigma()` 聚合 → `DrivingFactors.R` 做 24 排列分解。公式、函数契约与数据流见 `README.md` 与 `Code/*.R` 内注释。

### 3.1 已拍板口径（用户确认，勿再改）

| # | 口径 | 决定 | 日期 |
|---|---|---|---|
| ① | 目录命名 | 保留 `Code/Data/Result`，不迁移 A 型默认契约 | 261026 |
| ② | GBD 2023 RR 接入 | 下载 GHDx 曲线并编译为项目可用 CR 表 | 261026 |
| ③ | PWRR 归一化 | 按域 × 终点 × 年龄档计算；CI 分支用对应 PWRR | 261026 |
| ④ | DrivingFactors 24 序列 | 按注释顺序明文表；state 规则显式；domain 注入 | 261026 |
| ⑤ | Mortality 计算结构 | 按 endpoint × agegroup 分组遍历；每组 grid 级长表；future-ready | 261026 |
| ⑥ | MRBRT 默认指向 | `set_Model("MRBRT")` = `MRBRT2023`；`MRBRT2021` 保留 | 261026 |

### 3.2 代理决策（agent 决定，用户未反对）

| # | 选择项 | 取值 | 理由 | 日期 |
|---|---|---|---|---|
| ① | 新模型命名 | `MRBRT2023` 为显式 2023 别名；`MRBRT2021` 为历史别名 | 默认指向见「已拍板口径」⑥ | 261026 |
| ② | MRBRT2023 终点 | copd、dm2、ihd、lc、lri、stroke | 与 MRBRT2021 终点集一致；死亡率表无 dementia | 261026 |
| ③ | MRBRT2023 网格 | 0–300 µg/m³，步长 0.1；全年龄 `_ALL` 填充 | 与现有查找表 join 网格一致；源曲线只给全年龄 | 261026 |
| ④ | 源文件尺度 | COPD/DM2/LC 按 RR；IHD/Stroke/LRI 按 log(RR) | 压缩包内两批文件尺度不同 | 261026 |
| ⑤ | 插值 | log(RR) 线性插值到 0.1 网格 | 保持正值、平滑 | 261026 |
| ⑥ | 输出文件名 | `MRBRT2023_Lookup_Table_Build_261026.xlsx` | 固定构建日期，配置稳定 | 261026 |
| ⑦ | 原始 zip | 放 `Data/`，加入 `.gitignore` | 185 MB，不入库 | 261026 |
| ⑧ | RANGE 分支 PWRR | 三分支各用对应 PWRR | 与分子 RR 分支一致 | 261026 |

### 3.3 当前缺口（摘要）

- 未用 `MRBRT2023` 重跑完整健康负担；影响幅度未量化。
- `DrivingFactors.R` 已重写为 24 序列明文表；完整 24 序列（120 次 `Mortality()`）未跑，仅做状态审计与 200 格真实子集测试。
- `Mortality()` 已改为 endpoint × agegroup 分组遍历；未在 0.1° 全球格网压测，`future` 并发未启用；最终宽表仍随 格数 × 组合数 增长。
- 旧健康负担结果基于单域 PWRR，已失效；需用 v5.2 重跑 `HealthBurdenCalc.R` 与 `DrivingFactors.R`，并更新结果表、图件与稿件。
- dementia、birth weight、gestational age 曲线未编译。
- `Code/README.md`、`Result/README.md` 与目录级 `AGENTS.md` 未建；本次为最小收口。
- `CLAUDE.md` 已于 261026 删除；详细架构正文仅存于 git 历史（commit `405fd74`）。
- 研究意图基线、成功标准与服务对象未经用户确认。
- `Result/` 下有未入库的 MRBRT2023 对比图（`MRBRT2023_vs_2021.png`）。

## 4. 基本规则（全局）

- **环境**：Windows；R 4.6.1，`C:\Program Files\R\R-4.6.1\bin\Rscript.exe`；从项目根 `source()` 运行。
- **原始数据只读**：任何脚本不得写 `Data/*_raw/` 或原始 zip。
- **目录**：脚本写 `Code/`；实例表与查找表写 `Data/`；结果与图写 `Result/`。临时件用完即删；退役件移入根 `deprecated/<原目录>/`，只归档不删除。
- **文档分层**：本文件只写定位、必守、指向（≤9 KB）；文件清单归各目录 `README.md`；细节归专门规范文件或代码注释；沿革归 `handoff/`。
- **变更必同步**：计算/数据、口径、结论、规划、目标变更后，全项目非归档件同轮同步；未同步项写进当次 handoff。规程用 skill `downstream-sync`。
- **HDD（强制）**：代码改动先提可证伪假设与验证方式，再动手；改完以验证结果收口。机械改动可简化，但须声明“预期不变/预期值”并核对。
- **需求确认（先听后问）**：目标或口径有歧义时先开放问，再回放，最后确认；代理决定必须说出口并登记 3.2。
- **写作**：直白、朴素、准确；先结论后证据；不编造未验证数值。
- **交接**：开工建 `handoff/<yyMMdd>_<主题>.md`，收口定稿；有 git 时必须提交入库。

## 5. 主要假设与当前约束

1. PWRR 归一化：按域 × 终点 × 年龄档计算；域内零 PM2.5 基线一致；归因死亡与 `RR−1` 成正比。
2. MR-BRT 曲线只提供全年龄 RR，按 `_ALL` 填到各年龄档；这不是年龄特异曲线。
3. GBD 2023 与 2021：IHD、Stroke、LRI 更新；COPD、DM2、LC 与 2021 接近，300 µg/m³ 处差异 ≤1.1%。
4. IHME 免费非商业许可；原始 zip 不入库，重下需 IHME 账号。
5. 查找表覆盖 0–300 µg/m³；超过 300 的格无法匹配（与现有 MRBRT2021 一致）。

## 6. 当前进展

GBD 2023 MR-BRT RR 查找表已接入并验证；PWRR 已修正为按域 × 终点 × 年龄档计算（v5.2）；`DrivingFactors.R` 的 24 序列已改为明文表并完成状态审计（v5.3）；`Mortality()` 已改为 endpoint × agegroup 分组遍历（v5.4）；`MRBRT` 默认指向 `MRBRT2023`（v5.5）。完整健康负担未重跑。最新交接：`handoff/261026_mrbrt-default-2023.md`。

## 7. 联系与来源

- GHDx 记录：https://ghdx.healthdata.org/record/ihme-data/gbd-2023-air-pollution-exposure-estimates-1990-2023
- DOI：https://doi.org/10.6069/K4TW-Q814
- 许可：IHME Free-of-Charge Non-Commercial User Agreement。
