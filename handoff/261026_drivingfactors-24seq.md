# Handoff 261026 — DrivingFactors 24 序列明文重写

> 交接文档：一份 = 一个工作会话的收口快照。只引用，不复制。定稿后只增不改。

## 0. 元信息

- 收口时间：2026-10-26 02:17
- 上一份交接：`handoff/261026_pwrr-fix.md`
- 下一会话焦点：用重写后的 `DrivingFactors.R` 跑完整 24 序列，更新分解结果表、图件与稿件。

## 1. 改动清单

- **24 序列改为明文表**：`DrivingFactors.R` 删除 `expand_grid()` + `slice()` 生成 24 排列的写法；新增 `decomp_sequences` tribble，按文档注释顺序写死 24 行。
- **因子状态改为显式映射**：新增 `mort_state(prefix, start.y, end.y)`，明确 `PG → Pop`、`PA → AgeStruc`、`EXP → Conc_c`、`ORF → Conc_r + MortRate`；删除 `Mort_1`/`Mort_2`/`Mort_3` 的 `if` 链。
- **domain 注入**：`Mortality()` 输出只有 `x,y` 与 endpoint-age 列，没有 `Country`。新增 `add_domain()`，在每个状态后按 `x,y` 注入检测到的 domain 列；`Decomposition()` 与 `aggregate_drivers()` 新增 `domain` 参数。
- **输出审计表**：`aggregate_drivers()` 在结果 xlsx 中增加 `Sequences` sheet，列出 24 序列。
- **文档同步**：`README.md` 加 v5.3 说明；`AGENTS.md` 更新「已拍板口径」「当前缺口」「当前进展」。
- **新增本交接**：`handoff/261026_drivingfactors-24seq.md`。

## 2. 验证结果

- **状态审计**：用会记录实参的假 `Mortality()` 跑完 24 序列 × 5 个状态；与显式规则逐项比对，**0 处错配**。
- **旧代码问题 1（序列顺序）**：旧 `expand_grid()` 生成的 `serie = 1` 实际是 `PA PG EXP ORF`，注释第 1 行是 `PG PA EXP ORF`。24 个排列集合相同，汇总均值不受影响；逐 sheet 按注释核对会看到多处“错位”。
- **旧代码问题 2（运行失败）**：旧 `Decomposition()` 的 `pivot_longer(cols = -c(x, y, Country))` 需要 `Country`，`Mortality()` 不返回该列，脚本会在该处报错。新代码已改为 domain 注入。
- **真实子集**：200 个中国格、`NCD+LRI`、`serie = 1`，`Decomposition()` 返回 3,000 行 × 10 列，NA = 0；列为 `x,y,Country,Cause_Age,Start,PG,PA,EXP,ORF,End`。
- **小规模端到端**：在同一 200 格子集上调用 `aggregate_drivers(RUN24 = FALSE)`，成功写出 `1`、`Summary`、`Sequences` 三个 sheet；临时输出文件已删除。
- **未验证**：完整 24 序列 `aggregate_drivers()` 未跑（120 次 `Mortality()`）；24 序列汇总数值未与旧结果逐项对比；`Conc_cf` 路径缺失时 `read_files()` 会警告，`Decomposition()` 不使用 `Conc_cf`。

## 3. 口径变化

- 无研究口径变化。
- 分解结构变化：24 序列顺序与注释一致；序列 sheet 编号现在可逐行对应；domain 列可配置，不再硬编码 `Country`。
- 旧 per-serie 输出不可按注释直接核对；旧汇总均值在旧代码能运行的版本上应与新表一致（24 个排列集合相同）。

## 4. 遗留问题与下一步

- 跑完整 24 序列，更新分解结果表、图件与稿件。
- 确认旧代码是否在任何版本上实际跑过；若跑过，用同一数据对比 24 序列汇总均值，确认“集合相同、均值不变”。
- `read_files()` 的 `Conc_cf` 路径指向不存在的 `GridPM25_cf_instance_260325.xlsx`；`Decomposition()` 不使用它，可删除或修复路径。
- `Code/README.md`、`Result/README.md`、目录级 `AGENTS.md` 仍未建。
- `Result/MRBRT2023_vs_2021.png` 仍为未入库的对比图。

## 5. 建议技能（下一会话调用）

- `result-verification`：完整 24 序列重跑后的锚点核对。
- `downstream-sync`：旧分解结果、旧图、旧稿件表述的清扫。
- `manuscript-writing`：同步方法、结果与图注文字。

## 6. 指针

- 代码：`DrivingFactors.R`
- 总纲：`AGENTS.md`
- 说明：`README.md`（v5.3）
- 上一份交接：`handoff/261026_pwrr-fix.md`
- 提交：本次收口提交（`git log -1`）
- 注入预算：`AGENTS.md` 7,664 B；与用户级 7,368 B 合计 15,032 B（22.9%），低于 60% 目标线。
