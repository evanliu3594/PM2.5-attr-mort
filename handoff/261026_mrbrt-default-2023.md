# Handoff 261026 — MRBRT 默认指向 MRBRT2023

> 交接文档：一份 = 一个工作会话的收口快照。只引用，不复制。定稿后只增不改。

## 0. 元信息

- 收口时间：2026-10-26 02:30
- 上一份交接：`handoff/261026_mortality-grouped.md`
- 下一会话焦点：用 `set_Model("MRBRT")`（现为 GBD 2023）重跑完整健康负担；确认输出标签与旧结果区分。

## 1. 改动清单

- **`MRBRT` 默认改为 2023**：`Data/RR_std_config.json` 的 `"MRBRT"` 条目 label 改为 `PM2.5_MRBRT2023`，lookup 改为 `MRBRT2023_Lookup_Table_Build_261026.xlsx`。
- **保留历史模型**：`"MRBRT2021"` 条目不变；`"MRBRT2023"` 显式条目不变。
- **文档同步**：`README.md` 加 v5.5 说明，更新 `set_Model('MRBRT')` 注释；`AGENTS.md` 加「已拍板口径」⑥，更新「代理决策」①；`HealthBurdenCalc.R` 注释更新。
- **新增本交接**：`handoff/261026_mrbrt-default-2023.md`。

## 2. 验证结果

- `set_Model("MRBRT")` 输出：`PM2.5_MRBRT2023`；`.RR_std_tbl` 855,285 × 5。
- `MRBRT` 与 `MRBRT2023` 的 `.RR_std_tbl` 逐值相同：`identical = TRUE`。
- `set_Model("MRBRT2021")` 仍输出 `PM2.5_MRBRT2021`；`.RR_std_tbl` 855,285 × 5。
- 200 个中国格真实子集，`set_Model("MRBRT")` + `Mortality_at(CI = "MEAN")`：200 × 97，NA = 0。
- 未验证：完整健康负担重跑；旧 `MRBRT` 结果与新默认结果的差异表；全 24 序列 DrivingFactors（不直接使用 MRBRT）。

## 3. 口径变化

- 有。`set_Model("MRBRT")` 从 GBD 2021 改为 GBD 2023；输出标签从 `PM2.5_MRBRT2021` 变为 `PM2.5_MRBRT2023`。
- `MRBRT2021` 保留，历史结果可复现。
- 根 `AGENTS.md` 的「已拍板口径」⑥ 与「代理决策」① 已同步。
- 旧 `MRBRT` 结果文件标签为 2021；新运行的标签为 2023，不能混用。

## 4. 遗留问题与下一步

- 用新默认 `MRBRT` 重跑完整健康负担，并与 `MRBRT2021` 对比总量、分终点、分年龄与 CI。
- 检查下游脚本/文档中是否还有“`MRBRT` = 2021”的旧表述；本仓库根 `README.md` 的 v5.1 历史条目已标注为旧行为。
- `HealthBurdenCalc.R` 当前默认仍是 `NCD+LRI`；需要 MR-BRT 时显式 `set_Model('MRBRT')`。
- 完整 24 序列 DrivingFactors 仍未跑。

## 5. 建议技能（下一会话调用）

- `result-verification`：新旧默认模型的总量与分项对比。
- `downstream-sync`：旧标签、旧结果、旧稿件表述的清扫。

## 6. 指针

- 配置：`Data/RR_std_config.json`
- 查找表：`Data/RR_index/MRBRT2023_Lookup_Table_Build_261026.xlsx`
- 总纲：`AGENTS.md`
- 说明：`README.md`（v5.5）
- 上一份交接：`handoff/261026_mortality-grouped.md`
- 提交：本次收口提交（`git log -1`）
- 注入预算：`AGENTS.md` 8,166 B；与用户级 7,368 B 合计 15,534 B（23.7%），低于 60% 目标线。
