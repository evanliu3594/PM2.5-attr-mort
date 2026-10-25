# Handoff 261026 — PWRR 分组缺陷修复（domain × endpoint × agegroup）

> 交接文档：一份 = 一个工作会话的收口快照。只引用，不复制。定稿后只增不改。

## 0. 元信息

- 收口时间：2026-10-26 02:10
- 上一份交接：`handoff/261026_claude-md-removal.md`
- 下一会话焦点：用 v5.2 重跑 `HealthBurdenCalc.R` 与 `DrivingFactors.R`，更新结果表、图件与稿件；再审计 `Uncertainty()` 与 `aggregate_sigma` 的 PAF 口径。

## 1. 改动清单

- **修复 PWRR 分组**：`Code/mortality.R` 的 PWRR 从 `group_by(domain)` 改为 `group_by(domain, endpoint, agegroup)`；动机：旧代码把一个域内所有终点/年龄的 RR 混成一个标量，再套到每个终点；影响面：`Mortality()` / `Mortality_at()` 全部结果。
- **修复 RANGE 分支 PWRR**：`CI = "RANGE"` 时 MEAN/UP/LOW 各用自己的 PWRR，不再统一用 MEAN 分母；影响面：`Mortality(CI="RANGE")` 的 CI 宽度。
- **PWRR 守卫改写**：NA PWRR 不再整域删除，只删受影响的 domain/endpoint/agegroup 行；报错信息带组合键；影响面：远端无数据域的保留行为。
- **文档同步**：`README.md` 更新 PWRR 公式与 v5.2 说明；`AGENTS.md` 更新「已拍板口径」「代理决策」「主要假设」「当前缺口」；`HealthBurdenCalc.R` 与 `DrivingFactors.R` 加版本/失效提示。
- **新增本交接**：`handoff/261026_pwrr-fix.md`。
- **临时验证材料**：`Result/` 下验证脚本与 old/new RDS 已删除；数值见第 2 节。

## 2. 验证结果

- **缺陷确认**：旧代码 `group_by(domain) |> summarise(PWRR = weighted.mean(RR, Pop))`，PWRR 只有一个域级标量；`mort_data` 只按 domain 连接，endpoint/agegroup 不参与分母。实测中国：旧 PWRR = 1.76；正确 PWRR 在 1.39（DM2）到 2.41（COPD）之间，比值 0.789–1.37。
- **覆盖**：175 个 domain、16,625 个 domain×endpoint×agegroup 组合；比值全样本 0.755–1.404，中位 1.003。
- **旧 vs 新（样本全球，MRBRT2021，`base2015`）**：
  - 总量 MEAN：7,085,753 → 6,715,235（−5.23%）。
  - 总量 UP：9,993,399 → 8,204,761（−17.9%）。
  - 总量 LOW：4,600,763 → 4,892,366（+6.34%）。
  - MEAN 分终点：COPD −20.9%，DM2 +6.88%，IHD −4.95%，LC +9.56%，LRI +1.98%，Stroke +1.29%。
- **新输出**：`Mortality(CI="RANGE")` 53,713 行 × 287 列，NA = 0；CI 排序 `LOW ≤ MEAN ≤ UP` 全部为真；`Mortality(CI="MEAN")` 53,713 行 × 97 列，NA = 0，总量与 RANGE 的 `_MEAN` 分支一致（6,715,235）。
- **未验证**：`DrivingFactors.R` 未重跑；`aggregate_range()` / `aggregate_sigma()` 未做端到端重跑；`Uncertainty()` 仍用 PAF 口径，未重新推导；结果表、图件、稿件未更新。

## 3. 口径变化

- 有。PWRR 从「域级标量」改为「域 × 终点 × 年龄档」；`CI="RANGE"` 三分支各用对应 PWRR。根 `AGENTS.md` 的「已拍板口径」③ 与「代理决策」⑧ 已同步。
- 旧健康负担结果（2026-10-26 之前）基于单域 PWRR，已失效；所有下游结果必须用 v5.2 重跑。
- 旧 CI 宽度不成立：旧 UP 偏高、LOW 偏低，CI 偏宽。

## 4. 遗留问题与下一步

- 用 v5.2 重跑 `HealthBurdenCalc.R` 与 `DrivingFactors.R`；更新 `Result/` 结果表、图件与论文数字。
- 审计 `Uncertainty()` / `aggregate_sigma()`：它们用 `PAF = 1 − 1/RR`，未使用 PWRR；需确认与 PWRR 主公式是否一致。
- `Code/README.md`、`Result/README.md`、目录级 `AGENTS.md` 仍未建。
- `Result/MRBRT2023_vs_2021.png` 仍为未入库的对比图。
- 研究意图基线、成功标准与服务对象仍待确认。

## 5. 建议技能（下一会话调用）

- `result-verification`：重跑后核对总量、分终点、分年龄锚点。
- `data-visualization`：更新健康负担图与分解图。
- `manuscript-writing`：同步方法、结果与图注文字。
- `downstream-sync`：旧结果、旧图、旧稿件表述的清扫。

## 6. 指针

- 代码：`Code/mortality.R`
- 总纲：`AGENTS.md`
- 说明：`README.md`（v5.2）
- 上一份交接：`handoff/261026_claude-md-removal.md`
- 删除前的架构正文：`git show 405fd74:CLAUDE.md`（旧文，PWRR 描述已过时；以当前 `README.md` 与 `Code/mortality.R` 为准）
- 提交：`fba26bc`（主改动）；本次指针更新为紧随其后的独立提交。
- 注入预算：`AGENTS.md` 7,302 B；与用户级 7,368 B 合计 14,670 B（22.4%），低于 60% 目标线。
