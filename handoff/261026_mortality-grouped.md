# Handoff 261026 — Mortality 按 endpoint × agegroup 分组重构

> 交接文档：一份 = 一个工作会话的收口快照。只引用，不复制。定稿后只增不改。

## 0. 元信息

- 收口时间：2026-10-26 02:23
- 上一份交接：`handoff/261026_drivingfactors-24seq.md`
- 下一会话焦点：0.1° 全球格网内存/耗时压测；用 `future_map2()` 启用并发；重跑完整健康负担与 DrivingFactors。

## 1. 改动清单

- **Mortality() 改为分组遍历**：`Code/mortality.R` 不再一次性构造 `grids × endpoint × agegroup` 长表；按 `RR_all` 中的 `endpoint × agegroup` 逐组计算。
- **组内网格级连接**：每组只保留 `base_c`/`base_r`（每格一行），逐组 join RR、MortRate、AgeStructure、PWRR，计算 `Mort` 后立即收窄为 `x, y, value`。
- **PWRR 组内计算**：每组用 `base_r` 和该组 RR 计算 `weighted.mean(RR, Pop)`；RANGE 三分支各用自己的 PWRR。
- **future-ready**：`compute_group(ep, age)` 无副作用，返回 `list(data, skipped)`；主循环为 `map2(groups$endpoint, groups$agegroup, compute_group)`，可直接替换为 `future_map2()`。
- **调试保留**：组内继续使用 `join_report()`；`debug = TRUE` 可用，按组报告缺失键。
- **文档同步**：`README.md` 加 v5.4 说明；`AGENTS.md` 更新「已拍板口径」「当前缺口」「当前进展」。
- **新增本交接**：`handoff/261026_mortality-grouped.md`。

## 2. 验证结果

- **NCD+LRI 逐值一致**：重构前保存基线，重构后 MEAN 与 RANGE 均与基线逐值相同：
  - MEAN：53,713 × 17，NA = 0，max abs diff = 0。
  - RANGE：53,713 × 47，NA = 0，max abs diff = 0。
  - x/y 键集合一致，列名一致。
- **MRBRT2021 全样本**：RANGE 53,713 × 287，NA = 0；耗时 31.77 s；总量 `_MEAN` = 6,715,235、`_UP` = 8,204,761、`_LOW` = 4,892,366，与 PWRR 修复后基线一致。
- **debug 路径**：20 个中国格子集，`Mortality_at(CI = "MEAN", debug = TRUE)` 返回 20 × 17，NA = 0。
- **单分支一致**：200 个中国格，`CI = "UP"` 与 `CI = "RANGE"` 的 `_UP` 列逐值一致，max abs diff = 0。
- **警告**：95 个 domain/endpoint/age 组合无有效 PWRR 的提示与旧版一致（French Southern and Antarctic Lands）。
- **未验证**：0.1° 全球格网压测；`future_map2()` 并发；完整 `DrivingFactors.R` 24 序列；最终宽表在超大格网下的内存上限。

## 3. 口径变化

- 无研究口径变化。
- 计算结构变化：`Mortality()` 的中间长表从 `grids × endpoint × agegroup` 降为 `grids`（每组一次）；输出宽表结构与数值不变。
- 旧版“PWRR 行数下降超过 50%”的全局警告被移除；保留 orphan grid/pop 警告与按组的 NA PWRR 组合警告。

## 4. 遗留问题与下一步

- 用 `future_map2()` 替换 `map2()`，按 endpoint-agegroup 分组并发；注意每个 worker 会复制 `base_c`/`base_r`/`RR_all`，需实测内存与加速比。
- 在 0.1° 全球格网上做内存/耗时压测；最终宽表仍随 格数 × 组合数 增长，必要时考虑分块输出或 long 格式落盘。
- 重跑完整 `HealthBurdenCalc.R` 与 `DrivingFactors.R`，更新结果表、图件与稿件。
- `Result/` 下只保留未入库的 `MRBRT2023_vs_2021.png`；本次验证脚本与 RDS 已删除。
- `Code/README.md`、`Result/README.md`、目录级 `AGENTS.md` 仍未建。

## 5. 建议技能（下一会话调用）

- `code-implementation`：`future_map2()` 并发改造与压测。
- `result-verification`：重跑后的锚点核对。
- `downstream-sync`：旧结果、旧图、旧稿件表述的清扫。

## 6. 指针

- 代码：`Code/mortality.R`
- 总纲：`AGENTS.md`
- 说明：`README.md`（v5.4）
- 上一份交接：`handoff/261026_drivingfactors-24seq.md`
- 提交：`549332a`（主改动）；本次指针更新为紧随其后的独立提交。
- 注入预算：`AGENTS.md` 8,024 B；与用户级 7,368 B 合计 15,392 B（23.5%），低于 60% 目标线。
