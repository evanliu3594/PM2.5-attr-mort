# Handoff 261026 — 删除 CLAUDE.md 并更新 AGENTS.md 指针

> 交接文档：一份 = 一个工作会话的收口快照。只引用，不复制。定稿后只增不改。

## 0. 元信息

- 收口时间：2026-10-26 02:02
- 上一份交接：`handoff/261026_gbd2023-mrbrt.md`
- 下一会话焦点：未指定。若需恢复详细架构正文，从 commit `405fd74` 取 `CLAUDE.md`；若继续完善文档，补 `Code/README.md`、`Result/README.md` 与目录级 `AGENTS.md`。

## 1. 改动清单

- **删除 `CLAUDE.md`**：按用户 261026 指令执行 `git rm CLAUDE.md`；动机：用户要求移除该文件；影响面：根目录不再有该文件，内容保留在 git 历史 commit `405fd74`。
- **更新 `AGENTS.md` 指针**：4 处 `CLAUDE.md` 引用改为 `README.md` 与 `Code/*.R` 内注释；「当前缺口」新增一条，说明详细架构正文仅存于 git 历史。
- **新增本交接**：`handoff/261026_claude-md-removal.md`。

## 2. 验证结果

- `Test-Path CLAUDE.md` 返回 `False`；`git status` 显示 `D CLAUDE.md`，删除已入索引。
- `AGENTS.md` 内已无 `CLAUDE.md` 引用；仅本交接与 git 历史保留该文件名。
- 本次为文档删除，未改代码、数据、RR 查找表或配置；计算逻辑预期不变，无需重跑健康负担。
- 未验证：无。恢复路径已记录在下方指针。

## 3. 口径变化

- 无研究口径变化。
- 文档权威变化：`CLAUDE.md` 不再是现行文档；现行入口为 `AGENTS.md`、`README.md`、各目录 `README.md` 与代码注释。
- 根 `AGENTS.md` 的「当前缺口」已同步；「已拍板口径」与「代理决策」未改。

## 4. 遗留问题与下一步

- 详细架构正文（PWRR 推导、设计模式、聚合设计、版本沿革）现在只在 git 历史 `405fd74` 中；工作区没有对应正文。若后续需要，恢复 `git show 405fd74:CLAUDE.md`，或迁移到中性的 `Code/ARCHITECTURE.md`。
- `Code/README.md`、`Result/README.md` 与目录级 `AGENTS.md` 仍未建；本次未扩大范围。
- 研究意图基线、成功标准与服务对象仍待确认。

## 5. 建议技能（下一会话调用）

- `project-conventions`：若要把详细架构正文迁到中性规范文件，或补目录级 README/AGENTS。
- `result-verification`：若继续做 `MRBRT2023` 与 `MRBRT2021` 的健康负担对比。

## 6. 指针

- 现行总纲：`AGENTS.md`
- 上一份交接：`handoff/261026_gbd2023-mrbrt.md`
- 被删除文件的内容：`git show 405fd74:CLAUDE.md`
- 删除提交：`dba7c99`（删除与交接）；指针更新为紧随其后的独立提交。
- 注入预算：`AGENTS.md` 约 6.8 KB，低于 9 KB 上限；与用户级 7,368 B 合计约 14.2 KB，低于 60% 目标线。
