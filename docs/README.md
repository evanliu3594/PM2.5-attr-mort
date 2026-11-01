# docs/ — 程序设计文档

本目录存放使用 superpowers skills 产生的程序设计文档，随 git 入库。
它与 `handoff/`（本地会话交接，不入 git）分工不同：`docs/` 放对实现者有约束力的设计与计划，`handoff/` 放会话过程与验收记录。

## 内容

| 路径 | 内容 | 产生方式 |
|---|---|---|
| `data-flow.md` | 计算管线数据流：输入、`Mortality()`、聚合、分解与连接规则 | 方法文档 |
| `superpowers/plans/` | 实现计划，文件名 `YYYY-MM-DD-<主题>.md` | superpowers 的 writing-plans 技能 |

## 约定

- 计划在实现前提交评审；实现完成后在计划内更新状态，并在 `handoff/` 留验收记录。
- 计划中的接口、任务顺序与验收口径是实现者的依据；偏离计划要写进当次交接。
- 计划的保存路径由项目约定固定为 `docs/superpowers/plans/`，不使用技能默认路径。
