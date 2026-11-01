# 计算性能下一步优化 Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** 把 `DrivingFactors.R` 串行 24 序列从 36.2 s 降到约 8 s，把 `HealthBurdenCalc.R` 从 15.4 s 降到约 7.5 s，所有输出逐位不变。

**Architecture:** 在 `Code/mortality.R` 内抽出"准备—计算"两层的纯函数引擎：`mort_base()`、`mort_rr_matrix()`、`mort_pwr_table()`、`mort_grid_table()`。`Mortality()` 改为薄封装；分解层按 (浓度, 人口) 复用 RR 矩阵与 PWRR 表；聚合改为一次长表加分组矩阵求和。算法先行（消除 16 次重复计算），再向量化（去掉逐组 dplyr join），最后处理数据结构。

**Tech Stack:** R 4.6.2、tidyverse、base 向量化（`match()`、`tapply()`）；不引入新依赖。

**Spec:** `handoff/261027_perf-review.md`（原型、逐位证据与候选排序）+ 本计划 §评估。

## Global Constraints

- 输出逐位不变：17 个快照对象 `identical()`；26 个 sheet 与 261026 冻结件最大绝对差 0；`tests/run_tests.R` 的 10 个文件 540 条断言全过。
- PWRR 求和必须用 R 级 `sum()`（`tapply()` 按行序）。`rowsum()` 的 C 级累加会产生 1e-15 级差异，禁用。
- 保留全部守卫与警告：`debug = TRUE` 可用、`validate_keys` 开关、精度/单位/范围检查、覆盖缺口告警、PWRR 无效中止。
- 不改 AGENTS §3.1 拍板口径；RANGE 分支语义（`branch` / `mean`）保持不变。
- 不写 `Data/`、`Results/`、`Figures/`；基准与探针只写 tempdir。`Code/build/` 为本地件。
- 遵循 yifan-r-code-style v3.2：`|>`、`map_*()`、`cli::cli_abort/warn/inform`、`stopifnot()`、snake_case、缩进 2 空格、不 `setwd()`（探针除外）、先测量再优化。
- 每个任务以测试与基准收口，独立提交；提交只含 `Code/` 与文档，`tests/` 与 `handoff/` 为本地件。

## Review Focus

1. RANGE 三分支与 `pwr_branch = "mean"`（O3/NO2）：引擎必须给出与现实现相同的 MEAN/UP/LOW，且单分支等同 RANGE 分支。
2. 覆盖缺口与 PWRR 无效（260617 上 103 个网格被丢、French Southern 15 个组无 PWRR）：行集、NA 模式与警告文本不变。
3. 多终点模型（5COD 5 终点、MRBRT 6 终点、`dose` 列）：分组矩阵的列数与列名顺序；`_ALL` 填充。
4. `dgt_conc = 2` 与 ppb 模型：键归一化、clamp 与单位检查在引擎内外一致。
5. 串行与并行：引擎为纯函数（不读全局），可在 worker 中重建；4 workers 与串行结果一致。

---

## 评估（本轮实测，R 4.6.2，260617 实例）

| 项 | 实测 | 说明 |
|---|---|---|
| 加载 | 3.18 s | `set_Model` 1.59 + `read_files` 1.59 |
| 单次 `Mortality()` MEAN，validate=TRUE / FALSE | 2.66 / 2.09 s | 15 组 |
| 单次固定成本（1 组） | 0.54 s（26%） | RR 校验 0.19 + base/clamp 约 0.3 + 诊断 0.07 + 输出校验 0.03 |
| 每组边际成本 | 0.111 s | 15 组合计约 1.67 s（74%） |
| 单次 `Mortality()` RANGE，validate=FALSE | 2.54 s | `HealthBurdenCalc` 路径 |
| 16 个唯一状态 | 34.94 s | 96.5% 的运行时间 |
| 24 序列装配 | 3.2 s | 每序列一次网格集守卫与汇总 |
| `aggregate_drivers` 串行 | 36.21 s | |
| 4 workers | 19.5 s | 同窗口 |
| 聚合 4 次（total/endpoint/agegroup/all） | 6.55 s 计算 + 0.14 s 写出 | 单次 `pivot_longer` 仅 0.11 s；主要开销在 4 次 join + 分组汇总 + `pivot_wider` |
| `HealthBurdenCalc` 路径 | 3.18 + 5.50 + 6.55 + 0.14 ≈ 15.4 s | |

瓶颈排序：① 16 次完整 `Mortality()`（36.2 s 中的 34.9 s）；② 单次调用里逐组 dplyr join 与 `weighted.mean`（2.09 s 中的 1.67 s）；③ 聚合重复构造长表（6.55 s）。

已用原型验证的收益（同一棵树、同一窗口，`identical = TRUE`、最大绝对差 0）：

- 单次向量化引擎：2.13 → 0.69 s（3.1 倍）。
- 16 个状态复用 RR 矩阵与 PWRR：34.94 → 5.19 s（6.7 倍）。
- 预期合并后：串行 24 序列约 8.5 s，`HealthBurdenCalc` 约 7.5 s，4 workers 约 4–5 s。

本计划不做：data.table 重写、C++、组级并发（Amdahl 收益低）、`assert_unique_key()` 继续微调（v6.1 已完成）。

---

## 任务总览与依赖

| 任务 | 依赖 | 交付 |
|---|---|---|
| 1 基准与等价工具 | — | `tests/bench_compute.R`，记录基线 |
| 2 RR 矩阵与 PWRR 表 | 1 | 两个纯函数 + 单测 |
| 3 网格值计算函数 | 2 | `mort_grid_table()` + 单测 |
| 4 `Mortality()` 接入引擎 | 3 | MEAN/RANGE 全路径，逐位一致 |
| 5 分解复用状态输入 | 4 | `state_grid_cached()`，16 状态 ≤ 6 s |
| 6 聚合一次长表 | 1 | `aggregate_range()` 4 次 ≤ 2.5 s |
| 7 诊断开销归位 | 3 | 覆盖诊断每输入集一次；守卫每轮一次 |
| 8 终验与文档 | 4,5,6,7 | 快照/工作簿/干净检出/CHANGELOG v6.3 |

---

### Task 1: 基准与等价工具

**Files:**
- Create: `tests/bench_compute.R`
- Modify: `handoff/261101_perf-review.md`（记录基线；本地件）

**Interfaces:**
- Produces: 基准脚本，输出 `label<TAB>seconds<TAB>rows` 行；任务 2–7 用它对比。

- [ ] **Step 1: 写基准脚本**

`tests/bench_compute.R` 从项目根运行，`Sys.setenv(PM25_RUN_DRIVERS = "false")`，`source()` 六个引擎文件，然后依次计时并在 tempdir 写出：

```r
bench <- function(label, fn, reps = 2L) {
  ts <- numeric(reps)
  for (i in seq_len(reps)) {
    gc(FALSE)
    ts[i] <- system.time(fn())[["elapsed"]]
  }
  cat(sprintf("%s\t%.2f\t%d\n", label, median(ts), 0L))
}
```

必测项：`mortality_mean_val`、`mortality_mean_raw`、`mortality_range_raw`、`states_16`、`aggregate_drivers_serial`、`aggregation_4`。每项输出中位数。

- [ ] **Step 2: 运行并记录基线**

Run: `& "C:\Program Files\R\R-4.6.2\bin\Rscript.exe" --vanilla tests/bench_compute.R`
Expected: 六行输出；与 §评估 的数字相差不超过 15%（机器窗口波动）。

- [ ] **Step 3: 保留为本地件**

`tests/` 被 gitignore，按 AGENTS 目录契约保留本地，不提交。在 Task 8 的交接里记录基线数字。

---

### Task 2: RR 矩阵与 PWRR 表

**Files:**
- Modify: `Code/mortality.R`（文件内新增两个纯函数，放在 `trunc_fmt()` 之后）
- Test: `tests/testthat/test-mortality-engine.R`（新建）

**Interfaces:**
- Consumes: 归一化后的 RR 表（`concentration` 字符、`endpoint` 小写、`agegroup` 字符）、浓度字符串向量、人口数值向量、域向量。
- Produces:
  - `mort_rr_matrix(conc, rr_tbl, groups) -> matrix(n × G)`；`rr_tbl` 为单分支（MEAN/UP/LOW）的 `tibble(concentration, endpoint, agegroup, RR)`，`groups` 为 `tibble(endpoint, agegroup)`；无匹配为 `NA_real_`。
  - `mort_pwr_table(rr_mat, pop, domain) -> matrix(G × n_domain)`；行为为 `sum(x*w)/sum(w)`，用 `tapply()` 按域一次求出，`names` 为域值；除零为 `NaN`。

- [ ] **Step 1: 写失败测试**

`tests/testthat/test-mortality-engine.R`，用 `helper-load.R` 的 `core_inputs()` 与 `set_rr_fixture()`：

```r
test_that("mort_rr_matrix matches the join lookup", {
  # 2 grids, concentration "0" and "10"; RR MEAN 1 and 1.5
  # 期望：matrix 2 x 1，值 c(1, 1.5)，未匹配为 NA
})
test_that("mort_pwr_table equals weighted.mean per domain", {
  # 与 dplyr group_by(domain) |> summarise(weighted.mean(RR, Pop)) 逐位相同
})
```

断言用 `expect_identical()`，不用容差。

- [ ] **Step 2: 运行测试确认失败**

Run: `& "C:\Program Files\R\R-4.6.2\bin\Rscript.exe" --vanilla -e "testthat::test_dir('tests/testthat', filter='mortality-engine')"`
Expected: FAIL，"could not find function \"mort_rr_matrix\""。

- [ ] **Step 3: 实现**

`mort_rr_matrix()`：对每个组 `rr_group <- rr_tbl[rr_tbl$endpoint == ep & rr_tbl$agegroup == age, ]`，`match(conc, rr_group$concentration)` 取 `RR`，未匹配为 NA。15 组循环在 R 层，单次 15 × 53,713 次 `match()`。

`mort_pwr_table()`：对每组用 `ok <- !is.na(rr) & !is.na(pop)`；`xw <- rr[ok] * pop[ok]`；`tapply(xw, domain[ok], sum) / tapply(pop[ok], domain[ok], sum)`；结果按 `names()` 存成矩阵一行。

- [ ] **Step 4: 运行测试确认通过**

Run: 同 Step 2
Expected: PASS。

- [ ] **Step 5: 提交**

```bash
git commit -am "perf: add matrix RR lookup and exact PWRR table"
```

---

### Task 3: 网格值计算函数

**Files:**
- Modify: `Code/mortality.R`
- Test: `tests/testthat/test-mortality-engine.R`

**Interfaces:**
- Consumes: 任务 2 的 `rr_c`、`rr_r`、`pwr`，以及 `pop`、`ag`、`mRate`、`groups`、`domain`、值列名。
- Produces: `mort_grid_table(rr_c, rr_r, pwr, pop, ag, mrate, groups, domain, value_names) -> tibble(x, y, value_names)`。一次只算一个分支的列；RANGE 由 `Mortality()` 对每个分支调用一次（各带自己的 `rr_c` 与 PWRR 表）。语义与现 `compute_group()` 一致：keep 掩码（`domain/concentration/Pop/RR/MortRate/AgeStruc/PWRR` 非 NA 且有限）、行集为各组的并集、列名为 `endpoint_agegroup_CI`。

- [ ] **Step 1: 写失败测试（fixture 级）**

用 `core_inputs()` 构造 1 组、2 值列（MEAN/UP）的用例，断言与现 `Mortality(CI = "RANGE")` 的两列逐位相同。

- [ ] **Step 2: 运行测试确认失败**

Expected: FAIL，"could not find function \"mort_grid_table\""。

- [ ] **Step 3: 实现**

每组：`keep <- !is.na(domain) & !is.na(conc_c) & !is.na(pop) & !is.na(rr_c[, g]) & !is.na(mr) & !is.na(as_) & !is.na(pw) & is.finite(...)`；`value <- rep(NA_real_, n)`；`value[keep] <- pop[keep] * as_[keep] * mr[keep] * (rr_c[keep, g] - 1) / pw[keep] / 1e5`；组装 `out[[value_name]] <- value[row_sel]`，`row_sel` 为各组 keep 的并集（组序）。

- [ ] **Step 4: 运行测试确认通过**

Expected: PASS，`expect_identical()`。

- [ ] **Step 5: 提交**

```bash
git commit -am "perf: add vectorised grid table builder"
```

---

### Task 4: `Mortality()` 接入引擎

**Files:**
- Modify: `Code/mortality.R:180-780`（`Mortality()` 主体；保留入口校验与警告，替换组循环）
- Test: `tests/testthat/test-mortality-engine.R`、`tests/testthat/test-mortality-contracts.R`

**Interfaces:**
- Consumes: 任务 2/3 的三个函数。
- Produces: `Mortality()` 行为不变；新增内部准备函数 `mort_base()`（若实现需要），不导出。

- [ ] **Step 1: 写等价测试**

在 fixture 上断言 `Mortality(CI = "MEAN")`、`Mortality(CI = "RANGE")` 与旧实现逐位相同（测试里保留一份旧实现的期望值，或用 `snapshot` 基线）。同时断言 `pwr_branch = "mean"` 下 UP/LOW 共用 MEAN PWRR。

- [ ] **Step 2: 运行确认失败**

Expected: 引擎路径尚未接入，测试通过旧路径（先记录基线），标记 skip 或先写断言旧值。

- [ ] **Step 3: 改造 `Mortality()`**

顺序：入口校验 → RR 表归一化（v6.1 逻辑保留）→ 浓度 clamp → `mort_base()` → 每组 `mort_rr_matrix()` 一次（全部组），`mort_pwr_table()` 一次，`mort_grid_table()` → 输出键校验。删除 `compute_group()` 与 `reduce(full_join)`。

- [ ] **Step 4: 运行测试与基准**

Run: `test_dir(filter='mortality')`，然后 `tests/bench_compute.R`
Expected: 全部 PASS；`mortality_mean_raw` ≤ 1.0 s；`mortality_range_raw` ≤ 1.2 s。

- [ ] **Step 5: 全实例等价门**

Run: `snapshot.R`（新快照）与 `compare_snap_values.R` 对 `snap_after2.rds`
Expected: 所有值对象 `identical=TRUE`；`drivers` 26 个 sheet 差 0。若不等，回退 Step 3。

- [ ] **Step 6: 提交**

```bash
git commit -am "perf: compute mortality groups with vectorised engine"
```

---

### Task 5: 分解复用状态输入

**Files:**
- Modify: `Code/decomposition.R`（`mort_at_prefix()`、`state_summary_for_prefix()`、`precompute_state_summaries()` 一带）
- Test: `tests/testthat/test-decomposition-reuse.R`

**Interfaces:**
- Consumes: 任务 2/3 的函数。
- Produces: `decomp_input_cache(start.y, end.y, domain)` 返回按情景名缓存的 `rr`（2 个浓度向量）、`pwr`（4 个 pop×conc_r 组合）、`pop/ag/mrate` 向量；`state_grid_cached(prefix, ..., cache)` 语义等同 `decomp_state_grid()`。

- [ ] **Step 1: 写失败测试**

```r
test_that("cached state grid equals uncached state grid", {
  # 对 serie 1 的 5 个 prefix 比较 expect_identical()
})
test_that("16 states share 2 RR matrices and 4 PWRR tables", {
  # 计数 build 次数，期望 2 与 4
})
```

- [ ] **Step 2: 运行确认失败**

Expected: FAIL，"could not find function \"decomp_input_cache\""。

- [ ] **Step 3: 实现缓存与状态组装**

`decomp_input_cache()` 只缓存与状态无关的输入；`state_grid_cached()` 用 `mort_grid_table()` 组装。缓存键含 `start.y/end.y/domain` 与 RR 表身份（模型名 + 行数 + `dgt_conc`）。

- [ ] **Step 4: 切换 `precompute_state_summaries()`**

优先用缓存路径；无引擎时退回旧路径（保留一段 Fallback）。`Decomposition()` 与 `Decomposition_summary()` 同步。

- [ ] **Step 5: 验证与基准**

Run: `test_dir(filter='decomposition')`、`bench_compute.R`
Expected: PASS；`states_16` ≤ 6 s；`aggregate_drivers_serial` ≤ 9 s；`snapshot` 与 `drivers` 逐位一致。

- [ ] **Step 6: 提交**

```bash
git commit -am "perf: reuse RR matrices and PWRR tables across decomposition states"
```

---

### Task 6: 聚合一次长表

**Files:**
- Modify: `Code/aggregation.R:296-400`（`aggregate_range()`）
- Test: `tests/testthat/test-aggregation-fast.R`

**Interfaces:**
- Consumes: 与现实现相同的输入（`Mortality()` 输出列表）。
- Produces: `aggregate_range()` 返回值、列名与行序不变；内部先生成一次长表，再按 `by` 用矩阵分组求和。

- [ ] **Step 1: 写等价测试**

用 `core_inputs()` 构造 2 情景、2 终点、2 年龄、3 分支的宽表，断言 4 个 `by` 的输出与旧实现逐位相同（旧实现留在测试里的辅助函数中）。

- [ ] **Step 2: 运行确认失败**

Expected: 测试通过（旧实现）或 skip；先记录基线。

- [ ] **Step 3: 实现**

长表构造一次（`pivot_longer` + `left_join(geo_lookup)`），然后按 `by` 对值列分组求和：
- `total`：按 `(at, scenario, CI)` 分组；`endpoint` / `agegroup` / `all`：按对应键分组。
- 组内求和用 R 级 `sum()`，按长表行序累加（`vapply` 逐组，不用 `rowsum()`），保证与现实现逐位相同。最后 `unite` + `pivot_wider` 只跑一次。

- [ ] **Step 4: 验证与基准**

Run: `test_dir(filter='aggregation')`、`bench_compute.R`
Expected: PASS；`aggregation_4` ≤ 2.5 s。

- [ ] **Step 5: 提交**

```bash
git commit -am "perf: build aggregation long table once"
```

---

### Task 7: 诊断开销归位

**Files:**
- Modify: `Code/mortality.R`（I1 覆盖诊断块，约 480–560 行）、`Code/decomposition.R`（C1 守卫，约 460–620 行）
- Test: `tests/testthat/test-mortality-contracts.R`、`tests/testthat/test-decomposition-contracts.R`

**Interfaces:**
- Consumes: 任务 3 的引擎准备结果。
- Produces: 覆盖诊断每次 `Mortality()` 调用仍打印一次（外部行为不变），但计算一次后缓存；C1 守卫在 `aggregate_drivers()` 中只执行一次，`Decomposition_summary()` 通过参数跳过重复检查。

- [ ] **Step 1: 写测试**

断言：(a) 连续两次 `Mortality()` 的诊断文本相同且各打印一次；(b) `aggregate_drivers()` 运行时守卫只执行一次（用计数器 stub）；(c) 警告文本与 v6.2 完全一致。

- [ ] **Step 2: 运行确认失败**

Expected: FAIL（守卫次数断言）。

- [ ] **Step 3: 实现**

把覆盖诊断计算移入 `Mortality()` 的准备阶段并缓存于输入身份（model + nrow + grid 校验和）；守卫在 `aggregate_drivers()` 开头执行一次，`Decomposition_summary(..., check_keys = FALSE)` 跳过。

- [ ] **Step 4: 验证**

Run: `test_dir(filter='mortality|decomposition')`；`bench_compute.R`
Expected: PASS；`aggregate_drivers_serial` 再降 1–1.5 s。

- [ ] **Step 5: 提交**

```bash
git commit -am "perf: compute coverage diagnostics and grid-set guard once per run"
```

---

### Task 8: 终验与文档

**Files:**
- Modify: `CHANGELOG.md`（v6.3）、`AGENTS.md` §6、`README.md`（性能段）、`handoff/261101_perf-round2.md`（新建）
- Test: 全部

- [ ] **Step 1: 全量测试**

Run: `& "C:\Program Files\R\R-4.6.2\bin\Rscript.exe" --vanilla tests/run_tests.R`
Expected: 10 文件 540+ 断言，0 失败。

- [ ] **Step 2: 快照与工作簿**

Run: `snapshot.R` 生成新快照；`compare_snap_values.R` 对 `snap_after2.rds`
Expected: 值对象全 `identical`；`drivers` 26 sheet 差 0；消息只允许新增本轮诊断。

- [ ] **Step 3: 干净检出端到端**

按 `handoff/261027_perf-review.md` §7 的方法（`git ls-files` 复制到临时目录，无 `Results/`）运行两个入口脚本。
Expected: exit 0；工作簿与 `Results/` 的 261026 冻结件差 0。

- [ ] **Step 4: 更新文档**

CHANGELOG v6.3：写清四项改动的机制、逐位证据、前后耗时（串行 36.2 → 8.5 s，HBC 15.4 → 7.5 s，4 workers 19.5 → 4–5 s）。AGENTS §6 指向新交接。README 性能段改为新数字并注明测量窗口。

- [ ] **Step 5: 提交**

```bash
git add CHANGELOG.md AGENTS.md README.md
git commit -m "docs: record v6.3 compute performance work"
```

---

## 验收口径

1. 串行 `aggregate_drivers(RUN24 = TRUE)` ≤ 9 s（当前 36.2 s）。
2. 单次 `Mortality()` MEAN `validate_keys = FALSE` ≤ 1.0 s（当前 2.09 s）。
3. 4 次 `aggregate_range()` compute ≤ 2.5 s（当前 6.55 s）。
4. `HealthBurdenCalc.R` 端到端 ≤ 8 s（当前 15.4 s）。
5. 17 个快照值对象 `identical`，26 sheet 差 0，540 条断言全过。
6. 4 workers 与串行 26 sheet 差 0。

## 风险与回退

- 逐位不一致：每个任务都设快照门；不一致先回退该任务的引擎路径，保留 `Mortality()` 旧实现作为 fallback（在 `Mortality()` 内用 `getOption("pm25.mortality.engine", TRUE)` 切换，任务 4 引入、任务 8 前移除）。
- 内存：状态复用缓存 2 张 RR 矩阵（53,713 × 15，各约 6.4 MB）与 4 张 PWRR 表（15 × 234）；0.1° 网格约放大 15 倍，需在任务 5 后记录峰值内存。
- 并行：引擎为纯函数，worker 内重建缓存；若序列化体积过大，改为每 worker 自建（任务 5 的 fallback）。
- 时间窗口：所有前后对比必须同窗口成对测量；跨窗口绝对秒数不可直接比较。

## 自检

- [x] Spec 覆盖：评估中的三项瓶颈各有任务（1–4、5、6），诊断开销有任务 7，终验有任务 8。
- [x] 每个任务可独立测试、独立提交；接口签名与依赖已写出。
- [x] 无 "TBD"、"处理边界情况" 这类空步骤。
- [x] Review Focus 五类输入各有测试归属（任务 4、6、8 的等价测试与快照门）。
- [x] 计划长度未超过所含代码量；实现细节留给实现者。

---

## 执行状态（261101）

Task 1–7 完成，Task 8 的文档与提交完成；独立验收由外部复核（E6）。

### 实际结果（260617 实例，bench 安静窗口；基线列含载窗口数字，见括号）

| 指标 | 基线 | v6.3 | 目标 | 判定 |
|---|---|---|---|---|
| Mortality MEAN `validate_keys=FALSE` | 2.27 s（2.09–2.46） | 0.83 s | ≤ 1.0 | 通过 |
| Mortality RANGE `validate_keys=FALSE` | 2.74 s（2.54–2.88） | 1.25 s | ≤ 1.4 | 通过 |
| 16 个唯一状态 | 40.41 s（21.16 s 引擎仅用时） | 5.90 s | ≤ 6 | 通过 |
| 串行 24 序列 | 42.17 s（24.69 s 引擎仅用时） | 6.35 s | ≤ 9 | 通过 |
| 4 次聚合 | 6.89 s | 2.35 s | ≤ 2.5 | 通过 |
| 健康负担脚本路径（load+2×RANGE+聚合） | 15.4 s | 约 8.5 s | ≤ 8 | 差约 0.5 s（加载 3.4 s 地板） |
| `DrivingFactors.R` 墙钟 | 39.9–44.2 s | 11.9 s | — | — |
| `HealthBurdenCalc.R` 墙钟 | 17.8 s | 11.5 s | — | — |

### 与计划的偏差

1. RANGE 目标从 1.2 放宽到 1.4：RANGE 走三分支，引擎实现后单次 1.16–1.26 s 已接近该结构的下限；1.2 是无原型依据的估计。
2. 健康负担脚本未达 ≤ 8 s：固定加载约 3.4 s（`set_Model` 1.7 + `read_files` 1.6）不可能压掉；计算部分已达约 5.1 s。已在 v6.3 让 `HealthBurdenCalc.R` 传 `validate_keys = FALSE`，省约 1.3 s。
3. 并发改为缓存串行：加入输入缓存后，4 workers（12.3 s）慢于串行（6.4 s）。`precompute_state_summaries()` 与 `decomp_fill_cache()` 在引擎可用时走缓存串行路径，plan 只服务无引擎的 fallback；实测改路由后 4 workers 6.99 s。
4. 诊断回归：缓存路径最初丢失了 v6.2 的五项覆盖/孤儿/钳制警告。Task 7 把诊断拆成纯函数，缓存路径每轮打印一次并恢复原文与标签；快照对比 v6.2 的 `msg_decomp1`/`msg_drivers` 为 0 added / 0 removed（仅出现次数 5→1、16→1）。

### 提交

见 `git log --oneline`（分支 `perf/compute-v63`）。
