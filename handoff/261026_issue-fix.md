# 261026 评审问题核验与修复

## 0. 结论

上轮 Kimi 评审报告中的 11 项代码问题全部进行了独立复核。部分问题附触发条件，但未发现不存在的项。

可明确判定的代码缺陷已修复。研究口径类问题保持阻止或待确认状态。完整 24 序列、真实 O3/NO2 负担和 0.1° 压测未执行。

回归测试：26 个测试、94 个断言，0 失败、0 错误、0 警告。

## 1. 问题核验与处置

| 编号 | 核验结果 | 处置 |
|---|---|---|
| D1 缺失终点聚合为零 | 确认存在 | `aggregate_range()` 不再使用 `na.rm=TRUE`，缺失保持 NA |
| D2 数值辅助列进入总量 | 条件存在：Grid_info 有数值列时触发 | 只透视和求和死亡率列 |
| D3 重复网格键复制死亡 | 确认存在 | `read_files()` 与 `Mortality()` 检查键唯一性 |
| D4 无关 NA 删除网格 | 确认存在 | 只按本组必需列过滤 |
| D5 mean 单分支 PWRR 错误 | 条件存在：mean 模式直接算 UP/LOW 时触发 | 单分支分母改为同一 MEAN PWRR |
| D6 Uncertainty 与 PWRR 不一致 | 确认存在 | 退场旧实现，`aggregate_sigma()` 改用 PWRR 分支 |
| D7 build_CR.R 解析失败 | 确认存在 | 修 4 处管道，并修当前 dplyr 的运行时错误 |
| D8 dgt_conc 未贯通 | 条件存在：精度不是 1 时触发 | 精度写入 RR 表属性并贯通计算 |
| D9 PWE 越界与下划线域 | 确认存在 | 旧 PWE 路径退场；新路径按列连接，不用拼字符串 |
| D10 显式缺失 Conc_cf 回退 | 条件存在：显式传入不存在路径时触发 | 显式缺失报错，省略参数才允许 NULL |
| D11 坐标大小写 | 确认存在 | 改为真正的大小写无关匹配 |

补充确认的构建和入口问题：

- `HealthBurdenCalc.R` 整段运行不落盘：已改为默认写四类国家聚合。
- 情景列依赖前两列位置：已改用 `get_scenarios()`。
- 构建输出与入口固定 260617 脱钩：已增加 `Data/instance_manifest.json`。
- 构建目录缺失与 llmjoin 凭据依赖：已改为环境变量、启动前检查和映射缓存。
- 无种子随机 Conc_cf：已删除自动生成逻辑。
- 2030 列混合源年份：现有实例仍未重算，重建默认阻止混合年份。

## 2. 代码改动

| 文件 | 改动 |
|---|---|
| `Code/aggregation.R` | 重写按列聚合；保留 NA；`aggregate_sigma()` 使用 PWRR；`aggregate_range()` 不再把数值元数据计入死亡 |
| `Code/data.R` | 键唯一性、单位、精度、坐标大小写、显式 Conc_cf、manifest 解析 |
| `Code/mortality.R` | PWRR mean 单分支修复；必需列过滤；精度和单位校验；键唯一性；输出唯一性 |
| `Code/model.R` | RR 精度与单位/范围元数据；legacy 表非有限值和 CI 交叉改为警示 |
| `Code/uncertainty.R` | 旧 PAF/PWE 实现退场，指向 `aggregate_sigma()` |
| `Code/build_CR.R` | 修管道解析、条件重命名和当前 dplyr 的 `pick()` |
| `Code/build_CR_MRBRT2023.R` | 显式终点尺度表、源范围断言、CI 顺序断言 |
| `Code/build_CR_GBD2023_gas.R` | 源范围、全分支有限值和 RR≥1 断言；只对 NO2 LOW 截断 |
| `build_instance.R` | 年份显式、混合年份默认阻止、manifest、映射缓存、删除随机 cf、输入早期检查 |
| `HealthBurdenCalc.R` | manifest、单位、情景名、默认写出 |
| `DrivingFactors.R` | manifest、单位、情景列验证、不请求缺失 cf、可关闭自动全量 |
| `Data/RR_std_config.json` | 每个模型增加 `conc_unit` 和 `conc_range` |
| `tests/` | 新增 testthat 回归套件 |
| `README.md`、`Data/README.md`、`Data/RR_index/README.md`、`Result/README.md`、`AGENTS.md` | 同步 v5.7、单位、manifest、测试和未决项 |

## 3. 验收证据

### 3.1 回归测试

命令：

```powershell
& 'C:\Program Files\R\R-4.6.1\bin\Rscript.exe' --vanilla 'tests\run_tests.R'
```

结果：26 tests，94 expectations，0 failed，0 errors，0 warnings。

覆盖：

- 缺失终点聚合保持 NA。
- 数值 Grid_info 列不进入死亡总量。
- 完整输入聚合守恒。
- 重复 Pop/Grid/MortRate 键报错。
- 无关 Province NA 不改变输出。
- mean/branch 两种模式的单分支等于 RANGE。
- `dgt_conc=2` 实际按两位匹配。
- 精度不一致报错。
- 单位和范围配置完整。
- 显式缺失 Conc_cf 报错，省略时保持 NULL。
- 坐标大小写匹配与歧义拒绝。
- manifest 可解析。
- 旧 Uncertainty 明确拒绝。
- 入口使用 manifest、情景名和默认写出。
- 三个构建器解析。

### 3.2 默认完整样本零漂移

修复前后逐值比较：

| 模型/情景 | 行×列 | 最大绝对差 | NA |
|---|---:|---:|---:|
| NCD+LRI `base2015` | 53,713×47 | 0 | 0 |
| NCD+LRI `SSP1-Baseline_2030` | 53,713×47 | 0 | 0 |
| MRBRT2023 `base2015` | 53,713×287 | 0 | 0 |

结论：修复不改变默认 PM2.5 主路径数值。

### 3.3 全样本聚合与不确定性

- `aggregate_range()` 国家总量对网格求和的差：1.46×10^-11 例死亡。
- `aggregate_sigma()` 与 MEAN 总量差：0。
- CI 顺序 `LOW ≤ MEAN ≤ UP`：167/167 国家通过。
- NA：0。

### 3.4 端到端入口

`HealthBurdenCalc.R` 成功运行，并写出四个文件：

- `PM2.5_GEMM_NCD+LRI_Country_total_Build261026.xlsx`
- `PM2.5_GEMM_NCD+LRI_Country_endpoint_Build261026.xlsx`
- `PM2.5_GEMM_NCD+LRI_Country_agegroup_Build261026.xlsx`
- `PM2.5_GEMM_NCD+LRI_Country_endpoint+agegroup_Build261026.xlsx`

驱动因素中国 20 格、序列 1：300×10，NA=0，守恒误差 0。完整 24 序列未跑。

### 3.5 构建和模型加载

- 18 个脚本和测试文件全部通过 `parse()`。
- 13 个模型名全部可 `set_Model()` 加载。
- IER2010 有 26 行 legacy CI 交叉，MRBRT2019 有 148,545 个非有限 RR，均保留加载并警告；新建表使用严格断言。
- `build_instance.R` 在默认配置下按预期阻止混合年份，未写任何实例。

## 4. 未完成与待决

1. **2030 混合年份**：现有 260617 实例的未来列仍是 2030 暴露、2020 人口/死亡率、2019 年龄结构。用户需批准这个代理，或提供真正的 2030 人口、死亡率和年龄结构。批准后设置 `ALLOW_MIXED_SCENARIO_YEARS <- TRUE` 重建。
2. **Conc_cf**：旧 `GridPM25_cf_instance_260617.xlsx` 有负值，默认分解不使用。正式反事实分析前需提供经批准的输入。新构建不再自动生成 cf。
3. **NO2 年龄**：0–14 岁是否纳入仍未确认。
4. **IER LRI 年龄**：0–95 岁复用 `LRI_ALL` 的适用性仍未确认。
5. **legacy 气体单位**：配置暂标 `unknown`，未从来源核实。
6. **超范围浓度**：当前代码 clamp，用户口径仍待确认。
7. **完整验证**：完整 24 序列、真实 O3/NO2 负担、0.1° 全球格网压测未做。
8. **构建器**：`build_CR.R` 已通过解析。修复 `rename_with()` 和 `pick()` 的运行时错误后，IER2017 全量计算已进入运行，但超过本轮等待时间，未完成全表重建验证。
9. **build_instance**：因 `PM25_POP_RAW_DIR` 未提供，未执行真实构建；只验证了早期混合年份阻止。

## 5. 回退

- 代码回退：`git revert <本次提交>`。
- 旧实例和旧 cf 文件未删除。
- `Uncertainty()` 已退场。需要旧不确定性算法时，从提交 `0863a87:Code/uncertainty.R` 取回，但该算法与 PWRR 不一致。
- `aggregate_sigma()` 现在只支持 `by="total"`。端点/年龄分项使用 `aggregate_range()`。

## 6. 指针

- 测试入口：`tests/run_tests.R`
- 实例清单：`Data/instance_manifest.json`
- 配置：`Data/RR_std_config.json`
- 上轮评审：`handoff/261026_design-implementation-review.md`
