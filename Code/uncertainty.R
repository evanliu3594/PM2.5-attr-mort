# 不确定性模块
#   修改 261026b：删除了旧的 PAF-at-PWE 实现。它与 PWRR 估计量不一致。
#   现在由 aggregate_sigma() 从网格级 Mortality() 分支与同一 PWRR 公式
#   计算不确定性。

# 已退役的 PAF/PWE 不确定性入口。... 被忽略，调用总是中止，并提示改用
# 带情景名的 aggregate_sigma()。
Uncertainty <- function(...) {
  cli::cli_abort(c(
    "x" = "Uncertainty() has been retired.",
    "i" = paste0(
      "It used RR at population-weighted exposure and PAF = 1 - 1/RR, ",
      "which does not match the PWRR estimator."
    ),
    "i" = paste0(
      "Use aggregate_sigma() with scenario names. It computes bounds from ",
      "grid-level Mortality() branches."
    )
  ))
}
