# Uncertainty propagation module — analytic error propagation
#   - Uncertainty() — finite-difference sensitivity: σ_M² = Σ (∂M/∂x_i)² × σ_xi²
#     Supports CR uncertainty (RR UP/LOW branches) and optional concentration
#     uncertainty via domain-level PWE perturbation.
#   Modified 260610-260613: fix double-counted sigma in CI formula, change
#     Conc_RMSE from absolute to percentage-based, fix includeConc forwarding
#     and concentration perturbation join logic.

#' Uncertainties Calculation
#'
#' Analytic error propagation combining CR uncertainty (RR UP/LOW branches)
#' with optional concentration uncertainty via domain-level PWE perturbation.
#'
#' @param PWE population-weighted exposure by domain
#' @param aggr_pop aggregated population by domain
#' @param age_struc age structure by domain
#' @param m_Rate baseline mortality rate data
#' @param includeConc whether to include concentration uncertainty, default \code{FALSE}
#' @param Conc_ERR relative concentration uncertainty in percent, default \code{12} (±12%)
#' @param verbose whether to print detailed logs, default \code{FALSE}
#'
#' @return aggregated uncertainty ranges by endpoint and domain (data.frame)
#' @export
#'
#' @examples
Uncertainty <- function(
  PWE,
  aggr_pop,
  age_struc,
  m_Rate,
  includeConc = FALSE,
  Conc_ERR = 12,
  verbose = FALSE
) {
  if (includeConc) {
    log_msg(
      WARN,
      "Including concentration uncertainty at +/-{Conc_ERR}% of PWE."
    )
  }

  PWE <- PWE |>
    na.omit() |>
    rename_with(~'domain', where(is.character)) |>
    rename_with(~'concentration', where(is.numeric))

  aggr_pop <- aggr_pop |>
    na.omit() |>
    rename_with(~'domain', where(is.character))

  RR_std_tbl <- .RR_std_tbl

  RR_base <- PWE |>
    mutate(concentration = matchable(concentration, 1)) |>
    left_join(
      RR_std_tbl |> filter(CI == "MEAN") |> select(-CI),
      by = "concentration"
    ) |>
    mutate(PAF_base = 1 - 1 / RR) |>
    select(-concentration, -RR)

  RR_test_up <- PWE |>
    mutate(concentration = matchable(concentration, 1)) |>
    left_join(
      RR_std_tbl |> filter(CI == "UP") |> select(-CI),
      by = "concentration"
    ) |>
    mutate(PAF_test = 1 - 1 / RR) |>
    select(-concentration, -RR)

  RR_test_low <- PWE |>
    mutate(concentration = matchable(concentration, 1)) |>
    left_join(
      RR_std_tbl |> filter(CI == "LOW") |> select(-CI),
      by = "concentration"
    ) |>
    mutate(PAF_test = 1 - 1 / RR) |>
    select(-concentration, -RR)

  test_PAF_up <- left_join(RR_base, RR_test_up) |>
    mutate(varname = str_glue("test_CR_{domain}_{endpoint}_{agegroup}")) |>
    pivot_wider(names_from = 'varname', values_from = 'PAF_test') |>
    mutate(across(
      matches("^test_CR"),
      ~ replace(.x, is.na(.x), PAF_base[is.na(.x)])
    ))

  test_PAF_low <- left_join(RR_base, RR_test_low) |>
    mutate(varname = str_glue("test_CR_{domain}_{endpoint}_{agegroup}")) |>
    pivot_wider(names_from = 'varname', values_from = 'PAF_test') |>
    mutate(across(
      matches("^test_CR"),
      ~ replace(.x, is.na(.x), PAF_base[is.na(.x)])
    ))

  if (includeConc) {
    # Perturb concentration by +/- Conc_ERR%, look up new RR, compute perturbed PAF.
    # Must preserve domain, endpoint, agegroup for 1:1 join with RR_base below.
    conc_perturb_up <- PWE |>
      mutate(
        concentration = matchable(concentration * (1 + Conc_ERR / 100), 1)
      ) |>
      left_join(RR_std_tbl |> filter(CI == "MEAN") |> select(-CI), by = "concentration") |>
      mutate(PAF_test = 1 - 1 / RR) |>
      select(domain, endpoint, agegroup, PAF_test)

    test_PAF_up <- test_PAF_up |>
      left_join(
        conc_perturb_up |>
          left_join(RR_base, by = c("domain", "endpoint", "agegroup")) |>
          mutate(varname = str_glue("test_Pollu_{domain}_Pollu_Pollu")) |>
          pivot_wider(names_from = 'varname', values_from = 'PAF_test') |>
          mutate(across(
            matches("^test_Pollu"),
            ~ replace(.x, is.na(.x), PAF_base[is.na(.x)])
          )) |>
          select(-PAF_base),
        by = c("domain", "endpoint", "agegroup")
      )

    conc_perturb_low <- PWE |>
      mutate(
        concentration = matchable(concentration * (1 - Conc_ERR / 100), 1)
      ) |>
      left_join(RR_std_tbl |> filter(CI == "MEAN") |> select(-CI), by = "concentration") |>
      mutate(PAF_test = 1 - 1 / RR) |>
      select(domain, endpoint, agegroup, PAF_test)

    test_PAF_low <- test_PAF_low |>
      left_join(
        conc_perturb_low |>
          left_join(RR_base, by = c("domain", "endpoint", "agegroup")) |>
          mutate(varname = str_glue("test_Pollu_{domain}_Pollu_Pollu")) |>
          pivot_wider(names_from = 'varname', values_from = 'PAF_test') |>
          mutate(across(
            matches("^test_Pollu"),
            ~ replace(.x, is.na(.x), PAF_base[is.na(.x)])
          )) |>
          select(-PAF_base),
        by = c("domain", "endpoint", "agegroup")
      )
  }

  Sensi_up <- list(test_PAF_up, aggr_pop, age_struc, m_Rate) |>
    reduce(left_join) |>
    mutate(across(
      matches('^test'),
      ~ Pop * MortRate * AgeStruc * abs(.x - PAF_base) / 1e5
    )) |>
    select(matches('^test')) |>
    map_df(sum, na.rm = T) |>
    pivot_longer(
      matches('^test'),
      names_to = c("item", 'domain', 'endpoint', 'agegroup'),
      names_pattern = "test_(.+)_(.+)_(.+)_(.+)",
      values_to = 'Sensi'
    )

  Sensi_low <- list(test_PAF_low, aggr_pop, age_struc, m_Rate) |>
    reduce(left_join) |>
    mutate(across(
      matches('^test'),
      ~ Pop * MortRate * AgeStruc * abs(.x - PAF_base) / 1e5
    )) |>
    select(matches('^test')) |>
    map_df(sum, na.rm = T) |>
    pivot_longer(
      matches('^test'),
      names_to = c("item", 'domain', 'endpoint', 'agegroup'),
      names_pattern = "test_(.+)_(.+)_(.+)_(.+)",
      values_to = 'Sensi'
    )

  test_sigma_up <- bind_rows(
    left_join(RR_base, RR_test_up) |>
      mutate(sigma = abs(PAF_test - PAF_base), .keep = 'unused') |>
      add_column(item = 'CR'),
    PWE |>
      bind_cols(item = 'Pollu', endpoint = "Pollu", agegroup = 'Pollu') |>
      mutate(sigma = Conc_ERR, .keep = 'unused')
  )

  test_sigma_low <- bind_rows(
    left_join(RR_base, RR_test_low) |>
      mutate(sigma = abs(PAF_test - PAF_base), .keep = 'unused') |>
      add_column(item = 'CR'),
    PWE |>
      bind_cols(item = 'Pollu', endpoint = "Pollu", agegroup = 'Pollu') |>
      mutate(sigma = Conc_ERR, .keep = 'unused')
  )

  # Error propagation: σ_M² = Σ (∂M/∂x_i)² × σ_xi²
  # With finite-difference: ∂M/∂x_i ≈ ΔM_i / σ_xi, where ΔM_i (= Sensi) is the
  # mortality change from a 1σ perturbation. Therefore σ_M² = Σ Sensi_i².
  if (verbose) {
    ci_debug <- Sensi_up |>
      group_by(domain, item) |>
      summarise(contrib = sqrt(sum(Sensi^2)), .groups = "drop") |>
      pivot_wider(names_from = item, values_from = contrib, values_fill = 0)
    cat("\n--- Uncertainty diagnostics (UP side) ---\n")
    print(ci_debug, n = Inf)
    if (includeConc && "Pollu" %in% names(Sensi_up$item)) {
      ci_with <- Sensi_up |>
        group_by(domain) |>
        summarise(CI = sqrt(sum(Sensi^2)), .groups = "drop")
      ci_without <- Sensi_up |>
        filter(item == "CR") |>
        group_by(domain) |>
        summarise(CI = sqrt(sum(Sensi^2)), .groups = "drop")
      ci_compare <- left_join(
        ci_with,
        ci_without,
        by = "domain",
        suffix = c("_withConc", "_CRonly")
      )
      cat("\n--- includeConc impact ---\n")
      print(ci_compare, n = Inf)
    }
  }

  result <- left_join(
    Sensi_up |>
      group_by(domain) |>
      summarise(CI_UP = sqrt(sum(Sensi^2)), .groups = "drop"),
    Sensi_low |>
      group_by(domain) |>
      summarise(CI_LOW = sqrt(sum(Sensi^2)), .groups = "drop"),
    by = "domain"
  )
  return(result)
}
