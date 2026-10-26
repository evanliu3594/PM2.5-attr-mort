#====================================================#
# retrieve driving forces to the PM2.5 health burden #
#====================================================#
# NOTE 261026: PWRR is now computed per domain × endpoint × agegroup.
# Decomposition outputs produced before this date are invalid; rerun this script.
# NOTE 261026: the 24 sequences are written explicitly in the documented order;
# the state of each prefix is derived by one explicit mapping, not by an if chain.

# Core Module Load ----

library(tidyverse)
library(writexl)
library(readxl)
source('./Code/model.R', encoding = 'UTF8')
source('./Code/data.R', encoding = 'UTF8')
source('./Code/mortality.R', encoding = 'UTF8')
source('./Code/uncertainty.R', encoding = 'UTF8')
source('./Code/aggregation.R', encoding = 'UTF8')

# C-R function setting ----
# this section used to choose C-R function for Health Impact Calculation.
# Supported C-R functions:
#              'IER', 'NCD+LRI'(Part of GEMM), '5COD'(Part of GEMM), 'MRBRT', 'MRBRT2023'

set_Model('NCD+LRI')

# Data load ----

instance_files <- resolve_instance_files()
read_files(
  Grids = instance_files$Grids,
  Pop = instance_files$Pop,
  Conc_real = instance_files$Conc_real,
  MortRate = instance_files$MortRate,
  AgeGroup = instance_files$AgeGroup,
  conc_unit = "ug/m3"
)

# 24 decomposition sequences, written explicitly in the documented order ----
# Columns step1..step4 are the order in which the factors are applied.

decomp_sequences <- tribble(
  ~serie, ~step1, ~step2, ~step3, ~step4,
  1,  "PG",  "PA",  "EXP", "ORF",
  2,  "PG",  "PA",  "ORF", "EXP",
  3,  "PG",  "EXP", "PA",  "ORF",
  4,  "PG",  "EXP", "ORF", "PA",
  5,  "PG",  "ORF", "PA",  "EXP",
  6,  "PG",  "ORF", "EXP", "PA",
  7,  "PA",  "PG",  "EXP", "ORF",
  8,  "PA",  "PG",  "ORF", "EXP",
  9,  "PA",  "EXP", "PG",  "ORF",
  10, "PA",  "EXP", "ORF", "PG",
  11, "PA",  "ORF", "PG",  "EXP",
  12, "PA",  "ORF", "EXP", "PG",
  13, "EXP", "PG",  "PA",  "ORF",
  14, "EXP", "PG",  "ORF", "PA",
  15, "EXP", "PA",  "PG",  "ORF",
  16, "EXP", "PA",  "ORF", "PG",
  17, "EXP", "ORF", "PA",  "PG",
  18, "EXP", "ORF", "PG",  "PA",
  19, "ORF", "PG",  "PA",  "EXP",
  20, "ORF", "PG",  "EXP", "PA",
  21, "ORF", "PA",  "PG",  "EXP",
  22, "ORF", "PA",  "EXP", "PG",
  23, "ORF", "EXP", "PG",  "PA",
  24, "ORF", "EXP", "PA",  "PG"
)

# State after a prefix of factors ----
# PG  -> Pop (start -> end)
# PA  -> AgeStruc (start -> end)
# EXP -> Conc_c (start -> end)
# ORF -> Conc_r (start -> end) and MortRate (start -> end)

mort_state <- function(prefix, start.y, end.y) {
  list(
    pop = if ("PG" %in% prefix) end.y else start.y,
    ag = if ("PA" %in% prefix) end.y else start.y,
    conc_c = if ("EXP" %in% prefix) end.y else start.y,
    conc_r = if ("ORF" %in% prefix) end.y else start.y,
    mrate = if ("ORF" %in% prefix) end.y else start.y
  )
}

validate_scenario_columns <- function(start.y, end.y) {
  datasets <- list(
    Conc_real = Conc_real,
    Pop = Pop,
    MortRate = MortRate,
    AgeGroup = AgeGroup
  )
  for (name in names(datasets)) {
    missing_cols <- setdiff(c(start.y, end.y), names(datasets[[name]]))
    if (length(missing_cols) > 0) {
      stop(
        name, " is missing scenario column(s): ",
        paste(missing_cols, collapse = ", ")
      )
    }
  }
  invisible(TRUE)
}

add_domain <- function(mort, domain) {
  mort |>
    left_join(
      Grid_info |> select(x, y, all_of(domain)),
      by = c("x", "y")
    )
}

mort_at_prefix <- function(prefix, start.y, end.y, domain) {
  state <- mort_state(prefix, start.y, end.y)
  Mortality(
    Grids = Grid_info,
    CI = "MEAN",
    pop = Pop |> select(x, y, Pop = !!sym(state$pop)),
    ag = AgeGroup |>
      select(all_of("domain"), agegroup, AgeStruc = !!sym(state$ag)),
    Conc_c = Conc_real |>
      select(x, y, concentration = !!sym(state$conc_c)),
    Conc_r = Conc_real |>
      select(x, y, concentration = !!sym(state$conc_r)),
    mRate = getMortRate(state$mrate),
    domain = domain
  )
}

Decomposition <- function(serie, start.y, end.y, domain = NULL) {
  validate_scenario_columns(start.y, end.y)
  if (is.null(domain)) {
    domain <- detect_domain()
  }

  seq_row <- decomp_sequences |> filter(serie == .env$serie)
  if (nrow(seq_row) != 1) {
    stop("serie must be one of 1:24.")
  }

  steps <- seq_row |>
    select(step1:step4) |>
    unlist(use.names = FALSE)

  prefixes <- list(
    character(0),
    steps[1],
    steps[1:2],
    steps[1:3],
    steps[1:4]
  )

  Decomp <- prefixes |>
    map(
      mort_at_prefix,
      start.y = start.y,
      end.y = end.y,
      domain = domain
    ) |>
    map(add_domain, domain = domain) |>
    set_names(str_c("Mort_", 0:4))

  Decomp <- Decomp |>
    imap_dfr(
      ~ .x |>
        pivot_longer(
          cols = -all_of(c("x", "y", domain)),
          names_to = "Cause_Age",
          values_to = "Mort"
        ) |>
        mutate(Step = .y)
    ) |>
    pivot_wider(names_from = "Step", values_from = "Mort") |>
    mutate(
      Start = Mort_0,
      !!steps[1] := Mort_1 - Mort_0,
      !!steps[2] := Mort_2 - Mort_1,
      !!steps[3] := Mort_3 - Mort_2,
      !!steps[4] := Mort_4 - Mort_3,
      End = Mort_4,
      .keep = "unused"
    )

  # Print Result ----
  {
    cat(str_c("Drivers Between ", start.y, " and ", end.y, ":\n"))
    for (step_name in steps) {
      cat(
        str_c(
          step_name,
          ":\t",
          sum(Decomp[[step_name]] |> sum() |> round())
        ),
        "\n"
      )
    }
  }

  return(Decomp)
}

aggregate_drivers <- function(
  RUN24 = TRUE,
  start.year = 'base2015',
  end.year = 'SSP1-Baseline_2030',
  domain = NULL
) {
  validate_scenario_columns(start.year, end.year)
  if (is.null(domain)) {
    domain <- detect_domain()
  }

  n <- if (RUN24) 1:24 else 1

  ls1 <- n |>
    set_names() |>
    map(
      ~ Decomposition(
        serie = .x,
        start.y = start.year,
        end.y = end.year,
        domain = domain
      ) |>
        group_by(across(all_of(domain))) |>
        summarise(across(Start:End, sum))
    )

  ls1[["Summary"]] <- ls1 |>
    bind_rows() |>
    group_by(across(all_of(domain))) |>
    summarise(across(Start:End, mean))

  ls1[["Sequences"]] <- decomp_sequences

  ls1 |>
    write_xlsx(str_glue(
      "./Results/{tell_Model()}_DrivingFactors_",
      "{start.year}-{end.year}_Build{format(Sys.Date(), '%y%m%d')}.xlsx"
    ))
}

RUN_DRIVERS <- tolower(Sys.getenv("PM25_RUN_DRIVERS", unset = "true")) == "true"
if (RUN_DRIVERS) {
  aggregate_drivers(start.year = 'base2015', end.year = 'SSP1-Baseline_2030')
}
