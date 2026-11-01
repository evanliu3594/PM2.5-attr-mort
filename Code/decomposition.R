# Decomposition module — driving forces of the PM2.5 health burden
#
#   decomp_sequences          — 24 cumulative factor sequences (documented order)
#   Decomposition()           — grid-level contributions for one sequence
#   Decomposition_summary()   — country-level contributions for one sequence
#   aggregate_drivers()       — all 24 sequences; writes Summary and Sequences
#
# State cache: the 24 sequences share 16 unique factor states. get_decomp_state()
# and decomp_state_summary() cache them by sorted factor set. decomp_fill_cache()
# builds missing states in one parallel batch when a Code/kernel.R plan is
# active; without a plan the cache path stays serial.
#
# Input cache (v6.3): every factor state differs only in which scenario column
# each factor reads, so decomp_input_cache() builds the two RR matrices and the
# four PWRR tables once per run and state_grid_cached() reuses them. When the
# vectorised engine (Code/mortality.R) is absent, or a harness replaces
# Mortality() in its own environment, the module keeps the join path.
#
# Requires: tidyverse, writexl. Loaded by: DrivingFactors.R.
# NOTE 261026: PWRR is computed per domain × endpoint × agegroup.
# NOTE 261027: parallel state construction via Code/kernel.R (optional).
# Modified 261101: grid-set guard before differencing (C1); one concise NA and
#   dropped-grid report per run (I7); decomp_sequences validated at load (I8);
#   aggregate_drivers() caches state summaries only (I9) and creates Results/
#   before writing (C2).
# Modified 261101 (v6.3): state-independent inputs are cached once per run
#   (decomp_input_cache(), state_grid_cached()); 16 states build two RR
#   matrices and four PWRR tables instead of sixteen of each. Values stay
#   bit-identical because the cached path calls the same engine functions.
# Modified 261101 (v6.3 diagnostics): the cached path prints the same clamp,
#   orphan, and coverage reports that Mortality() printed per state, once per
#   run and with the v6.2 labels (Conc_r / Conc_c). aggregate_drivers() runs the
#   grid-set guard once over the 16 summaries and every Decomposition_summary()
#   call skips it.

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

# Fail at load, not after a 30-second run: a mistyped sequence row would apply
# one factor twice and drop another, and the decomposition output would carry
# no trace of the mistake (I8).
validate_decomp_sequences <- function(x = decomp_sequences) {
  required <- c("serie", "step1", "step2", "step3", "step4")
  missing_cols <- setdiff(required, names(x))
  if (length(missing_cols) > 0) {
    stop(
      "decomp_sequences is missing column(s): ",
      paste(missing_cols, collapse = ", "),
      "."
    )
  }
  if (nrow(x) != 24) {
    stop("decomp_sequences must have 24 rows, got ", nrow(x), ".")
  }
  duplicated_serie <- unique(x$serie[duplicated(x$serie)])
  if (length(duplicated_serie) > 0) {
    stop(
      "decomp_sequences$serie must be unique; duplicated value(s): ",
      paste(duplicated_serie, collapse = ", "),
      "."
    )
  }

  expected <- c("PG", "PA", "EXP", "ORF")
  step_cols <- c("step1", "step2", "step3", "step4")
  steps <- as.matrix(x[step_cols])
  is_permutation <- apply(steps, 1, function(row) {
    setequal(row, expected) && !anyDuplicated(row)
  })
  if (!all(is_permutation)) {
    bad_rows <- which(!is_permutation)
    bad_text <- x |>
      slice(bad_rows) |>
      mutate(across(all_of(step_cols), as.character)) |>
      unite("row", all_of(step_cols), sep = ", ") |>
      pull(row)
    stop(
      "decomp_sequences row(s) ",
      paste(bad_rows, collapse = ", "),
      " must each be a permutation of PG, PA, EXP, ORF; got: ",
      paste(bad_text, collapse = "; "),
      "."
    )
  }
  invisible(TRUE)
}

validate_decomp_sequences(decomp_sequences)

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
    domain = domain,
    validate_keys = FALSE
  )
}

# TRUE when the cached input path may run. It needs the v6.3 engine functions,
# the diagnostic helpers they print through, and an RR table in the same scope
# as Mortality(), because the cached path reproduces Mortality()'s engine
# internals. A harness that replaces Mortality() with a stub in its own
# environment keeps the join path; a checkout without the engine falls back
# cleanly. `env` is the scope that would run the cached path (the caller's frame
# by default).
decomp_engine_available <- function(env = parent.frame()) {
  engine_functions <- c(
    "mort_rr_matrix", "mort_pwr_table", "mort_grid_table",
    "mortality_clamp_report", "mortality_clamp_text",
    "mortality_orphan_report", "mortality_coverage_cached",
    "mortality_assert_rr_key"
  )
  present <- vapply(
    engine_functions,
    function(name) {
      exists(name, envir = env, mode = "function", inherits = TRUE)
    },
    logical(1)
  )
  if (!all(present) ||
    !exists("Mortality", envir = env, mode = "function", inherits = TRUE)) {
    return(FALSE)
  }
  rr_raw <- get0(
    ".RR_std_tbl",
    envir = env,
    inherits = TRUE,
    ifnotfound = NULL
  )
  if (is.null(rr_raw) || nrow(rr_raw) == 0) {
    return(FALSE)
  }
  # Same scope: in production all four functions come from globalenv. A stub
  # Mortality() assigned to a test environment does not share the engine scope.
  identical(
    environment(get("Mortality", envir = env, inherits = TRUE)),
    environment(get("mort_grid_table", envir = env, inherits = TRUE))
  )
}

# Identity of one cached input set: scenario names, domain, and the RR table
# identity (model name, row count, concentration precision).
decomp_input_key <- function(start.y, end.y, domain) {
  rr_raw <- get0(".RR_std_tbl", ifnotfound = NULL, inherits = TRUE)
  model <- get0(".CR_Model", ifnotfound = NA_character_, inherits = TRUE)
  dgt <- if (is.null(rr_raw)) {
    NA_integer_
  } else {
    as.integer(
      attr(rr_raw, "dgt_conc", exact = TRUE) %||%
        get0(".DGT_CONC", ifnotfound = 1L, inherits = TRUE)
    )
  }
  str_c(
    start.y,
    end.y,
    str_c(as.character(domain), collapse = "+"),
    model %||% NA_character_,
    nrow(rr_raw) %||% 0L,
    dgt,
    sep = "::"
  )
}

# Cache states within one decomposition run; sorted prefixes identify equivalent inputs.
new_decomp_cache <- function() {
  cache <- new.env(parent = emptyenv())
  cache$states <- new.env(parent = emptyenv())
  cache$n_mortality <- 0L
  # Input cache, built lazily by decomp_inputs_for() on the first state.
  cache$inputs <- NULL
  cache$engine <- exists("decomp_engine_available", mode = "function") &&
    decomp_engine_available()
  cache
}

decomp_state_key <- function(prefix, start.y, end.y, domain) {
  factors <- if (length(prefix) == 0) {
    "<start>"
  } else {
    str_c(sort(prefix), collapse = "+")
  }
  str_c(start.y, end.y, str_c(domain, collapse = "+"), factors, sep = "::")
}

# State-independent inputs plus the RR matrices and PWRR tables every state
# shares. Called once per (start.y, end.y, domain, RR table) run.
#
# Memory: one RR matrix is n_grid x G doubles; the cache holds two of them, four
# G x D PWRR tables, the two population vectors, and the G x D per-domain
# age-structure and mortality-rate matrices. At 68,302 grids and 15 groups that
# is about 18 MB, plus about 33 MB for the four pre-expanded G x n_grid copies
# (cached only while n_grid x G stays at or below 2e6 cells). At 0.1 degree
# (about 800,000 grids) one RR matrix is about 96 MB, the cache stays near the
# 192 MB of its two RR matrices, and the group matrices expand per state.
decomp_input_cache <- function(start.y, end.y, domain) {
  rr_raw <- .RR_std_tbl
  if (is.null(rr_raw) || nrow(rr_raw) == 0) {
    stop("RR lookup table is empty. Run set_Model() first.")
  }
  require_columns(
    rr_raw,
    c("concentration", "endpoint", "agegroup", "CI", "RR"),
    "RR lookup table"
  )

  # ---- Same precision, unit, and normalisation guards as Mortality() ----
  conc_dgt <- get0(".DGT_CONC", envir = globalenv(), ifnotfound = 1)
  rr_dgt <- attr(rr_raw, "dgt_conc", exact = TRUE) %||% conc_dgt
  if (!identical(as.integer(conc_dgt), as.integer(rr_dgt))) {
    stop(
      "RR table precision (", rr_dgt, ") differs from .DGT_CONC (",
      conc_dgt, "). Rebuild the RR table with the requested precision."
    )
  }
  rr_meta <- get0(".RR_meta", envir = globalenv(), ifnotfound = list())
  conc_unit <- get0(".CONC_UNIT", envir = globalenv(), ifnotfound = NULL)
  rr_unit <- attr(rr_raw, "conc_unit", exact = TRUE) %||%
    rr_meta$conc_unit %||%
    NULL
  if (!is.null(conc_unit) && !is.null(rr_unit) && !identical(conc_unit, rr_unit)) {
    stop(
      "Concentration unit mismatch: RR model expects \"", rr_unit,
      "\", data were loaded as \"", conc_unit, "\"."
    )
  }
  if (!isTRUE(attr(rr_raw, "rr_normalized", exact = TRUE))) {
    rr_raw <- rr_raw |>
      mutate(
        concentration = matchable(as.numeric(concentration), conc_dgt),
        endpoint = tolower(endpoint),
        agegroup = matchable(as.numeric(agegroup), 0)
      )
    # RR_std() key-checks the tables it builds before setting rr_normalized, so
    # the expensive duplicate scan runs for hand-built tables only. The check is
    # shared with Mortality() and runs once per table identity.
    mortality_assert_rr_key(rr_raw)
  }
  if (!setequal(unique(rr_raw$CI), c("MEAN", "UP", "LOW"))) {
    stop("RR lookup table must contain exactly MEAN, UP, and LOW branches.")
  }
  rr_conc_range <- range(as.numeric(rr_raw$concentration), na.rm = TRUE)

  # ---- Scenario columns, one entry per distinct scenario name ----
  scenarios <- unique(c(start.y, end.y))
  require_columns(Grid_info, c("x", "y", domain), "Grids")
  require_columns(Pop, c("x", "y", scenarios), "pop")
  require_columns(Conc_real, c("x", "y", scenarios), "Conc_real")
  require_columns(AgeGroup, c("domain", "agegroup", scenarios), "AgeGroup")
  grid_domain <- Grid_info |>
    select(x, y, domain = all_of(domain))
  # Every state grid is a row subset of Grid_info, so one key check here covers
  # the per-state "Mortality output" check that Mortality() runs.
  assert_unique_key(grid_domain, c("x", "y"), "Grid_info")

  # Concentrations: the real and counterfactual column both come from Conc_real,
  # so one cached vector serves either role. One join carries every scenario
  # column; clamping and matchable() stay per column, exactly as Mortality().
  #
  # A scenario column is Conc_r in some factor states and Conc_c in others. v6.2
  # printed one report per role per state; the unique texts are one per role, so
  # clamp each column once and describe both roles with the same counts.
  conc_frame <- Conc_real |> select(x, y, all_of(scenarios))
  clamp_reports <- character()
  for (at in scenarios) {
    report <- mortality_clamp_report(
      as.numeric(conc_frame[[at]]),
      rr_conc_range,
      "Conc_r"
    )
    conc_frame[[at]] <- matchable(report$value, conc_dgt)
    clamp_reports <- c(
      clamp_reports,
      report$text,
      mortality_clamp_text(report$n_lo, report$n_hi, rr_conc_range, "Conc_c")
    )
  }
  for (text in unique(clamp_reports)) {
    log_msg(WARN, "{text}")
  }
  conc_joined <- grid_domain |> left_join(conc_frame, by = c("x", "y"))
  conc <- map(scenarios, function(at) conc_joined[[at]]) |>
    set_names(scenarios)

  pop_frame <- Pop |> select(x, y, all_of(scenarios))
  for (at in scenarios) {
    values <- as.numeric(pop_frame[[at]])
    if (any(values < 0, na.rm = TRUE)) {
      stop("Population contains negative values.")
    }
    pop_frame[[at]] <- values
  }
  pop_joined <- grid_domain |> left_join(pop_frame, by = c("x", "y"))
  pop <- map(scenarios, function(at) pop_joined[[at]]) |>
    set_names(scenarios)

  # The grid sets of Conc_real and Pop do not change with the scenario column, so
  # one orphan report each covers every state; v6.2 repeated the same text per
  # state. show_n is 6 because Decomposition() has no verbose switch.
  orphan_reports <- c(
    mortality_orphan_report(
      grid_domain[c("x", "y")],
      conc_frame |> select(x, y) |> distinct(),
      Grid_info,
      domain,
      "concentration",
      6
    ),
    mortality_orphan_report(
      grid_domain[c("x", "y")],
      pop_frame |> select(x, y) |> distinct(),
      Grid_info,
      domain,
      "population",
      6
    )
  )
  for (text in orphan_reports) {
    log_msg(WARN, "{text}")
  }

  ag <- map(scenarios, function(at) {
    AgeGroup |>
      select(domain, agegroup, AgeStruc = !!sym(at)) |>
      mutate(
        agegroup = matchable(as.numeric(agegroup), 0),
        AgeStruc = as.numeric(AgeStruc)
      )
  }) |>
    set_names(scenarios)

  mrate <- map(scenarios, function(at) {
    getMortRate(at) |>
      mutate(
        endpoint = tolower(endpoint),
        agegroup = matchable(as.numeric(agegroup), 0),
        MortRate = as.numeric(MortRate)
      )
  }) |>
    set_names(scenarios)

  # ---- Groups, RR matrices, PWRR tables: the shared expensive part ----
  mean_branch <- rr_raw[
    rr_raw$CI == "MEAN",
    c("concentration", "endpoint", "agegroup", "RR")
  ]
  groups <- mean_branch |> distinct(endpoint, agegroup)
  if (nrow(groups) == 0) {
    stop("RR lookup table has no endpoint-agegroup groups.")
  }
  # ---- Coverage report for every scenario combination a state can read ----
  # Population and counterfactual concentration select the grid rows; the rate
  # and age tables select the covered endpoint/agegroup groups. The report reads
  # no values, only NA masks and coverage sets, so mortality_coverage_cached()
  # collapses the 16 combinations to one computation here. v6.2 repeated the same
  # text once per state; print one copy per distinct text.
  domain_vec <- grid_domain[["domain"]]
  coverage_reports <- character()
  for (conc_at in scenarios) {
    for (pop_at in scenarios) {
      base_c <- tibble(
        x = grid_domain$x,
        y = grid_domain$y,
        domain = domain_vec,
        concentration = conc[[conc_at]],
        Pop = pop[[pop_at]]
      )
      for (mrate_at in scenarios) {
        for (ag_at in scenarios) {
          usable_groups <- groups |>
            semi_join(
              mrate[[mrate_at]] |> distinct(endpoint, agegroup),
              by = c("endpoint", "agegroup")
            ) |>
            semi_join(
              ag[[ag_at]] |> distinct(agegroup),
              by = "agegroup"
            )
          # Mortality() aborts before its coverage report when no group is
          # usable, so print nothing for that combination.
          if (nrow(usable_groups) == 0) {
            next
          }
          coverage_reports <- c(
            coverage_reports,
            mortality_coverage_cached(
              base_c = base_c,
              usable_groups = usable_groups,
              mrate = mrate[[mrate_at]],
              ag = ag[[ag_at]],
              domain = domain,
              show_n = 6
            )
          )
        }
      }
    }
  }
  for (text in unique(coverage_reports)) {
    log_msg(WARN, "{text}")
  }

  value_names <- str_c(groups$endpoint, groups$agegroup, "MEAN", sep = "_")

  dom_names <- unique(as.character(domain_vec[!is.na(domain_vec)]))
  dom_code <- match(as.character(domain_vec), dom_names)

  rr <- map(scenarios, function(at) {
    mort_rr_matrix(conc[[at]], mean_branch, groups)
  }) |>
    set_names(scenarios)

  pwr <- list()
  for (pop_at in scenarios) {
    # One call per population scenario: the branch variant splits the domain
    # rows and the population denominator once for both concentration columns.
    tables <- mort_pwr_table_branches(
      rr[scenarios],
      pop[[pop_at]],
      domain_vec
    )
    for (index in seq_along(scenarios)) {
      pwr[[str_c(pop_at, "|", scenarios[[index]])]] <- tables[[index]]
    }
  }

  # Per-domain group values (G x D) expand to G x n_grid per state, which keeps
  # the cache small when a 0.1 degree grid makes one G x n_grid matrix 96 MB.
  group_domain_matrix <- function(table, by_columns, value_column) {
    out <- matrix(NA_real_, nrow = nrow(groups), ncol = length(dom_names))
    for (g in seq_len(nrow(groups))) {
      keep <- rep(TRUE, nrow(table))
      for (column in by_columns) {
        keep <- keep & table[[column]] == groups[[column]][[g]]
      }
      one <- table[keep, , drop = FALSE]
      out[g, ] <- one[[value_column]][match(dom_names, as.character(one$domain))]
    }
    out
  }
  ag_dom <- map(ag, function(table) {
    group_domain_matrix(table, "agegroup", "AgeStruc")
  }) |>
    set_names(scenarios)
  mrate_dom <- map(mrate, function(table) {
    group_domain_matrix(table, c("endpoint", "agegroup"), "MortRate")
  }) |>
    set_names(scenarios)

  # Expand the four G x n_grid matrices once when they stay small. At 68,302
  # grids and 15 groups they cost about 33 MB; at 0.1 degree (about 800,000
  # grids) the same four would cost about 384 MB, so the cache keeps only the
  # per-domain tables and expands per state there. Values are identical either
  # way; the limit bounds the cache near the two RR matrices.
  expanded_cells <- nrow(grid_domain) * nrow(groups)
  if (expanded_cells <= 2e6) {
    ag_grid <- map(ag_dom, function(one) one[, dom_code, drop = FALSE]) |>
      set_names(scenarios)
    mrate_grid <- map(mrate_dom, function(one) one[, dom_code, drop = FALSE]) |>
      set_names(scenarios)
  } else {
    ag_grid <- NULL
    mrate_grid <- NULL
  }

  list(
    key = decomp_input_key(start.y, end.y, domain),
    start.y = start.y,
    end.y = end.y,
    domain = domain,
    grid_xy = grid_domain[c("x", "y")],
    domain_vec = domain_vec,
    groups = groups,
    value_names = value_names,
    dom_names = dom_names,
    dom_code = dom_code,
    conc = conc,
    rr = rr,
    pwr = pwr,
    pop = pop,
    ag_dom = ag_dom,
    mrate_dom = mrate_dom,
    ag_grid = ag_grid,
    mrate_grid = mrate_grid,
    n_rr = length(rr),
    n_pwr = length(pwr)
  )
}

# Build the input cache lazily for one decomp cache and reuse it until the
# scenario names, domain, or RR table identity change.
decomp_inputs_for <- function(cache, start.y, end.y, domain) {
  key <- decomp_input_key(start.y, end.y, domain)
  inputs <- cache$inputs
  if (is.null(inputs) || !identical(inputs$key, key)) {
    inputs <- decomp_input_cache(start.y, end.y, domain)
    cache$inputs <- inputs
  }
  inputs
}

# PWRR guard per state in Mortality()'s report order: Inf or PWRR < 1 aborts,
# NaN warns and drops the affected rows. Running it per state keeps the warning
# count of the join path (one warning per Mortality() call).
decomp_check_pwrr <- function(pwr_tbl, groups, dom_names, show_n = 6) {
  skipped_keys <- character()
  for (g in seq_len(nrow(groups))) {
    one <- pwr_tbl[g, ]
    is_inf <- is.infinite(one)
    is_lt1 <- one < 1 & !is.na(one)
    is_nan <- is.nan(one)
    if (any(is_inf) || any(is_lt1)) {
      bad_pos <- which(is_inf | is_lt1)
      bad_keys <- str_c(
        dom_names[bad_pos],
        groups$endpoint[[g]],
        groups$agegroup[[g]],
        sep = "/"
      )
      stop(
        "Invalid PWRR detected for endpoint/agegroup ",
        groups$endpoint[[g]],
        "/",
        groups$agegroup[[g]],
        ". ",
        if (any(is_inf)) "Some domains have Inf PWRR. ",
        if (any(is_lt1)) "Some have PWRR < 1. ",
        "Check concentration and population for: ",
        trunc_fmt(unique(bad_keys), n = show_n)
      )
    }
    if (any(is_nan)) {
      nan_pos <- which(is_nan)
      skipped_keys <- c(
        skipped_keys,
        str_c(
          dom_names[nan_pos],
          groups$endpoint[[g]],
          groups$agegroup[[g]],
          sep = "/"
        )
      )
    }
  }
  skipped_keys <- unique(skipped_keys)
  if (length(skipped_keys) > 0) {
    log_msg(
      WARN,
      "{length(skipped_keys)} domain/endpoint/age combination(s) have no valid ",
      "PWRR; affected rows will be dropped: ",
      trunc_fmt(skipped_keys, n = show_n)
    )
  }
  invisible(NULL)
}

# One state grid from cached inputs. Semantics equal decomp_state_grid(): the
# same groups, RR lookup, PWRR denominator, keep mask, and add_domain() tail.
state_grid_from_inputs <- function(prefix, inputs, domain) {
  state <- mort_state(prefix, inputs$start.y, inputs$end.y)
  pwr <- inputs$pwr[[str_c(state$pop, "|", state$conc_r)]]
  decomp_check_pwrr(pwr, inputs$groups, inputs$dom_names)
  ag <- if (is.null(inputs$ag_grid)) {
    inputs$ag_dom[[state$ag]][, inputs$dom_code, drop = FALSE]
  } else {
    inputs$ag_grid[[state$ag]]
  }
  mrate <- if (is.null(inputs$mrate_grid)) {
    inputs$mrate_dom[[state$mrate]][, inputs$dom_code, drop = FALSE]
  } else {
    inputs$mrate_grid[[state$mrate]]
  }
  mort <- mort_grid_table(
    rr_c = inputs$rr[[state$conc_c]],
    rr_r = inputs$rr[[state$conc_r]],
    pwr = pwr,
    pop = inputs$pop[[state$pop]],
    ag = ag,
    mrate = mrate,
    groups = inputs$groups,
    domain = inputs$domain_vec,
    value_names = inputs$value_names,
    grid_xy = inputs$grid_xy
  )
  if (nrow(mort) == 0) {
    stop("Mortality result has 0 rows. Check input coverage.")
  }
  # Output keys need no check here: decomp_input_cache() validates Grid_info and
  # every output row is one of its rows.
  add_domain(mort, domain = domain)
}

# Grid-level state that reuses the shared input cache. Equivalent to
# decomp_state_grid() when the v6.3 engine is present.
state_grid_cached <- function(prefix, start.y, end.y, domain, cache = NULL) {
  if (is.null(cache)) {
    cache <- new_decomp_cache()
  }
  inputs <- decomp_inputs_for(cache, start.y, end.y, domain)
  state_grid_from_inputs(prefix, inputs, domain)
}

# Country-level summary of one cached state.
state_summary_cached <- function(prefix, start.y, end.y, domain, cache) {
  state_grid_cached(prefix, start.y, end.y, domain, cache = cache) |>
    state_summary_from_grid(domain = domain)
}

# One series as its five cumulative factor prefixes: start, then each step.
decomp_series_prefixes <- function(serie) {
  seq_row <- decomp_sequences |> filter(serie == .env$serie)
  if (nrow(seq_row) != 1) {
    stop("serie must be one of 1:24.")
  }
  steps <- seq_row |>
    select(step1:step4) |>
    unlist(use.names = FALSE)
  list(
    character(0),
    steps[1],
    steps[1:2],
    steps[1:3],
    steps[1:4]
  )
}

# Unique factor states needed by the requested series, keyed by decomp_state_key().
# 24 sequences share 16 states; one series needs 5.
decomp_unique_prefixes <- function(start.y, end.y, domain, RUN24 = TRUE) {
  series <- if (RUN24) 1:24 else 1
  prefixes <- series |>
    map(decomp_series_prefixes) |>
    unlist(recursive = FALSE)
  keys <- prefixes |>
    map_chr(
      decomp_state_key,
      start.y = start.y,
      end.y = end.y,
      domain = domain
    )
  prefixes[!duplicated(keys)] |>
    set_names(unique(keys))
}

# Country-level summary of one state: attributable deaths summed by domain × Cause_Age.
# The summary carries the state's grid count and (x, y) key set, so
# Decomposition_summary() can abort when states cover different grids (C1).
# NA stays NA: the conservation identity needs NA propagation, not na.rm.
#
# A grouped sum on the wide grid, then one stack of the small domain-level
# table, gives the same values as stacking the grid to long form and summing
# per (domain, Cause_Age): each group sums the same grid rows in the same
# order. arrange() over the two group keys reproduces group_by()'s output
# order, so the summary stays bit-identical while avoiding the 800k-row long
# table that dominated the state loop.
state_summary_from_grid <- function(mort, domain) {
  value_cols <- setdiff(names(mort), c("x", "y", domain))
  summary <- mort |>
    group_by(across(all_of(domain))) |>
    summarise(across(all_of(value_cols), sum), .groups = "drop") |>
    pivot_longer(
      cols = -all_of(domain),
      names_to = "Cause_Age",
      values_to = "Mort"
    ) |>
    arrange(.data[[domain]], Cause_Age)
  attr(summary, "decomp_n_grid") <- nrow(mort)
  attr(summary, "decomp_grid_keys") <- str_c(mort$x, mort$y, sep = "|")
  summary
}

# C1 grid-set guard over state summaries: every state must carry the same (x, y)
# key set before the differences are taken. Returns invisible(TRUE); aborts on a
# mismatch and warns when the summaries carry no key set. Decomposition_summary()
# calls it for its own five summaries; aggregate_drivers() calls it once over the
# 16 unique states and then passes check_keys = FALSE to every series.
decomp_guard_summary_grids <- function(summaries, state_labels, caller) {
  key_sets <- map(summaries, function(summary) {
    attr(summary, "decomp_grid_keys", exact = TRUE)
  })
  if (!all(map_lgl(key_sets, is.character))) {
    log_msg(
      WARN,
      caller, ": state summaries carry no grid key set; the grid-set guard is ",
      "skipped. Build summaries with state_summary_from_grid() to enable it."
    )
    return(invisible(FALSE))
  }
  n_keys <- map_int(key_sets, length)
  same_keys <- all(map_lgl(
    key_sets,
    function(keys) setequal(keys, key_sets[[1]])
  ))
  if (length(unique(n_keys)) > 1 || !same_keys) {
    stop(
      caller, " aborted: the factor states cover different (x, y) grid sets (",
      str_c(state_labels, " = ", n_keys, " grids", collapse = ", "),
      "). Differencing them would sum over different grids and bias every ",
      "domain total. Rebuild the state summaries with ",
      "state_summary_from_grid() from one grid set."
    )
  }
  invisible(TRUE)
}

# One kernel task: build a state grid, then return only its small summary.
state_summary_for_prefix <- function(prefix, start.y, end.y, domain) {
  mort_at_prefix(prefix, start.y, end.y, domain) |>
    add_domain(domain = domain) |>
    state_summary_from_grid(domain = domain)
}

# Summarise the unique states once. The serial path builds the shared input
# cache once and reuses it for every state; parallel dispatch keeps the
# Mortality() path below because each worker would otherwise rebuild the two RR
# matrices per task.
precompute_state_summaries <- function(
  states,
  start.y,
  end.y,
  domain,
  cache = NULL
) {
  if (is.null(cache)) {
    cache <- new_decomp_cache()
  }
  kernel_on <- exists("kernel_active", mode = "function") &&
    isTRUE(kernel_active())
  # v6.3: with the engine available the serial input cache is faster than
  # dispatch (measured 6.4 s vs 12.3 s for 16 states), so the cache wins even
  # when a plan is active. The kernel path stays for checkouts without the
  # engine, and its tests still cover it through fixture environments.
  if (isTRUE(cache$engine)) {
    if (kernel_on) {
      log_msg(
        INFO,
        "kernel: state assembly uses the serial input cache; ",
        "dispatch would be slower after v6.3."
      )
    }
    summaries <- map(states, function(prefix) {
      state_summary_cached(prefix, start.y, end.y, domain, cache = cache)
    })
    return(set_names(summaries, names(states)))
  }

  summaries <- kernel_map(
    states,
    state_summary_for_prefix,
    start.y = start.y,
    end.y = end.y,
    domain = domain,
    .labels = names(states),
    .label = "DrivingFactors states",
    .error = "stop"
  )
  set_names(summaries, names(states))
}

# Grid-level state: the mortality table for one cumulative factor prefix.
decomp_state_grid <- function(prefix, start.y, end.y, domain) {
  mort_at_prefix(prefix, start.y, end.y, domain) |>
    add_domain(domain = domain)
}

# Fill cache entries that are still missing, one kernel task per state.
# No-op when no kernel plan is active, so the serial path is unchanged.
decomp_fill_cache <- function(prefixes, start.y, end.y, domain, cache) {
  kernel_on <- exists("kernel_active", mode = "function") &&
    isTRUE(kernel_active())
  # The cached engine path builds states on demand in the main process; parallel
  # fill would rebuild the input matrices in every worker.
  if (isTRUE(cache$engine)) {
    return(invisible(cache))
  }
  if (!kernel_on) {
    return(invisible(cache))
  }

  keys <- prefixes |>
    map_chr(
      decomp_state_key,
      start.y = start.y,
      end.y = end.y,
      domain = domain
    )
  is_cached <- vapply(
    keys,
    function(key) exists(key, envir = cache$states, inherits = FALSE),
    logical(1)
  )
  if (all(is_cached)) {
    return(invisible(cache))
  }

  states <- prefixes[!is_cached] |> set_names(keys[!is_cached])
  grids <- kernel_map(
    states,
    decomp_state_grid,
    start.y = start.y,
    end.y = end.y,
    domain = domain,
    .labels = names(states),
    .label = "Decomposition states",
    .error = "stop"
  )

  # Store fetched grids under their canonical keys and count the Mortality calls.
  for (index in seq_along(grids)) {
    assign(
      names(grids)[[index]],
      list(grid = grids[[index]], summary = NULL),
      envir = cache$states
    )
  }
  cache$n_mortality <- cache$n_mortality + length(grids)
  invisible(cache)
}

get_decomp_state <- function(prefix, start.y, end.y, domain, cache) {
  key <- decomp_state_key(prefix, start.y, end.y, domain)
  if (exists(key, envir = cache$states, inherits = FALSE)) {
    return(get(key, envir = cache$states, inherits = FALSE))
  }

  mort <- if (isTRUE(cache$engine)) {
    state_grid_cached(prefix, start.y, end.y, domain, cache = cache)
  } else {
    decomp_state_grid(prefix, start.y, end.y, domain)
  }
  entry <- list(grid = mort, summary = NULL)
  assign(key, entry, envir = cache$states)
  cache$n_mortality <- cache$n_mortality + 1L
  entry
}

decomp_state_summary <- function(prefix, start.y, end.y, domain, cache) {
  entry <- get_decomp_state(prefix, start.y, end.y, domain, cache)
  if (is.null(entry$summary)) {
    entry$summary <- state_summary_from_grid(entry$grid, domain)
    key <- decomp_state_key(prefix, start.y, end.y, domain)
    assign(key, entry, envir = cache$states)
  }
  entry$summary
}

# Print the driver totals and, when state issues are supplied, one concise
# report on NA cells, affected domains, and dropped-grid counts (I7).
# issues: named list with n_grid, n_na, and na_domains per factor state.
print_decomp_drivers <- function(
  Decomp,
  steps,
  start.y,
  end.y,
  issues = NULL,
  domain = NULL,
  grid_total = NULL,
  report_only = FALSE
) {
  if (!report_only) {
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
  if (is.null(issues)) {
    return(invisible(NULL))
  }

  labels <- names(issues)
  n_grid <- map_int(issues, ~ as.integer(.x$n_grid %||% NA_integer_))
  n_na <- map_int(issues, "n_na")
  na_domains <- sort(unique(unlist(map(issues, "na_domains"), use.names = FALSE)))
  na_states <- labels[n_na > 0]

  cat(str_c(
    "Decomposition state report (", start.y, " -> ", end.y,
    if (is.null(domain)) "" else str_c(", ", domain),
    "):\n"
  ))

  if (all(is.na(n_grid))) {
    coverage <- "grid counts unavailable"
    dropped <- "unavailable"
  } else {
    widest <- max(n_grid, na.rm = TRUE)
    short <- !is.na(n_grid) & n_grid < widest
    dropped <- if (any(short)) {
      str_c(
        labels[short],
        " (",
        format(n_grid[short], big.mark = ","),
        ")",
        collapse = ", "
      )
    } else {
      "none"
    }
    coverage <- str_c(
      str_c(
        format(sort(unique(n_grid[!is.na(n_grid)])), big.mark = ","),
        collapse = "/"
      ),
      " grid(s) per state",
      if (is.null(grid_total)) {
        ""
      } else {
        str_c(" of ", format(grid_total, big.mark = ","), " in Grid_info")
      }
    )
  }
  cat(str_c(
    "  coverage: ", coverage,
    "; dropped vs widest state: ", dropped,
    ".\n"
  ))

  na_detail <- if (length(na_states) == 0) {
    "none"
  } else {
    str_c(
      na_states,
      " (",
      format(n_na[match(na_states, labels)], big.mark = ","),
      " cell(s))",
      collapse = ", "
    )
  }
  domains_detail <- if (length(na_domains) == 0) {
    "none"
  } else {
    shown <- head(na_domains, 6)
    str_c(
      str_c(shown, collapse = ", "),
      if (length(na_domains) > length(shown)) {
        str_c(" (+", length(na_domains) - length(shown), " more)")
      } else {
        ""
      }
    )
  }
  cat(str_c(
    "  states with NA: ", na_detail,
    "; affected domains: ", domains_detail,
    ".\n"
  ))
  invisible(NULL)
}

Decomposition <- function(serie, start.y, end.y, domain = NULL, cache = NULL) {
  validate_scenario_columns(start.y, end.y)
  if (is.null(domain)) {
    domain <- detect_domain()
  }

  prefixes <- decomp_series_prefixes(serie)
  steps <- prefixes[[5]]

  if (is.null(cache)) {
    cache <- new_decomp_cache()
  }

  # Build missing states in parallel when a plan is active; no-op otherwise.
  decomp_fill_cache(prefixes, start.y, end.y, domain, cache)

  grids <- prefixes |>
    map(
      get_decomp_state,
      start.y = start.y,
      end.y = end.y,
      domain = domain,
      cache = cache
    ) |>
    map("grid")

  # C1: a state that lost grids (NA concentration, population, rate, or PWRR)
  # would make every difference a sum over a different grid set. Abort before
  # differencing instead of returning a silently biased domain total.
  # Kept inline: the module harnesses parse a fixed list of top-level names, so
  # a new helper here would be invisible to them.
  state_labels <- c("<start>", steps)
  key_sets <- map(grids, function(grid) str_c(grid$x, grid$y, sep = "|"))
  n_keys <- map_int(key_sets, length)
  same_keys <- all(map_lgl(
    key_sets,
    function(keys) setequal(keys, key_sets[[1]])
  ))
  if (length(unique(n_keys)) > 1 || !same_keys) {
    stop(
      "Decomposition aborted: the factor states cover different (x, y) grid sets (",
      str_c(state_labels, " = ", n_keys, " grids", collapse = ", "),
      "). Differencing them would sum over different grids and bias every ",
      "domain total. Mortality() drops grids whose concentration, population, ",
      "mortality rate, or PWRR input is NA; inspect the scenario columns each ",
      "state reads."
    )
  }
  state_issues <- map(grids, function(grid) {
    mort_cols <- setdiff(names(grid), c("x", "y", domain))
    if (length(mort_cols) == 0) {
      return(list(
        n_grid = nrow(grid),
        n_na = 0L,
        na_domains = character(0)
      ))
    }
    na_cells <- is.na(as.matrix(grid[mort_cols]))
    list(
      n_grid = nrow(grid),
      n_na = sum(na_cells),
      na_domains = unique(grid[[domain]][rowSums(na_cells) > 0])
    )
  }) |>
    set_names(state_labels)

  Decomp <- grids |> set_names(str_c("Mort_", 0:4))

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

  print_decomp_drivers(
    Decomp, steps, start.y, end.y,
    issues = state_issues,
    domain = domain,
    grid_total = nrow(Grid_info)
  )

  return(Decomp)
}

# Build country-level sequence contributions from cached state summaries.
# Decomposition() keeps the grid-level output; aggregate_drivers() uses this path.
# state_summaries, when supplied, replaces the cache: it must be keyed by
# decomp_state_key() and hold one state_summary_from_grid() table per state.
# check_keys = FALSE skips the C1 grid-set guard; aggregate_drivers() runs it once
# over its 16 unique summaries and passes FALSE for all 24 series.
Decomposition_summary <- function(
  serie,
  start.y,
  end.y,
  domain = NULL,
  cache = NULL,
  state_summaries = NULL,
  report = TRUE,
  check_keys = TRUE
) {
  validate_scenario_columns(start.y, end.y)
  if (is.null(domain)) {
    domain <- detect_domain()
  }

  prefixes <- decomp_series_prefixes(serie)
  steps <- prefixes[[5]]

  if (is.null(cache)) {
    cache <- new_decomp_cache()
  }

  if (is.null(state_summaries)) {
    # Build missing states in parallel when a plan is active; no-op otherwise.
    decomp_fill_cache(prefixes, start.y, end.y, domain, cache)
    summaries <- prefixes |>
      map(
        decomp_state_summary,
        start.y = start.y,
        end.y = end.y,
        domain = domain,
        cache = cache
      )
  } else {
    keys <- prefixes |>
      map_chr(
        decomp_state_key,
        start.y = start.y,
        end.y = end.y,
        domain = domain
      )
    missing_keys <- setdiff(keys, names(state_summaries))
    if (length(missing_keys) > 0) {
      stop(
        "state_summaries lacks ",
        length(missing_keys),
        " of ",
        length(keys),
        " state(s) needed by serie ",
        serie,
        "."
      )
    }
    summaries <- state_summaries[keys]
  }

  # C1: summaries from state_summary_from_grid() carry the state's (x, y) key
  # set. Differencing states built on different grids biases every domain sum,
  # so abort before subtracting. Summaries built elsewhere cannot be checked;
  # warn instead of pretending the guard ran.
  state_labels <- c("<start>", steps)
  if (check_keys) {
    decomp_guard_summary_grids(summaries, state_labels, "Decomposition_summary")
  }

  state_issues <- NULL
  if (report) {
    state_issues <- map(summaries, function(summary) {
      is_na <- is.na(summary$Mort)
      list(
        n_grid = attr(summary, "decomp_n_grid", exact = TRUE),
        n_na = sum(is_na),
        na_domains = unique(summary[[domain]][is_na])
      )
    }) |>
      set_names(state_labels)
  }

  Decomp <- summaries |> set_names(str_c("Mort_", 0:4))

  Decomp <- Decomp |>
    imap_dfr(
      ~ .x |>
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

  print_decomp_drivers(
    Decomp, steps, start.y, end.y,
    issues = state_issues,
    domain = domain,
    grid_total = nrow(Grid_info)
  )

  Decomp
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

  # Summarise the unique factor states once and keep only their summaries
  # (I9, 261101). The old serial path cached 16 full grid tables (~117 MB)
  # although only summaries are consumed; this mirrors the parallel path.
  states <- decomp_unique_prefixes(
    start.year, end.year, domain,
    RUN24 = RUN24
  )
  kernel_on <- exists("kernel_active", mode = "function") &&
    isTRUE(kernel_active())
  cache <- new_decomp_cache()
  engine_on <- isTRUE(cache$engine)
  log_msg(
    INFO,
    if (engine_on) "" else if (kernel_on) "kernel: " else "",
    "summarising ",
    length(states),
    " unique factor state(s) for ",
    length(n),
    " sequence(s)",
    if (engine_on) {
      " on the serial input cache"
    } else if (kernel_on) {
      str_c(" on ", kernel_workers(), " worker(s)")
    } else {
      ""
    },
    "."
  )
  state_summaries <- precompute_state_summaries(
    states, start.year, end.year, domain, cache = cache
  )

  # C1 once per run: check the 16 unique summaries here, then let every
  # Decomposition_summary() call below skip the guard (check_keys = FALSE).
  # Inside a series the guard would repeat the same key-set comparison 24 times.
  state_labels <- str_remove(names(state_summaries), ".*::")
  decomp_guard_summary_grids(
    state_summaries,
    state_labels,
    "aggregate_drivers"
  )

  # One NA and dropped-grid report for the whole run (I7); the per-series
  # Decomposition_summary() calls below run with report = FALSE.
  state_issues <- map(state_summaries, function(summary) {
    is_na <- is.na(summary$Mort)
    list(
      n_grid = attr(summary, "decomp_n_grid", exact = TRUE),
      n_na = sum(is_na),
      na_domains = unique(summary[[domain]][is_na])
    )
  }) |>
    set_names(state_labels)
  print_decomp_drivers(
    Decomp = NULL,
    steps = character(0),
    start.y = start.year,
    end.y = end.year,
    issues = state_issues,
    domain = domain,
    grid_total = nrow(Grid_info),
    report_only = TRUE
  )

  ls1 <- n |>
    set_names() |>
    map(
      ~ Decomposition_summary(
        serie = .x,
        start.y = start.year,
        end.y = end.year,
        domain = domain,
        state_summaries = state_summaries,
        report = FALSE,
        check_keys = FALSE
      ) |>
        group_by(across(all_of(domain))) |>
        summarise(across(Start:End, sum))
    )

  ls1[["Summary"]] <- ls1 |>
    bind_rows() |>
    group_by(across(all_of(domain))) |>
    summarise(across(Start:End, mean))

  ls1[["Sequences"]] <- decomp_sequences

  # C2: Results/ is gitignored, so a clean checkout has no directory here.
  dir.create("./Results", showWarnings = FALSE, recursive = TRUE)
  ls1 |>
    write_xlsx(str_glue(
      "./Results/{tell_Model()}_DrivingFactors_",
      "{start.year}-{end.year}_Build{format(Sys.Date(), '%y%m%d')}.xlsx"
    ))
}
