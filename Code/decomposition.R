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
# Requires: tidyverse, writexl. Loaded by: DrivingFactors.R.
# NOTE 261026: PWRR is computed per domain × endpoint × agegroup.
# NOTE 261027: parallel state construction via Code/kernel.R (optional).

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
    domain = domain,
    validate_keys = FALSE
  )
}

# Cache states within one decomposition run; sorted prefixes identify equivalent inputs.
new_decomp_cache <- function() {
  cache <- new.env(parent = emptyenv())
  cache$states <- new.env(parent = emptyenv())
  cache$n_mortality <- 0L
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
state_summary_from_grid <- function(mort, domain) {
  mort |>
    pivot_longer(
      cols = -all_of(c("x", "y", domain)),
      names_to = "Cause_Age",
      values_to = "Mort"
    ) |>
    group_by(across(all_of(c(domain, "Cause_Age")))) |>
    summarise(Mort = sum(Mort), .groups = "drop")
}

# One kernel task: build a state grid, then return only its small summary.
state_summary_for_prefix <- function(prefix, start.y, end.y, domain) {
  mort_at_prefix(prefix, start.y, end.y, domain) |>
    add_domain(domain = domain) |>
    state_summary_from_grid(domain = domain)
}

# Summarise the unique states once, in parallel when a kernel plan is active.
precompute_state_summaries <- function(states, start.y, end.y, domain) {
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

  mort <- decomp_state_grid(prefix, start.y, end.y, domain)
  entry <- list(grid = mort, summary = NULL)
  assign(key, entry, envir = cache$states)
  cache$n_mortality <- cache$n_mortality + 1L
  entry
}

decomp_state_summary <- function(prefix, start.y, end.y, domain, cache) {
  entry <- get_decomp_state(prefix, start.y, end.y, domain, cache)
  if (is.null(entry$summary)) {
    entry$summary <- entry$grid |>
      pivot_longer(
        cols = -all_of(c("x", "y", domain)),
        names_to = "Cause_Age",
        values_to = "Mort"
      ) |>
      group_by(across(all_of(c(domain, "Cause_Age")))) |>
      summarise(Mort = sum(Mort), .groups = "drop")
    key <- decomp_state_key(prefix, start.y, end.y, domain)
    assign(key, entry, envir = cache$states)
  }
  entry$summary
}

print_decomp_drivers <- function(Decomp, steps, start.y, end.y) {
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

  Decomp <- prefixes |>
    map(
      get_decomp_state,
      start.y = start.y,
      end.y = end.y,
      domain = domain,
      cache = cache
    ) |>
    map("grid") |>
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

  print_decomp_drivers(Decomp, steps, start.y, end.y)

  return(Decomp)
}

# Build country-level sequence contributions from cached state summaries.
# Decomposition() keeps the grid-level output; aggregate_drivers() uses this path.
# state_summaries, when supplied, replaces the cache: it must be keyed by
# decomp_state_key() and hold one state_summary_from_grid() table per state.
Decomposition_summary <- function(
  serie,
  start.y,
  end.y,
  domain = NULL,
  cache = NULL,
  state_summaries = NULL
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

  print_decomp_drivers(Decomp, steps, start.y, end.y)

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
  cache <- new_decomp_cache()

  # Parallel path: summarise the unique factor states once, then assemble the
  # sequences from those summaries. The serial cache path below stays unchanged.
  kernel_on <- exists("kernel_active", mode = "function") &&
    isTRUE(kernel_active())
  state_summaries <- NULL
  if (kernel_on) {
    states <- decomp_unique_prefixes(
      start.year, end.year, domain,
      RUN24 = RUN24
    )
    log_msg(
      INFO,
      "kernel: summarising ",
      length(states),
      " unique factor state(s) for ",
      length(n),
      " sequence(s) on ",
      kernel_workers(),
      " worker(s)."
    )
    state_summaries <- precompute_state_summaries(
      states, start.year, end.year, domain
    )
  }

  ls1 <- n |>
    set_names() |>
    map(
      ~ Decomposition_summary(
        serie = .x,
        start.y = start.year,
        end.y = end.year,
        domain = domain,
        cache = cache,
        state_summaries = state_summaries
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
