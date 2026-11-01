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
# Modified 261101: grid-set guard before differencing (C1); one concise NA and
#   dropped-grid report per run (I7); decomp_sequences validated at load (I8);
#   aggregate_drivers() caches state summaries only (I9) and creates Results/
#   before writing (C2).

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
# The summary carries the state's grid count and (x, y) key set, so
# Decomposition_summary() can abort when states cover different grids (C1).
# NA stays NA: the conservation identity needs NA propagation, not na.rm.
state_summary_from_grid <- function(mort, domain) {
  summary <- mort |>
    pivot_longer(
      cols = -all_of(c("x", "y", domain)),
      names_to = "Cause_Age",
      values_to = "Mort"
    ) |>
    group_by(across(all_of(c(domain, "Cause_Age")))) |>
    summarise(Mort = sum(Mort), .groups = "drop")
  attr(summary, "decomp_n_grid") <- nrow(mort)
  attr(summary, "decomp_grid_keys") <- str_c(mort$x, mort$y, sep = "|")
  summary
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
Decomposition_summary <- function(
  serie,
  start.y,
  end.y,
  domain = NULL,
  cache = NULL,
  state_summaries = NULL,
  report = TRUE
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
  key_sets <- map(summaries, function(summary) {
    attr(summary, "decomp_grid_keys", exact = TRUE)
  })
  if (all(map_lgl(key_sets, is.character))) {
    n_keys <- map_int(key_sets, length)
    same_keys <- all(map_lgl(
      key_sets,
      function(keys) setequal(keys, key_sets[[1]])
    ))
    if (length(unique(n_keys)) > 1 || !same_keys) {
      stop(
        "Decomposition_summary aborted: the factor states cover different ",
        "(x, y) grid sets (",
        str_c(state_labels, " = ", n_keys, " grids", collapse = ", "),
        "). Differencing them would sum over different grids and bias every ",
        "domain total. Rebuild the state summaries with ",
        "state_summary_from_grid() from one grid set."
      )
    }
  } else {
    log_msg(
      WARN,
      "Decomposition_summary: state summaries carry no grid key set; the ",
      "grid-set guard is skipped. Build summaries with ",
      "state_summary_from_grid() to enable it."
    )
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
  log_msg(
    INFO,
    if (kernel_on) "kernel: " else "",
    "summarising ",
    length(states),
    " unique factor state(s) for ",
    length(n),
    " sequence(s)",
    if (kernel_on) str_c(" on ", kernel_workers(), " worker(s)") else "",
    "."
  )
  state_summaries <- precompute_state_summaries(
    states, start.year, end.year, domain
  )

  # One NA and dropped-grid report for the whole run (I7); the per-series
  # Decomposition_summary() calls below run with report = FALSE.
  state_labels <- str_remove(names(state_summaries), ".*::")
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
        report = FALSE
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
