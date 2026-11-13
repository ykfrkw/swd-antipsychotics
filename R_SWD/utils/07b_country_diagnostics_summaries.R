#1. Copyright statement -----
# Copyright (c) 2026 the SWD study authors. Released under the MIT License (see LICENSE).

#2. Author -----
# Author: SWD study authors (see LICENSE)
# Date: 2026-11-13 (last revised)

#3. File description -----
# Purpose: Per-country summaries for the country diagnostics: SWD
#          distribution statistics (dispersion, normalized entropy, histogram),
#          funnel, comprehension and engagement rows, and the crux strata.
# Inputs:  none (functions receive the joined respondent table)
# Outputs: none (function definitions)
# Depends: 00a_config.R; tidyverse
# Style:   r_script_style_guide.md (Tier B)

#4. source() and library() -----
# None: the calling script loads tidyverse and sources 00a_config.R.

# Constants -----
SWD_LEVELS <- seq(0, 60, by = 5)
FOCAL_SWD <- 15
DIAGNOSTIC_COUNTRIES <- c("DE", "JP")
ACCEPTED_STATUSES <- c("completed", "always_willing")
REPORTED_STATUSES <- c(
  "completed", "always_willing", "never_willing", "incomplete",
  "no_response", "none"
)
CRUX_STATISTICS <- c("n", "pct_at_15", "pct_at_mode", "entropy", "median")

#5. Function definitions -----
# Normalized entropy: 0 = single spike, 1 = perfectly flat.
compute_normalized_entropy <- function(swd) {
  swd <- swd[!is.na(swd)]
  if (length(swd) == 0) {
    return(NA_real_)
  }
  proportions <- table(factor(swd, levels = SWD_LEVELS)) / length(swd)
  proportions <- proportions[proportions > 0]
  as.numeric(-sum(proportions * log(proportions)) / log(length(SWD_LEVELS)))
}

summarise_swd_distribution <- function(swd) {
  swd <- swd[!is.na(swd)]
  n_swd <- length(swd)
  if (n_swd == 0) {
    return(tibble(
      n = 0L, median = NA, iqr = NA, sd = NA,
      pct_at_mode = NA, pct_at_15 = NA, entropy = NA
    ))
  }
  swd_counts <- table(factor(swd, levels = SWD_LEVELS))
  tibble(
    n = n_swd,
    median = stats::median(swd),
    iqr = stats::IQR(swd),
    sd = stats::sd(swd),
    pct_at_mode = round(100 * max(swd_counts) / n_swd, 1),
    pct_at_15 = round(100 * sum(swd == FOCAL_SWD) / n_swd, 1),
    entropy = round(compute_normalized_entropy(swd), 3)
  )
}

percent_true <- function(is_true) round(100 * mean(is_true, na.rm = TRUE), 1)

format_median_iqr <- function(values) {
  sprintf(
    "%.0f [%.0f-%.0f]",
    stats::median(values, na.rm = TRUE),
    stats::quantile(values, .25, na.rm = TRUE),
    stats::quantile(values, .75, na.rm = TRUE)
  )
}

diagnostic_row <- function(metric, country_code, stratum, statistic, value) {
  tibble(
    metric = metric, country = country_code, stratum = stratum,
    statistic = statistic, value = as.character(value)
  )
}

# Crux cross-link: the usable respondents stratified by response quality.
build_quality_strata <- function(records, usable) {
  median_time <- stats::median(records$time_sum, na.rm = TRUE)
  list(
    comp_pass = filter(usable, .data$comp_all_ok),
    comp_fail = filter(usable, !.data$comp_all_ok),
    time_slow = filter(usable, .data$time_sum >= median_time),
    time_fast = filter(usable, .data$time_sum < median_time)
  )
}

summarise_country <- function(records, country_code) {
  usable <- filter(records, .data$usable)
  add_row <- function(metric, stratum, statistic, value) {
    diagnostic_row(metric, country_code, stratum, statistic, value)
  }
  n_decided <- sum(records$status %in% DETERMINABLE_STATUSES)
  n_accepted <- sum(records$status %in% ACCEPTED_STATUSES)
  dispersion <- summarise_swd_distribution(usable$swd)
  swd_counts <- table(factor(usable$swd, levels = SWD_LEVELS))
  strata <- build_quality_strata(records, usable)

  bind_rows(
    add_row("funnel", "all", "n_records", nrow(records)),
    add_row("funnel", "all", "finished_pct", percent_true(records$finished == 1)),
    add_row("funnel", "all", "entered_swd_n", sum(records$n_visited > 0, na.rm = TRUE)),
    map(REPORTED_STATUSES, \(status) {
      add_row("status", status, "n", sum(records$status == status, na.rm = TRUE))
    }),
    add_row("acceptance", "decided", "acceptance_pct", round(100 * n_accepted / n_decided, 1)),
    # Comprehension gate: CO02/CO03 are hard gates in the questionnaire, so
    # wrong answers among usable respondents should be ~0; CO01 is lenient.
    add_row("gate", "usable", "co01_wrong_n", sum(!usable$co01_ok, na.rm = TRUE)),
    add_row("gate", "usable", "co02_wrong_n", sum(!usable$co02_ok, na.rm = TRUE)),
    add_row("gate", "usable", "co03_wrong_n", sum(!usable$co03_ok, na.rm = TRUE)),
    add_row("quality", "all", "time_sum_med_iqr_s", format_median_iqr(records$time_sum)),
    add_row("quality", "all", "time_rsi_med_iqr", format_median_iqr(records$time_rsi)),
    add_row(
      "quality", "all", "missing_pct_med",
      round(stats::median(records$missing_pct, na.rm = TRUE), 1)
    ),
    add_row("comprehension", "all", "co01_pct", percent_true(records$co01_ok)),
    add_row("comprehension", "all", "co02_pct", percent_true(records$co02_ok)),
    add_row("comprehension", "all", "co03_pct", percent_true(records$co03_ok)),
    add_row("comprehension", "all", "all3_correct_pct", percent_true(records$comp_all_ok)),
    map(names(dispersion), \(statistic) {
      add_row("dispersion", "usable", statistic, dispersion[[statistic]][1])
    }),
    map(SWD_LEVELS, \(swd_level) {
      add_row(
        "histogram", "usable", paste0("swd_", swd_level),
        swd_counts[[as.character(swd_level)]]
      )
    }),
    map(names(strata), \(stratum) {
      summarise_stratum(strata[[stratum]], stratum, add_row)
    })
  )
}

summarise_stratum <- function(stratum_records, stratum, add_row) {
  stratum_distribution <- summarise_swd_distribution(stratum_records$swd)
  bind_rows(map(CRUX_STATISTICS, \(statistic) {
    add_row("crux", stratum, statistic, stratum_distribution[[statistic]][1])
  }))
}

#6. Executed statements -----
# None (sourced by 07_country_diagnostics.R).
