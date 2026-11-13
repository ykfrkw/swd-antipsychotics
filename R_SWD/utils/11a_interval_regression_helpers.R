#1. Copyright statement -----
# Copyright (c) 2026 the SWD study authors. Released under the MIT License (see LICENSE).

#2. Author -----
# Author: SWD study authors (see LICENSE)
# Date: 2026-11-13 (last revised)

#3. File description -----
# Purpose: Helper functions for 11_interval_regression.R: convert the
#          staircase SWD into censoring intervals, verify that mapping
#          against STAIRCASE_TREE, and fit / tidy Gaussian interval
#          regressions (survival::survreg).
# Inputs:  none (functions receive data frames from the caller)
# Outputs: none (function definitions)
# Depends: 00a_config.R (STAIRCASE_TREE), 02d_swd_linear_regression.R
#          (INTERCEPT_TERM via 02g), 09a_additional_descriptives_helpers.R
#          (SWD_FLOOR, SWD_CEILING, SWD_STEP, CONFIDENCE_LEVEL, describe_swd,
#          split_by_country); tidyverse, survival
# Style:   r_script_style_guide.md (Tier B)

#4. source() and library() -----
# None: the calling script loads tidyverse and sources 00a_config.R and the
# files listed under Depends.

# Constants -----
STAIRCASE_ROOT <- "0"
# Terminal nodes of STAIRCASE_TREE: 12 completed + always + never willing.
N_STAIRCASE_TERMINALS <- 14L
N_COMPLETED_TERMINALS <- 12L
INTERVAL_DISTRIBUTION <- "gaussian"
SIGNIFICANCE_THRESHOLD <- 0.05
LOG_SCALE_TERM <- "Log(scale)"

#5. Function definitions -----
# The SWD is the relapse-rate reduction from a 60% baseline. A "worthwhile"
# answer at rate r means SWD <= 60 - r; a "not worthwhile" answer at r + 5
# means SWD > 60 - r - 5. Hence true SWD lies in (s - 5, s].
derive_interval_bounds <- function(swd, swd_status) {
  is_always_willing <- swd_status == "always_willing"
  is_never_willing <- swd_status == "never_willing"
  is_completed <- swd_status == "completed"
  if (any(!(is_always_willing | is_never_willing | is_completed))) {
    stop("Interval bounds are defined only for SWD-determinable statuses.")
  }
  tibble(
    swd_lower = case_when(
      is_completed ~ swd - SWD_STEP,
      is_never_willing ~ SWD_CEILING,
      is_always_willing ~ NA_real_
    ),
    swd_upper = case_when(
      is_completed ~ swd,
      is_never_willing ~ NA_real_,
      is_always_willing ~ SWD_FLOOR
    )
  )
}

# Walks STAIRCASE_TREE and returns, per terminal node, the largest rate
# answered "worthwhile" and the smallest rate answered "not worthwhile".
collect_staircase_terminals <- function(node_name = STAIRCASE_ROOT,
                                        yes_rates = numeric(0),
                                        no_rates = numeric(0)) {
  node <- STAIRCASE_TREE[[node_name]]
  rate <- as.numeric(node_name)
  branches <- list(
    yes = list(child = node$yes, yes = c(yes_rates, rate), no = no_rates),
    no = list(child = node$no, yes = yes_rates, no = c(no_rates, rate))
  )
  bind_rows(lapply(branches, \(branch) {
    is_terminal <- is.list(branch$child)
    if (!is_terminal) {
      return(collect_staircase_terminals(branch$child, branch$yes, branch$no))
    }
    tibble(
      swd_status = branch$child$terminal,
      swd = branch$child$swd,
      max_yes_rate = if (length(branch$yes)) max(branch$yes) else NA_real_,
      min_no_rate = if (length(branch$no)) min(branch$no) else NA_real_
    )
  }))
}

# Implied bounds from the answers themselves must equal derive_interval_bounds.
verify_interval_mapping <- function() {
  terminals <- collect_staircase_terminals()
  bounds <- derive_interval_bounds(terminals$swd, terminals$swd_status)
  implied_lower <- SWD_CEILING - terminals$min_no_rate
  implied_upper <- SWD_CEILING - terminals$max_yes_rate
  is_lower_match <- coalesce(
    implied_lower == bounds$swd_lower,
    is.na(implied_lower) & is.na(bounds$swd_lower)
  )
  is_upper_match <- coalesce(
    implied_upper == bounds$swd_upper,
    is.na(implied_upper) & is.na(bounds$swd_upper)
  )
  # "never" has no "worthwhile" answer, so its upper bound is +Inf; "always"
  # has no "not worthwhile" answer, so its lower bound is -Inf.
  stopifnot(
    nrow(terminals) == N_STAIRCASE_TERMINALS,
    all(is_lower_match), all(is_upper_match),
    sum(terminals$swd_status == "completed") == N_COMPLETED_TERMINALS,
    all(sort(terminals$swd[terminals$swd_status == "completed"]) ==
      seq(SWD_STEP, SWD_CEILING, by = SWD_STEP))
  )
  invisible(terminals)
}

add_interval_bounds <- function(frame) {
  bind_cols(frame, derive_interval_bounds(frame$swd, frame$swd_status))
}

fit_interval_model <- function(model_data, covariate = NULL) {
  predictors <- if (is.null(covariate)) "1" else covariate
  model_formula <- stats::reformulate(
    predictors,
    response = quote(survival::Surv(swd_lower, swd_upper, type = "interval2"))
  )
  survival::survreg(model_formula,
    data = model_data,
    dist = INTERVAL_DISTRIBUTION
  )
}

wald_interval <- function(estimate, std_error) {
  z_critical <- stats::qnorm(1 - (1 - CONFIDENCE_LEVEL) / 2)
  list(
    low = estimate - z_critical * std_error,
    high = estimate + z_critical * std_error
  )
}

tidy_survreg_table <- function(fit) {
  coefficient_table <- summary(fit)$table
  estimates <- tibble(
    term = rownames(coefficient_table),
    estimate = coefficient_table[, "Value"],
    std_error = coefficient_table[, "Std. Error"],
    p_value = coefficient_table[, "p"]
  )
  bounds <- wald_interval(estimates$estimate, estimates$std_error)
  estimates %>%
    mutate(conf_low = bounds$low, conf_high = bounds$high)
}

count_censoring <- function(model_data) {
  c(
    n_left_censored = sum(is.na(model_data$swd_lower)),
    n_right_censored = sum(is.na(model_data$swd_upper))
  )
}

# Scale CI is built on log(scale) and back-transformed to stay positive.
summarise_intercept_model <- function(model_data, specification, country) {
  fit <- fit_interval_model(model_data)
  coefficients <- tidy_survreg_table(fit)
  intercept <- coefficients[coefficients$term == INTERCEPT_TERM, ]
  log_scale <- coefficients[coefficients$term == LOG_SCALE_TERM, ]
  log_scale_bounds <- wald_interval(log_scale$estimate, log_scale$std_error)
  observed <- describe_swd(model_data$swd)
  censoring <- count_censoring(model_data)
  tibble(
    specification = specification, country = country, n = nrow(model_data),
    n_left_censored_at_0 = censoring[["n_left_censored"]],
    n_right_censored_above_60 = censoring[["n_right_censored"]],
    interval_mean = intercept$estimate,
    interval_mean_conf_low = intercept$conf_low,
    interval_mean_conf_high = intercept$conf_high,
    interval_sd = fit$scale,
    interval_sd_conf_low = exp(log_scale_bounds$low),
    interval_sd_conf_high = exp(log_scale_bounds$high),
    observed_mean = observed$mean, observed_sd = observed$sd,
    observed_median = observed$median,
    observed_q1 = observed$q1, observed_q3 = observed$q3
  )
}

summarise_country_intercepts <- function(frame, specification) {
  subsets <- split_by_country(frame)
  bind_rows(lapply(names(subsets), \(name) {
    summarise_intercept_model(subsets[[name]], specification, name)
  }))
}

run_univariable_interval <- function(frame, covariate) {
  model_data <- frame %>%
    select(all_of(c("swd_lower", "swd_upper", covariate))) %>%
    filter(!is.na(.data[[covariate]])) %>%
    droplevels()
  fit <- fit_interval_model(model_data, covariate)
  tidy_survreg_table(fit) %>%
    filter(!(.data$term %in% c(INTERCEPT_TERM, LOG_SCALE_TERM))) %>%
    mutate(covariate = covariate, n = nrow(model_data), .before = 1)
}

classify_agreement <- function(interval_estimate, interval_p, ols_estimate,
                               ols_p) {
  is_same_direction <- sign(interval_estimate) == sign(ols_estimate)
  is_interval_significant <- interval_p < SIGNIFICANCE_THRESHOLD
  is_ols_significant <- ols_p < SIGNIFICANCE_THRESHOLD
  tibble(
    is_same_direction = is_same_direction,
    is_interval_significant = is_interval_significant,
    is_ols_significant = is_ols_significant,
    is_same_conclusion = is_same_direction &
      is_interval_significant == is_ols_significant
  )
}

compare_with_ols <- function(interval_table, ols_table) {
  ols_renamed <- ols_table %>%
    filter(.data$status == "estimated") %>%
    select(all_of(c(
      "covariate", "term", "estimate", "conf_low",
      "conf_high", "p_value", "n"
    ))) %>%
    rename_with(\(name) paste0("ols_", name), -c("covariate", "term"))
  interval_renamed <- interval_table %>%
    select(-"std_error") %>%
    rename_with(
      \(name) paste0("interval_", name),
      -c("covariate", "term")
    )
  joined <- left_join(interval_renamed, ols_renamed,
    by = c("covariate", "term")
  )
  bind_cols(joined, classify_agreement(
    joined$interval_estimate, joined$interval_p_value,
    joined$ols_estimate, joined$ols_p_value
  ))
}

#6. Executed statements -----
# None (sourced by 11_interval_regression.R).
