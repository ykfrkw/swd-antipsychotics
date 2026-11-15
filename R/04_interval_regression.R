# 1. Copyright statement -----
# Copyright (c) 2026 Yuki Furukawa. Released under the MIT License (see LICENSE).

# 2. Author -----
# Author: SWD study authors (see LICENSE)
# Date: 2026-11-14 (last revised)

# 3. File description -----
# Purpose: Sensitivity analysis treating the staircase SWD as interval-
#          censored: Gaussian interval regressions, intercept-only (overall
#          and by country, also excluding never willing respondents) and
#          univariable per covariate, next to the univariable OLS estimates.
# Inputs:  data/processed/swd_individual.csv
# Outputs: outputs/interval_regression_intercepts.csv,
#          outputs/interval_regression_univariable.csv
# Depends: 00_config.R, 00c_analysis_set.R; tidyverse, survival
#
# Run from the project root:  Rscript R/04_interval_regression.R
#
# Analysis
# The staircase locates each SWD only to a 5-point interval: a completed
# staircase with SWD s means the true SWD lies in (s - 5, s]; always willing
# respondents are left-censored at 0 and never willing respondents right-
# censored above 60. Gaussian interval regressions
# (survival::survreg(Surv(lower, upper, type = "interval2"))) estimate the
# mean SWD with Wald 95% CIs; the SD is back-transformed from log(scale).
# Univariable interval regressions use the covariates of 03_regression.R
# in the primary analysis set (available cases per covariate) and are
# compared with the univariable OLS in direction and significance at p < 0.05.
# Not reported.

# 4. source() and library() -----
suppressPackageStartupMessages(library(tidyverse))
source(file.path("R", "00_config.R"))
source(file.path(PATH_R, "00c_analysis_set.R"))

# Constants -----
INTERCEPT_TERM <- "(Intercept)"
LOG_SCALE_TERM <- "Log(scale)"
STAIRCASE_ROOT <- "0"
# Terminal nodes of STAIRCASE_TREE: 12 completed + always + never willing.
N_STAIRCASE_TERMINALS <- 14L
SIGNIFICANCE_THRESHOLD <- 0.05

# 5. Function definitions -----
run_interval_regression <- function() {
  verify_interval_mapping()
  individual <- read_individual()
  primary <- add_interval_bounds(make_regression_frame(individual, USABLE_SWD_STATUSES))
  excluding_never <- add_interval_bounds(
    make_regression_frame(individual, USABLE_SWD_STATUSES_EXCLUDING_NEVER)
  )
  check_analysis_set_size(primary)
  intercepts <- bind_rows(
    summarise_intercepts(primary, PRIMARY_SPECIFICATION),
    summarise_intercepts(excluding_never, EXCLUDING_NEVER_SPECIFICATION)
  )
  covariates <- valid_regression_covariates(primary)
  univariable <- map(covariates, \(covariate) fit_univariable_pair(primary, covariate)) %>%
    bind_rows()
  write_output_csv(intercepts, "interval_regression_intercepts.csv")
  write_output_csv(univariable, "interval_regression_univariable.csv")
}

# A "worthwhile" answer at relapse rate r means SWD <= 60 - r; a "not
# worthwhile" answer at r + 5 means SWD > 60 - r - 5.
derive_interval_bounds <- function(swd, swd_status) {
  stopifnot(all(swd_status %in% USABLE_SWD_STATUSES))
  tibble(
    swd_lower = case_when(
      swd_status == "completed" ~ swd - SWD_STEP,
      swd_status == "never_willing" ~ SWD_CEILING,
      swd_status == "always_willing" ~ NA_real_
    ),
    swd_upper = case_when(
      swd_status == "completed" ~ swd,
      swd_status == "never_willing" ~ NA_real_,
      swd_status == "always_willing" ~ SWD_FLOOR
    )
  )
}

# Walks STAIRCASE_TREE: per terminal node, the largest rate answered
# "worthwhile" and the smallest rate answered "not worthwhile".
collect_staircase_terminals <- function(node_name = STAIRCASE_ROOT,
                                        yes_rates = numeric(0),
                                        no_rates = numeric(0)) {
  node <- STAIRCASE_TREE[[node_name]]
  rate <- as.numeric(node_name)
  branches <- list(
    list(child = node$yes, yes = c(yes_rates, rate), no = no_rates),
    list(child = node$no, yes = yes_rates, no = c(no_rates, rate))
  )
  map(branches, \(branch) {
    if (!is.list(branch$child)) {
      return(collect_staircase_terminals(branch$child, branch$yes, branch$no))
    }
    tibble(
      swd_status = branch$child$terminal, swd = branch$child$swd,
      max_yes_rate = if (length(branch$yes)) max(branch$yes) else NA_real_,
      min_no_rate = if (length(branch$no)) min(branch$no) else NA_real_
    )
  }) %>% bind_rows()
}

# The bounds implied by the answers must equal derive_interval_bounds().
verify_interval_mapping <- function() {
  terminals <- collect_staircase_terminals()
  bounds <- derive_interval_bounds(terminals$swd, terminals$swd_status)
  is_same <- function(implied, bound) coalesce(implied == bound, is.na(implied) & is.na(bound))
  stopifnot(
    nrow(terminals) == N_STAIRCASE_TERMINALS,
    all(is_same(SWD_CEILING - terminals$min_no_rate, bounds$swd_lower)),
    all(is_same(SWD_CEILING - terminals$max_yes_rate, bounds$swd_upper)),
    all(sort(terminals$swd[terminals$swd_status == "completed"]) ==
          seq(SWD_STEP, SWD_CEILING, by = SWD_STEP))
  )
}

add_interval_bounds <- function(frame) {
  bind_cols(frame, derive_interval_bounds(frame$swd, frame$swd_status))
}

fit_interval_model <- function(model_data, predictor = "1") {
  model_formula <- stats::reformulate(
    predictor,
    response = quote(survival::Surv(swd_lower, swd_upper, type = "interval2"))
  )
  survival::survreg(model_formula, data = model_data, dist = "gaussian")
}

tidy_survreg <- function(fit) {
  coefficient_table <- summary(fit)$table
  z_critical <- stats::qnorm(1 - (1 - CONFIDENCE_LEVEL) / 2)
  tibble(
    term = rownames(coefficient_table),
    estimate = coefficient_table[, "Value"],
    std_error = coefficient_table[, "Std. Error"],
    p_value = coefficient_table[, "p"]
  ) %>%
    mutate(
      conf_low = .data$estimate - z_critical * .data$std_error,
      conf_high = .data$estimate + z_critical * .data$std_error
    )
}

summarise_intercepts <- function(frame, specification) {
  imap(split_by_country(frame), \(model_data, country) {
    fit <- fit_interval_model(model_data)
    coefficients <- tidy_survreg(fit)
    intercept <- coefficients[coefficients$term == INTERCEPT_TERM, ]
    log_scale <- coefficients[coefficients$term == LOG_SCALE_TERM, ]
    quartiles <- stats::quantile(model_data$swd, c(0.25, 0.75), names = FALSE)
    tibble(
      specification = specification, country = country, n = nrow(model_data),
      n_left_censored_at_0 = sum(is.na(model_data$swd_lower)),
      n_right_censored_above_60 = sum(is.na(model_data$swd_upper)),
      interval_mean = intercept$estimate,
      interval_mean_conf_low = intercept$conf_low,
      interval_mean_conf_high = intercept$conf_high,
      interval_sd = fit$scale,
      interval_sd_conf_low = exp(log_scale$conf_low),
      interval_sd_conf_high = exp(log_scale$conf_high),
      observed_mean = mean(model_data$swd), observed_sd = stats::sd(model_data$swd),
      observed_median = stats::median(model_data$swd),
      observed_q1 = quartiles[1], observed_q3 = quartiles[2]
    )
  }) %>% bind_rows()
}

# Interval regression and OLS of the same covariate on the same respondents.
fit_univariable_pair <- function(frame, covariate) {
  model_data <- frame %>%
    select(all_of(c("swd", "swd_lower", "swd_upper", covariate))) %>%
    filter(!is.na(.data[[covariate]])) %>%
    droplevels()
  interval <- tidy_survreg(fit_interval_model(model_data, covariate)) %>%
    filter(!(.data$term %in% c(INTERCEPT_TERM, LOG_SCALE_TERM))) %>%
    select(-"std_error")
  ols_fit <- stats::lm(stats::reformulate(covariate, response = "swd"), data = model_data)
  ols_intervals <- stats::confint(ols_fit)
  ols <- tibble(
    term = rownames(summary(ols_fit)$coefficients),
    ols_estimate = summary(ols_fit)$coefficients[, "Estimate"],
    ols_conf_low = ols_intervals[, 1], ols_conf_high = ols_intervals[, 2],
    ols_p_value = summary(ols_fit)$coefficients[, "Pr(>|t|)"]
  )
  interval %>%
    rename_with(\(name) paste0("interval_", name), -"term") %>%
    left_join(ols, by = "term") %>%
    mutate(
      covariate = covariate, n = nrow(model_data), .before = 1
    ) %>%
    mutate(
      is_same_direction = sign(.data$interval_estimate) == sign(.data$ols_estimate),
      is_same_conclusion = .data$is_same_direction &
        (.data$interval_p_value < SIGNIFICANCE_THRESHOLD) ==
          (.data$ols_p_value < SIGNIFICANCE_THRESHOLD)
    )
}

# 6. Executed statements -----
run_interval_regression()
