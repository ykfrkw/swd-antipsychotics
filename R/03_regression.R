# 1. Copyright statement -----
# Copyright (c) 2026 the SWD study authors. Released under the MIT License (see LICENSE).

# 2. Author -----
# Author: SWD study authors (see LICENSE)
# Date: 2026-11-14 (last revised)

# 3. File description -----
# Purpose: Correlates of the SWD (eAppendix 11 and 12): univariable linear
#          regressions and a LASSO, primary and excluding never willing
#          respondents; univariable logistic regressions and a logistic LASSO
#          of SWD > 40; Holm-adjusted p-values.
# Inputs:  data/processed/swd_individual.csv
# Outputs: outputs/regression_linear.csv, outputs/regression_logistic_swd_gt40.csv
# Depends: 00_config.R, 00c_analysis_set.R; tidyverse, glmnet
# Style:   r_script_style_guide.md (Tier B)
#
# Run from the project root:  Rscript R/03_regression.R
#
# Analysis
# Outcome: the SWD (0-60 percentage points). Covariates (REGRESSION_COVARIATES
# in 00c_analysis_set.R): sex, age band, education, employment, country, age
# at onset, family history of schizophrenia, ever used an antipsychotic,
# currently using the randomized drug, FIBSER total, number of psychiatric
# admissions and the randomized drug; ordinal scores enter as continuous
# covariates, the others as factors.
# (1) Primary: univariable ordinary least squares regression of the SWD on
#     each covariate in the analysis set (n = 225, never willing = 60;
#     available cases per covariate), t-based 95% CIs.
# (2) Sensitivity: the same excluding never willing respondents (n = 193).
# (3) Binary outcome: univariable logistic regression of SWD > 40 (the SWD
#     exceeds the real risk difference of maintenance treatment, 60% -> 20%),
#     never willing included; odds ratios with Wald 95% CIs.
# For each of (1)-(3), a LASSO enters all covariates jointly (complete cases,
# standardized, 10-fold cross-validation with a fixed seed, coefficients at
# lambda.min; gaussian or binomial). Holm's method adjusts the univariable
# p-values within each analysis over its non-reference terms.

# 4. source() and library() -----
suppressPackageStartupMessages(library(tidyverse))
source(file.path("R", "00_config.R"))
source(file.path(PATH_R, "00c_analysis_set.R"))

# Constants -----
INTERCEPT_TERM <- "(Intercept)"
# Seed value unchanged since the first run of the LASSO.
LASSO_SEED <- 20240529
LASSO_ALPHA <- 1
MAX_LASSO_FOLDS <- 10L
# SWD above the real risk difference of maintenance treatment (40 points).
BINARY_SWD_CUTOFF <- 40
BINARY_OUTCOME <- "swd_gt40"
BINARY_SPECIFICATION <- "binary_swd_gt40_including_never"

# 5. Function definitions -----
run_regressions <- function() {
  individual <- read_individual()
  linear <- bind_rows(
    fit_specification(
      make_regression_frame(individual, USABLE_SWD_STATUSES),
      PRIMARY_SPECIFICATION, "swd", "gaussian"
    ),
    fit_specification(
      make_regression_frame(individual, USABLE_SWD_STATUSES_EXCLUDING_NEVER),
      EXCLUDING_NEVER_SPECIFICATION, "swd", "gaussian"
    )
  )
  binary_frame <- make_regression_frame(individual, USABLE_SWD_STATUSES) %>%
    mutate(swd_gt40 = as.integer(.data$swd > BINARY_SWD_CUTOFF))
  logistic <- fit_specification(
    binary_frame, BINARY_SPECIFICATION, BINARY_OUTCOME, "binomial"
  ) %>%
    mutate(
      odds_ratio = exp(.data$estimate),
      or_conf_low = exp(.data$conf_low),
      or_conf_high = exp(.data$conf_high),
      .after = "conf_high"
    )
  write_output_csv(add_holm(linear), "regression_linear.csv")
  write_output_csv(add_holm(logistic), "regression_logistic_swd_gt40.csv")
}

# Univariable models for every covariate with variation, then the LASSO.
fit_specification <- function(model_frame, specification, outcome, family) {
  covariates <- valid_regression_covariates(model_frame)
  univariable <- map(covariates, \(covariate) {
    fit_univariable(model_frame, covariate, outcome, family)
  }) %>% bind_rows()
  bind_rows(univariable, fit_lasso(model_frame, covariates, outcome, family)) %>%
    mutate(specification = specification, .before = 1)
}

fit_univariable <- function(model_frame, covariate, outcome, family) {
  model_data <- model_frame %>%
    select(all_of(c(outcome, covariate))) %>%
    drop_na() %>%
    droplevels()
  model_formula <- stats::reformulate(covariate, response = outcome)
  is_linear <- family == "gaussian"
  fit <- if (is_linear) {
    stats::lm(model_formula, data = model_data)
  } else {
    stats::glm(model_formula, data = model_data, family = stats::binomial())
  }
  coefficients <- as.data.frame(summary(fit)$coefficients)
  names(coefficients) <- c("estimate", "std_error", "statistic", "p_value")
  intervals <- as.data.frame(if (is_linear) stats::confint(fit) else stats::confint.default(fit))
  names(intervals) <- c("conf_low", "conf_high")
  tibble(
    model = if (is_linear) "univariable" else "univariable_logistic",
    covariate = covariate,
    term = rownames(coefficients),
    coefficients,
    conf_low = intervals[rownames(coefficients), "conf_low"],
    conf_high = intervals[rownames(coefficients), "conf_high"],
    n = nrow(model_data),
    r_squared = if (is_linear) summary(fit)$r.squared else NA_real_,
    lambda = NA_real_,
    status = "estimated"
  ) %>%
    filter(.data$term != INTERCEPT_TERM)
}

# Complete cases of the outcome and all covariates; covariates and design
# columns that are constant in the complete cases are dropped.
fit_lasso <- function(model_frame, covariates, outcome, family) {
  complete_data <- model_frame %>%
    select(all_of(c(outcome, covariates))) %>%
    drop_na() %>%
    droplevels()
  covariates <- covariates[vapply(covariates, \(covariate) {
    has_model_variation(complete_data[[covariate]])
  }, logical(1))]
  design_matrix <- stats::model.matrix(
    stats::reformulate(covariates, response = outcome),
    data = complete_data
  )
  design_matrix <- design_matrix[, colnames(design_matrix) != INTERCEPT_TERM, drop = FALSE]
  is_varying_column <- apply(design_matrix, 2, stats::sd) > 0
  design_matrix <- design_matrix[, is_varying_column, drop = FALSE]
  outcome_values <- complete_data[[outcome]]
  set.seed(LASSO_SEED)
  fit <- glmnet::cv.glmnet(
    x = design_matrix, y = outcome_values, alpha = LASSO_ALPHA, family = family,
    nfolds = min(MAX_LASSO_FOLDS, length(outcome_values)), standardize = TRUE
  )
  coefficients <- as.matrix(stats::coef(fit, s = "lambda.min"))
  tibble(term = rownames(coefficients), estimate = as.numeric(coefficients[, 1])) %>%
    filter(.data$term != INTERCEPT_TERM, .data$estimate != 0) %>%
    mutate(
      model = if (family == "gaussian") "lasso" else "lasso_logistic",
      covariate = NA_character_, .before = 1
    ) %>%
    mutate(
      n = nrow(complete_data), lambda = fit$lambda.min, status = "selected"
    )
}

# Holm adjustment within each specification over its univariable terms
# (LASSO rows have no p-value).
add_holm <- function(regression) {
  regression %>%
    group_by(.data$specification) %>%
    mutate(p_holm = stats::p.adjust(.data$p_value, method = "holm")) %>%
    ungroup()
}

# 6. Executed statements -----
run_regressions()
