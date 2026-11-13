#1. Copyright statement -----
# Copyright (c) 2026 the SWD study authors. Released under the MIT License (see LICENSE).

#2. Author -----
# Author: SWD study authors (see LICENSE)
# Date: 2026-11-13 (last revised)

#3. File description -----
# Purpose: Binary SWD models (SWD > 40, never willing included): univariable
#          logistic glm() per covariate and a logistic LASSO, with odds ratios.
#          Split off from 02d_swd_linear_regression.R, which sources this file.
# Inputs:  none (works on the individual table)
# Outputs: R_SWD/data/processed/swd_regression_binary.csv
#          (written by run_swd_binary_regressions())
# Depends: 02d_swd_linear_regression.R (make_regression_frame,
#          valid_regression_covariates, has_model_variation, constants),
#          02g_swd_linear_models.R (empty_regression_row, lm_coefficient_table,
#          LASSO helpers); 00a_config.R (PATH_OUT, USABLE_SWD_STATUSES);
#          tidyverse, glmnet
# Style:   r_script_style_guide.md (Tier B)

#4. source() and library() -----
# None: sourced by 02d_swd_linear_regression.R.

# Constants -----
# SWD above the real risk difference of maintenance treatment (40 points).
BINARY_SWD_CUTOFF <- 40
BINARY_OUTCOME <- "swd_gt40"
BINARY_SPECIFICATION <- "binary_swd_gt40_including_never"

#5. Function definitions -----
empty_binary_regression_row <- function(specification, model, covariate, status,
                                        n = NA_integer_) {
  empty_regression_row(specification, model, covariate, status, n) %>%
    mutate(
      odds_ratio = NA_real_,
      or_conf_low = NA_real_,
      or_conf_high = NA_real_,
      .after = "conf_high"
    )
}

tidy_glm_coefficients <- function(fit, specification, covariate, n) {
  lm_coefficient_table(fit, stats::confint.default) %>%
    transmute(
      specification = specification,
      model = "univariable_logistic",
      covariate = covariate,
      term = .data$term,
      estimate = as.numeric(.data$estimate),
      std_error = as.numeric(.data$std_error),
      statistic = as.numeric(.data$statistic),
      p_value = as.numeric(.data$p_value),
      conf_low = as.numeric(.data$conf_low),
      conf_high = as.numeric(.data$conf_high),
      odds_ratio = exp(.data$estimate),
      or_conf_low = exp(.data$conf_low),
      or_conf_high = exp(.data$conf_high),
      n = as.integer(n),
      r_squared = NA_real_,
      lambda = NA_real_,
      status = "estimated"
    )
}

run_univariable_logistic_regression <- function(model_frame, covariate, specification) {
  model_data <- model_frame %>%
    select(all_of(c(BINARY_OUTCOME, covariate))) %>%
    drop_na()

  skip_row <- function(status) {
    empty_binary_regression_row(
      specification = specification,
      model = "univariable_logistic",
      covariate = covariate,
      status = status,
      n = nrow(model_data)
    )
  }

  has_enough_data <- nrow(model_data) >= MIN_UNIVARIABLE_N &&
    has_model_variation(model_data[[covariate]]) &&
    has_model_variation(model_data[[BINARY_OUTCOME]])
  if (!has_enough_data) {
    return(skip_row(REGRESSION_STATUS$insufficient_data))
  }

  model_data <- droplevels(model_data)

  fit <- tryCatch(
    stats::glm(
      stats::reformulate(covariate, response = BINARY_OUTCOME),
      data = model_data,
      family = stats::binomial()
    ),
    error = \(e) e
  )

  if (inherits(fit, "error")) {
    return(skip_row(paste0("skipped_glm_error: ", conditionMessage(fit))))
  }

  tidy_glm_coefficients(fit, specification, covariate, nrow(model_data))
}

run_lasso_logistic_regression <- function(model_frame, covariates, specification) {
  skip_row <- function(status, n = NA_integer_) {
    empty_binary_regression_row(specification, "lasso_logistic", NA_character_, status, n)
  }

  if (!requireNamespace("glmnet", quietly = TRUE)) {
    warning(
      "glmnet is not installed; skipping logistic LASSO for ", specification, ".",
      call. = FALSE
    )
    return(skip_row(REGRESSION_STATUS$glmnet_missing))
  }

  complete_data <- lasso_complete_cases(model_frame, BINARY_OUTCOME, covariates)
  n_complete <- nrow(complete_data)

  if (n_complete < MIN_LASSO_N || !has_model_variation(complete_data[[BINARY_OUTCOME]])) {
    warning(
      "Too few complete cases or no outcome variation for logistic LASSO in ",
      specification, "; skipping.",
      call. = FALSE
    )
    return(skip_row(REGRESSION_STATUS$too_few_complete_cases, n_complete))
  }

  covariates <- keep_varying_covariates(complete_data, covariates)

  if (length(covariates) == 0) {
    warning(
      "No varying covariates for logistic LASSO in ", specification, "; skipping.",
      call. = FALSE
    )
    return(skip_row(REGRESSION_STATUS$no_varying_covariates, n_complete))
  }

  design_matrix <- build_lasso_matrix(complete_data, covariates, BINARY_OUTCOME)

  if (ncol(design_matrix) == 0) {
    warning(
      "No usable model-matrix columns for logistic LASSO in ", specification,
      "; skipping.",
      call. = FALSE
    )
    return(skip_row(REGRESSION_STATUS$no_model_columns, n_complete))
  }

  fit <- fit_lasso(design_matrix, complete_data[[BINARY_OUTCOME]], family = "binomial")

  if (inherits(fit, "error")) {
    warning(
      "Logistic LASSO failed for ", specification, ": ", conditionMessage(fit),
      call. = FALSE
    )
    return(skip_row(REGRESSION_STATUS$lasso_error, n_complete))
  }

  selected <- select_lasso_terms(fit)

  if (nrow(selected) == 0) {
    no_terms_row <- skip_row(REGRESSION_STATUS$no_terms_selected, n_complete) %>%
      mutate(lambda = fit$lambda.min)
    return(no_terms_row)
  }

  selected %>%
    transmute(
      specification = specification,
      model = "lasso_logistic",
      covariate = NA_character_,
      term = .data$term,
      estimate = .data$estimate,
      std_error = NA_real_,
      statistic = NA_real_,
      p_value = NA_real_,
      conf_low = NA_real_,
      conf_high = NA_real_,
      odds_ratio = exp(.data$estimate),
      or_conf_low = NA_real_,
      or_conf_high = NA_real_,
      n = as.integer(n_complete),
      r_squared = NA_real_,
      lambda = fit$lambda.min,
      status = "selected"
    )
}

run_swd_binary_regressions <- function(individual) {
  model_frame <- make_regression_frame(individual, statuses = USABLE_SWD_STATUSES) %>%
    mutate(swd_gt40 = as.integer(.data$swd > BINARY_SWD_CUTOFF))

  covariates <- valid_regression_covariates(model_frame)

  univariable <- bind_rows(lapply(covariates, \(covariate) {
    run_univariable_logistic_regression(model_frame, covariate, BINARY_SPECIFICATION)
  }))

  lasso <- run_lasso_logistic_regression(model_frame, covariates, BINARY_SPECIFICATION)
  regression <- bind_rows(univariable, lasso)

  dir.create(PATH_OUT, recursive = TRUE, showWarnings = FALSE)
  write_excel_csv(regression, file.path(PATH_OUT, "swd_regression_binary.csv"), na = "")

  regression
}

#6. Executed statements -----
# None (sourced by 02d_swd_linear_regression.R).
