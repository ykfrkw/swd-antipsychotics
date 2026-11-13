#1. Copyright statement -----
# Copyright (c) 2026 the SWD study authors. Released under the MIT License (see LICENSE).

#2. Author -----
# Author: SWD study authors (see LICENSE)
# Date: 2026-11-13 (last revised)

#3. File description -----
# Purpose: Linear SWD models: univariable lm() per covariate and a LASSO
#          (cv.glmnet, gaussian) on all covariates, returned as tidy rows.
#          Also the LASSO helpers shared with the logistic models (02e).
#          Split off from 02d_swd_linear_regression.R, which sources this file.
# Inputs:  none (works on the regression frame)
# Outputs: none (function definitions)
# Depends: 02d_swd_linear_regression.R (has_model_variation, MIN_*, LASSO_*,
#          REGRESSION_STATUS); tidyverse, glmnet
# Style:   r_script_style_guide.md (Tier B)

#4. source() and library() -----
# None: sourced by 02d_swd_linear_regression.R.

# Constants -----
INTERCEPT_TERM <- "(Intercept)"

#5. Function definitions -----
empty_regression_row <- function(specification, model, covariate, status, n = NA_integer_) {
  tibble(
    specification = specification,
    model = model,
    covariate = covariate,
    term = NA_character_,
    estimate = NA_real_,
    std_error = NA_real_,
    statistic = NA_real_,
    p_value = NA_real_,
    conf_low = NA_real_,
    conf_high = NA_real_,
    n = as.integer(n),
    r_squared = NA_real_,
    lambda = NA_real_,
    status = status
  )
}

# Coefficient table without the intercept: estimate, std_error, statistic,
# p_value, conf_low, conf_high (NA when the interval cannot be computed).
lm_coefficient_table <- function(fit, confint_function) {
  coefficients <- as.data.frame(summary(fit)$coefficients)
  coefficients$term <- rownames(coefficients)
  names(coefficients)[seq_len(4)] <- c("estimate", "std_error", "statistic", "p_value")
  n_terms <- nrow(coefficients)

  confidence <- tryCatch(
    as.data.frame(confint_function(fit)),
    error = \(e) {
      tibble(`2.5 %` = rep(NA_real_, n_terms), `97.5 %` = rep(NA_real_, n_terms))
    }
  )
  confidence$term <- rownames(confidence)
  names(confidence)[seq_len(2)] <- c("conf_low", "conf_high")

  coefficients %>%
    as_tibble() %>%
    left_join(as_tibble(confidence), by = "term") %>%
    filter(.data$term != INTERCEPT_TERM)
}

tidy_lm_coefficients <- function(fit, specification, covariate, n) {
  lm_coefficient_table(fit, stats::confint) %>%
    transmute(
      specification = specification,
      model = "univariable",
      covariate = covariate,
      term = .data$term,
      estimate = as.numeric(.data$estimate),
      std_error = as.numeric(.data$std_error),
      statistic = as.numeric(.data$statistic),
      p_value = as.numeric(.data$p_value),
      conf_low = as.numeric(.data$conf_low),
      conf_high = as.numeric(.data$conf_high),
      n = as.integer(n),
      r_squared = as.numeric(summary(fit)$r.squared),
      lambda = NA_real_,
      status = "estimated"
    )
}

run_univariable_regression <- function(model_frame, covariate, specification) {
  model_data <- model_frame %>%
    select(all_of(c("swd", covariate))) %>%
    drop_na()

  skip_row <- function(status) {
    empty_regression_row(
      specification = specification,
      model = "univariable",
      covariate = covariate,
      status = status,
      n = nrow(model_data)
    )
  }

  if (nrow(model_data) < MIN_UNIVARIABLE_N || !has_model_variation(model_data[[covariate]])) {
    return(skip_row(REGRESSION_STATUS$insufficient_data))
  }

  model_data <- droplevels(model_data)

  fit <- tryCatch(
    stats::lm(stats::reformulate(covariate, response = "swd"), data = model_data),
    error = \(e) e
  )

  if (inherits(fit, "error")) {
    return(skip_row(paste0("skipped_lm_error: ", conditionMessage(fit))))
  }

  tidy_lm_coefficients(fit, specification, covariate, nrow(model_data))
}

# Complete cases of the response and covariates, unused factor levels dropped.
lasso_complete_cases <- function(model_frame, response, covariates) {
  model_frame %>%
    select(all_of(c(response, covariates))) %>%
    drop_na() %>%
    droplevels()
}

keep_varying_covariates <- function(complete_data, covariates) {
  covariates[vapply(covariates, \(covariate) {
    has_model_variation(complete_data[[covariate]])
  }, logical(1))]
}

# Model matrix without the intercept and without constant columns.
build_lasso_matrix <- function(complete_data, covariates, response) {
  design_matrix <- stats::model.matrix(
    stats::reformulate(covariates, response = response),
    data = complete_data
  )
  design_matrix <- design_matrix[, colnames(design_matrix) != INTERCEPT_TERM, drop = FALSE]
  is_varying_column <- vapply(
    seq_len(ncol(design_matrix)),
    \(column_idx) stats::sd(design_matrix[, column_idx]) > 0,
    logical(1)
  )
  design_matrix[, is_varying_column, drop = FALSE]
}

fit_lasso <- function(design_matrix, outcome, family) {
  n_folds <- min(MAX_LASSO_FOLDS, length(outcome))
  set.seed(LASSO_SEED)
  tryCatch(
    glmnet::cv.glmnet(
      x = design_matrix,
      y = outcome,
      alpha = LASSO_ALPHA,
      family = family,
      nfolds = n_folds,
      standardize = TRUE
    ),
    error = \(e) e
  )
}

# Non-zero coefficients at lambda.min, intercept excluded.
select_lasso_terms <- function(fit) {
  coefficients <- as.matrix(stats::coef(fit, s = "lambda.min"))
  tibble(
    term = rownames(coefficients),
    estimate = as.numeric(coefficients[, 1])
  ) %>%
    filter(.data$term != INTERCEPT_TERM, .data$estimate != 0)
}

run_lasso_regression <- function(model_frame, covariates, specification) {
  skip_row <- function(status, n = NA_integer_) {
    empty_regression_row(specification, "lasso", NA_character_, status, n)
  }

  if (!requireNamespace("glmnet", quietly = TRUE)) {
    warning("glmnet is not installed; skipping LASSO for ", specification, ".", call. = FALSE)
    return(skip_row(REGRESSION_STATUS$glmnet_missing))
  }

  complete_data <- lasso_complete_cases(model_frame, "swd", covariates)
  n_complete <- nrow(complete_data)

  if (n_complete < MIN_LASSO_N) {
    warning("Too few complete cases for LASSO in ", specification, "; skipping.", call. = FALSE)
    return(skip_row(REGRESSION_STATUS$too_few_complete_cases, n_complete))
  }

  covariates <- keep_varying_covariates(complete_data, covariates)

  if (length(covariates) == 0) {
    warning("No varying covariates for LASSO in ", specification, "; skipping.", call. = FALSE)
    return(skip_row(REGRESSION_STATUS$no_varying_covariates, n_complete))
  }

  design_matrix <- build_lasso_matrix(complete_data, covariates, "swd")

  if (ncol(design_matrix) == 0) {
    warning(
      "No usable model-matrix columns for LASSO in ", specification, "; skipping.",
      call. = FALSE
    )
    return(skip_row(REGRESSION_STATUS$no_model_columns, n_complete))
  }

  fit <- fit_lasso(design_matrix, complete_data$swd, family = "gaussian")

  if (inherits(fit, "error")) {
    warning("LASSO failed for ", specification, ": ", conditionMessage(fit), call. = FALSE)
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
      model = "lasso",
      covariate = NA_character_,
      term = .data$term,
      estimate = .data$estimate,
      std_error = NA_real_,
      statistic = NA_real_,
      p_value = NA_real_,
      conf_low = NA_real_,
      conf_high = NA_real_,
      n = as.integer(n_complete),
      r_squared = NA_real_,
      lambda = fit$lambda.min,
      status = "selected"
    )
}

#6. Executed statements -----
# None (sourced by 02d_swd_linear_regression.R).
