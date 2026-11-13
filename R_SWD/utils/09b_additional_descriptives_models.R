#1. Copyright statement -----
# Copyright (c) 2026 the SWD study authors. Released under the MIT License (see LICENSE).

#2. Author -----
# Author: SWD study authors (see LICENSE)
# Date: 2026-11-13 (last revised)

#3. File description -----
# Purpose: Missing-outcome and covariate-missingness summaries and the
#          country-adjusted / conventional multivariable OLS (complete case,
#          optional HC3 CIs) for 09_additional_descriptives.R; the complete-
#          case OLS is reused by 12c_secondary_descriptives.R. Split off from
#          09a_additional_descriptives_helpers.R, which sources this file.
# Inputs:  none (functions receive data frames from the caller)
# Outputs: none (function definitions)
# Depends: 09a_additional_descriptives_helpers.R (split_by_country,
#          ALL_COUNTRIES_LABEL, CONFIDENCE_LEVEL), 02d_swd_linear_regression.R
#          (has_model_variation, tidy_lm_coefficients); tidyverse;
#          sandwich (optional, for HC3 CIs)
# Style:   r_script_style_guide.md (Tier B)

#4. source() and library() -----
# None: sourced by 09a_additional_descriptives_helpers.R; the calling script
# loads tidyverse and sources 00a_config.R.

# Constants -----
MIN_EXPECTED_FOR_CHISQ <- 5
COMPLETE_CASES_LABEL <- "complete_cases_all_12_covariates"

#5. Function definitions -----
test_association <- function(group, has_swd) {
  contingency <- table(group, has_swd)
  if (nrow(contingency) < 2 || ncol(contingency) < 2) {
    return(list(test = NA_character_, p_value = NA_real_))
  }
  expected <- suppressWarnings(stats::chisq.test(contingency)$expected)
  if (any(expected < MIN_EXPECTED_FOR_CHISQ)) {
    return(list(
      test = "Fisher exact",
      p_value = stats::fisher.test(contingency)$p.value
    ))
  }
  list(
    test = "Pearson chi-square",
    p_value = stats::chisq.test(contingency, correct = FALSE)$p.value
  )
}

summarise_outcome_by <- function(starters, variable) {
  is_overall <- is.null(variable)
  group <- if (is_overall) {
    rep(ALL_COUNTRIES_LABEL, nrow(starters))
  } else {
    as.character(starters[[variable]])
  }
  association <- if (is_overall) {
    list(test = NA_character_, p_value = NA_real_)
  } else {
    test_association(group, starters$has_swd)
  }
  counts <- starters %>%
    mutate(level = group) %>%
    group_by(.data$level) %>%
    summarise(
      n_total = n(),
      n_with_swd = sum(.data$has_swd),
      n_without_swd = sum(!.data$has_swd),
      .groups = "drop"
    )
  counts %>%
    mutate(
      section = "missing_by_group",
      variable = if (is_overall) "overall" else variable,
      pct_without_swd = 100 * .data$n_without_swd / .data$n_total,
      test = association$test,
      p_value = association$p_value,
      .before = 1
    )
}

summarise_stop_stage <- function(starters) {
  non_completers <- filter(starters, !.data$has_swd)
  stage_counts <- lapply(split_by_country(non_completers), \(country_subset) {
    count(country_subset, level = .data$stop_stage, name = "n_without_swd")
  })
  bind_rows(stage_counts, .id = "variable") %>%
    mutate(
      section = "stop_stage_non_completers",
      variable = paste0("country=", .data$variable), .before = 1
    )
}

summarise_missing_covariates <- function(frame, covariates, specification) {
  subsets <- split_by_country(frame)
  per_covariate <- bind_rows(lapply(names(subsets), \(name) {
    country_subset <- subsets[[name]]
    tibble(
      specification = specification, country = name, covariate = covariates,
      n = nrow(country_subset),
      n_missing = vapply(covariates, \(covariate) {
        sum(is.na(country_subset[[covariate]]))
      }, integer(1))
    )
  }))
  complete_cases <- bind_rows(lapply(names(subsets), \(name) {
    model_columns <- subsets[[name]][, c("swd", covariates)]
    n_complete <- sum(stats::complete.cases(model_columns))
    tibble(
      specification = specification, country = name,
      covariate = COMPLETE_CASES_LABEL, n = nrow(model_columns),
      n_missing = as.integer(nrow(model_columns) - n_complete)
    )
  }))
  bind_rows(per_covariate, complete_cases) %>%
    mutate(
      n_available = .data$n - .data$n_missing,
      pct_missing = 100 * .data$n_missing / .data$n
    )
}

fit_complete_case_lm <- function(frame, predictors) {
  model_data <- frame %>%
    select(all_of(c("swd", predictors))) %>%
    drop_na() %>%
    droplevels()
  # Mirrors run_lasso_regression(): a covariate that becomes constant in the
  # complete cases cannot be estimated and is dropped (and reported).
  is_varying <- vapply(predictors, \(predictor) {
    has_model_variation(model_data[[predictor]])
  }, logical(1))
  fit <- stats::lm(
    stats::reformulate(predictors[is_varying], response = "swd"),
    data = model_data
  )
  fit$dropped_constant <- predictors[!is_varying]
  fit
}

tidy_ols <- function(fit, specification, model_label, covariate) {
  dropped_note <- if (length(fit$dropped_constant) == 0) {
    NA_character_
  } else {
    paste0(
      "dropped (constant in complete cases): ",
      paste(fit$dropped_constant, collapse = ", ")
    )
  }
  tidy_lm_coefficients(fit, specification, covariate, stats::nobs(fit)) %>%
    mutate(model = model_label, note = dropped_note)
}

add_hc3_intervals <- function(tidy_rows, fit) {
  if (!requireNamespace("sandwich", quietly = TRUE)) {
    return(tidy_rows)
  }
  robust_se <- sqrt(diag(sandwich::vcovHC(fit, type = "HC3")))
  critical <- stats::qt(1 - (1 - CONFIDENCE_LEVEL) / 2, df = fit$df.residual)
  tidy_rows %>%
    mutate(
      se_hc3 = unname(robust_se[.data$term]),
      conf_low_hc3 = .data$estimate - critical * .data$se_hc3,
      conf_high_hc3 = .data$estimate + critical * .data$se_hc3,
      p_value_hc3 = 2 * stats::pt(-abs(.data$estimate / .data$se_hc3),
        df = fit$df.residual
      )
    )
}

run_country_adjusted_models <- function(frame, covariates, specification) {
  univariable <- bind_rows(lapply(covariates, \(covariate) {
    predictors <- unique(c(covariate, "country"))
    fit <- fit_complete_case_lm(frame, predictors)
    tidy_ols(fit, specification, "univariable_country_adjusted", covariate) %>%
      filter(covariate == "country" | !startsWith(.data$term, "country"))
  }))
  multivariable_fit <- fit_complete_case_lm(frame, covariates)
  multivariable <- multivariable_fit %>%
    tidy_ols(specification, "multivariable_ols", "all") %>%
    add_hc3_intervals(multivariable_fit)
  bind_rows(univariable, multivariable)
}

#6. Executed statements -----
# None (sourced by 09a_additional_descriptives_helpers.R).
