# 1. Copyright statement -----
# Copyright (c) 2026 Yuki Furukawa. Released under the MIT License (see LICENSE).

# 2. Author -----
# Author: SWD study authors (see LICENSE)
# Date: 2026-11-14 (last revised)

# 3. File description -----
# Purpose: Multiple-imputation sensitivity analyses (eAppendix 14): impute
#          the missing SWD of the eligible starters, pool median / mean /
#          proportion accepting with Rubin's rules, with delta-adjusted and
#          best / worst-case analyses; multiple imputation of missing
#          covariates for the multivariable linear regression.
# Inputs:  data/processed/swd_individual.csv; data/raw/data_swd_{ja,de}*.xlsx
#          (newest; comprehension items, staircase answers)
# Outputs: outputs/mi_population_counts.csv, mi_stop_stages.csv,
#          mi_staircase_stopper_bounds.csv, mi_pooled_estimates.csv,
#          mi_per_imputation_medians.csv, mi_covariate_vs_complete_case.csv,
#          mi_covariate_logged_events.csv
# Depends: 00_config.R, 00c_analysis_set.R, 01a_raw_data.R,
#          05a_mi_population.R, 05b_mi_outcome.R, 05c_mi_pooling.R;
#          tidyverse, mice, sandwich
#
# Run from the project root:  Rscript R/05_multiple_imputation.R
#
# Analysis
# Population: the 276 eligible starters who were not excluded for
# misunderstanding the task (225 with an SWD, 51 without: stopped before the
# comprehension questions, failed them, gave no trade-off answer, or stopped
# part-way through the staircase). Only country and the randomized drug were
# collected before the SWD questions, so they are the only predictors.
# The SWD is imputed 50 times by predictive mean matching (5 donors, donors
# drawn from an approximate Bayesian bootstrap), so only observed 5-point
# values are imputed; respondents who stopped part-way only receive values
# compatible with their answers. Model A imputes within each country with
# the drug as predictor; Model B imputes in the pooled sample with country
# and drug as main effects; Model A is repeated without the 15 respondents
# who failed the comprehension questions. Per dataset: median (bootstrap
# variance, 500 resamples), mean, and proportion accepting the drug at its
# current efficacy (SWD <= 40, logit scale), overall and by country, and the
# Japan minus Germany mean (OLS, HC3 variance) and median differences;
# pooled with Rubin's rules (Barnard-Rubin df). Departures from missing at
# random: imputed values of non-completers shifted by delta (rounded to 5,
# kept within the admissible range), for all or only Japanese non-completers;
# best case (SWD 0 or the lower bound) and worst case (60 or the upper bound).
# Covariate MI: the 12-covariate multivariable OLS of the SWD in the analysis
# set, complete cases (n = 212) vs mice default methods (m = 50, 20
# iterations, the SWD as a predictor; n = 225), pooled with Rubin's rules.

# 4. source() and library() -----
suppressPackageStartupMessages(library(tidyverse))
source(file.path("R", "00_config.R"))
source(file.path(PATH_R, "00c_analysis_set.R"))
source(file.path(PATH_R, "01a_raw_data.R"))
source(file.path(PATH_R, "05a_mi_population.R"))
source(file.path(PATH_R, "05b_mi_outcome.R"))
source(file.path(PATH_R, "05c_mi_pooling.R"))

# Constants -----
IMPUTATIONS <- 50L
MI_SEED <- 20261113L
COVARIATE_MI_SEED <- 20261114L
COVARIATE_MI_ITERATIONS <- 20L # several covariates are jointly missing
EXPECTED_COUNTS <- c(
  population = 276L, analysed = ANALYSIS_N_TOTAL, missing = 51L,
  missing_JP = 42L, missing_DE = 9L, eligible = 288L, complete_cases = 212L,
  comprehension_failed = 15L, population_comprehension = 261L
)
LABEL_COMPLETE_CASE <- "Complete case"
LABEL_MI_COUNTRY <- "MI, imputed separately by country"
LABEL_MI_POOLED <- "MI, pooled (country + drug)"
LABEL_MI_COMPREHENSION <- "MI by country, excluding 15 comprehension-check failures"
LABEL_BEST_CASE <- "Best case: non-completers SWD = 0 (stoppers: lower bound)"
LABEL_WORST_CASE <- "Worst case: non-completers SWD = 60 (stoppers: upper bound)"

# 5. Function definitions -----
run_multiple_imputation <- function() {
  individual <- read_individual()
  analysed_frame <- make_regression_frame(individual, USABLE_SWD_STATUSES)
  eligible_starters <- build_eligible_starters(individual)
  write_output_csv(count_stop_stages(eligible_starters), "mi_stop_stages.csv")
  partial_bounds <- stopper_bounds(eligible_starters)
  population <- build_imputation_population(
    eligible_starters, analysed_frame, partial_bounds
  )
  population_comprehension <- exclude_comprehension_failures(population)
  counts <- count_population(
    population, population_comprehension, eligible_starters, analysed_frame
  )
  stopifnot(
    all(counts == EXPECTED_COUNTS[names(counts)]),
    identical(is.na(population$swd), !population$has_swd),
    nrow(partial_bounds) == sum(population$is_staircase_stopper),
    all(partial_bounds$swd_lower <= partial_bounds$swd_upper)
  )
  write_output_csv(enframe(counts, name = "count", value = "n"), "mi_population_counts.csv")
  write_output_csv(partial_bounds, "mi_staircase_stopper_bounds.csv")
  impute_and_pool_outcome(population, population_comprehension)
  impute_covariates(analysed_frame)
}

count_population <- function(population, population_comprehension,
                             eligible_starters, analysed_frame) {
  is_missing <- !population$has_swd
  c(
    population = nrow(population), analysed = sum(population$has_swd),
    missing = sum(is_missing),
    missing_JP = sum(is_missing & population$country == "JP"),
    missing_DE = sum(is_missing & population$country == "DE"),
    eligible = nrow(eligible_starters),
    complete_cases = sum(stats::complete.cases(
      analysed_frame[, c("swd", REGRESSION_COVARIATES)]
    )),
    comprehension_failed = nrow(population) - nrow(population_comprehension),
    population_comprehension = nrow(population_comprehension)
  )
}

impute_and_pool_outcome <- function(population, population_comprehension) {
  datasets_country <- impute_by_country(population, IMPUTATIONS, MI_SEED)
  datasets_pooled <- impute_pooled(population, IMPUTATIONS, MI_SEED)
  datasets_comprehension <- impute_by_country(
    population_comprehension, IMPUTATIONS, MI_SEED
  )
  assert_imputations_admissible(
    c(datasets_country, datasets_pooled, datasets_comprehension)
  )
  estimates_country <- estimate_datasets(datasets_country)
  estimates_pooled <- estimate_datasets(datasets_pooled)
  estimates_comprehension <- estimate_datasets(datasets_comprehension)
  complete_case <- population %>% filter(.data$has_swd)
  pooled <- bind_rows(
    pool_estimates(estimate_dataset(complete_case), LABEL_COMPLETE_CASE),
    pool_estimates(estimates_country, LABEL_MI_COUNTRY),
    pool_estimates(estimates_pooled, LABEL_MI_POOLED),
    pool_estimates(estimates_comprehension, LABEL_MI_COMPREHENSION),
    run_delta_analyses(datasets_country),
    pool_estimates(
      estimate_dataset(fill_non_completers(population, "swd_lower")), LABEL_BEST_CASE
    ),
    pool_estimates(
      estimate_dataset(fill_non_completers(population, "swd_upper")), LABEL_WORST_CASE
    )
  )
  per_imputation_medians <- bind_rows(
    tally_imputation_medians(estimates_country, LABEL_MI_COUNTRY),
    tally_imputation_medians(estimates_pooled, LABEL_MI_POOLED),
    tally_imputation_medians(estimates_comprehension, LABEL_MI_COMPREHENSION)
  )
  write_output_csv(pooled, "mi_pooled_estimates.csv")
  write_output_csv(per_imputation_medians, "mi_per_imputation_medians.csv")
}

# Complete-case OLS; a covariate that is constant in the complete cases
# (ever used an antipsychotic) cannot be estimated and is dropped.
fit_complete_case_lm <- function(frame, predictors) {
  model_data <- frame %>%
    select(all_of(c("swd", predictors))) %>%
    drop_na() %>%
    droplevels()
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

# mice default methods by type (pmm numeric, logreg binary factor, polyreg
# unordered factor); all covariates and the SWD enter the imputation model,
# the analysis model uses the covariates estimable in the complete cases.
impute_covariates <- function(analysed_frame) {
  covariates <- valid_regression_covariates(analysed_frame)
  complete_case_fit <- fit_complete_case_lm(analysed_frame, covariates)
  complete_case_intervals <- stats::confint(complete_case_fit, level = CONFIDENCE_LEVEL)
  complete_case <- tibble(
    term = rownames(summary(complete_case_fit)$coefficients),
    cc_estimate = summary(complete_case_fit)$coefficients[, "Estimate"],
    cc_conf_low = complete_case_intervals[, 1],
    cc_conf_high = complete_case_intervals[, 2],
    cc_p_value = summary(complete_case_fit)$coefficients[, "Pr(>|t|)"]
  )
  model_data <- analysed_frame %>%
    select(all_of(c("swd", covariates))) %>%
    droplevels() %>%
    as.data.frame()
  imputation <- mice::mice(model_data,
    m = IMPUTATIONS, maxit = COVARIATE_MI_ITERATIONS, seed = COVARIATE_MI_SEED,
    printFlag = FALSE
  )
  analysis_formula <- stats::reformulate(
    setdiff(covariates, complete_case_fit$dropped_constant),
    response = "swd"
  )
  fits <- map(seq_len(IMPUTATIONS), \(index) {
    stats::lm(analysis_formula, data = mice::complete(imputation, index))
  })
  pooled <- summary(mice::pool(mice::as.mira(fits)),
    conf.int = TRUE, conf.level = CONFIDENCE_LEVEL
  )
  comparison <- complete_case %>%
    full_join(
      tibble(
        term = as.character(pooled$term), mi_estimate = pooled$estimate,
        mi_conf_low = pooled[["2.5 %"]], mi_conf_high = pooled[["97.5 %"]],
        mi_p_value = pooled$p.value
      ),
      by = "term"
    ) %>%
    mutate(
      cc_n = stats::nobs(complete_case_fit), mi_n = nrow(analysed_frame),
      cc_dropped_constant = paste(complete_case_fit$dropped_constant, collapse = ", ")
    )
  check_logged_events(as_tibble(imputation$loggedEvents), complete_case_fit$dropped_constant)
  stopifnot(comparison$cc_n[1] == EXPECTED_COUNTS[["complete_cases"]])
  write_output_csv(as_tibble(imputation$loggedEvents), "mi_covariate_logged_events.csv")
  write_output_csv(comparison, "mi_covariate_vs_complete_case.csv")
}

# mice may only log the removal, as a predictor, of the covariate that is
# constant among complete cases (ever_used_antipsychotic: its 2 "No" answers
# have currently_using_randomized_drug missing); anything else stops.
check_logged_events <- function(events, dropped_constant) {
  if (nrow(events) == 0 || length(dropped_constant) == 0) {
    stopifnot(nrow(events) == 0)
    return(invisible(TRUE))
  }
  dropped_pattern <- paste0("^(", paste(dropped_constant, collapse = "|"), ")")
  stopifnot(all(str_detect(events$out, dropped_pattern)))
}

# 6. Executed statements -----
run_multiple_imputation()
