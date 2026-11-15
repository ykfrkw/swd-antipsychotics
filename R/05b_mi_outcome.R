# 1. Copyright statement -----
# Copyright (c) 2026 Yuki Furukawa. Released under the MIT License (see LICENSE).

# 2. Author -----
# Author: SWD study authors (see LICENSE)
# Date: 2026-11-14 (last revised)

# 3. File description -----
# Purpose: Outcome imputation for 05_multiple_imputation.R: bounded
#          predictive mean matching through mice (Model A by country, Model B
#          pooled with country + drug), the comprehension-failure sensitivity
#          population, delta adjustment and the best / worst cases.
# Inputs:  none (functions receive the imputation population)
# Outputs: none (function definitions)
# Depends: 00_config.R (SWD_STEP), 05a_mi_population.R
#          (STOP_STAGE_FAILED_CHECK); tidyverse, mice

# 4. source() and library() -----
# None: the calling script loads tidyverse and sources 00_config.R.

# Constants -----
PMM_DONORS <- 5L
IMPUTATION_ITERATIONS <- 1L # only the outcome is incomplete

# 5. Function definitions -----
# Bounded predictive mean matching with bootstrap of the observed cases:
# parameter uncertainty is propagated by refitting the linear model on a
# bootstrap sample of the observed cases; each missing value is then drawn
# from the k donors (of that bootstrap sample) whose predicted means are
# closest, restricted to donors whose observed SWD lies in the respondent's
# admissible set. Called by mice() via method = "pmmbounded".
mice.impute.pmmbounded <- function(y, ry, x, wy = NULL, swd_lower, swd_upper,
                                   donors = PMM_DONORS, ...) {
  if (is.null(wy)) wy <- !ry
  design <- cbind(1, as.matrix(x))
  observed_index <- which(ry)
  bootstrap_index <- sample(observed_index, replace = TRUE)
  fit <- stats::lm.fit(
    design[bootstrap_index, , drop = FALSE],
    y[bootstrap_index]
  )
  coefficients <- fit$coefficients
  coefficients[is.na(coefficients)] <- 0
  donor_mean <- drop(design[bootstrap_index, , drop = FALSE] %*% coefficients)
  donor_value <- y[bootstrap_index]
  target_index <- which(wy)
  vapply(target_index, \(row) {
    is_admissible <- donor_value >= swd_lower[row] &
      donor_value <= swd_upper[row]
    if (!any(is_admissible)) {
      stop("No admissible donor for row ", row, "; widen the donor pool.")
    }
    distance <- abs(donor_mean[is_admissible] -
                      drop(design[row, ] %*% coefficients))
    # Random tie-breaking: predictors are cell indicators, so ties are common.
    nearest <- order(distance, stats::runif(length(distance)))
    n_pool <- min(donors, length(nearest))
    donor_pool <- donor_value[is_admissible][nearest][seq_len(n_pool)]
    donor_pool[sample.int(length(donor_pool), 1L)]
  }, numeric(1))
}

run_bounded_mice <- function(population, predictors, imputations, seed) {
  imputation_data <- population %>%
    select(all_of(c("swd", predictors))) %>%
    as.data.frame()
  imputation_methods <- set_names(
    rep("", ncol(imputation_data)), names(imputation_data)
  )
  imputation_methods[["swd"]] <- "pmmbounded"
  imputation <- mice::mice(
    imputation_data,
    m = imputations, method = imputation_methods,
    maxit = IMPUTATION_ITERATIONS, seed = seed, printFlag = FALSE,
    blots = list(swd = list(
      swd_lower = population$swd_lower,
      swd_upper = population$swd_upper
    ))
  )
  map(seq_len(imputations), \(index) {
    completed <- mice::complete(imputation, action = index)
    population %>% mutate(swd = completed$swd)
  })
}

# Model A: separate imputation within each country (predictor: drug arm);
# the two country-specific completed datasets are stacked per imputation.
impute_by_country <- function(population, imputations, seed) {
  populations_by_country <- split(population, population$country)
  by_country <- map(populations_by_country, \(country_population) {
    run_bounded_mice(
      country_population, c("drug_ari", "drug_ola"), imputations, seed
    )
  })
  map(seq_len(imputations), \(index) {
    bind_rows(map(by_country, \(datasets) datasets[[index]]))
  })
}

# Model B: pooled imputation with main effects country + drug (no
# interaction, which would saturate the model and reproduce Model A), so the
# drug effect borrows strength across countries.
impute_pooled <- function(population, imputations, seed) {
  run_bounded_mice(
    population, c("is_japan", "drug_ari", "drug_ola"),
    imputations, seed
  )
}

# Sensitivity population: drop the non-completers who answered the
# comprehension check but got >= 1 item wrong (stopped at the gate).
exclude_comprehension_failures <- function(population) {
  population %>%
    filter(.data$stop_stage != STOP_STAGE_FAILED_CHECK)
}

assert_imputations_admissible <- function(datasets) {
  walk(datasets, \(completed) {
    stopifnot(
      !anyNA(completed$swd),
      all(completed$swd %% SWD_STEP == 0),
      all(completed$swd >= completed$swd_lower),
      all(completed$swd <= completed$swd_upper)
    )
  })
}

# Delta adjustment (MNAR): shift imputed values of non-completers, round to
# the 5-point grid, truncate to [0, 60] and to the admissible set. Rounding
# is half up because round() rounds half to even, which would send a
# midpoint up or down depending on the grid value.
apply_delta <- function(datasets, delta, is_shifted_country) {
  map(datasets, \(completed) {
    is_shifted <- !completed$has_swd & is_shifted_country(completed$country)
    shifted <- SWD_STEP * floor((completed$swd + delta) / SWD_STEP + 0.5)
    shifted <- pmin(pmax(shifted, completed$swd_lower), completed$swd_upper)
    completed %>% mutate(swd = if_else(is_shifted, shifted, .data$swd))
  })
}

# Best / worst case within each non-completer's admissible set: the bound
# is SWD_FLOOR / SWD_CEILING except for the staircase stoppers, whose
# partial answers bound their SWD (columns swd_lower / swd_upper).
fill_non_completers <- function(population, bound_column) {
  population %>%
    mutate(swd = if_else(.data$has_swd, .data$swd, .data[[bound_column]]))
}

# 6. Executed statements -----
# None (sourced by 05_multiple_imputation.R).
