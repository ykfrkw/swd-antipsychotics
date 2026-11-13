#1. Copyright statement -----
# Copyright (c) 2026 the SWD study authors. Released under the MIT License (see LICENSE).

#2. Author -----
# Author: SWD study authors (see LICENSE)
# Date: 2026-11-13 (last revised)

#3. File description -----
# Purpose: Helper functions for 12_multiple_imputation.R: build the
#          imputation population (225 analysed + 51 eligible starters without
#          an SWD), derive the admissible SWD set of mid-staircase stoppers
#          from their partial answers, run bounded predictive mean matching
#          through mice (Model A by country, Model B pooled with country +
#          drug), and build the comprehension-failure sensitivity population
#          and the bound-respecting best/worst cases.
# Inputs:  R_SWD/data/raw/data_swd_{ja,de}*.xlsx (newest; staircase answers,
#          read by read_assigned_staircase())
# Outputs: none (function definitions)
# Depends: 00a_config.R, 01a_raw_data_loading.R,
#          09a_additional_descriptives_helpers.R (SWD_FLOOR, SWD_CEILING,
#          SWD_STEP, STOP_STAGE_*, as_integer_code, exclusion_list_table),
#          01f_test_respondent_exclusion.R (RESPONDENT_KEY);
#          tidyverse, mice
# Style:   r_script_style_guide.md (Tier B)

#4. source() and library() -----
# None: the calling script loads tidyverse and sources 00a_config.R and the
# files listed under Depends.

# Constants -----
PMM_DONORS <- 5L
IMPUTATION_ITERATIONS <- 1L # only the outcome is incomplete

#5. Function definitions -----
misunderstanding_exclusions <- function() {
  exclusion_list_table(ANALYSIS_EXCLUDE)
}

# Staircase answers (1 = worthwhile, 2 = not worthwhile) on the assigned
# track, one row per respondent and relapse rate (never-shown nodes are
# absent from the export, hence any_of()).
read_assigned_staircase <- function(country) {
  survey_data <- suppressWarnings(read_sosci_data_excel(
    find_raw_file(PATH_RAW, tolower(country), "data", "xlsx")
  ))
  nodes <- SWD_NODES_BY_COUNTRY[[tolower(country)]]
  node_long <- nodes %>%
    pivot_longer(-"relapse_pct", names_to = "drug", values_to = "node")
  survey_data %>%
    transmute(
      respondent_id = as.character(as_integer_code(.data$CASE)),
      drug = DRUG_TRACKS$drug[match(
        as_integer_code(.data$RA01),
        DRUG_TRACKS$ra01
      )],
      across(any_of(unique(node_long$node)), as_integer_code)
    ) %>%
    filter(!is.na(.data$respondent_id), !is.na(.data$drug)) %>%
    pivot_longer(-c("respondent_id", "drug"),
      names_to = "node",
      values_to = "answer"
    ) %>%
    inner_join(node_long, by = c("drug", "node")) %>%
    filter(.data$answer %in% c(YES, NO)) %>%
    mutate(country = country)
}

# A "worthwhile" answer at relapse rate r implies SWD <= 60 - r; a "not
# worthwhile" answer at r implies SWD >= 60 - r + 5
# (cf. 11a_interval_regression_helpers.R).
derive_partial_bounds <- function(staircase_answers) {
  staircase_answers %>%
    group_by(.data$country, .data$respondent_id) %>%
    summarise(
      n_answered = n(),
      max_yes_rate = suppressWarnings(
        max(.data$relapse_pct[.data$answer == YES])
      ),
      min_no_rate = suppressWarnings(
        min(.data$relapse_pct[.data$answer == NO])
      ),
      answered_path = paste0(.data$relapse_pct, "%:",
        if_else(.data$answer == YES, "yes", "no"),
        collapse = " "
      ),
      .groups = "drop"
    ) %>%
    mutate(
      swd_lower = if_else(is.finite(.data$min_no_rate),
        SWD_CEILING - .data$min_no_rate + SWD_STEP, SWD_FLOOR
      ),
      swd_upper = if_else(is.finite(.data$max_yes_rate),
        SWD_CEILING - .data$max_yes_rate, SWD_CEILING
      )
    )
}

# 276 = 225 analysed + 51 eligible starters without an SWD. The 12
# respondents excluded for misunderstanding the task stay excluded.
build_imputation_population <- function(eligible_starters, analysed_frame,
                                        partial_bounds) {
  outcome <- analysed_frame %>%
    transmute(
      country = as.character(.data$country),
      respondent_id = .data$respondent_id, swd = .data$swd
    )
  bounds <- partial_bounds %>%
    select(
      "country", "respondent_id", "answered_path", "swd_lower",
      "swd_upper"
    )
  eligible_starters %>%
    anti_join(misunderstanding_exclusions(),
      by = RESPONDENT_KEY
    ) %>%
    left_join(outcome, by = RESPONDENT_KEY) %>%
    left_join(bounds, by = RESPONDENT_KEY) %>%
    mutate(
      is_staircase_stopper = .data$stop_stage == STOP_STAGE_STAIRCASE,
      swd_lower = if_else(.data$is_staircase_stopper, .data$swd_lower,
        SWD_FLOOR
      ),
      swd_upper = if_else(.data$is_staircase_stopper, .data$swd_upper,
        SWD_CEILING
      ),
      is_japan = as.numeric(.data$country == "JP"),
      drug_ari = as.numeric(.data$randomized_drug == "aripiprazole"),
      drug_ola = as.numeric(.data$randomized_drug == "olanzapine")
    ) %>%
    arrange(.data$country, as.integer(.data$respondent_id))
}

# Bounded PMM. Parameter uncertainty is propagated with an approximate
# Bayesian bootstrap of the observed cases; each missing value is then drawn
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
# the 5-point grid, truncate to [0, 60] and to the admissible set.
apply_delta <- function(datasets, delta, is_shifted_country) {
  map(datasets, \(completed) {
    is_shifted <- !completed$has_swd & is_shifted_country(completed$country)
    shifted <- SWD_STEP * round((completed$swd + delta) / SWD_STEP)
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

#6. Executed statements -----
# None (sourced by 12_multiple_imputation.R).
