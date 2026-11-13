#1. Copyright statement -----
# Copyright (c) 2026 the SWD study authors. Released under the MIT License (see LICENSE).

#2. Author -----
# Author: SWD study authors (see LICENSE)
# Date: 2026-11-13 (last revised)

#3. File description -----
# Purpose: Secondary analyses for 12_multiple_imputation.R: (i) multiple
#          imputation of missing covariates among the 225 analysed and the
#          pooled 12-covariate OLS vs complete case (n = 212); (ii) extra
#          numbers for the response letter (non-monotone staircases,
#          comprehension-check counts, FIBSER, reproduced median SWD).
# Inputs:  none (functions receive data frames from the caller)
# Outputs: none (function definitions)
# Depends: 02d_swd_linear_regression.R, 09a_additional_descriptives_helpers.R
#          (fit_complete_case_lm via 09b, CONFIDENCE_LEVEL),
#          01f_test_respondent_exclusion.R (RESPONDENT_KEY); tidyverse, mice
# Style:   r_script_style_guide.md (Tier B)

#4. source() and library() -----
# None: the calling script loads tidyverse and sources 00a_config.R and the
# files listed under Depends.

# Constants -----
QUANTILE_TYPE_R_DEFAULT <- 7L
QUANTILE_TYPE_SPSS <- 6L
QUARTILE_PROBABILITIES <- c(0.25, 0.75)
COVARIATE_MI_ITERATIONS <- 20L # several covariates are jointly missing
EXTRA_COUNT_STATISTICS <- c("n", "denominator", "pct")

#5. Function definitions -----
tidy_lm_table <- function(fit) {
  coefficients <- summary(fit)$coefficients
  intervals <- stats::confint(fit, level = CONFIDENCE_LEVEL)
  tibble(
    term = rownames(coefficients),
    estimate = coefficients[, "Estimate"],
    conf_low = intervals[, 1], conf_high = intervals[, 2],
    p_value = coefficients[, "Pr(>|t|)"]
  )
}

# mice default methods by type (pmm numeric, logreg binary factor, polyreg
# unordered factor); the outcome SWD is a predictor of every covariate. All
# covariates enter the imputation model; the analysis model uses the
# covariates estimable in the complete cases, so both models match.
run_covariate_mi <- function(analysed_frame, covariates, analysis_covariates,
                             imputations, seed) {
  model_data <- analysed_frame %>%
    select(all_of(c("swd", covariates))) %>%
    droplevels() %>%
    as.data.frame()
  imputation <- mice::mice(model_data,
    m = imputations,
    maxit = COVARIATE_MI_ITERATIONS, seed = seed,
    printFlag = FALSE
  )
  model_formula <- stats::reformulate(analysis_covariates, response = "swd")
  fits <- map(seq_len(imputations), \(index) {
    stats::lm(model_formula, data = mice::complete(imputation, index))
  })
  pooled <- summary(mice::pool(mice::as.mira(fits)),
    conf.int = TRUE,
    conf.level = CONFIDENCE_LEVEL
  )
  mi_table <- tibble(
    term = as.character(pooled$term),
    mi_estimate = pooled$estimate,
    mi_conf_low = pooled[["2.5 %"]],
    mi_conf_high = pooled[["97.5 %"]],
    mi_p_value = pooled$p.value
  )
  list(
    table = mi_table, methods = imputation$method,
    n_missing = colSums(is.na(model_data)),
    logged_events = as_tibble(imputation$loggedEvents)
  )
}

compare_covariate_models <- function(analysed_frame, covariates, imputations,
                                     seed) {
  complete_case_fit <- fit_complete_case_lm(analysed_frame, covariates)
  complete_case <- tidy_lm_table(complete_case_fit) %>%
    rename_with(\(name) paste0("cc_", name), -"term")
  analysis_covariates <- setdiff(covariates, complete_case_fit$dropped_constant)
  covariate_mi <- run_covariate_mi(
    analysed_frame, covariates, analysis_covariates,
    imputations, seed
  )
  comparison_table <- complete_case %>%
    full_join(covariate_mi$table, by = "term") %>%
    mutate(
      cc_n = stats::nobs(complete_case_fit),
      mi_n = nrow(analysed_frame),
      cc_dropped_constant = paste(complete_case_fit$dropped_constant,
        collapse = ", "
      )
    )
  list(
    table = comparison_table, methods = covariate_mi$methods,
    n_missing = covariate_mi$n_missing,
    logged_events = covariate_mi$logged_events,
    dropped_constant = complete_case_fit$dropped_constant
  )
}

# Logged events are expected only when mice removes, as a predictor, a
# covariate that is constant among complete cases (ever_used_antipsychotic:
# its 2 "No" answers have currently_using_randomized_drug missing, so it is
# constant wherever that covariate is observed). That covariate is already
# dropped from the analysis model; any other logged event is unexplained.
unexplained_logged_events <- function(comparison) {
  events <- comparison$logged_events
  if (nrow(events) == 0 || length(comparison$dropped_constant) == 0) {
    return(events)
  }
  dropped_pattern <- paste0("^(", paste(comparison$dropped_constant,
    collapse = "|"
  ), ")")
  events %>% filter(!str_detect(.data$out, dropped_pattern))
}

describe_covariate_imputation <- function(comparison) {
  is_imputed <- comparison$n_missing > 0
  paste0(names(comparison$n_missing)[is_imputed], " (n missing ",
    comparison$n_missing[is_imputed], ", ",
    comparison$methods[names(comparison$n_missing)[is_imputed]], ")",
    collapse = "; "
  )
}

assigned_non_monotone <- function(analysed_frame, individual) {
  flags <- individual %>%
    transmute(
      country = .data$country,
      respondent_id = as.character(.data$respondent_id),
      is_non_monotone = case_when(
        .data$randomized_drug == "risperidone" ~ .data$non_monotone_ris,
        .data$randomized_drug == "aripiprazole" ~ .data$non_monotone_ari,
        .data$randomized_drug == "olanzapine" ~ .data$non_monotone_ola
      )
    )
  analysed_frame %>%
    mutate(country = as.character(.data$country)) %>%
    left_join(flags, by = RESPONDENT_KEY)
}

describe_numeric <- function(values) {
  observed <- values[!is.na(values)]
  quartiles_r <- stats::quantile(observed, QUARTILE_PROBABILITIES,
    type = QUANTILE_TYPE_R_DEFAULT, names = FALSE
  )
  quartiles_spss <- stats::quantile(observed, QUARTILE_PROBABILITIES,
    type = QUANTILE_TYPE_SPSS, names = FALSE
  )
  tibble(
    n = length(observed), n_missing = sum(is.na(values)),
    median = stats::median(observed), q1 = quartiles_r[1],
    q3 = quartiles_r[2], q1_type6 = quartiles_spss[1],
    q3_type6 = quartiles_spss[2], mean = mean(observed),
    sd = stats::sd(observed)
  )
}

describe_by <- function(frame, variable, group_variable) {
  groups <- if (is.null(group_variable)) {
    ALL_COUNTRIES_LABEL
  } else {
    as.character(frame[[group_variable]])
  }
  map(sort(unique(groups)), \(level) {
    describe_numeric(frame[[variable]][groups == level]) %>%
      mutate(
        variable = variable,
        group = if (is.null(group_variable)) "overall" else group_variable,
        level = level, .before = 1
      )
  }) %>%
    bind_rows()
}

count_row <- function(item, group, count, denominator, note = NA_character_) {
  tibble(
    item = item, group = group, n = count, denominator = denominator,
    pct = 100 * count / denominator, note = note
  )
}

# Questionnaire gate (SoSci page "comprehension*"): CO01 a, b and d checked,
# CO02 and CO03 correct. The pipeline flag is_comprehension_correct is
# stricter: it also requires CO01 c (raised blood pressure) unchecked.
summarise_comprehension <- function(eligible_starters) {
  stage_counts <- eligible_starters %>% count(.data$stop_stage)
  failed_any <- eligible_starters %>%
    filter(.data$has_comprehension_answer, !.data$is_comprehension_correct)
  denominator <- nrow(eligible_starters)
  bind_rows(
    pmap(stage_counts, \(stop_stage, n) {
      count_row("stop_stage", stop_stage, n, denominator)
    }),
    count_row(
      "comprehension_any_item_incorrect_strict", "all", nrow(failed_any),
      denominator, "pipeline flag incl. CO01 c unchecked"
    ),
    count_row(
      "comprehension_any_item_incorrect_strict", "with SWD",
      sum(failed_any$has_swd), denominator,
      "passed the questionnaire gate (CO01 c not gated)"
    ),
    count_row(
      "comprehension_any_item_incorrect_strict", "without SWD",
      sum(!failed_any$has_swd), denominator, "stopped at the gate"
    )
  )
}

build_extra_numbers <- function(analysed_frame, individual, eligible_starters) {
  monotone <- assigned_non_monotone(analysed_frame, individual)
  stopifnot(!anyNA(monotone$is_non_monotone))
  non_monotone <- bind_rows(
    count_row(
      "non_monotone_staircase", ALL_COUNTRIES_LABEL,
      sum(monotone$is_non_monotone), nrow(monotone)
    ),
    map(c("DE", "JP"), \(name) {
      country_rows <- monotone[monotone$country == name, ]
      count_row(
        "non_monotone_staircase", name,
        sum(country_rows$is_non_monotone), nrow(country_rows)
      )
    }) %>% bind_rows()
  )
  descriptives <- bind_rows(
    describe_by(analysed_frame, "fibser_total", NULL),
    describe_by(analysed_frame, "fibser_total", "country"),
    describe_by(analysed_frame, "swd", NULL),
    describe_by(analysed_frame, "swd", "country"),
    describe_by(analysed_frame, "swd", "randomized_drug")
  )
  list(
    counts = bind_rows(
      non_monotone,
      summarise_comprehension(eligible_starters)
    ),
    descriptives = descriptives
  )
}

# Long table of the extra numbers (counts, then descriptives).
tidy_extra_numbers <- function(extra) {
  counts <- extra$counts %>%
    pivot_longer(all_of(EXTRA_COUNT_STATISTICS), names_to = "statistic") %>%
    rename(level = "group")
  descriptives <- extra$descriptives %>%
    unite("item", "variable", "group", sep = " by ") %>%
    pivot_longer(-c("item", "level"), names_to = "statistic") %>%
    mutate(note = NA_character_)
  bind_rows(counts, descriptives)
}

#6. Executed statements -----
# None (sourced by 12_multiple_imputation.R).
