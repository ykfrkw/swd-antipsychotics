#1. Copyright statement -----
# Copyright (c) 2026 the SWD study authors. Released under the MIT License (see LICENSE).

#2. Author -----
# Author: SWD study authors (see LICENSE)
# Date: 2026-11-13 (last revised)

#3. File description -----
# Purpose: SWD regressions on respondent characteristics. Defines the
#          regression covariates and their derivation (employ_score,
#          n_admissions_cat, currently_using_randomized_drug), builds the
#          regression frame and runs the linear models (univariable + LASSO)
#          for the primary and sensitivity specifications. The model fits live
#          in 02g_swd_linear_models.R and the binary (SWD > 40) logistic
#          models in 02e_swd_logistic_regression.R; both are sourced here so
#          callers of this file keep one entry point.
# Inputs:  none (works on the individual table)
# Outputs: R_SWD/data/processed/swd_regression.csv (run_swd_regressions());
#          swd_regression_binary.csv (run_swd_binary_regressions(), 02e)
# Depends: 00a_config.R, 01d_swd_summary.R (assigned_usable_swd_long),
#          01f_test_respondent_exclusion.R (filter_analysis_exclusions),
#          02e_swd_logistic_regression.R, 02g_swd_linear_models.R;
#          tidyverse, glmnet
# Style:   r_script_style_guide.md (Tier B)

#4. source() and library() -----
# The calling script loads tidyverse and sources 00a_config.R,
# 01d_swd_summary.R and 01f_test_respondent_exclusion.R.
source(file.path(PATH_UTILS, "02g_swd_linear_models.R"))
source(file.path(PATH_UTILS, "02e_swd_logistic_regression.R"))

# Constants -----
REGRESSION_COVARIATES <- c(
  "sex",
  "age_band_score",
  "educ_score",
  "employ_score",
  "country",
  "onset_age_score",
  "family_history_schizophrenia",
  "ever_used_antipsychotic",
  "currently_using_randomized_drug",
  "fibser_total",
  "n_admissions_cat",
  "randomized_drug"
)

# Psychiatric admissions as a 5-level categorical covariate (reference = None).
# Derived from the harmonized n_admissions_group, collapsing 6-10 and >10 into
# "6 or more" to match Tables 1 and 2. Exact-match map; unknown labels stop().
N_ADMISSIONS_CAT_MAP <- c(
  "none" = "None",
  "1" = "1",
  "2" = "2",
  "3-5" = "3-5",
  "6-10" = "6 or more",
  ">10" = "6 or more"
)
N_ADMISSIONS_CAT_LEVELS <- c("None", "1", "2", "3-5", "6 or more")

# Employment as an ordinal score (parallel to educ_score): derived inline from
# the harmonized employ_3level factor. Exact-match map; unknown labels stop().
EMPLOY_SCORE_MAP <- c(
  employed = 1,
  supported_or_other = 2,
  not_employed = 3
)

# Currently using the randomized drug, derived from the drug-history item of
# the assigned drug (exact-match labels; unknown labels stop()).
# Yes = currently using; No = previously used or never used;
# don't know / missing -> NA.
CURRENT_USE_RESPONSE_MAP <- c(
  "現在使っている" = "Yes",
  "Derzeit in Verwendung" = "Yes",
  "以前使ったことがある" = "No",
  "Früher verwendet" = "No",
  "使ったことがない" = "No",
  "Noch nie verwendet" = "No",
  "わからない/覚えていない" = NA_character_,
  "Weiß nicht / Weiß nicht mehr" = NA_character_
)

UNKNOWN_RESPONSE_PATTERN <- paste0(
  "unknown|prefer|weiss|weiß|mochte ich nicht|möchte ich nicht|",
  "weiss nicht|weiß nicht"
)

# Model sizes and LASSO settings (seed value unchanged since the first run).
MIN_UNIVARIABLE_N <- 3
MIN_LASSO_N <- 8
MAX_LASSO_FOLDS <- 10L
LASSO_SEED <- 20240529
LASSO_ALPHA <- 1

# Row status labels shared by the linear (02g) and logistic (02e) models.
REGRESSION_STATUS <- list(
  insufficient_data = "skipped_insufficient_data",
  glmnet_missing = "skipped_glmnet_missing",
  too_few_complete_cases = "skipped_too_few_complete_cases",
  no_varying_covariates = "skipped_no_varying_covariates",
  no_model_columns = "skipped_no_model_columns",
  lasso_error = "skipped_lasso_error",
  no_terms_selected = "no_terms_selected"
)

#5. Function definitions -----
blank_to_na <- function(labels) {
  labels[!is.na(labels) & trimws(labels) == ""] <- NA_character_
  labels
}

stop_on_unknown_labels <- function(labels, known_labels, target) {
  is_unknown <- !is.na(labels) & !(labels %in% known_labels)
  if (any(is_unknown)) {
    stop(sprintf(
      "Unknown %s: %s",
      target,
      paste(sprintf("[%s]", unique(labels[is_unknown])), collapse = " | ")
    ))
  }
}

derive_n_admissions_cat <- function(individual) {
  admissions_group <- blank_to_na(as.character(individual$n_admissions_group))
  stop_on_unknown_labels(
    admissions_group, names(N_ADMISSIONS_CAT_MAP),
    "n_admissions_group label(s) for n_admissions_cat"
  )
  factor(unname(N_ADMISSIONS_CAT_MAP[admissions_group]), levels = N_ADMISSIONS_CAT_LEVELS)
}

derive_employ_score <- function(individual) {
  employ_3level <- blank_to_na(as.character(individual$employ_3level))
  stop_on_unknown_labels(
    employ_3level, names(EMPLOY_SCORE_MAP),
    "employ_3level label(s) for employ_score"
  )
  unname(EMPLOY_SCORE_MAP[employ_3level])
}

derive_current_use <- function(individual) {
  randomized_drug <- as.character(individual$randomized_drug)
  drug_history <- case_when(
    randomized_drug == "risperidone" ~ as.character(individual$drug_history_risperidone),
    randomized_drug == "aripiprazole" ~ as.character(individual$drug_history_aripiprazole),
    randomized_drug == "olanzapine" ~ as.character(individual$drug_history_olanzapine),
    TRUE ~ NA_character_
  )
  drug_history <- blank_to_na(drug_history)
  stop_on_unknown_labels(
    drug_history, names(CURRENT_USE_RESPONSE_MAP),
    "drug-history label(s) for currently_using_randomized_drug"
  )
  unname(CURRENT_USE_RESPONSE_MAP[drug_history])
}

# JP and DE yes / no / don't know answers collapsed to English levels.
regression_binary_response <- function(x) {
  response_text <- as.character(x)
  lower <- tolower(trimws(response_text))

  case_when(
    is.na(response_text) | trimws(response_text) == "" ~ NA_character_,
    grepl("^(yes|y|1|ja)$", lower) | grepl("はい", response_text) ~ "Yes",
    grepl("^(no|n|2|nein)$", lower) | grepl("いいえ", response_text) ~ "No",
    grepl(UNKNOWN_RESPONSE_PATTERN, lower) |
      grepl("わからない|答えたくない", response_text) ~
      "Unknown / prefer not to answer",
    TRUE ~ response_text
  )
}

make_regression_covariates <- function(individual) {
  individual %>%
    mutate(
      respondent_id = as.character(.data$respondent_id),
      currently_using_randomized_drug = derive_current_use(individual),
      employ_score = derive_employ_score(individual),
      n_admissions_cat = derive_n_admissions_cat(individual)
    ) %>%
    select(all_of(c("respondent_id", "country")), any_of(REGRESSION_COVARIATES))
}

make_regression_frame <- function(individual, statuses) {
  assigned <- assigned_usable_swd_long(individual, statuses) %>%
    transmute(
      respondent_id = as.character(.data$respondent_id),
      country = as.character(.data$country),
      swd = as.numeric(.data$swd),
      swd_status = as.character(.data$status)
    ) %>%
    filter_analysis_exclusions()

  assigned %>%
    left_join(make_regression_covariates(individual), by = c("respondent_id", "country")) %>%
    mutate(
      age_band_score = as.numeric(.data$age_band_score),
      sex = factor(.data$sex, levels = SEX_LEVELS),
      educ_score = as.numeric(.data$educ_score),
      employ_score = as.numeric(.data$employ_score),
      onset_age_score = as.numeric(.data$onset_age_score),
      n_admissions_cat = factor(.data$n_admissions_cat, levels = N_ADMISSIONS_CAT_LEVELS),
      fibser_total = as.numeric(.data$fibser_total),
      randomized_drug = factor(.data$randomized_drug, levels = DRUG_TRACKS$drug),
      family_history_schizophrenia = factor(
        regression_binary_response(.data$family_history_schizophrenia)
      ),
      ever_used_antipsychotic = factor(
        regression_binary_response(.data$ever_used_antipsychotic)
      ),
      currently_using_randomized_drug = factor(
        .data$currently_using_randomized_drug,
        levels = c("No", "Yes")
      ),
      across(any_of("country"), \(x) factor(as.character(x)))
    )
}

has_model_variation <- function(x) {
  observed <- x[!is.na(x)]
  length(unique(observed)) >= 2
}

valid_regression_covariates <- function(model_frame) {
  covariates <- REGRESSION_COVARIATES[REGRESSION_COVARIATES %in% names(model_frame)]
  covariates[vapply(
    covariates,
    \(covariate) has_model_variation(model_frame[[covariate]]),
    logical(1)
  )]
}

run_swd_regressions_for_spec <- function(individual, specification, statuses) {
  model_frame <- make_regression_frame(individual, statuses)
  covariates <- valid_regression_covariates(model_frame)

  univariable <- bind_rows(lapply(covariates, \(covariate) {
    run_univariable_regression(model_frame, covariate, specification)
  }))

  lasso <- run_lasso_regression(model_frame, covariates, specification)
  bind_rows(univariable, lasso)
}

run_swd_regressions <- function(individual) {
  # Primary analysis includes never willing respondents (SWD = 60, n up to 225);
  # the sensitivity analysis excludes them (n up to 193).
  regression <- bind_rows(
    run_swd_regressions_for_spec(
      individual,
      specification = "primary_including_never_willing",
      statuses = USABLE_SWD_STATUSES
    ),
    run_swd_regressions_for_spec(
      individual,
      specification = "sensitivity_excluding_never_willing",
      statuses = USABLE_SWD_STATUSES_EXCLUDING_NEVER
    )
  )

  dir.create(PATH_OUT, recursive = TRUE, showWarnings = FALSE)
  write_excel_csv(regression, file.path(PATH_OUT, "swd_regression.csv"), na = "")

  regression
}

#6. Executed statements -----
# None (sourced by 02_report_outputs.R, 05_table3_regression.R,
# 09_additional_descriptives.R, 11_interval_regression.R and
# 12_multiple_imputation.R).
