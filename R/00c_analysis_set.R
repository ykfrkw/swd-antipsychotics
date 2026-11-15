# 1. Copyright statement -----
# Copyright (c) 2026 Yuki Furukawa. Released under the MIT License (see LICENSE).

# 2. Author -----
# Author: SWD study authors (see LICENSE)
# Date: 2026-11-14 (last revised)

# 3. File description -----
# Purpose: The analysis set and the regression covariates, shared by every
#          analysis script: read the individual table, keep the assigned
#          drug track with a usable SWD, apply the post hoc exclusions and
#          derive the covariates (employment score, admissions category,
#          current use of the randomized drug).
# Inputs:  data/processed/swd_individual.csv (via read_individual())
# Outputs: none (function definitions; write_output_csv() writes to outputs/)
# Depends: 00_config.R (exclusion lists, status sets, DRUG_TRACKS,
#          SEX_LEVELS, PATH_OUTPUTS); tidyverse

# 4. source() and library() -----
# None: the calling script loads tidyverse and sources 00_config.R.

# Constants -----
SWD_TRACK_COLUMN_PATTERN <- "^(swd|status|threshold|n_visited|non_monotone)_"
REGRESSION_COVARIATES <- c(
  "sex", "age_band_score", "educ_score", "employ_score", "country",
  "onset_age_score", "family_history_schizophrenia", "ever_used_antipsychotic",
  "currently_using_randomized_drug", "fibser_total", "n_admissions_cat",
  "randomized_drug"
)
# Psychiatric admissions as a 5-level factor (reference = None): 6-10 and
# >10 are collapsed into "6 or more", as in Tables 1 and 2.
N_ADMISSIONS_CAT_MAP <- c(
  "none" = "None", "1" = "1", "2" = "2", "3-5" = "3-5",
  "6-10" = "6 or more", ">10" = "6 or more"
)
N_ADMISSIONS_CAT_LEVELS <- c("None", "1", "2", "3-5", "6 or more")
# Employment as an ordinal score, parallel to the education score.
EMPLOY_SCORE_MAP <- c(employed = 1, supported_or_other = 2, not_employed = 3)
# Current use of the randomized drug from its drug-history item: currently
# using = Yes; previously or never used = No; don't know = missing.
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

# 5. Function definitions -----
read_individual <- function() {
  read_csv(
    INDIVIDUAL_PATH,
    col_types = cols(respondent_id = col_character(), .default = col_guess())
  )
}

write_output_csv <- function(table, file_name) {
  dir.create(PATH_OUTPUTS, recursive = TRUE, showWarnings = FALSE)
  write_excel_csv(table, file.path(PATH_OUTPUTS, file_name), na = "")
}

# One row per respondent and drug track.
make_swd_long <- function(individual) {
  individual %>%
    select(
      all_of(c("respondent_id", "country", "age_band", "sex", "randomized_drug")),
      matches(SWD_TRACK_COLUMN_PATTERN)
    ) %>%
    pivot_longer(
      cols = matches(SWD_TRACK_COLUMN_PATTERN),
      names_to = c(".value", "drug_code"),
      names_pattern = "^(swd|status|threshold|n_visited|non_monotone)_(ris|ari|ola)$"
    ) %>%
    mutate(drug = factor(
      DRUG_TRACKS$drug[match(.data$drug_code, DRUG_TRACKS$drug_code)],
      levels = DRUG_TRACKS$drug
    ))
}

# Rows of the randomized (assigned) drug track with a usable SWD value.
assigned_usable_swd_long <- function(individual, statuses = USABLE_SWD_STATUSES) {
  make_swd_long(individual) %>%
    filter(
      as.character(.data$drug) == as.character(.data$randomized_drug),
      .data$status %in% statuses,
      !is.na(.data$swd)
    )
}

# Named list of ID vectors (one per country) as a country / respondent_id table.
exclusion_list_table <- function(ids_by_country) {
  tibble(
    country = rep(names(ids_by_country), lengths(ids_by_country)),
    respondent_id = unlist(ids_by_country, use.names = FALSE)
  )
}

# Post hoc exclusions: free-text answer inconsistent with the staircase
# (ANALYSIS_EXCLUDE) and psychiatric admission within the past 6 months
# reported despite the screening criterion (RECENT_ADMISSION_EXCLUDE).
filter_analysis_exclusions <- function(respondents) {
  exclusions <- bind_rows(
    exclusion_list_table(ANALYSIS_EXCLUDE),
    exclusion_list_table(RECENT_ADMISSION_EXCLUDE)
  ) %>%
    distinct()
  anti_join(respondents, exclusions, by = RESPONDENT_KEY)
}

check_analysis_set_size <- function(analysis) {
  stopifnot(
    nrow(analysis) == ANALYSIS_N_TOTAL,
    sum(analysis$country == "DE") == ANALYSIS_N_DE,
    sum(analysis$country == "JP") == ANALYSIS_N_JP
  )
}

# Primary analysis set (n = 225) with every column of the individual table,
# the assigned-track status (status_assigned) and the SWD.
build_analysis_set <- function(individual) {
  analysis <- assigned_usable_swd_long(individual) %>%
    select(all_of(RESPONDENT_KEY), status_assigned = "status", "swd") %>%
    filter_analysis_exclusions() %>%
    left_join(individual, by = RESPONDENT_KEY)
  check_analysis_set_size(analysis)
  analysis
}

blank_to_na <- function(labels) {
  labels[!is.na(labels) & trimws(labels) == ""] <- NA_character_
  labels
}

# Exact-match label maps: a label that is not in the map stops the script.
map_labels <- function(raw_labels, label_map, target) {
  labels <- blank_to_na(as.character(raw_labels))
  is_unknown <- !is.na(labels) & !(labels %in% names(label_map))
  if (any(is_unknown)) {
    stop(sprintf(
      "Unknown label(s) in %s: %s", target,
      paste(sprintf("[%s]", unique(labels[is_unknown])), collapse = " | ")
    ))
  }
  unname(label_map[labels])
}

# Drug-history answer for the drug the respondent was randomized to.
assigned_drug_history <- function(individual) {
  randomized_drug <- as.character(individual$randomized_drug)
  case_when(
    randomized_drug == "risperidone" ~ as.character(individual$drug_history_risperidone),
    randomized_drug == "aripiprazole" ~ as.character(individual$drug_history_aripiprazole),
    randomized_drug == "olanzapine" ~ as.character(individual$drug_history_olanzapine),
    TRUE ~ NA_character_
  )
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
      currently_using_randomized_drug = map_labels(
        assigned_drug_history(individual), CURRENT_USE_RESPONSE_MAP,
        "drug history (currently_using_randomized_drug)"
      ),
      employ_score = map_labels(
        individual$employ_3level, EMPLOY_SCORE_MAP, "employ_3level (employ_score)"
      ),
      n_admissions_cat = factor(
        map_labels(
          individual$n_admissions_group, N_ADMISSIONS_CAT_MAP,
          "n_admissions_group (n_admissions_cat)"
        ),
        levels = N_ADMISSIONS_CAT_LEVELS
      )
    ) %>%
    select(all_of(RESPONDENT_KEY), any_of(REGRESSION_COVARIATES))
}

# Outcome (swd, swd_status) and covariates of the analysis set restricted to
# the given staircase statuses.
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
    left_join(make_regression_covariates(individual), by = RESPONDENT_KEY) %>%
    mutate(
      age_band_score = as.numeric(.data$age_band_score),
      sex = factor(.data$sex, levels = SEX_LEVELS),
      educ_score = as.numeric(.data$educ_score),
      employ_score = as.numeric(.data$employ_score),
      onset_age_score = as.numeric(.data$onset_age_score),
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
      country = factor(.data$country)
    )
}

has_model_variation <- function(x) {
  length(unique(x[!is.na(x)])) >= 2
}

valid_regression_covariates <- function(model_frame) {
  covariates <- intersect(REGRESSION_COVARIATES, names(model_frame))
  covariates[vapply(
    covariates, \(covariate) has_model_variation(model_frame[[covariate]]),
    logical(1)
  )]
}

# The whole frame ("All") followed by one subset per country (sorted).
split_by_country <- function(frame) {
  country_names <- sort(unique(as.character(frame$country)))
  subsets <- lapply(country_names, \(name) frame[frame$country == name, ])
  c(
    stats::setNames(list(frame), ALL_COUNTRIES_LABEL),
    stats::setNames(subsets, country_names)
  )
}

# 6. Executed statements -----
# None (sourced by the analysis scripts 02-07).
