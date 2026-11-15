# 1. Copyright statement -----
# Copyright (c) 2026 Yuki Furukawa. Released under the MIT License (see LICENSE).

# 2. Author -----
# Author: SWD study authors (see LICENSE)
# Date: 2026-11-14 (last revised)

# 3. File description -----
# Purpose: Harmonize the JP and DE demographic items into one table: value
#          labels, standardized factors and ordinal scores, FIBSER total.
# Inputs:  none (works on the list returned by load_raw_data())
# Outputs: none (function definitions)
# Depends: 00_config.R (DEMO_MAP, AGE_LEVELS, SEX_LEVELS, DRUG_TRACKS,
#          ADDITIONAL_DEMO_FIELDS, NA_CODE), 00b_de_value_labels.R
#          (DE_VALUE_LABELS); tidyverse

# 4. source() and library() -----
# None: the calling script loads tidyverse and sources 00_config.R and
# 00b_de_value_labels.R.

# Constants -----
EDUC_3LEVEL_LEVELS <- c("low", "middle", "high")
EMPLOY_3LEVEL_LEVELS <- c("employed", "supported_or_other", "not_employed")
N_ADMISSIONS_GROUP_LEVELS <- c("none", "1", "2", "3-5", "6-10", ">10")
ONSET_AGE_GROUP_LEVELS <- c(
  "<=5", "6-10", "11-15", "16-20", "21-25", "26-30", "31-35", "36-40",
  "41-45", "46-50", "51-55", "56-60", "61-65", "66-70", "71-75", "75+"
)
# FIBSER items are coded 1-7; each contributes (code - 1) to the total.
FIBSER_MIN_CODE <- 1L
FIBSER_MAX_CODE <- 7L

# 5. Function definitions -----
get_column_or_na <- function(survey_data, column_name) {
  is_missing_column <- is.null(column_name) || is.na(column_name) ||
    !column_name %in% names(survey_data)
  if (is_missing_column) {
    return(rep(NA, nrow(survey_data)))
  }

  survey_data[[column_name]]
}

as_integer_safe <- function(x) {
  suppressWarnings(as.integer(as.character(x)))
}

lookup_value_labels <- function(values, var, codes) {
  codes_chr <- as.character(codes)
  labels <- values %>%
    filter(.data$VAR == var, .data$RESPONSE != as.character(NA_CODE)) %>%
    mutate(RESPONSE = as.character(.data$RESPONSE))

  matched_labels <- labels$MEANING[match(codes_chr, labels$RESPONSE)]
  if_else(is.na(codes_chr), NA_character_, matched_labels)
}

lookup_builtin_de_labels <- function(var, codes) {
  codes_chr <- as.character(codes)
  labels <- DE_VALUE_LABELS[[var]]
  if (is.null(labels)) {
    return(rep(NA_character_, length(codes_chr)))
  }

  matched_labels <- unname(labels[codes_chr])
  if_else(is.na(codes_chr), NA_character_, matched_labels)
}

# The DE values export lacks some items; those use the built-in DE labels.
label_demographic_field <- function(survey_data, values, map, field, country = NULL) {
  var <- map[[field]]
  raw_codes <- get_column_or_na(survey_data, var)
  has_var <- !is.null(var) && !is.na(var)

  if (has_var && var %in% values$VAR) {
    return(lookup_value_labels(values, var, raw_codes))
  }
  if (identical(country, "DE") && has_var) {
    return(lookup_builtin_de_labels(var, raw_codes))
  }

  rep(NA_character_, nrow(survey_data))
}

label_demographic_fields <- function(survey_data, values, map, fields, country = NULL) {
  labelled_fields <- lapply(fields, \(field) {
    label_demographic_field(survey_data, values, map, field, country)
  })
  names(labelled_fields) <- fields
  as_tibble(labelled_fields)
}

standardize_age_band <- function(x) {
  labels <- AGE_LEVELS[as_integer_safe(x)]
  factor(labels, levels = AGE_LEVELS, ordered = TRUE)
}

standardize_sex <- function(x) {
  labels <- case_when(
    as_integer_safe(x) == 1L ~ "female",
    as_integer_safe(x) == 2L ~ "male",
    as_integer_safe(x) == 3L ~ "other_or_prefer_not_to_say",
    TRUE ~ NA_character_
  )
  factor(labels, levels = SEX_LEVELS, ordered = FALSE)
}

standardize_randomized_drug <- function(x) {
  codes <- as_integer_safe(x)
  drug <- DRUG_TRACKS$drug[match(codes, DRUG_TRACKS$ra01)]
  factor(drug, levels = DRUG_TRACKS$drug)
}

standardize_educ_3level <- function(country, x) {
  codes <- as_integer_safe(x)
  labels <- case_when(
    country == "JP" & codes == 1L ~ "low",
    country == "JP" & codes %in% 2:4 ~ "middle",
    country == "JP" & codes %in% 5:7 ~ "high",
    country == "DE" & codes %in% 1:2 ~ "low",
    country == "DE" & codes %in% 3:5 ~ "middle",
    country == "DE" & codes %in% 6:9 ~ "high",
    TRUE ~ NA_character_
  )
  factor(labels, levels = EDUC_3LEVEL_LEVELS)
}

standardize_employ_3level <- function(country, x) {
  codes <- as_integer_safe(x)
  labels <- case_when(
    country == "JP" & codes %in% 1:2 ~ "employed",
    country == "JP" & codes %in% 3:4 ~ "supported_or_other",
    country == "JP" & codes == 5L ~ "not_employed",
    country == "DE" & codes %in% 1:2 ~ "employed",
    country == "DE" & codes %in% c(3L, 5L, 6L) ~ "supported_or_other",
    country == "DE" & codes == 4L ~ "not_employed",
    TRUE ~ NA_character_
  )
  factor(labels, levels = EMPLOY_3LEVEL_LEVELS)
}

# Code k (1-based) maps to the k-th label; other codes become NA.
label_codes_in_order <- function(x, labels) {
  codes <- as_integer_safe(x)
  coded_labels <- rep(NA_character_, length(codes))
  is_valid <- !is.na(codes) & codes >= 1L & codes <= length(labels)
  coded_labels[is_valid] <- labels[codes[is_valid]]
  factor(coded_labels, levels = labels, ordered = TRUE)
}

standardize_n_admissions_group <- function(x) {
  label_codes_in_order(x, N_ADMISSIONS_GROUP_LEVELS)
}

standardize_onset_age_group <- function(x) {
  label_codes_in_order(x, ONSET_AGE_GROUP_LEVELS)
}

score_codes <- function(x, min_code, max_code, offset = 0L) {
  codes <- as_integer_safe(x)
  scores <- rep(NA_real_, length(codes))
  is_valid <- !is.na(codes) & codes >= min_code & codes <= max_code
  scores[is_valid] <- as.numeric(codes[is_valid] + offset)
  scores
}

standardize_age_band_score <- function(x) {
  score_codes(x, min_code = 1L, max_code = length(AGE_LEVELS))
}

standardize_educ_score <- function(country, x) {
  codes <- as_integer_safe(x)
  case_when(
    country == "JP" & codes == 1L ~ 1,
    country == "JP" & codes %in% 2:4 ~ 2,
    country == "JP" & codes %in% 5:7 ~ 3,
    country == "DE" & codes %in% 1:2 ~ 1,
    country == "DE" & codes %in% 3:5 ~ 2,
    country == "DE" & codes %in% 6:9 ~ 3,
    TRUE ~ NA_real_
  )
}

standardize_onset_age_score <- function(x) {
  score_codes(x, min_code = 1L, max_code = length(ONSET_AGE_GROUP_LEVELS))
}

# Score 0 = no admission (code 1) up to 5 = more than 10 admissions (code 6).
standardize_n_admissions_score <- function(x) {
  score_codes(
    x,
    min_code = 1L, max_code = length(N_ADMISSIONS_GROUP_LEVELS), offset = -1L
  )
}

fibser_code <- function(x) {
  codes <- as_integer_safe(x)
  codes[!is.na(codes) & codes == NA_CODE] <- NA_integer_
  codes
}

standardize_fibser_total <- function(duration, severity, interference) {
  codes <- tibble(
    duration = fibser_code(duration),
    severity = fibser_code(severity),
    interference = fibser_code(interference)
  )
  is_invalid <- unlist(lapply(codes, \(x) {
    !is.na(x) & (x < FIBSER_MIN_CODE | x > FIBSER_MAX_CODE)
  }))
  if (any(is_invalid)) {
    stop("FIBSER source items must be 1-7, -9, or NA.", call. = FALSE)
  }

  fibser_total <- (codes$duration - 1L) + (codes$severity - 1L) +
    (codes$interference - 1L)
  fibser_total[rowSums(is.na(codes)) > 0] <- NA_integer_
  as.numeric(fibser_total)
}

harmonize_country_demographics <- function(survey_data, values, country, map) {
  # Inside tibble() below, `country` would mean the (recycled) new column,
  # so the scalar country code is kept under its own name.
  country_code <- country
  age_raw <- get_column_or_na(survey_data, map[["age_band"]])
  sex_raw <- get_column_or_na(survey_data, map[["sex"]])
  educ_raw <- get_column_or_na(survey_data, map[["educ"]])
  employ_raw <- get_column_or_na(survey_data, map[["employ"]])
  n_admissions_raw <- get_column_or_na(survey_data, map[["n_psych_admissions"]])
  onset_raw <- get_column_or_na(survey_data, map[["onset_age_band"]])
  randomized_raw <- get_column_or_na(survey_data, map[["randomized_drug"]])
  free_text <- get_column_or_na(survey_data, map[["free_text_no_value_at_zero"]])

  base_demographics <- tibble(
    country = country_code,
    respondent_id = as.character(get_column_or_na(survey_data, "CASE")),
    age_band = standardize_age_band(age_raw),
    sex = standardize_sex(sex_raw),
    educ = label_demographic_field(survey_data, values, map, "educ", country_code),
    employ = label_demographic_field(survey_data, values, map, "employ", country_code),
    age_band_score = standardize_age_band_score(age_raw),
    educ_3level = standardize_educ_3level(country_code, educ_raw),
    educ_score = standardize_educ_score(country_code, educ_raw),
    employ_3level = standardize_employ_3level(country_code, employ_raw),
    n_admissions_group = standardize_n_admissions_group(n_admissions_raw),
    n_admissions_score = standardize_n_admissions_score(n_admissions_raw),
    onset_age_group = standardize_onset_age_group(onset_raw),
    onset_age_score = standardize_onset_age_score(onset_raw),
    fibser_total = standardize_fibser_total(
      get_column_or_na(survey_data, "DE13"),
      get_column_or_na(survey_data, "DE14"),
      get_column_or_na(survey_data, "DE15")
    )
  )

  additional_demographics <- label_demographic_fields(
    survey_data, values, map, ADDITIONAL_DEMO_FIELDS, country_code
  )

  bind_cols(
    base_demographics,
    additional_demographics,
    tibble(
      randomized_drug = standardize_randomized_drug(randomized_raw),
      sw16_01 = as.character(free_text)
    )
  )
}

harmonize_demographics <- function(raw) {
  bind_rows(
    harmonize_country_demographics(raw$jp_data, raw$jp_values, "JP", DEMO_MAP$jp),
    harmonize_country_demographics(raw$de_data, raw$de_values, "DE", DEMO_MAP$de)
  )
}

# 6. Executed statements -----
# None (sourced by 01_prepare_data.R).
