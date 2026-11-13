#1. Copyright statement -----
# Copyright (c) 2026 the SWD study authors. Released under the MIT License (see LICENSE).

#2. Author -----
# Author: SWD study authors (see LICENSE)
# Date: 2026-11-13 (last revised)

#3. File description -----
# Purpose: Table 1 analysis set with its consistency checks (recent-admission
#          exclusion, expected N) and the per-respondent aggregation of the
#          nine non-randomized antipsychotics; analysis-set counts cited in
#          the text (current antipsychotic use, always willing).
# Inputs:  none (functions receive swd_individual.csv as read by the caller)
# Outputs: none (function definitions)
# Depends: 00a_config.R, 03a_table_label_maps.R, 03b_table_blocks.R; tidyverse
# Style:   r_script_style_guide.md (Tier B)

#4. source() and library() -----
# None: the calling script loads tidyverse and sources 00a_config.R.

# Constants -----
DRUG_HISTORY_PRIORITY <- c("Currently used", "Previously used", "Don't know")
DRUG_HISTORY_DEFAULT <- "Never used"
CURRENTLY_USED_LEVEL <- "Currently used"

#5. Function definitions -----
format_respondent_keys <- function(respondents) {
  paste(respondents$country, respondents$respondent_id)
}

# Within the pre-exclusion analysis set, the respondents who reported a
# psychiatric admission within the past 6 months (DE09) must be exactly
# RECENT_ADMISSION_EXCLUDE. Detects drift on data updates.
check_recent_admission_drift <- function(analysis_pre) {
  last_admission_pre <- harmonize_labels(
    analysis_pre$last_psych_admission, LAST_ADMISSION_MAP,
    "last_psych_admission (recent-admission consistency check)"
  )
  is_recent <- !is.na(last_admission_pre) &
    last_admission_pre %in% LAST_ADMISSION_RECENT_LEVELS
  recent_observed <- format_respondent_keys(analysis_pre[is_recent, ])
  recent_expected <- format_respondent_keys(
    build_exclusion_table(RECENT_ADMISSION_EXCLUDE)
  )
  if (!setequal(recent_observed, recent_expected)) {
    stop(sprintf(
      "RECENT_ADMISSION_EXCLUDE drift: observed {%s} vs configured {%s}",
      paste(sort(recent_observed), collapse = ", "),
      paste(sort(recent_expected), collapse = ", ")
    ))
  }
  message(sprintf(
    "Recent-admission consistency OK: %d respondents (%s)",
    length(recent_expected), paste(sort(recent_expected), collapse = ", ")
  ))
}

check_analysis_set_size <- function(analysis) {
  n_total <- nrow(analysis)
  n_de <- sum(analysis$country == "DE")
  n_jp <- sum(analysis$country == "JP")
  is_expected_size <- n_total == ANALYSIS_N_TOTAL && n_de == ANALYSIS_N_DE &&
    n_jp == ANALYSIS_N_JP
  if (!is_expected_size) {
    stop(sprintf(
      paste0(
        "Analysis set size mismatch: total %d (expected %d), ",
        "DE %d (expected %d), JP %d (expected %d)"
      ),
      n_total, ANALYSIS_N_TOTAL, n_de, ANALYSIS_N_DE, n_jp, ANALYSIS_N_JP
    ))
  }
  message(sprintf(
    "Analysis set OK: total %d, Germany %d, Japan %d", n_total, n_de, n_jp
  ))
}

# The recent-admission levels must be empty in the final analysis set.
check_no_recent_admissions <- function(last_admission_values) {
  n_recent_left <- sum(
    last_admission_values %in% LAST_ADMISSION_RECENT_LEVELS,
    na.rm = TRUE
  )
  if (n_recent_left != 0) {
    stop(sprintf(
      "Found %d respondent(s) with an admission within 6 months after exclusion",
      n_recent_left
    ))
  }
  message("No admissions within 6 months remain in the analysis set.")
}

# Analysis set: determinable assigned-drug staircase, minus ANALYSIS_EXCLUDE
# and RECENT_ADMISSION_EXCLUDE. Returns the set and its harmonized
# last-admission labels.
load_table1_analysis_set <- function(individual) {
  analysis_pre <- filter_determinable_assigned(individual)
  check_recent_admission_drift(analysis_pre)

  analysis <- analysis_pre %>%
    anti_join(build_exclusion_table(RECENT_ADMISSION_EXCLUDE),
      by = EXCLUSION_KEYS
    )
  check_analysis_set_size(analysis)

  last_admission_values <- harmonize_labels(
    analysis$last_psych_admission, LAST_ADMISSION_MAP, "last_psych_admission"
  )
  check_no_recent_admissions(last_admission_values)
  list(analysis = analysis, last_admission_values = last_admission_values)
}

# Hierarchy: currently used > previously used > don't know > never used;
# all items missing -> missing.
summarise_drug_history <- function(responses) {
  answered <- responses[!is.na(responses)]
  if (length(answered) == 0) {
    return(NA_character_)
  }
  for (priority_level in DRUG_HISTORY_PRIORITY) {
    if (any(answered == priority_level)) {
      return(priority_level)
    }
  }
  DRUG_HISTORY_DEFAULT
}

# Per-person aggregation of the 9 non-randomized drugs (footnote d).
aggregate_other_drug_history <- function(analysis) {
  other_drug_matrix <- vapply(
    OTHER_DRUG_VARS,
    \(variable) harmonize_labels(analysis[[variable]], DRUG_HISTORY_MAP, variable),
    character(nrow(analysis))
  )
  apply(other_drug_matrix, 1, summarise_drug_history)
}

# Analysis-set counts cited in the text without a table: respondents
# currently using any antipsychotic (at least one of the 12 drug-history
# items answered "currently used") and always-willing respondents.
summarise_analysis_set_counts <- function(analysis) {
  drug_history_matrix <- vapply(
    names(DRUG_HISTORY_VARS),
    \(variable) harmonize_labels(analysis[[variable]], DRUG_HISTORY_MAP, variable),
    character(nrow(analysis))
  )
  is_current_user <- rowSums(drug_history_matrix == CURRENTLY_USED_LEVEL,
    na.rm = TRUE
  ) > 0
  is_always_willing <- analysis$status_assigned == "always_willing"
  counts <- c(sum(is_current_user), sum(is_always_willing))
  tibble(
    measure = c("currently_using_any_antipsychotic", "always_willing"),
    n = counts,
    denominator = nrow(analysis),
    percent = round(100 * counts / nrow(analysis), digits = 1)
  )
}

#6. Executed statements -----
# None (sourced by 03_table1_by_country.R).
