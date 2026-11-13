#1. Copyright statement -----
# Copyright (c) 2026 the SWD study authors. Released under the MIT License (see LICENSE).

#2. Author -----
# Author: SWD study authors (see LICENSE)
# Date: 2026-11-13 (last revised)

#3. File description -----
# Purpose: Respondent exclusions. (1) Test respondents, found by ID or by a
#          test keyword in a free-text answer, are removed from the analysis
#          data. (2) The post-hoc analysis exclusions (free-text inconsistency
#          and eligibility-criterion violation) are applied by the figures and
#          regressions.
# Inputs:  none (ID lists come from 00a_config.R)
# Outputs: none (function definitions)
# Depends: 00a_config.R (EXPLICIT_TEST_IDS, TEST_KEYWORD_PATTERN,
#          ANALYSIS_EXCLUDE, RECENT_ADMISSION_EXCLUDE); tidyverse
# Style:   r_script_style_guide.md (Tier B)

#4. source() and library() -----
# None: the calling script loads tidyverse and sources 00a_config.R.

# Constants -----
RESPONDENT_KEY <- c("country", "respondent_id")

#5. Function definitions -----
extract_first_keyword <- function(response) {
  keyword_match <- regexpr(TEST_KEYWORD_PATTERN, response, perl = TRUE)
  match_length <- attr(keyword_match, "match.length")
  keywords <- rep(NA_character_, length(response))
  has_keyword <- keyword_match > 0
  keywords[has_keyword] <- substring(
    response[has_keyword],
    keyword_match[has_keyword],
    keyword_match[has_keyword] + match_length[has_keyword] - 1
  )
  keywords
}

clean_matched_text <- function(response) {
  gsub("[\r\n]+", " ", response)
}

identify_test_respondents <- function(raw, free_text_long) {
  explicit_rows <- EXPLICIT_TEST_IDS %>%
    mutate(
      reason = "explicit_id",
      matched_text = NA_character_
    )

  keyword_rows <- free_text_long %>%
    mutate(keyword = extract_first_keyword(.data$response)) %>%
    filter(!is.na(.data$keyword)) %>%
    transmute(
      country = .data$country,
      respondent_id = .data$respondent_id,
      reason = paste0("keyword:", .data$keyword),
      matched_text = clean_matched_text(.data$response)
    ) %>%
    distinct()

  bind_rows(explicit_rows, keyword_rows) %>%
    arrange(.data$country, .data$respondent_id, .data$reason)
}

excluded_ids <- function(exclusions) {
  exclusions %>%
    distinct(.data$country, .data$respondent_id)
}

filter_excluded_respondents <- function(respondents, exclusions) {
  ids <- excluded_ids(exclusions)

  if (nrow(ids) == 0) {
    return(respondents)
  }

  respondents %>%
    anti_join(ids, by = RESPONDENT_KEY)
}

# Combined post-hoc exclusions: free-text inconsistency (ANALYSIS_EXCLUDE)
# and eligibility-criterion violation (RECENT_ADMISSION_EXCLUDE).
# Formerly defined twice, in 02c_swd_figures.R and 02d_swd_linear_regression.R.
analysis_exclusion_table <- function() {
  exclusion_tables <- lapply(
    list(ANALYSIS_EXCLUDE, RECENT_ADMISSION_EXCLUDE),
    \(exclusions) {
      tibble(
        country = rep(names(exclusions), lengths(exclusions)),
        respondent_id = unlist(exclusions, use.names = FALSE)
      )
    }
  )

  bind_rows(exclusion_tables) %>%
    distinct()
}

# Former name used by 02c_swd_figures.R.
figure_exclusion_table <- function() {
  analysis_exclusion_table()
}

filter_analysis_exclusions <- function(respondents) {
  anti_join(
    respondents,
    analysis_exclusion_table(),
    by = RESPONDENT_KEY
  )
}

#6. Executed statements -----
# None (sourced by 01_analysis_data.R, 02_report_outputs.R and every script
# that sources 02d_swd_linear_regression.R).
