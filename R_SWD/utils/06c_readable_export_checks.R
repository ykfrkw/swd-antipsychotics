#1. Copyright statement -----
# Copyright (c) 2026 the SWD study authors. Released under the MIT License (see LICENSE).

#2. Author -----
# Author: SWD study authors (see LICENSE)
# Date: 2026-11-13 (last revised)

#3. File description -----
# Purpose: Validate the readable exports against the expected counts and
#          the configured exclusion lists, and count the free-text reasons.
# Inputs:  none (functions receive the readable tables)
# Outputs: none (function definitions; stop() on any mismatch)
# Depends: 00a_config.R, 06a_readable_export_helpers.R; tidyverse
# Style:   r_script_style_guide.md (Tier B)

#4. source() and library() -----
# None: the calling script loads tidyverse and sources 00a_config.R.

# Constants -----
EXPECTED_ROWS <- c(JP = 226, DE = 128)
EXPECTED_CALCULABLE <- c(JP = 141, DE = 101)
EXPECTED_EXCLUDED <- c(JP = 15, DE = 2)
EXPECTED_FREE_TEXT <- c(jp_always = 11, de_always = 5, de_never = 10)
NEVER_WILLING_JA <- STATUS_LABEL_JA[["never_willing"]]
NEVER_WILLING_DE <- STATUS_LABEL_DE[["never_willing"]]

#5. Function definitions -----
check_exclusion_sets <- function(jp_out, de_out) {
  jp_excluded <- as.character(c(ANALYSIS_EXCLUDE$JP, RECENT_ADMISSION_EXCLUDE$JP))
  de_excluded <- as.character(c(ANALYSIS_EXCLUDE$DE, RECENT_ADMISSION_EXCLUDE$DE))
  if (!setequal(jp_out$CASE[jp_out$除外 == "1"], jp_excluded)) {
    stop("JP exclusion CASE set does not match EXCLUDE_JP + RECENT_JP.", call. = FALSE)
  }
  if (!setequal(de_out$CASE[de_out$除外 == "1"], de_excluded)) {
    stop("DE exclusion CASE set does not match EXCLUDE_DE + RECENT_DE.", call. = FALSE)
  }
}

# Free-text exclusions must all be never_willing; recent-admission
# exclusions must all report an admission within the past 6 months.
check_exclusion_reasons <- function(jp_out, de_out) {
  jp_freetext_status <- jp_out$判定[jp_out$除外理由 == REASON_FREETEXT_JA]
  if (!all(jp_freetext_status == NEVER_WILLING_JA)) {
    stop("JP free-text excluded rows include non-never_willing respondents.",
      call. = FALSE
    )
  }
  de_freetext_status <- de_out$Status[de_out$除外理由 == REASON_FREETEXT_DE]
  if (!all(de_freetext_status == NEVER_WILLING_DE)) {
    stop("DE free-text excluded rows include non-never_willing respondents.",
      call. = FALSE
    )
  }
  jp_recent_label <- jp_out$最終退院時期[jp_out$除外理由 == REASON_RECENT_JA]
  if (!all(jp_recent_label %in% RECENT_LABELS_JA)) {
    stop("JP recent-admission excluded rows lack a <=6-month admission label.",
      call. = FALSE
    )
  }
  de_recent_label <- de_out$letzte_Entlassung[de_out$除外理由 == REASON_RECENT_DE]
  if (!all(de_recent_label %in% RECENT_LABELS_DE)) {
    stop("DE recent-admission excluded rows lack a <=6-month admission label.",
      call. = FALSE
    )
  }
  validate_count(
    "JP recent-admission exclusion count",
    sum(jp_out$除外理由 == REASON_RECENT_JA),
    length(RECENT_ADMISSION_EXCLUDE$JP)
  )
  validate_count(
    "DE recent-admission exclusion count",
    sum(de_out$除外理由 == REASON_RECENT_DE),
    length(RECENT_ADMISSION_EXCLUDE$DE)
  )
}

check_analysed_counts <- function(jp_out, de_out) {
  n_jp_analysed <- sum(jp_out$SWD算出可否 == CALCULABLE_JA & jp_out$除外 != "1")
  n_de_analysed <- sum(de_out$SWD_berechenbar == CALCULABLE_DE & de_out$除外 != "1")
  validate_count(
    "JP calculable count excluding flagged rows", n_jp_analysed, ANALYSIS_N_JP
  )
  validate_count(
    "DE calculable count excluding flagged rows", n_de_analysed, ANALYSIS_N_DE
  )
  validate_count(
    "Total calculable count excluding flagged rows",
    n_jp_analysed + n_de_analysed, ANALYSIS_N_TOTAL
  )
  validate_count(
    "JP never_willing SWD value 99999 count",
    sum(jp_out$判定 == NEVER_WILLING_JA & jp_out$SWD値 == NEVER_WILLING_SWD_CODE),
    sum(jp_out$判定 == NEVER_WILLING_JA)
  )
  validate_count(
    "DE never_willing SWD value 99999 count",
    sum(de_out$Status == NEVER_WILLING_DE & de_out$SWD_Wert == NEVER_WILLING_SWD_CODE),
    sum(de_out$Status == NEVER_WILLING_DE)
  )
}

# Returns the number of non-missing FIBSER totals per country.
validate_readable_exports <- function(jp_out, de_out) {
  validate_count("JP row count", nrow(jp_out), EXPECTED_ROWS[["JP"]])
  validate_count("DE row count", nrow(de_out), EXPECTED_ROWS[["DE"]])
  validate_count(
    "JP calculable count", sum(jp_out$SWD算出可否 == CALCULABLE_JA),
    EXPECTED_CALCULABLE[["JP"]]
  )
  validate_count(
    "DE calculable count", sum(de_out$SWD_berechenbar == CALCULABLE_DE),
    EXPECTED_CALCULABLE[["DE"]]
  )
  validate_count(
    "JP exclusion count", sum(jp_out$除外 == "1"), EXPECTED_EXCLUDED[["JP"]]
  )
  validate_count(
    "DE exclusion count", sum(de_out$除外 == "1"), EXPECTED_EXCLUDED[["DE"]]
  )
  fibser_counts <- c(
    JP = validate_fibser_range("JP FIBSER", jp_out$FIBSER合計),
    DE = validate_fibser_range("DE FIBSER", de_out$FIBSER_Summe)
  )
  check_exclusion_sets(jp_out, de_out)
  check_exclusion_reasons(jp_out, de_out)
  check_analysed_counts(jp_out, de_out)

  if (!any(jp_out$判定 == NEVER_WILLING_JA & trimws(jp_out$理由) != "")) {
    stop("JP never_willing reason text was not joined.", call. = FALSE)
  }
  fibser_counts
}

count_free_text <- function(country_source, status, reason_column) {
  sum(
    country_source$assigned_status == status &
      clean_reason_text(country_source[[reason_column]]) != ""
  )
}

count_free_text_reasons <- function(jp_source, de_source) {
  free_text_counts <- c(
    jp_always = count_free_text(jp_source, "always_willing", "reason_always"),
    de_always = count_free_text(de_source, "always_willing", "reason_always"),
    de_never = count_free_text(de_source, "never_willing", "reason_never")
  )
  validate_count(
    "JP always_willing free-text count", free_text_counts[["jp_always"]],
    EXPECTED_FREE_TEXT[["jp_always"]]
  )
  validate_count(
    "DE always_willing free-text count", free_text_counts[["de_always"]],
    EXPECTED_FREE_TEXT[["de_always"]]
  )
  validate_count(
    "DE never_willing free-text count", free_text_counts[["de_never"]],
    EXPECTED_FREE_TEXT[["de_never"]]
  )
  free_text_counts
}

#6. Executed statements -----
# None (sourced by 06_readable_export.R).
