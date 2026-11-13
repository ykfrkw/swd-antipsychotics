#1. Copyright statement -----
# Copyright (c) 2026 the SWD study authors. Released under the MIT License (see LICENSE).

#2. Author -----
# Author: SWD study authors (see LICENSE)
# Date: 2026-11-13 (last revised)

#3. File description -----
# Purpose: Labels, fallback texts and small helpers for the readable
#          per-respondent exports (Japanese and German CSVs): assigned-drug
#          status/SWD, reason texts, FIBSER sum, code labelling, validation
#          and the UTF-8 BOM fix.
# Inputs:  none (functions receive the data)
# Outputs: none (function definitions)
# Depends: 00a_config.R; tidyverse
# Style:   r_script_style_guide.md (Tier B)

#4. source() and library() -----
# None: the calling script loads tidyverse and sources 00a_config.R.

# Constants -----
REASON_FREETEXT_JA <- "自由記述"
REASON_RECENT_JA <- "6か月以内の精神科入院"
REASON_FREETEXT_DE <- "Freitext"
REASON_RECENT_DE <- "stationäre Aufnahme innerhalb der letzten 6 Monate"

# Raw DE09 labels meaning an admission within the past 6 months.
RECENT_LABELS_JA <- c("1ヶ月前", "2−3ヶ月前", "4−6ヶ月前")
RECENT_LABELS_DE <- c("Vor 1 Monat", "Vor 2-3 Monaten", "Vor 4-6 Monaten")

CALCULABLE_JA <- "算出可能"
NOT_CALCULABLE_JA <- "算出不可"
CALCULABLE_DE <- "berechenbar"
NOT_CALCULABLE_DE <- "nicht berechenbar"

# never_willing has no finite SWD; the readable files mark it with this code.
NEVER_WILLING_SWD_CODE <- 99999
MISSING_RESPONSE_CODE <- "-9"
FIBSER_ITEM_MIN <- 1
FIBSER_ITEM_MAX <- 7
FIBSER_TOTAL_MAX <- 18
UTF8_BOM <- as.raw(c(0xef, 0xbb, 0xbf))

STATUS_LABEL_JA <- c(
  completed = "算出完了(completed)",
  always_willing = "常に価値あり(always_willing)",
  never_willing = "価値なし(never_willing)",
  no_response = "無回答(no_response)",
  incomplete = "途中終了(incomplete)",
  not_assigned = "薬剤未割当(not_assigned)"
)

STATUS_LABEL_DE <- c(
  completed = "abgeschlossen (completed)",
  always_willing = "immer bereit (always_willing)",
  never_willing = "nie bereit (never_willing)",
  no_response = "keine Antwort (no_response)",
  incomplete = "unvollständig (incomplete)",
  not_assigned = "kein Medikament zugewiesen (not_assigned)"
)

SEX_LABEL_JA <- c(
  female = "女性",
  male = "男性",
  other_or_prefer_not_to_say = "その他/回答しない"
)

JP_NEVER_FALLBACK <- "再発確率がゼロでも『内服する意義はない』と回答（＝どんな好条件でも内服の価値を認めず、支払意思なし）"
JP_ALWAYS_FALLBACK <- "最大提示条件でも継続/支払意思あり（節点60でも『はい』）"
DE_NEVER_FALLBACK <- paste0(
  "Gab an, dass die Einnahme selbst bei 0% Rückfallrisiko keinen Nutzen hat ",
  "(→ unter keiner Bedingung nutzen-/zahlungsbereit)"
)
DE_ALWAYS_FALLBACK <- "Auch bei maximaler Vorgabe zahlungs-/therapiebereit (bei Knoten 60 „Ja“)"

#5. Function definitions -----
stop_if_missing <- function(path) {
  if (!file.exists(path)) {
    stop("Required file not found: ", path, call. = FALSE)
  }
}

require_columns <- function(survey_data, required_columns, label) {
  missing_columns <- setdiff(required_columns, names(survey_data))
  if (length(missing_columns) > 0) {
    stop(
      label,
      " is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }
}

coalesce_assigned_status <- function(status_ris, status_ari, status_ola) {
  statuses <- c(status_ris, status_ari, status_ola)
  statuses <- statuses[!is.na(statuses) & statuses != "not_assigned"]
  if (length(statuses) == 0) {
    return("not_assigned")
  }
  statuses[[1]]
}

coalesce_assigned_swd <- function(swd_ris, swd_ari, swd_ola) {
  swd_values <- suppressWarnings(as.numeric(c(swd_ris, swd_ari, swd_ola)))
  swd_values <- swd_values[!is.na(swd_values)]
  if (length(swd_values) == 0) {
    return(NA_real_)
  }
  swd_values[[1]]
}

add_assigned_swd <- function(individual) {
  individual %>%
    rowwise() %>%
    mutate(
      assigned_status = coalesce_assigned_status(
        .data$status_ris,
        .data$status_ari,
        .data$status_ola
      ),
      assigned_swd = coalesce_assigned_swd(
        .data$swd_ris,
        .data$swd_ari,
        .data$swd_ola
      )
    ) %>%
    ungroup()
}

nonempty_or_fallback <- function(text, fallback) {
  ifelse(is.na(text) | trimws(text) == "", fallback, text)
}

clean_reason_text <- function(raw_text) {
  reason_text <- as.character(raw_text)
  is_blank <- is.na(reason_text) | reason_text == MISSING_RESPONSE_CODE |
    trimws(reason_text) == ""
  reason_text[is_blank] <- ""
  reason_text
}

make_reason <- function(status, never_text, always_text, always_fallback,
                        never_fallback) {
  case_when(
    status == "never_willing" ~ nonempty_or_fallback(never_text, never_fallback),
    status == "always_willing" ~ nonempty_or_fallback(always_text, always_fallback),
    TRUE ~ ""
  )
}

readable_swd_value <- function(status, swd) {
  ifelse(status == "never_willing", NEVER_WILLING_SWD_CODE, swd)
}

clean_fibser_code <- function(raw_codes) {
  codes <- as.character(raw_codes)
  is_blank <- is.na(codes) | codes == MISSING_RESPONSE_CODE | trimws(codes) == ""
  codes[is_blank] <- NA_character_
  suppressWarnings(as.integer(codes))
}

# FIBSER total = sum of the three items rescaled to 0-6 (empty if any is missing).
sum_fibser_items <- function(duration, severity, interference) {
  codes <- tibble(
    duration = clean_fibser_code(duration),
    severity = clean_fibser_code(severity),
    interference = clean_fibser_code(interference)
  )
  is_invalid <- unlist(lapply(codes, function(item_codes) {
    !is.na(item_codes) &
      (item_codes < FIBSER_ITEM_MIN | item_codes > FIBSER_ITEM_MAX)
  }))
  if (any(is_invalid)) {
    stop("FIBSER source items must be 1-7, -9, or NA.", call. = FALSE)
  }

  fibser_total <- (codes$duration - 1) + (codes$severity - 1) +
    (codes$interference - 1)
  ifelse(rowSums(is.na(codes)) > 0, "", as.character(fibser_total))
}

localize_codes <- function(codes, label_map) {
  labels <- unname(label_map[as.character(codes)])
  labels[is.na(labels)] <- ""
  labels
}

label_codes <- function(raw_codes, label_map) {
  codes <- as.character(raw_codes)
  codes[is.na(codes) | codes == MISSING_RESPONSE_CODE] <- NA_character_
  labels <- unname(label_map[codes])
  labels[is.na(labels)] <- ""
  labels
}

validate_count <- function(label, actual, expected) {
  if (!identical(as.integer(actual), as.integer(expected))) {
    stop(label, " expected ", expected, ", got ", actual, call. = FALSE)
  }
}

validate_fibser_range <- function(label, fibser_totals) {
  totals <- suppressWarnings(as.integer(fibser_totals[fibser_totals != ""]))
  if (any(is.na(totals)) || any(totals < 0 | totals > FIBSER_TOTAL_MAX)) {
    stop(label, " values must be empty or integers from 0 to 18.", call. = FALSE)
  }
  length(totals)
}

ensure_utf8_bom <- function(path) {
  first_bytes <- readBin(path, "raw", n = 3)
  if (identical(first_bytes, UTF8_BOM)) {
    return(invisible(NULL))
  }
  csv_bytes <- readBin(path, "raw", n = file.info(path)$size)
  connection <- file(path, open = "wb")
  on.exit(close(connection), add = TRUE)
  writeBin(c(UTF8_BOM, csv_bytes), connection)
}

#6. Executed statements -----
# None (sourced by 06_readable_export.R).
