#1. Copyright statement -----
# Copyright (c) 2026 the SWD study authors. Released under the MIT License (see LICENSE).

#2. Author -----
# Author: SWD study authors (see LICENSE)
# Date: 2026-11-13 (last revised)

#3. File description -----
# Purpose: Console summary of the readable exports (counts, FIBSER checks and
#          example rows) printed as CSV blocks for manual review.
# Inputs:  none (functions receive the readable tables)
# Outputs: none (writes to the console only)
# Depends: 00a_config.R, 06a_readable_export_helpers.R; tidyverse
# Style:   r_script_style_guide.md (Tier B)

#4. source() and library() -----
# None: the calling script loads tidyverse and sources 00a_config.R.

# Constants -----
N_COMPLETED_EXAMPLES <- 3

#5. Function definitions -----
print_csv_block <- function(title, block_table, has_trailing_blank = TRUE) {
  cat(title, "\n", sep = "")
  write_csv(block_table, stdout(), na = "")
  if (has_trailing_blank) {
    cat("\n")
  }
}

summarise_fibser_range <- function(jp_out, de_out, fibser_counts) {
  jp_totals <- as.integer(jp_out$FIBSER合計[jp_out$FIBSER合計 != ""])
  de_totals <- as.integer(de_out$FIBSER_Summe[de_out$FIBSER_Summe != ""])
  tibble(
    country = c("JP", "DE"),
    nonmissing = c(fibser_counts[["JP"]], fibser_counts[["DE"]]),
    min = c(min(jp_totals), min(de_totals)),
    max = c(max(jp_totals), max(de_totals))
  )
}

# The first complete Japanese FIBSER row, with the sum spelled out.
select_fibser_example <- function(raw_fibser, jp_out) {
  raw_fibser %>%
    filter(
      .data$country == "JP", .data$respondent_id %in% jp_out$CASE,
      .data$fibser_sum != ""
    ) %>%
    transmute(
      country = .data$country,
      CASE = .data$respondent_id,
      DE13 = .data$fibser_duration_code,
      DE14 = .data$fibser_severity_code,
      DE15 = .data$fibser_interference_code,
      FIBSER_sum = .data$fibser_sum,
      manual = paste0(
        "(", .data$fibser_duration_code, "-1)+(",
        .data$fibser_severity_code, "-1)+(",
        .data$fibser_interference_code, "-1)=",
        (.data$fibser_duration_code - 1) +
          (.data$fibser_severity_code - 1) +
          (.data$fibser_interference_code - 1)
      )
    ) %>%
    slice_head(n = 1)
}

select_never_examples <- function(jp_out, de_out) {
  bind_rows(
    jp_out %>%
      filter(.data$判定 == NEVER_WILLING_JA) %>%
      transmute(
        country = "JP", CASE = .data$CASE, swd_value = .data$SWD値,
        reason = .data$理由
      ) %>%
      slice_head(n = 1),
    de_out %>%
      filter(.data$Status == NEVER_WILLING_DE) %>%
      transmute(
        country = "DE", CASE = .data$CASE, swd_value = .data$SWD_Wert,
        reason = .data$Grund
      ) %>%
      slice_head(n = 1)
  )
}

# First row with a real free-text reason (not the fallback). Respondent wording
# is deliberately not hard-coded in the shared code.
select_free_text_example <- function(readable, status_column, status_label,
                                     reason_column, fallback) {
  readable %>%
    filter(
      .data[[status_column]] == status_label,
      .data[[reason_column]] != fallback
    ) %>%
    select(all_of(c("CASE", reason_column))) %>%
    slice_head(n = 1)
}

print_readable_summary <- function(readable, raw_fibser, fibser_counts,
                                   free_text_counts) {
  jp_out <- readable$jp
  de_out <- readable$de
  cat("ROW_COUNTS\n")
  cat("JP,", nrow(jp_out), "\n", sep = "")
  cat("DE,", nrow(de_out), "\n\n", sep = "")

  print_csv_block("CALCULABILITY_COUNTS_JP", jp_out %>% count(.data$SWD算出可否, name = "n"))
  print_csv_block(
    "CALCULABILITY_COUNTS_DE", de_out %>% count(.data$SWD_berechenbar, name = "n")
  )
  print_csv_block("FIBSER_SUMMARY", summarise_fibser_range(jp_out, de_out, fibser_counts))
  print_csv_block(
    "FIBSER_MANUAL_CHECK_JP_FIRST_COMPLETE", select_fibser_example(raw_fibser, jp_out)
  )
  print_csv_block("FREE_TEXT_REASON_COUNTS", tibble(
    item = c(
      "JP always_willing with free text",
      "DE always_willing with free text",
      "DE never_willing with free text"
    ),
    n = unname(free_text_counts)
  ))
  print_csv_block("NEVER_WILLING_SWD_EXAMPLES", select_never_examples(jp_out, de_out))
  print_csv_block("JP_ALWAYS_WILLING_FREE_TEXT_EXAMPLE", select_free_text_example(
    jp_out, "判定", STATUS_LABEL_JA[["always_willing"]], "理由",
    JP_ALWAYS_FALLBACK
  ))
  print_csv_block("DE_ALWAYS_WILLING_FREE_TEXT_EXAMPLE", select_free_text_example(
    de_out, "Status", STATUS_LABEL_DE[["always_willing"]], "Grund",
    DE_ALWAYS_FALLBACK
  ))
  print_csv_block("DE_NEVER_WILLING_FREE_TEXT_EXAMPLE", select_free_text_example(
    de_out, "Status", NEVER_WILLING_DE, "Grund",
    DE_NEVER_FALLBACK
  ))
  print_csv_block(
    "DE_COMPLETED_FIRST3_ALL_COLUMNS",
    de_out %>%
      filter(.data$Status == STATUS_LABEL_DE[["completed"]]) %>%
      slice_head(n = N_COMPLETED_EXAMPLES)
  )
  print_csv_block(
    "JP_NEVER_WILLING_FIRST1",
    jp_out %>% filter(.data$判定 == NEVER_WILLING_JA) %>% slice_head(n = 1),
    has_trailing_blank = FALSE
  )
}

#6. Executed statements -----
# None (sourced by 06_readable_export.R).
