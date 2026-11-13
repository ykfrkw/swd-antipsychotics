#1. Copyright statement -----
# Copyright (c) 2026 the SWD study authors. Released under the MIT License (see LICENSE).

#2. Author -----
# Author: SWD study authors (see LICENSE)
# Date: 2026-11-13 (last revised)

#3. File description -----
# Purpose: Convert the Table 2 SWD median and quartiles into the worthwhile
#          relapse rate, risk difference, odds ratio and standardized mean
#          difference at a given baseline (no-treatment) relapse rate.
# Inputs:  R_SWD/data/processed/table2.csv
# Outputs: R_SWD/data/processed/swd_conversion.csv
# Depends: 00a_config.R (PATH_OUT); tidyverse
# Style:   r_script_style_guide.md (Tier B)

#4. source() and library() -----
# None: the calling script loads tidyverse and sources 00a_config.R.

# Constants -----
# Relapse rate (%) without maintenance treatment assumed by the survey.
BASELINE_RELAPSE_PCT <- 60
CONVERSION_INPUT_COLUMNS <- c("median", "q1", "q3")

#5. Function definitions -----
swd_conversion_metrics <- function(swd, baseline) {
  treatment_risk_pct <- baseline - swd
  no_treatment_risk <- baseline / 100
  treatment_risk <- treatment_risk_pct / 100

  is_valid <- !is.na(swd) &
    !is.na(baseline) &
    baseline > 0 &
    baseline < 100 &
    treatment_risk >= 0 &
    treatment_risk < 1

  no_treatment_odds <- no_treatment_risk / (1 - no_treatment_risk)
  treatment_odds <- treatment_risk / (1 - treatment_risk)
  odds_ratio <- if_else(is_valid, treatment_odds / no_treatment_odds, NA_real_)

  # The SMD uses the logistic-distribution conversion log(OR) * sqrt(3) / pi.
  tibble(
    worthwhile_relapse_rate = if_else(is_valid, treatment_risk_pct, NA_real_),
    risk_difference = if_else(is_valid, -swd / 100, NA_real_),
    risk_difference_pct = if_else(is_valid, -swd, NA_real_),
    odds_ratio = odds_ratio,
    smd = log(odds_ratio) * sqrt(3) / pi
  )
}

convert_swd <- function(median_swd, q1_swd, q3_swd, baseline = BASELINE_RELAPSE_PCT) {
  conversion_input <- tibble(
    baseline = baseline,
    median_swd = median_swd,
    q1_swd = q1_swd,
    q3_swd = q3_swd
  )

  median_metrics <- swd_conversion_metrics(conversion_input$median_swd, conversion_input$baseline)
  q1_metrics <- swd_conversion_metrics(conversion_input$q1_swd, conversion_input$baseline)
  q3_metrics <- swd_conversion_metrics(conversion_input$q3_swd, conversion_input$baseline)

  conversion_input %>%
    bind_cols(
      median_metrics %>% rename_with(\(x) paste0(x, "_median")),
      q1_metrics %>% rename_with(\(x) paste0(x, "_q1")),
      q3_metrics %>% rename_with(\(x) paste0(x, "_q3"))
    )
}

conversion_table_from_table2 <- function(
  table2_path = file.path(PATH_OUT, "table2.csv"),
  output_path = file.path(PATH_OUT, "swd_conversion.csv"),
  baseline = BASELINE_RELAPSE_PCT
) {
  table2 <- read_csv(table2_path, show_col_types = FALSE)

  missing_required <- CONVERSION_INPUT_COLUMNS[!CONVERSION_INPUT_COLUMNS %in% names(table2)]
  if (length(missing_required) > 0) {
    stop(
      "table2.csv is missing required columns: ",
      paste(missing_required, collapse = ", "),
      call. = FALSE
    )
  }

  conversion <- convert_swd(
    median_swd = table2$median,
    q1_swd = table2$q1,
    q3_swd = table2$q3,
    baseline = baseline
  )

  conversion_table <- table2 %>%
    select(any_of(c("group_type", "group", "drug", "n"))) %>%
    bind_cols(conversion)

  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  write_excel_csv(conversion_table, output_path, na = "")

  conversion_table
}

#6. Executed statements -----
# None (sourced by 02_report_outputs.R).
