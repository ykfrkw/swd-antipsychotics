#1. Copyright statement -----
# Copyright (c) 2026 the SWD study authors. Released under the MIT License (see LICENSE).

#2. Author -----
# Author: SWD study authors (see LICENSE)
# Date: 2026-11-13 (last revised)

#3. File description -----
# Purpose: Table 3 for the SWD paper (univariable OLS + LASSO, primary
#          specification including never willing) and Appendix Table S2
#          (Section A: sensitivity excluding never willing; Section B: binary
#          logistic SWD > 40). Re-runs the regressions first, so it also
#          regenerates the two regression CSVs in R_SWD/data/processed.
# Inputs:  R_SWD/data/processed/swd_individual.csv
# Outputs: R_SWD/outputs/report/table3_swd_correlates.csv,
#          R_SWD/outputs/report/table_s2_regression_sensitivity.csv,
#          R_SWD/data/processed/swd_regression.csv,
#          R_SWD/data/processed/swd_regression_binary.csv
# Depends: 00a_config.R, 01d_swd_summary.R, 01f_test_respondent_exclusion.R,
#          02d_swd_linear_regression.R, 05a_table3_formatting.R;
#          tidyverse, glmnet
# Style:   r_script_style_guide.md (Tier B)
#
# Run from the project root:  Rscript R_SWD/utils/05_table3_regression.R

#4. source() and library() -----
suppressPackageStartupMessages(library(tidyverse))
source(file.path("R_SWD", "utils", "00a_config.R"))
source(file.path(PATH_UTILS, "01d_swd_summary.R"))
source(file.path(PATH_UTILS, "01f_test_respondent_exclusion.R"))
source(file.path(PATH_UTILS, "02d_swd_linear_regression.R"))
source(file.path(PATH_UTILS, "05a_table3_formatting.R"))

# Constants -----
PRIMARY_SPEC <- "primary_including_never_willing"
SENSITIVITY_SPEC <- "sensitivity_excluding_never_willing"
SCORE_SUPERSCRIPTS <- c("ᵃ", "")

TABLE3_COLUMN_NAMES <- c(
  "variable", "level", "n", "β (95% CI)", "p value", "LASSO coefficient"
)
TABLE_S2_COLUMN_NAMES <- c(
  "variable", "level", "n", "Estimate (95% CI)", "p value", "LASSO coefficient"
)

TABLE3_NOTE <- paste0(
  "Univariable ordinary least squares regressions of the smallest worthwhile ",
  "difference (SWD, 0-60 percentage points), including never willing ",
  "respondents (never willing assigned SWD = 60; primary analysis set ",
  "n = 225; n varies by covariate owing to missing data). LASSO = least ",
  "absolute shrinkage and selection operator; all covariates entered jointly, ",
  "10-fold cross-validation, coefficients at lambda_min; — indicates the ",
  "covariate was not selected. CI = confidence interval."
)

TABLE_S2_NOTE <- paste0(
  "Section A: sensitivity analysis — univariable linear regressions of SWD ",
  "(0-60) excluding never willing respondents (analysis set n = 193; n varies ",
  "by covariate owing to missing data); estimates are beta (95% CI). ",
  "Section B: univariable logistic regressions of SWD greater than 40 ",
  "including never willing respondents (analysis set n = 225); estimates are ",
  "odds ratios (95% CI); the LASSO coefficient is on the log-odds scale. ",
  "LASSO: all covariates entered jointly, 10-fold cross-validation, ",
  "coefficients at lambda_min; — indicates the covariate was not selected. ",
  "CI = confidence interval."
)

SECTION_A_TITLE <- paste0(
  "Section A: Sensitivity analysis — linear regression of SWD excluding ",
  "never willing respondents (β)"
)
SECTION_B_TITLE <- paste0(
  "Section B: Binary outcome — logistic regression of SWD > 40 including ",
  "never willing respondents (odds ratio)"
)

#5. Function definitions -----
run_table3_regression <- function() {
  individual <- read_csv(
    file.path(PATH_OUT, "swd_individual.csv"),
    col_types = cols(.default = col_character()),
    show_col_types = FALSE
  )
  # Also rewrites swd_regression.csv and swd_regression_binary.csv.
  regression <- run_swd_regressions(individual)
  regression_binary <- run_swd_binary_regressions(individual)
  message(sprintf(
    "Regressions rerun: swd_regression %d rows, swd_regression_binary %d rows",
    nrow(regression), nrow(regression_binary)
  ))

  lasso <- list(
    primary = select_lasso(regression, PRIMARY_SPEC),
    sensitivity = select_lasso(regression, SENSITIVITY_SPEC),
    binary = extract_lasso_coefficients(
      regression_binary %>% filter(.data$model == "lasso_logistic")
    )
  )
  write_table3(regression, lasso$primary)
  write_table_s2(regression, regression_binary, lasso)

  report_lasso("primary", lasso$primary)
  report_lasso("sensitivity", lasso$sensitivity)
  report_lasso("binary", lasso$binary)
}

select_univariable <- function(regression, specification) {
  regression %>%
    filter(
      .data$specification == !!specification,
      .data$model == "univariable"
    )
}

select_lasso <- function(regression, specification) {
  lasso_rows <- regression %>%
    filter(.data$specification == !!specification, .data$model == "lasso")
  extract_lasso_coefficients(lasso_rows)
}

write_report_csv <- function(report_table, file_name) {
  report_path <- file.path(REPORT_OUTPUT_DIR, file_name)
  write_excel_csv(report_table, report_path, na = "")
  message(sprintf("Wrote %s (%d rows)", report_path, nrow(report_table)))
}

# Table 3: primary specification (including never willing).
write_table3 <- function(regression, primary_lasso) {
  table3 <- format_regression_rows(
    select_univariable(regression, PRIMARY_SPEC), primary_lasso,
    use_or = FALSE
  )
  names(table3) <- TABLE3_COLUMN_NAMES
  table3 <- bind_rows(
    table3,
    make_note_rows(
      c(SCORE_NOTE, TABLE3_NOTE), SCORE_SUPERSCRIPTS, TABLE3_COLUMN_NAMES
    )
  )
  dir.create(REPORT_OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
  write_report_csv(table3, "table3_swd_correlates.csv")
}

make_section_row <- function(text) {
  section_row <- tibble(
    variable = text, level = "", n = "", est = "", p = "", lasso = ""
  )
  names(section_row) <- TABLE_S2_COLUMN_NAMES
  section_row
}

# Table S2: sensitivity (excluding never willing) + binary logistic (SWD > 40).
write_table_s2 <- function(regression, regression_binary, lasso) {
  sensitivity_rows <- format_regression_rows(
    select_univariable(regression, SENSITIVITY_SPEC), lasso$sensitivity,
    use_or = FALSE
  )
  names(sensitivity_rows) <- TABLE_S2_COLUMN_NAMES

  binary_univariable <- regression_binary %>%
    filter(.data$model == "univariable_logistic")
  binary_rows <- format_regression_rows(
    binary_univariable, lasso$binary,
    use_or = TRUE
  )
  names(binary_rows) <- TABLE_S2_COLUMN_NAMES

  table_s2 <- bind_rows(
    make_section_row(SECTION_A_TITLE),
    sensitivity_rows,
    make_section_row(SECTION_B_TITLE),
    binary_rows,
    make_note_rows(
      c(SCORE_NOTE, TABLE_S2_NOTE), SCORE_SUPERSCRIPTS, TABLE_S2_COLUMN_NAMES
    )
  )
  write_report_csv(table_s2, "table_s2_regression_sensitivity.csv")
}

# Console summary of the LASSO selections (for the report).
report_lasso <- function(label, coefficients) {
  if (length(coefficients) == 0) {
    message(sprintf("LASSO %s: no terms selected", label))
    return(invisible(NULL))
  }
  message(sprintf(
    "LASSO %s: %s", label,
    paste(sprintf("%s=%.2f", names(coefficients), coefficients), collapse = ", ")
  ))
}

#6. Executed statements -----
run_table3_regression()
