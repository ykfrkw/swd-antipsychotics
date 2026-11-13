#1. Copyright statement -----
# Copyright (c) 2026 the SWD study authors. Released under the MIT License (see LICENSE).

#2. Author -----
# Author: SWD study authors (see LICENSE)
# Date: 2026-11-13 (last revised)

#3. File description -----
# Purpose: Holm correction of the univariable regression p-values
#          (peer-review round 1, multiplicity). Each specification is
#          corrected separately over its own family of non-reference terms.
# Inputs:  R_SWD/data/processed/swd_regression.csv (continuous SWD)
#          R_SWD/data/processed/swd_regression_binary.csv (SWD > 40)
# Outputs: R_SWD/outputs/r1_reviewer1/holm_univariable.csv
# Depends: 00a_config.R; tidyverse
# Style:   r_script_style_guide.md (Tier B)
#
# Run from the project root:  Rscript R_SWD/utils/10_holm_adjustment.R

#4. source() and library() -----
suppressPackageStartupMessages(library(tidyverse))
source(file.path("R_SWD", "utils", "00a_config.R"))

# Constants -----
HOLM_OUTPUT_DIR <- file.path(PATH_LEGACY_OUT, "r1_reviewer1")
REGRESSION_INPUT_FILES <- c(
  continuous = "swd_regression.csv",
  binary_swd_above_40 = "swd_regression_binary.csv"
)
UNIVARIABLE_MODEL <- "univariable"
SIGNIFICANCE_THRESHOLD <- 0.05
HOLM_OUTPUT_COLUMNS <- c(
  "outcome", "specification", "covariate", "term", "estimate",
  "conf_low", "conf_high", "n", "p_value", "p_holm",
  "n_terms_in_family", "is_significant_unadjusted", "is_significant_holm"
)

#5. Function definitions -----
run_holm_adjustment <- function() {
  univariable_terms <- bind_rows(
    Map(read_univariable, names(REGRESSION_INPUT_FILES), REGRESSION_INPUT_FILES)
  )
  holm_adjusted <- adjust_holm(univariable_terms)
  dir.create(HOLM_OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)
  output_path <- file.path(HOLM_OUTPUT_DIR, "holm_univariable.csv")
  write_excel_csv(holm_adjusted, output_path, na = "")
  message(sprintf("Wrote %s (%d rows)", output_path, nrow(holm_adjusted)))
}

read_univariable <- function(outcome, file_name) {
  read_csv(file.path(PATH_OUT, file_name), show_col_types = FALSE) %>%
    filter(
      startsWith(.data$model, UNIVARIABLE_MODEL),
      !is.na(.data$p_value)
    ) %>%
    mutate(outcome = outcome)
}

adjust_holm <- function(univariable_terms) {
  univariable_terms %>%
    group_by(.data$outcome, .data$specification) %>%
    mutate(
      n_terms_in_family = n(),
      p_holm = stats::p.adjust(.data$p_value, method = "holm"),
      is_significant_unadjusted = .data$p_value < SIGNIFICANCE_THRESHOLD,
      is_significant_holm = .data$p_holm < SIGNIFICANCE_THRESHOLD
    ) %>%
    ungroup() %>%
    select(all_of(HOLM_OUTPUT_COLUMNS)) %>%
    arrange(.data$outcome, .data$specification, .data$p_value)
}

#6. Executed statements -----
run_holm_adjustment()
