#1. Copyright statement -----
# Copyright (c) 2026 the SWD study authors. Released under the MIT License (see LICENSE).

#2. Author -----
# Author: SWD study authors (see LICENSE)
# Date: 2026-11-13 (last revised)

#3. File description -----
# Purpose: Build the analysis data set: load the raw survey exports, harmonize
#          the demographics, derive the SWD per respondent and drug track,
#          exclude test respondents and write the individual and summary tables.
# Inputs:  R_SWD/data/raw/ (SoSci Survey exports: data, values, variables,
#          codebook for JP and DE); R_SWD/data/exclusion_ids.csv
# Outputs: R_SWD/data/processed/swd_individual.csv, swd_summary.csv,
#          swd_summary.html, swd_free_text.csv, swd_excluded.csv
#          (and deletes stale copies of the first three in R_SWD/outputs/)
# Depends: 00a_config.R, 00b_de_value_labels.R, 01a_raw_data_loading.R,
#          01b_demographics_harmonization.R, 01c_swd_staircase.R,
#          01d_swd_summary.R, 01e_free_text.R, 01f_test_respondent_exclusion.R;
#          tidyverse, readxl, fs, gt (optional)
# Style:   r_script_style_guide.md (Tier B)
#
# Run from the project root:  Rscript R_SWD/utils/01_analysis_data.R

#4. source() and library() -----
suppressPackageStartupMessages(library(tidyverse))
source(file.path("R_SWD", "utils", "00a_config.R"))
source(file.path(PATH_UTILS, "00b_de_value_labels.R"))
source(file.path(PATH_UTILS, "01a_raw_data_loading.R"))
source(file.path(PATH_UTILS, "01b_demographics_harmonization.R"))
source(file.path(PATH_UTILS, "01c_swd_staircase.R"))
source(file.path(PATH_UTILS, "01d_swd_summary.R"))
source(file.path(PATH_UTILS, "01e_free_text.R"))
source(file.path(PATH_UTILS, "01f_test_respondent_exclusion.R"))

# Constants -----
INDIVIDUAL_FILE <- "swd_individual.csv"
SUMMARY_FILE <- "swd_summary.csv"
SUMMARY_HTML_FILE <- "swd_summary.html"
FREE_TEXT_FILE <- "swd_free_text.csv"
EXCLUDED_FILE <- "swd_excluded.csv"
# Earlier versions wrote these three files to R_SWD/outputs/.
LEGACY_OUTPUT_FILES <- c(INDIVIDUAL_FILE, SUMMARY_FILE, SUMMARY_HTML_FILE)

#5. Function definitions -----
write_summary_html <- function(gt_table, path) {
  if (requireNamespace("gt", quietly = TRUE) && inherits(gt_table, "gt_tbl")) {
    gt::gtsave(gt_table, filename = path)
  } else {
    writeLines(gt_table, con = path, useBytes = TRUE)
  }
}

delete_legacy_outputs <- function() {
  legacy_files <- file.path(PATH_LEGACY_OUT, LEGACY_OUTPUT_FILES)
  existing_files <- legacy_files[fs::file_exists(legacy_files)]

  if (length(existing_files) > 0) {
    fs::file_delete(existing_files)
  }
}

run_analysis_data <- function() {
  check_compute_swd()
  fs::dir_create(PATH_OUT)
  delete_legacy_outputs()

  raw_data <- load_raw_data()
  demographics_all <- harmonize_demographics(raw_data)
  swd_all <- apply_swd_per_person(raw_data$jp_data, raw_data$de_data)
  free_text_all <- make_free_text_table(raw_data)
  exclusions <- identify_test_respondents(raw_data, free_text_all)

  demographics <- filter_excluded_respondents(demographics_all, exclusions)
  swd <- filter_excluded_respondents(swd_all, exclusions)
  free_text <- filter_excluded_respondents(free_text_all, exclusions)
  summary_result <- make_summary(demographics, swd)

  individual_path <- file.path(PATH_OUT, INDIVIDUAL_FILE)
  summary_path <- file.path(PATH_OUT, SUMMARY_FILE)
  html_path <- file.path(PATH_OUT, SUMMARY_HTML_FILE)
  free_text_path <- file.path(PATH_OUT, FREE_TEXT_FILE)
  excluded_path <- file.path(PATH_OUT, EXCLUDED_FILE)

  write_excel_csv(summary_result$individual, individual_path, na = "")
  write_excel_csv(summary_result$summary, summary_path, na = "")
  write_summary_html(summary_result$gt_table, html_path)
  write_excel_csv(free_text, free_text_path, na = "")
  write_excel_csv(exclusions, excluded_path, na = "")

  invisible(list(
    individual_path = individual_path,
    summary_path = summary_path,
    html_path = html_path,
    free_text_path = free_text_path,
    excluded_path = excluded_path,
    n_individual = nrow(summary_result$individual),
    n_summary = nrow(summary_result$summary),
    n_free_text = nrow(free_text),
    n_excluded = nrow(exclusions),
    is_gt_available = requireNamespace("gt", quietly = TRUE)
  ))
}

# A separate step so that warnings from run_analysis_data() are printed
# before this report, as in the console log of earlier runs.
report_analysis_data <- function(pipeline_result) {
  message("SWD pipeline completed.")
  message("individual_rows=", pipeline_result$n_individual)
  message("summary_rows=", pipeline_result$n_summary)
  message("free_text_rows=", pipeline_result$n_free_text)
  message("excluded_rows=", pipeline_result$n_excluded)
  message("gt_available=", pipeline_result$is_gt_available)
  message("outputs=", paste(
    basename(pipeline_result$individual_path),
    basename(pipeline_result$summary_path),
    basename(pipeline_result$html_path),
    basename(pipeline_result$free_text_path),
    basename(pipeline_result$excluded_path),
    sep = ", "
  ))
}

#6. Executed statements -----
pipeline_result <- run_analysis_data()
report_analysis_data(pipeline_result)
