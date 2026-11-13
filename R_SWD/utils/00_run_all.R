#1. Copyright statement -----
# Copyright (c) 2026 the SWD study authors. Released under the MIT License (see LICENSE).

#2. Author -----
# Author: SWD study authors (see LICENSE)
# Date: 2026-11-13 (last revised)

#3. File description -----
# Purpose: Run the full analysis pipeline in order, each entry script in a
#          fresh R process (so no object or random-number state leaks from
#          one script into the next), then the Python table formatter.
# Inputs:  R_SWD/data/raw/ (SoSci exports), R_SWD/data/exclusion_ids.csv
# Outputs: everything under R_SWD/data/processed/ and R_SWD/outputs/
# Depends: every entry script listed in PIPELINE_SCRIPTS; Python 3 with
#          openpyxl for the last step
# Style:   r_script_style_guide.md (Tier B)
#
# Run from the project root:  Rscript R_SWD/utils/00_run_all.R
# (or open swd-antipsychotics.Rproj in RStudio and source this file).

#4. source() and library() -----
# None: each step runs in its own process.

# Constants -----
UTILS_DIR <- file.path("R_SWD", "utils")
PIPELINE_SCRIPTS <- c(
  "01_analysis_data.R",
  "02_report_outputs.R",
  "03_table1_by_country.R",
  "04_table2_subgroups.R",
  "05_table3_regression.R",
  "06_readable_export.R",
  "07_country_diagnostics.R",
  "08_flow_diagram.R",
  "09_additional_descriptives.R",
  "10_holm_adjustment.R",
  "11_interval_regression.R",
  "12_multiple_imputation.R",
  "13_data_dictionary.R",
  "15_fibser_never_willing.R"
)
TABLE_FORMATTER_SCRIPT <- "14_make_tables_xlsx.py"
PYTHON_COMMAND <- Sys.getenv("SWD_PYTHON", unset = "python3")

#5. Function definitions -----
run_step <- function(command, script_name) {
  message(sprintf("==> %s", script_name))
  exit_status <- system2(command, args = shQuote(file.path(UTILS_DIR, script_name)))
  if (exit_status != 0L) {
    stop(sprintf("%s failed (exit status %d)", script_name, exit_status), call. = FALSE)
  }
  invisible(exit_status)
}

run_all <- function() {
  if (!file.exists(file.path(UTILS_DIR, "00a_config.R"))) {
    stop("Run this script from the repository root.", call. = FALSE)
  }
  rscript_command <- file.path(R.home("bin"), "Rscript")
  for (script_name in PIPELINE_SCRIPTS) {
    run_step(rscript_command, script_name)
  }
  run_step(PYTHON_COMMAND, TABLE_FORMATTER_SCRIPT)
  message("Pipeline completed.")
}

#6. Executed statements -----
run_all()
