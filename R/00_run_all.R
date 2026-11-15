# 1. Copyright statement -----
# Copyright (c) 2026 Yuki Furukawa. Released under the MIT License (see LICENSE).

# 2. Author -----
# Author: SWD study authors (see LICENSE)
# Date: 2026-11-14 (last revised)

# 3. File description -----
# Purpose: Run the whole analysis in order, each script in a fresh R process
#          (so no object or random-number state leaks from one script into
#          the next), then save the session information once.
# Inputs:  data/raw/ (SoSci exports), data/exclusion_ids.csv
# Outputs: data/processed/ and outputs/ (see README.md), outputs/sessionInfo.txt
# Depends: every script listed in ANALYSIS_SCRIPTS
#
# Run from the project root:  Rscript R/00_run_all.R
# (or open swd-antipsychotics.Rproj in RStudio and source this file).

# 4. source() and library() -----
# None: each step runs in its own process.

# Constants -----
SCRIPT_DIR <- "R"
ANALYSIS_SCRIPTS <- c(
  "01_prepare_data.R",
  "02_descriptives.R",
  "03_regression.R",
  "04_interval_regression.R",
  "05_multiple_imputation.R",
  "06_fibser_never_willing.R",
  "07_figures.R"
)
ANALYSIS_PACKAGES <- c(
  "tidyverse", "here", "readxl", "fs", "glmnet", "survival", "mice", "sandwich"
)
SESSION_INFO_PATH <- file.path("outputs", "sessionInfo.txt")

# 5. Function definitions -----
run_script <- function(script_name) {
  message(sprintf("==> %s", script_name))
  exit_status <- system2(
    file.path(R.home("bin"), "Rscript"),
    args = shQuote(file.path(SCRIPT_DIR, script_name))
  )
  if (exit_status != 0L) {
    stop(sprintf("%s failed (exit status %d)", script_name, exit_status), call. = FALSE)
  }
}

save_session_info <- function() {
  invisible(lapply(ANALYSIS_PACKAGES, loadNamespace))
  writeLines(utils::capture.output(utils::sessionInfo()), SESSION_INFO_PATH)
}

run_all <- function() {
  if (!file.exists(file.path(SCRIPT_DIR, "00_config.R"))) {
    stop("Run this script from the repository root.", call. = FALSE)
  }
  for (script_name in ANALYSIS_SCRIPTS) {
    run_script(script_name)
  }
  save_session_info()
  message("Analysis completed.")
}

# 6. Executed statements -----
run_all()
