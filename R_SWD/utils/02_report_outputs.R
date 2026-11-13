#1. Copyright statement -----
# Copyright (c) 2026 the SWD study authors. Released under the MIT License (see LICENSE).

#2. Author -----
# Author: SWD study authors (see LICENSE)
# Date: 2026-11-13 (last revised)

#3. File description -----
# Purpose: Core report from the individual table: Table 1 and Table 2 by drug
#          arm, Figures 1 and 2, the linear and logistic SWD regressions and
#          the SWD-to-effect-size conversion.
# Inputs:  R_SWD/data/processed/swd_individual.csv (from 01_analysis_data.R)
# Outputs: R_SWD/data/processed/table1.csv, table2.csv, swd_regression.csv,
#          swd_regression_binary.csv, swd_conversion.csv;
#          R_SWD/outputs/report/figure1_swd_distribution.{png,pdf},
#          figure2_swd_by_drug.{png,pdf}
# Depends: 00a_config.R, 01d_swd_summary.R, 01f_test_respondent_exclusion.R,
#          02a_table1_by_drug.R, 02b_table2_by_drug.R, 02c_swd_figures.R,
#          02d_swd_linear_regression.R (sources 02e and 02g),
#          02f_swd_conversion.R; tidyverse, glmnet
# Style:   r_script_style_guide.md (Tier B)
#
# Run from the project root:  Rscript R_SWD/utils/02_report_outputs.R

#4. source() and library() -----
suppressPackageStartupMessages(library(tidyverse))
source(file.path("R_SWD", "utils", "00a_config.R"))
source(file.path(PATH_UTILS, "01d_swd_summary.R"))
source(file.path(PATH_UTILS, "01f_test_respondent_exclusion.R"))
source(file.path(PATH_UTILS, "02a_table1_by_drug.R"))
source(file.path(PATH_UTILS, "02b_table2_by_drug.R"))
source(file.path(PATH_UTILS, "02c_swd_figures.R"))
source(file.path(PATH_UTILS, "02d_swd_linear_regression.R"))
source(file.path(PATH_UTILS, "02f_swd_conversion.R"))

# Constants -----
PROCESSED_OUTPUT_FILES <- c(
  "table1.csv",
  "table2.csv",
  "swd_regression.csv",
  "swd_regression_binary.csv",
  "swd_conversion.csv"
)
REPORT_OUTPUT_FILES <- c(
  "figure1_swd_distribution.png",
  "figure1_swd_distribution.pdf",
  "figure2_swd_by_drug.png",
  "figure2_swd_by_drug.pdf"
)

#5. Function definitions -----
run_reporting <- function() {
  individual_path <- file.path(PATH_OUT, "swd_individual.csv")
  individual <- read_csv(individual_path, show_col_types = FALSE)

  table1 <- make_table1(individual)
  table2 <- make_table2(individual)
  figure1 <- fig_swd_distribution(individual)
  figure2 <- fig_swd_violin(individual)
  regression <- run_swd_regressions(individual)
  binary_regression <- run_swd_binary_regressions(individual)
  conversion <- conversion_table_from_table2()

  artifact_paths <- c(
    file.path(PATH_OUT, PROCESSED_OUTPUT_FILES),
    file.path(REPORT_OUTPUT_DIR, REPORT_OUTPUT_FILES)
  )

  reporting_result <- list(
    table1 = table1,
    table2 = table2,
    fig1 = figure1,
    fig2 = figure2,
    regression = regression,
    binary_regression = binary_regression,
    conversion = conversion,
    artifact_paths = artifact_paths
  )

  message("SWD reporting completed.")
  message("individual_rows=", nrow(individual))
  message("table1_rows=", nrow(table1))
  message("table2_rows=", nrow(table2))
  message("regression_rows=", nrow(regression))
  message("binary_regression_rows=", nrow(binary_regression))
  message("conversion_rows=", nrow(conversion))
  message("outputs=", paste(basename(artifact_paths), collapse = ", "))

  invisible(reporting_result)
}

#6. Executed statements -----
reporting_result <- run_reporting()
