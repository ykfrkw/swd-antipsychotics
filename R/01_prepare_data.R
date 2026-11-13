# 1. Copyright statement -----
# Copyright (c) 2026 the SWD study authors. Released under the MIT License (see LICENSE).

# 2. Author -----
# Author: SWD study authors (see LICENSE)
# Date: 2026-11-14 (last revised)

# 3. File description -----
# Purpose: Build the analysis data set: load the raw survey exports,
#          harmonize the demographics of the two survey versions, derive the
#          SWD of every respondent from the staircase and remove test
#          respondents.
# Inputs:  data/raw/ (SoSci Survey exports: data, values, variables for JP
#          and DE); data/exclusion_ids.csv
# Outputs: data/processed/swd_individual.csv (one row per respondent),
#          swd_free_text.csv (open answers), swd_excluded.csv (test
#          respondents removed)
# Depends: 00_config.R, 00b_de_value_labels.R, 01a_raw_data.R,
#          01b_demographics.R, 01c_staircase_swd.R; tidyverse, readxl, fs
# Style:   r_script_style_guide.md (Tier B)
#
# Run from the project root:  Rscript R/01_prepare_data.R
#
# Analysis
# Each respondent was randomized (RA01) to one antipsychotic and answered a
# staircase of "would you take it?" questions at relapse rates between 0% and
# 60% (relapse without treatment = 60%). The SWD is the smallest reduction in
# the one-year relapse rate (percentage points, 0-60, in steps of 5) for which
# the respondent would take the drug, read off the terminal node of the
# staircase tree (STAIRCASE_TREE in 00_config.R). Respondents who would take
# it even at 60% (no reduction) are "always willing" (SWD = 0); those who
# would not take it even at 0% are "never willing" (SWD = 60). Demographic
# items are harmonized across the Japanese and German versions (education and
# employment to three levels, ordinal scores for age band, education, onset
# age and admissions; FIBSER total = sum of the three items, 0-18). Test
# respondents (by ID or a test keyword in a free-text answer) are removed
# here; the post hoc analysis exclusions are applied by the analysis scripts.

# 4. source() and library() -----
suppressPackageStartupMessages(library(tidyverse))
source(file.path("R", "00_config.R"))
source(file.path(PATH_R, "00b_de_value_labels.R"))
source(file.path(PATH_R, "01a_raw_data.R"))
source(file.path(PATH_R, "01b_demographics.R"))
source(file.path(PATH_R, "01c_staircase_swd.R"))

# 5. Function definitions -----
prepare_analysis_data <- function() {
  check_compute_swd()
  fs::dir_create(PATH_PROCESSED)
  raw_data <- load_raw_data()
  demographics_all <- harmonize_demographics(raw_data)
  swd_all <- apply_swd_per_person(raw_data$jp_data, raw_data$de_data)
  free_text_all <- make_free_text_table(raw_data)
  exclusions <- identify_test_respondents(raw_data, free_text_all)
  individual <- make_individual_table(
    filter_excluded_respondents(demographics_all, exclusions),
    filter_excluded_respondents(swd_all, exclusions)
  )
  free_text <- filter_excluded_respondents(free_text_all, exclusions)
  write_excel_csv(individual, INDIVIDUAL_PATH, na = "")
  write_excel_csv(free_text, file.path(PATH_PROCESSED, "swd_free_text.csv"), na = "")
  write_excel_csv(exclusions, file.path(PATH_PROCESSED, "swd_excluded.csv"), na = "")
  message(sprintf(
    "Analysis data: %d respondents, %d free-text answers, %d test exclusions.",
    nrow(individual), nrow(free_text), nrow(exclusions)
  ))
}

# 6. Executed statements -----
prepare_analysis_data()
