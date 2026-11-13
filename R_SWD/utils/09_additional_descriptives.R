#1. Copyright statement -----
# Copyright (c) 2026 the SWD study authors. Released under the MIT License (see LICENSE).

#2. Author -----
# Author: SWD study authors (see LICENSE)
# Date: 2026-11-13 (last revised)

#3. File description -----
# Purpose: Additional statistics requested in peer-review round 1: SWD
#          distribution descriptives, missing-outcome analysis among eligible
#          starters, covariate missingness in the analysis set, and
#          country-adjusted / conventional multivariable OLS (complete case).
# Inputs:  R_SWD/data/processed/swd_individual.csv
#          R_SWD/data/raw/data_swd_{ja,de}*.xlsx (newest; comprehension items,
#          MAXPAGE)
# Outputs: R_SWD/outputs/r1_reviewer4/
#            descriptives_swd.csv, missing_outcome.csv,
#            missing_covariates.csv, regression_country_adjusted.csv, README.md
# Depends: 00a_config.R, 01a_raw_data_loading.R, 01d_swd_summary.R,
#          01f_test_respondent_exclusion.R, 02d_swd_linear_regression.R,
#          09a_additional_descriptives_helpers.R (sources 09b); tidyverse;
#          sandwich (optional)
# Style:   r_script_style_guide.md (Tier B)
#
# Run from the project root:  Rscript R_SWD/utils/09_additional_descriptives.R

#4. source() and library() -----
# The sourced pipeline files contain UTF-8 Japanese/German labels; under a C
# locale (e.g. non-interactive shells) they are mis-parsed.
if (!l10n_info()$`UTF-8`) invisible(Sys.setlocale("LC_CTYPE", "en_US.UTF-8"))
suppressPackageStartupMessages(library(tidyverse))
source(file.path("R_SWD", "utils", "00a_config.R"))
source(file.path(PATH_UTILS, "01a_raw_data_loading.R"))
source(file.path(PATH_UTILS, "01d_swd_summary.R"))
source(file.path(PATH_UTILS, "01f_test_respondent_exclusion.R"))
source(file.path(PATH_UTILS, "02d_swd_linear_regression.R"))
source(file.path(PATH_UTILS, "09a_additional_descriptives_helpers.R"))

# Constants -----
ADDITIONAL_DESCRIPTIVES_DIR <- file.path(PATH_LEGACY_OUT, "r1_reviewer4")
SPECIFICATIONS <- stats::setNames(
  list(USABLE_SWD_STATUSES, USABLE_SWD_STATUSES_EXCLUDING_NEVER),
  c(PRIMARY_SPECIFICATION, EXCLUDING_NEVER_SPECIFICATION)
)
EXPECTED_WITH_SWD <- 237L
REGRESSION_OUTPUT_COLUMNS <- c(
  "specification", "model", "covariate", "term", "estimate", "conf_low",
  "conf_high", "p_value", "n", "r_squared", "se_hc3", "conf_low_hc3",
  "conf_high_hc3", "p_value_hc3", "note"
)

#5. Function definitions -----
run_additional_descriptives <- function() {
  individual <- read_csv(file.path(PATH_OUT, "swd_individual.csv"),
    show_col_types = FALSE
  )
  analysis_frames <- lapply(SPECIFICATIONS, \(statuses) {
    make_regression_frame(individual, statuses)
  })
  check_analysed_counts(analysis_frames[[PRIMARY_SPECIFICATION]])

  descriptives <- bind_rows(lapply(names(analysis_frames), \(name) {
    describe_swd_by_country(analysis_frames[[name]], name)
  }))
  eligible_starters <- build_eligible_starters(individual)
  missing_outcome <- summarise_missing_outcome(eligible_starters)
  missing_covariates <- summarise_missingness(analysis_frames)
  regression <- fit_country_adjusted_models(analysis_frames)

  dir.create(
    ADDITIONAL_DESCRIPTIVES_DIR,
    recursive = TRUE, showWarnings = FALSE
  )
  write_review_csv(
    descriptives, ADDITIONAL_DESCRIPTIVES_DIR, "descriptives_swd.csv"
  )
  write_review_csv(
    missing_outcome, ADDITIONAL_DESCRIPTIVES_DIR, "missing_outcome.csv"
  )
  write_review_csv(
    missing_covariates, ADDITIONAL_DESCRIPTIVES_DIR, "missing_covariates.csv"
  )
  write_review_csv(
    regression, ADDITIONAL_DESCRIPTIVES_DIR, "regression_country_adjusted.csv"
  )

  readme_lines <- build_readme_lines(
    descriptives, missing_outcome, missing_covariates, regression,
    eligible_starters
  )
  writeLines(readme_lines, file.path(ADDITIONAL_DESCRIPTIVES_DIR, "README.md"),
    useBytes = TRUE
  )
  cat(readme_lines, sep = "\n")
}

summarise_missing_outcome <- function(eligible_starters) {
  reconciliation <- tibble(
    section = "reconciliation_with_flow",
    variable = c("eligible_starters", "with_swd", "without_swd"),
    level = ALL_COUNTRIES_LABEL,
    n_total = c(
      nrow(eligible_starters), sum(eligible_starters$has_swd),
      sum(!eligible_starters$has_swd)
    ),
    expected_manuscript = c(
      EXPECTED_ELIGIBLE_STARTERS, EXPECTED_WITH_SWD,
      EXPECTED_ELIGIBLE_STARTERS - EXPECTED_WITH_SWD
    )
  ) %>%
    mutate(matches_manuscript = .data$n_total == .data$expected_manuscript)
  bind_rows(
    reconciliation,
    summarise_outcome_by(eligible_starters, NULL),
    summarise_outcome_by(eligible_starters, "country"),
    summarise_outcome_by(eligible_starters, "randomized_drug"),
    summarise_stop_stage(eligible_starters)
  )
}

summarise_missingness <- function(analysis_frames) {
  missing_covariates <- bind_rows(lapply(names(analysis_frames), \(name) {
    summarise_missing_covariates(
      analysis_frames[[name]], REGRESSION_COVARIATES, name
    )
  }))
  primary_complete_cases <- missing_covariates %>%
    filter(
      .data$specification == PRIMARY_SPECIFICATION,
      .data$country == ALL_COUNTRIES_LABEL,
      .data$covariate == COMPLETE_CASES_LABEL
    ) %>%
    pull(.data$n_available)
  stopifnot(primary_complete_cases == EXPECTED_COMPLETE_CASES)
  missing_covariates
}

fit_country_adjusted_models <- function(analysis_frames) {
  bind_rows(lapply(names(analysis_frames), \(name) {
    frame <- analysis_frames[[name]]
    run_country_adjusted_models(frame, valid_regression_covariates(frame), name)
  })) %>%
    select(any_of(REGRESSION_OUTPUT_COLUMNS))
}

describe_line <- function(descriptives, name, country) {
  is_selected <- descriptives$specification == name &
    descriptives$country == country
  row <- descriptives[is_selected, ]
  sprintf(
    paste0(
      "%s/%s: n=%d, mean %.1f (SD %.1f), median %.0f [%.0f-%.0f], ",
      "skew %.2f, SW W=%.3f p=%s, %%0=%.1f, %%60=%.1f"
    ),
    name, country, row$n, row$mean, row$sd, row$median, row$q1, row$q3,
    row$skewness, row$shapiro_w, format_p_value(row$shapiro_p), row$pct_at_0,
    row$pct_at_60
  )
}

build_readme_lines <- function(descriptives, missing_outcome,
                               missing_covariates, regression,
                               eligible_starters) {
  country_test <- missing_outcome[missing_outcome$variable == "country", ][1, ]
  drug_test <- missing_outcome[
    missing_outcome$variable == "randomized_drug",
  ][1, ]
  complete_case_counts <- missing_covariates %>%
    filter(
      .data$country == ALL_COUNTRIES_LABEL,
      .data$covariate == COMPLETE_CASES_LABEL
    )
  has_hc3 <- "conf_low_hc3" %in% names(regression)
  c(
    "# Reviewer #4 additional analyses (peer-review round 1)",
    "",
    "Generated by `R_SWD/utils/09_additional_descriptives.R`.",
    "",
    paste0(
      "- `descriptives_swd.csv`: ",
      describe_line(descriptives, PRIMARY_SPECIFICATION, ALL_COUNTRIES_LABEL),
      "; ",
      describe_line(
        descriptives, EXCLUDING_NEVER_SPECIFICATION, ALL_COUNTRIES_LABEL
      ),
      "."
    ),
    sprintf(
      paste0(
        "- `missing_outcome.csv`: %d eligible starters (manuscript %d),",
        " %d with SWD (manuscript %d); missing SWD by country %s p=%s,",
        " by drug %s p=%s; stop stage of non-completers tabulated."
      ),
      nrow(eligible_starters), EXPECTED_ELIGIBLE_STARTERS,
      sum(eligible_starters$has_swd), EXPECTED_WITH_SWD,
      country_test$test, format_p_value(country_test$p_value),
      drug_test$test, format_p_value(drug_test$p_value)
    ),
    sprintf(
      paste0(
        "- `missing_covariates.csv`: complete cases for the 12-covariate",
        " model = %d (primary, of %d) and %d (excluding never willing,",
        " of %d). 'Unknown / prefer not to answer' is a factor level,",
        " not missing, as in 02d_swd_linear_regression.R."
      ),
      complete_case_counts$n_available[1], complete_case_counts$n[1],
      complete_case_counts$n_available[2], complete_case_counts$n[2]
    ),
    paste0(
      "- `regression_country_adjusted.csv`: (a) each covariate + country ",
      "fixed effect; (b) multivariable OLS with all covariates, complete case. ",
      if (has_hc3) {
        "HC3 robust CIs included for (b). "
      } else {
        "HC3 CIs skipped (sandwich not installed). "
      },
      "Covariates constant in the complete cases are dropped from (b) ",
      "(see `note`): ", paste(unique(stats::na.omit(regression$note)),
        collapse = "; "
      ), "."
    )
  )
}

#6. Executed statements -----
run_additional_descriptives()
