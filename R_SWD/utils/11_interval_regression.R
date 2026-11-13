#1. Copyright statement -----
# Copyright (c) 2026 the SWD study authors. Released under the MIT License (see LICENSE).

#2. Author -----
# Author: SWD study authors (see LICENSE)
# Date: 2026-11-13 (last revised)

#3. File description -----
# Purpose: Sensitivity analysis requested in peer-review round 1: treat the
#          staircase-elicited SWD as interval data (completed (s-5, s];
#          always willing (-Inf, 0]; never willing (60, +Inf)) and fit
#          Gaussian interval regressions: (a) intercept-only, overall and by
#          country, next to observed median / IQR; (b) univariable models for
#          each REGRESSION_COVARIATES entry, side by side with the existing
#          univariable OLS; (c) intercept-only excluding never willing
#          respondents.
# Inputs:  R_SWD/data/processed/swd_individual.csv
# Outputs: R_SWD/outputs/r2_interval_regression/
#            interval_intercept_primary.csv,
#            interval_univariable_vs_ols.csv,
#            interval_intercept_excluding_never_willing.csv,
#            README.md, sessionInfo.txt
# Depends: 00a_config.R, 01a_raw_data_loading.R, 01d_swd_summary.R,
#          01f_test_respondent_exclusion.R, 02d_swd_linear_regression.R,
#          09a_additional_descriptives_helpers.R,
#          11a_interval_regression_helpers.R; tidyverse, survival
# Style:   r_script_style_guide.md (Tier B)
#
# Run from the project root:  Rscript R_SWD/utils/11_interval_regression.R

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
source(file.path(PATH_UTILS, "11a_interval_regression_helpers.R"))

# Constants -----
INTERVAL_REGRESSION_DIR <- file.path(PATH_LEGACY_OUT, "r2_interval_regression")
README_KEY_COVARIATES <- c("country", "randomized_drug")

#5. Function definitions -----
run_interval_regression <- function() {
  verify_interval_mapping()

  individual <- read_csv(file.path(PATH_OUT, "swd_individual.csv"),
    show_col_types = FALSE
  )
  primary_frame <- make_regression_frame(individual, USABLE_SWD_STATUSES) %>%
    add_interval_bounds()
  excluding_never_frame <- individual %>%
    make_regression_frame(USABLE_SWD_STATUSES_EXCLUDING_NEVER) %>%
    add_interval_bounds()
  check_analysed_counts(primary_frame)

  intercept_primary <- summarise_country_intercepts(
    primary_frame, PRIMARY_SPECIFICATION
  )
  intercept_excluding_never <- summarise_country_intercepts(
    excluding_never_frame, EXCLUDING_NEVER_SPECIFICATION
  )
  univariable_comparison <- compare_univariable_models(primary_frame)

  dir.create(INTERVAL_REGRESSION_DIR, recursive = TRUE, showWarnings = FALSE)
  write_review_csv(
    intercept_primary, INTERVAL_REGRESSION_DIR, "interval_intercept_primary.csv"
  )
  write_review_csv(
    univariable_comparison, INTERVAL_REGRESSION_DIR,
    "interval_univariable_vs_ols.csv"
  )
  write_review_csv(
    intercept_excluding_never, INTERVAL_REGRESSION_DIR,
    "interval_intercept_excluding_never_willing.csv"
  )

  readme_lines <- build_readme(
    intercept_primary, intercept_excluding_never, univariable_comparison
  )
  writeLines(readme_lines, file.path(INTERVAL_REGRESSION_DIR, "README.md"),
    useBytes = TRUE
  )
  writeLines(
    utils::capture.output(utils::sessionInfo()),
    file.path(INTERVAL_REGRESSION_DIR, "sessionInfo.txt")
  )
  cat(readme_lines, sep = "\n")
}

compare_univariable_models <- function(primary_frame) {
  covariates <- valid_regression_covariates(primary_frame)
  interval_univariable <- bind_rows(lapply(covariates, \(covariate) {
    run_univariable_interval(primary_frame, covariate)
  }))
  ols_univariable <- bind_rows(lapply(covariates, \(covariate) {
    run_univariable_regression(primary_frame, covariate, PRIMARY_SPECIFICATION)
  }))
  univariable_comparison <- compare_with_ols(
    interval_univariable, ols_univariable
  )
  stopifnot(all(
    univariable_comparison$interval_n == univariable_comparison$ols_n
  ))
  univariable_comparison
}

describe_intercept_line <- function(intercept_table, country) {
  row <- intercept_table[intercept_table$country == country, ]
  sprintf(
    paste0(
      "%s: n=%d, interval-regression mean %.1f (95%% CI %.1f to %.1f), ",
      "SD %.1f; observed median %.0f [IQR %.0f-%.0f]"
    ),
    country, row$n, row$interval_mean, row$interval_mean_conf_low,
    row$interval_mean_conf_high, row$interval_sd, row$observed_median,
    row$observed_q1, row$observed_q3
  )
}

describe_term_line <- function(comparison, term_name) {
  row <- comparison[comparison$term == term_name, ]
  sprintf(
    paste0(
      "%s: interval %.1f (%.1f to %.1f; p=%s) vs OLS %.1f ",
      "(%.1f to %.1f; p=%s)"
    ),
    term_name, row$interval_estimate, row$interval_conf_low,
    row$interval_conf_high, format_p_value(row$interval_p_value),
    row$ols_estimate, row$ols_conf_low, row$ols_conf_high,
    format_p_value(row$ols_p_value)
  )
}

describe_intercept_lines <- function(intercept_table) {
  paste0("- ", vapply(intercept_table$country, \(country) {
    describe_intercept_line(intercept_table, country)
  }, character(1)))
}

describe_manuscript_summary <- function(primary, excluding_never, comparison) {
  is_de <- primary$country == "DE"
  is_jp <- primary$country == "JP"
  c(
    sprintf(
      paste0(
        "Treating each respondent's SWD as an interval defined by the last ",
        "'worthwhile' and first 'not worthwhile' answers of the staircase ",
        "(with always willing and never willing respondents left- and ",
        "right-censored), a Gaussian interval regression estimated a mean SWD ",
        "of %.1f percentage points (95%% CI %.1f to %.1f; SD %.1f) overall, ",
        "%.1f (%.1f to %.1f) in Germany and %.1f (%.1f to %.1f) in Japan."
      ),
      primary$interval_mean[1], primary$interval_mean_conf_low[1],
      primary$interval_mean_conf_high[1], primary$interval_sd[1],
      primary$interval_mean[is_de], primary$interval_mean_conf_low[is_de],
      primary$interval_mean_conf_high[is_de],
      primary$interval_mean[is_jp], primary$interval_mean_conf_low[is_jp],
      primary$interval_mean_conf_high[is_jp]
    ),
    sprintf(
      paste0(
        "Excluding never willing respondents gave a mean of %.1f (%.1f to ",
        "%.1f). Univariable interval regressions agreed with the OLS analyses ",
        "in direction and statistical significance for %d of %d terms."
      ),
      excluding_never$interval_mean[1],
      excluding_never$interval_mean_conf_low[1],
      excluding_never$interval_mean_conf_high[1],
      sum(comparison$is_same_conclusion), nrow(comparison)
    )
  )
}

build_readme <- function(primary, excluding_never, comparison) {
  discordant_terms <- comparison$term[!comparison$is_same_conclusion]
  key_terms <- comparison$term[comparison$covariate %in% README_KEY_COVARIATES]
  discordant_text <- if (length(discordant_terms)) {
    paste(discordant_terms, collapse = ", ")
  } else {
    "none"
  }
  c(
    "# Reviewer #2: interval regression of the SWD (peer-review round 1)",
    "",
    "Generated by `R_SWD/utils/11_interval_regression.R`.",
    "Model: `survival::survreg(Surv(lower, upper, type = 'interval2') ~ ...,",
    "dist = 'gaussian')`. Completed staircase: (s-5, s]; always willing:",
    "left-censored at 0; never willing: right-censored above 60.",
    "Wald 95% CIs; SD CI back-transformed from log(scale).",
    "",
    "## (a) Intercept-only, primary analysis set",
    describe_intercept_lines(primary),
    "",
    "## (c) Intercept-only, excluding never willing",
    describe_intercept_lines(excluding_never),
    "",
    "## (b) Univariable interval regression vs univariable OLS",
    paste0("- ", vapply(key_terms, \(term_name) {
      describe_term_line(comparison, term_name)
    }, character(1))),
    sprintf(
      paste0(
        "- %d of %d terms agree in direction and in significance ",
        "at p<0.05. Discordant: %s."
      ),
      sum(comparison$is_same_conclusion), nrow(comparison), discordant_text
    ),
    "",
    "## Manuscript-ready summary",
    "",
    "(Draft; edit numbers from the CSVs if the analysis set changes.)",
    "",
    describe_manuscript_summary(primary, excluding_never, comparison)
  )
}

#6. Executed statements -----
run_interval_regression()
