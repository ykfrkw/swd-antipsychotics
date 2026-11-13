#1. Copyright statement -----
# Copyright (c) 2026 the SWD study authors. Released under the MIT License (see LICENSE).

#2. Author -----
# Author: SWD study authors (see LICENSE)
# Date: 2026-11-13 (last revised)

#3. File description -----
# Purpose: Multiple-imputation sensitivity analysis requested in peer-review
#          round 2: impute the SWD of the 51 eligible starters without one
#          (bounded PMM; by country and pooled with country + drug), pool
#          median / mean / % accepting with Rubin's rules, sensitivity
#          without the comprehension-check failures, delta-adjusted MNAR
#          tipping point and bound-respecting best/worst case; secondary
#          covariate MI for the 12-covariate OLS; extra numbers for the
#          response letter.
# Inputs:  R_SWD/data/processed/swd_individual.csv
#          R_SWD/data/raw/data_swd_{ja,de}*.xlsx (newest; staircase answers,
#          comprehension items)
# Outputs: R_SWD/outputs/r2_reviewer5/
#            population_counts.csv, staircase_stopper_bounds.csv,
#            mi_pooled_estimates.csv, mi_results_table.csv (+ .docx),
#            mi_per_imputation_medians.csv, covariate_mi_vs_complete_case.csv,
#            covariate_mi_logged_events.csv, extra_numbers.csv, README.md,
#            sessionInfo.txt
# Depends: 00a_config.R, 01a_raw_data_loading.R, 01d_swd_summary.R,
#          01f_test_respondent_exclusion.R, 02d_swd_linear_regression.R,
#          09a_additional_descriptives_helpers.R, 12a_mi_helpers.R,
#          12b_mi_estimation.R, 12c_secondary_descriptives.R,
#          12d_mi_readme.R; tidyverse, mice, sandwich,
#          flextable + officer (optional, docx)
# Style:   r_script_style_guide.md (Tier B)
#
# Run from the project root:  Rscript R_SWD/utils/12_multiple_imputation.R

#4. source() and library() -----
# Sourced pipeline files contain UTF-8 labels; a C locale mis-parses them.
if (!l10n_info()$`UTF-8`) invisible(Sys.setlocale("LC_CTYPE", "en_US.UTF-8"))
suppressPackageStartupMessages(library(tidyverse))
source(file.path("R_SWD", "utils", "00a_config.R"))
source(file.path(PATH_UTILS, "01a_raw_data_loading.R"))
source(file.path(PATH_UTILS, "01d_swd_summary.R"))
source(file.path(PATH_UTILS, "01f_test_respondent_exclusion.R"))
source(file.path(PATH_UTILS, "02d_swd_linear_regression.R"))
source(file.path(PATH_UTILS, "09a_additional_descriptives_helpers.R"))
source(file.path(PATH_UTILS, "12a_mi_helpers.R"))
source(file.path(PATH_UTILS, "12b_mi_estimation.R"))
source(file.path(PATH_UTILS, "12c_secondary_descriptives.R"))
source(file.path(PATH_UTILS, "12d_mi_readme.R"))

# Constants -----
MULTIPLE_IMPUTATION_DIR <- file.path(PATH_LEGACY_OUT, "r2_reviewer5")
IMPUTATIONS <- 50L
MI_SEED <- 20261113L
COVARIATE_MI_SEED <- 20261114L
EXPECTED_COUNTS <- c(
  population = 276L, analysed = ANALYSIS_N_TOTAL, missing = 51L,
  missing_JP = 42L, missing_DE = 9L, eligible = EXPECTED_ELIGIBLE_STARTERS,
  complete_cases = EXPECTED_COMPLETE_CASES, comprehension_failed = 15L,
  population_comprehension = 261L
)
EXPECTED_COMPLETE_CASE_MEDIAN <- c(All = 15, JP = 27.5) # Table 2
LABEL_COMPLETE_CASE <- "Complete case"
LABEL_MI_COUNTRY <- "MI, imputed separately by country"
LABEL_MI_POOLED <- "MI, pooled (country + drug)"
LABEL_MI_COMPREHENSION <-
  "MI by country, excluding 15 comprehension-check failures"
LABEL_BEST_CASE <- "Best case: non-completers SWD = 0 (stoppers: lower bound)"
LABEL_WORST_CASE <-
  "Worst case: non-completers SWD = 60 (stoppers: upper bound)"
LOGGED_EVENT_COLUMNS <- tibble(
  it = integer(), im = integer(), dep = character(), meth = character(),
  out = character()
)
RESULTS_DOCX_FONT_SIZE <- 7

#5. Function definitions -----
run_multiple_imputation <- function() {
  dir.create(MULTIPLE_IMPUTATION_DIR, recursive = TRUE, showWarnings = FALSE)
  individual <- read_csv(file.path(PATH_OUT, "swd_individual.csv"),
    show_col_types = FALSE
  )
  analysed_frame <- make_regression_frame(individual, USABLE_SWD_STATUSES)
  eligible_starters <- build_eligible_starters(individual)
  inputs <- prepare_population(individual, analysed_frame, eligible_starters)

  outcome_results <- run_outcome_imputation(
    inputs$population, inputs$population_comprehension
  )
  covariate_comparison <- run_covariate_imputation(analysed_frame)

  extra_table <- build_extra_numbers(
    analysed_frame, individual, eligible_starters
  ) %>%
    tidy_extra_numbers()
  write_mi_csv(extra_table, "extra_numbers.csv")
  stopifnot(extract_number(extra_table, "swd by country", "JP", "median") ==
    EXPECTED_COMPLETE_CASE_MEDIAN[["JP"]])

  readme_lines <- build_readme_lines(list(
    imputations = IMPUTATIONS, counts = inputs$population_counts,
    pooled = outcome_results$pooled,
    results_table = outcome_results$results_table,
    partial_bounds = inputs$partial_bounds,
    per_imputation_medians = outcome_results$per_imputation_medians,
    complete_case = outcome_results$complete_case,
    covariate_comparison = covariate_comparison, extra_table = extra_table
  ))
  writeLines(readme_lines, file.path(MULTIPLE_IMPUTATION_DIR, "README.md"),
    useBytes = TRUE
  )
  writeLines(
    capture.output(sessionInfo()),
    file.path(MULTIPLE_IMPUTATION_DIR, "sessionInfo.txt")
  )
  cat(readme_lines, sep = "\n")
}

write_mi_csv <- function(table, file_name) {
  write_review_csv(table, MULTIPLE_IMPUTATION_DIR, file_name)
}

# Imputation population with the partial-answer bounds of the staircase
# stoppers; writes and checks population_counts.csv and the stopper bounds.
prepare_population <- function(individual, analysed_frame, eligible_starters) {
  staircase_answers <- map(COUNTRY_CODES, read_assigned_staircase) %>%
    bind_rows()
  stopper_ids <- eligible_starters %>%
    filter(.data$stop_stage == STOP_STAGE_STAIRCASE) %>%
    select("country", "respondent_id", "randomized_drug")
  partial_bounds <- staircase_answers %>%
    semi_join(stopper_ids, by = RESPONDENT_KEY) %>%
    derive_partial_bounds() %>%
    left_join(stopper_ids, by = RESPONDENT_KEY)
  population <- build_imputation_population(
    eligible_starters, analysed_frame, partial_bounds
  )
  population_comprehension <- exclude_comprehension_failures(population)
  population_counts <- count_population(
    population, population_comprehension, eligible_starters, analysed_frame
  )
  write_mi_csv(
    enframe(population_counts, name = "count", value = "n") %>%
      mutate(expected = EXPECTED_COUNTS[.data$count]),
    "population_counts.csv"
  )
  stopifnot(
    all(population_counts == EXPECTED_COUNTS[names(population_counts)]),
    identical(is.na(population$swd), !population$has_swd),
    sum(population$has_swd) == sum(population_comprehension$has_swd),
    nrow(partial_bounds) == sum(population$is_staircase_stopper),
    all(partial_bounds$swd_lower <= partial_bounds$swd_upper)
  )
  write_mi_csv(partial_bounds, "staircase_stopper_bounds.csv")
  list(
    population = population,
    population_comprehension = population_comprehension,
    population_counts = population_counts, partial_bounds = partial_bounds
  )
}

count_population <- function(population, population_comprehension,
                             eligible_starters, analysed_frame) {
  is_missing <- !population$has_swd
  complete_columns <- analysed_frame[, c("swd", REGRESSION_COVARIATES)]
  c(
    population = nrow(population), analysed = sum(population$has_swd),
    missing = sum(is_missing),
    missing_JP = sum(is_missing & population$country == "JP"),
    missing_DE = sum(is_missing & population$country == "DE"),
    eligible = nrow(eligible_starters),
    complete_cases = sum(stats::complete.cases(complete_columns)),
    comprehension_failed = nrow(population) - nrow(population_comprehension),
    population_comprehension = nrow(population_comprehension)
  )
}

# Imputes the missing SWD, pools every analysis and writes the outcome CSVs
# (and the docx table when flextable and officer are installed).
run_outcome_imputation <- function(population, population_comprehension) {
  datasets_country <- impute_by_country(population, IMPUTATIONS, MI_SEED)
  datasets_pooled <- impute_pooled(population, IMPUTATIONS, MI_SEED)
  datasets_comprehension <- impute_by_country(
    population_comprehension, IMPUTATIONS, MI_SEED
  )
  assert_imputations_admissible(
    c(datasets_country, datasets_pooled, datasets_comprehension)
  )

  estimates_country <- estimate_datasets(datasets_country)
  estimates_pooled <- estimate_datasets(datasets_pooled)
  estimates_comprehension <- estimate_datasets(datasets_comprehension)
  complete_case <- population %>% filter(.data$has_swd)
  best_case <- fill_non_completers(population, "swd_lower")
  worst_case <- fill_non_completers(population, "swd_upper")
  pooled <- bind_rows(
    pool_estimates(estimate_dataset(complete_case), LABEL_COMPLETE_CASE),
    pool_estimates(estimates_country, LABEL_MI_COUNTRY),
    pool_estimates(estimates_pooled, LABEL_MI_POOLED),
    pool_estimates(estimates_comprehension, LABEL_MI_COMPREHENSION),
    run_delta_analyses(datasets_country),
    pool_estimates(estimate_dataset(best_case), LABEL_BEST_CASE),
    pool_estimates(estimate_dataset(worst_case), LABEL_WORST_CASE)
  )
  stopifnot(
    complete_case_median(pooled, ALL_COUNTRIES_LABEL) ==
      EXPECTED_COMPLETE_CASE_MEDIAN[["All"]],
    complete_case_median(pooled, "JP") ==
      EXPECTED_COMPLETE_CASE_MEDIAN[["JP"]]
  )
  analysis_order <- unique(pooled$analysis)
  results_table <- format_results_table(pooled) %>%
    arrange(match(.data$analysis, analysis_order))
  per_imputation_medians <- bind_rows(
    tally_imputation_medians(estimates_country, LABEL_MI_COUNTRY),
    tally_imputation_medians(estimates_pooled, LABEL_MI_POOLED),
    tally_imputation_medians(
      estimates_comprehension, LABEL_MI_COMPREHENSION
    )
  )
  write_mi_csv(pooled, "mi_pooled_estimates.csv")
  write_mi_csv(results_table, "mi_results_table.csv")
  write_mi_csv(per_imputation_medians, "mi_per_imputation_medians.csv")
  save_results_docx(
    results_table, file.path(MULTIPLE_IMPUTATION_DIR, "mi_results_table.docx")
  )
  list(
    pooled = pooled, results_table = results_table,
    per_imputation_medians = per_imputation_medians,
    complete_case = complete_case
  )
}

run_covariate_imputation <- function(analysed_frame) {
  covariate_comparison <- compare_covariate_models(
    analysed_frame, valid_regression_covariates(analysed_frame), IMPUTATIONS,
    COVARIATE_MI_SEED
  )
  write_mi_csv(
    bind_rows(LOGGED_EVENT_COLUMNS, covariate_comparison$logged_events),
    "covariate_mi_logged_events.csv"
  )
  unexplained_events <- unexplained_logged_events(covariate_comparison)
  if (nrow(unexplained_events) > 0) {
    print(unexplained_events)
    stop(
      "Unexplained mice logged events in the covariate MI; inspect ",
      "covariate_mi_logged_events.csv before using the results."
    )
  }
  stopifnot(covariate_comparison$table$cc_n[1] ==
    EXPECTED_COUNTS[["complete_cases"]])
  write_mi_csv(covariate_comparison$table, "covariate_mi_vs_complete_case.csv")
  covariate_comparison
}

save_results_docx <- function(results_table, path) {
  has_docx_tools <- requireNamespace("flextable", quietly = TRUE) &&
    requireNamespace("officer", quietly = TRUE)
  if (!has_docx_tools) {
    return(invisible(FALSE))
  }
  table_docx <- flextable::flextable(results_table) %>%
    flextable::fontsize(size = RESULTS_DOCX_FONT_SIZE, part = "all") %>%
    flextable::autofit()
  flextable::save_as_docx(
    table_docx,
    path = path,
    pr_section = officer::prop_section(
      page_size = officer::page_size(orient = "landscape")
    )
  )
  invisible(TRUE)
}

complete_case_median <- function(pooled, subset_name) {
  pooled %>%
    filter(
      .data$analysis == LABEL_COMPLETE_CASE,
      .data$subset == subset_name, .data$estimand == "median"
    ) %>%
    pull(.data$estimate)
}

#6. Executed statements -----
run_multiple_imputation()
