#1. Copyright statement -----
# Copyright (c) 2026 the SWD study authors. Released under the MIT License (see LICENSE).

#2. Author -----
# Author: SWD study authors (see LICENSE)
# Date: 2026-11-13 (last revised)

#3. File description -----
# Purpose: README builder for 12_multiple_imputation.R: draft methods text,
#          the results table in Markdown, JP-DE differences, per-imputation
#          median distribution, covariate MI summary and extra numbers for
#          the response letter.
# Inputs:  none (build_readme_lines() receives the results)
# Outputs: none (function definitions)
# Depends: 12b_mi_estimation.R (format_interval, MEDIAN_BOOT_REPLICATES,
#          DIFFERENCE_SUBSET), 12c_secondary_descriptives.R
#          (COVARIATE_MI_ITERATIONS, describe_covariate_imputation),
#          09a_additional_descriptives_helpers.R (ALL_COUNTRIES_LABEL);
#          tidyverse
# Style:   r_script_style_guide.md (Tier B)

#4. source() and library() -----
# None: the calling script loads tidyverse and sources 00a_config.R and the
# files listed under Depends.

# Constants -----
README_DIGITS <- 1
README_COVARIATE_TERMS <- c("countryJP", "n_admissions_cat2")
# SWD value at which the German median is degenerate (see README notes).
GERMAN_MODAL_SWD <- 15

#5. Function definitions -----
extract_number <- function(extra_table, item_name, level_name, statistic_name) {
  extra_table %>%
    filter(
      .data$item == item_name, .data$level == level_name,
      .data$statistic == statistic_name
    ) %>%
    pull(.data$value)
}

describe_fibser <- function(extra_table, level_name) {
  item_name <- if (level_name == ALL_COUNTRIES_LABEL) {
    "fibser_total by overall"
  } else {
    "fibser_total by country"
  }
  lookup_value <- \(statistic) {
    extract_number(extra_table, item_name, level_name, statistic)
  }
  sprintf(
    "%s: median %g [IQR %g-%g], mean %.1f (SD %.1f), n = %d",
    level_name, lookup_value("median"), lookup_value("q1"),
    lookup_value("q3"), lookup_value("mean"), lookup_value("sd"),
    as.integer(lookup_value("n"))
  )
}

table_to_markdown <- function(table) {
  c(
    paste("|", paste(names(table), collapse = " | "), "|"),
    paste("|", paste(rep("---", ncol(table)), collapse = " | "), "|"),
    apply(table, 1, \(row) paste("|", paste(row, collapse = " | "), "|"))
  )
}

describe_methods <- function(results) {
  paste0(
    "The SWD of the 51 eligible starters without an SWD (Japan 42, ",
    "Germany 9) was multiply imputed (m = ", results$imputations, ") by ",
    "predictive mean matching (5 donors, approximate Bayesian bootstrap), ",
    "separately by country with randomised drug as predictor (Model A), ",
    "and, alternatively, in the pooled sample with country and randomised ",
    "drug as main effects (Model B; no interaction, so the drug effect ",
    "borrows strength across countries). For the ",
    nrow(results$partial_bounds), " respondents who stopped during the ",
    "staircase, donors were restricted to values compatible with their ",
    "answers. In each completed dataset the median was the conventional ",
    "sample median (mean of the two middle values for an even n), with ",
    "within-imputation variance from a nonparametric bootstrap of the ",
    "median (R = ", MEDIAN_BOOT_REPLICATES, "; for the JP-DE median ",
    "difference, the difference of country medians with resampling ",
    "stratified by country). The JP-DE mean difference used OLS with an HC3 ",
    "heteroskedasticity-robust SE. Estimates were combined with Rubin's ",
    "rules (proportions on the logit scale, with a 0.5 continuity ",
    "correction if a proportion was 0 or 1). Because the SWD takes 5-point ",
    "values, the median is discrete: the pooled median is the average of ",
    "the per-imputation medians and its CI is approximate; the distribution ",
    "of the per-imputation medians is reported. A sensitivity analysis ",
    "repeated Model A after excluding the ",
    results$counts[["comprehension_failed"]], " non-completers who ",
    "answered the comprehension check incorrectly (n = ",
    results$counts[["population_comprehension"]], "). Delta adjustment and ",
    "best/worst cases explored departures from missing at random; the ",
    "best/worst cases set non-completers to 0/60 except the staircase ",
    "stoppers, who were set to the lower/upper bound of their admissible ",
    "SWD set."
  )
}

describe_imputation_medians <- function(per_imputation_medians) {
  per_imputation_medians %>%
    group_by(.data$analysis, .data$subset) %>%
    summarise(text = paste0(.data$median_value, " (", .data$n_imputations,
      ")",
      collapse = ", "
    ), .groups = "drop") %>%
    with(sprintf("- %s / %s: %s", analysis, subset, text))
}

describe_covariate_terms <- function(covariate_comparison) {
  terms <- covariate_comparison$table %>%
    filter(.data$term %in% README_COVARIATE_TERMS)
  sprintf(
    "- %s: CC (n=%d) %s vs MI (n=%d) %s", terms$term, terms$cc_n,
    format_interval(
      terms$cc_estimate, terms$cc_conf_low,
      terms$cc_conf_high, README_DIGITS
    ),
    terms$mi_n,
    format_interval(
      terms$mi_estimate, terms$mi_conf_low,
      terms$mi_conf_high, README_DIGITS
    )
  )
}

describe_logged_events <- function(covariate) {
  events <- covariate$logged_events
  if (nrow(events) == 0) {
    return("- mice loggedEvents: none.")
  }
  c(
    sprintf(
      paste0(
        "- mice loggedEvents: %d (`covariate_mi_logged_events.csv`)",
        ", all removing %s as a predictor of %s."
      ),
      nrow(events), paste(covariate$dropped_constant, collapse = ", "),
      paste(unique(paste0(events$dep, " [", events$meth, "]")),
        collapse = ", "
      )
    ),
    paste0(
      "  It is constant (Yes) wherever these covariates are observed, ",
      "so mice drops it; it is also dropped from the analysis model."
    )
  )
}

build_readme_lines <- function(results) {
  differences <- results$pooled %>% filter(.data$subset == DIFFERENCE_SUBSET)
  bounds <- results$partial_bounds
  complete_case <- results$complete_case
  covariate <- results$covariate_comparison
  extra_table <- results$extra_table
  c(
    "# Reviewer #5: multiple imputation of the missing SWD (peer-review round 2)",
    "", "Generated by `R_SWD/utils/12_multiple_imputation.R`.", "",
    "## Methods (draft)", "", describe_methods(results), "",
    "## Key results", "", table_to_markdown(results$results_table), "",
    "JP minus DE differences (estimate [95% CI]):",
    sprintf(
      "- %s / %s: %s", differences$analysis, differences$estimand,
      format_interval(
        differences$estimate, differences$conf_low,
        differences$conf_high, README_DIGITS
      )
    ), "",
    "Per-imputation medians, value (number of the imputations):",
    describe_imputation_medians(results$per_imputation_medians), "",
    "Staircase stoppers (admissible SWD set from partial answers):",
    sprintf(
      "- %s %s (%s): %s -> SWD in [%g, %g]", bounds$country,
      bounds$respondent_id, bounds$randomized_drug,
      bounds$answered_path, bounds$swd_lower, bounds$swd_upper
    ), "",
    "## Secondary: covariate MI among the 225 (12-covariate OLS)", "",
    paste0(
      "- Imputed covariates (mice, maxit = ", COVARIATE_MI_ITERATIONS,
      "): ", describe_covariate_imputation(covariate), "."
    ),
    paste0(
      "- Dropped from both models (constant in complete cases): ",
      covariate$table$cc_dropped_constant[1], "."
    ),
    describe_covariate_terms(covariate),
    describe_logged_events(covariate), "",
    "## Extra numbers for the response letter (`extra_numbers.csv`)", "",
    sprintf(
      "- Non-monotone staircase responses: %d of %d analysed.",
      as.integer(extract_number(
        extra_table, "non_monotone_staircase",
        ALL_COUNTRIES_LABEL, "n"
      )),
      results$counts[["analysed"]]
    ),
    paste0(
      "- FIBSER total among the 225: ",
      paste(map_chr(COUNTRY_SUBSETS, \(level_name) {
        describe_fibser(extra_table, level_name)
      }), collapse = "; "), "."
    ),
    "", "## Files", "",
    "- `population_counts.csv`, `staircase_stopper_bounds.csv`",
    "- `mi_pooled_estimates.csv` (long; incl. JP-DE differences, df, B)",
    "- `mi_results_table.csv` / `.docx` (eAppendix table)",
    "- `mi_per_imputation_medians.csv` (distribution of the 50 medians)",
    "- `covariate_mi_vs_complete_case.csv` (12-covariate OLS, MI vs CC n=212)",
    "- `covariate_mi_logged_events.csv` (mice loggedEvents; empty = none)",
    "- `extra_numbers.csv` (non-monotone, comprehension, FIBSER, SWD medians)",
    "- `data_dictionary.xlsx` (from `13_data_dictionary.R`)", "",
    "Notes: median CIs are approximate (discrete 5-point outcome; bootstrap",
    "variance of the median plus Rubin's between-imputation variance).",
    sprintf(
      paste0(
        "In Germany %d of %d completers had SWD = 15 (Q1 = median ",
        "= Q3), so every bootstrap and imputed median is 15 and ",
        "the interval is degenerate [15, 15]."
      ),
      sum(complete_case$country == "DE" &
        complete_case$swd == GERMAN_MODAL_SWD),
      sum(complete_case$country == "DE")
    ),
    "Complete-case medians reproduce Table 2 (overall 15, Japan 27.5).",
    "Stop stage is constant among completers and so cannot enter the",
    "imputation model; it informs the imputation only through the bounds."
  )
}

#6. Executed statements -----
# None (sourced by 12_multiple_imputation.R).
