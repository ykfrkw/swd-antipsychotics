#1. Copyright statement -----
# Copyright (c) 2026 the SWD study authors. Released under the MIT License (see LICENSE).

#2. Author -----
# Author: SWD study authors (see LICENSE)
# Date: 2026-11-13 (last revised)

#3. File description -----
# Purpose: Helpers shared by the review analyses (09, 11, 12): constants of
#          the analysis set, CSV writing and p-value formatting, SWD
#          descriptives, and the eligible-starter table (assigned track,
#          comprehension check, stop stage). The missingness summaries and
#          the OLS models live in 09b_additional_descriptives_models.R, which
#          is sourced here so callers of this file keep one entry point.
# Inputs:  R_SWD/data/raw/data_swd_{ja,de}*.xlsx (newest; comprehension items,
#          MAXPAGE; read by load_comprehension())
# Outputs: none (function definitions)
# Depends: 00a_config.R, 01a_raw_data_loading.R, 01d_swd_summary.R,
#          01f_test_respondent_exclusion.R, 02d_swd_linear_regression.R,
#          09b_additional_descriptives_models.R; tidyverse
# Style:   r_script_style_guide.md (Tier B)

#4. source() and library() -----
# The calling script loads tidyverse and sources 00a_config.R,
# 01a_raw_data_loading.R, 01d_swd_summary.R, 01f_test_respondent_exclusion.R
# and 02d_swd_linear_regression.R.
source(file.path(PATH_UTILS, "09b_additional_descriptives_models.R"))

# Constants -----
DRUG_STATUS_COLUMNS <- c("status_ris", "status_ari", "status_ola")
VISITED_COLUMNS <- c("n_visited_ris", "n_visited_ari", "n_visited_ola")
SWD_FLOOR <- 0
SWD_CEILING <- 60
# Width of one staircase step (percentage points of relapse rate).
SWD_STEP <- 5
CONFIDENCE_LEVEL <- 0.95
P_VALUE_DISPLAY_FLOOR <- 0.001
ALL_COUNTRIES_LABEL <- "All"
PRIMARY_SPECIFICATION <- "primary_including_never_willing"
EXCLUDING_NEVER_SPECIFICATION <- "sensitivity_excluding_never_willing"
EXPECTED_ANALYSED_COUNTS <- c(
  All = ANALYSIS_N_TOTAL, DE = ANALYSIS_N_DE, JP = ANALYSIS_N_JP
)
# Eligible starters = randomized minus the post hoc recent-admission
# ineligibles (Randomized box of 08_flow_diagram.R).
EXPECTED_ELIGIBLE_STARTERS <- 288L
EXPECTED_COMPLETE_CASES <- 212L
STOP_STAGE_STAIRCASE <- "staircase started, not finished"
STOP_STAGE_FAILED_CHECK <-
  "comprehension check answered, >=1 item incorrect"

# Comprehension keys copied from 07_country_diagnostics.R, which is an entry
# script (writes outputs) and therefore cannot be sourced.
CO01_KEY <- c(CO01_01 = 2L, CO01_02 = 2L, CO01_03 = 1L, CO01_04 = 2L)
CO02_KEY <- c(DE = 3L, JP = 7L)
CO03_KEY <- 2L

#5. Function definitions -----
write_review_csv <- function(table, output_dir, file_name) {
  write_excel_csv(table, file.path(output_dir, file_name), na = "")
}

format_p_value <- function(p_value) {
  ifelse(
    p_value < P_VALUE_DISPLAY_FLOOR, "<0.001", sprintf("%.3f", p_value)
  )
}

# Stops when the primary analysis set drifts from the manuscript counts.
check_analysed_counts <- function(primary_frame) {
  analysed_counts <- c(
    All = nrow(primary_frame),
    table(as.character(primary_frame$country))
  )
  stopifnot(all(
    analysed_counts[names(EXPECTED_ANALYSED_COUNTS)] == EXPECTED_ANALYSED_COUNTS
  ))
}

as_integer_code <- function(values) {
  suppressWarnings(as.integer(round(as.numeric(values))))
}

# Named list of ID vectors (one per country) as a country / respondent_id table.
exclusion_list_table <- function(ids_by_country) {
  tibble(
    country = rep(names(ids_by_country), lengths(ids_by_country)),
    respondent_id = unlist(ids_by_country, use.names = FALSE)
  )
}

compute_skewness <- function(values) {
  observed <- values[!is.na(values)]
  if (length(observed) < 3) {
    return(NA_real_)
  }
  centred <- observed - mean(observed)
  second_moment <- mean(centred^2)
  if (second_moment == 0) {
    return(NA_real_)
  }
  mean(centred^3) / second_moment^1.5
}

describe_swd <- function(swd_values) {
  observed <- swd_values[!is.na(swd_values)]
  has_shapiro_range <- length(observed) >= 3 && length(observed) <= 5000 &&
    stats::sd(observed) > 0
  shapiro <- if (has_shapiro_range) stats::shapiro.test(observed) else NULL
  quartiles <- stats::quantile(observed, probs = c(0.25, 0.75), names = FALSE)
  tibble(
    n = length(observed),
    mean = mean(observed),
    sd = stats::sd(observed),
    median = stats::median(observed),
    q1 = quartiles[1],
    q3 = quartiles[2],
    min = min(observed),
    max = max(observed),
    skewness = compute_skewness(observed),
    shapiro_w = if (is.null(shapiro)) NA_real_ else unname(shapiro$statistic),
    shapiro_p = if (is.null(shapiro)) NA_real_ else shapiro$p.value,
    pct_at_0 = 100 * mean(observed == SWD_FLOOR),
    pct_at_60 = 100 * mean(observed == SWD_CEILING)
  )
}

split_by_country <- function(frame) {
  country_names <- sort(unique(as.character(frame$country)))
  subsets <- lapply(country_names, \(name) frame[frame$country == name, ])
  c(
    stats::setNames(list(frame), ALL_COUNTRIES_LABEL),
    stats::setNames(subsets, country_names)
  )
}

describe_swd_by_country <- function(frame, specification) {
  subsets <- split_by_country(frame)
  bind_rows(lapply(names(subsets), \(name) {
    describe_swd(subsets[[name]]$swd) %>%
      mutate(specification = specification, country = name, .before = 1)
  }))
}

# Each randomized respondent has exactly one assigned track (as in
# assigned_status() of 08_flow_diagram.R).
derive_assigned_track <- function(individual) {
  pick_status <- function(row_index) {
    statuses <- unlist(individual[row_index, DRUG_STATUS_COLUMNS])
    assigned <- statuses[!is.na(statuses) & statuses != "not_assigned"]
    if (length(assigned) == 0) "none" else assigned[[1]]
  }
  visited <- as.matrix(individual[, VISITED_COLUMNS])
  tibble(
    respondent_id = as.character(individual$respondent_id),
    country = individual$country,
    randomized_drug = individual$randomized_drug,
    is_randomized = !is.na(individual$randomized_drug) &
      individual$randomized_drug != "",
    status = vapply(seq_len(nrow(individual)), pick_status, character(1)),
    n_staircase_items = apply(visited, 1, \(row) max(c(0, row), na.rm = TRUE))
  )
}

is_code_equal <- function(values, key) {
  as_integer_code(values) == key
}

load_comprehension <- function(country) {
  path <- find_raw_file(PATH_RAW, tolower(country), "data", "xlsx")
  survey_data <- suppressWarnings(read_sosci_data_excel(path))
  co02_correct_code <- CO02_KEY[[country]]
  survey_data %>%
    filter(!is.na(as_integer_code(.data$CASE))) %>%
    transmute(
      respondent_id = as.character(as_integer_code(.data$CASE)),
      country = country,
      max_page = as_integer_code(.data$MAXPAGE),
      has_comprehension_answer = !is.na(as_integer_code(.data$CO02)) |
        !is.na(as_integer_code(.data$CO03)),
      is_comprehension_correct = coalesce(
        is_code_equal(.data$CO01_01, CO01_KEY[["CO01_01"]]) &
          is_code_equal(.data$CO01_02, CO01_KEY[["CO01_02"]]) &
          is_code_equal(.data$CO01_03, CO01_KEY[["CO01_03"]]) &
          is_code_equal(.data$CO01_04, CO01_KEY[["CO01_04"]]) &
          is_code_equal(.data$CO02, co02_correct_code) &
          is_code_equal(.data$CO03, CO03_KEY),
        FALSE
      )
    )
}

classify_stop_stage <- function(starters) {
  starters %>%
    mutate(stop_stage = case_when(
      .data$has_swd ~ "SWD determinable",
      .data$n_staircase_items > 0 ~ STOP_STAGE_STAIRCASE,
      .data$is_comprehension_correct ~
        "comprehension check passed, no staircase answer",
      .data$has_comprehension_answer ~ STOP_STAGE_FAILED_CHECK,
      TRUE ~ "after randomization, before comprehension answer"
    ))
}

# Eligible starters (n = 288): randomized respondents minus the post hoc
# recent-admission ineligibles, with comprehension answers and stop stage.
build_eligible_starters <- function(individual) {
  recent_admission <- exclusion_list_table(RECENT_ADMISSION_EXCLUDE)
  comprehension <- bind_rows(
    load_comprehension("JP"),
    load_comprehension("DE")
  )
  randomized <- derive_assigned_track(individual) %>%
    filter(.data$is_randomized)
  randomized %>%
    anti_join(recent_admission, by = RESPONDENT_KEY) %>%
    left_join(comprehension, by = RESPONDENT_KEY) %>%
    mutate(has_swd = .data$status %in% USABLE_SWD_STATUSES) %>%
    classify_stop_stage()
}

#6. Executed statements -----
# None (sourced by 09_additional_descriptives.R, 11_interval_regression.R
# and 12_multiple_imputation.R).
