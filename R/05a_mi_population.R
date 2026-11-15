# 1. Copyright statement -----
# Copyright (c) 2026 Yuki Furukawa. Released under the MIT License (see LICENSE).

# 2. Author -----
# Author: SWD study authors (see LICENSE)
# Date: 2026-11-14 (last revised)

# 3. File description -----
# Purpose: Imputation population for 05_multiple_imputation.R: the eligible
#          starters (randomized, eligible) with their comprehension-check
#          answers and the stage at which non-completers stopped, the
#          admissible SWD range of respondents who stopped part-way through
#          the staircase, and the 276 respondents to impute.
# Inputs:  data/raw/data_swd_{ja,de}*.xlsx (newest; comprehension items,
#          staircase answers)
# Outputs: none (function definitions)
# Depends: 00_config.R, 00c_analysis_set.R, 01a_raw_data.R; tidyverse

# 4. source() and library() -----
# None: the calling script loads tidyverse and sources the files listed
# under Depends.

# Constants -----
STOP_STAGE_STAIRCASE <- "staircase started, not finished"
STOP_STAGE_FAILED_CHECK <- "comprehension check answered, >=1 item incorrect"
# Comprehension-check answer keys (CO02 differs between the versions).
CO01_KEY <- c(CO01_01 = 2L, CO01_02 = 2L, CO01_03 = 1L, CO01_04 = 2L)
CO02_KEY <- c(DE = 3L, JP = 7L)
CO03_KEY <- 2L

# 5. Function definitions -----
as_integer_code <- function(values) {
  suppressWarnings(as.integer(round(as.numeric(values))))
}

read_raw_survey <- function(country) {
  suppressWarnings(read_sosci_data_excel(
    find_raw_file(PATH_RAW, tolower(country), "data", "xlsx")
  ))
}

# Every item correct (stricter than the questionnaire gate, which did not
# require CO01 c to be left unchecked).
load_comprehension <- function(country) {
  survey_data <- read_raw_survey(country)
  is_correct <- function(column, key) as_integer_code(survey_data[[column]]) == key
  co02_key <- CO02_KEY[[country]]
  tibble(
    respondent_id = as.character(as_integer_code(survey_data$CASE)),
    country = country,
    has_comprehension_answer = !is.na(as_integer_code(survey_data$CO02)) |
      !is.na(as_integer_code(survey_data$CO03)),
    is_comprehension_correct = coalesce(
      is_correct("CO01_01", CO01_KEY[["CO01_01"]]) &
        is_correct("CO01_02", CO01_KEY[["CO01_02"]]) &
        is_correct("CO01_03", CO01_KEY[["CO01_03"]]) &
        is_correct("CO01_04", CO01_KEY[["CO01_04"]]) &
        is_correct("CO02", co02_key) & is_correct("CO03", CO03_KEY),
      FALSE
    )
  ) %>%
    filter(!is.na(.data$respondent_id))
}

# Eligible starters (n = 288): randomized respondents minus the post hoc
# recent-admission ineligibles, with the stage at which they stopped.
build_eligible_starters <- function(individual) {
  status_matrix <- as.matrix(individual[, paste0("status_", DRUG_TRACKS$drug_code)])
  visited_matrix <- as.matrix(individual[, paste0("n_visited_", DRUG_TRACKS$drug_code)])
  randomized <- tibble(
    respondent_id = as.character(individual$respondent_id),
    country = individual$country,
    randomized_drug = individual$randomized_drug,
    status = apply(status_matrix, 1, \(statuses) {
      assigned <- statuses[!is.na(statuses) & statuses != "not_assigned"]
      if (length(assigned) == 0) "none" else assigned[[1]]
    }),
    n_staircase_items = apply(visited_matrix, 1, \(row) max(c(0, row), na.rm = TRUE))
  ) %>%
    filter(!is.na(.data$randomized_drug), .data$randomized_drug != "")
  comprehension <- bind_rows(load_comprehension("JP"), load_comprehension("DE"))
  randomized %>%
    anti_join(exclusion_list_table(RECENT_ADMISSION_EXCLUDE), by = RESPONDENT_KEY) %>%
    left_join(comprehension, by = RESPONDENT_KEY) %>%
    mutate(
      has_swd = .data$status %in% USABLE_SWD_STATUSES,
      stop_stage = case_when(
        .data$has_swd ~ "SWD determinable",
        .data$n_staircase_items > 0 ~ STOP_STAGE_STAIRCASE,
        .data$is_comprehension_correct ~ "comprehension check passed, no staircase answer",
        .data$has_comprehension_answer ~ STOP_STAGE_FAILED_CHECK,
        TRUE ~ "after randomization, before comprehension answer"
      )
    )
}

# Stage counts of the eligible starters and the respondents who got at least
# one comprehension item wrong (with and without an SWD).
count_stop_stages <- function(eligible_starters) {
  failed_any <- eligible_starters %>%
    filter(.data$has_comprehension_answer, !.data$is_comprehension_correct)
  stage_by_country <- imap(split_by_country(eligible_starters), \(starters, country) {
    count(starters, level = .data$stop_stage) %>%
      mutate(item = "stop_stage", group = country, .before = 1)
  }) %>%
    bind_rows()
  bind_rows(
    stage_by_country,
    tibble(
      item = "comprehension_any_item_incorrect_strict",
      group = c("all", "with SWD", "without SWD"),
      n = c(nrow(failed_any), sum(failed_any$has_swd), sum(!failed_any$has_swd))
    )
  ) %>%
    mutate(denominator = nrow(eligible_starters))
}

# Staircase answers (1 = worthwhile, 2 = not worthwhile) on the assigned
# track, one row per respondent and relapse rate.
read_assigned_staircase <- function(country) {
  node_long <- SWD_NODES_BY_COUNTRY[[tolower(country)]] %>%
    pivot_longer(-"relapse_pct", names_to = "drug", values_to = "node")
  read_raw_survey(country) %>%
    transmute(
      respondent_id = as.character(as_integer_code(.data$CASE)),
      drug = DRUG_TRACKS$drug[match(as_integer_code(.data$RA01), DRUG_TRACKS$ra01)],
      across(any_of(unique(node_long$node)), as_integer_code)
    ) %>%
    filter(!is.na(.data$respondent_id), !is.na(.data$drug)) %>%
    pivot_longer(-c("respondent_id", "drug"), names_to = "node", values_to = "answer") %>%
    inner_join(node_long, by = c("drug", "node")) %>%
    filter(.data$answer %in% c(YES, NO)) %>%
    mutate(country = country)
}

# A "worthwhile" answer at relapse rate r implies SWD <= 60 - r; a "not
# worthwhile" answer at r implies SWD >= 60 - r + 5.
derive_partial_bounds <- function(staircase_answers) {
  staircase_answers %>%
    group_by(.data$country, .data$respondent_id) %>%
    summarise(
      n_answered = n(),
      max_yes_rate = suppressWarnings(max(.data$relapse_pct[.data$answer == YES])),
      min_no_rate = suppressWarnings(min(.data$relapse_pct[.data$answer == NO])),
      answered_path = paste0(.data$relapse_pct, "%:",
        if_else(.data$answer == YES, "yes", "no"),
        collapse = " "
      ),
      .groups = "drop"
    ) %>%
    mutate(
      swd_lower = if_else(is.finite(.data$min_no_rate),
        SWD_CEILING - .data$min_no_rate + SWD_STEP, SWD_FLOOR
      ),
      swd_upper = if_else(is.finite(.data$max_yes_rate),
        SWD_CEILING - .data$max_yes_rate, SWD_CEILING
      )
    )
}

stopper_bounds <- function(eligible_starters) {
  stopper_ids <- eligible_starters %>%
    filter(.data$stop_stage == STOP_STAGE_STAIRCASE) %>%
    select("country", "respondent_id", "randomized_drug")
  map(COUNTRY_CODES, read_assigned_staircase) %>%
    bind_rows() %>%
    semi_join(stopper_ids, by = RESPONDENT_KEY) %>%
    derive_partial_bounds() %>%
    left_join(stopper_ids, by = RESPONDENT_KEY)
}

# 276 = 225 analysed + 51 eligible starters without an SWD. The 12
# respondents excluded for misunderstanding the task stay excluded. Non-
# completers may take any SWD in [0, 60] unless they stopped part-way.
build_imputation_population <- function(eligible_starters, analysed_frame,
                                        partial_bounds) {
  outcome <- analysed_frame %>%
    transmute(
      country = as.character(.data$country),
      respondent_id = .data$respondent_id, swd = .data$swd
    )
  bounds <- partial_bounds %>%
    select("country", "respondent_id", "answered_path", "swd_lower", "swd_upper")
  eligible_starters %>%
    anti_join(exclusion_list_table(ANALYSIS_EXCLUDE), by = RESPONDENT_KEY) %>%
    left_join(outcome, by = RESPONDENT_KEY) %>%
    left_join(bounds, by = RESPONDENT_KEY) %>%
    mutate(
      is_staircase_stopper = .data$stop_stage == STOP_STAGE_STAIRCASE,
      swd_lower = if_else(.data$is_staircase_stopper, .data$swd_lower, SWD_FLOOR),
      swd_upper = if_else(.data$is_staircase_stopper, .data$swd_upper, SWD_CEILING),
      is_japan = as.numeric(.data$country == "JP"),
      drug_ari = as.numeric(.data$randomized_drug == "aripiprazole"),
      drug_ola = as.numeric(.data$randomized_drug == "olanzapine")
    ) %>%
    arrange(.data$country, as.integer(.data$respondent_id))
}

# 6. Executed statements -----
# None (sourced by 05_multiple_imputation.R).
