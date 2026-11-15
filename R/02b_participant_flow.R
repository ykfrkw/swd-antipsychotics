# 1. Copyright statement -----
# Copyright (c) 2026 Yuki Furukawa. Released under the MIT License (see LICENSE).

# 2. Author -----
# Author: SWD study authors (see LICENSE)
# Date: 2026-11-14 (last revised)

# 3. File description -----
# Purpose: Participant flow counts per country (Figure 2): from survey link
#          clicks through screening, randomization and staircase status to
#          the analysis set, with the internal-consistency identities.
# Inputs:  none (functions receive swd_individual.csv and read the raw SoSci
#          data exports via 01a_raw_data.R)
# Outputs: none (function definitions)
# Depends: 00_config.R, 01a_raw_data.R (find_raw_file,
#          read_sosci_data_excel); tidyverse

# 4. source() and library() -----
# None: the calling script loads tidyverse and sources 00_config.R and
# 01a_raw_data.R.

# Constants -----
# Source: SoSci Survey admin overview, 2026-07-06 (clicks on the survey link
# and valid cases that entered the survey; not present in the data exports).
FLOW_CLICKS <- c(JP = 1029L, DE = 248L)
FLOW_ENTERED <- c(JP = 227L, DE = 128L)
SCREENING_COLUMNS <- c("WE02", "WE03", "WE05", "WE06", "WE07")
NOT_RANDOMIZED_STATUS <- "none"

# 5. Function definitions -----
as_integer_code <- function(values) {
  suppressWarnings(as.integer(round(as.numeric(values))))
}

# Each randomized respondent has exactly one track that is not "not_assigned".
assigned_status <- function(individual) {
  status_matrix <- as.matrix(individual[, paste0("status_", DRUG_TRACKS$drug_code)])
  tibble(
    respondent_id = as.character(individual$respondent_id),
    country = individual$country,
    randomized = !is.na(individual$randomized_drug) & individual$randomized_drug != "",
    status = apply(status_matrix, 1, \(statuses) {
      assigned <- statuses[!is.na(statuses) & statuses != "not_assigned"]
      if (length(assigned) == 0) NOT_RANDOMIZED_STATUS else assigned[[1]]
    })
  )
}

# WE03 = 2 no schizophrenia/schizoaffective diagnosis; WE05 = 2 under 18;
# WE06 = 2 medication changed within 6 months; WE07 = 1 psychiatric admission
# within 6 months; WE02 = consent (NA / -9 = never answered).
load_screening <- function(country_code) {
  raw_screening <- read_sosci_data_excel(
    find_raw_file(PATH_RAW, tolower(country_code), "data", "xlsx")
  )
  raw_screening %>%
    filter(!is.na(as_integer_code(.data$CASE))) %>%
    transmute(
      respondent_id = as.character(as_integer_code(.data$CASE)),
      country = country_code,
      across(all_of(intersect(SCREENING_COLUMNS, names(raw_screening))), as_integer_code)
    ) %>%
    mutate(
      crit_no_dx = coalesce(.data$WE03 == NO, FALSE),
      crit_under_18 = coalesce(.data$WE05 == NO, FALSE),
      crit_meds = coalesce(.data$WE06 == NO, FALSE),
      crit_admission = coalesce(.data$WE07 == YES, FALSE),
      ineligible = crit_no_dx | crit_under_18 | crit_meds | crit_admission,
      left_consent = !ineligible & (is.na(.data$WE02) | .data$WE02 == NA_CODE),
      left_after = !ineligible & !left_consent
    )
}

# Non-randomized respondents with their screening flags.
load_non_randomized <- function(person) {
  screening <- bind_rows(load_screening("JP"), load_screening("DE")) %>%
    anti_join(EXPLICIT_TEST_IDS, by = RESPONDENT_KEY)
  person_not_randomized <- filter(person, !.data$randomized)
  stopifnot(nrow(anti_join(person_not_randomized, screening, by = RESPONDENT_KEY)) == 0)
  left_join(person_not_randomized, screening, by = RESPONDENT_KEY)
}

# The recent-admission respondents (admission within 6 months reported in
# the demographics despite WE07) are randomized with a completed staircase;
# their counts move to Ineligible. The free-text exclusions are all never
# willing.
check_exclusion_statuses <- function(person) {
  for (country_code in COUNTRY_CODES) {
    is_recent <- person$country == country_code &
      person$respondent_id %in% RECENT_ADMISSION_EXCLUDE[[country_code]]
    is_excluded <- person$country == country_code &
      person$respondent_id %in% ANALYSIS_EXCLUDE[[country_code]]
    stopifnot(
      sum(is_recent) == length(RECENT_ADMISSION_EXCLUDE[[country_code]]),
      all(person$randomized[is_recent]),
      all(person$status[is_recent] == "completed"),
      sum(is_excluded) == length(ANALYSIS_EXCLUDE[[country_code]]),
      all(person$status[is_excluded] == "never_willing")
    )
  }
}

# Stage counts as a list of named (JP, DE) integer vectors.
compute_flow_counts <- function(individual) {
  person <- assigned_status(individual)
  non_randomized <- load_non_randomized(person)
  check_exclusion_statuses(person)
  per_country <- function(count_one) vapply(COUNTRY_CODES, count_one, integer(1))
  count_flag <- function(flag) {
    per_country(\(code) sum(non_randomized[[flag]][non_randomized$country == code]))
  }
  count_status <- function(status) {
    per_country(\(code) sum(person$country == code & person$randomized & person$status == status))
  }
  assessed <- per_country(\(code) sum(person$country == code))
  randomized <- per_country(\(code) sum(person$country == code & person$randomized))
  recent_n <- per_country(\(code) length(RECENT_ADMISSION_EXCLUDE[[code]]))
  excluded <- per_country(\(code) length(ANALYSIS_EXCLUDE[[code]]))
  completed <- count_status("completed")
  never <- count_status("never_willing")
  always <- count_status("always_willing")
  determinable <- completed + never + always - recent_n
  list(
    clicked = FLOW_CLICKS, left_info = FLOW_CLICKS - FLOW_ENTERED,
    entered = FLOW_ENTERED, test_removed = FLOW_ENTERED - assessed,
    assessed = assessed, ineligible = count_flag("ineligible") + recent_n,
    crit_meds = count_flag("crit_meds"),
    crit_admission = count_flag("crit_admission") + recent_n,
    crit_no_dx = count_flag("crit_no_dx"), crit_under_18 = count_flag("crit_under_18"),
    left_consent = count_flag("left_consent"), left_after = count_flag("left_after"),
    randomized = randomized - recent_n, no_response = count_status("no_response"),
    incomplete = count_status("incomplete"), determinable = determinable,
    completed = completed - recent_n, never = never, always = always,
    excluded = excluded, analyzed = determinable - excluded
  )
}

check_flow_identities <- function(flow) {
  stopifnot(
    all(flow$test_removed >= 0),
    all(flow$clicked == flow$left_info + flow$entered),
    all(flow$entered - flow$test_removed == flow$assessed),
    all(flow$assessed == flow$ineligible + flow$left_consent + flow$left_after +
      flow$randomized),
    all(flow$randomized == flow$no_response + flow$incomplete + flow$determinable),
    all(flow$determinable == flow$completed + flow$never + flow$always),
    all(flow$determinable == flow$excluded + flow$analyzed),
    all(flow$analyzed == c(ANALYSIS_N_JP, ANALYSIS_N_DE))
  )
}

flow_counts_table <- function(flow) {
  tibble(
    stage = names(flow),
    n_jp = vapply(flow, \(counts) counts[["JP"]], integer(1)),
    n_de = vapply(flow, \(counts) counts[["DE"]], integer(1))
  ) %>%
    mutate(n_total = .data$n_jp + .data$n_de)
}

# 6. Executed statements -----
# None (sourced by 02_descriptives.R).
