#1. Copyright statement -----
# Copyright (c) 2026 the SWD study authors. Released under the MIT License (see LICENSE).

#2. Author -----
# Author: SWD study authors (see LICENSE)
# Date: 2026-11-13 (last revised)

#3. File description -----
# Purpose: Participant flow counts per country (JP / DE) for the flow
#          diagram: assigned-drug status per respondent, screening items of
#          the non-randomized respondents, stage counts, the reclassification
#          of RECENT_ADMISSION_EXCLUDE and the internal-consistency checks.
# Inputs:  none (functions receive swd_individual.csv and read the raw SoSci
#          data exports via 01a_raw_data_loading.R)
# Outputs: none (function definitions)
# Depends: 00a_config.R, 01a_raw_data_loading.R; tidyverse
# Style:   r_script_style_guide.md (Tier B)

#4. source() and library() -----
# None: the calling script loads tidyverse and sources 00a_config.R.

# Constants -----
FLOW_COUNTRIES <- c("JP", "DE")
FLOW_KEYS <- c("country", "respondent_id")
SCREEN_COLS <- c("WE02", "WE03", "WE05", "WE06", "WE07")
NOT_RANDOMIZED_STATUS <- "none"

#5. Function definitions -----
as_int <- function(x) suppressWarnings(as.integer(round(as.numeric(x))))

# Same coalesce pattern as assigned_track() in 07_country_diagnostics.R: each
# randomized respondent has exactly one non-"not_assigned" track.
pick_assigned_status <- function(individual, row_index) {
  for (drug_code in DRUG_TRACKS$drug_code) {
    status <- individual[[paste0("status_", drug_code)]][row_index]
    if (!is.na(status) && status != "not_assigned") {
      return(status)
    }
  }
  NOT_RANDOMIZED_STATUS
}

assigned_status <- function(individual) {
  tibble(
    respondent_id = as.character(individual$respondent_id),
    country = individual$country,
    randomized = !is.na(individual$randomized_drug) &
      individual$randomized_drug != "",
    status = vapply(seq_len(nrow(individual)), \(row_index) {
      pick_assigned_status(individual, row_index)
    }, character(1))
  )
}

load_screening <- function(country_code) {
  path <- find_raw_file(PATH_RAW, tolower(country_code), "data", "xlsx")
  raw_screening <- read_sosci_data_excel(path)
  kept_columns <- intersect(SCREEN_COLS, names(raw_screening))
  raw_screening %>%
    filter(!is.na(as_int(.data$CASE))) %>% # defensive: drop label rows
    transmute(
      respondent_id = as.character(as_int(.data$CASE)),
      country = country_code,
      across(all_of(kept_columns), as_int)
    )
}

# WE03 = 2 no schizophrenia/schizoaffective diagnosis; WE05 = 2 under 18;
# WE06 = 2 medication changed within 6 months; WE07 = 1 psychiatric admission
# within 6 months; WE02 = consent (NA / -9 = never answered).
flag_screening_criteria <- function(non_randomized) {
  non_randomized %>%
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

# Non-randomized respondents with their screening flags; every one of them
# must be found in the raw export.
load_non_randomized <- function(person) {
  screening <- bind_rows(load_screening("JP"), load_screening("DE")) %>%
    anti_join(
      EXPLICIT_TEST_IDS %>% mutate(respondent_id = as.character(.data$respondent_id)),
      by = FLOW_KEYS
    )
  person_not_randomized <- filter(person, !.data$randomized)
  stopifnot(
    nrow(anti_join(person_not_randomized, screening, by = FLOW_KEYS)) == 0
  )
  person_not_randomized %>%
    left_join(screening, by = FLOW_KEYS) %>%
    flag_screening_criteria()
}

count_per_country <- function(count_one) {
  vapply(FLOW_COUNTRIES, count_one, integer(1))
}

# The RECENT_ADMISSION_EXCLUDE respondents (admission within the past 6
# months reported in the demographics, DE09, despite screening item WE07)
# must all be randomized with a completed staircase before their counts are
# moved to the Ineligible stage.
check_recent_admission_cases <- function(person) {
  for (country_code in FLOW_COUNTRIES) {
    is_recent <- person$country == country_code &
      person$respondent_id %in% RECENT_ADMISSION_EXCLUDE[[country_code]]
    stopifnot(
      sum(is_recent) == length(RECENT_ADMISSION_EXCLUDE[[country_code]]),
      all(person$randomized[is_recent]),
      all(person$status[is_recent] == "completed")
    )
  }
}

# Every documented free-text exclusion must exist with status never_willing.
check_freetext_exclusions <- function(person) {
  for (country_code in FLOW_COUNTRIES) {
    is_excluded <- person$country == country_code &
      person$respondent_id %in% ANALYSIS_EXCLUDE[[country_code]]
    excluded_status <- person$status[is_excluded]
    stopifnot(
      length(excluded_status) == length(ANALYSIS_EXCLUDE[[country_code]]),
      all(excluded_status == "never_willing")
    )
  }
}

check_flow_identities <- function(flow, person) {
  assessed_sum <- flow$ineligible + flow$left_consent + flow$left_after +
    flow$randomized
  randomized_sum <- flow$no_response + flow$incomplete + flow$determinable
  # every randomized respondent must carry one of the five staircase statuses
  randomized_statuses <- c("no_response", "incomplete", DETERMINABLE_STATUSES)
  stopifnot(
    all(flow$test_removed >= 0),
    all(flow$clicked == flow$left_info + flow$entered),
    all(flow$entered - flow$test_removed == flow$assessed),
    all(flow$assessed == assessed_sum),
    all(flow$randomized == randomized_sum),
    all(flow$determinable == flow$completed + flow$never + flow$always),
    all(flow$determinable == flow$excluded + flow$analyzed),
    all(person$status[person$randomized] %in% randomized_statuses),
    all(person$status[!person$randomized] == NOT_RANDOMIZED_STATUS)
  )
}

# Stage counts (named JP / DE vectors), keyed like the flow-diagram stages.
compute_flow_counts <- function(person, non_randomized, clicks, entered) {
  count_flag <- function(flag) {
    count_per_country(\(country_code) {
      sum(non_randomized[[flag]][non_randomized$country == country_code],
        na.rm = TRUE
      )
    })
  }
  count_status <- function(status) {
    count_per_country(\(country_code) {
      is_counted <- person$country == country_code & person$randomized
      sum(is_counted & person$status == status)
    })
  }
  assessed <- count_per_country(\(country_code) sum(person$country == country_code))
  randomized <- count_per_country(\(country_code) {
    sum(person$country == country_code & person$randomized)
  })
  completed <- count_status("completed")
  never <- count_status("never_willing")
  always <- count_status("always_willing")

  # Reclassify RECENT_ADMISSION_EXCLUDE from the randomized arm to Ineligible.
  check_recent_admission_cases(person)
  recent_admission_n <- count_per_country(\(country_code) {
    length(RECENT_ADMISSION_EXCLUDE[[country_code]])
  })
  determinable <- completed + never + always - recent_admission_n
  excluded <- count_per_country(\(country_code) {
    length(ANALYSIS_EXCLUDE[[country_code]])
  })

  list(
    clicked = clicks, left_info = clicks - entered, entered = entered,
    test_removed = entered - assessed, assessed = assessed,
    ineligible = count_flag("ineligible") + recent_admission_n,
    crit_meds = count_flag("crit_meds"),
    crit_admission = count_flag("crit_admission") + recent_admission_n,
    crit_no_dx = count_flag("crit_no_dx"),
    crit_under_18 = count_flag("crit_under_18"),
    left_consent = count_flag("left_consent"),
    left_after = count_flag("left_after"),
    randomized = randomized - recent_admission_n,
    no_response = count_status("no_response"),
    incomplete = count_status("incomplete"),
    determinable = determinable,
    completed = completed - recent_admission_n, never = never, always = always,
    excluded = excluded, analyzed = determinable - excluded,
    recent_admission_n = recent_admission_n
  )
}

#6. Executed statements -----
# None (sourced by 08_flow_diagram.R).
