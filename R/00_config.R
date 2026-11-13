# 1. Copyright statement -----
# Copyright (c) 2026 the SWD study authors. Released under the MIT License (see LICENSE).

# 2. Author -----
# Author: SWD study authors (see LICENSE)
# Date: 2026-11-14 (last revised)

# 3. File description -----
# Purpose: Shared configuration for every analysis script: project paths,
#          survey codes, the staircase tree, the item maps, the expected size
#          of the analysis set and the respondent exclusion lists (read at
#          run time from a local, untracked file).
# Inputs:  data/exclusion_ids.csv (not shared; see README.md)
# Outputs: none (defines constants in the calling environment)
# Depends: tidyverse; here (optional, falls back to the working directory)
# Style:   r_script_style_guide.md (Tier B)

# 4. source() and library() -----
suppressPackageStartupMessages(library(tidyverse))

# Constants -----
# here::here() finds swd-antipsychotics.Rproj; without {here} the scripts
# must be run from the repository root.
PROJECT_ROOT <- if (requireNamespace("here", quietly = TRUE)) {
  here::here()
} else {
  normalizePath(getwd(), mustWork = TRUE)
}
PATH_R <- file.path(PROJECT_ROOT, "R")
PATH_RAW <- file.path(PROJECT_ROOT, "data", "raw")
PATH_PROCESSED <- file.path(PROJECT_ROOT, "data", "processed")
PATH_OUTPUTS <- file.path(PROJECT_ROOT, "outputs")
EXCLUSION_IDS_PATH <- file.path(PROJECT_ROOT, "data", "exclusion_ids.csv")
INDIVIDUAL_PATH <- file.path(PATH_PROCESSED, "swd_individual.csv")
COUNTRY_CODES <- c("JP", "DE")
RESPONDENT_KEY <- c("country", "respondent_id")

NA_CODE <- -9L
YES <- 1L
NO <- 2L

DRUG_TRACKS <- tribble(
  ~ra01, ~drug, ~drug_code,
  1L, "risperidone", "ris",
  2L, "aripiprazole", "ari",
  3L, "olanzapine", "ola"
)

SWD_NODES_JP <- tribble(
  ~relapse_pct, ~risperidone, ~aripiprazole, ~olanzapine,
  0, "SW14", "SW56", "SW69",
  5, "SW18", "SW57", "SW70",
  10, "SW19", "SW58", "SW71",
  15, "SW20", "SW59", "SW72",
  20, "SW21", "SW60", "SW73",
  25, "SW29", "SW61", "SW74",
  30, "SW22", "SW62", "SW75",
  35, "SW23", "SW63", "SW76",
  40, "SW24", "SW64", "SW77",
  45, "SW25", "SW65", "SW78",
  50, "SW26", "SW66", "SW79",
  55, "SW27", "SW67", "SW80",
  60, "SW28", "SW68", "SW81"
)

SWD_NODES_DE <- tribble(
  ~relapse_pct, ~risperidone, ~aripiprazole, ~olanzapine,
  0, "SW56", "SW71", "SW84",
  5, "SW57", "SW72", "SW85",
  10, "SW58", "SW73", "SW86",
  15, "SW59", "SW74", "SW87",
  20, "SW60", "SW75", "SW88",
  25, "SW61", "SW76", "SW89",
  30, "SW62", "SW77", "SW90",
  35, "SW63", "SW78", "SW91",
  40, "SW64", "SW79", "SW92",
  45, "SW65", "SW80", "SW93",
  50, "SW66", "SW81", "SW94",
  55, "SW67", "SW82", "SW95",
  60, "SW68", "SW83", "SW96"
)

SWD_NODES_BY_COUNTRY <- list(
  jp = SWD_NODES_JP,
  de = SWD_NODES_DE
)

STAIRCASE_TREE <- list(
  "0" = list(
    yes = "30",
    no = list(terminal = "never_willing", swd = 60)
  ),
  "30" = list(yes = "45", no = "15"),
  "45" = list(yes = "55", no = "35"),
  "55" = list(yes = "60", no = "50"),
  "60" = list(
    yes = list(terminal = "always_willing", swd = 0),
    no = list(terminal = "completed", swd = 5)
  ),
  "50" = list(
    yes = list(terminal = "completed", swd = 10),
    no = list(terminal = "completed", swd = 15)
  ),
  "35" = list(
    yes = "40",
    no = list(terminal = "completed", swd = 30)
  ),
  "40" = list(
    yes = list(terminal = "completed", swd = 20),
    no = list(terminal = "completed", swd = 25)
  ),
  "15" = list(yes = "25", no = "5"),
  "25" = list(
    yes = list(terminal = "completed", swd = 35),
    no = "20"
  ),
  "20" = list(
    yes = list(terminal = "completed", swd = 40),
    no = list(terminal = "completed", swd = 45)
  ),
  "5" = list(
    yes = "10",
    no = list(terminal = "completed", swd = 60)
  ),
  "10" = list(
    yes = list(terminal = "completed", swd = 50),
    no = list(terminal = "completed", swd = 55)
  )
)

STATUS_LEVELS <- c(
  "completed",
  "always_willing",
  "never_willing",
  "incomplete",
  "no_response",
  "not_assigned"
)

ADDITIONAL_DEMO_FIELDS <- c(
  "insurance_burden", "living_situation", "n_psych_admissions",
  "last_psych_admission", "onset_age_band", "family_history_schizophrenia",
  "family_history_bipolar", "family_history_depression",
  "family_history_anxiety", "family_history_ocd",
  "family_history_developmental", "drug_history_asenapine",
  "drug_history_aripiprazole", "drug_history_olanzapine",
  "drug_history_quetiapine", "drug_history_clozapine",
  "drug_history_haloperidol", "drug_history_paliperidone",
  "drug_history_brexpiprazole", "drug_history_blonanserin",
  "drug_history_perospirone", "drug_history_risperidone",
  "drug_history_lurasidone", "ae_duration", "ae_severity", "ae_interference",
  "ever_used_antipsychotic"
)

DEMO_MAP <- list(
  jp = c(
    age_band = "DE02",
    sex = "DE03",
    educ = "DE04",
    employ = "DE19",
    insurance_burden = "DE06",
    living_situation = "DE07",
    n_psych_admissions = "DE08",
    last_psych_admission = "DE09",
    onset_age_band = "DE10",
    family_history_schizophrenia = "DE11_01",
    family_history_bipolar = "DE11_02",
    family_history_depression = "DE11_03",
    family_history_anxiety = "DE11_04",
    family_history_ocd = "DE11_05",
    family_history_developmental = "DE11_06",
    drug_history_asenapine = "DE12_01",
    drug_history_aripiprazole = "DE12_02",
    drug_history_olanzapine = "DE12_03",
    drug_history_quetiapine = "DE12_04",
    drug_history_clozapine = "DE12_05",
    drug_history_haloperidol = "DE12_06",
    drug_history_paliperidone = "DE12_07",
    drug_history_brexpiprazole = "DE12_08",
    drug_history_blonanserin = "DE12_09",
    drug_history_perospirone = "DE12_10",
    drug_history_risperidone = "DE12_11",
    drug_history_lurasidone = "DE12_12",
    ae_duration = "DE13",
    ae_severity = "DE14",
    ae_interference = "DE15",
    ever_used_antipsychotic = "DE17",
    randomized_drug = "RA01",
    free_text_no_value_at_zero = "SW16_01"
  ),
  de = c(
    age_band = "DE02",
    sex = "DE03",
    educ = "DE19",
    employ = "DE05",
    country_of_residence = "DE04",
    insurance_burden = "DE06",
    living_situation = "DE07",
    n_psych_admissions = "DE08",
    last_psych_admission = "DE09",
    onset_age_band = "DE10",
    family_history_schizophrenia = "DE11_01",
    family_history_bipolar = "DE11_02",
    family_history_depression = "DE11_03",
    family_history_anxiety = "DE11_04",
    family_history_ocd = "DE11_05",
    family_history_developmental = "DE11_06",
    drug_history_aripiprazole = "DE12_01",
    drug_history_asenapine = "DE12_02",
    drug_history_blonanserin = "DE12_03",
    drug_history_brexpiprazole = "DE12_04",
    drug_history_clozapine = "DE12_05",
    drug_history_haloperidol = "DE12_06",
    drug_history_lurasidone = "DE12_07",
    drug_history_olanzapine = "DE12_08",
    drug_history_paliperidone = "DE12_09",
    drug_history_perospirone = "DE12_10",
    drug_history_quetiapine = "DE12_11",
    drug_history_risperidone = "DE12_12",
    ae_duration = "DE13",
    ae_severity = "DE14",
    ae_interference = "DE15",
    ever_used_antipsychotic = "DE16",
    randomized_drug = "RA01",
    free_text_no_value_at_zero = "SW69_01"
  )
)

AGE_LEVELS <- c("18-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75+")
SEX_LEVELS <- c("female", "male", "other_or_prefer_not_to_say")

TEST_KEYWORD_PATTERN <- "(?i)(test|テスト|てすと|tesuto|dummy|ダミー|sample|サンプル)"

# Shared analysis-set constants -----
# Expected size of the primary analysis set; the scripts stop on drift.
ANALYSIS_N_TOTAL <- 225L
ANALYSIS_N_DE <- 99L
ANALYSIS_N_JP <- 126L
# Relapse-rate reductions (percentage points) offered by the staircase.
SWD_RATE_GRID <- seq(0, 60, by = 5)
# Staircase outcomes with a usable SWD value (never willing is SWD = 60).
USABLE_SWD_STATUSES <- c("completed", "always_willing", "never_willing")
# The same set without never willing (sensitivity analysis).
USABLE_SWD_STATUSES_EXCLUDING_NEVER <- c("completed", "always_willing")
# SWD range and staircase step (percentage points of relapse rate).
SWD_FLOOR <- 0
SWD_CEILING <- 60
SWD_STEP <- 5
CONFIDENCE_LEVEL <- 0.95
ALL_COUNTRIES_LABEL <- "All"
PRIMARY_SPECIFICATION <- "primary_including_never_willing"
EXCLUDING_NEVER_SPECIFICATION <- "sensitivity_excluding_never_willing"

# 5. Function definitions -----
# Respondent IDs are personal data, so they live in an untracked file with
# columns list_name, country, respondent_id (all read as character).
read_exclusion_ids <- function(path = EXCLUSION_IDS_PATH) {
  if (!file.exists(path)) {
    stop("Exclusion ID file not found: ", path, " (see README.md)", call. = FALSE)
  }
  read_csv(path, col_types = cols(.default = col_character()))
}

select_exclusion_ids <- function(exclusion_ids, list_name) {
  exclusion_ids[exclusion_ids$list_name == list_name, , drop = FALSE]
}

# Named list (JP, DE) of character IDs, as used by the exclusion helpers.
exclusion_ids_by_country <- function(exclusion_ids, list_name) {
  selected_ids <- select_exclusion_ids(exclusion_ids, list_name)
  ids_by_country <- lapply(COUNTRY_CODES, function(country_code) {
    selected_ids$respondent_id[selected_ids$country == country_code]
  })
  names(ids_by_country) <- COUNTRY_CODES
  ids_by_country
}

# 6. Executed statements -----
# Only the exclusion lists are loaded (this file is sourced by every script).
EXCLUSION_IDS <- read_exclusion_ids()

# Test respondents identified by ID (in addition to the keyword search).
EXPLICIT_TEST_IDS <- tibble(
  country = select_exclusion_ids(EXCLUSION_IDS, "explicit_test")$country,
  respondent_id = select_exclusion_ids(EXCLUSION_IDS, "explicit_test")$respondent_id
)

# Post-hoc exclusion: free-text answer inconsistent with the staircase.
ANALYSIS_EXCLUDE <- exclusion_ids_by_country(EXCLUSION_IDS, "analysis_exclude")

# Eligibility-criterion violation: reported a psychiatric admission within the
# past 6 months in the demographics (DE09) despite screening exclusion (WE07).
RECENT_ADMISSION_EXCLUDE <- exclusion_ids_by_country(
  EXCLUSION_IDS, "recent_admission_exclude"
)
