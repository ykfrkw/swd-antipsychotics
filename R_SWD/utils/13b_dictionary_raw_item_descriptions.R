#1. Copyright statement -----
# Copyright (c) 2026 the SWD study authors. Released under the MIT License (see LICENSE).

#2. Author -----
# Author: SWD study authors (see LICENSE)
# Date: 2026-11-13 (last revised)

#3. File description -----
# Purpose: English descriptions of the raw SoSci items for sheet 1 of the
#          data dictionary (13_data_dictionary.R), from the pipeline maps in
#          00a_config.R and the questionnaire. Split off from
#          13a_dictionary_derived_variables.R.
# Inputs:  none (DEMO_MAP, SWD_NODES_BY_COUNTRY from 00a_config.R)
# Outputs: none (function definitions)
# Depends: 00a_config.R; tidyverse
# Style:   r_script_style_guide.md (Tier B)

#4. source() and library() -----
# None: the calling script loads tidyverse and sources 00a_config.R.

# Constants -----
FIELD_DESCRIPTIONS <- c(
  age_band = "Age band (1 = 18-24 ... 7 = 75+)",
  sex = "Gender (1 = female, 2 = male, 3 = other / prefer not to say)",
  educ = "Highest education", employ = "Employment status",
  country_of_residence = "Country of residence",
  insurance_burden = paste0(
    "Medication cost: DE monthly out-of-pocket cost; ",
    "JP co-payment share of medical costs"
  ),
  living_situation = "Living situation",
  n_psych_admissions = "Number of psychiatric admissions (lifetime)",
  last_psych_admission = "Time since last psychiatric admission",
  onset_age_band = "Age at onset (5-year bands)",
  ae_duration = "FIBSER item 1: frequency of side effects",
  ae_severity = "FIBSER item 2: severity of side effects",
  ae_interference = "FIBSER item 3: interference/burden of side effects",
  ever_used_antipsychotic = "Ever used an antipsychotic (yes / no)",
  randomized_drug = paste0(
    "Randomised drug track (1 = risperidone, ",
    "2 = aripiprazole, 3 = olanzapine)"
  ),
  free_text_no_value_at_zero = paste0(
    "Free text, asked after 'no' at 0% ",
    "relapse (never willing): why not take ",
    "the drug even if relapse risk were zero"
  )
)
FIXED_DESCRIPTIONS <- c(
  WE02 = "Screening: consent to take part in the online study",
  WE03 = "Screening: diagnosed with schizophrenia or schizoaffective disorder",
  WE05 = "Screening: aged 18 years or older",
  WE06 = "Screening: medication changed in type or dose in the past 6 months",
  WE07 = "Screening: psychiatric inpatient treatment in the past 6 months",
  CO01 = paste0(
    "Comprehension check 1 (multi-select: symptoms of ",
    "schizophrenia); SoSci residual/count variable"
  ),
  CO01_01 = paste0(
    "Comprehension check 1, option a (hearing voices; ",
    "correct = checked)"
  ),
  CO01_02 = paste0(
    "Comprehension check 1, option b (feeling watched; ",
    "correct = checked)"
  ),
  CO01_03 = paste0(
    "Comprehension check 1, option c (raised blood pressure; ",
    "correct = not checked; not enforced by the gate)"
  ),
  CO01_04 = paste0(
    "Comprehension check 1, option d (blunted joy/sadness; ",
    "correct = checked)"
  ),
  CO02 = paste0(
    "Comprehension check 2: untreated 1-year relapse per 100 ",
    "stable patients (correct = 60; code 3 DE, 7 JP)"
  ),
  CO03 = paste0(
    "Comprehension check 3: true statement on side effects ",
    "(correct = b, drowsiness/insomnia common; code 2)"
  ),
  RA01_CP = "Randomisation urn: number of complete urn cycles so far",
  DE05 = "Country of residence (JP version; DE version: DE04)",
  DE20 = "Consent to be contacted by e-mail (gift voucher)",
  SW17_01 = paste0(
    "Free text, asked after 'yes' at 60% relapse (always ",
    "willing): why take the drug with no benefit"
  ),
  SW70_01 = paste0(
    "Free text, asked after 'yes' at 60% relapse (always ",
    "willing): why take the drug with no benefit"
  )
)
SYSTEM_VARIABLE_REGEX <- paste0(
  "^(CASE|SERIAL|REF|QUESTNNR|MODE|STARTED|",
  "TIME[0-9]+|TIME_SUM|TIME_RSI|MAILSENT|",
  "LASTDATA|STATUS|FINISHED|Q_VIEWER|LASTPAGE|",
  "MAXPAGE|MISSING|MISSREL)$"
)

OTHER_FREE_TEXT_REGEX <- "^DE0[4-7](s|_0[46])$"

#5. Function definitions -----
describe_staircase_nodes <- function(country) {
  SWD_NODES_BY_COUNTRY[[tolower(country)]] %>%
    pivot_longer(-"relapse_pct", names_to = "drug", values_to = "variable") %>%
    transmute(variable = .data$variable, description = sprintf(
      paste0(
        "Staircase (%s track): take %s if %d of 100 relapse within a ",
        "year vs 60 of 100 untreated? 1 = yes, 2 = no"
      ),
      .data$drug, .data$drug, as.integer(.data$relapse_pct)
    ))
}

describe_demographic_items <- function(country) {
  demo_map <- DEMO_MAP[[tolower(country)]]
  fields <- names(demo_map)
  description <- coalesce(
    unname(FIELD_DESCRIPTIONS[fields]),
    str_replace(fields, "^family_history_", "Family history: ") %>%
      str_replace("^drug_history_", "Use history of ") %>%
      str_replace_all("_", " ")
  )
  tibble(
    variable = unname(demo_map),
    description = paste0(description, " [harmonised as `", fields, "`]")
  )
}

describe_raw_variable <- function(variables, country) {
  lookup <- bind_rows(
    describe_staircase_nodes(country),
    describe_demographic_items(country),
    enframe(FIXED_DESCRIPTIONS,
      name = "variable",
      value = "description"
    )
  ) %>%
    distinct(.data$variable, .keep_all = TRUE)
  described <- lookup$description[match(variables, lookup$variable)]
  case_when(
    !is.na(described) ~ described,
    str_detect(variables, SYSTEM_VARIABLE_REGEX) ~ "SoSci system variable",
    str_detect(variables, OTHER_FREE_TEXT_REGEX) ~
      "Free text for an 'other' option",
    TRUE ~ NA_character_
  )
}

#6. Executed statements -----
# None (sourced by 13_data_dictionary.R).
