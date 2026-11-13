#1. Copyright statement -----
# Copyright (c) 2026 the SWD study authors. Released under the MIT License (see LICENSE).

#2. Author -----
# Author: SWD study authors (see LICENSE)
# Date: 2026-11-13 (last revised)

#3. File description -----
# Purpose: Table 1 by randomized drug arm: n (%) per level of each
#          demographic variable, overall and per arm, with a Missing level.
# Inputs:  none (works on the individual table)
# Outputs: R_SWD/data/processed/table1.csv (written by make_table1())
# Depends: 00a_config.R (ADDITIONAL_DEMO_FIELDS, AGE_LEVELS, SEX_LEVELS,
#          DRUG_TRACKS, PATH_OUT); tidyverse
# Style:   r_script_style_guide.md (Tier B)

#4. source() and library() -----
# None: the calling script loads tidyverse and sources 00a_config.R.

# Constants -----
TABLE1_MISSING_LEVEL <- "Missing"
UNKNOWN_LEVEL <- "Unknown / prefer not to answer"
BINARY_LEVELS <- c("Yes", "No")
FAMILY_HISTORY_PATTERN <- "^family_history_"
DRUG_HISTORY_PATTERN <- "^drug_history_"
EVER_USED_VARIABLE <- "ever_used_antipsychotic"
TABLE1_VARIABLE_LABELS <- c(
  country = "Country",
  age_band = "Age band",
  sex = "Sex",
  educ = "Education",
  employ = "Employment",
  insurance_burden = "Insurance burden",
  living_situation = "Living situation",
  n_psych_admissions = "Psychiatric admissions",
  last_psych_admission = "Last psychiatric admission",
  onset_age_band = "Age at onset",
  family_history_schizophrenia = "Family history: schizophrenia",
  family_history_bipolar = "Family history: bipolar disorder",
  family_history_depression = "Family history: depression",
  family_history_anxiety = "Family history: anxiety disorder",
  family_history_ocd = "Family history: obsessive-compulsive disorder",
  family_history_developmental = "Family history: developmental disorder",
  drug_history_asenapine = "Drug history: asenapine",
  drug_history_aripiprazole = "Drug history: aripiprazole",
  drug_history_olanzapine = "Drug history: olanzapine",
  drug_history_quetiapine = "Drug history: quetiapine",
  drug_history_clozapine = "Drug history: clozapine",
  drug_history_haloperidol = "Drug history: haloperidol",
  drug_history_paliperidone = "Drug history: paliperidone",
  drug_history_brexpiprazole = "Drug history: brexpiprazole",
  drug_history_blonanserin = "Drug history: blonanserin",
  drug_history_perospirone = "Drug history: perospirone",
  drug_history_risperidone = "Drug history: risperidone",
  drug_history_lurasidone = "Drug history: lurasidone",
  ae_duration = "Adverse-effect duration",
  ae_severity = "Adverse-effect severity",
  ae_interference = "Adverse-effect interference",
  ever_used_antipsychotic = "Ever used an antipsychotic"
)

#5. Function definitions -----
table1_variable_order <- function() {
  c("country", "age_band", "sex", "educ", "employ", ADDITIONAL_DEMO_FIELDS)
}

table1_variable_labels <- function() {
  TABLE1_VARIABLE_LABELS
}

label_table1_variable <- function(variable) {
  labels <- table1_variable_labels()
  variable_label <- unname(labels[variable])
  ifelse(is.na(variable_label), gsub("_", " ", variable), variable_label)
}

is_yes_no_variable <- function(variable) {
  grepl(FAMILY_HISTORY_PATTERN, variable) | variable == EVER_USED_VARIABLE
}

is_blank_response <- function(response_text) {
  is.na(response_text) | trimws(response_text) == ""
}

# The \u escapes are the Japanese answer labels (yes, no, don't know).
collapse_binary_response <- function(x) {
  response_text <- as.character(x)
  lower <- tolower(trimws(response_text))

  case_when(
    is_blank_response(response_text) ~ NA_character_,
    grepl("^(yes|y|1)$", lower) | grepl("\\u306f\\u3044", response_text) ~ "Yes",
    grepl("^(no|n|2)$", lower) | grepl("\\u3044\\u3044\\u3048", response_text) ~ "No",
    grepl("unknown|prefer", lower) |
      grepl("\\u308f\\u304b\\u3089\\u306a\\u3044", response_text) ~ UNKNOWN_LEVEL,
    TRUE ~ response_text
  )
}

# The \u escapes are the Japanese answer labels (currently, previously, never).
collapse_drug_history_response <- function(x) {
  response_text <- as.character(x)
  lower <- tolower(trimws(response_text))

  case_when(
    is_blank_response(response_text) ~ NA_character_,
    grepl("current|previous|ever|used", lower) |
      grepl("\\u73fe\\u5728", response_text) |
      grepl("\\u4ee5\\u524d", response_text) ~ "Yes",
    grepl("never|not used|no", lower) |
      grepl("\\u4f7f\\u3063\\u305f\\u3053\\u3068\\u304c\\u306a\\u3044", response_text) ~ "No",
    TRUE ~ response_text
  )
}

normalise_table1_values <- function(x, variable) {
  if (is_yes_no_variable(variable)) {
    return(collapse_binary_response(x))
  }
  if (grepl(DRUG_HISTORY_PATTERN, variable)) {
    return(collapse_drug_history_response(x))
  }

  response_text <- as.character(x)
  if_else(is_blank_response(response_text), NA_character_, response_text)
}

ordered_table1_levels <- function(values, variable) {
  observed <- unique(values[!is.na(values)])

  preferred <- case_when(
    variable == "age_band" ~ list(AGE_LEVELS),
    variable == "sex" ~ list(SEX_LEVELS),
    is_yes_no_variable(variable) ~ list(c(BINARY_LEVELS, UNKNOWN_LEVEL)),
    grepl(DRUG_HISTORY_PATTERN, variable) ~ list(BINARY_LEVELS),
    TRUE ~ list(character())
  )[[1]]

  table_levels <- c(preferred[preferred %in% observed], observed[!observed %in% preferred])
  if (length(table_levels) == 0) {
    table_levels <- character()
  }

  if (any(is.na(values))) {
    table_levels <- c(table_levels, TABLE1_MISSING_LEVEL)
  }

  table_levels
}

format_n_percent <- function(n, denominator) {
  if (is.na(denominator) || denominator == 0) {
    return(sprintf("%d (NA)", n))
  }

  sprintf("%d (%.1f)", n, 100 * n / denominator)
}

summarise_table1_column <- function(respondents, variable, table_levels, column_name) {
  denominator <- nrow(respondents)
  values <- normalise_table1_values(respondents[[variable]], variable)
  values[is.na(values)] <- TABLE1_MISSING_LEVEL

  column_summary <- tibble(
    level = table_levels,
    value = vapply(
      table_levels,
      \(level) format_n_percent(sum(values == level, na.rm = TRUE), denominator),
      character(1)
    )
  )
  names(column_summary)[names(column_summary) == "value"] <- column_name
  column_summary
}

make_table1_variable <- function(individual, variable) {
  variable_values <- normalise_table1_values(individual[[variable]], variable)
  table_levels <- ordered_table1_levels(variable_values, variable)

  if (length(table_levels) == 0) {
    table_levels <- TABLE1_MISSING_LEVEL
  }

  variable_rows <- tibble(
    variable = label_table1_variable(variable),
    level = table_levels
  )

  overall <- summarise_table1_column(individual, variable, table_levels, "Overall")
  variable_rows <- left_join(variable_rows, overall, by = "level")

  for (drug in DRUG_TRACKS$drug) {
    arm_respondents <- individual %>%
      filter(as.character(.data$randomized_drug) == drug)

    arm_summary <- summarise_table1_column(arm_respondents, variable, table_levels, drug)
    variable_rows <- left_join(variable_rows, arm_summary, by = "level")
  }

  variable_rows
}

make_table1 <- function(individual) {
  variables <- table1_variable_order()
  variables <- variables[variables %in% names(individual)]

  table1 <- bind_rows(lapply(variables, \(variable) {
    make_table1_variable(individual, variable)
  }))

  dir.create(PATH_OUT, recursive = TRUE, showWarnings = FALSE)
  write_excel_csv(table1, file.path(PATH_OUT, "table1.csv"), na = "")

  table1
}

#6. Executed statements -----
# None (sourced by 02_report_outputs.R).
