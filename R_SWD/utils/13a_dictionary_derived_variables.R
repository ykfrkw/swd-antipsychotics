#1. Copyright statement -----
# Copyright (c) 2026 the SWD study authors. Released under the MIT License (see LICENSE).

#2. Author -----
# Author: SWD study authors (see LICENSE)
# Date: 2026-11-13 (last revised)

#3. File description -----
# Purpose: Content of sheets 2 and 3 of the data dictionary
#          (13_data_dictionary.R): the derived / analysis variable table and
#          the sources sheet. Definitions restate the code of the cited
#          scripts.
# Inputs:  none
# Outputs: none (function definitions)
# Depends: tidyverse
# Style:   r_script_style_guide.md (Tier B)

#4. source() and library() -----
# None: the calling script loads tidyverse and sources 00a_config.R.

# Constants -----
# Scripts cited in the source_script column (where the variable is derived).
SCRIPT_HARMONIZATION <- "01b_demographics_harmonization.R"
SCRIPT_STAIRCASE <- "01c_swd_staircase.R"
SCRIPT_REGRESSION <- "02d_swd_linear_regression.R"
SCRIPT_ELIGIBLE_STARTERS <- "09a_additional_descriptives_helpers.R"
DERIVED_COLUMNS <- c("name", "definition", "type", "levels", "source_script")

#5. Function definitions -----
build_derived_dictionary <- function() {
  tribble(
    ~name, ~definition, ~type, ~levels, ~source_script,
    "respondent_id",
    "SoSci CASE number; unique within country",
    "character",
    "",
    SCRIPT_HARMONIZATION,
    "country", "Survey version", "character", "JP; DE", SCRIPT_HARMONIZATION,
    "age_band",
    "DE02 code mapped to AGE_LEVELS",
    "ordered factor",
    "18-24; 25-34; 35-44; 45-54; 55-64; 65-74; 75+",
    SCRIPT_HARMONIZATION,
    "age_band_score",
    "DE02 code (1-7) as numeric",
    "numeric",
    "1-7",
    SCRIPT_HARMONIZATION,
    "sex",
    "DE03: 1 female, 2 male, 3 other/prefer not to say",
    "factor",
    "female; male; other_or_prefer_not_to_say",
    SCRIPT_HARMONIZATION,
    "educ / employ",
    paste(
      "Original-language label of the education (JP DE04, DE DE19) /",
      "employment (JP DE19, DE DE05) item"
    ),
    "character",
    "",
    SCRIPT_HARMONIZATION,
    "educ_3level",
    paste(
      "JP DE04: 1 low, 2-4 middle, 5-7 high; DE DE19: 1-2 low, 3-5",
      "middle, 6-9 high"
    ),
    "factor",
    "low; middle; high",
    SCRIPT_HARMONIZATION,
    "educ_score",
    "educ_3level as 1/2/3",
    "numeric",
    "1-3",
    SCRIPT_HARMONIZATION,
    "employ_3level",
    paste(
      "JP DE19: 1-2 employed, 3-4 supported/other, 5 not employed; DE",
      "DE05: 1-2 employed, 3/5/6 supported/other, 4 not employed"
    ),
    "factor",
    "employed; supported_or_other; not_employed",
    SCRIPT_HARMONIZATION,
    "employ_score",
    "employ_3level as 1/2/3 (employed = 1)",
    "numeric",
    "1-3",
    SCRIPT_REGRESSION,
    "n_admissions_group",
    "DE08 code 1-6",
    "ordered factor",
    "none; 1; 2; 3-5; 6-10; >10",
    SCRIPT_HARMONIZATION,
    "n_admissions_score",
    "DE08 code minus 1",
    "numeric",
    "0-5",
    SCRIPT_HARMONIZATION,
    "n_admissions_cat",
    "n_admissions_group with 6-10 and >10 merged; reference None",
    "factor",
    "None; 1; 2; 3-5; 6 or more",
    SCRIPT_REGRESSION,
    "onset_age_group / onset_age_score",
    "DE10 code 1-16 (5-year bands) as label / numeric",
    "ordered factor / numeric",
    "<=5 ... 75+ / 1-16",
    SCRIPT_HARMONIZATION,
    "fibser_total",
    "FIBSER total = (DE13-1)+(DE14-1)+(DE15-1); NA if any item missing",
    "numeric",
    "0-18",
    SCRIPT_HARMONIZATION,
    "insurance_burden ... ever_used_antipsychotic",
    "Original-language labels of the DEMO_MAP items (see sheet 1)",
    "character",
    "",
    SCRIPT_HARMONIZATION,
    "family_history_schizophrenia / ever_used_antipsychotic (regression)",
    paste(
      "Labels recoded to Yes / No / Unknown or prefer not to answer (a",
      "level, not missing)"
    ),
    "factor",
    "No; Unknown / prefer not to answer; Yes",
    SCRIPT_REGRESSION,
    "currently_using_randomized_drug",
    paste(
      "Drug-history item of the assigned drug: currently using = Yes;",
      "previously or never used = No; don't know = NA"
    ),
    "factor",
    "No; Yes",
    SCRIPT_REGRESSION,
    "randomized_drug",
    "RA01: 1 risperidone, 2 aripiprazole, 3 olanzapine",
    "factor",
    "risperidone; aripiprazole; olanzapine",
    SCRIPT_HARMONIZATION,
    "status_{ris,ari,ola}",
    paste(
      "Staircase walk on STAIRCASE_TREE for each track; not_assigned for",
      "non-randomised tracks"
    ),
    "character",
    paste(
      "completed; always_willing; never_willing; incomplete; no_response;",
      "not_assigned"
    ),
    SCRIPT_STAIRCASE,
    "swd_{ris,ari,ola}",
    paste(
      "SWD = 60 minus the relapse rate of the last 'yes' on the completed",
      "path; always willing = 0, never willing = 60; NA otherwise"
    ),
    "numeric",
    "0-60 by 5",
    SCRIPT_STAIRCASE,
    "threshold_{ris,ari,ola}",
    "Largest relapse rate answered 'yes' on the path so far",
    "numeric",
    "0-60",
    SCRIPT_STAIRCASE,
    "n_visited_{ris,ari,ola}",
    "Number of staircase items answered on the track",
    "integer",
    "0-13",
    SCRIPT_STAIRCASE,
    "non_monotone_{ris,ari,ola}",
    paste(
      "TRUE if, sorted by relapse rate, a 'yes' follows a 'no' at a lower",
      "rate"
    ),
    "logical",
    "",
    SCRIPT_STAIRCASE,
    "swd (analysis)",
    paste(
      "SWD of the assigned track for status completed / always_willing /",
      "never_willing, after exclusions (n = 225)"
    ),
    "numeric",
    "0-60 by 5",
    SCRIPT_REGRESSION,
    "analysis exclusions",
    paste(
      "ANALYSIS_EXCLUDE (12: answers inconsistent with free text, task",
      "misunderstood) and RECENT_ADMISSION_EXCLUDE (5: admission within 6",
      "months)"
    ),
    "id list",
    "",
    "00a_config.R",
    "is_comprehension_correct",
    paste(
      "CO01 a, b, d checked and c unchecked, CO02 correct (DE 3, JP 7),",
      "CO03 = 2 (stricter than the questionnaire gate, which ignores CO01",
      "c)"
    ),
    "logical",
    "",
    SCRIPT_ELIGIBLE_STARTERS,
    "stop_stage",
    "Furthest stage reached by an eligible starter without SWD",
    "character",
    paste(
      "SWD determinable; staircase started, not finished; comprehension",
      "check passed, no staircase answer; comprehension check answered,",
      ">=1 item incorrect; after randomization, before comprehension",
      "answer"
    ),
    SCRIPT_ELIGIBLE_STARTERS,
    "has_swd",
    "status in completed / always_willing / never_willing",
    "logical",
    "",
    SCRIPT_ELIGIBLE_STARTERS,
    "swd_lower / swd_upper",
    paste(
      "Admissible SWD set of staircase stoppers: lower = 60 - smallest",
      "'no' rate + 5, upper = 60 - largest 'yes' rate; 0 / 60 otherwise"
    ),
    "numeric",
    "0-60",
    "12a_mi_helpers.R",
    "accepts current efficacy",
    "SWD <= 40 (would take a drug that lowers relapse from 60% to 20%)",
    "logical",
    "",
    "12b_mi_estimation.R"
  ) %>%
    select(all_of(DERIVED_COLUMNS))
}

build_source_notes <- function() {
  tribble(
    ~topic, ~note,
    "Column set and SoSci labels",
    "Newest data_swd_{ja,de}*.xlsx export (label row)",
    "Question text and codes",
    paste(
      "SoSci Variable View PDFs of 2026-05-22; first occurrence per",
      "variable"
    ),
    "Response codes, JP",
    "values_swd_ja*.csv (SoSci export), else Variable View PDF",
    "Response codes, DE",
    paste(
      "Variable View PDF, else R_SWD/utils/00b_de_value_labels.R",
      "(pipeline labels)"
    ),
    "Codes -9", "SoSci 'not answered'; recoded to NA in the pipeline",
    "Known gaps",
    paste(
      "2026-05-22 exports predate later items; empty codes mean no source",
      "in the raw codebooks"
    )
  )
}

#6. Executed statements -----
# None (sourced by 13_data_dictionary.R).
