#1. Copyright statement -----
# Copyright (c) 2026 the SWD study authors. Released under the MIT License (see LICENSE).

#2. Author -----
# Author: SWD study authors (see LICENSE)
# Date: 2026-11-13 (last revised)

#3. File description -----
# Purpose: Table 1 by country (Total / Germany / Japan) and the supplementary
#          appendix table (Table S1) for the SWD paper.
# Inputs:  R_SWD/data/processed/swd_individual.csv
# Outputs: R_SWD/outputs/report/table1_country.csv,
#          R_SWD/outputs/report/table_s1_appendix.csv,
#          R_SWD/outputs/report/analysis_set_counts.csv
# Depends: 00a_config.R, 03a_table_label_maps.R, 03b_table_blocks.R,
#          03c_table1_analysis_set.R; tidyverse
# Style:   r_script_style_guide.md (Tier B)
#
# Run from the project root:  Rscript R_SWD/utils/03_table1_by_country.R
#
# Analysis set: respondents with a randomized drug whose status for that drug
# is completed / never_willing / always_willing, minus ANALYSIS_EXCLUDE
# (free-text inconsistency) and RECENT_ADMISSION_EXCLUDE (eligibility-criterion
# violation: psychiatric admission within the past 6 months reported in the
# demographics). The expected N comes from 00a_config.R; the script stops
# otherwise. Table 1 shows no Missing rows (missing counts go into an
# auto-generated footnote); the appendix keeps explicit Missing rows.

#4. source() and library() -----
suppressPackageStartupMessages(library(tidyverse))
source(file.path("R_SWD", "utils", "00a_config.R"))
source(file.path(PATH_UTILS, "03a_table_label_maps.R"))
source(file.path(PATH_UTILS, "03b_table_blocks.R"))
source(file.path(PATH_UTILS, "03c_table1_analysis_set.R"))

# Constants -----
OTHER_DRUGS_DISPLAY <- "Drug history: other antipsychoticsᵈ"
LAST_ADMISSION_DISPLAY <- "Last psychiatric admission"
FIBSER_DISPLAY <- "FIBSER total (0-18), mean (SD)"
NOTE_SUPERSCRIPTS <- c("ᵃ", "ᵇ", "ᶜ", "ᵈ", "")

COST_NOTE <- paste0(
  "Germany: asked directly as monthly out-of-pocket medication cost in the ",
  "bands shown. Japan: asked as the co-payment share of medical costs (0%, ",
  "10%, or 30%, the latter typical of standard health insurance; 0% ",
  "includes public assistance recipients); shares were converted to ",
  "approximate monthly amounts assuming a typical antipsychotic ",
  "prescription of about 10,000 yen per month (0% = 0 yen; 10% ≈ 1,000 ",
  "yen ≈ 6 Euro; 30% ≈ 3,000 yen ≈ 17 Euro; 180 yen per Euro) ",
  "and assigned to the corresponding band."
)

OTHER_DRUGS_NOTE <- paste0(
  "Other antipsychotics = asenapine, quetiapine, clozapine, haloperidol, ",
  "paliperidone, brexpiprazole, blonanserin, perospirone, or lurasidone, ",
  "aggregated per respondent (currently used if any is currently used; ",
  "otherwise previously used if any was previously used; otherwise don't ",
  "know if any response was don't know; otherwise never used)."
)

DENOMINATOR_TEXT <- sprintf(
  paste0(
    "percentages are based on the full column denominators ",
    "(total N = %d, Germany N = %d, Japan N = %d)."
  ),
  ANALYSIS_N_TOTAL, ANALYSIS_N_DE, ANALYSIS_N_JP
)

#5. Function definitions -----
run_table1_by_country <- function() {
  individual <- read_csv(
    file.path(PATH_OUT, "swd_individual.csv"),
    col_types = cols(.default = col_character())
  )
  analysis_set <- load_table1_analysis_set(individual)

  table1_country <- build_table1(
    analysis_set$analysis, analysis_set$last_admission_values
  )
  write_report_table(table1_country, "table1_country.csv")

  table_s1 <- build_table_s1(analysis_set$analysis)
  write_report_table(table_s1, "table_s1_appendix.csv")

  analysis_set_counts <- summarise_analysis_set_counts(analysis_set$analysis)
  write_excel_csv(
    analysis_set_counts,
    file.path(REPORT_OUTPUT_DIR, "analysis_set_counts.csv"),
    na = ""
  )
}

# Table 1 blocks in display order (no Missing rows).
build_table1_blocks <- function(analysis, last_admission_values) {
  build_row_block <- function(variable, display, label_map, levels) {
    build_block_from_variable(analysis, variable, display, label_map, levels,
      show_missing = FALSE
    )
  }
  c(
    list(
      build_row_block("age_band", "Age band", AGE_MAP, AGE_TABLE_LEVELS),
      build_row_block("sex", "Sex", SEX_MAP, SEX_LEVELS_EN),
      build_row_block(
        "onset_age_group", "Age at onset, years", ONSET_MAP, ONSET_TABLE_LEVELS
      ),
      build_row_block(
        "educ_3level", "Education (3-level)ᵃ", EDUC_MAP, EDUC_LEVELS
      ),
      build_row_block(
        "employ_3level", "Employment (3-level)ᵇ", EMPLOY_MAP, EMPLOY_LEVELS
      ),
      build_row_block(
        "living_situation", "Living situation", LIVING_MAP, LIVING_LEVELS
      ),
      build_row_block(
        "insurance_burden", "Monthly out-of-pocket medication costᶜ",
        COST_MAP, COST_LEVELS
      ),
      build_row_block(
        "n_admissions_group", "Psychiatric admissions",
        ADMISSIONS_MAP, ADMISSIONS_LEVELS
      ),
      build_categorical_block(
        last_admission_values, analysis$country, LAST_ADMISSION_DISPLAY,
        LAST_ADMISSION_TABLE_LEVELS,
        show_missing = FALSE
      ),
      build_row_block(
        "family_history_schizophrenia", "Family history: schizophrenia",
        FAMILY_HISTORY_MAP, FAMILY_HISTORY_LEVELS
      ),
      build_row_block(
        "ever_used_antipsychotic", "Ever used antipsychotic",
        EVER_ANTIPSYCHOTIC_MAP, EVER_ANTIPSYCHOTIC_LEVELS
      )
    ),
    map(names(TABLE1_DRUG_VARS), \(variable) {
      build_row_block(
        variable, TABLE1_DRUG_VARS[[variable]],
        DRUG_HISTORY_MAP, DRUG_HISTORY_LEVELS
      )
    }),
    list(
      build_categorical_block(
        aggregate_other_drug_history(analysis), analysis$country,
        OTHER_DRUGS_DISPLAY, DRUG_HISTORY_LEVELS,
        show_missing = FALSE
      ),
      build_numeric_block(analysis, "fibser_total", FIBSER_DISPLAY)
    )
  )
}

format_table1_missing_note <- function(missing_log) {
  missing_groups <- missing_log %>%
    filter(.data$m_total > 0) %>%
    group_missing_counts()
  if (nrow(missing_groups) == 0) {
    return(paste("No missing responses;", DENOMINATOR_TEXT))
  }
  paste0(
    "Missing responses are not shown; ", DENOMINATOR_TEXT,
    " Respondents with missing data: ",
    describe_missing_groups(missing_groups),
    "."
  )
}

build_table1_notes <- function(missing_note) {
  note_rows <- tibble(
    variable = "Note",
    level = NOTE_SUPERSCRIPTS,
    total = c(
      EDUCATION_NOTE, EMPLOYMENT_NOTE, COST_NOTE, OTHER_DRUGS_NOTE, missing_note
    ),
    de = "",
    jp = ""
  )
  names(note_rows) <- TABLE1_COLUMN_NAMES
  note_rows
}

# Footnotes are appended after all assertions (excluded from column sums).
build_table1 <- function(analysis, last_admission_values) {
  blocks <- build_table1_blocks(analysis, last_admission_values)
  missing_log <- bind_rows(map(blocks, \(block) {
    summarise_block_missing(block$table$variable[1], block$missing)
  }))
  bind_rows(
    bind_rows(map(blocks, "table")),
    build_table1_notes(format_table1_missing_note(missing_log))
  )
}

# Appendix (Table S1): full distributions with Missing rows.
build_table_s1 <- function(analysis) {
  build_appendix_block <- function(variable, display, label_map, levels) {
    build_block_from_variable(analysis, variable, display, label_map, levels,
      show_missing = TRUE
    )$table
  }
  family_blocks <- map(names(FAMILY_HISTORY_APPENDIX_VARS), \(variable) {
    build_appendix_block(
      variable, FAMILY_HISTORY_APPENDIX_VARS[[variable]],
      FAMILY_HISTORY_MAP, FAMILY_HISTORY_LEVELS
    )
  })
  drug_blocks <- map(names(DRUG_HISTORY_VARS), \(variable) {
    build_appendix_block(
      variable, DRUG_HISTORY_VARS[[variable]],
      DRUG_HISTORY_MAP, DRUG_HISTORY_LEVELS
    )
  })
  fibser_blocks <- list(
    build_appendix_block(
      "ae_duration", "FIBSER: adverse-effect duration",
      AE_DURATION_MAP, AE_DURATION_LEVELS
    ),
    build_appendix_block(
      "ae_severity", "FIBSER: adverse-effect severity",
      AE_SEVERITY_MAP, AE_SEVERITY_LEVELS
    ),
    build_appendix_block(
      "ae_interference", "FIBSER: adverse-effect interference",
      AE_INTERFERENCE_MAP, AE_INTERFERENCE_LEVELS
    )
  )
  bind_rows(c(family_blocks, drug_blocks, fibser_blocks))
}

write_report_table <- function(report_table, file_name) {
  dir.create(REPORT_OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
  table_path <- file.path(REPORT_OUTPUT_DIR, file_name)
  write_excel_csv(report_table, table_path, na = "")
  message(sprintf(
    "Wrote %s (%d rows, %d variables)",
    table_path, nrow(report_table), length(unique(report_table$variable))
  ))
}

#6. Executed statements -----
run_table1_by_country()
