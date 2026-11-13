#1. Copyright statement -----
# Copyright (c) 2026 the SWD study authors. Released under the MIT License (see LICENSE).

#2. Author -----
# Author: SWD study authors (see LICENSE)
# Date: 2026-11-13 (last revised)

#3. File description -----
# Purpose: Table 2 for the SWD paper: SWD described by subgroup (n, median
#          (IQR), never willing n (%)) in the primary analysis set.
# Inputs:  R_SWD/data/processed/swd_individual.csv
# Outputs: R_SWD/outputs/report/table2_swd_subgroups.csv
# Depends: 00a_config.R, 03a_table_label_maps.R, 03b_table_blocks.R; tidyverse
# Style:   r_script_style_guide.md (Tier B)
#
# Run from the project root:  Rscript R_SWD/utils/04_table2_subgroups.R
#
# Analysis set: same as 03_table1_by_country.R (randomized drug with status
# completed / never_willing / always_willing, minus ANALYSIS_EXCLUDE and
# RECENT_ADMISSION_EXCLUDE). SWD includes always_willing = 0 and
# never_willing = 60 as numeric values. Missing / don't-know rows are not
# shown; their counts go into auto-generated footnotes.

#4. source() and library() -----
suppressPackageStartupMessages(library(tidyverse))
source(file.path("R_SWD", "utils", "00a_config.R"))
source(file.path(PATH_UTILS, "03a_table_label_maps.R"))
source(file.path(PATH_UTILS, "03b_table_blocks.R"))

# Constants -----
TABLE2_COLUMN_NAMES <- c(
  "variable", "level", "n", "Median (IQR)", "Never willing, n (%)"
)
EXPECTED_OVERALL_MEDIAN_SWD <- 15
LOWER_QUARTILE <- 0.25
UPPER_QUARTILE <- 0.75
DONT_KNOW <- "Don't know"
CURRENT_USE_DISPLAY <- "Currently using the assigned antipsychoticᵃ"

DRUG_ARM_MAP <- c(
  risperidone = "Risperidone",
  aripiprazole = "Aripiprazole",
  olanzapine = "Olanzapine"
)
DRUG_ARM_LEVELS <- c("Risperidone", "Aripiprazole", "Olanzapine")

COUNTRY_MAP <- c(DE = "Germany", JP = "Japan")
COUNTRY_LEVELS <- c("Germany", "Japan")

CURRENT_USE_MAP <- c(
  "現在使っている" = "Yes",
  "Derzeit in Verwendung" = "Yes",
  "以前使ったことがある" = "No",
  "Früher verwendet" = "No",
  "使ったことがない" = "No",
  "Noch nie verwendet" = "No",
  "わからない/覚えていない" = "Don't know",
  "Weiß nicht / Weiß nicht mehr" = "Don't know"
)
CURRENT_USE_LEVELS <- c("Yes", "No")

SWD_DEFINITION_NOTE <- paste0(
  ". SWD = smallest worthwhile difference (0-60 percentage points); ",
  "always willing counted as 0 and never willing as 60."
)

#5. Function definitions -----
run_table2_subgroups <- function() {
  individual <- read_csv(
    file.path(PATH_OUT, "swd_individual.csv"),
    col_types = cols(.default = col_character())
  )
  analysis <- load_table2_analysis_set(individual)
  current_use <- derive_current_use(analysis)
  report_current_use(current_use, analysis$country)

  blocks <- build_table2_blocks(analysis, current_use)
  table2 <- bind_rows(map(blocks, "table"))
  names(table2) <- TABLE2_COLUMN_NAMES
  missing_log <- bind_rows(map(blocks, "missing"))
  table2 <- bind_rows(table2, build_table2_notes(missing_log, current_use))

  dir.create(REPORT_OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
  table2_path <- file.path(REPORT_OUTPUT_DIR, "table2_swd_subgroups.csv")
  write_excel_csv(table2, table2_path, na = "")
  message(sprintf("Wrote %s (%d rows)", table2_path, nrow(table2)))
}

format_median_iqr <- function(swd) {
  sprintf(
    "%g (%g–%g)",
    stats::median(swd),
    stats::quantile(swd, LOWER_QUARTILE),
    stats::quantile(swd, UPPER_QUARTILE)
  )
}

load_table2_analysis_set <- function(individual) {
  analysis <- filter_determinable_assigned(individual) %>%
    mutate(swd = as.numeric(case_when(
      .data$randomized_drug == "risperidone" ~ .data$swd_ris,
      .data$randomized_drug == "aripiprazole" ~ .data$swd_ari,
      .data$randomized_drug == "olanzapine" ~ .data$swd_ola,
      TRUE ~ NA_character_
    ))) %>%
    anti_join(build_exclusion_table(RECENT_ADMISSION_EXCLUDE),
      by = EXCLUSION_KEYS
    )
  stopifnot(
    nrow(analysis) == ANALYSIS_N_TOTAL,
    sum(analysis$country == "JP") == ANALYSIS_N_JP,
    sum(analysis$country == "DE") == ANALYSIS_N_DE,
    all(!is.na(analysis$swd)),
    stats::median(analysis$swd) == EXPECTED_OVERALL_MEDIAN_SWD
  )
  message(sprintf(
    "Analysis set OK: total %d (Japan %d / Germany %d), overall median SWD %g",
    nrow(analysis), sum(analysis$country == "JP"),
    sum(analysis$country == "DE"), stats::median(analysis$swd)
  ))
  analysis
}

# Currently using the assigned antipsychotic: drug-history item of the
# randomized drug (exact-match map, stop on unknown labels). Returns the
# harmonized labels including "Don't know".
derive_current_use <- function(analysis) {
  current_use_raw <- case_when(
    analysis$randomized_drug == "risperidone" ~ analysis$drug_history_risperidone,
    analysis$randomized_drug == "aripiprazole" ~ analysis$drug_history_aripiprazole,
    analysis$randomized_drug == "olanzapine" ~ analysis$drug_history_olanzapine,
    TRUE ~ NA_character_
  )
  harmonize_labels(
    current_use_raw, CURRENT_USE_MAP, "currently_using_randomized_drug"
  )
}

# "Don't know" answers are hidden like missing ones.
hide_dont_know <- function(current_use) {
  ifelse(!is.na(current_use) & current_use == DONT_KNOW, NA_character_, current_use)
}

format_current_use_counts <- function(current_use) {
  current_use_shown <- hide_dont_know(current_use)
  sprintf(
    "Yes %d / No %d / Don't know %d / Missing %d",
    sum(current_use_shown == "Yes", na.rm = TRUE),
    sum(current_use_shown == "No", na.rm = TRUE),
    sum(current_use == DONT_KNOW, na.rm = TRUE),
    sum(is.na(current_use))
  )
}

report_current_use <- function(current_use, country) {
  message(paste(
    "Currently using the assigned antipsychotic:",
    format_current_use_counts(current_use)
  ))
  for (country_code in c("DE", "JP")) {
    is_country <- country == country_code
    message(sprintf(
      "  %s: %s", country_code, format_current_use_counts(current_use[is_country])
    ))
  }
}

# values: harmonized labels (NA = not shown). The shown-plus-hidden total
# must equal the analysis N. Returns list(table, missing).
build_subgroup_block <- function(values, display, levels, analysis) {
  is_never_willing <- analysis$status_assigned == "never_willing"
  rows <- lapply(levels, \(level) {
    is_level <- !is.na(values) & values == level
    n_level <- sum(is_level)
    has_rows <- n_level > 0
    tibble(
      variable = display,
      level = level,
      n = as.character(n_level),
      median_iqr = if (has_rows) format_median_iqr(analysis$swd[is_level]) else "",
      never = if (has_rows) {
        format_n_percent(sum(is_never_willing[is_level]), n_level)
      } else {
        ""
      }
    )
  })

  n_shown <- sum(!is.na(values) & values %in% levels)
  n_hidden <- sum(is.na(values))
  if (n_shown + n_hidden != ANALYSIS_N_TOTAL) {
    stop(sprintf(
      "Row sums do not match N for %s: shown %d + hidden %d != %d",
      display, n_shown, n_hidden, ANALYSIS_N_TOTAL
    ))
  }

  missing_counts <- c(
    total = n_hidden,
    de = sum(is.na(values[analysis$country == "DE"])),
    jp = sum(is.na(values[analysis$country == "JP"]))
  )
  list(
    table = bind_rows(rows),
    missing = summarise_block_missing(display, missing_counts)
  )
}

build_overall_row <- function(analysis) {
  tibble(
    variable = "Overall",
    level = "",
    n = as.character(nrow(analysis)),
    median_iqr = format_median_iqr(analysis$swd),
    never = format_n_percent(
      sum(analysis$status_assigned == "never_willing"), nrow(analysis)
    )
  )
}

build_table2_blocks <- function(analysis, current_use) {
  build_harmonized_block <- function(variable, label_map, display, levels) {
    build_subgroup_block(
      harmonize_labels(analysis[[variable]], label_map, variable),
      display, levels, analysis
    )
  }
  list(
    list(table = build_overall_row(analysis), missing = NULL),
    build_harmonized_block(
      "randomized_drug", DRUG_ARM_MAP, "Randomized drug arm", DRUG_ARM_LEVELS
    ),
    build_harmonized_block("country", COUNTRY_MAP, "Country", COUNTRY_LEVELS),
    build_subgroup_block(
      hide_dont_know(current_use), CURRENT_USE_DISPLAY, CURRENT_USE_LEVELS,
      analysis
    ),
    build_harmonized_block("sex", SEX_MAP, "Sex", SEX_LEVELS_EN),
    build_harmonized_block("age_band", AGE_MAP, "Age band", AGE_TABLE_LEVELS),
    build_harmonized_block(
      "n_admissions_group", ADMISSIONS_MAP, "Psychiatric admissions",
      ADMISSIONS_LEVELS
    ),
    build_harmonized_block(
      "employ_3level", EMPLOY_MAP, "Employment (3-level)ᵇ", EMPLOY_LEVELS
    ),
    build_harmonized_block(
      "educ_3level", EDUC_MAP, "Education (3-level)ᶜ", EDUC_LEVELS
    )
  )
}

# The currently-using block has its own footnote (a), so it is left out of
# the generic missing-data sentence.
format_table2_missing_note <- function(missing_log) {
  missing_groups <- missing_log %>%
    filter(.data$m_total > 0, .data$display != CURRENT_USE_DISPLAY) %>%
    group_missing_counts()
  if (nrow(missing_groups) == 0) {
    return("No missing responses among the subgrouping variables.")
  }
  paste0(
    "Missing responses are not shown; medians and percentages are based on ",
    "the respondents shown in each row. Respondents with missing data: ",
    describe_missing_groups(missing_groups),
    SWD_DEFINITION_NOTE
  )
}

build_table2_notes <- function(missing_log, current_use) {
  current_use_note <- sprintf(
    paste0(
      "Derived from the drug-history item for the randomized drug. ",
      "Yes = currently using; No = previously used or never used. ",
      "Respondents who answered don't know (n = %d) or had a missing ",
      "drug-history response (n = %d) are not shown."
    ),
    sum(current_use == DONT_KNOW, na.rm = TRUE), sum(is.na(current_use))
  )
  note_rows <- tibble(
    variable = "Note",
    level = c("ᵃ", "ᵇ", "ᶜ", ""),
    n = c(
      current_use_note, EMPLOYMENT_NOTE, EDUCATION_NOTE,
      format_table2_missing_note(missing_log)
    ),
    median_iqr = "",
    never = ""
  )
  names(note_rows) <- TABLE2_COLUMN_NAMES
  note_rows
}

#6. Executed statements -----
run_table2_subgroups()
