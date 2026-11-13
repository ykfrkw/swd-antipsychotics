# 1. Copyright statement -----
# Copyright (c) 2026 the SWD study authors. Released under the MIT License (see LICENSE).

# 2. Author -----
# Author: SWD study authors (see LICENSE)
# Date: 2026-11-14 (last revised)

# 3. File description -----
# Purpose: Descriptive results: participant flow counts (Figure 2),
#          participant characteristics (Table 1, eAppendix 8 and 9), SWD by
#          subgroup (Table 2), the shape of the SWD distribution and the
#          FIBSER totals cited in the text.
# Inputs:  data/processed/swd_individual.csv; data/raw/ (screening items)
# Outputs: outputs/flow_counts.csv, characteristics_by_country.csv,
#          fibser_total_descriptives.csv, analysis_set_counts.csv,
#          swd_by_subgroup.csv, swd_distribution.csv,
#          staircase_non_monotone.csv
# Depends: 00_config.R, 00c_analysis_set.R, 01a_raw_data.R,
#          02a_category_maps.R, 02b_participant_flow.R; tidyverse
# Style:   r_script_style_guide.md (Tier B)
#
# Run from the project root:  Rscript R/02_descriptives.R
#
# Analysis
# Population: the primary analysis set (n = 225; Japan 126, Germany 99), i.e.
# randomized respondents whose staircase gave an SWD (completed, always
# willing = 0, never willing = 60), minus the post hoc exclusions. Categorical
# characteristics are n and percentage of the full country column (missing
# answers counted in their own row). The SWD is described by its median and
# interquartile range (R default quantiles, type 7) overall and by subgroup,
# with the number never willing; the shape of its distribution by skewness
# (moment estimator) and the Shapiro-Wilk test, also excluding never willing
# respondents. Nothing is modelled here.

# 4. source() and library() -----
suppressPackageStartupMessages(library(tidyverse))
source(file.path("R", "00_config.R"))
source(file.path(PATH_R, "00c_analysis_set.R"))
source(file.path(PATH_R, "01a_raw_data.R"))
source(file.path(PATH_R, "02a_category_maps.R"))
source(file.path(PATH_R, "02b_participant_flow.R"))

# Constants -----
MISSING_LEVEL <- "Missing"
DONT_KNOW_LEVEL <- "Don't know"
QUARTILE_PROBABILITIES <- c(0.25, 0.75)
CURRENT_USE_TABLE2_MAP <- c(
  "現在使っている" = "Yes", "Derzeit in Verwendung" = "Yes",
  "以前使ったことがある" = "No", "Früher verwendet" = "No",
  "使ったことがない" = "No", "Noch nie verwendet" = "No",
  "わからない/覚えていない" = DONT_KNOW_LEVEL,
  "Weiß nicht / Weiß nicht mehr" = DONT_KNOW_LEVEL
)

# 5. Function definitions -----
run_descriptives <- function() {
  individual <- read_individual()
  flow <- compute_flow_counts(individual)
  check_flow_identities(flow)
  write_output_csv(flow_counts_table(flow), "flow_counts.csv")
  analysis <- build_analysis_set(individual)
  check_recent_admissions(individual)
  write_output_csv(build_characteristics(analysis), "characteristics_by_country.csv")
  write_output_csv(describe_fibser(analysis), "fibser_total_descriptives.csv")
  write_output_csv(count_analysis_set(analysis), "analysis_set_counts.csv")
  write_output_csv(build_swd_by_subgroup(analysis), "swd_by_subgroup.csv")
  frames <- list(
    make_regression_frame(individual, USABLE_SWD_STATUSES),
    make_regression_frame(individual, USABLE_SWD_STATUSES_EXCLUDING_NEVER)
  )
  names(frames) <- c(PRIMARY_SPECIFICATION, EXCLUDING_NEVER_SPECIFICATION)
  swd_distribution <- imap(frames, \(frame, specification) {
    imap(split_by_country(frame), \(subset, country) {
      describe_swd(subset$swd) %>%
        mutate(specification = specification, country = country, .before = 1)
    }) %>%
      bind_rows()
  }) %>%
    bind_rows()
  write_output_csv(swd_distribution, "swd_distribution.csv")
  write_output_csv(count_non_monotone(analysis), "staircase_non_monotone.csv")
}

# Respondents who reported an admission within the past 6 months must be
# exactly RECENT_ADMISSION_EXCLUDE (detects drift on data updates).
check_recent_admissions <- function(individual) {
  before_exclusion <- assigned_usable_swd_long(individual) %>%
    select(all_of(RESPONDENT_KEY)) %>%
    anti_join(exclusion_list_table(ANALYSIS_EXCLUDE), by = RESPONDENT_KEY) %>%
    left_join(individual, by = RESPONDENT_KEY)
  last_admission <- map_labels(
    before_exclusion$last_psych_admission, LAST_ADMISSION_MAP, "last_psych_admission"
  )
  is_recent <- last_admission %in% LAST_ADMISSION_RECENT_LEVELS
  observed <- paste(before_exclusion$country, before_exclusion$respondent_id)[is_recent]
  expected <- with(exclusion_list_table(RECENT_ADMISSION_EXCLUDE), paste(country, respondent_id))
  stopifnot(setequal(observed, expected))
}

# n and % of the full column (Total, Germany, Japan) for each category, with
# a Missing row when any answer is missing.
count_categories <- function(categories, country, levels, table, variable) {
  shown_levels <- c(levels, if (anyNA(categories)) MISSING_LEVEL)
  categories[is.na(categories)] <- MISSING_LEVEL
  columns <- list(total = rep(TRUE, length(country)), de = country == "DE", jp = country == "JP")
  counts <- map(columns, \(is_column) {
    vapply(shown_levels, \(level) sum(categories[is_column] == level), integer(1))
  })
  stopifnot(
    sum(counts$total) == ANALYSIS_N_TOTAL, sum(counts$de) == ANALYSIS_N_DE,
    sum(counts$jp) == ANALYSIS_N_JP
  )
  tibble(
    table = table, variable = variable, level = shown_levels,
    n_total = counts$total, pct_total = 100 * counts$total / ANALYSIS_N_TOTAL,
    n_de = counts$de, pct_de = 100 * counts$de / ANALYSIS_N_DE,
    n_jp = counts$jp, pct_jp = 100 * counts$jp / ANALYSIS_N_JP
  )
}

# Other antipsychotics per respondent: currently used if any is currently
# used, else previously used, else don't know, else never used.
aggregate_other_drug_history <- function(analysis) {
  history <- vapply(paste0("drug_history_", OTHER_DRUGS), \(variable) {
    map_labels(analysis[[variable]], DRUG_HISTORY_MAP, variable)
  }, character(nrow(analysis)))
  apply(history, 1, \(answers) {
    answered <- answers[!is.na(answers)]
    if (length(answered) == 0) {
      return(NA_character_)
    }
    priority <- c("Currently used", "Previously used", "Don't know", "Never used")
    priority[min(match(answered, priority))]
  })
}

build_characteristics <- function(analysis) {
  categorical <- pmap(CATEGORICAL_VARIABLES, \(table, variable, label_map) {
    categories <- map_labels(analysis[[variable]], label_map, variable)
    count_categories(categories, analysis$country, unique(label_map), table, variable)
  })
  last_admission <- map_labels(
    analysis$last_psych_admission, LAST_ADMISSION_MAP, "last_psych_admission"
  )
  stopifnot(!any(last_admission %in% LAST_ADMISSION_RECENT_LEVELS))
  bind_rows(
    categorical,
    count_categories(
      last_admission, analysis$country, LAST_ADMISSION_TABLE_LEVELS,
      "table1", "last_psych_admission"
    ),
    count_categories(
      aggregate_other_drug_history(analysis), analysis$country, DRUG_HISTORY_LEVELS,
      "table1", "drug_history_other_antipsychotics"
    )
  )
}

describe_numeric <- function(values) {
  observed <- values[!is.na(values)]
  quartiles <- stats::quantile(observed, QUARTILE_PROBABILITIES, names = FALSE)
  tibble(
    n = length(observed), n_missing = sum(is.na(values)),
    mean = mean(observed), sd = stats::sd(observed),
    median = stats::median(observed), q1 = quartiles[1], q3 = quartiles[2]
  )
}

describe_fibser <- function(analysis) {
  imap(split_by_country(analysis), \(subset, country) {
    describe_numeric(as.numeric(subset$fibser_total)) %>%
      mutate(country = country, .before = 1)
  }) %>%
    bind_rows()
}

# Counts cited in the text: respondents currently using any antipsychotic
# (any of the 12 drug-history items "currently used") and always willing.
count_analysis_set <- function(analysis) {
  history <- vapply(paste0("drug_history_", ALL_DRUGS), \(variable) {
    map_labels(analysis[[variable]], DRUG_HISTORY_MAP, variable)
  }, character(nrow(analysis)))
  counts <- c(
    currently_using_any_antipsychotic = sum(rowSums(history == "Currently used", na.rm = TRUE) > 0),
    always_willing = sum(analysis$status_assigned == "always_willing"),
    never_willing = sum(analysis$status_assigned == "never_willing")
  )
  tibble(
    measure = names(counts), n = unname(counts), denominator = nrow(analysis),
    pct = 100 * unname(counts) / nrow(analysis)
  )
}

describe_swd_subgroup <- function(analysis, variable, categories, levels) {
  shown_levels <- c(
    levels, intersect(DONT_KNOW_LEVEL, categories),
    if (anyNA(categories)) MISSING_LEVEL
  )
  categories[is.na(categories)] <- MISSING_LEVEL
  is_never_willing <- analysis$status_assigned == "never_willing"
  map(shown_levels, \(level) {
    is_level <- categories == level
    is_reported <- level %in% levels && any(is_level)
    level_swd <- analysis$swd[is_level]
    tibble(
      variable = variable, level = level, n = sum(is_level),
      median = if (is_reported) stats::median(level_swd) else NA_real_,
      q1 = if (is_reported) stats::quantile(level_swd, 0.25, names = FALSE) else NA_real_,
      q3 = if (is_reported) stats::quantile(level_swd, 0.75, names = FALSE) else NA_real_,
      n_never_willing = if (is_reported) sum(is_never_willing[is_level]) else NA_integer_
    )
  }) %>%
    bind_rows()
}

# Table 2. Rows "Don't know" and "Missing" give only the number of
# respondents not shown in the table.
build_swd_by_subgroup <- function(analysis) {
  subgroups <- list(
    overall = list(rep("All", nrow(analysis)), "All"),
    randomized_drug = list(analysis$randomized_drug, DRUG_TRACKS$drug),
    country = list(analysis$country, c("DE", "JP")),
    currently_using_assigned_drug = list(
      map_labels(assigned_drug_history(analysis), CURRENT_USE_TABLE2_MAP, "current use"),
      c("Yes", "No")
    ),
    sex = list(map_labels(analysis$sex, SEX_MAP, "sex"), unique(SEX_MAP)),
    age_band = list(map_labels(analysis$age_band, AGE_MAP, "age_band"), unique(AGE_MAP)),
    n_admissions_group = list(
      map_labels(analysis$n_admissions_group, N_ADMISSIONS_CAT_MAP, "n_admissions_group"),
      N_ADMISSIONS_CAT_LEVELS
    ),
    employ_3level = list(
      map_labels(analysis$employ_3level, EMPLOY_MAP, "employ"), unique(EMPLOY_MAP)
    ),
    educ_3level = list(map_labels(analysis$educ_3level, EDUC_MAP, "educ"), unique(EDUC_MAP))
  )
  imap(subgroups, \(subgroup, variable) {
    describe_swd_subgroup(analysis, variable, as.character(subgroup[[1]]), subgroup[[2]])
  }) %>%
    bind_rows() %>%
    mutate(pct_never_willing = 100 * .data$n_never_willing / .data$n)
}

compute_skewness <- function(observed) {
  centred <- observed - mean(observed)
  mean(centred^3) / mean(centred^2)^1.5
}

describe_swd <- function(swd) {
  observed <- swd[!is.na(swd)]
  shapiro <- stats::shapiro.test(observed)
  describe_numeric(observed) %>%
    select(-"n_missing") %>%
    mutate(
      min = min(observed), max = max(observed),
      skewness = compute_skewness(observed),
      shapiro_w = unname(shapiro$statistic), shapiro_p = shapiro$p.value,
      pct_at_0 = 100 * mean(observed == SWD_FLOOR),
      pct_at_60 = 100 * mean(observed == SWD_CEILING)
    )
}

# Answers that say "yes" at a higher relapse rate after a "no" at a lower one.
count_non_monotone <- function(analysis) {
  is_non_monotone <- case_when(
    analysis$randomized_drug == "risperidone" ~ analysis$non_monotone_ris,
    analysis$randomized_drug == "aripiprazole" ~ analysis$non_monotone_ari,
    analysis$randomized_drug == "olanzapine" ~ analysis$non_monotone_ola
  )
  stopifnot(!anyNA(is_non_monotone))
  imap(split_by_country(mutate(analysis, is_non_monotone = is_non_monotone)), \(subset, country) {
    tibble(country = country, n = sum(subset$is_non_monotone), denominator = nrow(subset))
  }) %>%
    bind_rows()
}

# 6. Executed statements -----
run_descriptives()
