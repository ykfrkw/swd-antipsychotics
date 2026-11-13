#1. Copyright statement -----
# Copyright (c) 2026 the SWD study authors. Released under the MIT License (see LICENSE).

#2. Author -----
# Author: SWD study authors (see LICENSE)
# Date: 2026-11-13 (last revised)

#3. File description -----
# Purpose: Building blocks shared by the manuscript tables: label
#          harmonization, the primary analysis set filter, categorical and
#          continuous Table 1 blocks, and the missing-data footnote helpers.
# Inputs:  none (functions receive the data)
# Outputs: none (function definitions)
# Depends: 00a_config.R; tidyverse
# Style:   r_script_style_guide.md (Tier B)

#4. source() and library() -----
# None: the calling script loads tidyverse and sources 00a_config.R.

# Constants -----
COL_TOTAL <- sprintf("Total (N=%d)", ANALYSIS_N_TOTAL)
COL_DE <- sprintf("Germany (N=%d)", ANALYSIS_N_DE)
COL_JP <- sprintf("Japan (N=%d)", ANALYSIS_N_JP)
TABLE1_COLUMN_NAMES <- c("variable", "level", COL_TOTAL, COL_DE, COL_JP)
MISSING_LEVEL <- "Missing"
EXCLUSION_KEYS <- c("country", "respondent_id")

EDUCATION_NOTE <- paste0(
  "Education harmonized across countries. Junior high school or lower = ",
  "junior high school (Japan) / no school-leaving certificate or lower ",
  "secondary school certificate (Germany). Middle (including high school) = ",
  "high school, vocational school, or junior college (Japan) / intermediate ",
  "school certificate, upper secondary school certificate, or vocational ",
  "qualification (Germany). Bachelor or higher = four-year university, ",
  "master's, or doctoral degree (Japan) / university or university of ",
  "applied sciences degree, bachelor's, master's, or doctoral degree ",
  "(Germany)."
)

EMPLOYMENT_NOTE <- paste0(
  "Employment harmonized across countries. Employed = full-time or ",
  "part-time work (both countries). Supported or other = sheltered workshop ",
  "or other (Japan) / supported employment, volunteer work, or other ",
  "(Germany). Not employed = unemployed (both countries)."
)

#5. Function definitions -----
format_n_percent <- function(n, denominator) {
  sprintf("%d (%.1f)", n, 100 * n / denominator)
}

# Empty strings are treated as missing; unknown non-missing labels stop().
harmonize_labels <- function(raw_labels, label_map, variable) {
  labels <- as.character(raw_labels)
  labels[!is.na(labels) & trimws(labels) == ""] <- NA_character_
  is_unknown <- !is.na(labels) & !(labels %in% names(label_map))
  if (any(is_unknown)) {
    stop(sprintf(
      "Unknown label(s) in %s: %s",
      variable,
      paste(sprintf("[%s]", unique(labels[is_unknown])), collapse = " | ")
    ))
  }
  unname(label_map[labels])
}

build_exclusion_table <- function(exclusions) {
  tibble(
    country = rep(names(exclusions), lengths(exclusions)),
    respondent_id = unlist(exclusions, use.names = FALSE)
  )
}

# Randomized respondents whose assigned-drug staircase is determinable,
# minus the free-text exclusions (ANALYSIS_EXCLUDE).
filter_determinable_assigned <- function(individual) {
  randomized <- individual %>%
    filter(!is.na(.data$randomized_drug), trimws(.data$randomized_drug) != "") %>%
    mutate(
      status_assigned = case_when(
        .data$randomized_drug == "risperidone" ~ .data$status_ris,
        .data$randomized_drug == "aripiprazole" ~ .data$status_ari,
        .data$randomized_drug == "olanzapine" ~ .data$status_ola,
        TRUE ~ NA_character_
      ),
      respondent_id = as.character(.data$respondent_id)
    )
  randomized %>%
    filter(.data$status_assigned %in% DETERMINABLE_STATUSES) %>%
    anti_join(build_exclusion_table(ANALYSIS_EXCLUDE), by = EXCLUSION_KEYS)
}

count_levels <- function(values, levels) {
  vapply(levels, \(level) sum(values == level, na.rm = TRUE), integer(1))
}

# Column check: shown n (+ hidden missing n in Table 1 style) = column N.
check_block_sums <- function(display, counts, hidden_counts) {
  is_matching <- sum(counts$total) + hidden_counts[["total"]] == ANALYSIS_N_TOTAL &&
    sum(counts$de) + hidden_counts[["de"]] == ANALYSIS_N_DE &&
    sum(counts$jp) + hidden_counts[["jp"]] == ANALYSIS_N_JP
  if (is_matching) {
    return(invisible(TRUE))
  }
  stop(sprintf(
    paste0(
      "Column sums (+ missing) do not match N for %s ",
      "(total %d+%d, DE %d+%d, JP %d+%d)"
    ),
    display, sum(counts$total), hidden_counts[["total"]], sum(counts$de),
    hidden_counts[["de"]], sum(counts$jp), hidden_counts[["jp"]]
  ))
}

# One categorical block from already-harmonized values.
# show_missing = TRUE appends a Missing row when needed (appendix style);
# show_missing = FALSE suppresses Missing rows (Table 1 style), so the column
# check becomes "shown n + missing n = column N".
# Returns list(table = tibble, missing = c(total, de, jp)).
build_categorical_block <- function(values, country, display, levels,
                                    show_missing) {
  is_de <- country == "DE"
  is_jp <- country == "JP"
  missing_counts <- c(
    total = sum(is.na(values)),
    de = sum(is.na(values[is_de])),
    jp = sum(is.na(values[is_jp]))
  )

  shown_levels <- levels
  if (show_missing && missing_counts[["total"]] > 0) {
    shown_levels <- c(shown_levels, MISSING_LEVEL)
    values <- ifelse(is.na(values), MISSING_LEVEL, values)
  }
  counts <- list(
    total = count_levels(values, shown_levels),
    de = count_levels(values[is_de], shown_levels),
    jp = count_levels(values[is_jp], shown_levels)
  )
  hidden_counts <- if (show_missing) {
    c(total = 0L, de = 0L, jp = 0L)
  } else {
    missing_counts
  }
  check_block_sums(display, counts, hidden_counts)

  block_table <- tibble(
    variable = display,
    level = shown_levels,
    total = vapply(counts$total, format_n_percent, character(1),
      denominator = ANALYSIS_N_TOTAL
    ),
    de = vapply(counts$de, format_n_percent, character(1),
      denominator = ANALYSIS_N_DE
    ),
    jp = vapply(counts$jp, format_n_percent, character(1),
      denominator = ANALYSIS_N_JP
    )
  )
  names(block_table) <- TABLE1_COLUMN_NAMES
  list(table = block_table, missing = missing_counts)
}

build_block_from_variable <- function(analysis, variable, display, label_map,
                                      levels, show_missing) {
  values <- harmonize_labels(analysis[[variable]], label_map, variable)
  build_categorical_block(values, analysis$country, display, levels,
    show_missing = show_missing
  )
}

format_mean_sd <- function(values) {
  sprintf(
    "%.1f (%.1f)",
    mean(values, na.rm = TRUE), sd(values, na.rm = TRUE)
  )
}

# Continuous variable: one mean (SD) row; missing counts go to the footnote.
build_numeric_block <- function(analysis, variable, display) {
  raw_values <- as.character(analysis[[variable]])
  raw_values[!is.na(raw_values) & trimws(raw_values) == ""] <- NA_character_
  values <- suppressWarnings(as.numeric(raw_values))
  is_non_numeric <- !is.na(raw_values) & is.na(values)
  if (any(is_non_numeric)) {
    stop(sprintf(
      "Non-numeric value(s) in %s: %s",
      variable, paste(unique(raw_values[is_non_numeric]), collapse = " | ")
    ))
  }

  is_de <- analysis$country == "DE"
  is_jp <- analysis$country == "JP"
  block_table <- tibble(
    variable = display,
    level = "Mean (SD)",
    total = format_mean_sd(values),
    de = format_mean_sd(values[is_de]),
    jp = format_mean_sd(values[is_jp])
  )
  names(block_table) <- TABLE1_COLUMN_NAMES

  list(
    table = block_table,
    missing = c(
      total = sum(is.na(values)),
      de = sum(is.na(values[is_de])),
      jp = sum(is.na(values[is_jp]))
    )
  )
}

summarise_block_missing <- function(display, missing_counts) {
  tibble(
    display = display,
    m_total = missing_counts[["total"]],
    m_de = missing_counts[["de"]],
    m_jp = missing_counts[["jp"]]
  )
}

format_missing_countries <- function(count_de, count_jp) {
  if (count_de > 0 && count_jp > 0) {
    return(sprintf("Germany %d / Japan %d", count_de, count_jp))
  }
  if (count_de > 0) {
    return(sprintf("Germany %d", count_de))
  }
  sprintf("Japan %d", count_jp)
}

# Variables with identical missing counts share one footnote entry.
group_missing_counts <- function(missing_log) {
  missing_log %>%
    group_by(.data$m_total, .data$m_de, .data$m_jp) %>%
    summarise(vars = paste(.data$display, collapse = ", "), .groups = "drop") %>%
    arrange(.data$m_total)
}

describe_missing_groups <- function(missing_groups) {
  paste(
    sprintf(
      "%s, n = %d (%s)",
      missing_groups$vars, missing_groups$m_total,
      mapply(format_missing_countries, missing_groups$m_de, missing_groups$m_jp)
    ),
    collapse = "; "
  )
}

#6. Executed statements -----
# None (sourced by 03_table1_by_country.R and 04_table2_subgroups.R).
