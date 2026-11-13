#1. Copyright statement -----
# Copyright (c) 2026 the SWD study authors. Released under the MIT License (see LICENSE).

#2. Author -----
# Author: SWD study authors (see LICENSE)
# Date: 2026-11-13 (last revised)

#3. File description -----
# Purpose: Table 2: SWD summary (n, median, quartiles, mean, SD, status
#          counts) overall and by randomized drug arm, assigned track only.
# Inputs:  none (works on the individual table)
# Outputs: R_SWD/data/processed/table2.csv (written by make_table2())
# Depends: 00a_config.R (DRUG_TRACKS, PATH_OUT), 01d_swd_summary.R
#          (assigned_usable_swd_long, summarise_group); tidyverse
# Style:   r_script_style_guide.md (Tier B)

#4. source() and library() -----
# None: the calling script loads tidyverse and sources 00a_config.R and
# 01d_swd_summary.R.

# Constants -----
QUANTILE_TYPE <- 7
MIN_N_FOR_QUARTILES <- 2L
TABLE2_KEY <- c("group_type", "group", "drug")
TABLE2_COUNT_COLUMNS <- c(
  "n_records",
  "n",
  "n_completed",
  "n_never_willing",
  "n_always_willing",
  "n_incomplete",
  "n_no_response",
  "n_not_assigned"
)

#5. Function definitions -----
swd_quantile <- function(x, probability, min_n = 1L) {
  observed <- x[!is.na(x)]
  if (length(observed) < min_n) {
    return(NA_real_)
  }

  as.numeric(stats::quantile(
    observed,
    probs = probability, names = FALSE, type = QUANTILE_TYPE
  ))
}

summarise_group_quantiles <- function(swd_long, group_type, group_expr) {
  swd_long %>%
    mutate(group = {{ group_expr }}) %>%
    group_by(.data$group, .data$drug) %>%
    summarise(
      q1 = swd_quantile(.data$swd, 0.25, min_n = MIN_N_FOR_QUARTILES),
      q3 = swd_quantile(.data$swd, 0.75, min_n = MIN_N_FOR_QUARTILES),
      .groups = "drop"
    ) %>%
    mutate(
      group_type = group_type,
      group = as.character(.data$group),
      drug = as.character(.data$drug)
    ) %>%
    select(all_of(c(TABLE2_KEY, "q1", "q3")))
}

format_decimal <- function(x) {
  if_else(is.na(x), NA_character_, sprintf("%.1f", x))
}

format_median_iqr <- function(median, q1, q3) {
  if_else(
    is.na(median),
    NA_character_,
    sprintf("%.1f (%.1f, %.1f)", median, q1, q3)
  )
}

format_mean_sd <- function(mean, sd) {
  if_else(
    is.na(mean),
    NA_character_,
    sprintf("%.1f (%.1f)", mean, sd)
  )
}

key_columns_as_character <- function(summary) {
  summary %>%
    mutate(
      group_type = as.character(.data$group_type),
      group = as.character(.data$group),
      drug = as.character(.data$drug)
    )
}

# Every expected row (overall + each arm) is present, with zero counts when
# an arm has no usable SWD.
complete_table2_rows <- function(summary) {
  expected_rows <- bind_rows(
    tibble(group_type = "overall", group = "all", drug = "overall", order = 0L),
    tibble(
      group_type = "drug_arm",
      group = DRUG_TRACKS$drug,
      drug = DRUG_TRACKS$drug,
      order = seq_along(DRUG_TRACKS$drug)
    )
  )

  summary %>%
    key_columns_as_character() %>%
    right_join(expected_rows, by = TABLE2_KEY) %>%
    arrange(.data$order) %>%
    mutate(across(
      any_of(TABLE2_COUNT_COLUMNS),
      \(x) if_else(is.na(x), 0, as.integer(x))
    )) %>%
    select(-"order")
}

make_table2 <- function(individual) {
  swd_long <- assigned_usable_swd_long(individual)

  overall_swd <- swd_long %>%
    mutate(drug = "overall")

  overall_summary <- summarise_group(overall_swd, "overall", "all")
  overall_quantiles <- summarise_group_quantiles(overall_swd, "overall", "all")

  arm_summary <- summarise_group(swd_long, "drug_arm", .data$drug)
  arm_quantiles <- summarise_group_quantiles(swd_long, "drug_arm", .data$drug)

  summary_with_quartiles <- bind_rows(overall_summary, arm_summary) %>%
    key_columns_as_character() %>%
    left_join(bind_rows(overall_quantiles, arm_quantiles), by = TABLE2_KEY)

  table2 <- summary_with_quartiles %>%
    complete_table2_rows() %>%
    mutate(
      iqr = if_else(.data$n < MIN_N_FOR_QUARTILES, NA_real_, .data$iqr),
      median_iqr = format_median_iqr(.data$median, .data$q1, .data$q3),
      mean_sd = format_mean_sd(.data$mean, .data$sd)
    ) %>%
    select(all_of(c(TABLE2_KEY, "n", "median", "q1", "q3", "iqr")), everything())

  dir.create(PATH_OUT, recursive = TRUE, showWarnings = FALSE)
  write_excel_csv(table2, file.path(PATH_OUT, "table2.csv"), na = "")

  table2
}

#6. Executed statements -----
# None (sourced by 02_report_outputs.R).
