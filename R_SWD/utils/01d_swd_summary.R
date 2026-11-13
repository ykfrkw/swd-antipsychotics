#1. Copyright statement -----
# Copyright (c) 2026 the SWD study authors. Released under the MIT License (see LICENSE).

#2. Author -----
# Author: SWD study authors (see LICENSE)
# Date: 2026-11-13 (last revised)

#3. File description -----
# Purpose: Join demographics and SWD into the individual table, reshape it to
#          one row per respondent and drug track, and summarise SWD by group
#          (as a data frame and as an HTML table).
# Inputs:  none (works on the harmonized demographics and SWD tables)
# Outputs: none (function definitions)
# Depends: 00a_config.R (ADDITIONAL_DEMO_FIELDS, DRUG_TRACKS,
#          USABLE_SWD_STATUSES); tidyverse, gt (optional)
# Style:   r_script_style_guide.md (Tier B)

#4. source() and library() -----
# None: the calling script loads tidyverse and sources 00a_config.R.

# Constants -----
SWD_TRACK_COLUMN_PATTERN <- "^(swd|status|threshold|n_visited|non_monotone)_"
SWD_SUMMARY_TITLE <- "SWD Summary"
SWD_SUMMARY_CSS <- paste0(
  "body{font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif;",
  "margin:24px;color:#1f2933;}",
  "table{border-collapse:collapse;width:100%;font-size:13px;}",
  "th,td{border:1px solid #d8dee4;padding:6px 8px;text-align:left;",
  "vertical-align:top;}",
  "th{background:#f6f8fa;font-weight:600;}",
  "caption{caption-side:top;text-align:left;font-size:18px;font-weight:700;",
  "margin-bottom:12px;}"
)

#5. Function definitions -----
swd_stat <- function(x, fn, min_n = 1L) {
  observed <- x[!is.na(x)]
  if (length(observed) < min_n) {
    return(NA_real_)
  }
  as.numeric(fn(observed))
}

make_individual_table <- function(demo, swd) {
  demo %>%
    left_join(swd, by = c("country", "respondent_id")) %>%
    select(all_of(c(
      "respondent_id",
      "country",
      "age_band",
      "sex",
      "educ",
      "employ",
      "age_band_score",
      "educ_3level",
      "educ_score",
      "employ_3level",
      "n_admissions_group",
      "n_admissions_score",
      "onset_age_group",
      "onset_age_score",
      "fibser_total",
      ADDITIONAL_DEMO_FIELDS,
      "randomized_drug",
      "swd_ris",
      "swd_ari",
      "swd_ola",
      "status_ris",
      "status_ari",
      "status_ola",
      "threshold_ris",
      "threshold_ari",
      "threshold_ola",
      "n_visited_ris",
      "n_visited_ari",
      "n_visited_ola",
      "non_monotone_ris",
      "non_monotone_ari",
      "non_monotone_ola"
    )))
}

make_swd_long <- function(individual) {
  individual %>%
    select(
      all_of(c("respondent_id", "country", "age_band", "sex", "randomized_drug")),
      matches(SWD_TRACK_COLUMN_PATTERN)
    ) %>%
    pivot_longer(
      cols = matches(SWD_TRACK_COLUMN_PATTERN),
      names_to = c(".value", "drug_code"),
      names_pattern = "^(swd|status|threshold|n_visited|non_monotone)_(ris|ari|ola)$"
    ) %>%
    mutate(
      drug = recode(
        .data$drug_code,
        ris = "risperidone",
        ari = "aripiprazole",
        ola = "olanzapine"
      ),
      drug = factor(.data$drug, levels = DRUG_TRACKS$drug)
    )
}

# Rows of the randomized (assigned) drug track with a usable SWD value.
# Moved here from 02b_table2_by_drug.R so the figures and regressions share it.
assigned_usable_swd_long <- function(individual, statuses = USABLE_SWD_STATUSES) {
  make_swd_long(individual) %>%
    filter(
      as.character(.data$drug) == as.character(.data$randomized_drug),
      .data$status %in% statuses,
      !is.na(.data$swd)
    )
}

summarise_group <- function(swd_long, group_type, group_expr) {
  group_summary <- swd_long %>%
    mutate(group = {{ group_expr }}) %>%
    group_by(.data$group, .data$drug) %>%
    summarise(
      n_records = n(),
      n = sum(!is.na(.data$swd)),
      mean = swd_stat(.data$swd, mean),
      sd = swd_stat(.data$swd, stats::sd, min_n = 2L),
      median = swd_stat(.data$swd, stats::median),
      iqr = swd_stat(.data$swd, stats::IQR),
      min = swd_stat(.data$swd, min),
      max = swd_stat(.data$swd, max),
      n_completed = sum(.data$status == "completed", na.rm = TRUE),
      n_never_willing = sum(.data$status == "never_willing", na.rm = TRUE),
      n_always_willing = sum(.data$status == "always_willing", na.rm = TRUE),
      n_incomplete = sum(.data$status == "incomplete", na.rm = TRUE),
      n_no_response = sum(.data$status == "no_response", na.rm = TRUE),
      n_not_assigned = sum(.data$status == "not_assigned", na.rm = TRUE),
      .groups = "drop"
    )

  group_summary %>%
    mutate(
      group_type = group_type,
      group = as.character(.data$group)
    ) %>%
    select(all_of(c("group_type", "group", "drug")), everything())
}

make_summary_table <- function(individual) {
  swd_long <- make_swd_long(individual)

  bind_rows(
    summarise_group(swd_long, "overall", "all"),
    summarise_group(swd_long, "country", .data$country),
    summarise_group(swd_long, "age_band", .data$age_band),
    summarise_group(swd_long, "sex", .data$sex)
  ) %>%
    arrange(.data$group_type, .data$group, .data$drug)
}

escape_html <- function(x) {
  escaped <- as.character(x)
  escaped[is.na(escaped)] <- ""
  escaped <- gsub("&", "&amp;", escaped, fixed = TRUE)
  escaped <- gsub("<", "&lt;", escaped, fixed = TRUE)
  escaped <- gsub(">", "&gt;", escaped, fixed = TRUE)
  escaped <- gsub('"', "&quot;", escaped, fixed = TRUE)
  escaped
}

summary_to_html <- function(summary) {
  headers <- names(summary)
  header_html <- paste0("<th>", escape_html(headers), "</th>", collapse = "")
  body_html <- apply(summary, 1, \(row) {
    paste0("<tr><td>", paste0(escape_html(row), collapse = "</td><td>"), "</td></tr>")
  })

  paste0(
    "<!doctype html>\n",
    "<html lang=\"en\">\n",
    "<head>\n",
    "<meta charset=\"utf-8\">\n",
    "<title>", SWD_SUMMARY_TITLE, "</title>\n",
    "<style>",
    SWD_SUMMARY_CSS,
    "</style>\n",
    "</head>\n",
    "<body>\n",
    "<table>\n",
    "<caption>", SWD_SUMMARY_TITLE, "</caption>\n",
    "<thead><tr>",
    header_html,
    "</tr></thead>\n",
    "<tbody>\n",
    paste(body_html, collapse = "\n"),
    "\n</tbody>\n",
    "</table>\n",
    "</body>\n",
    "</html>\n"
  )
}

make_gt_or_html <- function(summary) {
  if (!requireNamespace("gt", quietly = TRUE)) {
    return(summary_to_html(summary))
  }

  summary %>%
    gt::gt() %>%
    gt::tab_header(title = SWD_SUMMARY_TITLE) %>%
    gt::fmt_number(
      columns = c("mean", "sd", "median", "iqr", "min", "max"),
      decimals = 1
    )
}

make_summary <- function(demo, swd) {
  individual <- make_individual_table(demo, swd)
  summary <- make_summary_table(individual)
  gt_table <- make_gt_or_html(summary)

  list(
    individual = individual,
    summary = summary,
    gt_table = gt_table
  )
}

#6. Executed statements -----
# None (sourced by 01_analysis_data.R, 02_report_outputs.R and the later
# analysis scripts that rebuild the long SWD table).
