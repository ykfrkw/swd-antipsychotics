#1. Copyright statement -----
# Copyright (c) 2026 the SWD study authors. Released under the MIT License (see LICENSE).

#2. Author -----
# Author: SWD study authors (see LICENSE)
# Date: 2026-11-13 (last revised)

#3. File description -----
# Purpose: Post hoc comparison of the FIBSER total (adverse-effect burden)
#          between never-willing and all other analysed respondents
#          (Mann-Whitney test, normal approximation), with two figures:
#          the FIBSER distribution by group and the overall FIBSER histogram.
# Inputs:  R_SWD/data/processed/swd_individual.csv
# Outputs: R_SWD/outputs/figures/fibser_never_willing_test.csv,
#          R_SWD/outputs/figures/fibser_by_never_willing_regenerated.png,
#          R_SWD/outputs/figures/fibser_histogram_regenerated.png
# Depends: 00a_config.R, 01d_swd_summary.R, 01f_test_respondent_exclusion.R
#          (run 01_analysis_data.R first); tidyverse
# Style:   r_script_style_guide.md (Tier B)
#
# Run from the project root:  Rscript R_SWD/utils/15_fibser_never_willing.R
#
# Groups: the analysis set (N = 225; usable assigned-drug SWD minus the
# post hoc exclusions) split into never willing vs completed + always
# willing; respondents without a FIBSER total drop out of the test. The
# overall histogram uses every respondent in swd_individual.csv with a
# FIBSER total, as the earlier stand-alone figure did.

#4. source() and library() -----
suppressPackageStartupMessages(library(tidyverse))
source(file.path("R_SWD", "utils", "00a_config.R"))
source(file.path(PATH_UTILS, "01d_swd_summary.R"))
source(file.path(PATH_UTILS, "01f_test_respondent_exclusion.R"))

# Constants -----
FIGURE_OUTPUT_DIR <- file.path(PATH_LEGACY_OUT, "figures")
FIBSER_RANGE <- 0:18
NEVER_WILLING_LABEL <- "Never willing"
OTHERS_LABEL <- "Others: completed + always-willing"
GROUP_COLORS <- c("#4C72B0", "#C44E52")
HISTOGRAM_COLOR <- "#4C72B0"
MEAN_LINE_COLOR <- "#D62728"
MEDIAN_LINE_COLOR <- "#2CA02C"
FIGURE_DPI <- 150
FIBSER_AXIS_TITLE <- "FIBSER total score (0-18)"

#5. Function definitions -----
run_fibser_never_willing <- function() {
  individual <- read_csv(file.path(PATH_OUT, "swd_individual.csv"),
    show_col_types = FALSE
  )
  analysed <- build_fibser_analysis_set(individual)
  test_result <- test_fibser_by_never_willing(analysed)

  dir.create(FIGURE_OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
  write_excel_csv(test_result,
    file.path(FIGURE_OUTPUT_DIR, "fibser_never_willing_test.csv"),
    na = ""
  )
  save_figure(
    plot_fibser_by_group(analysed, test_result),
    "fibser_by_never_willing_regenerated.png",
    height = 7
  )
  save_figure(
    plot_fibser_histogram(as.numeric(individual$fibser_total)),
    "fibser_histogram_regenerated.png",
    height = 5
  )
  print(test_result)
}

build_fibser_analysis_set <- function(individual) {
  analysis_set <- assigned_usable_swd_long(individual) %>%
    mutate(
      respondent_id = as.character(.data$respondent_id),
      country = as.character(.data$country)
    ) %>%
    filter_analysis_exclusions()
  stopifnot(nrow(analysis_set) == ANALYSIS_N_TOTAL)

  fibser_scores <- individual %>%
    transmute(
      respondent_id = as.character(.data$respondent_id),
      country = as.character(.data$country),
      fibser_total = as.numeric(.data$fibser_total)
    )
  analysis_set %>%
    left_join(fibser_scores, by = c("country", "respondent_id")) %>%
    mutate(group = if_else(.data$status == "never_willing",
      NEVER_WILLING_LABEL, OTHERS_LABEL
    )) %>%
    filter(!is.na(.data$fibser_total))
}

test_fibser_by_never_willing <- function(analysed) {
  never_scores <- analysed$fibser_total[analysed$group == NEVER_WILLING_LABEL]
  other_scores <- analysed$fibser_total[analysed$group == OTHERS_LABEL]
  mann_whitney <- stats::wilcox.test(never_scores, other_scores, exact = FALSE)
  # Two effect sizes: rank-biserial correlation 2W / (n1 n2) - 1 (shown in
  # the figure title) and r = |z| / sqrt(N), z recovered from the p-value.
  z_value <- stats::qnorm(mann_whitney$p.value / 2)
  n_pairs <- length(never_scores) * length(other_scores)
  tibble(
    n_never_willing = length(never_scores),
    n_others = length(other_scores),
    median_never_willing = stats::median(never_scores),
    median_others = stats::median(other_scores),
    mean_never_willing = mean(never_scores),
    mean_others = mean(other_scores),
    w_statistic = unname(mann_whitney$statistic),
    p_value = mann_whitney$p.value,
    z_value = abs(z_value),
    rank_biserial_r = 2 * unname(mann_whitney$statistic) / n_pairs - 1,
    z_based_r = abs(z_value) / sqrt(length(never_scores) + length(other_scores)),
    method = paste(mann_whitney$method, "(exact = FALSE)")
  )
}

summarise_group_lines <- function(analysed) {
  analysed %>%
    group_by(.data$group) %>%
    summarise(
      median = stats::median(.data$fibser_total),
      mean = mean(.data$fibser_total),
      n = n(),
      .groups = "drop"
    ) %>%
    mutate(
      panel = sprintf("%s (n=%d)", .data$group, .data$n),
      line_label = sprintf("Median=%g\nMean=%.1f", .data$median, .data$mean)
    )
}

plot_fibser_by_group <- function(analysed, test_result) {
  group_lines <- summarise_group_lines(analysed)
  panel_levels <- group_lines$panel[match(
    c(OTHERS_LABEL, NEVER_WILLING_LABEL), group_lines$group
  )]
  group_percent <- analysed %>%
    count(.data$group, .data$fibser_total) %>%
    group_by(.data$group) %>%
    mutate(percent = 100 * .data$n / sum(.data$n)) %>%
    ungroup() %>%
    left_join(select(group_lines, "group", "panel"), by = "group")
  group_percent$panel <- factor(group_percent$panel, levels = panel_levels)
  group_lines$panel <- factor(group_lines$panel, levels = panel_levels)

  ggplot(group_percent, aes(x = .data$fibser_total, y = .data$percent)) +
    geom_col(aes(fill = .data$panel), width = 1, color = "white") +
    geom_vline(data = group_lines, aes(xintercept = .data$median), linetype = "dotted") +
    geom_vline(data = group_lines, aes(xintercept = .data$mean), linetype = "dashed") +
    geom_text(
      data = group_lines, aes(x = max(FIBSER_RANGE), y = Inf, label = .data$line_label),
      hjust = 1, vjust = 1.3, size = 3.2
    ) +
    facet_wrap(~panel, ncol = 1, scales = "free_y") +
    scale_fill_manual(values = GROUP_COLORS, guide = "none") +
    scale_x_continuous(breaks = FIBSER_RANGE, name = FIBSER_AXIS_TITLE) +
    labs(
      y = "% within group",
      title = sprintf(
        "FIBSER distribution: never-willing vs others (Mann-Whitney p=%.4f, r=%.2f)",
        test_result$p_value, test_result$rank_biserial_r
      )
    ) +
    theme_classic(base_size = 11)
}

plot_fibser_histogram <- function(fibser_total) {
  fibser_observed <- fibser_total[!is.na(fibser_total)]
  summary_lines <- tibble(
    statistic = c(
      sprintf("Mean = %.1f", mean(fibser_observed)),
      sprintf("Median = %g", stats::median(fibser_observed))
    ),
    value = c(mean(fibser_observed), stats::median(fibser_observed))
  )
  ggplot(tibble(fibser_total = fibser_observed), aes(x = .data$fibser_total)) +
    geom_bar(fill = HISTOGRAM_COLOR, color = "white", width = 1) +
    geom_vline(
      data = summary_lines,
      aes(xintercept = .data$value, color = .data$statistic, linetype = .data$statistic)
    ) +
    scale_color_manual(values = c(MEAN_LINE_COLOR, MEDIAN_LINE_COLOR), name = NULL) +
    scale_linetype_manual(values = c("dashed", "dotted"), name = NULL) +
    scale_x_continuous(breaks = FIBSER_RANGE, name = FIBSER_AXIS_TITLE) +
    labs(
      y = "Number of respondents",
      title = sprintf(
        "Distribution of FIBSER total (n=%d, SD=%.1f)",
        length(fibser_observed), stats::sd(fibser_observed)
      )
    ) +
    theme_classic(base_size = 11) +
    theme(legend.position = c(0.9, 0.9))
}

save_figure <- function(plot, file_name, height) {
  ggsave(file.path(FIGURE_OUTPUT_DIR, file_name),
    plot = plot, width = 8, height = height, dpi = FIGURE_DPI, bg = "white"
  )
}

#6. Executed statements -----
run_fibser_never_willing()
