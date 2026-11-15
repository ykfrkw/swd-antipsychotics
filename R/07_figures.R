# 1. Copyright statement -----
# Copyright (c) 2026 Yuki Furukawa. Released under the MIT License (see LICENSE).

# 2. Author -----
# Author: SWD study authors (see LICENSE)
# Date: 2026-11-14 (last revised)

# 3. File description -----
# Purpose: Figures: participant flow (Figure 2), distribution of the SWD
#          with cumulative percentiles (Figure 3) and FIBSER total by
#          willingness to take the drug (eAppendix 13). Plain ggplot2.
# Inputs:  data/processed/swd_individual.csv, outputs/flow_counts.csv
#          (from 02_descriptives.R)
# Outputs: outputs/figure2_flow_diagram.png, outputs/figure3_swd_distribution.png,
#          outputs/figure3_swd_distribution_data.csv,
#          outputs/eappendix13_fibser_by_never_willing.png
# Depends: 00_config.R, 00c_analysis_set.R; tidyverse
#
# Run from the project root:  Rscript R/07_figures.R
#
# Analysis
# Figure 3: percentage of the analysis set (n = 225) at each SWD value,
# stacked by country; never willing respondents (SWD = 60 by design) are a
# separate column; the secondary axis gives the cumulative percentage
# (always willing + completed up to that SWD, never willing last). Lines mark
# the median and the real risk difference of maintenance treatment (40).

# 4. source() and library() -----
suppressPackageStartupMessages(library(tidyverse))
source(file.path("R", "00_config.R"))
source(file.path(PATH_R, "00c_analysis_set.R"))

# Constants -----
NEVER_WILLING_X <- 67.5
REAL_RISK_DIFFERENCE <- 40
FIGURE_DPI <- 300
COUNTRY_NAMES <- c(JP = "Japan", DE = "Germany")
FIBSER_RANGE <- 0:18
# Flow diagram: stage, column (1 = main, 2 = exclusions) and row.
FLOW_LAYOUT <- tribble(
  ~stage, ~column, ~row, ~label,
  "clicked", 1, 1, "Clicked the survey link",
  "left_info", 2, 1.5, "Left at the study information page",
  "entered", 1, 2, "Entered the survey",
  "test_removed", 2, 2.5, "Test entry removed",
  "assessed", 1, 3, "Assessed for eligibility",
  "ineligible", 2, 3.5, "Ineligible or left before randomization",
  "randomized", 1, 4, "Randomized to one antipsychotic",
  "no_swd", 2, 4.5, "Staircase not answered or incomplete",
  "determinable", 1, 5, "SWD determinable",
  "excluded", 2, 5.5, "Excluded post hoc",
  "analyzed", 1, 6, "Included in analysis"
)

# 5. Function definitions -----
run_figures <- function() {
  individual <- read_individual()
  analysis <- build_analysis_set(individual)
  save_figure(plot_flow_diagram(), "figure2_flow_diagram.png", width = 8, height = 8)
  distribution <- swd_distribution_data(analysis)
  write_output_csv(distribution, "figure3_swd_distribution_data.csv")
  save_figure(
    plot_swd_distribution(distribution, stats::median(analysis$swd)),
    "figure3_swd_distribution.png",
    width = 7, height = 4.5
  )
  save_figure(plot_fibser_by_group(analysis), "eappendix13_fibser_by_never_willing.png",
    width = 8, height = 7
  )
}

save_figure <- function(plot, file_name, width, height) {
  ggsave(file.path(PATH_OUTPUTS, file_name),
    plot = plot, width = width, height = height, dpi = FIGURE_DPI, bg = "white"
  )
}

plot_flow_diagram <- function() {
  flow <- read_csv(file.path(PATH_OUTPUTS, "flow_counts.csv"), show_col_types = FALSE)
  count_of <- function(stages) colSums(flow[flow$stage %in% stages, c("n_total", "n_jp", "n_de")])
  side_stages <- list(
    ineligible = c("ineligible", "left_consent", "left_after"),
    no_swd = c("no_response", "incomplete")
  )
  boxes <- FLOW_LAYOUT %>%
    mutate(counts = map(.data$stage, \(stage) {
      count_of(if (stage %in% names(side_stages)) side_stages[[stage]] else stage)
    })) %>%
    mutate(text = map2_chr(.data$label, .data$counts, \(label, counts) {
      sprintf("%s\nn = %d (Japan %d, Germany %d)", label, counts[1], counts[2], counts[3])
    }))
  main_boxes <- filter(boxes, .data$column == 1)
  ggplot(boxes, aes(x = .data$column, y = .data$row)) +
    geom_segment(
      data = tibble(y = head(main_boxes$row, -1), yend = tail(main_boxes$row, -1)),
      aes(x = 1, xend = 1, y = .data$y + 0.2, yend = .data$yend - 0.2),
      inherit.aes = FALSE, arrow = arrow(length = unit(2, "mm"))
    ) +
    geom_label(aes(label = .data$text), size = 3) +
    scale_y_reverse() +
    scale_x_continuous(limits = c(0.4, 2.6)) +
    theme_void()
}

swd_distribution_data <- function(analysis) {
  n_analysed <- nrow(analysis)
  n_always_willing <- sum(analysis$status_assigned == "always_willing")
  completed_swd <- analysis$swd[analysis$status_assigned == "completed"]
  cumulative <- tibble(
    x_position = c(SWD_RATE_GRID, NEVER_WILLING_X),
    cumulative_pct = c(
      100 * (n_always_willing + vapply(SWD_RATE_GRID, \(x) sum(completed_swd <= x), integer(1))) /
        n_analysed,
      100
    )
  )
  analysis %>%
    mutate(
      x_position = if_else(.data$status_assigned == "never_willing", NEVER_WILLING_X, .data$swd),
      country = COUNTRY_NAMES[.data$country]
    ) %>%
    count(.data$x_position, .data$country, name = "n") %>%
    mutate(pct = 100 * .data$n / n_analysed) %>%
    full_join(cumulative, by = "x_position") %>%
    arrange(.data$x_position, .data$country)
}

plot_swd_distribution <- function(distribution, median_swd) {
  x_breaks <- c(SWD_RATE_GRID, NEVER_WILLING_X)
  cumulative <- distinct(distribution, .data$x_position, .data$cumulative_pct)
  ggplot(filter(distribution, !is.na(.data$n)), aes(x = .data$x_position, y = .data$pct)) +
    geom_col(aes(fill = .data$country), width = 5, position = "stack") +
    geom_vline(xintercept = median_swd, linetype = "dashed") +
    geom_vline(xintercept = REAL_RISK_DIFFERENCE, linetype = "dotted") +
    scale_x_continuous(
      breaks = x_breaks, labels = c(SWD_RATE_GRID, "Never\nwilling"),
      name = "Smallest Worthwhile Difference, %",
      sec.axis = dup_axis(
        breaks = cumulative$x_position, labels = sprintf("%.0f", cumulative$cumulative_pct),
        name = "Cumulative percentile, %"
      )
    ) +
    labs(y = "Frequency, %", fill = NULL) +
    theme_minimal() +
    theme(legend.position = "top")
}

plot_fibser_by_group <- function(analysis) {
  groups <- analysis %>%
    filter(!is.na(.data$fibser_total)) %>%
    mutate(group = if_else(.data$status_assigned == "never_willing", "Never willing", "Others"))
  group_summary <- groups %>%
    group_by(.data$group) %>%
    summarise(median = stats::median(.data$fibser_total), mean = mean(.data$fibser_total))
  groups %>%
    count(.data$group, .data$fibser_total) %>%
    group_by(.data$group) %>%
    mutate(percent = 100 * .data$n / sum(.data$n)) %>%
    ggplot(aes(x = .data$fibser_total, y = .data$percent)) +
    geom_col(width = 1) +
    geom_vline(data = group_summary, aes(xintercept = .data$median), linetype = "dotted") +
    geom_vline(data = group_summary, aes(xintercept = .data$mean), linetype = "dashed") +
    facet_wrap(~group, ncol = 1, scales = "free_y") +
    scale_x_continuous(breaks = FIBSER_RANGE, name = "FIBSER total score (0-18)") +
    labs(y = "% within group") +
    theme_classic()
}

# 6. Executed statements -----
run_figures()
