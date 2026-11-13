#1. Copyright statement -----
# Copyright (c) 2026 the SWD study authors. Released under the MIT License (see LICENSE).

#2. Author -----
# Author: SWD study authors (see LICENSE)
# Date: 2026-11-13 (last revised)

#3. File description -----
# Purpose: Figure 1 (SWD distribution by country with cumulative percentiles)
#          and Figure 2 (SWD by randomized drug arm) for the analysis set.
# Inputs:  none (works on the individual table)
# Outputs: R_SWD/outputs/report/figure1_swd_distribution.{png,pdf},
#          R_SWD/outputs/report/figure2_swd_by_drug.{png,pdf}
# Depends: 00a_config.R (REPORT_OUTPUT_DIR, DRUG_TRACKS, SWD_RATE_GRID,
#          USABLE_SWD_STATUSES), 01d_swd_summary.R (assigned_usable_swd_long),
#          01f_test_respondent_exclusion.R (figure_exclusion_table); tidyverse
# Style:   r_script_style_guide.md (Tier B)

#4. source() and library() -----
# None: the calling script loads tidyverse and sources 00a_config.R,
# 01d_swd_summary.R and 01f_test_respondent_exclusion.R.

# Constants -----
# Analysis set for the manuscript: N = 225 (Japan 126 / Germany 99);
# completed (177) + always willing (16) = 193 of them have a finite SWD.
EXPECTED_N_FIGURE1 <- 225L
EXPECTED_N_FIGURE2 <- 193L
MAX_SWD <- 60
# Never willing is drawn as its own column to the right of SWD = 60.
NEVER_WILLING_X <- 67.5
REAL_RISK_DIFFERENCE <- 40
FIGURE_WIDTH_IN <- 7
FIGURE_HEIGHT_IN <- 4.5
FIGURE_DPI <- 300
BASE_FONT_SIZE <- 11
ANNOTATION_SIZE <- 3
REFERENCE_LINE_WIDTH <- 0.6
ANNOTATION_VJUST <- 1.4
ANNOTATION_HJUST <- -0.05
POINT_COLOR <- "#263238"
COUNTRY_COLORS <- c(Japan = "#c17f32", Germany = "#2f6f73")
DRUG_COLORS <- c(
  risperidone = "#4b8f8c",
  aripiprazole = "#c17f32",
  olanzapine = "#6f6aa8"
)

#5. Function definitions -----
report_figure_data <- function(individual, statuses) {
  assigned_usable_swd_long(individual, statuses) %>%
    mutate(
      respondent_id = as.character(.data$respondent_id),
      country = as.character(.data$country),
      swd = as.numeric(.data$swd),
      drug = factor(.data$drug, levels = DRUG_TRACKS$drug)
    ) %>%
    anti_join(figure_exclusion_table(), by = c("country", "respondent_id"))
}

save_report_figure <- function(plot, basename, width = FIGURE_WIDTH_IN,
                               height = FIGURE_HEIGHT_IN) {
  dir.create(REPORT_OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

  png_path <- file.path(REPORT_OUTPUT_DIR, paste0(basename, ".png"))
  pdf_path <- file.path(REPORT_OUTPUT_DIR, paste0(basename, ".pdf"))

  ggsave(png_path, plot = plot, width = width, height = height, dpi = FIGURE_DPI)
  ggsave(pdf_path, plot = plot, width = width, height = height)

  invisible(c(png = png_path, pdf = pdf_path))
}

stop_without_ggplot2 <- function() {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required for report figures.", call. = FALSE)
  }
}

# Cumulative percentile in display order:
# always willing + completed by ascending SWD -> never willing.
cumulative_percentile_labels <- function(figure_data) {
  n_always_willing <- sum(figure_data$status == "always_willing")
  completed_swd <- figure_data$swd[figure_data$status == "completed"]
  percentiles <- vapply(
    SWD_RATE_GRID,
    \(x) 100 * (n_always_willing + sum(completed_swd <= x)) / nrow(figure_data),
    numeric(1)
  )
  sprintf("%.0f", c(percentiles, 100))
}

add_reference_line <- function(plot, x, label, linetype, fontface = "plain") {
  plot +
    geom_vline(xintercept = x, linewidth = REFERENCE_LINE_WIDTH, linetype = linetype) +
    annotate(
      "text",
      x = x,
      y = Inf,
      label = label,
      fontface = fontface,
      vjust = ANNOTATION_VJUST,
      hjust = ANNOTATION_HJUST,
      size = ANNOTATION_SIZE
    )
}

plot_swd_distribution <- function(bar_data, cumulative_labels) {
  x_breaks <- c(SWD_RATE_GRID, NEVER_WILLING_X)
  x_labels <- c(as.character(SWD_RATE_GRID), "Never\nwilling")

  ggplot(
    bar_data,
    aes(x = .data$x_center, y = .data$pct, fill = .data$country_label)
  ) +
    geom_col(width = 5, color = "white", position = "stack") +
    scale_fill_manual(values = COUNTRY_COLORS, name = NULL) +
    scale_x_continuous(
      limits = c(-2.5, 70),
      breaks = x_breaks,
      labels = x_labels,
      name = "Smallest Worthwhile Difference, %",
      sec.axis = dup_axis(
        breaks = x_breaks,
        labels = cumulative_labels,
        name = "Cumulative percentile, %"
      )
    ) +
    scale_y_continuous(name = "Frequency, %", expand = expansion(mult = c(0, 0.08))) +
    theme_minimal(base_size = BASE_FONT_SIZE) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title.position = "plot",
      legend.position = "top",
      legend.justification = "left",
      legend.margin = margin(0, 0, 0, 0)
    )
}

fig_swd_distribution <- function(individual) {
  stop_without_ggplot2()

  figure_data <- report_figure_data(individual, statuses = USABLE_SWD_STATUSES)
  stopifnot(nrow(figure_data) == EXPECTED_N_FIGURE1)

  figure_data <- figure_data %>%
    mutate(
      country_label = factor(
        .data$country,
        levels = c("JP", "DE"),
        labels = c("Japan", "Germany")
      )
    )

  # Always willing (SWD = 0 by design) is shown inside the SWD = 0 bin;
  # never willing (SWD = 60 by design) is a separate column on the right.
  stopifnot(
    all(figure_data$swd[figure_data$status == "always_willing"] == 0),
    all(figure_data$swd[figure_data$status == "never_willing"] == MAX_SWD)
  )

  bar_data <- figure_data %>%
    mutate(
      x_center = case_when(
        .data$status == "never_willing" ~ NEVER_WILLING_X,
        TRUE ~ .data$swd
      )
    ) %>%
    count(.data$x_center, .data$country_label, name = "n") %>%
    mutate(pct = 100 * .data$n / nrow(figure_data))

  plot <- plot_swd_distribution(bar_data, cumulative_percentile_labels(figure_data))

  median_swd <- stats::median(figure_data$swd, na.rm = TRUE)
  if (!is.na(median_swd)) {
    plot <- add_reference_line(
      plot, median_swd, sprintf("Median %g", median_swd),
      linetype = "dashed"
    )
  }

  # Reference line: real risk difference of maintenance treatment.
  plot <- add_reference_line(
    plot, REAL_RISK_DIFFERENCE,
    sprintf("Real Risk Difference %g", REAL_RISK_DIFFERENCE),
    linetype = "dotted", fontface = "italic"
  )

  save_report_figure(plot, "figure1_swd_distribution")
  plot
}

fig_swd_violin <- function(individual) {
  stop_without_ggplot2()

  figure_data <- report_figure_data(
    individual,
    statuses = USABLE_SWD_STATUSES_EXCLUDING_NEVER
  )
  stopifnot(nrow(figure_data) == EXPECTED_N_FIGURE2)

  # geom_jitter() draws without a fixed seed, so the PNG/PDF point positions
  # differ between runs (kept as in earlier runs).
  plot <- ggplot(figure_data, aes(x = .data$drug, y = .data$swd)) +
    geom_violin(
      aes(fill = .data$drug),
      trim = FALSE, alpha = 0.35, color = NA, na.rm = TRUE
    ) +
    geom_boxplot(
      width = 0.16, outlier.shape = NA, fill = "white", color = POINT_COLOR,
      na.rm = TRUE
    ) +
    geom_jitter(
      width = 0.08, height = 0, alpha = 0.7, size = 1.8, color = POINT_COLOR,
      na.rm = TRUE
    ) +
    scale_y_continuous(
      limits = c(0, MAX_SWD),
      breaks = seq(0, MAX_SWD, by = 10),
      name = "SWD, percentage-point relapse-rate reduction"
    ) +
    scale_x_discrete(name = NULL, drop = FALSE) +
    scale_fill_manual(values = DRUG_COLORS, guide = "none", drop = FALSE) +
    labs(title = "SWD by randomized drug arm") +
    theme_minimal(base_size = BASE_FONT_SIZE) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title.position = "plot"
    )

  save_report_figure(plot, "figure2_swd_by_drug")
  plot
}

#6. Executed statements -----
# None (sourced by 02_report_outputs.R).
