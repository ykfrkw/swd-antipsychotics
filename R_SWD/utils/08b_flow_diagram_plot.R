#1. Copyright statement -----
# Copyright (c) 2026 the SWD study authors. Released under the MIT License (see LICENSE).

#2. Author -----
# Author: SWD study authors (see LICENSE)
# Date: 2026-11-13 (last revised)

#3. File description -----
# Purpose: Draw the participant flow diagram (ggplot2 only) from the stage
#          counts computed in 08a_flow_diagram_counts.R.
# Inputs:  none (functions receive the stage counts)
# Outputs: none (returns the ggplot object)
# Depends: 00a_config.R; tidyverse, grid
# Style:   r_script_style_guide.md (Tier B)

#4. source() and library() -----
# None: the calling script loads tidyverse and sources 00a_config.R.

# Constants -----
# Geometry tuned for 12 pt text on an A4-fitting canvas (8.2 x 10.7 in);
# 1 x-unit = 1 y-unit = 0.082 in.
MAIN_X <- 21.5 # main column: centered boxes
MAIN_W <- 40
SIDE_X <- 72 # side column: exclusion boxes
SIDE_W <- 54
BOX_LINE_WIDTH <- 0.4
TEXT_LINE_HEIGHT <- 1.15
TEXT_SIZE <- 12 / .pt # render text at 12 pt
Y_LIMITS <- c(136, 0) # y grows downward (top of page = 0)
X_LIMITS <- c(0, 100)

#5. Function definitions -----
format_n_by_country <- function(counts) {
  sprintf(
    "n = %d (Japan %d / Germany %d)", sum(counts), counts[["JP"]], counts[["DE"]]
  )
}

# y increases downward (the y scale is reversed); y_top / h in plot units.
build_main_boxes <- function(flow) {
  tribble(
    ~key, ~y_top, ~h, ~label,
    "clicked", 1, 7,
    paste0("Clicked the survey link\n", format_n_by_country(flow$clicked)),
    "entered", 18, 7,
    paste0("Entered the survey\n", format_n_by_country(flow$entered)),
    "assessed", 35, 7,
    paste0("Assessed for eligibility\n", format_n_by_country(flow$assessed)),
    "randomized", 87, 7,
    paste0(
      "Randomized to one antipsychotic arm\n",
      format_n_by_country(flow$randomized)
    ),
    "determinable", 104, 7,
    paste0("SWD determinable\n", format_n_by_country(flow$determinable)),
    "analyzed", 128, 7,
    paste0("Included in analysis\n", format_n_by_country(flow$analyzed))
  )
}

format_ineligible_label <- function(flow) {
  sprintf(
    paste0(
      "Ineligible\n%s\n",
      "Medication changed within 6 months  %d\n",
      "Psychiatric admission within 6 months  %d\n",
      "(%d identified post hoc from the demographics)\n",
      "No schizophrenia / schizoaffective diagnosis  %d\n",
      "Under 18 years  %d\n",
      "(criteria not mutually exclusive)"
    ),
    format_n_by_country(flow$ineligible), sum(flow$crit_meds),
    sum(flow$crit_admission), sum(flow$recent_admission_n),
    sum(flow$crit_no_dx), sum(flow$crit_under_18)
  )
}

# The no-SWD split shows that nearly all missing SWDs arose before the
# trade-off task, not from fatigue during it.
format_no_swd_label <- function(flow) {
  sprintf(
    paste0(
      "Incomplete data for calculating SWD\n%s\n",
      "Left before the trade-off questions  %d\n",
      "Stopped during the trade-off questions  %d"
    ),
    format_n_by_country(flow$no_response + flow$incomplete),
    sum(flow$no_response), sum(flow$incomplete)
  )
}

format_excluded_label <- function(flow) {
  sprintf(
    paste0(
      "Excluded\n%s\n",
      "answered \"never willing\" but free text indicated\n",
      "satisfaction with their current antipsychotic"
    ),
    format_n_by_country(flow$excluded)
  )
}

build_side_boxes <- function(flow) {
  tribble(
    ~key, ~y_top, ~h, ~label,
    "left_info", 9.5, 7,
    paste0(
      "Left at the study information page\n",
      format_n_by_country(flow$left_info)
    ),
    "test", 26.5, 7,
    paste0("Test entry removed\n", format_n_by_country(flow$test_removed)),
    "ineligible", 43.5, 25, format_ineligible_label(flow),
    "left_consent", 70, 7,
    paste0(
      "Left before completing consent / screening\n",
      format_n_by_country(flow$left_consent)
    ),
    "left_after", 78.5, 7,
    paste0(
      "Left after screening, before randomization\n",
      format_n_by_country(flow$left_after)
    ),
    "no_swd_data", 92, 14, format_no_swd_label(flow),
    "excluded", 112.5, 14, format_excluded_label(flow)
  )
}

add_box_geometry <- function(boxes, center_x, width) {
  boxes %>%
    mutate(
      xmin = center_x - width / 2, xmax = center_x + width / 2,
      ymin = .data$y_top, ymax = .data$y_top + .data$h,
      yc = .data$y_top + .data$h / 2
    )
}

build_flow_plot <- function(flow) {
  main_boxes <- add_box_geometry(build_main_boxes(flow), MAIN_X, MAIN_W)
  side_boxes <- add_box_geometry(build_side_boxes(flow), SIDE_X, SIDE_W)
  # vertical arrows between consecutive main boxes
  v_arrows <- tibble(
    x = MAIN_X, xend = MAIN_X,
    y = main_boxes$ymax[-nrow(main_boxes)],
    yend = main_boxes$ymin[-1]
  )
  # horizontal branch arrows: off the vertical connector at each side box's center
  h_arrows <- tibble(
    x = MAIN_X, xend = side_boxes$xmin,
    y = side_boxes$yc, yend = side_boxes$yc
  )
  arrow_spec <- grid::arrow(length = grid::unit(2.8, "mm"), type = "closed")

  ggplot() +
    geom_segment(
      data = v_arrows,
      aes(x = x, xend = xend, y = y, yend = yend),
      linewidth = BOX_LINE_WIDTH, arrow = arrow_spec
    ) +
    geom_segment(
      data = h_arrows,
      aes(x = x, xend = xend, y = y, yend = yend),
      linewidth = BOX_LINE_WIDTH, arrow = arrow_spec
    ) +
    geom_rect(
      data = bind_rows(main_boxes, side_boxes),
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = "white", color = "black", linewidth = BOX_LINE_WIDTH
    ) +
    geom_text(
      data = main_boxes,
      aes(x = MAIN_X, y = yc, label = label),
      size = TEXT_SIZE, lineheight = TEXT_LINE_HEIGHT
    ) +
    geom_text(
      data = side_boxes,
      aes(x = SIDE_X, y = yc, label = label),
      size = TEXT_SIZE, lineheight = TEXT_LINE_HEIGHT
    ) +
    scale_y_reverse(limits = Y_LIMITS) +
    coord_cartesian(xlim = X_LIMITS, expand = FALSE) +
    theme_void()
}

#6. Executed statements -----
# None (sourced by 08_flow_diagram.R).
