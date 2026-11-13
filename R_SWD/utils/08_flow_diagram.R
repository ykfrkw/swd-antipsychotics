#1. Copyright statement -----
# Copyright (c) 2026 the SWD study authors. Released under the MIT License (see LICENSE).

#2. Author -----
# Author: SWD study authors (see LICENSE)
# Date: 2026-11-13 (last revised)

#3. File description -----
# Purpose: Read-only figure script: participant flow diagram (manuscript
#          Figure 2) from survey click to analysis set, per country (JP / DE).
#          Upstream numbers (link clicks, valid survey entries) are hardcoded
#          from the SoSci Survey admin overview; everything downstream is
#          recomputed from swd_individual.csv (randomization, staircase
#          status, post-hoc exclusions) and the raw SoSci exports (screening
#          items WE02/WE03/WE05/WE06/WE07 for the non-randomized respondents).
#          Internal-consistency identities are enforced with stopifnot(); the
#          expected manuscript numbers are only PRINTED alongside the computed
#          ones, so drift is visible without breaking the script.
# Inputs:  R_SWD/data/processed/swd_individual.csv, raw SoSci data exports in
#          R_SWD/data/raw (via 01a_raw_data_loading.R)
# Outputs: R_SWD/outputs/report/flow_diagram.png (dpi = 300),
#          R_SWD/outputs/report/flow_diagram.pdf
# Depends: 00a_config.R, 01a_raw_data_loading.R, 08a_flow_diagram_counts.R,
#          08b_flow_diagram_plot.R; tidyverse, grid
# Style:   r_script_style_guide.md (Tier B)
#
# Run from the project root:  Rscript R_SWD/utils/08_flow_diagram.R

#4. source() and library() -----
suppressWarnings(suppressMessages(library(tidyverse)))
source(file.path("R_SWD", "utils", "00a_config.R"))
source(file.path(PATH_UTILS, "01a_raw_data_loading.R"))
source(file.path(PATH_UTILS, "08a_flow_diagram_counts.R"))
source(file.path(PATH_UTILS, "08b_flow_diagram_plot.R"))

# Constants -----
# Source: SoSci Survey admin overview, 2026-07-06 (clicks on the survey link
# and valid cases that entered the survey; not present in the data exports).
CLICKS <- c(JP = 1029L, DE = 248L)
ENTERED <- c(JP = 227L, DE = 128L)

FIGURE_WIDTH_IN <- 8.2
FIGURE_HEIGHT_IN <- 11.15
FIGURE_DPI <- 300

# Expected = manuscript Figure 2 (JP, DE); printed for comparison only.
EXPECTED <- list(
  clicked = c(1029L, 248L), left_info = c(802L, 120L), entered = c(227L, 128L),
  test_removed = c(1L, 0L), assessed = c(226L, 128L), ineligible = c(39L, 17L),
  crit_meds = c(25L, 14L), crit_admission = c(19L, 4L), crit_no_dx = c(2L, 0L),
  crit_under_18 = c(1L, 1L), left_consent = c(7L, 2L), left_after = c(1L, 0L),
  randomized = c(179L, 109L), no_response = c(39L, 9L), incomplete = c(3L, 0L),
  determinable = c(137L, 100L), completed = c(92L, 85L), never = c(34L, 10L),
  always = c(11L, 5L), excluded = c(11L, 1L), analyzed = c(126L, 99L)
)

STAGE_LABELS <- c(
  clicked = "Clicked the survey link",
  left_info = "  Left at the study information page",
  entered = "Entered the survey",
  test_removed = "  Test entry removed",
  assessed = "Assessed for eligibility",
  ineligible = "  Ineligible",
  crit_meds = "    - medication changed within 6 months",
  crit_admission = "    - psychiatric admission within 6 months",
  crit_no_dx = "    - no schizophrenia/schizoaffective dx",
  crit_under_18 = "    - under 18 years",
  left_consent = "  Left before consent / screening",
  left_after = "  Left after screening, pre-randomization",
  randomized = "Randomized to one antipsychotic arm",
  no_response = "  Staircase questions not answered",
  incomplete = "  Staircase incomplete",
  determinable = "SWD determinable",
  completed = "    - completed staircase",
  never = "    - never willing",
  always = "    - always willing",
  excluded = "  Excluded post hoc",
  analyzed = "Included in analysis"
)

#5. Function definitions -----
run_flow_diagram <- function() {
  individual <- read_csv(
    file.path(PATH_OUT, "swd_individual.csv"),
    show_col_types = FALSE
  )
  person <- assigned_status(individual)
  non_randomized <- load_non_randomized(person)
  flow <- compute_flow_counts(person, non_randomized, CLICKS, ENTERED)
  check_freetext_exclusions(person)
  check_flow_identities(flow, person)

  print_flow_summary(flow)
  save_flow_diagram(build_flow_plot(flow))
}

print_flow_summary <- function(flow) {
  cat("\n=== participant flow (computed vs expected manuscript numbers) ===\n")
  cat(sprintf(
    "%-46s %19s   %19s\n", "stage", "computed T (JP/DE)", "expected T (JP/DE)"
  ))
  is_stage_matching <- vapply(names(STAGE_LABELS), \(stage) {
    all(flow[[stage]] == EXPECTED[[stage]])
  }, logical(1))
  for (stage in names(STAGE_LABELS)) {
    computed <- flow[[stage]]
    expected <- EXPECTED[[stage]]
    cat(sprintf(
      "%-46s %6d (%4d/%4d)   %6d (%4d/%4d)%s\n",
      STAGE_LABELS[[stage]], sum(computed), computed[[1]], computed[[2]],
      sum(expected), expected[[1]], expected[[2]],
      if (is_stage_matching[[stage]]) "" else "   << MISMATCH"
    ))
  }
  n_mismatch <- sum(!is_stage_matching)
  if (n_mismatch > 0) {
    cat(sprintf(
      "\nWARNING: %d stage(s) differ from the manuscript numbers (data drift?).\n",
      n_mismatch
    ))
    return(invisible(NULL))
  }
  cat("\nAll stages match the expected manuscript numbers.\n")
}

# Same two-ggsave convention as save_report_figure() in 02c_swd_figures.R.
save_flow_diagram <- function(flow_plot) {
  dir.create(REPORT_OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
  png_path <- file.path(REPORT_OUTPUT_DIR, "flow_diagram.png")
  pdf_path <- file.path(REPORT_OUTPUT_DIR, "flow_diagram.pdf")
  ggsave(png_path,
    plot = flow_plot, width = FIGURE_WIDTH_IN, height = FIGURE_HEIGHT_IN,
    dpi = FIGURE_DPI, bg = "white"
  )
  ggsave(pdf_path,
    plot = flow_plot, width = FIGURE_WIDTH_IN, height = FIGURE_HEIGHT_IN,
    bg = "white"
  )
  cat("\nWrote:\n  ", png_path, "\n  ", pdf_path, "\n")
}

#6. Executed statements -----
run_flow_diagram()
