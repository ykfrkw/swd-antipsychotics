# 1. Copyright statement -----
# Copyright (c) 2026 the SWD study authors. Released under the MIT License (see LICENSE).

# 2. Author -----
# Author: SWD study authors (see LICENSE)
# Date: 2026-11-14 (last revised)

# 3. File description -----
# Purpose: Post hoc comparison of the FIBSER total (antipsychotic side-effect
#          burden) between never-willing and all other analysed respondents.
# Inputs:  data/processed/swd_individual.csv
# Outputs: outputs/fibser_never_willing_test.csv
# Depends: 00_config.R, 00c_analysis_set.R; tidyverse
# Style:   r_script_style_guide.md (Tier B)
#
# Run from the project root:  Rscript R/06_fibser_never_willing.R
#
# Analysis
# Population: the analysis set (n = 225) with a FIBSER total, split into
# never willing respondents and the others (completed + always willing).
# Two-sided Mann-Whitney (Wilcoxon rank-sum) test with the normal
# approximation (exact = FALSE). Effect sizes: rank-biserial correlation
# 2W / (n1 n2) - 1 and r = |z| / sqrt(N), z recovered from the p-value.

# 4. source() and library() -----
suppressPackageStartupMessages(library(tidyverse))
source(file.path("R", "00_config.R"))
source(file.path(PATH_R, "00c_analysis_set.R"))

# 5. Function definitions -----
# Analysis set with a FIBSER total and the never-willing indicator.
build_fibser_groups <- function(individual) {
  build_analysis_set(individual) %>%
    transmute(
      fibser_total = as.numeric(.data$fibser_total),
      is_never_willing = .data$status_assigned == "never_willing"
    ) %>%
    filter(!is.na(.data$fibser_total))
}

test_fibser_by_never_willing <- function(fibser_groups) {
  never_scores <- fibser_groups$fibser_total[fibser_groups$is_never_willing]
  other_scores <- fibser_groups$fibser_total[!fibser_groups$is_never_willing]
  mann_whitney <- stats::wilcox.test(never_scores, other_scores, exact = FALSE)
  w_statistic <- unname(mann_whitney$statistic)
  z_value <- abs(stats::qnorm(mann_whitney$p.value / 2))
  tibble(
    n_never_willing = length(never_scores), n_others = length(other_scores),
    median_never_willing = stats::median(never_scores),
    median_others = stats::median(other_scores),
    mean_never_willing = mean(never_scores), mean_others = mean(other_scores),
    w_statistic = w_statistic, p_value = mann_whitney$p.value, z_value = z_value,
    rank_biserial_r = 2 * w_statistic / (length(never_scores) * length(other_scores)) - 1,
    z_based_r = z_value / sqrt(length(never_scores) + length(other_scores))
  )
}

run_fibser_never_willing <- function() {
  fibser_groups <- build_fibser_groups(read_individual())
  write_output_csv(test_fibser_by_never_willing(fibser_groups), "fibser_never_willing_test.csv")
}

# 6. Executed statements -----
run_fibser_never_willing()
