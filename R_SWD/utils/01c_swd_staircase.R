#1. Copyright statement -----
# Copyright (c) 2026 the SWD study authors. Released under the MIT License (see LICENSE).

#2. Author -----
# Author: SWD study authors (see LICENSE)
# Date: 2026-11-13 (last revised)

#3. File description -----
# Purpose: Derive the smallest worthwhile difference (SWD) of one drug track by
#          walking the staircase tree (STAIRCASE_TREE) along the answers, and
#          self-check the walk on known answer patterns.
#          The per-respondent application lives in 01g_swd_person_tracks.R,
#          which this file sources so callers keep one entry point.
# Inputs:  none
# Outputs: none (function definitions)
# Depends: 00a_config.R (STAIRCASE_TREE, STATUS_LEVELS, SWD_RATE_GRID, YES,
#          NO), 01g_swd_person_tracks.R; tidyverse
# Style:   r_script_style_guide.md (Tier B)

#4. source() and library() -----
# The calling script loads tidyverse and sources 00a_config.R.
source(file.path(PATH_UTILS, "01g_swd_person_tracks.R"))

# Constants -----
FIRST_STAIRCASE_NODE <- "0"
# Terminal statuses whose terminal node is named by the status itself.
BOUNDARY_STATUSES <- c("always_willing", "never_willing")

#5. Function definitions -----
numeric_response <- function(x) {
  suppressWarnings(as.integer(as.character(x)))
}

detect_non_monotone <- function(responses, rate_grid) {
  is_visited <- !is.na(responses)
  if (!any(is_visited)) {
    return(FALSE)
  }

  observed_order <- order(rate_grid[is_visited])
  observed <- responses[is_visited][observed_order]
  has_seen_no <- cumsum(observed == NO) > 0
  any(has_seen_no & observed == YES)
}

is_terminal_step <- function(step) {
  is.list(step) && !is.null(step$terminal)
}

path_threshold <- function(yes_rates) {
  if (length(yes_rates) == 0) {
    return(NA_real_)
  }

  max(yes_rates)
}

label_terminal_node <- function(terminal_status, current_rate) {
  if (terminal_status %in% BOUNDARY_STATUSES) {
    return(terminal_status)
  }

  current_rate
}

make_swd_result <- function(
  threshold_pct,
  swd,
  status,
  n_visited,
  is_non_monotone,
  path,
  terminal_node = NA_real_
) {
  if (!status %in% STATUS_LEVELS) {
    stop(paste0("Unknown SWD status: ", status), call. = FALSE)
  }

  list(
    threshold_pct = threshold_pct,
    swd = swd,
    status = status,
    n_visited = n_visited,
    is_non_monotone = is_non_monotone,
    path = path,
    terminal_node = terminal_node
  )
}

compute_swd <- function(responses_vec, rate_grid) {
  responses <- numeric_response(responses_vec)
  rate_grid <- as.numeric(rate_grid)

  if (length(responses) != length(rate_grid)) {
    stop("responses_vec and rate_grid must have the same length.", call. = FALSE)
  }

  rate_order <- order(rate_grid)
  responses <- responses[rate_order]
  rate_grid <- rate_grid[rate_order]
  responses[!responses %in% c(YES, NO)] <- NA_integer_

  n_visited <- sum(!is.na(responses))
  is_non_monotone <- detect_non_monotone(responses, rate_grid)

  finish <- function(threshold_pct, swd, status, path, terminal_node = NA_real_) {
    if (is_non_monotone) {
      warning(
        "Non-monotone SWD responses detected; walking staircase path unchanged.",
        call. = FALSE
      )
    }

    make_swd_result(
      threshold_pct = threshold_pct,
      swd = swd,
      status = status,
      n_visited = n_visited,
      is_non_monotone = is_non_monotone,
      path = path,
      terminal_node = terminal_node
    )
  }

  if (n_visited == 0) {
    return(make_swd_result(
      threshold_pct = NA_real_,
      swd = NA_real_,
      status = "no_response",
      n_visited = 0L,
      is_non_monotone = FALSE,
      path = numeric(),
      terminal_node = NA_real_
    ))
  }

  responses_by_rate <- stats::setNames(responses, as.character(rate_grid))
  node <- FIRST_STAIRCASE_NODE
  path <- numeric()
  yes_rates <- numeric()
  step_count <- 0L

  repeat {
    step_count <- step_count + 1L
    if (step_count > length(STAIRCASE_TREE) + 1L) {
      stop("STAIRCASE_TREE traversal did not terminate.", call. = FALSE)
    }

    tree_node <- STAIRCASE_TREE[[node]]
    if (is.null(tree_node)) {
      stop(
        paste0("STAIRCASE_TREE has no node for relapse_pct=", node, "."),
        call. = FALSE
      )
    }

    # An unanswered node ends the walk: no answer at the first node means no
    # response at all, a later gap means the staircase was left incomplete.
    response <- responses_by_rate[[node]]
    if (is.null(response) || is.na(response)) {
      is_first_node <- length(path) == 0 && identical(node, FIRST_STAIRCASE_NODE)
      return(finish(
        threshold_pct = if (is_first_node) NA_real_ else path_threshold(yes_rates),
        swd = NA_real_,
        status = if (is_first_node) "no_response" else "incomplete",
        path = path
      ))
    }

    current_rate <- as.numeric(node)
    path <- c(path, current_rate)

    if (response == YES) {
      yes_rates <- c(yes_rates, current_rate)
      next_step <- tree_node$yes
    } else {
      next_step <- tree_node$no
    }

    if (is_terminal_step(next_step)) {
      return(finish(
        threshold_pct = path_threshold(yes_rates),
        swd = next_step$swd,
        status = next_step$terminal,
        path = path,
        terminal_node = label_terminal_node(next_step$terminal, current_rate)
      ))
    }

    node <- as.character(next_step)
  }
}

# SWD of a track answered only at the given rates (all other rates missing).
compute_swd_for_answers <- function(answered_rates, answers, rate_grid = SWD_RATE_GRID) {
  responses <- rep(NA_integer_, length(rate_grid))
  responses[match(answered_rates, rate_grid)] <- answers
  compute_swd(responses, rate_grid)
}

# Known answer patterns and the SWD / status the staircase must give.
check_compute_swd <- function(rate_grid = SWD_RATE_GRID) {
  all_yes <- compute_swd(rep(YES, length(rate_grid)), rate_grid)
  stopifnot(all_yes$swd == 0, all_yes$status == "always_willing")

  no_at_zero_only <- compute_swd_for_answers(0, NO, rate_grid)
  stopifnot(no_at_zero_only$swd == 60, no_at_zero_only$status == "never_willing")

  yes_at_zero_only <- compute_swd_for_answers(0, YES, rate_grid)
  stopifnot(yes_at_zero_only$status == "incomplete", is.na(yes_at_zero_only$swd))

  yes_up_to_55 <- compute_swd_for_answers(
    c(0, 30, 45, 55, 60), c(YES, YES, YES, YES, NO), rate_grid
  )
  stopifnot(yes_up_to_55$status == "completed", yes_up_to_55$swd == 5)

  no_from_5 <- compute_swd_for_answers(c(0, 30, 15, 5), c(YES, NO, NO, NO), rate_grid)
  stopifnot(no_from_5$status == "completed", no_from_5$swd == 60)

  no_at_35 <- compute_swd_for_answers(c(0, 30, 45, 35), c(YES, YES, NO, NO), rate_grid)
  stopifnot(no_at_35$status == "completed", no_at_35$swd == 30)

  open_after_35 <- compute_swd_for_answers(c(0, 30, 45, 35), c(YES, YES, NO, YES), rate_grid)
  stopifnot(open_after_35$status == "incomplete", is.na(open_after_35$swd))

  open_after_5 <- compute_swd_for_answers(c(0, 30, 15, 5), c(YES, NO, NO, YES), rate_grid)
  stopifnot(open_after_5$status == "incomplete")

  all_missing <- compute_swd(rep(NA, length(rate_grid)), rate_grid)
  stopifnot(
    is.na(all_missing$swd),
    all_missing$status %in% c("no_response", "not_assigned")
  )

  invisible(TRUE)
}

#6. Executed statements -----
# None (sourced by 01_analysis_data.R). The former stand-alone self-check
# block is now check_compute_swd(), called by 01_analysis_data.R.
