# 1. Copyright statement -----
# Copyright (c) 2026 the SWD study authors. Released under the MIT License (see LICENSE).

# 2. Author -----
# Author: SWD study authors (see LICENSE)
# Date: 2026-11-14 (last revised)

# 3. File description -----
# Purpose: Derive each respondent's smallest worthwhile difference (SWD) on
#          the assigned drug track by walking the staircase tree along the
#          answers, self-check the walk on known answer patterns, and join
#          the SWD to the harmonized demographics (one row per respondent).
# Inputs:  none (works on the raw survey data from load_raw_data())
# Outputs: none (function definitions)
# Depends: 00_config.R (STAIRCASE_TREE, STATUS_LEVELS, SWD_RATE_GRID, SWD_CEILING,
#          SWD_NODES_BY_COUNTRY, DRUG_TRACKS, ADDITIONAL_DEMO_FIELDS, YES,
#          NO); tidyverse
# Style:   r_script_style_guide.md (Tier B)

# 4. source() and library() -----
# None: the calling script loads tidyverse and sources 00_config.R.

# Constants -----
FIRST_STAIRCASE_NODE <- "0"
RANDOMIZATION_COLUMN <- "RA01"
TRACK_RESULT_FIELDS <- c(
  threshold = "threshold_pct", swd = "swd", status = "status",
  n_visited = "n_visited", non_monotone = "is_non_monotone"
)

# 5. Function definitions -----
numeric_response <- function(x) {
  suppressWarnings(as.integer(as.character(x)))
}

# A "yes" at a higher relapse rate after a "no" at a lower one.
detect_non_monotone <- function(responses, rate_grid) {
  is_visited <- !is.na(responses)
  if (!any(is_visited)) {
    return(FALSE)
  }
  observed <- responses[is_visited][order(rate_grid[is_visited])]
  has_seen_no <- cumsum(observed == NO) > 0
  any(has_seen_no & observed == YES)
}

swd_result <- function(threshold_pct, swd, status, n_visited, is_non_monotone) {
  if (!status %in% STATUS_LEVELS) {
    stop(paste0("Unknown SWD status: ", status), call. = FALSE)
  }
  list(
    threshold_pct = threshold_pct, swd = swd, status = status,
    n_visited = n_visited, is_non_monotone = is_non_monotone
  )
}

# Every terminal of STAIRCASE_TREE, with the rates answered yes on its path.
list_staircase_terminals <- function(node = FIRST_STAIRCASE_NODE, yes_rates = numeric()) {
  tree_node <- STAIRCASE_TREE[[node]]
  rate <- as.numeric(node)
  branches <- list(yes = c(yes_rates, rate), no = yes_rates)
  imap(branches, function(branch_yes_rates, answer) {
    next_step <- tree_node[[answer]]
    if (!is.list(next_step)) {
      return(list_staircase_terminals(next_step, branch_yes_rates))
    }
    list(list(
      status = next_step$terminal, swd = next_step$swd,
      max_yes_rate = if (length(branch_yes_rates) == 0) NA_real_ else max(branch_yes_rates)
    ))
  }) %>%
    flatten()
}

# The SWD at each terminal is the untreated relapse rate (SWD_CEILING) minus
# the highest relapse rate the respondent still accepted; never willing (no
# rate accepted, not even 0%) is set to SWD_CEILING.
check_staircase_tree <- function() {
  terminals <- list_staircase_terminals()
  for (terminal in terminals) {
    expected_swd <- if (is.na(terminal$max_yes_rate)) {
      SWD_CEILING
    } else {
      SWD_CEILING - terminal$max_yes_rate
    }
    stopifnot(terminal$swd == expected_swd)
  }
  invisible(TRUE)
}

# The staircase starts at a 0% relapse rate; each answer (YES = would take
# the drug at that relapse rate, NO = would not) selects the next rate until
# a terminal node gives the SWD. An unanswered node ends the walk: no answer
# at the first node means no response, a later gap an incomplete staircase.
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
  if (n_visited == 0) {
    return(swd_result(NA_real_, NA_real_, "no_response", 0L, FALSE))
  }
  if (is_non_monotone) {
    warning("Non-monotone SWD responses detected; walking staircase path unchanged.",
      call. = FALSE
    )
  }
  responses_by_rate <- stats::setNames(responses, as.character(rate_grid))
  node <- FIRST_STAIRCASE_NODE
  yes_rates <- numeric()
  for (step_count in seq_len(length(STAIRCASE_TREE) + 1L)) {
    tree_node <- STAIRCASE_TREE[[node]]
    response <- responses_by_rate[[node]]
    if (is.na(response)) {
      is_first_node <- identical(node, FIRST_STAIRCASE_NODE)
      threshold <- if (is_first_node || length(yes_rates) == 0) NA_real_ else max(yes_rates)
      status <- if (is_first_node) "no_response" else "incomplete"
      return(swd_result(threshold, NA_real_, status, n_visited, is_non_monotone))
    }
    if (response == YES) {
      yes_rates <- c(yes_rates, as.numeric(node))
    }
    next_step <- if (response == YES) tree_node$yes else tree_node$no
    if (is.list(next_step)) {
      threshold <- if (length(yes_rates) == 0) NA_real_ else max(yes_rates)
      return(swd_result(
        threshold, next_step$swd, next_step$terminal, n_visited, is_non_monotone
      ))
    }
    node <- as.character(next_step)
  }
  stop("STAIRCASE_TREE traversal did not terminate.", call. = FALSE)
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
  no_at_zero <- compute_swd_for_answers(0, NO, rate_grid)
  stopifnot(no_at_zero$swd == 60, no_at_zero$status == "never_willing")
  yes_at_zero <- compute_swd_for_answers(0, YES, rate_grid)
  stopifnot(yes_at_zero$status == "incomplete", is.na(yes_at_zero$swd))
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
  all_missing <- compute_swd(rep(NA, length(rate_grid)), rate_grid)
  stopifnot(is.na(all_missing$swd), all_missing$status == "no_response")
  invisible(TRUE)
}

# One row per respondent with threshold_/swd_/status_/n_visited_/
# non_monotone_ columns per drug code; only the randomized (RA01) track is
# evaluated, the other two are "not_assigned".
compute_person_tracks <- function(row, country, nodes) {
  assigned_ra01 <- suppressWarnings(as.integer(row[[RANDOMIZATION_COLUMN]][[1]]))
  track_results <- list()
  for (track_index in seq_len(nrow(DRUG_TRACKS))) {
    drug <- DRUG_TRACKS$drug[[track_index]]
    responses <- unlist(row[nodes[[drug]]], use.names = FALSE)
    is_assigned_track <- !is.na(assigned_ra01) &&
      assigned_ra01 == DRUG_TRACKS$ra01[[track_index]]
    if (!is.na(assigned_ra01) && !is_assigned_track &&
      sum(!is.na(numeric_response(responses))) > 0) {
      warning("RA01/randomized track mismatch in ", country, " CASE ",
        row[["CASE"]][[1]], ": ", drug, " responses present.",
        call. = FALSE
      )
    }
    result <- if (is_assigned_track) {
      compute_swd(responses, nodes$relapse_pct)
    } else {
      swd_result(NA_real_, NA_real_, "not_assigned", 0L, FALSE)
    }
    drug_code <- DRUG_TRACKS$drug_code[[track_index]]
    for (prefix in names(TRACK_RESULT_FIELDS)) {
      track_results[[paste0(prefix, "_", drug_code)]] <-
        result[[TRACK_RESULT_FIELDS[[prefix]]]]
    }
  }
  track_results
}

apply_swd_country <- function(survey_data, country, nodes) {
  swd_columns <- unique(unlist(nodes[DRUG_TRACKS$drug], use.names = FALSE))
  for (column_name in setdiff(swd_columns, names(survey_data))) {
    survey_data[[column_name]] <- NA_integer_
  }
  if (!RANDOMIZATION_COLUMN %in% names(survey_data)) {
    survey_data[[RANDOMIZATION_COLUMN]] <- NA_integer_
  }
  person_rows <- lapply(seq_len(nrow(survey_data)), \(row_index) {
    row <- survey_data[row_index, , drop = FALSE]
    as_tibble(c(
      list(country = country, respondent_id = as.character(row[["CASE"]][[1]])),
      compute_person_tracks(row, country, nodes)
    ))
  })
  bind_rows(person_rows)
}

apply_swd_per_person <- function(jp_data, de_data) {
  bind_rows(
    apply_swd_country(jp_data, "JP", SWD_NODES_BY_COUNTRY$jp),
    apply_swd_country(de_data, "DE", SWD_NODES_BY_COUNTRY$de)
  )
}

make_individual_table <- function(demographics, swd) {
  track_columns <- as.vector(t(outer(
    names(TRACK_RESULT_FIELDS)[c(2, 3, 1, 4, 5)], DRUG_TRACKS$drug_code,
    paste,
    sep = "_"
  )))
  demographics %>%
    left_join(swd, by = c("country", "respondent_id")) %>%
    select(all_of(c(
      "respondent_id", "country", "age_band", "sex", "educ", "employ",
      "age_band_score", "educ_3level", "educ_score", "employ_3level",
      "n_admissions_group", "n_admissions_score", "onset_age_group",
      "onset_age_score", "fibser_total", ADDITIONAL_DEMO_FIELDS,
      "randomized_drug", track_columns
    )))
}

# 6. Executed statements -----
# None (sourced by 01_prepare_data.R).
