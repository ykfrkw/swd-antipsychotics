#1. Copyright statement -----
# Copyright (c) 2026 the SWD study authors. Released under the MIT License (see LICENSE).

#2. Author -----
# Author: SWD study authors (see LICENSE)
# Date: 2026-11-13 (last revised)

#3. File description -----
# Purpose: Apply compute_swd() to every respondent and drug track of the JP
#          and DE surveys (one row per respondent, columns per drug code).
#          Split off from 01c_swd_staircase.R, which sources this file.
# Inputs:  none (works on the raw survey data from load_raw_data())
# Outputs: none (function definitions)
# Depends: 00a_config.R (DRUG_TRACKS, SWD_NODES_BY_COUNTRY),
#          01c_swd_staircase.R (compute_swd, numeric_response); tidyverse
# Style:   r_script_style_guide.md (Tier B)

#4. source() and library() -----
# None: sourced by 01c_swd_staircase.R; the calling script loads tidyverse
# and sources 00a_config.R.

# Constants -----
RANDOMIZATION_COLUMN <- "RA01"

#5. Function definitions -----
swd_nodes_for_country <- function(country) {
  country_key <- tolower(country)
  nodes <- SWD_NODES_BY_COUNTRY[[country_key]]

  if (is.null(nodes)) {
    stop(paste0("No SWD node map configured for country: ", country), call. = FALSE)
  }

  nodes
}

ensure_swd_columns <- function(survey_data, nodes) {
  swd_columns <- unique(unlist(nodes[DRUG_TRACKS$drug], use.names = FALSE))
  missing_columns <- setdiff(swd_columns, names(survey_data))

  for (column_name in missing_columns) {
    survey_data[[column_name]] <- NA_integer_
  }

  survey_data
}

get_track_responses <- function(row, drug, nodes) {
  columns <- nodes[[drug]]
  unlist(row[columns], use.names = FALSE)
}

empty_track_result <- function(status = "not_assigned") {
  list(
    threshold_pct = NA_real_,
    swd = NA_real_,
    status = status,
    n_visited = 0L,
    is_non_monotone = FALSE,
    path = numeric(),
    terminal_node = NA_real_
  )
}

warn_track_mismatch <- function(row, country, assigned_ra01, drug) {
  warning(
    paste0(
      "RA01/randomized track mismatch in ",
      country,
      " CASE ",
      as.character(row[["CASE"]][[1]]),
      ": RA01=",
      assigned_ra01,
      " but ",
      drug,
      " responses present."
    ),
    call. = FALSE
  )
}

compute_person_tracks <- function(row, country, nodes) {
  assigned_ra01 <- if (RANDOMIZATION_COLUMN %in% names(row)) {
    suppressWarnings(as.integer(row[[RANDOMIZATION_COLUMN]][[1]]))
  } else {
    NA_integer_
  }

  track_results <- list()

  for (track_idx in seq_len(nrow(DRUG_TRACKS))) {
    drug <- DRUG_TRACKS$drug[[track_idx]]
    drug_code <- DRUG_TRACKS$drug_code[[track_idx]]
    track_ra01 <- DRUG_TRACKS$ra01[[track_idx]]
    responses <- get_track_responses(row, drug, nodes)
    n_visited <- sum(!is.na(numeric_response(responses)))
    is_assigned_track <- !is.na(assigned_ra01) && assigned_ra01 == track_ra01

    if (!is.na(assigned_ra01) && !is_assigned_track && n_visited > 0) {
      warn_track_mismatch(row, country, assigned_ra01, drug)
    }

    track_result <- if (is_assigned_track) {
      compute_swd(responses, nodes$relapse_pct)
    } else {
      empty_track_result("not_assigned")
    }

    track_results[[paste0("threshold_", drug_code)]] <- track_result$threshold_pct
    track_results[[paste0("swd_", drug_code)]] <- track_result$swd
    track_results[[paste0("status_", drug_code)]] <- track_result$status
    track_results[[paste0("n_visited_", drug_code)]] <- track_result$n_visited
    track_results[[paste0("non_monotone_", drug_code)]] <- track_result$is_non_monotone
  }

  track_results
}

apply_swd_country <- function(survey_data, country, nodes = swd_nodes_for_country(country)) {
  survey_data <- ensure_swd_columns(survey_data, nodes)

  if (!RANDOMIZATION_COLUMN %in% names(survey_data)) {
    survey_data[[RANDOMIZATION_COLUMN]] <- NA_integer_
  }

  person_rows <- lapply(seq_len(nrow(survey_data)), \(row_idx) {
    row <- survey_data[row_idx, , drop = FALSE]
    as_tibble(c(
      list(
        country = country,
        respondent_id = as.character(row[["CASE"]][[1]])
      ),
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

#6. Executed statements -----
# None (sourced by 01c_swd_staircase.R).
