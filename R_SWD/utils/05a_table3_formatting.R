#1. Copyright statement -----
# Copyright (c) 2026 the SWD study authors. Released under the MIT License (see LICENSE).

#2. Author -----
# Author: SWD study authors (see LICENSE)
# Date: 2026-11-13 (last revised)

#3. File description -----
# Purpose: Covariate display metadata and formatting helpers that turn the
#          regression result rows into Table 3 / Table S2 rows.
# Inputs:  none (functions receive the regression result tables)
# Outputs: none (function definitions)
# Depends: 00a_config.R; tidyverse
# Style:   r_script_style_guide.md (Tier B)

#4. source() and library() -----
# None: the calling script loads tidyverse and sources 00a_config.R.

# Constants -----
P_VALUE_FLOOR <- 0.001
NOT_ESTIMATED <- "—"
BETA_DIGITS <- 1
ODDS_RATIO_DIGITS <- 2
LASSO_DIGITS <- 2

# Order and names aligned with Tables 1 and 2.
COVARIATE_SPECS <- list(
  list(
    covariate = "randomized_drug",
    display = "Randomized drug arm",
    type = "factor",
    levels = c(
      risperidone = "Risperidone",
      aripiprazole = "Aripiprazole",
      olanzapine = "Olanzapine"
    ),
    ref = "risperidone"
  ),
  list(
    covariate = "country",
    display = "Country (Japan vs Germany)",
    type = "single",
    term_level = "JP"
  ),
  list(
    covariate = "currently_using_randomized_drug",
    display = "Currently using the assigned antipsychotic (yes vs no)",
    type = "single",
    term_level = "Yes"
  ),
  list(
    covariate = "sex",
    display = "Sex",
    type = "factor",
    levels = c(
      female = "Female",
      male = "Male",
      other_or_prefer_not_to_say = "Other or prefer not to say"
    ),
    ref = "female"
  ),
  list(
    covariate = "age_band_score",
    display = "Age band scoreᵃ",
    type = "numeric"
  ),
  list(
    covariate = "n_admissions_cat",
    display = "Psychiatric admissions",
    type = "factor",
    levels = c(
      "None" = "None",
      "1" = "1",
      "2" = "2",
      "3-5" = "3-5",
      "6 or more" = "6 or more"
    ),
    ref = "None"
  ),
  list(
    covariate = "employ_score",
    display = "Employment scoreᵃ",
    type = "numeric"
  ),
  list(
    covariate = "educ_score",
    display = "Education scoreᵃ",
    type = "numeric"
  ),
  list(
    covariate = "onset_age_score",
    display = "Age at onset scoreᵃ",
    type = "numeric"
  ),
  list(
    covariate = "family_history_schizophrenia",
    display = "Family history: schizophrenia",
    type = "factor",
    levels = c(
      "No" = "No",
      "Yes" = "Yes",
      "Unknown / prefer not to answer" = "Unknown or prefer not to answer"
    ),
    ref = "No"
  ),
  list(
    covariate = "ever_used_antipsychotic",
    display = "Ever used antipsychotic (yes vs no)",
    type = "single",
    term_level = "Yes"
  ),
  list(
    covariate = "fibser_total",
    display = "FIBSER total (0-18)ᵃ",
    type = "numeric"
  )
)

SCORE_NOTE <- paste0(
  "Scores modeled as continuous covariates (estimates are per 1-point ",
  "increase): employment, 1 (employed) to 2 (supported or other) to 3 (not ",
  "employed); age band, 1 (18-24 years) to 7 (75 years or older); education, ",
  "1 (low) to 3 (high); age at onset, 1 (5 years or younger) to 16 (75 years ",
  "or older); FIBSER total, 0 to 18."
)

#5. Function definitions -----
format_p_value <- function(p_value) {
  ifelse(p_value < P_VALUE_FLOOR, "<0.001", sprintf("%.3f", p_value))
}

# Round to `digits` decimals and collapse negative zero (e.g. -0.04 -> 0.0)
# so a value that rounds to zero never prints a spurious minus sign.
round_no_neg_zero <- function(values, digits) {
  rounded <- round(values, digits)
  rounded[rounded == 0] <- 0
  rounded
}

format_estimate_ci <- function(estimate, conf_low, conf_high, digits) {
  number_format <- paste0("%.", digits, "f")
  sprintf(
    paste0(number_format, " (", number_format, " to ", number_format, ")"),
    round_no_neg_zero(estimate, digits),
    round_no_neg_zero(conf_low, digits),
    round_no_neg_zero(conf_high, digits)
  )
}

# Named vector of the selected LASSO coefficients.
extract_lasso_coefficients <- function(lasso_rows) {
  selected <- lasso_rows %>%
    filter(.data$status == "selected", !is.na(.data$term))
  stats::setNames(selected$estimate, selected$term)
}

format_lasso_coefficient <- function(term, coefficients) {
  ifelse(
    term %in% names(coefficients),
    sprintf("%.2f", round_no_neg_zero(coefficients[term], LASSO_DIGITS)),
    NOT_ESTIMATED
  )
}

# use_or = FALSE -> beta (95% CI, 1 decimal); TRUE -> OR (95% CI, 2 decimals).
lookup_estimate <- function(covariate_rows, term, is_estimated, use_or) {
  term_row <- covariate_rows %>% filter(.data$term == !!term)
  if (nrow(term_row) != 1 || !is_estimated) {
    return(list(estimate = NOT_ESTIMATED, p_value = NOT_ESTIMATED))
  }
  if (use_or) {
    estimate <- format_estimate_ci(
      term_row$odds_ratio, term_row$or_conf_low, term_row$or_conf_high,
      ODDS_RATIO_DIGITS
    )
  } else {
    estimate <- format_estimate_ci(
      term_row$estimate, term_row$conf_low, term_row$conf_high, BETA_DIGITS
    )
  }
  list(estimate = estimate, p_value = format_p_value(term_row$p_value))
}

build_estimate_row <- function(display, level, n_covariate, estimate,
                               p_value, lasso) {
  tibble(
    variable = display, level = level, n = n_covariate,
    est = estimate, p = p_value, lasso = lasso
  )
}

# Factor covariates get a Reference row plus one row per non-reference
# level; single-term and numeric covariates get one row.
format_covariate_rows <- function(spec, covariate_rows, coefficients, use_or) {
  n_covariate <- as.character(covariate_rows$n[1])
  is_estimated <- covariate_rows$status[1] == "estimated"
  build_term_row <- function(term, level) {
    term_estimate <- lookup_estimate(covariate_rows, term, is_estimated, use_or)
    build_estimate_row(
      spec$display, level, n_covariate, term_estimate$estimate,
      term_estimate$p_value, format_lasso_coefficient(term, coefficients)
    )
  }

  if (spec$type != "factor") {
    term <- if (spec$type == "single") {
      paste0(spec$covariate, spec$term_level)
    } else {
      spec$covariate
    }
    return(build_term_row(term, ""))
  }

  reference_row <- build_estimate_row(
    spec$display, unname(spec$levels[spec$ref]), n_covariate, "Reference",
    "", ""
  )
  level_rows <- map(setdiff(names(spec$levels), spec$ref), \(raw_level) {
    build_term_row(
      paste0(spec$covariate, raw_level), unname(spec$levels[raw_level])
    )
  })
  bind_rows(c(list(reference_row), level_rows))
}

# univariable_rows: already filtered to the specification and model.
format_regression_rows <- function(univariable_rows, coefficients,
                                   use_or = FALSE) {
  covariate_tables <- map(COVARIATE_SPECS, \(spec) {
    covariate_rows <- univariable_rows %>%
      filter(.data$covariate == spec$covariate)
    if (nrow(covariate_rows) == 0) {
      stop(sprintf(
        "No univariable rows found for covariate %s", spec$covariate
      ))
    }
    format_covariate_rows(spec, covariate_rows, coefficients, use_or = use_or)
  })
  bind_rows(covariate_tables)
}

make_note_rows <- function(texts, superscripts, column_names) {
  notes <- tibble(
    variable = "Note",
    level = superscripts,
    n = texts,
    est = "",
    p = "",
    lasso = ""
  )
  names(notes) <- column_names
  notes
}

#6. Executed statements -----
# None (sourced by 05_table3_regression.R).
