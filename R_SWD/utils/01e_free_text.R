#1. Copyright statement -----
# Copyright (c) 2026 the SWD study authors. Released under the MIT License (see LICENSE).

#2. Author -----
# Author: SWD study authors (see LICENSE)
# Date: 2026-11-13 (last revised)

#3. File description -----
# Purpose: Collect every non-empty open (free-text) answer of the JP and DE
#          surveys into one long table with the item label and question.
# Inputs:  none (works on the list returned by load_raw_data())
# Outputs: none (function definitions)
# Depends: 00a_config.R, 01a_raw_data_loading.R (open_variable_names);
#          tidyverse
# Style:   r_script_style_guide.md (Tier B)

#4. source() and library() -----
# None: the calling script loads tidyverse and sources 00a_config.R and
# 01a_raw_data_loading.R.

# Constants -----
FREE_TEXT_COLUMNS <- c(
  "respondent_id", "country", "variable", "label", "question", "response"
)

#5. Function definitions -----
make_free_text_country <- function(survey_data, variables, country) {
  open_variables <- open_variable_names(variables)
  open_variables <- intersect(open_variables, names(survey_data))

  if (length(open_variables) == 0) {
    empty_table <- as_tibble(
      set_names(rep(list(character()), length(FREE_TEXT_COLUMNS)), FREE_TEXT_COLUMNS)
    )
    return(empty_table)
  }

  metadata <- variables %>%
    filter(.data$VAR %in% open_variables) %>%
    transmute(
      variable = .data$VAR,
      label = .data$LABEL,
      question = .data$QUESTION
    )

  answers_long <- survey_data %>%
    transmute(
      respondent_id = as.character(.data$CASE),
      across(all_of(open_variables), as.character)
    ) %>%
    pivot_longer(
      cols = all_of(open_variables),
      names_to = "variable",
      values_to = "response",
      values_transform = list(response = as.character)
    )

  answers_long %>%
    filter(!is.na(.data$response), .data$response != "") %>%
    mutate(country = country, .before = "variable") %>%
    left_join(metadata, by = "variable") %>%
    select(all_of(FREE_TEXT_COLUMNS))
}

make_free_text_table <- function(raw) {
  bind_rows(
    make_free_text_country(raw$jp_data, raw$jp_variables, "JP"),
    make_free_text_country(raw$de_data, raw$de_variables, "DE")
  )
}

#6. Executed statements -----
# None (sourced by 01_analysis_data.R).
