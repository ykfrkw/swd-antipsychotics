# 1. Copyright statement -----
# Copyright (c) 2026 Yuki Furukawa. Released under the MIT License (see LICENSE).

# 2. Author -----
# Author: SWD study authors (see LICENSE)
# Date: 2026-11-14 (last revised)

# 3. File description -----
# Purpose: Read the SoSci Survey exports (data, values, variables) of
#          the Japanese and German survey versions and identify test
#          respondents (by ID or by a test keyword in an open answer; the
#          open answers are screened in memory and not written out).
# Inputs:  data/raw/{data,values,variables}_swd_{ja,de}<id>_<stamp>.*
#          (stamps pinned in RAW_EXPORT_STAMPS)
# Outputs: none (function definitions)
# Depends: 00_config.R (PATH_RAW, RAW_EXPORT_STAMPS, NA_CODE,
#          EXPLICIT_TEST_IDS, TEST_KEYWORD_PATTERN); tidyverse, readxl, fs

# 4. source() and library() -----
# None: the calling script loads tidyverse and sources 00_config.R.

# Constants -----
FILE_PREFIX_BY_COUNTRY <- c(jp = "ja", de = "de")
SOSCI_TSV_ENCODING <- "UTF-16LE"
OPEN_ANSWER_COLUMNS <- c("country", "respondent_id", "response")

# 5. Function definitions -----
find_raw_file <- function(path_raw, country, kind, extension) {
  country_prefix <- switch(country,
    jp = FILE_PREFIX_BY_COUNTRY[["jp"]],
    de = FILE_PREFIX_BY_COUNTRY[["de"]]
  )
  export_stamp <- RAW_EXPORT_STAMPS[[country]][[kind]]
  pattern <- paste0(
    "/", kind, "_swd_", country_prefix, "[0-9]+_", export_stamp, "\\.", extension, "$"
  )
  matched_files <- fs::dir_ls(path_raw, regexp = pattern, type = "file")

  if (length(matched_files) != 1) {
    stop(
      "Expected exactly one raw file matching ", pattern, " in ", path_raw,
      ", found ", length(matched_files), call. = FALSE
    )
  }

  matched_files[[1]]
}

open_variable_names <- function(variables) {
  if (!all(c("VAR", "TYPE", "INPUT") %in% names(variables))) {
    return(character())
  }

  variables %>%
    filter(
      toupper(.data$TYPE) == "TEXT",
      toupper(.data$INPUT) == "OPEN"
    ) %>%
    pull(.data$VAR)
}

# SoSci exports may carry a second header row with labels: its CASE cell is
# not a number.
drop_label_row <- function(survey_data) {
  if (nrow(survey_data) == 0 || !"CASE" %in% names(survey_data)) {
    return(survey_data)
  }

  first_case <- survey_data$CASE[[1]]
  is_label_row <- is.na(suppressWarnings(as.numeric(first_case)))
  if (isTRUE(is_label_row)) {
    survey_data <- survey_data[-1, , drop = FALSE]
  }

  survey_data
}

read_sosci_data_excel <- function(path, text_columns = character()) {
  survey_data <- readxl::read_excel(path, col_types = "text") %>%
    drop_label_row()

  survey_data_clean <- survey_data %>%
    mutate(across(everything(), \(x) na_if(x, as.character(NA_CODE))))

  converted <- suppressMessages(
    survey_data_clean %>%
      type_convert(na = c("", "NA"))
  )

  # Open text answers stay character even when they look numeric.
  text_columns <- intersect(text_columns, names(converted))
  converted[text_columns] <- survey_data_clean[text_columns]

  converted
}

read_sosci_tsv <- function(path) {
  read_tsv(
    path,
    locale = locale(encoding = SOSCI_TSV_ENCODING),
    show_col_types = FALSE
  )
}

load_raw_data <- function(path_raw = PATH_RAW) {
  read_country <- function(country) {
    variables <- read_sosci_tsv(find_raw_file(path_raw, country, "variables", "csv"))
    list(
      data = read_sosci_data_excel(
        find_raw_file(path_raw, country, "data", "xlsx"),
        text_columns = open_variable_names(variables)
      ),
      values = read_sosci_tsv(find_raw_file(path_raw, country, "values", "csv")),
      variables = variables
    )
  }
  jp <- read_country("jp")
  de <- read_country("de")
  list(
    jp_data = jp$data, de_data = de$data,
    jp_values = jp$values, de_values = de$values,
    jp_variables = jp$variables, de_variables = de$variables
  )
}

# Non-empty open answers in long form, for the test-keyword screen only.
collect_open_answers_country <- function(survey_data, variables, country) {
  open_variables <- open_variable_names(variables)
  open_variables <- intersect(open_variables, names(survey_data))

  if (length(open_variables) == 0) {
    empty_table <- as_tibble(
      set_names(rep(list(character()), length(OPEN_ANSWER_COLUMNS)), OPEN_ANSWER_COLUMNS)
    )
    return(empty_table)
  }

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
    mutate(country = country) %>%
    select(all_of(OPEN_ANSWER_COLUMNS))
}

collect_open_answers <- function(raw) {
  bind_rows(
    collect_open_answers_country(raw$jp_data, raw$jp_variables, "JP"),
    collect_open_answers_country(raw$de_data, raw$de_variables, "DE")
  )
}

# The Latin keywords need word boundaries (TEST_KEYWORD_PATTERN) so that a
# genuine answer containing e.g. "latest" or "protest" is not flagged.
extract_first_keyword <- function(response) {
  keyword_match <- regexpr(TEST_KEYWORD_PATTERN, response, perl = TRUE)
  match_length <- attr(keyword_match, "match.length")
  keywords <- rep(NA_character_, length(response))
  has_keyword <- keyword_match > 0
  keywords[has_keyword] <- substring(
    response[has_keyword],
    keyword_match[has_keyword],
    keyword_match[has_keyword] + match_length[has_keyword] - 1
  )
  keywords
}

clean_matched_text <- function(response) {
  gsub("[\r\n]+", " ", response)
}

identify_test_respondents <- function(open_answers) {
  explicit_rows <- EXPLICIT_TEST_IDS %>%
    mutate(
      reason = "explicit_id",
      matched_text = NA_character_
    )

  keyword_rows <- open_answers %>%
    mutate(keyword = extract_first_keyword(.data$response)) %>%
    filter(!is.na(.data$keyword)) %>%
    transmute(
      country = .data$country,
      respondent_id = .data$respondent_id,
      reason = paste0("keyword:", .data$keyword),
      matched_text = clean_matched_text(.data$response)
    ) %>%
    distinct()

  bind_rows(explicit_rows, keyword_rows) %>%
    arrange(.data$country, .data$respondent_id, .data$reason)
}

excluded_ids <- function(exclusions) {
  exclusions %>%
    distinct(.data$country, .data$respondent_id)
}

filter_excluded_respondents <- function(respondents, exclusions) {
  ids <- excluded_ids(exclusions)

  if (nrow(ids) == 0) {
    return(respondents)
  }

  respondents %>%
    anti_join(ids, by = RESPONDENT_KEY)
}

# 6. Executed statements -----
# None (sourced by 01_prepare_data.R, 02_descriptives.R and
# 05_multiple_imputation.R).
