#1. Copyright statement -----
# Copyright (c) 2026 the SWD study authors. Released under the MIT License (see LICENSE).

#2. Author -----
# Author: SWD study authors (see LICENSE)
# Date: 2026-11-13 (last revised)

#3. File description -----
# Purpose: Locate and read the SoSci Survey exports (data, values, variables,
#          codebook) for the Japanese and German survey versions.
# Inputs:  R_SWD/data/raw/{data,values,variables,codebook}_swd_{ja,de}*_*
# Outputs: none (function definitions)
# Depends: 00a_config.R (PATH_RAW, NA_CODE); tidyverse, readxl, fs
# Style:   r_script_style_guide.md (Tier B)

#4. source() and library() -----
# None: the calling script loads tidyverse and sources 00a_config.R.

# Constants -----
FILE_PREFIX_BY_COUNTRY <- c(jp = "ja", de = "de")
SOSCI_TSV_ENCODING <- "UTF-16LE"

#5. Function definitions -----
find_raw_file <- function(path_raw, country, kind, extension) {
  country_prefix <- switch(country,
    jp = FILE_PREFIX_BY_COUNTRY[["jp"]],
    de = FILE_PREFIX_BY_COUNTRY[["de"]]
  )
  pattern <- paste0(kind, "_swd_", country_prefix, "[0-9]+_.*\\.", extension, "$")
  matched_files <- fs::dir_ls(path_raw, regexp = pattern, type = "file")

  if (length(matched_files) == 0) {
    stop("Raw file not found: ", pattern, call. = FALSE)
  }

  if (length(matched_files) > 1) {
    file_info <- fs::file_info(matched_files)
    matched_files <- matched_files[which.max(file_info$modification_time)]
    warning(
      "Multiple raw files matched ", pattern, "; using newest file.",
      call. = FALSE
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
  jp_data_path <- find_raw_file(path_raw, "jp", "data", "xlsx")
  de_data_path <- find_raw_file(path_raw, "de", "data", "xlsx")
  jp_values_path <- find_raw_file(path_raw, "jp", "values", "csv")
  de_values_path <- find_raw_file(path_raw, "de", "values", "csv")
  jp_variables_path <- find_raw_file(path_raw, "jp", "variables", "csv")
  de_variables_path <- find_raw_file(path_raw, "de", "variables", "csv")
  jp_codebook_path <- find_raw_file(path_raw, "jp", "codebook", "xlsx")
  de_codebook_path <- find_raw_file(path_raw, "de", "codebook", "xlsx")
  jp_variables <- read_sosci_tsv(jp_variables_path)
  de_variables <- read_sosci_tsv(de_variables_path)
  jp_open_variables <- open_variable_names(jp_variables)
  de_open_variables <- open_variable_names(de_variables)

  list(
    jp_data = read_sosci_data_excel(jp_data_path, text_columns = jp_open_variables),
    de_data = read_sosci_data_excel(de_data_path, text_columns = de_open_variables),
    jp_values = read_sosci_tsv(jp_values_path),
    de_values = read_sosci_tsv(de_values_path),
    jp_variables = jp_variables,
    de_variables = de_variables,
    jp_codebook = readxl::read_excel(jp_codebook_path),
    de_codebook = readxl::read_excel(de_codebook_path),
    paths = list(
      jp_data = jp_data_path,
      de_data = de_data_path,
      jp_values = jp_values_path,
      de_values = de_values_path,
      jp_variables = jp_variables_path,
      de_variables = de_variables_path,
      jp_codebook = jp_codebook_path,
      de_codebook = de_codebook_path
    )
  )
}

#6. Executed statements -----
# None (sourced by 01_analysis_data.R, 06_readable_export.R,
# 07_country_diagnostics.R, 08_flow_diagram.R and later analysis scripts).
