#1. Copyright statement -----
# Copyright (c) 2026 the SWD study authors. Released under the MIT License (see LICENSE).

#2. Author -----
# Author: SWD study authors (see LICENSE)
# Date: 2026-11-13 (last revised)

#3. File description -----
# Purpose: Data dictionary for the peer-review round 2 response:
#          sheet 1 = raw SoSci survey items per country version (variable,
#          SoSci label, question text, response codes, English description);
#          sheet 2 = derived / analysis variables; sheet 3 = sources.
# Inputs:  R_SWD/data/raw/data_swd_{ja,de}*.xlsx (newest; column set + label
#          row), values_swd_*.csv, Variable View *.pdf (SoSci variable view)
# Outputs: R_SWD/outputs/r2_reviewer5/data_dictionary.xlsx,
#          R_SWD/outputs/r2_reviewer5/sessionInfo_data_dictionary.txt
# Depends: 00a_config.R, 01a_raw_data_loading.R, 00b_de_value_labels.R,
#          13a_dictionary_derived_variables.R,
#          13b_dictionary_raw_item_descriptions.R; tidyverse, readxl,
#          pdftools, openxlsx
# Style:   r_script_style_guide.md (Tier B)
#
# Run from the project root:  Rscript R_SWD/utils/13_data_dictionary.R

#4. source() and library() -----
# The sourced pipeline files contain UTF-8 Japanese/German labels; under a C
# locale (e.g. non-interactive shells) they are mis-parsed.
if (!l10n_info()$`UTF-8`) invisible(Sys.setlocale("LC_CTYPE", "en_US.UTF-8"))
suppressPackageStartupMessages(library(tidyverse))
source(file.path("R_SWD", "utils", "00a_config.R"))
source(file.path(PATH_UTILS, "01a_raw_data_loading.R"))
source(file.path(PATH_UTILS, "00b_de_value_labels.R"))
source(file.path(PATH_UTILS, "13a_dictionary_derived_variables.R"))
source(file.path(PATH_UTILS, "13b_dictionary_raw_item_descriptions.R"))

# Constants -----
DATA_DICTIONARY_DIR <- file.path(PATH_LEGACY_OUT, "r2_reviewer5")
DATA_DICTIONARY_FILE <- file.path(DATA_DICTIONARY_DIR, "data_dictionary.xlsx")
VARIABLE_VIEW_PATTERN <- c(
  JP = "^Variable View base .*\\.pdf$",
  DE = "^Variable View apple .*\\.pdf$"
)
MIN_ITEM_INDENT <- 20L # SoSci variable-view blocks are indented
MAX_QUESTION_INDENT <- 10L
MAX_QUESTION_CHARACTERS <- 400L
VARIABLE_LINE_REGEX <- "^\\s+([A-Z]{2}[0-9]{2}[A-Za-z0-9_]*) (.+)$"
CODE_LINE_REGEX <- "^\\s+(-?[0-9]+)\\s*=\\s*(.*)$"
NOT_A_QUESTION_REGEX <- "^RA01"
SKIP_LINE_REGEX <- paste0(
  "https://|Variable View|^[0-9]+/[0-9]+/[0-9]+,|",
  "[$;{}#→]|PHP code"
)
ITEM_MARKER_REGEX <- "\\s{2,}[A-Z]{2}[0-9]{2}\\b"
# A question starts at its number ("10.") or, for staircase items, at the
# hypothetical-efficacy sentence; earlier lines are page introductions.
QUESTION_START_REGEX <- "^([0-9]+\\.\\s|仮に|Wenn die Einnahme)"
PIPELINE_CODES_COUNTRY <- "DE"
URN_COUNTER_SUFFIX_REGEX <- "_CP$"
MIN_COLUMN_WIDTH <- 12
MAX_COLUMN_WIDTH <- 60

#5. Function definitions -----
indent_width <- function(lines) {
  nchar(lines) - nchar(str_trim(lines, side = "left"))
}

collapse_question <- function(buffer) {
  starts <- which(str_detect(buffer, QUESTION_START_REGEX))
  if (length(starts) > 0) {
    buffer <- buffer[max(starts):length(buffer)]
  }
  str_squish(paste(buffer, collapse = " "))
}

# Walks the SoSci "Variable View" text: question text sits at the left
# margin; each variable line is followed by its "code = label" lines, which
# apply to every variable of the block (e.g. CO01_01..CO01_04).
parse_variable_view <- function(pdf_path) {
  lines <- unlist(str_split(pdftools::pdf_text(pdf_path), "\n"))
  question_buffer <- character()
  current_question <- NA_character_
  pending_variables <- character()
  is_after_codes <- FALSE
  items <- list()
  codes <- list()
  for (line in lines) {
    indent <- indent_width(line)
    is_variable_line <- indent >= MIN_ITEM_INDENT &&
      str_detect(line, VARIABLE_LINE_REGEX)
    is_code_line <- indent >= MIN_ITEM_INDENT &&
      str_detect(line, CODE_LINE_REGEX)
    if (is_variable_line) {
      if (is_after_codes) pending_variables <- character()
      if (length(question_buffer) > 0) {
        current_question <- collapse_question(question_buffer)
        question_buffer <- character()
      }
      parts <- str_match(line, VARIABLE_LINE_REGEX)
      pending_variables <- c(pending_variables, parts[2])
      items[[length(items) + 1]] <- tibble(
        variable = parts[2],
        pdf_label = str_squish(parts[3]),
        question_text = current_question
      )
      is_after_codes <- FALSE
    } else if (is_code_line && length(pending_variables) > 0) {
      parts <- str_match(line, CODE_LINE_REGEX)
      codes[[length(codes) + 1]] <- tibble(
        variable = pending_variables,
        code = parts[2],
        label = str_squish(parts[3])
      )
      is_after_codes <- TRUE
    } else if (indent <= MAX_QUESTION_INDENT && str_trim(line) != "" &&
      !str_detect(line, SKIP_LINE_REGEX)) {
      # Question text closes the previous block: uncoded variables (free
      # text) must not inherit the codes of the next item.
      pending_variables <- character()
      question_buffer <- c(
        question_buffer,
        str_trim(str_remove_all(line, ITEM_MARKER_REGEX))
      )
    }
  }
  list(
    items = bind_rows(items) %>% distinct(.data$variable, .keep_all = TRUE),
    codes = bind_rows(codes) %>% distinct()
  )
}

collapse_codes <- function(code_table) {
  code_table %>%
    filter(.data$label != "") %>%
    group_by(.data$variable) %>%
    summarise(
      response_codes = paste0(.data$code, " = ", .data$label,
        collapse = "; "
      ),
      .groups = "drop"
    )
}

read_values_codes <- function(country) {
  read_sosci_tsv(find_raw_file(PATH_RAW, tolower(country), "values", "csv")) %>%
    transmute(
      variable = .data$VAR, code = as.character(.data$RESPONSE),
      label = .data$MEANING
    ) %>%
    collapse_codes()
}

pipeline_de_codes <- function() {
  imap(DE_VALUE_LABELS, \(labels, variable) {
    tibble(variable = variable, code = names(labels), label = unname(labels))
  }) %>%
    bind_rows() %>%
    collapse_codes()
}

# Column set and SoSci label row of the newest data export.
read_export_labels <- function(country) {
  path <- find_raw_file(PATH_RAW, tolower(country), "data", "xlsx")
  header <- suppressWarnings(readxl::read_excel(path,
    col_types = "text",
    n_max = 1
  ))
  tibble(
    variable = names(header), sosci_label = unlist(header[1, ]),
    column_order = seq_along(header)
  )
}

truncate_text <- function(text) {
  if_else(nchar(text) > MAX_QUESTION_CHARACTERS,
    paste0(str_sub(text, 1, MAX_QUESTION_CHARACTERS), " [...]"), text
  )
}

build_raw_sheet <- function(country) {
  pdf_path <- list.files(PATH_RAW,
    pattern = VARIABLE_VIEW_PATTERN[[country]],
    full.names = TRUE
  )
  stopifnot(length(pdf_path) == 1)
  variable_view <- parse_variable_view(pdf_path)
  values_codes <- read_values_codes(country) %>%
    rename(values_codes = "response_codes")
  pdf_codes <- variable_view$codes %>%
    filter(!str_detect(.data$variable, URN_COUNTER_SUFFIX_REGEX)) %>%
    collapse_codes() %>%
    rename(pdf_codes = "response_codes")
  pipeline_codes <- if (country == PIPELINE_CODES_COUNTRY) {
    pipeline_de_codes()
  } else {
    tibble(variable = character(), response_codes = character())
  }
  pipeline_codes <- rename(pipeline_codes, pipeline_codes = "response_codes")
  read_export_labels(country) %>%
    left_join(variable_view$items, by = "variable") %>%
    left_join(values_codes, by = "variable") %>%
    left_join(pdf_codes, by = "variable") %>%
    left_join(pipeline_codes, by = "variable") %>%
    mutate(
      country_version = country,
      response_codes = coalesce(
        .data$values_codes, .data$pdf_codes,
        .data$pipeline_codes
      ),
      codes_source = case_when(
        !is.na(.data$values_codes) ~ "SoSci values export",
        !is.na(.data$pdf_codes) ~ "SoSci variable view (PDF)",
        !is.na(.data$pipeline_codes) ~ "pipeline 00b_de_value_labels.R",
        TRUE ~ NA_character_
      ),
      # Randomisation variables are not questions; the PDF block before them
      # is the consent page.
      question_text = if_else(str_detect(.data$variable, NOT_A_QUESTION_REGEX),
        NA_character_,
        truncate_text(.data$question_text)
      ),
      english_description = describe_raw_variable(.data$variable, country)
    ) %>%
    select(
      "country_version", "column_order", "variable", "sosci_label",
      "english_description", "question_text", "response_codes",
      "codes_source"
    )
}

# Half the longest cell, bounded to [MIN_COLUMN_WIDTH, MAX_COLUMN_WIDTH].
column_widths <- function(table) {
  longest_cell <- map_int(table, \(column) {
    max(nchar(as.character(column)), 0, na.rm = TRUE)
  })
  pmin(MAX_COLUMN_WIDTH, pmax(MIN_COLUMN_WIDTH, longest_cell %/% 2))
}

write_dictionary <- function(sheets, path) {
  workbook <- openxlsx::createWorkbook()
  header_style <- openxlsx::createStyle(
    textDecoration = "bold",
    fgFill = "#DDEBF7", wrapText = TRUE
  )
  wrap_style <- openxlsx::createStyle(wrapText = TRUE, valign = "top")
  iwalk(sheets, \(table, sheet_name) {
    openxlsx::addWorksheet(workbook, sheet_name)
    openxlsx::writeData(workbook, sheet_name, table, headerStyle = header_style)
    openxlsx::addStyle(workbook, sheet_name, wrap_style,
      rows = seq_len(nrow(table)) + 1,
      cols = seq_len(ncol(table)), gridExpand = TRUE
    )
    openxlsx::setColWidths(workbook, sheet_name,
      cols = seq_len(ncol(table)),
      widths = column_widths(table)
    )
    openxlsx::freezePane(workbook, sheet_name, firstRow = TRUE)
  })
  openxlsx::saveWorkbook(workbook, path, overwrite = TRUE)
}

run_data_dictionary <- function() {
  dir.create(DATA_DICTIONARY_DIR, recursive = TRUE, showWarnings = FALSE)
  raw_items <- map(COUNTRY_CODES, build_raw_sheet) %>% bind_rows()
  derived_variables <- build_derived_dictionary()
  sources <- build_source_notes()

  stopifnot(
    !anyDuplicated(raw_items[, c("country_version", "variable")]),
    all(c("SW14", "SW56", "RA01", "CO02") %in%
      raw_items$variable[raw_items$country_version == "JP"]),
    all(!is.na(raw_items$response_codes[raw_items$variable %in%
      c("RA01", "CO02", "CO03", "DE13")]))
  )
  write_dictionary(list(
    raw_survey_items = raw_items,
    derived_variables = derived_variables,
    sources_and_notes = sources
  ), DATA_DICTIONARY_FILE)
  writeLines(
    capture.output(sessionInfo()),
    file.path(DATA_DICTIONARY_DIR, "sessionInfo_data_dictionary.txt")
  )
  cat(sprintf(
    "Wrote %s: %d raw items (JP %d, DE %d), %d derived variables.\n",
    DATA_DICTIONARY_FILE, nrow(raw_items),
    sum(raw_items$country_version == "JP"),
    sum(raw_items$country_version == "DE"), nrow(derived_variables)
  ))
  print(count(raw_items, .data$country_version, .data$codes_source))
}

#6. Executed statements -----
run_data_dictionary()
