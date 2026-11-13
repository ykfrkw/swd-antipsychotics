#1. Copyright statement -----
# Copyright (c) 2026 the SWD study authors. Released under the MIT License (see LICENSE).

#2. Author -----
# Author: SWD study authors (see LICENSE)
# Date: 2026-11-13 (last revised)

#3. File description -----
# Purpose: Readable per-respondent exports for the Japanese and German teams
#          (assigned drug, SWD status and value, reason texts, demographics,
#          FIBSER total and exclusion flags), validated against the expected
#          counts, plus a console summary for manual review.
# Inputs:  R_SWD/data/processed/swd_individual.csv, raw SoSci exports in
#          R_SWD/data/raw (via 01a_raw_data_loading.R)
# Outputs: R_SWD/data/processed/swd_readable_ja.csv,
#          R_SWD/data/processed/swd_readable_de.csv (UTF-8 with BOM)
# Depends: 00a_config.R, 00b_de_value_labels.R, 01a_raw_data_loading.R,
#          06a_readable_export_helpers.R, 06b_readable_export_tables.R,
#          06c_readable_export_checks.R, 06d_readable_export_console.R;
#          tidyverse
# Style:   r_script_style_guide.md (Tier B)
#
# Run from the project root:  Rscript R_SWD/utils/06_readable_export.R

#4. source() and library() -----
# The helper files and column names contain Japanese and German text.
invisible(suppressWarnings(try(Sys.setlocale("LC_CTYPE", "en_US.UTF-8"), silent = TRUE)))
suppressPackageStartupMessages(suppressWarnings(library(tidyverse)))
source(file.path("R_SWD", "utils", "00a_config.R"))
source(file.path(PATH_UTILS, "00b_de_value_labels.R"))
source(file.path(PATH_UTILS, "01a_raw_data_loading.R"))
source(file.path(PATH_UTILS, "06a_readable_export_helpers.R"))
source(file.path(PATH_UTILS, "06b_readable_export_tables.R"))
source(file.path(PATH_UTILS, "06c_readable_export_checks.R"))
source(file.path(PATH_UTILS, "06d_readable_export_console.R"))

# Constants -----
JP_REQUIRED_COLUMNS <- c("CASE", "SW16_01", "SW17_01", "DE13", "DE14", "DE15")
DE_REQUIRED_COLUMNS <- c("CASE", "SW69_01", "SW70_01", "DE13", "DE14", "DE15")

#5. Function definitions -----
run_readable_export <- function() {
  individual_path <- file.path(PATH_OUT, "swd_individual.csv")
  output_paths <- c(
    jp = file.path(PATH_OUT, "swd_readable_ja.csv"),
    de = file.path(PATH_OUT, "swd_readable_de.csv")
  )
  stop_if_missing(individual_path)
  individual <- read_csv(
    individual_path,
    col_types = cols(.default = col_character()),
    show_col_types = FALSE
  )
  raw_data <- suppressWarnings(load_raw_data())
  jp_raw <- raw_data$jp_data %>% mutate(CASE = as.character(.data$CASE))
  de_raw <- raw_data$de_data %>% mutate(CASE = as.character(.data$CASE))
  check_inputs(individual, jp_raw, de_raw)

  raw_reasons <- build_raw_reasons(jp_raw, de_raw)
  raw_fibser <- build_raw_fibser(jp_raw, de_raw)
  jp_source <- build_country_source(individual, "JP", raw_reasons, raw_fibser)
  de_source <- build_country_source(individual, "DE", raw_reasons, raw_fibser) %>%
    mutate(CASE = .data$respondent_id) %>%
    left_join(build_de_demographics(de_raw), by = "CASE")
  readable <- list(
    jp = build_jp_readable(jp_source),
    de = build_de_readable(de_source)
  )

  fibser_counts <- validate_readable_exports(readable$jp, readable$de)
  free_text_counts <- count_free_text_reasons(jp_source, de_source)
  write_readable_exports(readable, output_paths)
  print_readable_summary(readable, raw_fibser, fibser_counts, free_text_counts)
}

check_inputs <- function(individual, jp_raw, de_raw) {
  respondent_keys <- paste(individual$country, individual$respondent_id, sep = "\r")
  if (anyDuplicated(respondent_keys) > 0) {
    stop("Duplicate country/respondent_id found in swd_individual.csv.", call. = FALSE)
  }
  if (anyDuplicated(de_raw$CASE) > 0) {
    stop("Duplicate CASE found in DE raw data.", call. = FALSE)
  }
  require_columns(jp_raw, JP_REQUIRED_COLUMNS, "JP raw data")
  require_columns(de_raw, DE_REQUIRED_COLUMNS, "DE raw data")
}

write_readable_exports <- function(readable, output_paths) {
  write_excel_csv(readable$jp, output_paths[["jp"]], na = "")
  write_excel_csv(readable$de, output_paths[["de"]], na = "")
  ensure_utf8_bom(output_paths[["jp"]])
  ensure_utf8_bom(output_paths[["de"]])

  cat("WROTE\n")
  cat(output_paths[["jp"]], "\n")
  cat(output_paths[["de"]], "\n\n")
}

#6. Executed statements -----
run_readable_export()
