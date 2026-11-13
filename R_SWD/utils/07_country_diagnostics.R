#1. Copyright statement -----
# Copyright (c) 2026 the SWD study authors. Released under the MIT License (see LICENSE).

#2. Author -----
# Author: SWD study authors (see LICENSE)
# Date: 2026-11-13 (last revised)

#3. File description -----
# Purpose: Read-only diagnostic: is the DE-vs-JP difference in the SWD
#          distribution (DE: sharp mode at SWD = 15; JP: flat curve) driven by
#          response quality / engagement or by genuine preference
#          heterogeneity? Joins response-quality variables from the raw SoSci
#          exports (completion time, comprehension checks, finished flag,
#          missingness) onto the per-person SWD result and stratifies the SWD
#          distribution within each country by response quality. If the
#          Japanese high-quality subgroup sharpens toward a mode at SWD = 15,
#          the flatness is noise; if it stays flat, it is real heterogeneity.
# Inputs:  R_SWD/data/processed/swd_individual.csv, raw SoSci data exports in
#          R_SWD/data/raw (via 01a_raw_data_loading.R)
# Outputs: R_SWD/data/processed/swd_country_diagnostics.csv
#          (tidy: metric, country, stratum, statistic, value),
#          R_SWD/data/processed/swd_country_diagnostics.md (summary)
# Depends: 00a_config.R, 01a_raw_data_loading.R,
#          07a_country_diagnostics_markdown.R,
#          07b_country_diagnostics_summaries.R; tidyverse
# Style:   r_script_style_guide.md (Tier B)
#
# Run from the project root:  Rscript R_SWD/utils/07_country_diagnostics.R

#4. source() and library() -----
suppressWarnings(suppressMessages(library(tidyverse)))
source(file.path("R_SWD", "utils", "00a_config.R"))
source(file.path(PATH_UTILS, "01a_raw_data_loading.R"))
source(file.path(PATH_UTILS, "07a_country_diagnostics_markdown.R"))
source(file.path(PATH_UTILS, "07b_country_diagnostics_summaries.R"))

# Constants -----
STRATA_NAMES <- c("comp_pass", "comp_fail", "time_slow", "time_fast")

# Comprehension answer keys (derived from the JP codebook and verified against
# the DE value distribution, since the DE codebook omits comprehension labels).
#   CO01 multi-select "which are schizophrenia symptoms": 2 = checked,
#     1 = not checked; correct = a & b & d checked, c (blood pressure) not.
#   CO02 "how many of 100 relapse without treatment" (correct = 60):
#     DE code 3, JP code 7.
#   CO03 "about the side effects" (correct = drowsiness/insomnia common):
#     code 2 in both.
CO02_KEY <- c(DE = 3L, JP = 7L)
CO03_KEY <- 2L
CO01_KEY <- c(CO01_01 = 2L, CO01_02 = 2L, CO01_03 = 1L, CO01_04 = 2L)

QUALITY_COLS <- c(
  "CO01_01", "CO01_02", "CO01_03", "CO01_04", "CO02", "CO03",
  "TIME_SUM", "TIME_RSI", "FINISHED", "LASTPAGE", "MAXPAGE", "MISSING", "MISSREL"
)

#5. Function definitions -----
run_country_diagnostics <- function() {
  csv_path <- file.path(PATH_OUT, "swd_country_diagnostics.csv")
  md_path <- file.path(PATH_OUT, "swd_country_diagnostics.md")
  individual <- read_csv(
    file.path(PATH_OUT, "swd_individual.csv"),
    show_col_types = FALSE
  )
  person <- assigned_track(individual) %>%
    apply_documented_exclusions() %>%
    mutate(usable = .data$status %in% DETERMINABLE_STATUSES & !is.na(.data$swd))
  quality <- bind_rows(load_quality("JP"), load_quality("DE")) %>%
    add_comprehension()
  respondents <- person %>%
    left_join(quality, by = c("respondent_id", "country"))

  diagnostics <- bind_rows(map(DIAGNOSTIC_COUNTRIES, \(country_code) {
    summarise_country(
      filter(respondents, .data$country == country_code), country_code
    )
  }))
  write_csv(diagnostics, csv_path)
  writeLines(
    build_diagnostics_markdown(diagnostics, DIAGNOSTIC_COUNTRIES, STRATA_NAMES),
    md_path
  )
  print_sanity_check(respondents, diagnostics)
  cat("\nWrote:\n  ", csv_path, "\n  ", md_path, "\n")
}

as_int <- function(x) suppressWarnings(as.integer(round(as.numeric(x))))
as_num <- function(x) suppressWarnings(as.numeric(x))

# The first track (ris, ari, ola) that is not "not_assigned".
pick_assigned_track <- function(individual, row_index) {
  for (drug_code in DRUG_TRACKS$drug_code) {
    status <- individual[[paste0("status_", drug_code)]][row_index]
    if (!is.na(status) && status != "not_assigned") {
      return(c(
        swd = individual[[paste0("swd_", drug_code)]][row_index],
        status = status,
        n_visited = individual[[paste0("n_visited_", drug_code)]][row_index]
      ))
    }
  }
  c(swd = NA, status = "none", n_visited = 0)
}

assigned_track <- function(individual) {
  tracks <- lapply(seq_len(nrow(individual)), \(row_index) {
    pick_assigned_track(individual, row_index)
  })
  tibble(
    respondent_id = as.character(individual$respondent_id),
    country = individual$country,
    swd = as_num(vapply(tracks, \(track) track[["swd"]], character(1))),
    status = vapply(tracks, \(track) track[["status"]], character(1)),
    n_visited = as_int(vapply(tracks, \(track) track[["n_visited"]], character(1)))
  )
}

apply_documented_exclusions <- function(person) {
  documented <- bind_rows(
    tibble(country = "JP", respondent_id = ANALYSIS_EXCLUDE$JP),
    tibble(country = "DE", respondent_id = ANALYSIS_EXCLUDE$DE),
    tibble(country = "JP", respondent_id = RECENT_ADMISSION_EXCLUDE$JP),
    tibble(country = "DE", respondent_id = RECENT_ADMISSION_EXCLUDE$DE),
    EXPLICIT_TEST_IDS %>% mutate(respondent_id = as.character(.data$respondent_id))
  ) %>%
    distinct(country, respondent_id)
  anti_join(person, documented, by = c("country", "respondent_id"))
}

load_quality <- function(country_code) {
  path <- find_raw_file(PATH_RAW, tolower(country_code), "data", "xlsx")
  raw_quality <- read_sosci_data_excel(path)
  kept_columns <- intersect(QUALITY_COLS, names(raw_quality))
  raw_quality %>%
    transmute(
      respondent_id = as.character(as_int(.data$CASE)),
      country = country_code,
      across(all_of(kept_columns))
    )
}

add_comprehension <- function(quality) {
  is_co01_correct <- quality$CO01_01 == CO01_KEY[["CO01_01"]] &
    quality$CO01_02 == CO01_KEY[["CO01_02"]] &
    quality$CO01_03 == CO01_KEY[["CO01_03"]] &
    quality$CO01_04 == CO01_KEY[["CO01_04"]]
  is_co02_correct <- quality$CO02 == CO02_KEY[quality$country]
  is_co03_correct <- quality$CO03 == CO03_KEY
  quality %>%
    mutate(
      co01_ok = coalesce(is_co01_correct, FALSE),
      co02_ok = coalesce(is_co02_correct, FALSE),
      co03_ok = coalesce(is_co03_correct, FALSE),
      comp_all_ok = co01_ok & co02_ok & co03_ok,
      time_sum = as_num(.data$TIME_SUM),
      time_rsi = as_num(.data$TIME_RSI),
      missing_pct = as_num(.data$MISSING),
      missrel_pct = as_num(.data$MISSREL),
      finished = as_int(.data$FINISHED)
    )
}

print_sanity_check <- function(respondents, diagnostics) {
  cat("\n=== comprehension keys (auditable) ===\n")
  cat("CO01 correct: 01=2 & 02=2 & 03=1 & 04=2 | CO02 correct: DE=3, JP=7 | CO03 correct: 2\n")
  cat("\n=== sanity check ===\n")
  for (country_code in DIAGNOSTIC_COUNTRIES) {
    usable <- filter(respondents, .data$country == country_code, .data$usable)
    n_never_willing <- sum(
      respondents$status[respondents$country == country_code] == "never_willing"
    )
    cat(sprintf(
      "%s: usable n=%d, at SWD=15 n=%d, never_willing n=%d, acceptance=%s%%\n",
      country_code, nrow(usable), sum(usable$swd == FOCAL_SWD, na.rm = TRUE),
      n_never_willing,
      lookup_diagnostic(diagnostics, "acceptance", country_code, "decided", "acceptance_pct")
    ))
  }
}

#6. Executed statements -----
run_country_diagnostics()
