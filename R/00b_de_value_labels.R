# -*- coding: UTF-8 -*-
# 1. Copyright statement -----
# Copyright (c) 2026 the SWD study authors. Released under the MIT License (see LICENSE).

# 2. Author -----
# Author: SWD study authors (see LICENSE)
# Date: 2026-11-14 (last revised)

# 3. File description -----
# Purpose: German response labels of the DE survey version, used when the
#          SoSci values export has no label for an item.
# Inputs:  none
# Outputs: none (defines DE_VALUE_LABELS, a named list of label vectors)
# Depends: none (sourced by 01_prepare_data.R after 00_config.R)
# Style:   r_script_style_guide.md (Tier B)

# 4. source() and library() -----
# None: the calling script loads tidyverse and sources 00_config.R.

# Constants -----
N_FAMILY_HISTORY_ITEMS <- 6L
N_DRUG_HISTORY_ITEMS <- 12L

DE02_AGE_LABELS <- c(
  "1" = "18–24 Jahre",
  "2" = "25–34 Jahre",
  "3" = "35–44 Jahre",
  "4" = "45–54 Jahre",
  "5" = "55–64 Jahre",
  "6" = "65–74 Jahre",
  "7" = "75 Jahre und älter"
)
DE03_SEX_LABELS <- c("1" = "weiblich", "2" = "männlich", "3" = "divers/keine Angabe")
DE19_EDUCATION_LABELS <- c(
  "1" = "kein Schulabschluss",
  "2" = "Hauptschulabschluss",
  "3" = "Mittlerer Schulabschluss",
  "4" = "Abitur/Fachabitur",
  "5" = "Berufsabschluss",
  "6" = "Hochschul-/Fachhochschulabschluss",
  "7" = "Bachelor",
  "8" = "Master",
  "9" = "Promotion"
)
DE05_EMPLOYMENT_LABELS <- c(
  "1" = "Vollzeit",
  "2" = "Teilzeit",
  "3" = "unterstützte Arbeit",
  "4" = "erwerbslos",
  "5" = "ehrenamtliche Tätigkeit",
  "6" = "Sonstiges"
)
DE04_COUNTRY_LABELS <- c(
  "1" = "Deutschland",
  "2" = "Österreich",
  "3" = "Die Schweiz",
  "-2" = "andere (Freitext)"
)
DE06_INSURANCE_LABELS <- c(
  "1" = "0 Euro",
  "2" = "1-10 Euro",
  "3" = "11-20 Euro",
  "4" = "21-50 Euro",
  "5" = "Mehr als 50 Euro"
)
DE07_LIVING_SITUATION_LABELS <- c(
  "1" = "alleinstehend",
  "2" = "mit Familie lebend",
  "3" = "in einer Pflegeeinrichtung lebend",
  "4" = "sonstiges"
)
DE08_ADMISSION_COUNT_LABELS <- c(
  "1" = "keine",
  "2" = "1-mal",
  "3" = "2-mal",
  "4" = "3–5-mal",
  "5" = "6–10-mal",
  "6" = "mehr als 10-mal"
)
DE09_LAST_ADMISSION_LABELS <- c(
  "1" = "Vor 1 Monat",
  "2" = "Vor 2-3 Monaten",
  "3" = "Vor 4-6 Monaten",
  "4" = "Vor 6 Monaten – 1 Jahr",
  "5" = "Vor 1-3 Jahren",
  "6" = "Vor mehr als 3 Jahren",
  "7" = "Ich war noch nie im Krankenhaus"
)
DE10_ONSET_AGE_LABELS <- c(
  "1" = "5 Jahre oder jünger",
  "2" = "6–10 Jahre",
  "3" = "11–15 Jahre",
  "4" = "16–20 Jahre",
  "5" = "21–25 Jahre",
  "6" = "26–30 Jahre",
  "7" = "31–35 Jahre",
  "8" = "36–40 Jahre",
  "9" = "41–45 Jahre",
  "10" = "46–50 Jahre",
  "11" = "51–55 Jahre",
  "12" = "56–60 Jahre",
  "13" = "61–65 Jahre",
  "14" = "66–70 Jahre",
  "15" = "71–75 Jahre",
  "16" = "75 Jahre und älter"
)
DE11_FAMILY_HISTORY_LABELS <- c(
  "1" = "Ja",
  "2" = "Nein",
  "3" = "Weiß ich nicht / Möchte ich nicht beantworten"
)
DE12_DRUG_USE_LABELS <- c(
  "1" = "Derzeit in Verwendung",
  "2" = "Früher verwendet",
  "3" = "Weiß nicht / Weiß nicht mehr",
  "4" = "Noch nie verwendet"
)
DE13_DURATION_LABELS <- c(
  "1" = "Nie",
  "2" = "10 % der Zeit",
  "3" = "25 % der Zeit",
  "4" = "50 % der Zeit",
  "5" = "75 % der Zeit",
  "6" = "90 % der Zeit",
  "7" = "Die ganze Zeit"
)
DE14_SEVERITY_LABELS <- c(
  "1" = "Keine",
  "2" = "Sehr leicht",
  "3" = "Leicht",
  "4" = "Mäßig",
  "5" = "Schwer",
  "6" = "Sehr schwer",
  "7" = "Unerträglich"
)
DE15_INTERFERENCE_LABELS <- c(
  "1" = "Überhaupt keine Beeinträchtigung",
  "2" = "Sehr geringe Beeinträchtigung",
  "3" = "Leichte Beeinträchtigung",
  "4" = "Mäßige Beeinträchtigung",
  "5" = "Deutliche Beeinträchtigung",
  "6" = "Schwere Beeinträchtigung",
  "7" = "So stark beeinträchtigt, dass der Alltag nicht möglich war"
)
DE16_EVER_USED_LABELS <- c("1" = "Ja", "2" = "Nein")

DE_VALUE_LABELS <- c(
  list(
    DE02 = DE02_AGE_LABELS,
    DE03 = DE03_SEX_LABELS,
    DE04 = DE04_COUNTRY_LABELS,
    DE05 = DE05_EMPLOYMENT_LABELS,
    DE06 = DE06_INSURANCE_LABELS,
    DE07 = DE07_LIVING_SITUATION_LABELS,
    DE08 = DE08_ADMISSION_COUNT_LABELS,
    DE09 = DE09_LAST_ADMISSION_LABELS,
    DE10 = DE10_ONSET_AGE_LABELS,
    DE13 = DE13_DURATION_LABELS,
    DE14 = DE14_SEVERITY_LABELS,
    DE15 = DE15_INTERFERENCE_LABELS,
    DE16 = DE16_EVER_USED_LABELS,
    DE19 = DE19_EDUCATION_LABELS
  ),
  stats::setNames(
    rep(list(DE11_FAMILY_HISTORY_LABELS), N_FAMILY_HISTORY_ITEMS),
    paste0("DE11_0", seq_len(N_FAMILY_HISTORY_ITEMS))
  ),
  stats::setNames(
    rep(list(DE12_DRUG_USE_LABELS), N_DRUG_HISTORY_ITEMS),
    sprintf("DE12_%02d", seq_len(N_DRUG_HISTORY_ITEMS))
  )
)

# 5. Function definitions -----
# None.

# 6. Executed statements -----
# None (sourced by 01_prepare_data.R).
