#1. Copyright statement -----
# Copyright (c) 2026 the SWD study authors. Released under the MIT License (see LICENSE).

#2. Author -----
# Author: SWD study authors (see LICENSE)
# Date: 2026-11-13 (last revised)

#3. File description -----
# Purpose: Build the readable per-respondent tables: raw reason texts and
#          FIBSER items, the per-country source rows, and the Japanese and
#          German export tables (column names in the survey language).
# Inputs:  none (functions receive swd_individual.csv and the raw exports)
# Outputs: none (function definitions)
# Depends: 00a_config.R, 00b_de_value_labels.R, 06a_readable_export_helpers.R;
#          tidyverse
# Style:   r_script_style_guide.md (Tier B)

#4. source() and library() -----
# None: the calling script loads tidyverse and sources 00a_config.R.

# Constants -----
JOIN_KEYS <- c("respondent_id", "country")

#5. Function definitions -----
extract_reasons <- function(survey_raw, country_code, never_column,
                            always_column) {
  survey_raw %>%
    transmute(
      respondent_id = .data$CASE,
      country = country_code,
      reason_never = clean_reason_text(.data[[never_column]]),
      reason_always = clean_reason_text(.data[[always_column]])
    )
}

build_raw_reasons <- function(jp_raw, de_raw) {
  bind_rows(
    extract_reasons(jp_raw, "JP", "SW16_01", "SW17_01"),
    extract_reasons(de_raw, "DE", "SW69_01", "SW70_01")
  )
}

extract_fibser <- function(survey_raw, country_code) {
  survey_raw %>%
    transmute(
      respondent_id = .data$CASE,
      country = country_code,
      fibser_duration_code = clean_fibser_code(.data$DE13),
      fibser_severity_code = clean_fibser_code(.data$DE14),
      fibser_interference_code = clean_fibser_code(.data$DE15),
      fibser_sum = sum_fibser_items(.data$DE13, .data$DE14, .data$DE15)
    )
}

build_raw_fibser <- function(jp_raw, de_raw) {
  bind_rows(extract_fibser(jp_raw, "JP"), extract_fibser(de_raw, "DE"))
}

build_country_source <- function(individual, country_code, raw_reasons,
                                 raw_fibser) {
  individual %>%
    filter(.data$country == country_code) %>%
    add_assigned_swd() %>%
    left_join(raw_reasons %>% filter(.data$country == country_code),
      by = JOIN_KEYS
    ) %>%
    left_join(raw_fibser %>% filter(.data$country == country_code),
      by = JOIN_KEYS
    )
}

flag_excluded <- function(respondent_ids, freetext_ids, recent_ids) {
  is_excluded <- as.character(respondent_ids) %in%
    as.character(c(freetext_ids, recent_ids))
  ifelse(is_excluded, "1", "")
}

describe_exclusion <- function(respondent_ids, freetext_ids, recent_ids,
                               freetext_reason, recent_reason) {
  case_when(
    as.character(respondent_ids) %in% as.character(freetext_ids) ~ freetext_reason,
    as.character(respondent_ids) %in% as.character(recent_ids) ~ recent_reason,
    TRUE ~ ""
  )
}

build_jp_readable <- function(jp_source) {
  tibble(
    CASE = jp_source$respondent_id,
    国 = "日本",
    SWD算出可否 = ifelse(
      jp_source$assigned_status %in% DETERMINABLE_STATUSES,
      CALCULABLE_JA, NOT_CALCULABLE_JA
    ),
    割当薬 = coalesce(jp_source$randomized_drug, "未割当"),
    判定 = unname(STATUS_LABEL_JA[jp_source$assigned_status]),
    SWD値 = readable_swd_value(jp_source$assigned_status, jp_source$assigned_swd),
    理由 = make_reason(
      jp_source$assigned_status,
      jp_source$reason_never,
      jp_source$reason_always,
      JP_ALWAYS_FALLBACK,
      JP_NEVER_FALLBACK
    ),
    年齢 = coalesce(jp_source$age_band, ""),
    性別 = localize_codes(jp_source$sex, SEX_LABEL_JA),
    学歴 = coalesce(jp_source$educ, ""),
    就労状況 = coalesce(jp_source$employ, ""),
    医療費自己負担 = coalesce(jp_source$insurance_burden, ""),
    生活状況 = coalesce(jp_source$living_situation, ""),
    精神科入院回数 = coalesce(jp_source$n_psych_admissions, ""),
    最終退院時期 = coalesce(jp_source$last_psych_admission, ""),
    発症年齢 = coalesce(jp_source$onset_age_band, ""),
    家族歴_統合失調症 = coalesce(jp_source$family_history_schizophrenia, ""),
    家族歴_双極症 = coalesce(jp_source$family_history_bipolar, ""),
    家族歴_うつ = coalesce(jp_source$family_history_depression, ""),
    家族歴_不安症 = coalesce(jp_source$family_history_anxiety, ""),
    家族歴_強迫症 = coalesce(jp_source$family_history_ocd, ""),
    家族歴_発達症 = coalesce(jp_source$family_history_developmental, ""),
    内服歴_アセナピン = coalesce(jp_source$drug_history_asenapine, ""),
    内服歴_アリピプラゾール = coalesce(jp_source$drug_history_aripiprazole, ""),
    内服歴_オランザピン = coalesce(jp_source$drug_history_olanzapine, ""),
    内服歴_クエチアピン = coalesce(jp_source$drug_history_quetiapine, ""),
    内服歴_クロザピン = coalesce(jp_source$drug_history_clozapine, ""),
    内服歴_ハロペリドール = coalesce(jp_source$drug_history_haloperidol, ""),
    内服歴_パリペリドン = coalesce(jp_source$drug_history_paliperidone, ""),
    内服歴_ブレクスピプラゾール = coalesce(jp_source$drug_history_brexpiprazole, ""),
    内服歴_ブロナンセリン = coalesce(jp_source$drug_history_blonanserin, ""),
    内服歴_ペロスピロン = coalesce(jp_source$drug_history_perospirone, ""),
    内服歴_リスペリドン = coalesce(jp_source$drug_history_risperidone, ""),
    内服歴_ルラシドン = coalesce(jp_source$drug_history_lurasidone, ""),
    副作用_持続 = coalesce(jp_source$ae_duration, ""),
    副作用_重症度 = coalesce(jp_source$ae_severity, ""),
    副作用_日常生活への支障 = coalesce(jp_source$ae_interference, ""),
    FIBSER合計 = jp_source$fibser_sum,
    抗精神病薬使用歴 = coalesce(jp_source$ever_used_antipsychotic, ""),
    除外 = flag_excluded(
      jp_source$respondent_id, ANALYSIS_EXCLUDE$JP, RECENT_ADMISSION_EXCLUDE$JP
    ),
    除外理由 = describe_exclusion(
      jp_source$respondent_id, ANALYSIS_EXCLUDE$JP, RECENT_ADMISSION_EXCLUDE$JP,
      REASON_FREETEXT_JA, REASON_RECENT_JA
    )
  )
}

# German demographics labelled from the raw codes (labels in 00b).
label_de_item <- function(de_raw, item) {
  label_codes(de_raw[[item]], DE_VALUE_LABELS[[item]])
}

build_de_demographics <- function(de_raw) {
  tibble(
    CASE = de_raw$CASE,
    Alter = label_de_item(de_raw, "DE02"),
    Geschlecht = label_de_item(de_raw, "DE03"),
    Bildungsabschluss = label_de_item(de_raw, "DE19"),
    Beschäftigungsstatus = label_de_item(de_raw, "DE05"),
    Wohnsitzland = label_de_item(de_raw, "DE04"),
    monatliche_medizinische_Kosten = label_de_item(de_raw, "DE06"),
    Lebenssituation = label_de_item(de_raw, "DE07"),
    Anzahl_stationärer_Aufenthalte = label_de_item(de_raw, "DE08"),
    letzte_Entlassung = label_de_item(de_raw, "DE09"),
    Alter_bei_Erkrankungsbeginn = label_de_item(de_raw, "DE10"),
    Familienanamnese_Schizophrenie = label_de_item(de_raw, "DE11_01"),
    Familienanamnese_Bipolar = label_de_item(de_raw, "DE11_02"),
    Familienanamnese_Depression = label_de_item(de_raw, "DE11_03"),
    Familienanamnese_Angststörung = label_de_item(de_raw, "DE11_04"),
    Familienanamnese_Zwangsstörung = label_de_item(de_raw, "DE11_05"),
    Familienanamnese_Entwicklungsstörung = label_de_item(de_raw, "DE11_06"),
    Medikamentenhistorie_Aripiprazol = label_de_item(de_raw, "DE12_01"),
    Medikamentenhistorie_Asenapin = label_de_item(de_raw, "DE12_02"),
    Medikamentenhistorie_Blonanserin = label_de_item(de_raw, "DE12_03"),
    Medikamentenhistorie_Brexpiprazol = label_de_item(de_raw, "DE12_04"),
    Medikamentenhistorie_Clozapin = label_de_item(de_raw, "DE12_05"),
    Medikamentenhistorie_Haloperidol = label_de_item(de_raw, "DE12_06"),
    Medikamentenhistorie_Lurasidon = label_de_item(de_raw, "DE12_07"),
    Medikamentenhistorie_Olanzapin = label_de_item(de_raw, "DE12_08"),
    Medikamentenhistorie_Paliperidon = label_de_item(de_raw, "DE12_09"),
    Medikamentenhistorie_Perospiron = label_de_item(de_raw, "DE12_10"),
    Medikamentenhistorie_Quetiapin = label_de_item(de_raw, "DE12_11"),
    Medikamentenhistorie_Risperidon = label_de_item(de_raw, "DE12_12"),
    Nebenwirkung_Dauer = label_de_item(de_raw, "DE13"),
    Nebenwirkung_Schweregrad = label_de_item(de_raw, "DE14"),
    Nebenwirkung_Alltagsbeeinträchtigung = label_de_item(de_raw, "DE15"),
    jemals_Antipsychotika = label_de_item(de_raw, "DE16")
  )
}

build_de_readable <- function(de_source) {
  tibble(
    CASE = de_source$CASE,
    Land = "Deutschland",
    SWD_berechenbar = ifelse(
      de_source$assigned_status %in% DETERMINABLE_STATUSES,
      CALCULABLE_DE, NOT_CALCULABLE_DE
    ),
    zugewiesenes_Medikament = coalesce(de_source$randomized_drug, "nicht zugewiesen"),
    Status = unname(STATUS_LABEL_DE[de_source$assigned_status]),
    SWD_Wert = readable_swd_value(de_source$assigned_status, de_source$assigned_swd),
    Grund = make_reason(
      de_source$assigned_status,
      de_source$reason_never,
      de_source$reason_always,
      DE_ALWAYS_FALLBACK,
      DE_NEVER_FALLBACK
    ),
    Alter = de_source$Alter,
    Geschlecht = de_source$Geschlecht,
    Bildungsabschluss = de_source$Bildungsabschluss,
    Beschäftigungsstatus = de_source$Beschäftigungsstatus,
    Wohnsitzland = de_source$Wohnsitzland,
    monatliche_medizinische_Kosten = de_source$monatliche_medizinische_Kosten,
    Lebenssituation = de_source$Lebenssituation,
    Anzahl_stationärer_Aufenthalte = de_source$Anzahl_stationärer_Aufenthalte,
    letzte_Entlassung = de_source$letzte_Entlassung,
    Alter_bei_Erkrankungsbeginn = de_source$Alter_bei_Erkrankungsbeginn,
    Familienanamnese_Schizophrenie = de_source$Familienanamnese_Schizophrenie,
    Familienanamnese_Bipolar = de_source$Familienanamnese_Bipolar,
    Familienanamnese_Depression = de_source$Familienanamnese_Depression,
    Familienanamnese_Angststörung = de_source$Familienanamnese_Angststörung,
    Familienanamnese_Zwangsstörung = de_source$Familienanamnese_Zwangsstörung,
    Familienanamnese_Entwicklungsstörung = de_source$Familienanamnese_Entwicklungsstörung,
    Medikamentenhistorie_Aripiprazol = de_source$Medikamentenhistorie_Aripiprazol,
    Medikamentenhistorie_Asenapin = de_source$Medikamentenhistorie_Asenapin,
    Medikamentenhistorie_Blonanserin = de_source$Medikamentenhistorie_Blonanserin,
    Medikamentenhistorie_Brexpiprazol = de_source$Medikamentenhistorie_Brexpiprazol,
    Medikamentenhistorie_Clozapin = de_source$Medikamentenhistorie_Clozapin,
    Medikamentenhistorie_Haloperidol = de_source$Medikamentenhistorie_Haloperidol,
    Medikamentenhistorie_Lurasidon = de_source$Medikamentenhistorie_Lurasidon,
    Medikamentenhistorie_Olanzapin = de_source$Medikamentenhistorie_Olanzapin,
    Medikamentenhistorie_Paliperidon = de_source$Medikamentenhistorie_Paliperidon,
    Medikamentenhistorie_Perospiron = de_source$Medikamentenhistorie_Perospiron,
    Medikamentenhistorie_Quetiapin = de_source$Medikamentenhistorie_Quetiapin,
    Medikamentenhistorie_Risperidon = de_source$Medikamentenhistorie_Risperidon,
    Nebenwirkung_Dauer = de_source$Nebenwirkung_Dauer,
    Nebenwirkung_Schweregrad = de_source$Nebenwirkung_Schweregrad,
    Nebenwirkung_Alltagsbeeinträchtigung = de_source$Nebenwirkung_Alltagsbeeinträchtigung,
    FIBSER_Summe = de_source$fibser_sum,
    jemals_Antipsychotika = de_source$jemals_Antipsychotika,
    除外 = flag_excluded(
      de_source$CASE, ANALYSIS_EXCLUDE$DE, RECENT_ADMISSION_EXCLUDE$DE
    ),
    除外理由 = describe_exclusion(
      de_source$CASE, ANALYSIS_EXCLUDE$DE, RECENT_ADMISSION_EXCLUDE$DE,
      REASON_FREETEXT_DE, REASON_RECENT_DE
    )
  )
}

#6. Executed statements -----
# None (sourced by 06_readable_export.R).
