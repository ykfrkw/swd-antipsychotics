#1. Copyright statement -----
# Copyright (c) 2026 the SWD study authors. Released under the MIT License (see LICENSE).

#2. Author -----
# Author: SWD study authors (see LICENSE)
# Date: 2026-11-13 (last revised)

#3. File description -----
# Purpose: Exact-match label maps (Japanese / German / harmonized codes ->
#          English) and display levels shared by the manuscript tables
#          (Table 1, Table S1 and Table 2).
# Inputs:  none
# Outputs: none (constant definitions)
# Depends: none
# Style:   r_script_style_guide.md (Tier B)

#4. source() and library() -----
# None: the calling script loads tidyverse and sources 00a_config.R.

# Constants -----
# Some Japanese labels use U+2212 (minus sign) and the German
# "Vor 6 Monaten - 1 Jahr" uses U+2013 (en dash).

AGE_MAP <- c(
  "18-24" = "18-24",
  "25-34" = "25-34",
  "35-44" = "35-44",
  "45-54" = "45-54",
  "55-64" = "55-64",
  "65-74" = "65 or older",
  "75+" = "65 or older"
)
AGE_TABLE_LEVELS <- c("18-24", "25-34", "35-44", "45-54", "55-64", "65 or older")

SEX_MAP <- c(
  female = "Female",
  male = "Male",
  other_or_prefer_not_to_say = "Other or prefer not to say"
)
SEX_LEVELS_EN <- c("Female", "Male", "Other or prefer not to say")

ONSET_MAP <- c(
  "<=5" = "20 or younger",
  "6-10" = "20 or younger",
  "11-15" = "20 or younger",
  "16-20" = "20 or younger",
  "21-25" = "21-30",
  "26-30" = "21-30",
  "31-35" = "31-40",
  "36-40" = "31-40",
  "41-45" = "41 or older",
  "46-50" = "41 or older",
  "51-55" = "41 or older",
  "56-60" = "41 or older",
  "61-65" = "41 or older",
  "66-70" = "41 or older",
  "71-75" = "41 or older",
  "75+" = "41 or older"
)
ONSET_TABLE_LEVELS <- c("20 or younger", "21-30", "31-40", "41 or older")

EDUC_MAP <- c(
  low = "Junior high school or lower",
  middle = "Middle (including high school)",
  high = "Bachelor or higher"
)
EDUC_LEVELS <- c(
  "Junior high school or lower",
  "Middle (including high school)",
  "Bachelor or higher"
)

EMPLOY_MAP <- c(
  employed = "Employed",
  supported_or_other = "Supported or other",
  not_employed = "Not employed"
)
EMPLOY_LEVELS <- c("Employed", "Supported or other", "Not employed")

LIVING_MAP <- c(
  "独居" = "Living alone",
  "alleinstehend" = "Living alone",
  "家族と同居" = "With family",
  "mit Familie lebend" = "With family",
  "施設在住" = "In a facility",
  "その他" = "Other",
  "sonstiges" = "Other"
)
LIVING_LEVELS <- c("Living alone", "With family", "In a facility", "Other")

# Merged monthly out-of-pocket medication cost (both countries).
# Japanese co-payment shares are converted to approximate monthly Euro bands
# assuming a typical antipsychotic prescription (~10,000 yen/month, 180 yen/Euro);
# see footnote c appended to the CSV.
COST_MAP <- c(
  "自己負担0割（例：生活保護）" = "0 Euro",
  "0 Euro" = "0 Euro",
  "自己負担1割（例：自立支援医療）" = "1-10 Euro",
  "1-10 Euro" = "1-10 Euro",
  "自己負担3割（例：一般的な健康保険）" = "11-20 Euro",
  "11-20 Euro" = "11-20 Euro",
  "21-50 Euro" = "21-50 Euro",
  "Mehr als 50 Euro" = "More than 50 Euro",
  "わからない/答えたくない" = "Unknown or prefer not to answer"
)
COST_LEVELS <- c(
  "0 Euro", "1-10 Euro", "11-20 Euro", "21-50 Euro", "More than 50 Euro",
  "Unknown or prefer not to answer"
)

ADMISSIONS_MAP <- c(
  "none" = "None",
  "1" = "1",
  "2" = "2",
  "3-5" = "3-5",
  "6-10" = "6 or more",
  ">10" = "6 or more"
)
ADMISSIONS_LEVELS <- c("None", "1", "2", "3-5", "6 or more")

LAST_ADMISSION_MAP <- c(
  "精神科入院したことがない" = "Never hospitalized",
  "Ich war noch nie im Krankenhaus" = "Never hospitalized",
  "1ヶ月前" = "Within 1 month",
  "Vor 1 Monat" = "Within 1 month",
  "2−3ヶ月前" = "2-3 months ago",
  "Vor 2-3 Monaten" = "2-3 months ago",
  "4−6ヶ月前" = "4-6 months ago",
  "Vor 4-6 Monaten" = "4-6 months ago",
  "6ヶ月から1年前" = "6-12 months ago",
  "Vor 6 Monaten – 1 Jahr" = "6-12 months ago",
  "1−3年前" = "1-3 years ago",
  "Vor 1-3 Jahren" = "1-3 years ago",
  "3年以上前" = "More than 3 years ago",
  "Vor mehr als 3 Jahren" = "More than 3 years ago"
)
LAST_ADMISSION_RECENT_LEVELS <- c(
  "Within 1 month", "2-3 months ago", "4-6 months ago"
)
LAST_ADMISSION_TABLE_LEVELS <- c(
  "Never hospitalized", "6-12 months ago", "1-3 years ago",
  "More than 3 years ago"
)

FAMILY_HISTORY_MAP <- c(
  "はい" = "Yes",
  "Ja" = "Yes",
  "いいえ" = "No",
  "Nein" = "No",
  "わからない/答えたくない" = "Unknown or prefer not to answer",
  "Weiß ich nicht / Möchte ich nicht beantworten" = "Unknown or prefer not to answer"
)
FAMILY_HISTORY_LEVELS <- c("Yes", "No", "Unknown or prefer not to answer")

EVER_ANTIPSYCHOTIC_MAP <- c(
  "はい" = "Yes",
  "Ja" = "Yes",
  "いいえ" = "No",
  "Nein" = "No"
)
EVER_ANTIPSYCHOTIC_LEVELS <- c("Yes", "No")

DRUG_HISTORY_MAP <- c(
  "現在使っている" = "Currently used",
  "Derzeit in Verwendung" = "Currently used",
  "以前使ったことがある" = "Previously used",
  "Früher verwendet" = "Previously used",
  "使ったことがない" = "Never used",
  "Noch nie verwendet" = "Never used",
  "わからない/覚えていない" = "Don't know",
  "Weiß nicht / Weiß nicht mehr" = "Don't know"
)
DRUG_HISTORY_LEVELS <- c(
  "Currently used", "Previously used", "Never used", "Don't know"
)

AE_DURATION_MAP <- c(
  "なし" = "None",
  "10%" = "10% of the time",
  "10 % der Zeit" = "10% of the time",
  "25%" = "25% of the time",
  "25 % der Zeit" = "25% of the time",
  "50%" = "50% of the time",
  "50 % der Zeit" = "50% of the time",
  "75%" = "75% of the time",
  "75 % der Zeit" = "75% of the time",
  "90%" = "90% of the time",
  "90 % der Zeit" = "90% of the time",
  "ずっと" = "All the time",
  "Die ganze Zeit" = "All the time"
)
AE_DURATION_LEVELS <- c(
  "None", "10% of the time", "25% of the time", "50% of the time",
  "75% of the time", "90% of the time", "All the time"
)

AE_SEVERITY_MAP <- c(
  "なし" = "None",
  "ごく軽度" = "Very mild",
  "Sehr leicht" = "Very mild",
  "軽度" = "Mild",
  "Leicht" = "Mild",
  "中等度" = "Moderate",
  "Mäßig" = "Moderate",
  "かなり重度" = "Marked",
  "Schwer" = "Marked",
  "重度" = "Severe",
  "Sehr schwer" = "Severe",
  "耐えられないほど重度" = "Intolerable",
  "Unerträglich" = "Intolerable"
)
AE_SEVERITY_LEVELS <- c(
  "None", "Very mild", "Mild", "Moderate", "Marked", "Severe", "Intolerable"
)

AE_INTERFERENCE_MAP <- c(
  "まったく支障なし" = "No interference",
  "Überhaupt keine Beeinträchtigung" = "No interference",
  "ごくわずかに支障あり" = "Minimal",
  "Sehr geringe Beeinträchtigung" = "Minimal",
  "軽い支障あり" = "Mild",
  "Leichte Beeinträchtigung" = "Mild",
  "中程度の支障あり" = "Moderate",
  "Mäßige Beeinträchtigung" = "Moderate",
  "かなりの支障あり" = "Marked",
  "Deutliche Beeinträchtigung" = "Marked",
  "重い支障あり" = "Severe",
  "Schwere Beeinträchtigung" = "Severe",
  "日常生活ができないほど支障あり" = "Unable to function",
  "So stark beeinträchtigt, dass der Alltag nicht möglich war" = "Unable to function"
)
AE_INTERFERENCE_LEVELS <- c(
  "No interference", "Minimal", "Mild", "Moderate", "Marked", "Severe",
  "Unable to function"
)

FAMILY_HISTORY_APPENDIX_VARS <- c(
  family_history_bipolar = "Family history: bipolar disorder",
  family_history_depression = "Family history: depression",
  family_history_anxiety = "Family history: anxiety disorder",
  family_history_ocd = "Family history: obsessive-compulsive disorder",
  family_history_developmental = "Family history: developmental disorder"
)

DRUG_HISTORY_VARS <- c(
  drug_history_asenapine = "Drug history: asenapine",
  drug_history_aripiprazole = "Drug history: aripiprazole",
  drug_history_olanzapine = "Drug history: olanzapine",
  drug_history_quetiapine = "Drug history: quetiapine",
  drug_history_clozapine = "Drug history: clozapine",
  drug_history_haloperidol = "Drug history: haloperidol",
  drug_history_paliperidone = "Drug history: paliperidone",
  drug_history_brexpiprazole = "Drug history: brexpiprazole",
  drug_history_blonanserin = "Drug history: blonanserin",
  drug_history_perospirone = "Drug history: perospirone",
  drug_history_risperidone = "Drug history: risperidone",
  drug_history_lurasidone = "Drug history: lurasidone"
)

TABLE1_DRUG_VARS <- c(
  drug_history_risperidone = "Drug history: risperidone",
  drug_history_aripiprazole = "Drug history: aripiprazole",
  drug_history_olanzapine = "Drug history: olanzapine"
)
OTHER_DRUG_VARS <- setdiff(names(DRUG_HISTORY_VARS), names(TABLE1_DRUG_VARS))

#5. Function definitions -----
# None.

#6. Executed statements -----
# None (sourced by 03_table1_by_country.R and 04_table2_subgroups.R).
