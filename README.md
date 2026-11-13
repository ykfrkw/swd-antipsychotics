# Smallest Worthwhile Difference of antipsychotics for relapse prevention

Analysis code for a cross-sectional survey (Germany and Japan, 2026) that
estimated the Smallest Worthwhile Difference (SWD) of antipsychotics for
schizophrenia relapse prevention with the benefit–harm trade-off method.

## Data availability

Individual participant data are **not** included and will not be shared.
`docs/data_dictionary.xlsx` describes every survey item (German and Japanese
versions, response codes) and every derived analysis variable, so that the
code can be read and re-used with data of the same structure.

The respondent exclusion lists are participant-level information, so they
are not part of the code either. The pipeline reads them from a local file,
`R_SWD/data/exclusion_ids.csv` (not shared), with three character columns:

| Column | Content |
|---|---|
| `list_name` | `explicit_test` (test respondent identified by ID), `analysis_exclude` (post hoc exclusion: free-text answer inconsistent with the staircase) or `recent_admission_exclude` (psychiatric admission within the past 6 months reported despite the screening criterion) |
| `country` | `JP` or `DE` |
| `respondent_id` | SoSci Survey `CASE` number |

## Layout

```
swd-antipsychotics.Rproj   RStudio project (sets the working directory)
.lintr                     lintr configuration (style guide naming rules)
R_SWD/utils/               all analysis scripts
R_SWD/data/raw/            SoSci Survey exports (not shared)
R_SWD/data/processed/      derived data written by the pipeline (not shared)
R_SWD/outputs/             analysis outputs (not shared)
docs/data_dictionary.xlsx
```

## How to run

Open `swd-antipsychotics.Rproj` in RStudio and source
`R_SWD/utils/00_run_all.R`, or run from the repository root:

```
Rscript R_SWD/utils/00_run_all.R
```

`00_run_all.R` runs every entry script below in order, each in a fresh R
process, then the Python table formatter (set `SWD_PYTHON` to choose the
Python executable). Each entry script can also be run on its own from the
repository root, e.g. `Rscript R_SWD/utils/03_table1_by_country.R`, once
the steps before it have written their inputs.

| Step | Entry script | Helpers | What it does | Outputs |
|---|---|---|---|---|
| 1 | `01_analysis_data.R` | `01a`–`01g` | Loads the raw exports, harmonises demographics, computes each respondent's SWD from the staircase, free text, test-respondent exclusion, summary table | `data/processed/swd_*.csv` |
| 2 | `02_report_outputs.R` | `02a`–`02g` | Tables 1 and 2 by drug arm, SWD figures, univariable and LASSO regressions (linear and SWD > 40), conversions | `data/processed/`, `outputs/report/figure*` |
| 3 | `03_table1_by_country.R` | `03a`–`03c` | Table 1 by country, Appendix Table S1, analysis-set counts | `outputs/report/` |
| 4 | `04_table2_subgroups.R` | (`03a`, `03b`) | Table 2: SWD by subgroup | `outputs/report/` |
| 5 | `05_table3_regression.R` | `05a` | Table 3 and Appendix Table S2 | `outputs/report/` |
| 6 | `06_readable_export.R` | `06a`–`06d` | Human-readable per-respondent exports with checks | `data/processed/swd_readable_*.csv` |
| 7 | `07_country_diagnostics.R` | `07a`, `07b` | Diagnostics of the DE vs JP SWD distributions | `data/processed/swd_country_diagnostics.*` |
| 8 | `08_flow_diagram.R` | `08a`, `08b` | Participant flow diagram | `outputs/report/flow_diagram.*` |
| 9 | `09_additional_descriptives.R` | `09a`, `09b` | SWD distribution descriptives, missing-data patterns, country-adjusted OLS | `outputs/r1_reviewer4/` |
| 10 | `10_holm_adjustment.R` | – | Holm correction of the univariable p-values | `outputs/r1_reviewer1/` |
| 11 | `11_interval_regression.R` | `11a` | Interval regression treating the staircase SWD as interval-censored | `outputs/r2_interval_regression/` |
| 12 | `12_multiple_imputation.R` | `12a`–`12d` | Multiple imputation of missing SWD (predictive mean matching, m = 50, by country and pooled), delta-adjusted and best/worst-case sensitivity analyses, covariate imputation | `outputs/r2_reviewer5/` |
| 13 | `13_data_dictionary.R` | `13a`, `13b` | Builds the data dictionary | `outputs/r2_reviewer5/data_dictionary.xlsx` |
| 14 | `14_make_tables_xlsx.py` (Python) | – | Formats Tables 1–3, S1 and S2 as Excel workbooks | `outputs/report/*.xlsx` |
| 15 | `15_fibser_never_willing.R` | – | Post hoc Mann–Whitney test of FIBSER total, never willing vs other analysed respondents, and FIBSER figures | `outputs/figures/` |

`00a_config.R` holds the paths, survey codes, the staircase tree, the item
maps and the exclusion lists (read from the local file above);
`00b_de_value_labels.R` holds German response labels. Letter-suffixed files
(`01a`, `02e`, ...) are helpers sourced by the entry script with the same
number. Output folder names (`r1_reviewer4`, `r2_reviewer5`, ...) are kept
as cited in the manuscript.

## Code style

All R files follow the authors' R script style guide: a six-section header
(copyright, author, file description with purpose, inputs, outputs and
dependencies, libraries, functions, executed statements), `{tidyverse}` with
the `%>%` pipe, at most 300 lines per file and 100 characters per line.
`{styler}` and `{lintr}` (configuration in `.lintr`) were run on every file.

## Software

R 4.5.1. Packages: tidyverse (dplyr, tidyr, readr, tibble, purrr, stringr,
ggplot2), here, readxl, fs, glmnet, mice (3.19), sandwich, survival,
flextable, officer, openxlsx, pdftools; gt is used for the HTML summary
when installed. Python 3 with openpyxl for `14_make_tables_xlsx.py`. Seeds
are fixed in the scripts; `sessionInfo()` is saved with the outputs.

## Licence

Code: MIT (see `LICENSE`).
