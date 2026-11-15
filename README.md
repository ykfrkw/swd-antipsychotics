# Smallest Worthwhile Difference of antipsychotics for relapse prevention

Analysis code for a cross-sectional survey (Germany and Japan, 2026) that
estimated the Smallest Worthwhile Difference (SWD) of antipsychotics for
schizophrenia relapse prevention with the benefit–harm trade-off method.

This repository contains the analysis only: every number in the manuscript
and its eAppendix is computed here and written as a plain numeric CSV.
Formatting of tables (rounding, labels, Word/Excel layout) is not part of
the code. The earlier full pipeline, which also produced the formatted
tables, readable exports and the data-dictionary generator, is preserved at
the git tag `full-pipeline-2026-11-13`; this version reproduces the same
numbers.

## Data availability

Individual participant data are **not** included and will not be shared.
`docs/data_dictionary.xlsx` describes every survey item (German and Japanese
versions, response codes) and every derived analysis variable, so that the
code can be read and re-used with data of the same structure.

The respondent exclusion lists are participant-level information, so they
are not part of the code either. The pipeline reads them from a local file,
`data/exclusion_ids.csv` (not shared), with three character columns:

| Column | Content |
|---|---|
| `list_name` | `explicit_test` (test respondent identified by ID), `analysis_exclude` (post hoc exclusion: free-text answer inconsistent with the staircase) or `recent_admission_exclude` (psychiatric admission within the past 6 months reported despite the screening criterion) |
| `country` | `JP` or `DE` |
| `respondent_id` | SoSci Survey `CASE` number |

## How the SWD is derived

Each respondent was randomised (`RA01`) to one drug: risperidone,
aripiprazole or olanzapine. They then answered a series of yes/no
questions of this form, with a different relapse rate *x* each time:

> Suppose that taking [drug] meant that *x* of 100 patients with stable
> schizophrenia would relapse within one year, compared with 60 of 100
> without treatment. Considering the side effects and costs of [drug],
> would you take it?

The untreated relapse rate is always 60 of 100 (`SWD_CEILING` in
`R/00_config.R`). The rates *x* on the drug run from 0 to 60 in steps of 5
(`SWD_RATE_GRID`). The questions follow a staircase: the first asks about
*x* = 0, and each answer selects the next *x* (bisection), so a respondent
answers at most five questions. The tree is `STAIRCASE_TREE` in
`R/00_config.R`:

```
x = 0  ── no ──> never willing (SWD = 60)
  └ yes ─> x = 30
            ├ yes ─> x = 45
            │         ├ yes ─> x = 55
            │         │         ├ yes ─> x = 60 ── yes: always willing (SWD = 0) / no: SWD = 5
            │         │         └ no ──> x = 50 ── yes: SWD = 10 / no: SWD = 15
            │         └ no ──> x = 35
            │                   ├ yes ─> x = 40 ── yes: SWD = 20 / no: SWD = 25
            │                   └ no ──> SWD = 30
            └ no ──> x = 15
                      ├ yes ─> x = 25
                      │         ├ yes ─> SWD = 35
                      │         └ no ──> x = 20 ── yes: SWD = 40 / no: SWD = 45
                      └ no ──> x = 5
                                ├ yes ─> x = 10 ── yes: SWD = 50 / no: SWD = 55
                                └ no ──> SWD = 60
```

The SWD is the smallest reduction in the one-year relapse rate
(percentage points) for which the respondent would take the drug:

**SWD = 60 − (highest relapse rate *x* answered "yes")**

Example: yes at 0, yes at 30, no at 45, no at 35. The highest accepted
rate is 30, so the SWD is 60 − 30 = 30. The respondent would take the
drug if it lowered relapse from 60 to 30 of 100 (a reduction of 30
points), but not from 60 to 35 (a reduction of 25).

Status of each respondent (`status_*` columns, `compute_swd()` in
`R/01c_staircase_swd.R`):

| Status | Pattern | SWD |
|---|---|---|
| `completed` | reached an end of the tree | 5–60, by the formula |
| `always_willing` | yes even at *x* = 60 (no benefit at all) | 0 |
| `never_willing` | no even at *x* = 0 (relapse prevented completely) | 60 (set to the maximum) |
| `incomplete` | stopped part-way | missing |
| `no_response` | did not answer the first question | missing |

The primary analyses include never-willing respondents with SWD = 60; the
sensitivity analyses exclude them. Respondents with a missing SWD are not
in the analysis set; the multiple-imputation sensitivity analysis (step 05)
imputes their SWD.

`check_staircase_tree()` verifies, at every run, that each end of the tree
satisfies the formula above. `check_compute_swd()` checks that typical
answer patterns give the expected SWD and status.
Answers that are not monotone (a "yes" at a higher rate after a "no" at a
lower one) do not change the path; they are counted in
`staircase_non_monotone.csv`. Because the routing only ever asks a higher
rate after a "yes" and a lower rate after a "no", the answers on the path
are monotone by construction; the check can only catch leftover answers
off the path (answers stored for questions that the final path does not
visit).

## Layout

```
swd-antipsychotics.Rproj   RStudio project (sets the working directory)
.lintr                     lintr configuration
R/                         analysis scripts, run in numeric order
data/raw/                  SoSci Survey exports (not shared, git-ignored)
data/processed/            analysis data written by step 01 (git-ignored)
data/exclusion_ids.csv     exclusion lists (not shared, git-ignored)
outputs/                   numeric CSVs and figures (git-ignored), sessionInfo.txt (tracked)
docs/data_dictionary.xlsx
```

`00_config.R` holds the paths, survey codes, the staircase tree, the item
maps, the analysis constants and the exclusion lists; `00b_de_value_labels.R`
holds the German response labels; `00c_analysis_set.R` defines the analysis
set (n = 225) and the regression covariates shared by the later steps.
Letter-suffixed files (`01a`, `05b`, ...) are helpers sourced by the entry
script with the same number. Each entry script starts with an "Analysis"
comment block that states the population, the estimand and the method.

## How to run

Put the raw exports in `data/raw/` and the exclusion file in `data/`. The
exports used are pinned by their SoSci export date stamp
(`RAW_EXPORT_STAMPS` in `R/00_config.R`), so `data/raw/` must contain
exactly one file of each of these names (the survey ID digits after `ja` /
`de` may be any number):

| Country | File |
|---|---|
| JP | `data_swd_ja<id>_2026-07-02_09-08.xlsx`, `values_swd_ja<id>_2026-05-22_16-02.csv`, `variables_swd_ja<id>_2026-05-22_16-02.csv` |
| DE | `data_swd_de<id>_2026-07-03_08-33.xlsx`, `values_swd_de<id>_2026-05-22_16-07.csv`, `variables_swd_de<id>_2026-05-22_16-07.csv` |

Other exports in the folder are ignored; a missing or duplicated file
stops the run. R must run in a UTF-8 locale (the Japanese labels and the
test-keyword pattern are UTF-8): `00_config.R` stops with a message
otherwise (set e.g. `LANG=en_US.UTF-8`). Open `swd-antipsychotics.Rproj`,
or run from the repository root:

```
Rscript R/00_run_all.R
```

This runs steps 01–07 in order, each in a fresh R process, and saves
`outputs/sessionInfo.txt`. Each step can also be run on its own from the
repository root (e.g. `Rscript R/03_regression.R`) once step 01 has written
`data/processed/swd_individual.csv` (step 07 also needs
`outputs/flow_counts.csv` from step 02).

| Step | Script | What it does |
|---|---|---|
| 01 | `01_prepare_data.R` (`01a`–`01c`) | Loads the raw exports, removes test respondents, harmonises demographics, derives each respondent's SWD from the staircase |
| 02 | `02_descriptives.R` (`02a`, `02b`) | Participant flow, characteristics by country, SWD by subgroup, SWD distribution |
| 03 | `03_regression.R` | Univariable and LASSO linear regressions of the SWD, logistic regression of SWD > 40, Holm adjustment |
| 04 | `04_interval_regression.R` | Interval regression treating the SWD as interval-censored (not reported) |
| 05 | `05_multiple_imputation.R` (`05a`–`05c`) | Multiple imputation of the missing SWD (predictive mean matching with bootstrap of the observed cases) with delta-adjusted and best/worst-case analyses; multiple imputation of missing covariates |
| 06 | `06_fibser_never_willing.R` | Post hoc Mann–Whitney test of the FIBSER total, never willing vs others |
| 07 | `07_figures.R` | Figures 2 and 3 and the eAppendix 13 figure |

## Where each reported number comes from

All files are in `outputs/` unless stated otherwise.

| Manuscript / eAppendix | Script | Output |
|---|---|---|
| Figure 2 (participant flow) and the flow counts in the Results | `02_descriptives.R` | `flow_counts.csv`; figure `figure2_flow_diagram.png` (`07_figures.R`) |
| Table 1 (characteristics by country) | `02_descriptives.R` | `characteristics_by_country.csv` (`table == "table1"`); FIBSER total: `fibser_total_descriptives.csv` |
| Results: current antipsychotic use, always / never willing counts | `02_descriptives.R` | `analysis_set_counts.csv` |
| Table 2 (SWD by subgroup) and the median SWD in the Abstract and Results | `02_descriptives.R` | `swd_by_subgroup.csv` |
| Results: SWD distribution (skewness, Shapiro–Wilk, excluding never willing) | `02_descriptives.R` | `swd_distribution.csv` |
| Non-monotone staircase answers | `02_descriptives.R` | `staircase_non_monotone.csv` |
| Figure 3 (SWD distribution with cumulative percentiles) | `07_figures.R` | `figure3_swd_distribution.png`, `figure3_swd_distribution_data.csv` |
| Results: post hoc FIBSER comparison | `06_fibser_never_willing.R` | `fibser_never_willing_test.csv` |
| eAppendix 8 (age and sex by country) and eAppendix 9 (further characteristics) | `02_descriptives.R` | `characteristics_by_country.csv` (`table == "table1"` / `"appendix9"`) |
| eAppendix 11 (Table 3: linear regression, including never willing) | `03_regression.R` | `regression_linear.csv` (`specification == "primary_including_never_willing"`) |
| eAppendix 12 section A (linear regression, excluding never willing) | `03_regression.R` | `regression_linear.csv` (`"sensitivity_excluding_never_willing"`) |
| eAppendix 12 section B (logistic regression, SWD > 40) | `03_regression.R` | `regression_logistic_swd_gt40.csv` |
| Holm-adjusted p-values (eAppendix 11 and 12) | `03_regression.R` | column `p_holm` of the two regression files |
| eAppendix 13 (FIBSER total by willingness) | `07_figures.R` | `eappendix13_fibser_by_never_willing.png` |
| eAppendix 14 and Results: imputation population (276, 51 missing, by country) | `05_multiple_imputation.R` | `mi_population_counts.csv` |
| eAppendix 14 and Limitations: stage at which non-completers stopped, comprehension check | `05_multiple_imputation.R` | `mi_stop_stages.csv` |
| eAppendix 14 Tables 14.1 and 14.2 (pooled median, mean, proportion accepting, Japan − Germany differences; delta, best and worst case) | `05_multiple_imputation.R` | `mi_pooled_estimates.csv` |
| eAppendix 14 Table 14.3 (per-imputation medians) | `05_multiple_imputation.R` | `mi_per_imputation_medians.csv` |
| eAppendix 14 Table 14.4 (covariate imputation vs complete case) | `05_multiple_imputation.R` | `mi_covariate_vs_complete_case.csv`, `mi_covariate_logged_events.csv` |
| eAppendix 14: admissible range of respondents who stopped part-way | `05_multiple_imputation.R` | `mi_staircase_stopper_bounds.csv` |
| eAppendix 15 (data dictionary) | – | `docs/data_dictionary.xlsx` (static) |
| Interval regression (not reported) | `04_interval_regression.R` | `interval_regression_intercepts.csv`, `interval_regression_univariable.csv` |

## Pooling of the imputed medians

Rubin's rules assume an approximately normal estimate with a valid
within-imputation variance, which a median of discrete 5-point data does
not have (earlier versions pooled a bootstrap variance and gave, for
Germany, an interval of 15–15 with SE 0). In `mi_pooled_estimates.csv`
the `median` and `median_difference` rows of the imputation analyses
therefore report the median of the per-imputation medians (for the Japan
minus Germany difference, of the per-imputation differences), with their
minimum and maximum across imputations in `imputation_min` and
`imputation_max`; `se`, `between_variance`, `df`, `conf_low` and
`conf_high` are empty. Single-dataset rows (complete case, best and worst
case) report the sample median without an interval and leave the range
empty. Mean and proportion rows are pooled with Rubin's rules
(Barnard–Rubin df) as before. The delta-adjusted analyses report the same
estimands as the other analyses, including the Japan minus Germany
differences. The distribution of the per-imputation medians is in
`mi_per_imputation_medians.csv`.

## Reproducibility

Seeds are constants in the scripts: 20240529 before each cross-validated
LASSO, 20261113 for the outcome imputation and 20261114 for the covariate
imputation. `00_run_all.R` writes `outputs/sessionInfo.txt`, which is
tracked in the repository (it records the R version, platform, locale and
package versions of the run that produced the reported numbers).

## Code style

All R files follow the authors' R script style guide: a six-section header
(copyright, author, file description with purpose, inputs, outputs and
dependencies, libraries, functions, executed statements), `{tidyverse}` with
the `%>%` pipe, at most 300 lines per file and 100 characters per line.
`{lintr}` (configuration in `.lintr`) was run on every file
(`lintr::lint_dir("R")`). The remaining lints are deliberate:

- `commented_code_linter` on header lines that list file paths (for example
  `# Inputs:  data/processed/swd_individual.csv`): the paths read as R code
  to the linter but are documentation.
- `object_name_linter` on `mice.impute.pmmbounded`: {mice} finds a
  user-defined imputation method only by the name `mice.impute.<method>`.
- `object_length_linter` on `USABLE_SWD_STATUSES_EXCLUDING_NEVER`: kept for
  a name that states exactly which statuses it holds.

## Software

R 4.5.1. Packages: tidyverse (dplyr, tidyr, readr, tibble, purrr, stringr,
ggplot2), readxl, fs, glmnet, survival, mice (3.19), sandwich.

## Licence

Code: MIT (see `LICENSE`).
