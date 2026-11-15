# 1. Copyright statement -----
# Copyright (c) 2026 Yuki Furukawa. Released under the MIT License (see LICENSE).

# 2. Author -----
# Author: SWD study authors (see LICENSE)
# Date: 2026-11-14 (last revised)

# 3. File description -----
# Purpose: Estimators and pooling for 05_multiple_imputation.R: median SWD,
#          mean SWD, proportion accepting the current efficacy (SWD <= 40;
#          logit scale), Japan minus Germany differences (mean: OLS with HC3
#          variance; median: difference of the country medians), Rubin's
#          rules for the mean and the proportion, the median of the
#          per-imputation values (with their range) for the medians, and the
#          delta-adjusted analyses.
# Inputs:  none (functions receive completed datasets)
# Outputs: none (function definitions)
# Depends: 00_config.R (CONFIDENCE_LEVEL, ALL_COUNTRIES_LABEL),
#          05b_mi_outcome.R (apply_delta); tidyverse, mice, sandwich

# 4. source() and library() -----
# None: the calling script loads tidyverse and sources 00_config.R.

# Constants -----
CURRENT_EFFICACY_SWD <- 40 # current antipsychotics: relapse 60% -> ~20%
LOGIT_CONTINUITY <- 0.5
COUNTRY_SUBSETS <- c(ALL_COUNTRIES_LABEL, "DE", "JP")
DIFFERENCE_SUBSET <- "JP minus DE"
ESTIMAND_MEAN_DIFFERENCE <- "mean_difference_hc3"
ESTIMAND_MEDIAN_DIFFERENCE <- "median_difference"
DIFFERENCE_ESTIMANDS <- c(ESTIMAND_MEAN_DIFFERENCE, ESTIMAND_MEDIAN_DIFFERENCE)
ESTIMAND_PCT_ACCEPTING <- "pct_accepting"
ESTIMAND_MEDIAN <- "median"
ESTIMANDS <- c(ESTIMAND_MEDIAN, "mean", ESTIMAND_PCT_ACCEPTING)
# Rubin's rules assume an approximately normal estimate with a valid
# within-imputation variance; a median of 5-point data has neither, so these
# estimands are summarised across imputations instead of pooled.
MEDIAN_ESTIMANDS <- c(ESTIMAND_MEDIAN, ESTIMAND_MEDIAN_DIFFERENCE)
# Delta shifts (SWD points) for the departure-from-MAR analysis.
DELTAS <- c(-20, -10, -5, 5, 10, 20)

# 5. Function definitions -----
subset_country <- function(completed, subset_name) {
  if (subset_name == ALL_COUNTRIES_LABEL) {
    return(completed)
  }
  completed[completed$country == subset_name, ]
}

# Logit of the proportion accepting with its delta-method variance. When no
# or every respondent accepts (logit undefined), a 0.5 continuity correction
# (Haldane-Anscombe) is added to both cells; the message records its use.
estimate_logit_share <- function(is_accepting) {
  n_accepting <- sum(is_accepting)
  n_total <- length(is_accepting)
  is_boundary <- n_accepting %in% c(0, n_total)
  correction <- if (is_boundary) LOGIT_CONTINUITY else 0
  if (is_boundary) {
    message(
      "Logit continuity correction applied (", n_accepting,
      " of ", n_total, " accepting)."
    )
  }
  accepting <- n_accepting + correction
  rejecting <- n_total - n_accepting + correction
  list(
    estimate = log(accepting / rejecting),
    variance = 1 / accepting + 1 / rejecting
  )
}

# One row per subset x estimand: estimate Q and its within variance U, on
# the scale used for pooling (logit for the proportion). The median has no
# within variance (conventional median: even n, mean of the two middle).
estimate_subset <- function(completed, subset_name) {
  swd <- subset_country(completed, subset_name)$swd
  n_subset <- length(swd)
  accept_logit <- estimate_logit_share(swd <= CURRENT_EFFICACY_SWD)
  tibble(
    subset = subset_name,
    estimand = ESTIMANDS,
    estimate = c(stats::median(swd), mean(swd), accept_logit$estimate),
    variance = c(NA_real_, stats::var(swd) / n_subset, accept_logit$variance),
    n = n_subset
  )
}

# Mean: OLS coefficient with HC3 (heteroskedasticity-robust) variance, as
# the SWD spread differs by country. Median: difference of country medians.
estimate_country_difference <- function(completed) {
  completed <- completed %>%
    mutate(country = factor(.data$country, levels = c("DE", "JP")))
  ols <- stats::lm(swd ~ country, data = completed)
  robust_variance <- sandwich::vcovHC(ols, type = "HC3")
  swd_jp <- completed$swd[completed$country == "JP"]
  swd_de <- completed$swd[completed$country == "DE"]
  tibble(
    subset = DIFFERENCE_SUBSET,
    estimand = DIFFERENCE_ESTIMANDS,
    estimate = c(
      stats::coef(ols)[["countryJP"]],
      stats::median(swd_jp) - stats::median(swd_de)
    ),
    variance = c(robust_variance["countryJP", "countryJP"], NA_real_),
    n = nrow(completed)
  )
}

estimate_dataset <- function(completed) {
  rows <- map(COUNTRY_SUBSETS, \(name) estimate_subset(completed, name))
  bind_rows(c(rows, list(estimate_country_difference(completed))))
}

estimate_datasets <- function(datasets) {
  imap(datasets, \(completed, index) {
    estimate_dataset(completed) %>% mutate(imputation = index)
  }) %>%
    bind_rows()
}

back_transform <- function(estimand, value) {
  if_else(estimand == ESTIMAND_PCT_ACCEPTING, 100 * stats::plogis(value), value)
}

# Medians: the median of the per-imputation values with their minimum and
# maximum; no standard error or interval. A single dataset (complete case,
# best / worst case) keeps its estimate and has no range.
summarise_median_rows <- function(rows) {
  is_single <- nrow(rows) == 1
  center <- stats::median(rows$estimate)
  tibble(
    n = rows$n[1], m = nrow(rows), pooled_scale = center,
    se = NA_real_, between_variance = NA_real_, df = NA_real_,
    estimate = center, conf_low = NA_real_, conf_high = NA_real_,
    imputation_min = if (is_single) NA_real_ else min(rows$estimate),
    imputation_max = if (is_single) NA_real_ else max(rows$estimate)
  )
}

# Mean and proportion: Rubin's rules with the Barnard-Rubin small-sample
# df; a single dataset (complete case, best/worst case) reduces to a Wald
# interval. Medians are summarised by summarise_median_rows().
pool_estimates <- function(estimates, analysis) {
  critical_level <- 1 - (1 - CONFIDENCE_LEVEL) / 2
  estimates %>%
    group_by(.data$subset, .data$estimand) %>%
    group_modify(\(rows, keys) {
      if (keys$estimand %in% MEDIAN_ESTIMANDS) {
        return(summarise_median_rows(rows))
      }
      parameters <- if (keys$estimand %in% DIFFERENCE_ESTIMANDS) 2 else 1
      is_single <- nrow(rows) == 1
      pooled <- if (is_single) {
        list(
          qbar = rows$estimate, t = rows$variance,
          df = rows$n - parameters, b = 0
        )
      } else {
        mice::pool.scalar(rows$estimate, rows$variance,
          n = rows$n[1],
          k = parameters
        )
      }
      # A zero total variance (every value equal) gives a degenerate interval.
      has_variance <- pooled$t > 0
      half_width <- if (has_variance) {
        stats::qt(critical_level, df = pooled$df) * sqrt(pooled$t)
      } else {
        0
      }
      tibble(
        n = rows$n[1], m = nrow(rows), pooled_scale = pooled$qbar,
        se = sqrt(pooled$t), between_variance = pooled$b, df = pooled$df,
        estimate = back_transform(keys$estimand, pooled$qbar),
        conf_low = back_transform(keys$estimand, pooled$qbar - half_width),
        conf_high = back_transform(keys$estimand, pooled$qbar + half_width),
        imputation_min = NA_real_, imputation_max = NA_real_
      )
    }) %>%
    ungroup() %>%
    mutate(analysis = analysis, .before = 1)
}

tally_imputation_medians <- function(estimates, analysis) {
  estimates %>%
    filter(.data$estimand == ESTIMAND_MEDIAN) %>%
    count(.data$subset,
      median_value = .data$estimate,
      name = "n_imputations"
    ) %>%
    mutate(analysis = analysis, .before = 1)
}

delta_label <- function(delta, scope) {
  sprintf("MI by country, delta %+d (%s)", delta, scope)
}

# Delta shifts applied to all non-completers, then to JP non-completers only.
run_delta_analyses <- function(datasets_country) {
  is_any_country <- \(country) rep(TRUE, length(country))
  is_japan_only <- \(country) country == "JP"
  scopes <- list(
    `all non-completers` = is_any_country,
    `JP non-completers only` = is_japan_only
  )
  delta_estimates <- imap(scopes, \(is_shifted_country, scope) {
    map(DELTAS, \(delta) {
      shifted <- apply_delta(datasets_country, delta, is_shifted_country)
      estimate_datasets(shifted) %>%
        pool_estimates(delta_label(delta, scope))
    })
  })
  delta_estimates %>%
    flatten() %>%
    bind_rows()
}

# 6. Executed statements -----
# None (sourced by 05_multiple_imputation.R).
