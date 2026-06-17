# =============================================================================
# 03b_dtw_robustness.R
# Purpose: Test whether the DTW metrics from script 03 are sensitive to
#          the choice of threat index variant.  Recomputes all three
#          DTW metrics using three alternative threat series and reports
#          how closely results align with the main analysis.
#
# Threat index variants tested
# --------------------------------
# 1. log_norm        (main)  Log-transformed total fatalities, normalised
#                            — the index used in script 03.
# 2. percap_norm     (alt)   Log fatalities per million population,
#                            normalised.  Controls for the fact that a
#                            conflict in a sparsely populated region
#                            represents a proportionally larger burden.
# 3. dist_norm       (alt)   Inverse-distance-weighted fatalities averaged
#                            across all EU countries, normalised.  Gives
#                            more weight to conflicts geographically
#                            closer to the EU centre of gravity — a
#                            Bartik-style instrument for threat proximity.
#
# All three variants are produced by script 01b_threat_robustness.R and
# stored in `data/processed/threat_index_variants.csv`.
#
# Robustness criterion
# ---------------------
# Pearson correlation between the main-analysis DTW scores and each
# alternative variant's DTW scores.  A correlation > 0.8 across all
# three metrics and both alternatives provides strong evidence that
# results are not driven by the specific threat operationalisation.
#
# "Sensitive" countries
# ----------------------
# Countries where `gov_responsiveness` varies by more than one standard
# deviation across the three threat variants are flagged as sensitive.
# These countries should be discussed in the paper's robustness section.
#
# Inputs
# ------
#   data/processed/stance_time_series.csv
#   data/processed/threat_index_variants.csv    (from script 01b)
#   data/processed/dtw_metrics.csv              (from script 03)
#
# Outputs
# -------
#   data/processed/dtw_metrics_robustness.csv
#       DTW metrics for every country × variant combination
#   data/processed/cluster_sensitivity.csv
#       Per-country range of gov_responsiveness across variants
#   data/processed/dtw_robustness_correlations.csv
#       Summary correlation table (variant × metric)
#   report/03b_dtw_robustness_report.txt
#
# References
# ----------
# See script 03_dtw_metrics.R for DTW references.
# =============================================================================

if (!require("tidyverse")) install.packages("tidyverse")
if (!require("dtw"))       install.packages("dtw")
if (!require("lubridate")) install.packages("lubridate")

library(tidyverse)
library(dtw)
library(lubridate)

# ── Load data ─────────────────────────────────────────────────────────────────
cat("Loading data...\n")
stance   <- read_csv("data/processed/stance_time_series.csv",
                     show_col_types = FALSE)
variants <- read_csv("data/processed/threat_index_variants.csv",
                     show_col_types = FALSE) %>%
  mutate(year = year(month))
main_dtw <- read_csv("data/processed/dtw_metrics.csv",
                     show_col_types = FALSE)

# ── Annual threat variants (aggregate monthly → annual, then normalise) ───────
threat_annual_variants <- variants %>%
  group_by(year) %>%
  summarise(
    log_annual    = mean(log_fatalities,           na.rm = TRUE),
    percap_annual = mean(log_per_million,           na.rm = TRUE),
    dist_annual   = mean(log_dist_weighted_eu_avg,  na.rm = TRUE),
    .groups       = "drop"
  ) %>%
  filter(year >= 2004, year <= 2024) %>%
  mutate(
    # Min-max normalise each variant to [0,1] for DTW comparability
    log_norm    = (log_annual    - min(log_annual,    na.rm = TRUE)) /
                  (max(log_annual,    na.rm = TRUE) - min(log_annual,    na.rm = TRUE)),
    percap_norm = (percap_annual - min(percap_annual, na.rm = TRUE)) /
                  (max(percap_annual, na.rm = TRUE) - min(percap_annual, na.rm = TRUE)),
    dist_norm   = (dist_annual   - min(dist_annual,   na.rm = TRUE)) /
                  (max(dist_annual,   na.rm = TRUE) - min(dist_annual,   na.rm = TRUE))
  )

# ── safe_dtw(): identical to the one in script 03 ────────────────────────────
safe_dtw <- function(x, y) {
  tryCatch({
    complete_idx <- !is.na(x) & !is.na(y)
    if (sum(complete_idx) < 3) return(NA_real_)
    x_c <- x[complete_idx]
    y_c <- y[complete_idx]
    norm <- function(v) {
      rng <- max(v) - min(v)
      if (rng == 0) return(rep(0, length(v)))
      (v - min(v)) / rng
    }
    dtw(norm(x_c), norm(y_c), distance.only = TRUE)$normalizedDistance
  }, error = function(e) NA_real_)
}

# ── Compute DTW for each variant × country ────────────────────────────────────
countries     <- unique(stance$country)
variants_list <- c("log_norm", "percap_norm", "dist_norm")

cat("Computing DTW robustness metrics for",
    length(countries), "countries ×",
    length(variants_list), "variants...\n")

robustness_results <- map_dfr(variants_list, function(vname) {
  # Build a two-column data frame: year + threat value for this variant
  threat_df <- threat_annual_variants %>%
    select(year) %>%
    mutate(threat = threat_annual_variants[[vname]])

  map_dfr(countries, function(cname) {
    country_data <- stance %>%
      filter(country == cname) %>%
      arrange(year) %>%
      left_join(threat_df, by = "year")

    tibble(
      country            = cname,
      variant            = vname,
      debate_intensity   = safe_dtw(country_data$gov_stance_locf,
                                     country_data$opp_stance_locf),
      gov_responsiveness = safe_dtw(country_data$gov_stance_locf,
                                     country_data$threat),
      opp_responsiveness = safe_dtw(country_data$opp_stance_locf,
                                     country_data$threat)
    )
  })
})

cat("Robustness results rows:", nrow(robustness_results), "\n")

# ── Compare with main analysis results ───────────────────────────────────────
# The main analysis corresponds to the "log_norm" variant; we cross-
# correlate the alternative variants against it.
main_long <- main_dtw %>%
  mutate(variant = "log_norm") %>%
  pivot_longer(
    cols      = c(debate_intensity, gov_responsiveness, opp_responsiveness),
    names_to  = "metric",
    values_to = "main_value"
  )

robustness_long <- robustness_results %>%
  pivot_longer(
    cols      = c(debate_intensity, gov_responsiveness, opp_responsiveness),
    names_to  = "metric",
    values_to = "alt_value"
  )

# Pearson r between main scores and each alternative variant
cor_summary <- robustness_long %>%
  filter(variant != "log_norm") %>%   # exclude main from comparison
  left_join(main_long %>% select(country, metric, main_value),
            by = c("country", "metric")) %>%
  group_by(variant, metric) %>%
  summarise(
    correlation = cor(main_value, alt_value, use = "complete.obs"),
    mean_diff   = mean(abs(alt_value - main_value), na.rm = TRUE),
    .groups     = "drop"
  )

# ── Identify sensitive countries ──────────────────────────────────────────────
# A country is "sensitive" if its gov_responsiveness varies by more than
# the inter-variant average range — flagged for discussion in the paper.
sensitivity <- robustness_results %>%
  group_by(country) %>%
  summarise(
    debate_range   = max(debate_intensity,   na.rm = TRUE) -
                     min(debate_intensity,   na.rm = TRUE),
    gov_resp_range = max(gov_responsiveness, na.rm = TRUE) -
                     min(gov_responsiveness, na.rm = TRUE),
    opp_resp_range = max(opp_responsiveness, na.rm = TRUE) -
                     min(opp_responsiveness, na.rm = TRUE),
    .groups        = "drop"
  ) %>%
  mutate(
    # Flag if this country's range exceeds 20% of the mean range
    is_sensitive = gov_resp_range > 0.2 * mean(gov_resp_range, na.rm = TRUE)
  ) %>%
  arrange(desc(gov_resp_range))

# ── Save outputs ──────────────────────────────────────────────────────────────
write_csv(robustness_results, "data/processed/dtw_metrics_robustness.csv")
write_csv(sensitivity,        "data/processed/cluster_sensitivity.csv")
write_csv(cor_summary,        "data/processed/dtw_robustness_correlations.csv")

cat("Saved: data/processed/dtw_metrics_robustness.csv\n")
cat("Saved: data/processed/cluster_sensitivity.csv\n")
cat("Saved: data/processed/dtw_robustness_correlations.csv\n")

# ── Report ────────────────────────────────────────────────────────────────────
report <- c(
  "================================================",
  "REPORT: 03b_dtw_robustness.R",
  paste("Run at:", Sys.time()),
  "================================================",
  "",
  "--- INPUT ---",
  paste("Countries:", length(countries)),
  paste("Variants tested:", paste(variants_list, collapse = ", ")),
  "  log_norm    = main analysis (log fatalities, normalised)",
  "  percap_norm = per million population, normalised",
  "  dist_norm   = distance-weighted EU average, normalised",
  "",
  "--- CORRELATION WITH MAIN RESULTS ---",
  capture.output(print(cor_summary, n = Inf)),
  "",
  "--- OVERALL ROBUSTNESS ASSESSMENT ---",
  paste("Mean correlation — debate_intensity:",
        round(mean(cor_summary$correlation[
          cor_summary$metric == "debate_intensity"], na.rm = TRUE), 3)),
  paste("Mean correlation — gov_responsiveness:",
        round(mean(cor_summary$correlation[
          cor_summary$metric == "gov_responsiveness"], na.rm = TRUE), 3)),
  paste("Mean correlation — opp_responsiveness:",
        round(mean(cor_summary$correlation[
          cor_summary$metric == "opp_responsiveness"], na.rm = TRUE), 3)),
  "",
  "--- SENSITIVE COUNTRIES ---",
  capture.output(
    sensitivity %>%
      filter(is_sensitive) %>%
      select(country, gov_resp_range, debate_range) %>%
      print()
  ),
  "",
  "--- FLAGS ---",
  if (all(cor_summary$correlation > 0.8, na.rm = TRUE))
    "OK: All variants highly correlated with main results (>0.8) — ROBUST"
  else if (any(cor_summary$correlation < 0.6, na.rm = TRUE))
    paste("WARNING: Some variants show low correlation (<0.6):",
          paste(cor_summary$variant[cor_summary$correlation < 0.6],
                collapse = ", "))
  else "INFO: Moderate robustness — some variants differ from main",
  paste("Sensitive countries:",
        paste(sensitivity$country[sensitivity$is_sensitive], collapse = ", ")),
  "",
  "--- OUTPUT FILES ---",
  "data/processed/dtw_metrics_robustness.csv",
  "data/processed/cluster_sensitivity.csv",
  "data/processed/dtw_robustness_correlations.csv",
  "",
  "--- DECISION ---",
  "If all correlations > 0.8: state 'results robust to alternative indices'.",
  "If some < 0.8: discuss sensitive countries in the paper's robustness section.",
  "Proceed to 04_clustering.R"
)

writeLines(report, "report/03b_dtw_robustness_report.txt")
cat("\nReport written to report/03b_dtw_robustness_report.txt\n")
