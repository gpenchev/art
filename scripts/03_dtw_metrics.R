# =============================================================================
# 03_dtw_metrics.R
# Purpose: Compute Dynamic Time Warping (DTW) distance metrics for all
#          EU28 + UK countries.  These metrics are the primary inputs to
#          the cluster analysis in script 04.
#
# Metrics computed (per country)
# --------------------------------
# 1. debate_intensity
#      DTW( gov_stance, opp_stance )
#      How much do governing and opposition parties differ in their
#      pro-military rhetoric over time?  A low DTW distance means
#      government and opposition move in parallel; a high distance
#      reflects divergent trajectories — i.e. polarised defence debate.
#
# 2. gov_responsiveness
#      DTW( gov_stance, threat_index )
#      How closely does government rhetoric track the Regional Threat
#      Index over time?  Low distance = government stance mirrors
#      threat fluctuations (responsive); high distance = government
#      rhetoric is decoupled from threat dynamics (unresponsive).
#
# 3. opp_responsiveness
#      DTW( opp_stance, threat_index )
#      Same as above for the parliamentary opposition.
#
# 4-5. gov_spending_similarity / opp_spending_similarity  (secondary)
#      DTW( stance, defence_pct_govt )
#      How closely does party rhetoric track actual defence spending?
#
# Why DTW?
# --------
# Pearson correlation assumes synchronous, linear co-movement.  DTW
# allows for temporal warping: a government may react to a threat *with
# a lag*, or the response may be compressed in time.  DTW finds the
# optimal non-linear alignment between two time series and returns a
# normalised distance.  Lower = more similar in shape and timing.
# See: Sakoe & Chiba (1978); Berndt & Clifford (1994).
#
# Normalisation and the `safe_dtw()` wrapper
# --------------------------------------------
# Before the DTW call, both series are min-max normalised to [0,1].
# This ensures that absolute level differences (e.g., high-spending
# Germany vs. low-spending Luxembourg) do not dominate the distance
# measure — only the *shape* of the trajectory matters.
# `safe_dtw()` additionally handles:
#   - Series shorter than 3 complete observations → returns NA
#   - Constant series after normalisation (range = 0) → mapped to 0
#   - Any computational errors → returns NA
#
# Country-specific vs EU-average threat
# ---------------------------------------
# Script 01b produces a country-specific distance-weighted threat index
# (`threat_index_country_specific.csv`) in which each EU country's
# threat exposure reflects the geographic proximity of conflict zones
# (via inverse-distance weights from `{geosphere}`).  When available,
# this per-country threat series is used in preference to the EU-average
# index, providing a more precise matching between a country's security
# environment and its political reaction.
#
# Inputs
# ------
#   data/processed/stance_time_series.csv
#   data/processed/regional_threat_index.csv
#   data/processed/threat_index_country_specific.csv  (from 01b, optional)
#   data/processed/defence_gdp_share.csv              (from 02f, optional)
#
# Outputs
# -------
#   data/processed/dtw_metrics.csv             Main three-metric table
#   data/processed/dtw_threat_metrics.csv      Threat metrics + source flag
#   data/processed/dtw_spending_metrics.csv    Spending similarity metrics
#   data/processed/dtw_metrics_all.csv         All metrics combined
#   report/03_dtw_report.txt
#
# References
# ----------
# Sakoe, H. & Chiba, S. (1978). Dynamic programming algorithm optimization
#   for spoken word recognition. IEEE Transactions on Acoustics, Speech,
#   and Signal Processing, 26(1), 43-49.
# Berndt, D.J. & Clifford, J. (1994). Using dynamic time warping to find
#   patterns in time series. KDD Workshop, 10(16), 359-370.
# =============================================================================

if (!require("tidyverse")) install.packages("tidyverse")
if (!require("dtw"))       install.packages("dtw")
if (!require("lubridate")) install.packages("lubridate")

library(tidyverse)
library(dtw)
library(lubridate)

# ── Load data ─────────────────────────────────────────────────────────────────
cat("Loading stance time series...\n")
stance <- read_csv("data/processed/stance_time_series.csv",
                   show_col_types = FALSE)

cat("Loading regional threat index...\n")
threat_monthly <- read_csv("data/processed/regional_threat_index.csv",
                           show_col_types = FALSE) %>%
  mutate(year = year(month))

cat("Loading defence spending...\n")
defence_file <- "data/processed/defence_gdp_share.csv"
if (file.exists(defence_file)) {
  defence     <- read_csv(defence_file, show_col_types = FALSE)
  has_defence <- TRUE
} else {
  cat("Note: defence_gdp_share.csv not found — spending metrics skipped\n")
  has_defence <- FALSE
}

# ── Load country-specific distance-weighted threat (from script 01b) ─────────
country_specific_file <- "data/processed/threat_index_country_specific.csv"
if (file.exists(country_specific_file)) {
  threat_country_specific <- read_csv(country_specific_file,
                                       show_col_types = FALSE) %>%
    mutate(year = year(month))
  has_country_specific <- TRUE
  cat("Country-specific threat index loaded:",
      n_distinct(threat_country_specific$eu_country), "EU countries\n")
} else {
  has_country_specific <- FALSE
  cat("Note: threat_index_country_specific.csv not found",
      "— using EU-average threat for all countries\n")
}

# ── Reference lookup tables ───────────────────────────────────────────────────
# ISO-3 codes (used in the country-specific threat file) → country names
eu_code_to_name <- c(
  AUT="Austria",       BEL="Belgium",       BGR="Bulgaria",      HRV="Croatia",
  CYP="Cyprus",        CZE="Czech Republic", DNK="Denmark",       EST="Estonia",
  FIN="Finland",       FRA="France",         DEU="Germany",       GRC="Greece",
  HUN="Hungary",       IRL="Ireland",        ITA="Italy",         LVA="Latvia",
  LTU="Lithuania",     LUX="Luxembourg",     MLT="Malta",         NLD="Netherlands",
  POL="Poland",        PRT="Portugal",       ROU="Romania",       SVK="Slovakia",
  SVN="Slovenia",      ESP="Spain",          SWE="Sweden",        GBR="United Kingdom"
)
eu_name_to_code <- setNames(names(eu_code_to_name), eu_code_to_name)

# ISO-2 → country name (for joining with defence spending table)
iso2_to_name <- c(
  AT="Austria", BE="Belgium", BG="Bulgaria", HR="Croatia",
  CY="Cyprus",  CZ="Czech Republic", DK="Denmark", EE="Estonia",
  FI="Finland", FR="France",  DE="Germany",  GR="Greece",
  HU="Hungary", IE="Ireland", IT="Italy",    LV="Latvia",
  LT="Lithuania", LU="Luxembourg", MT="Malta", NL="Netherlands",
  PL="Poland",  PT="Portugal", RO="Romania", SK="Slovakia",
  SI="Slovenia", ES="Spain",  SE="Sweden"
)
name_to_iso2 <- setNames(names(iso2_to_name), iso2_to_name)

# ── EU-average annual threat index (fallback) ─────────────────────────────────
# Used when no country-specific index is available.
# log(x+1) smooths the heavy-tailed fatality distribution; min-max
# normalisation scales the result to [0,1] for comparability with stance.
threat_annual_eu <- threat_monthly %>%
  group_by(year) %>%
  summarise(
    annual_fatalities = sum(total_fatalities, na.rm = TRUE),
    .groups           = "drop"
  ) %>%
  mutate(log_threat = log(annual_fatalities + 1)) %>%
  filter(year >= 2004, year <= 2024) %>%
  mutate(
    threat_norm = (log_threat - min(log_threat)) /
                  (max(log_threat) - min(log_threat))
  )

# ── safe_dtw(): normalised DTW with error handling ────────────────────────────
# This wrapper is the core computational unit used throughout scripts 03
# and 03b.
#   x, y       : raw numeric vectors (need not be pre-normalised)
#   Returns    : dtw$normalizedDistance (range [0,1])
#                or NA_real_ if inputs are too short / non-finite
#
# Internal steps:
#   1. Keep only indices where both x AND y are non-NA.
#   2. Require at least 3 complete paired observations.
#   3. Min-max normalise x and y independently to [0,1].
#      If a vector is constant (range=0), map it to all-zero — this
#      means "no variation", which is a legitimate input for DTW.
#   4. Call dtw(..., distance.only=TRUE) to skip the warp-path
#      backtracking (faster; we need only the scalar distance).
#   5. Return normalizedDistance (total path cost / path length).
safe_dtw <- function(x, y) {
  tryCatch({
    complete_idx <- !is.na(x) & !is.na(y)
    if (sum(complete_idx) < 3) return(NA_real_)
    x_clean <- x[complete_idx]
    y_clean <- y[complete_idx]
    norm <- function(v) {
      rng <- max(v) - min(v)
      if (rng == 0) return(rep(0, length(v)))
      (v - min(v)) / rng
    }
    result <- dtw(norm(x_clean), norm(y_clean), distance.only = TRUE)
    result$normalizedDistance
  }, error = function(e) NA_real_)
}

# ── Get countries ──────────────────────────────────────────────────────────────
countries <- unique(stance$country)
cat("Computing DTW metrics for", length(countries), "countries...\n")

# ── Compute threat DTW metrics ────────────────────────────────────────────────
dtw_threat_list <- purrr::map_dfr(countries, function(cname) {
  cat("  Processing:", cname, "\n")

  eu_code <- eu_name_to_code[cname]

  # Select threat series: country-specific if available, else EU average
  if (has_country_specific && !is.na(eu_code) &&
      eu_code %in% unique(threat_country_specific$eu_country)) {

    # Aggregate monthly distance-weighted fatalities to annual totals,
    # then apply the same log + min-max normalisation as the main index
    country_threat_annual <- threat_country_specific %>%
      filter(eu_country == eu_code) %>%
      group_by(year) %>%
      summarise(
        annual_dist_weighted = sum(dist_weighted_fatalities, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(year >= 2004, year <= 2024) %>%
      mutate(
        log_threat  = log(annual_dist_weighted + 1),
        threat_norm = (log_threat - min(log_threat)) /
                      (max(log_threat) - min(log_threat))
      )
    threat_source <- "country_specific_distance_weighted"
  } else {
    country_threat_annual <- threat_annual_eu
    threat_source         <- "eu_average"
  }

  country_data <- stance %>%
    filter(country == cname) %>%
    arrange(year) %>%
    left_join(country_threat_annual %>% select(year, threat_norm),
              by = "year")

  tibble(
    country            = cname,
    threat_source      = threat_source,
    debate_intensity   = safe_dtw(country_data$gov_stance_locf,
                                   country_data$opp_stance_locf),
    gov_responsiveness = safe_dtw(country_data$gov_stance_locf,
                                   country_data$threat_norm),
    opp_responsiveness = safe_dtw(country_data$opp_stance_locf,
                                   country_data$threat_norm),
    n_years            = sum(!is.na(country_data$gov_stance_locf) &
                                !is.na(country_data$threat_norm))
  )
})

cat("Threat DTW metrics computed:", nrow(dtw_threat_list), "countries\n")
cat("Using country-specific threat:",
    sum(dtw_threat_list$threat_source == "country_specific_distance_weighted"),
    "countries\n")
cat("Using EU-average threat:",
    sum(dtw_threat_list$threat_source == "eu_average"), "countries\n")

# ── Compute spending DTW metrics ──────────────────────────────────────────────
if (has_defence) {
  dtw_spending_list <- purrr::map_dfr(countries, function(cname) {
    iso2 <- name_to_iso2[cname]
    if (is.na(iso2)) return(NULL)   # UK has no Eurostat COFOG data

    country_defence <- defence %>%
      filter(country_code == iso2) %>%
      select(year, defence_pct_govt) %>%
      filter(!is.na(defence_pct_govt))

    if (nrow(country_defence) < 3) return(NULL)

    country_data <- stance %>%
      filter(country == cname) %>%
      arrange(year) %>%
      left_join(country_defence, by = "year")

    tibble(
      country                 = cname,
      gov_spending_similarity = safe_dtw(country_data$gov_stance_locf,
                                          country_data$defence_pct_govt),
      opp_spending_similarity = safe_dtw(country_data$opp_stance_locf,
                                          country_data$defence_pct_govt),
      n_years                 = sum(!is.na(country_data$gov_stance_locf) &
                                      !is.na(country_data$defence_pct_govt))
    )
  })
  cat("Spending DTW metrics computed:", nrow(dtw_spending_list), "countries\n")
} else {
  dtw_spending_list <- tibble(
    country                 = character(),
    gov_spending_similarity = numeric(),
    opp_spending_similarity = numeric(),
    n_years                 = integer()
  )
}

# ── Combined metrics table ────────────────────────────────────────────────────
dtw_all <- dtw_threat_list %>%
  left_join(dtw_spending_list %>%
              select(country, gov_spending_similarity,
                     opp_spending_similarity),
            by = "country")

# ── Save outputs ──────────────────────────────────────────────────────────────
write_csv(dtw_threat_list,   "data/processed/dtw_threat_metrics.csv")
write_csv(dtw_spending_list, "data/processed/dtw_spending_metrics.csv")
write_csv(dtw_all,           "data/processed/dtw_metrics_all.csv")

# Minimal three-metric table used as primary input to cluster analysis
dtw_metrics_paper <- dtw_threat_list %>%
  select(country, debate_intensity, gov_responsiveness, opp_responsiveness)
write_csv(dtw_metrics_paper, "data/processed/dtw_metrics.csv")

cat("Saved: data/processed/dtw_threat_metrics.csv\n")
cat("Saved: data/processed/dtw_spending_metrics.csv\n")
cat("Saved: data/processed/dtw_metrics_all.csv\n")
cat("Saved: data/processed/dtw_metrics.csv\n")

# ── Report ────────────────────────────────────────────────────────────────────
report <- c(
  "================================================",
  "REPORT: 03_dtw_metrics.R",
  paste("Run at:", Sys.time()),
  "================================================",
  "",
  "--- INPUT ---",
  paste("Countries:", length(countries)),
  paste("Years: 2004-2024"),
  paste("Country-specific threat index available:", has_country_specific),
  paste("Countries using country-specific threat:",
        sum(dtw_threat_list$threat_source ==
              "country_specific_distance_weighted")),
  paste("Countries using EU-average threat:",
        sum(dtw_threat_list$threat_source == "eu_average")),
  "",
  "--- THREAT DTW METRICS ---",
  paste("Countries computed:", nrow(dtw_threat_list)),
  paste("Missing debate_intensity:",
        sum(is.na(dtw_threat_list$debate_intensity))),
  paste("Missing gov_responsiveness:",
        sum(is.na(dtw_threat_list$gov_responsiveness))),
  paste("Missing opp_responsiveness:",
        sum(is.na(dtw_threat_list$opp_responsiveness))),
  "",
  "--- METRIC SUMMARIES ---",
  "Debate Intensity (DTW gov vs opp):",
  capture.output(summary(dtw_threat_list$debate_intensity)),
  "Government Threat Responsiveness (DTW gov vs threat):",
  capture.output(summary(dtw_threat_list$gov_responsiveness)),
  "Opposition Threat Responsiveness (DTW opp vs threat):",
  capture.output(summary(dtw_threat_list$opp_responsiveness)),
  "",
  "--- TOP 5 MOST POLARISED (highest debate intensity) ---",
  capture.output(
    dtw_threat_list %>%
      arrange(desc(debate_intensity)) %>%
      select(country, debate_intensity) %>%
      head(5) %>%
      print()
  ),
  "",
  "--- TOP 5 MOST RESPONSIVE TO THREAT (lowest gov_responsiveness) ---",
  capture.output(
    dtw_threat_list %>%
      arrange(gov_responsiveness) %>%
      select(country, gov_responsiveness) %>%
      head(5) %>%
      print()
  ),
  "",
  "--- SPENDING DTW METRICS ---",
  if (has_defence) {
    c(
      paste("Countries computed:", nrow(dtw_spending_list)),
      "Gov-Spending Similarity (DTW gov stance vs defence % govt):",
      capture.output(summary(dtw_spending_list$gov_spending_similarity)),
      "Opp-Spending Similarity (DTW opp stance vs defence % govt):",
      capture.output(summary(dtw_spending_list$opp_spending_similarity))
    )
  } else "Spending metrics not computed (defence data not available)",
  "",
  "--- INTER-METRIC CORRELATIONS ---",
  capture.output(
    dtw_threat_list %>%
      select(debate_intensity, gov_responsiveness, opp_responsiveness) %>%
      cor(use = "complete.obs") %>%
      round(3) %>%
      print()
  ),
  "",
  "--- FLAGS ---",
  if (any(is.na(dtw_threat_list$debate_intensity)))
    paste("WARNING: Missing DTW values for:",
          paste(dtw_threat_list$country[
            is.na(dtw_threat_list$debate_intensity)], collapse = ", "))
  else "OK: All threat DTW metrics computed",
  if (!has_country_specific)
    "INFO: Country-specific threat not available — EU average used for all"
  else "OK: Country-specific distance-weighted threat used where available",
  "",
  "--- OUTPUT FILES ---",
  "data/processed/dtw_metrics.csv            (main 3-metric table)",
  "data/processed/dtw_threat_metrics.csv",
  "data/processed/dtw_spending_metrics.csv",
  "data/processed/dtw_metrics_all.csv",
  "",
  "--- DECISION ---",
  "Review metric distributions and flag unexpected values.",
  "Proceed to 03b_dtw_robustness.R"
)

writeLines(report, "report/03_dtw_report.txt")
cat("\nReport written to report/03_dtw_report.txt\n")
