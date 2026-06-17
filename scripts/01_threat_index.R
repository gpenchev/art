# =============================================================================
# 01_threat_index.R
#
# Purpose:
#   Build a monthly Regional Threat Index for Europe's neighbourhood (1992-2024)
#   from UCDP Georeferenced Event Dataset (GED). Detects structural breaks in
#   conflict intensity using the PELT changepoint algorithm and labels the
#   resulting security regimes.
#
# Paper section: Section 3 – "Measuring External Threat"
#
# Data source (MANUAL DOWNLOAD REQUIRED):
#   UCDP GED v26.1
#   URL:      https://ucdp.uu.se/downloads/ged/ged261-csv.zip
#   The ZIP archive contains one file: GEDEvent_v26_1.csv
#
#   Download and prepare with these shell commands:
#     cd data/raw
#     curl -L -o ged261-csv.zip https://ucdp.uu.se/downloads/ged/ged261-csv.zip
#     unzip -j ged261-csv.zip
#     rm ged261-csv.zip
#     cd ../..
#
#   Licence:  Creative Commons Attribution 4.0 (CC BY 4.0)
#   Citation: Davies, S., Pettersson, T., & Öberg, M. (2026). Organized
#             violence 1989–2025, and violent political protests.
#             Journal of Peace Research.
#             https://doi.org/10.1093/jopres/xjag046
#             Sundberg, R. & Melander, E. (2013). Introducing the UCDP
#             Georeferenced Event Dataset. Journal of Peace Research 50(4).
#
# Input:
#   data/raw/GEDEvent_v26_1.csv
#
# Output:
#   data/processed/regional_threat_index.csv   – monthly fatalities + log index
#   data/processed/security_regimes.csv        – PELT-detected regime periods
#   report/01_threat_index_report.txt
#
# R packages required:
#   tidyverse, lubridate, changepoint, zoo
#   Install via: renv::restore()  (see README.md)
# =============================================================================

library(tidyverse)
library(lubridate)
library(changepoint)   # Killick & Eckley (2014) – PELT algorithm
library(zoo)

# ── 1. DEFINE NEIGHBOURHOOD ───────────────────────────────────────────────────
# The neighbourhood is defined as countries geographically proximate to the EU
# that have experienced organised armed conflict since 1992. Grouped by region
# to facilitate inspection and future extension.

balkans <- c("Albania", "Bosnia-Herzegovina", "Croatia", "Kosovo",
             "Montenegro", "North Macedonia", "Serbia", "Slovenia")

eastern_partnership <- c("Armenia", "Azerbaijan", "Belarus",
                          "Georgia", "Moldova", "Ukraine")

north_africa <- c("Algeria", "Egypt", "Libya", "Morocco", "Tunisia")

levant <- c("Israel", "Jordan", "Lebanon", "Palestine", "Syria",
            "Iraq", "Yemen", "Saudi Arabia", "Iran")

# Extended neighbourhood: other conflict-prone countries whose instability
# has strategic relevance for EU security (Sahel, Horn of Africa, Central Asia)
other <- c("Russia", "Turkey", "Afghanistan", "Pakistan",
           "Sudan", "South Sudan", "Somalia", "Mali",
           "Nigeria", "Cameroon", "Chad", "Niger",
           "Ethiopia", "Eritrea", "Djibouti")

neighbourhood_countries <- c(balkans, eastern_partnership,
                              north_africa, levant, other)

# ── 2. LOAD RAW GED DATA ──────────────────────────────────────────────────────
# GEDEvent_v26_1.csv must be placed at data/raw/GEDEvent_v26_1.csv before
# running. See the file header for download and unzip instructions.

cat("Loading UCDP GED data...\n")
ged_raw <- read_csv("data/raw/GEDEvent_v26_1.csv", show_col_types = FALSE)

cat("GED raw rows:", nrow(ged_raw), "\n")
cat("GED columns:", paste(names(ged_raw), collapse = ", "), "\n")

# ── 3. HARMONISE COUNTRY NAMES ────────────────────────────────────────────────
# GED uses historical country names (e.g. "Russia (Soviet Union)") that do not
# match our neighbourhood list. Remap to modern standard names.

ged_raw <- ged_raw %>%
  mutate(country = case_when(
    country == "Russia (Soviet Union)"  ~ "Russia",
    country == "Serbia (Yugoslavia)"    ~ "Serbia",
    country == "Yemen (North Yemen)"    ~ "Yemen",
    country == "Bosnia-Hercegovina"     ~ "Bosnia-Herzegovina",
    country == "Macedonia, FYR"         ~ "North Macedonia",
    country == "Macedonia"              ~ "North Macedonia",
    TRUE ~ country
  ))

# ── 4. FILTER TO NEIGHBOURHOOD, 1992–2024 ────────────────────────────────────
ged_filtered <- ged_raw %>%
  filter(country %in% neighbourhood_countries) %>%
  mutate(
    event_date = as_date(date_start),
    year       = year(event_date),
    month      = floor_date(event_date, "month")  # collapse to month-start date
  ) %>%
  filter(year >= 1992, year <= 2024)

cat("Filtered rows (neighbourhood, 1992-2024):", nrow(ged_filtered), "\n")
cat("Countries found:", n_distinct(ged_filtered$country), "\n")
cat("Countries in data:", paste(sort(unique(ged_filtered$country)), collapse = ", "), "\n")

# Countries not appearing in GED have zero conflict events (not a data error)
not_found <- neighbourhood_countries[
  !neighbourhood_countries %in% unique(ged_filtered$country)
]
cat("Countries not found in GED (assumed zero events):",
    paste(not_found, collapse = ", "), "\n")

# ── 5. AGGREGATE TO MONTHLY TOTALS ───────────────────────────────────────────
# Use GED's "best" estimate for fatalities (midpoint of low–high range).
# n_events and n_countries are retained for diagnostics.

monthly_index <- ged_filtered %>%
  group_by(month) %>%
  summarise(
    total_fatalities = sum(best, na.rm = TRUE),
    low_fatalities   = sum(low,  na.rm = TRUE),   # lower uncertainty bound
    high_fatalities  = sum(high, na.rm = TRUE),   # upper uncertainty bound
    n_events         = n(),
    n_countries      = n_distinct(country),
    .groups = "drop"
  ) %>%
  arrange(month)

# Fill months with zero recorded events (not all months have conflict)
all_months <- tibble(
  month = seq(
    floor_date(as_date("1992-01-01"), "month"),
    floor_date(as_date("2024-12-01"), "month"),
    by = "month"
  )
)

regional_threat_index <- all_months %>%
  left_join(monthly_index, by = "month") %>%
  mutate(across(where(is.numeric), ~replace_na(., 0)))

cat("Monthly index rows:", nrow(regional_threat_index), "\n")
cat("Date range:",
    format(min(regional_threat_index$month), "%Y-%m"),
    "to",
    format(max(regional_threat_index$month), "%Y-%m"), "\n")

# ── 6. LOG-TRANSFORM THE INDEX ────────────────────────────────────────────────
# Fatality distributions are extremely right-skewed (most months: hundreds;
# peak months: >100,000). Log(x + 1) compresses the scale while keeping zeros.
# The +1 offset ensures log is defined for zero-fatality months.

regional_threat_index <- regional_threat_index %>%
  mutate(log_fatalities = log(total_fatalities + 1))

# ── 7. CHANGEPOINT DETECTION (PELT) ──────────────────────────────────────────
# We use Pruned Exact Linear Time (PELT) to detect structural breaks in the
# mean and variance of the log-fatalities series.
#
# Method choice:
#   - PELT (Killick et al. 2012) is computationally efficient and consistent.
#   - BIC penalty balances fit vs. parsimony; avoids over-segmentation.
#   - minseglen = 24: each regime must span at least 24 months (2 years),
#     ensuring regimes correspond to structural shifts, not noise.
#
# Reference: Killick, R. & Eckley, I.A. (2014). changepoint: An R Package
#            for Changepoint Analysis. Journal of Statistical Software 58(3).

cat("\nRunning changepoint analysis (PELT, minseglen = 24)...\n")
log_series  <- regional_threat_index$log_fatalities
cpt_result  <- cpt.meanvar(log_series,
                            method    = "PELT",
                            penalty   = "BIC",
                            minseglen = 24)
cpt_positions <- cpts(cpt_result)
cat("Changepoints found at positions:", paste(cpt_positions, collapse = ", "), "\n")

# ── 8. BUILD SECURITY REGIMES ─────────────────────────────────────────────────
# Each regime spans the interval between two consecutive changepoints.
# Summaries (mean log fatalities, total fatalities) characterise threat level.

breakpoints    <- c(0, cpt_positions, nrow(regional_threat_index))

security_regimes <- purrr::map_dfr(seq_len(length(breakpoints) - 1), function(i) {
  start_idx <- breakpoints[i] + 1
  end_idx   <- breakpoints[i + 1]
  segment   <- regional_threat_index[start_idx:end_idx, ]

  tibble(
    regime_id           = i,
    period_start        = format(min(segment$month), "%B %Y"),
    period_end          = format(max(segment$month), "%B %Y"),
    start_date          = min(segment$month),
    end_date            = max(segment$month),
    n_months            = nrow(segment),
    mean_log_fatalities = mean(segment$log_fatalities, na.rm = TRUE),
    mean_fatalities     = mean(segment$total_fatalities, na.rm = TRUE),
    total_fatalities    = sum(segment$total_fatalities, na.rm = TRUE),
    period_description  = paste("Regime", i)  # update manually after inspection
  )
})

cat("Security regimes identified:", nrow(security_regimes), "\n")

# ── 9. SAVE OUTPUTS ───────────────────────────────────────────────────────────
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
write_csv(regional_threat_index, "data/processed/regional_threat_index.csv")
write_csv(security_regimes,      "data/processed/security_regimes.csv")
cat("Saved: data/processed/regional_threat_index.csv\n")
cat("Saved: data/processed/security_regimes.csv\n")

# ── 10. WRITE REPORT ──────────────────────────────────────────────────────────
dir.create("report", showWarnings = FALSE)

report <- c(
  "================================================",
  "REPORT: 01_threat_index.R",
  paste("Run at:", Sys.time()),
  "================================================",
  "",
  "--- INPUT ---",
  paste("GED raw rows:", nrow(ged_raw)),
  paste("GED columns:", ncol(ged_raw)),
  "",
  "--- COUNTRY NAME REMAPPING ---",
  "Russia (Soviet Union)  -> Russia",
  "Serbia (Yugoslavia)    -> Serbia",
  "Yemen (North Yemen)    -> Yemen",
  "Bosnia-Hercegovina     -> Bosnia-Herzegovina",
  "Macedonia / FYR        -> North Macedonia",
  "",
  "--- COUNTRY COVERAGE ---",
  paste("Neighbourhood countries defined:", length(neighbourhood_countries)),
  paste("Countries found in GED:", n_distinct(ged_filtered$country)),
  paste("Countries NOT found:", paste(not_found, collapse = ", ")),
  "",
  "--- FILTERED DATA ---",
  paste("Events after filter:", nrow(ged_filtered)),
  paste("Year range: 1992 to 2024"),
  "",
  "--- MONTHLY INDEX ---",
  paste("Total months:", nrow(regional_threat_index)),
  paste("Months with zero fatalities:",
        sum(regional_threat_index$total_fatalities == 0)),
  paste("Max fatalities (single month):",
        max(regional_threat_index$total_fatalities)),
  paste("Month of max:",
        format(regional_threat_index$month[
          which.max(regional_threat_index$total_fatalities)], "%Y-%m")),
  paste("Mean monthly fatalities:",
        round(mean(regional_threat_index$total_fatalities), 1)),
  "",
  "--- LOG SERIES SUMMARY ---",
  capture.output(summary(regional_threat_index$log_fatalities)),
  "",
  "--- CHANGEPOINTS ---",
  paste("Method: PELT with BIC penalty, minseglen = 24 months"),
  paste("Number of changepoints:", length(cpt_positions)),
  paste("Positions:", paste(cpt_positions, collapse = ", ")),
  paste("Corresponding dates:",
        paste(format(regional_threat_index$month[cpt_positions], "%Y-%m"),
              collapse = ", ")),
  "",
  "--- SECURITY REGIMES ---",
  paste("Number of regimes:", nrow(security_regimes)),
  capture.output(
    security_regimes %>%
      select(regime_id, period_start, period_end,
             mean_log_fatalities, total_fatalities) %>%
      print(n = Inf)
  ),
  "",
  "--- OUTPUT FILES ---",
  "data/processed/regional_threat_index.csv",
  "data/processed/security_regimes.csv",
  "",
  "--- FLAGS ---",
  if (length(not_found) > 0)
    paste("INFO: Countries not in GED (likely no conflict events):",
          paste(not_found, collapse = ", "))
  else "OK: All neighbourhood countries found in GED",
  if (nrow(security_regimes) > 8)
    paste("WARNING:", nrow(security_regimes),
          "regimes detected - consider increasing minseglen")
  else "OK: Number of regimes interpretable",
  if (sum(regional_threat_index$total_fatalities == 0) > 12)
    paste("INFO:", sum(regional_threat_index$total_fatalities == 0),
          "months with zero fatalities")
  else "OK: Zero months within expected range",
  "",
  "--- DECISION ---",
  "Review changepoint dates and regime descriptions.",
  "Manually assign period_description labels if needed.",
  "Proceed to 01b_threat_robustness.R"
)

writeLines(report, "report/01_threat_index_report.txt")
cat("\nReport written to report/01_threat_index_report.txt\n")
