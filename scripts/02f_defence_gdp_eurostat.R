# =============================================================================
# 02f_defence_gdp_eurostat.R
# Purpose: Compute defence expenditure as a share of GDP and as a share
#          of total government expenditure for EU-27 (2004-2024) using
#          Eurostat national-accounts data.
#
# Substantive role in the paper
# --------------------------------
# To complement the DTW-based analysis of *rhetorical* defence commitment
# (measured via Manifesto per104), we track *actual* defence spending.
# This addresses Reviewer 1's request for a defence % GDP series.
# The spending dimension also enters the secondary cluster analysis
# (script 04_clustering.R), where countries are grouped by how closely
# their rhetorical stance mirrors their defence budget trajectory.
#
# Data sources and COFOG definition
# -----------------------------------
# COFOG (Classification of the Functions of Government) code GF02 denotes
# "Defence" in the European System of Accounts.  We retrieve it from
# Eurostat's `gov_10a_exp` dataset:
#   - Sector S13  = General government (all levels combined)
#   - na_item TE  = Total expenditure (avoids double-counting of
#                   expenditure broken down by economic type)
#   - Unit MIO_EUR = Millions of euro at current prices
#
# Total government expenditure (denominator for % govt share) is taken
# from `gov_10a_main` (same na_item=TE, sector=S13 filter).  If that
# dataset is unavailable via the API, we fall back to the sum of all
# COFOG functions from `gov_10a_exp`.
#
# The GDP series (denominator for % GDP) is loaded from
# `data/processed/gdp_weights.csv` produced by script 02c.
#
# Eurostat note — Greece country code
# ------------------------------------
# Eurostat uses "EL" for Greece.  We recode to "GR" immediately after
# download so all subsequent joins use the standard ISO-2 code.
#
# Inputs
# ------
#   Eurostat API — fetched automatically (gov_10a_exp, gov_10a_main)
#   data/processed/gdp_weights.csv   (from script 02c)
#
# Outputs
# -------
#   data/processed/defence_gdp_share.csv
#       Per-country annual defence spending: absolute (M€), % GDP, % govt
#   data/processed/defence_eu_trends.csv
#       EU average defence spending trends (simple + GDP-weighted)
#   report/02f_defence_gdp_report.txt
#
# References
# ----------
# Eurostat (2025). Government expenditure by function — COFOG (gov_10a_exp).
#   https://ec.europa.eu/eurostat/databrowser/view/gov_10a_exp
# Eurostat (2025). Government revenue, expenditure and main aggregates
#   (gov_10a_main).
#   https://ec.europa.eu/eurostat/databrowser/view/gov_10a_main
# =============================================================================

if (!require("tidyverse")) install.packages("tidyverse")
if (!require("eurostat"))  install.packages("eurostat")

library(tidyverse)
library(eurostat)

# ── Country codes ──────────────────────────────────────────────────────────────
# Eurostat-specific codes (EL for Greece)
eu27_codes_eurostat <- c(
  "AT","BE","BG","HR","CY","CZ","DK","EE","FI","FR",
  "DE","EL","HU","IE","IT","LV","LT","LU","MT","NL",
  "PL","PT","RO","SK","SI","ES","SE"
)

iso2_to_name <- c(
  AT="Austria", BE="Belgium",  BG="Bulgaria",  HR="Croatia",
  CY="Cyprus",  CZ="Czech Republic", DK="Denmark", EE="Estonia",
  FI="Finland", FR="France",   DE="Germany",   GR="Greece",
  HU="Hungary", IE="Ireland",  IT="Italy",     LV="Latvia",
  LT="Lithuania", LU="Luxembourg", MT="Malta",  NL="Netherlands",
  PL="Poland",  PT="Portugal", RO="Romania",   SK="Slovakia",
  SI="Slovenia", ES="Spain",   SE="Sweden"
)

# ── Download COFOG GF02 defence expenditure ───────────────────────────────────
# na_item = "TE" (Total Expenditure) is critical: without this filter
# Eurostat returns expenditure broken down by economic type (wages,
# transfers, etc.), resulting in many duplicate country-year rows.
cat("Downloading COFOG defence expenditure from Eurostat (gov_10a_exp)...\n")

defence_raw <- get_eurostat(
  "gov_10a_exp",
  filters = list(
    cofog99 = "GF02",       # COFOG function 02 = Defence
    na_item = "TE",         # Total expenditure (avoids row duplication)
    unit    = "MIO_EUR",
    sector  = "S13",        # General government (all levels)
    geo     = eu27_codes_eurostat
  ),
  time_format = "num",
  cache       = TRUE
)

cat("Defence raw rows:", nrow(defence_raw), "\n")

# ── Download total government expenditure ─────────────────────────────────────
# Primary source: gov_10a_main (headline revenue/expenditure aggregates).
# Fallback: sum of all COFOG functions in gov_10a_exp (same filters minus
# the cofog99 argument).
cat("Downloading total government expenditure (gov_10a_main)...\n")

total_exp_raw <- tryCatch(
  get_eurostat(
    "gov_10a_main",
    filters = list(
      na_item = "TE",
      unit    = "MIO_EUR",
      sector  = "S13",
      geo     = eu27_codes_eurostat
    ),
    time_format = "num",
    cache       = TRUE
  ),
  error = function(e) {
    cat("  gov_10a_main failed:", conditionMessage(e), "\n")
    cat("  Falling back to gov_10a_exp with cofog99=GF01-GF10...\n")
    tryCatch(
      get_eurostat(
        "gov_10a_exp",
        filters = list(
          cofog99 = "GF01-GF10",   # all COFOG functions summed = total
          na_item = "TE",
          unit    = "MIO_EUR",
          sector  = "S13",
          geo     = eu27_codes_eurostat
        ),
        time_format = "num",
        cache       = TRUE
      ),
      error = function(e2) {
        cat("  Fallback also failed:", conditionMessage(e2), "\n")
        NULL
      }
    )
  }
)

cat("Total expenditure raw rows:",
    if (!is.null(total_exp_raw)) nrow(total_exp_raw) else 0, "\n")

# ── Process defence expenditure ───────────────────────────────────────────────
defence_clean <- defence_raw %>%
  select(geo, time, defence_meur = values) %>%
  rename(country_code = geo, year = time) %>%
  mutate(country_code = if_else(country_code == "EL", "GR", country_code)) %>%
  filter(year >= 2004, year <= 2024) %>%
  mutate(
    country_name = iso2_to_name[country_code],
    year         = as.integer(year)
  ) %>%
  filter(!is.na(defence_meur), !is.na(country_name))

cat("Defence clean rows:", nrow(defence_clean), "\n")

# ── Process total expenditure ─────────────────────────────────────────────────
if (!is.null(total_exp_raw) && nrow(total_exp_raw) > 0) {
  # Detect value column name (varies between Eurostat API versions)
  val_col <- if ("values" %in% names(total_exp_raw)) "values" else
             names(total_exp_raw)[ncol(total_exp_raw)]

  total_exp_clean <- total_exp_raw %>%
    select(geo, time, total_exp_meur = all_of(val_col)) %>%
    rename(country_code = geo, year = time) %>%
    mutate(country_code = if_else(country_code == "EL", "GR", country_code)) %>%
    filter(year >= 2004, year <= 2024) %>%
    mutate(year = as.integer(year)) %>%
    filter(!is.na(total_exp_meur))

  cat("Total exp clean rows:", nrow(total_exp_clean), "\n")
} else {
  cat("WARNING: Total expenditure data unavailable — defence_pct_govt will be NA\n")
  total_exp_clean <- tibble(
    country_code   = character(),
    year           = integer(),
    total_exp_meur = numeric()
  )
}

# ── Load GDP (produced by 02c_gdp_eurostat.R) ────────────────────────────────
gdp_data <- read_csv("data/processed/gdp_weights.csv",
                     show_col_types = FALSE) %>%
  select(country_code, year, gdp_meur)

# ── Compute spending shares ───────────────────────────────────────────────────
defence_shares <- defence_clean %>%
  left_join(total_exp_clean, by = c("country_code", "year")) %>%
  left_join(gdp_data,        by = c("country_code", "year")) %>%
  mutate(
    # defence_pct_govt: share of total government spending allocated to defence
    defence_pct_govt = (defence_meur / total_exp_meur) * 100,
    # defence_pct_gdp: defence budget as share of national output
    defence_pct_gdp  = (defence_meur / gdp_meur)       * 100
  )

cat("Defence shares rows:", nrow(defence_shares), "\n")

# Sanity check: the na_item=TE filter should prevent duplicate country-years
n_dupes <- defence_shares %>%
  group_by(country_code, year) %>%
  filter(n() > 1) %>%
  nrow()
cat("Duplicate country-year rows:", n_dupes,
    if (n_dupes == 0) "(OK)" else "(WARNING — check na_item filter)", "\n")

# ── GDP-weighted EU average defence trends ────────────────────────────────────
gdp_weights <- read_csv("data/processed/gdp_weights.csv",
                        show_col_types = FALSE) %>%
  select(country_code, year, gdp_weight)

defence_eu_trends <- defence_shares %>%
  left_join(gdp_weights, by = c("country_code", "year")) %>%
  group_by(year) %>%
  summarise(
    eu_avg_defence_pct_govt = mean(defence_pct_govt, na.rm = TRUE),
    eu_avg_defence_pct_gdp  = mean(defence_pct_gdp,  na.rm = TRUE),
    # GDP-weighted average: large-economy defence budgets get more weight
    eu_gdpw_defence_pct_gdp = weighted.mean(defence_pct_gdp,
                                             w     = gdp_weight,
                                             na.rm = TRUE),
    n_countries             = sum(!is.na(defence_pct_gdp)),
    .groups                 = "drop"
  )

# ── Coverage check ────────────────────────────────────────────────────────────
defence_coverage <- defence_shares %>%
  group_by(country_code, country_name) %>%
  summarise(
    n_years            = n(),
    n_missing_pct_govt = sum(is.na(defence_pct_govt)),
    n_missing_pct_gdp  = sum(is.na(defence_pct_gdp)),
    mean_pct_govt      = round(mean(defence_pct_govt, na.rm = TRUE), 2),
    mean_pct_gdp       = round(mean(defence_pct_gdp,  na.rm = TRUE), 2),
    .groups            = "drop"
  )

# ── Save outputs ──────────────────────────────────────────────────────────────
write_csv(defence_shares,    "data/processed/defence_gdp_share.csv")
write_csv(defence_eu_trends, "data/processed/defence_eu_trends.csv")

cat("Saved: data/processed/defence_gdp_share.csv\n")
cat("Saved: data/processed/defence_eu_trends.csv\n")

# ── Report ────────────────────────────────────────────────────────────────────
report <- c(
  "================================================",
  "REPORT: 02f_defence_gdp_eurostat.R",
  paste("Run at:", Sys.time()),
  "================================================",
  "",
  "--- METHODOLOGY NOTES ---",
  "Greece: Eurostat code 'EL' remapped to 'GR' after download",
  "Defence: cofog99=GF02 (COFOG Defence), na_item=TE (Total Expenditure)",
  "  The na_item=TE filter is essential: without it, expenditure by",
  "  economic type creates duplicate rows per country-year.",
  "",
  "--- EUROSTAT DOWNLOADS ---",
  paste("Defence (GF02, TE) rows:", nrow(defence_clean)),
  paste("Total expenditure rows:", nrow(total_exp_clean)),
  paste("Countries:", n_distinct(defence_clean$country_code), "/ 27 expected"),
  paste("Year range:", min(defence_clean$year), "to", max(defence_clean$year)),
  paste("Duplicate country-year rows:", n_dupes,
        if (n_dupes == 0) "— OK" else "— WARNING"),
  "",
  "--- COVERAGE BY COUNTRY ---",
  capture.output(print(defence_coverage, n = Inf)),
  "",
  "--- EU AVERAGE DEFENCE TRENDS ---",
  capture.output(
    defence_eu_trends %>%
      mutate(across(where(is.numeric), ~round(., 3))) %>%
      print(n = Inf)
  ),
  "",
  "--- HIGHEST DEFENCE SPENDERS % GDP (latest year) ---",
  capture.output(
    defence_shares %>%
      filter(year == max(year, na.rm = TRUE)) %>%
      arrange(desc(defence_pct_gdp)) %>%
      select(country_name, defence_pct_gdp, defence_pct_govt) %>%
      head(10) %>%
      mutate(across(where(is.numeric), ~round(., 2))) %>%
      print()
  ),
  "",
  "--- FLAGS ---",
  if (n_dupes > 0)
    paste("WARNING:", n_dupes,
          "duplicate country-year rows — na_item=TE filter may not have worked")
  else "OK: No duplicate country-year rows",
  if (nrow(total_exp_clean) == 0)
    "WARNING: Total expenditure data unavailable — defence_pct_govt is NA for all"
  else "OK: Total expenditure data available",
  if (any(defence_coverage$n_missing_pct_gdp > 3))
    paste("WARNING: Missing defence % GDP for:",
          paste(defence_coverage$country_name[
            defence_coverage$n_missing_pct_gdp > 3], collapse = ", "))
  else "OK: Defence % GDP coverage acceptable",
  if (n_distinct(defence_clean$country_code) < 27)
    paste("WARNING: Only", n_distinct(defence_clean$country_code),
          "/ 27 EU countries — missing:",
          paste(setdiff(c("AT","BE","BG","HR","CY","CZ","DK","EE","FI","FR",
                          "DE","GR","HU","IE","IT","LV","LT","LU","MT","NL",
                          "PL","PT","RO","SK","SI","ES","SE"),
                        unique(defence_clean$country_code)), collapse = ", "))
  else "OK: All 27 EU countries present",
  "",
  "--- OUTPUT FILES ---",
  "data/processed/defence_gdp_share.csv",
  "data/processed/defence_eu_trends.csv",
  "",
  "--- DECISION ---",
  "Use defence_pct_gdp series to add to Fig 4 (EU trends).",
  "Compare with SIPRI % GDP from 02e for robustness.",
  "Proceed to 03_dtw_metrics.R"
)

writeLines(report, "report/02f_defence_gdp_report.txt")
cat("\nReport written to report/02f_defence_gdp_report.txt\n")
