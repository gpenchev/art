# =============================================================================
# 02e_sipri_wdi.R
# Purpose: Download SIPRI military expenditure data for EU28 + UK via
#          the World Bank WDI API, and compare with the Eurostat COFOG
#          series produced in script 02f.
#
# Substantive role in the paper
# --------------------------------
# The paper's spending analysis uses Eurostat COFOG functional expenditure
# data (script 02f).  SIPRI is the most widely-cited independent source of
# military spending.  This script downloads the SIPRI series re-distributed
# by the World Bank and cross-validates it against COFOG; high correlation
# confirms that the two sources agree and that results are not artefacts of
# the particular data source choice.
#
# WDI indicators used
# --------------------
# MS.MIL.XPND.GD.ZS  Military expenditure (% of GDP)
# MS.MIL.XPND.ZS     Military expenditure (% of central government expenditure)
#
# Both are taken from SIPRI's Military Expenditure Database and served
# through the World Bank's public API.  See:
#   https://data.worldbank.org/indicator/MS.MIL.XPND.GD.ZS
#   https://data.worldbank.org/indicator/MS.MIL.XPND.ZS
#
# Rate-limiting the WDI API
# --------------------------
# Countries are downloaded one at a time with a 0.5-second pause between
# requests (`Sys.sleep(0.5)`).  Batch queries for 28 countries in a single
# call frequently trigger HTTP 429 (Too Many Requests) errors; the
# sequential loop avoids this reliably.
#
# Inputs
# ------
#   World Bank WDI API — fetched automatically
#   data/processed/defence_gdp_share.csv   (from script 02f, if available)
#
# Outputs
# -------
#   data/processed/sipri_spending.csv          Per-country annual SIPRI series
#   data/processed/sipri_eu_average.csv        EU average SIPRI series
#   data/processed/sipri_cofog_comparison.csv  SIPRI vs COFOG merged table
#   report/02e_sipri_wdi_report.txt
#
# References
# ----------
# SIPRI (2025). SIPRI Military Expenditure Database.
#   https://www.sipri.org/databases/milex
# World Bank (2025). World Development Indicators.
#   https://databank.worldbank.org/source/world-development-indicators
# =============================================================================

if (!require("tidyverse")) install.packages("tidyverse")
if (!require("WDI"))       install.packages("WDI")

library(tidyverse)
library(WDI)

# ── ISO code lookup tables ────────────────────────────────────────────────────
# WDI returns ISO-2 codes; we map back to ISO-3 and country names for
# consistency with the rest of the pipeline (which uses ISO-3 internally).
iso3_to_iso2 <- c(
  AUT="AT", BEL="BE", BGR="BG", HRV="HR", CYP="CY", CZE="CZ",
  DNK="DK", EST="EE", FIN="FI", FRA="FR", DEU="DE", GRC="GR",
  HUN="HU", IRL="IE", ITA="IT", LVA="LV", LTU="LT", LUX="LU",
  MLT="MT", NLD="NL", POL="PL", PRT="PT", ROU="RO", SVK="SK",
  SVN="SI", ESP="ES", SWE="SE", GBR="GB"
)

iso3_to_name <- c(
  AUT="Austria",       BEL="Belgium",       BGR="Bulgaria",
  HRV="Croatia",       CYP="Cyprus",        CZE="Czech Republic",
  DNK="Denmark",       EST="Estonia",       FIN="Finland",
  FRA="France",        DEU="Germany",       GRC="Greece",
  HUN="Hungary",       IRL="Ireland",       ITA="Italy",
  LVA="Latvia",        LTU="Lithuania",     LUX="Luxembourg",
  MLT="Malta",         NLD="Netherlands",   POL="Poland",
  PRT="Portugal",      ROU="Romania",       SVK="Slovakia",
  SVN="Slovenia",      ESP="Spain",         SWE="Sweden",
  GBR="United Kingdom"
)

# Name lookup (ISO-2 → country name), used for COFOG join below
iso2_to_name <- c(
  AT="Austria",  BE="Belgium",  BG="Bulgaria",  HR="Croatia",
  CY="Cyprus",   CZ="Czech Republic", DK="Denmark", EE="Estonia",
  FI="Finland",  FR="France",   DE="Germany",   GR="Greece",
  HU="Hungary",  IE="Ireland",  IT="Italy",     LV="Latvia",
  LT="Lithuania", LU="Luxembourg", MT="Malta",  NL="Netherlands",
  PL="Poland",   PT="Portugal", RO="Romania",   SK="Slovakia",
  SI="Slovenia", ES="Spain",    SE="Sweden",    GB="United Kingdom"
)

iso2_codes <- as.character(iso3_to_iso2)  # 28 two-letter codes

# ── Download SIPRI data via WDI ───────────────────────────────────────────────
cat("Downloading SIPRI data one country at a time to avoid API rate limits...\n")

sipri_raw <- purrr::map_dfr(seq_along(iso2_codes), function(i) {
  code <- iso2_codes[i]
  cat("  [", i, "/", length(iso2_codes), "] Downloading:", code, "\n")
  Sys.sleep(0.5)   # 0.5 s pause: avoids WDI HTTP 429 rate-limit errors
  tryCatch(
    WDI(
      indicator = c("MS.MIL.XPND.GD.ZS", "MS.MIL.XPND.ZS"),
      country   = code,
      start     = 2004,
      end       = 2024,
      extra     = FALSE
    ),
    error = function(e) {
      cat("    ERROR:", conditionMessage(e), "\n")
      NULL
    }
  )
})

cat("SIPRI raw rows:", nrow(sipri_raw), "\n")

# ── Clean and map to standard codes ──────────────────────────────────────────
sipri_clean <- sipri_raw %>%
  as_tibble() %>%
  select(
    iso2c,
    year,
    sipri_pct_gdp  = MS.MIL.XPND.GD.ZS,   # % of GDP
    sipri_pct_govt = MS.MIL.XPND.ZS        # % of central government spending
  ) %>%
  mutate(
    iso3c        = names(iso3_to_iso2)[match(iso2c, iso3_to_iso2)],
    country_name = iso3_to_name[iso3c],
    country_iso2 = iso2c
  ) %>%
  filter(!is.na(country_name)) %>%
  arrange(country_name, year)

cat("SIPRI clean rows:", nrow(sipri_clean), "\n")
cat("Countries:", n_distinct(sipri_clean$iso3c), "\n")

missing_iso3 <- setdiff(names(iso3_to_iso2), unique(sipri_clean$iso3c))
if (length(missing_iso3) > 0) {
  cat("Missing countries:", paste(missing_iso3, collapse = ", "), "\n")
}

sipri_coverage <- sipri_clean %>%
  group_by(iso3c, country_name) %>%
  summarise(
    n_years        = n(),
    n_missing_gdp  = sum(is.na(sipri_pct_gdp)),
    n_missing_govt = sum(is.na(sipri_pct_govt)),
    mean_pct_gdp   = round(mean(sipri_pct_gdp,  na.rm = TRUE), 2),
    mean_pct_govt  = round(mean(sipri_pct_govt, na.rm = TRUE), 2),
    .groups        = "drop"
  ) %>%
  arrange(desc(n_missing_gdp))

# ── Cross-validate against Eurostat COFOG (if available) ─────────────────────
# Script 02f must have run before this section is meaningful; the file
# is optional and the script proceeds gracefully if absent.
cofog_file <- "data/processed/defence_gdp_share.csv"
if (file.exists(cofog_file)) {
  cofog_data <- read_csv(cofog_file, show_col_types = FALSE) %>%
    select(country_code, year, defence_pct_govt) %>%
    filter(!is.na(defence_pct_govt)) %>%
    mutate(country_name = iso2_to_name[country_code]) %>%
    filter(!is.na(country_name)) %>%
    distinct()
  cat("COFOG data rows:", nrow(cofog_data), "\n")
} else {
  cat("Note: defence_gdp_share.csv not found — skipping COFOG comparison\n")
  cofog_data <- NULL
}

cor_sipri_cofog <- NA
sipri_cofog     <- sipri_clean

if (!is.null(cofog_data) && nrow(cofog_data) > 0) {
  sipri_cofog <- sipri_clean %>%
    left_join(
      cofog_data %>% select(country_name, year,
                             cofog_pct_govt = defence_pct_govt),
      by = c("country_name", "year")
    )

  n_complete <- sum(!is.na(sipri_cofog$sipri_pct_govt) &
                      !is.na(sipri_cofog$cofog_pct_govt))
  cat("Complete pairs for SIPRI vs COFOG correlation:", n_complete, "\n")

  if (n_complete >= 3) {
    cor_sipri_cofog <- tryCatch(
      cor(sipri_cofog$sipri_pct_govt,
          sipri_cofog$cofog_pct_govt,
          use = "complete.obs"),
      error = function(e) {
        cat("    WARN: cor() failed:", conditionMessage(e), "\n")
        NA_real_
      }
    )
    cat("Correlation SIPRI % govt vs COFOG % govt:",
        round(cor_sipri_cofog, 3), "\n")
  } else {
    cat("WARN: Insufficient complete pairs (", n_complete,
        ") for correlation — skipping\n")
  }

  write_csv(sipri_cofog, "data/processed/sipri_cofog_comparison.csv")
  cat("Saved: data/processed/sipri_cofog_comparison.csv\n")
}

# ── EU average SIPRI series ───────────────────────────────────────────────────
sipri_eu_avg <- sipri_clean %>%
  group_by(year) %>%
  summarise(
    eu_avg_pct_gdp  = mean(sipri_pct_gdp,  na.rm = TRUE),
    eu_avg_pct_govt = mean(sipri_pct_govt, na.rm = TRUE),
    n_countries     = sum(!is.na(sipri_pct_gdp)),
    .groups         = "drop"
  )

# ── Save outputs ──────────────────────────────────────────────────────────────
write_csv(sipri_clean,  "data/processed/sipri_spending.csv")
write_csv(sipri_eu_avg, "data/processed/sipri_eu_average.csv")

cat("Saved: data/processed/sipri_spending.csv\n")
cat("Saved: data/processed/sipri_eu_average.csv\n")

# ── Report ────────────────────────────────────────────────────────────────────
report <- c(
  "================================================",
  "REPORT: 02e_sipri_wdi.R",
  paste("Run at:", Sys.time()),
  "================================================",
  "",
  "--- WDI DOWNLOAD ---",
  "Indicators: MS.MIL.XPND.GD.ZS (% GDP), MS.MIL.XPND.ZS (% govt exp)",
  "Source: SIPRI data re-distributed via World Bank WDI API",
  paste("Raw rows:", nrow(sipri_raw)),
  paste("Clean rows:", nrow(sipri_clean)),
  paste("Countries:", n_distinct(sipri_clean$iso3c), "/ 28 expected"),
  paste("Year range:", min(sipri_clean$year), "to", max(sipri_clean$year)),
  if (length(missing_iso3) > 0)
    paste("Missing ISO3 codes:", paste(missing_iso3, collapse = ", "))
  else "OK: All 28 countries downloaded",
  "",
  "--- COVERAGE BY COUNTRY ---",
  capture.output(print(sipri_coverage, n = Inf)),
  "",
  "--- EU AVERAGE SIPRI SERIES (selected years) ---",
  capture.output(
    sipri_eu_avg %>%
      filter(year %in% c(2004, 2008, 2014, 2018, 2022, 2024)) %>%
      mutate(across(where(is.numeric), ~round(., 2))) %>%
      print()
  ),
  "",
  "--- SIPRI vs COFOG COMPARISON ---",
  if (!is.na(cor_sipri_cofog))
    paste("Correlation SIPRI % govt vs COFOG % govt:",
          round(cor_sipri_cofog, 3))
  else "COFOG comparison not available or insufficient complete pairs",
  "",
  "--- HIGHEST MILITARY SPENDERS % GDP (latest year) ---",
  capture.output(
    sipri_clean %>%
      filter(year == max(year)) %>%
      arrange(desc(sipri_pct_gdp)) %>%
      select(country_name, sipri_pct_gdp, sipri_pct_govt) %>%
      head(10) %>%
      print()
  ),
  "",
  "--- FLAGS ---",
  if (n_distinct(sipri_clean$iso3c) < 28)
    paste("WARNING: Only", n_distinct(sipri_clean$iso3c),
          "/ 28 countries downloaded — missing:",
          paste(missing_iso3, collapse = ", "))
  else "OK: All 28 countries downloaded",
  if (any(sipri_coverage$n_missing_gdp > 5))
    paste("WARNING: High missing SIPRI % GDP for:",
          paste(sipri_coverage$country_name[
            sipri_coverage$n_missing_gdp > 5], collapse = ", "))
  else "OK: SIPRI % GDP coverage acceptable",
  if (!is.na(cor_sipri_cofog) && cor_sipri_cofog < 0.7)
    paste("WARNING: Low correlation SIPRI vs COFOG:",
          round(cor_sipri_cofog, 3),
          "— methodological difference is significant; discuss in paper")
  else if (!is.na(cor_sipri_cofog))
    paste("OK: SIPRI and COFOG well-correlated at", round(cor_sipri_cofog, 3))
  else "INFO: COFOG correlation could not be computed",
  "",
  "--- OUTPUT FILES ---",
  "data/processed/sipri_spending.csv",
  "data/processed/sipri_eu_average.csv",
  "data/processed/sipri_cofog_comparison.csv (if COFOG data available)",
  "",
  "--- DECISION ---",
  "If SIPRI vs COFOG correlation > 0.8: state results robust to source choice.",
  "If correlation < 0.8: discuss methodological difference in Section 4.",
  "Proceed to 02f_defence_gdp_eurostat.R"
)

writeLines(report, "report/02e_sipri_wdi_report.txt")
cat("\nReport written to report/02e_sipri_wdi_report.txt\n")
