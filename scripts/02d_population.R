# =============================================================================
# 02d_population.R
# Purpose: Download population data for EU-27 (from Eurostat) and for
#          the conflict-affected neighbourhood countries (from World Bank
#          WDI).  The combined dataset is used by script 01b to normalise
#          conflict fatalities to a per-million-population basis, providing
#          a size-adjusted robustness variant of the Regional Threat Index.
#
# Why two sources?
# ----------------
# Eurostat provides harmonised, high-quality population figures for the 27
# EU member states back to the early 2000s.  Neighbourhood countries
# (conflict zones in Eastern Europe, the Middle East, the Sahel, etc.) are
# outside Eurostat's remit; the World Bank WDI `SP.POP.TOTL` indicator
# covers them from the early 1990s onward.
#
# Rate-limiting the WDI API
# --------------------------
# The WDI package's `WDI()` function is called one country at a time with
# a 0.5-second pause (`Sys.sleep(0.5)`) between requests.  Batch queries
# for many countries at once can trigger HTTP 429 (Too Many Requests)
# errors from the World Bank API; the per-country loop prevents this.
#
# Data sources
# ------------
# EU population — Eurostat, dataset "demo_pjan":
#   Downloaded automatically via the {eurostat} R package.
#
# Neighbourhood population — World Bank WDI:
#   Indicator: SP.POP.TOTL (Population, total)
#   Downloaded automatically via the {WDI} R package.
#   No manual download required for either source.
#
# Inputs
# ------
#   Eurostat API — fetched automatically
#   World Bank WDI API — fetched automatically
#
# Outputs
# -------
#   data/processed/population_eu.csv
#   data/processed/population_neighbourhood.csv
#   data/processed/population_combined.csv
#   data/processed/annual_neighbourhood_pop.csv
#       Total annual population across all neighbourhood countries
#       (used for per-capita normalisation in script 01b)
#   report/02d_population_report.txt
#
# References
# ----------
# Eurostat (2025). Population on 1 January (demo_pjan).
#   https://ec.europa.eu/eurostat/databrowser/view/demo_pjan
# World Bank (2025). Population, total (SP.POP.TOTL). World Development
#   Indicators. https://data.worldbank.org/indicator/SP.POP.TOTL
# =============================================================================

if (!require("tidyverse")) install.packages("tidyverse")
if (!require("eurostat"))  install.packages("eurostat")
if (!require("WDI"))       install.packages("WDI")

library(tidyverse)
library(eurostat)
library(WDI)

# ── Reference tables ──────────────────────────────────────────────────────────
eu27_codes <- c(
  "AT","BE","BG","HR","CY","CZ","DK","EE","FI","FR",
  "DE","GR","HU","IE","IT","LV","LT","LU","MT","NL",
  "PL","PT","RO","SK","SI","ES","SE"
)

iso2_to_name <- c(
  AT="Austria",    BE="Belgium",         BG="Bulgaria",  HR="Croatia",
  CY="Cyprus",     CZ="Czech Republic",  DK="Denmark",   EE="Estonia",
  FI="Finland",    FR="France",          DE="Germany",   GR="Greece",
  HU="Hungary",    IE="Ireland",         IT="Italy",     LV="Latvia",
  LT="Lithuania",  LU="Luxembourg",      MT="Malta",     NL="Netherlands",
  PL="Poland",     PT="Portugal",        RO="Romania",   SK="Slovakia",
  SI="Slovenia",   ES="Spain",           SE="Sweden"
)

# ── 1. EU POPULATION FROM EUROSTAT ───────────────────────────────────────────
cat("Downloading EU population from Eurostat (demo_pjan)...\n")

# demo_pjan = Population on 1 January; age=TOTAL, sex=T (both sexes combined)
pop_eu_raw <- get_eurostat(
  "demo_pjan",
  filters = list(
    age = "TOTAL",
    sex = "T",
    geo = eu27_codes
  ),
  time_format = "num",
  cache       = TRUE
)

cat("EU population raw rows:", nrow(pop_eu_raw), "\n")

population_eu <- pop_eu_raw %>%
  select(geo, time, population = values) %>%
  rename(country_code = geo, year = time) %>%
  filter(year >= 2004, year <= 2024) %>%
  mutate(
    country_name = iso2_to_name[country_code],
    year         = as.integer(year),
    source       = "Eurostat"
  ) %>%
  filter(!is.na(population))

cat("EU population clean rows:", nrow(population_eu), "\n")
cat("Countries:", n_distinct(population_eu$country_code), "\n")

# ── 2. NEIGHBOURHOOD POPULATION FROM WDI ─────────────────────────────────────
# The neighbourhood country list mirrors the UCDP conflict zones used in
# script 01_threat_index.R: Western Balkans, Eastern Partnership,
# Middle East & North Africa, Gulf, Russia/Caucasus, Afghanistan/Pakistan,
# Sub-Saharan conflict states.
cat("Downloading neighbourhood population from WDI (one country at a time)...\n")

neighbourhood_iso2 <- c(
  # Western Balkans
  "AL", "BA", "MK", "ME", "RS",
  # Eastern neighbourhood
  "AM", "AZ", "BY", "GE", "MD", "UA",
  # North Africa
  "DZ", "EG", "LY", "MA", "TN",
  # Levant / Middle East
  "IL", "JO", "LB", "PS", "SY",
  # Gulf / broader Middle East
  "IQ", "YE", "SA", "IR",
  # Russia & Central Asia gateway
  "RU", "TR", "AF", "PK",
  # Sub-Saharan Africa
  "SD", "SS", "SO", "ML",
  "NG", "CM", "TD", "NE",
  "ET", "ER", "DJ"
)

# ISO-2 → ISO-3 mapping needed to store country_code consistently
iso2_to_iso3 <- c(
  AL="ALB", BA="BIH", MK="MKD", ME="MNE", RS="SRB",
  AM="ARM", AZ="AZE", BY="BLR", GE="GEO", MD="MDA", UA="UKR",
  DZ="DZA", EG="EGY", LY="LBY", MA="MAR", TN="TUN",
  IL="ISR", JO="JOR", LB="LBN", PS="PSE", SY="SYR",
  IQ="IRQ", YE="YEM", SA="SAU", IR="IRN",
  RU="RUS", TR="TUR", AF="AFG", PK="PAK",
  SD="SDN", SS="SSD", SO="SOM", ML="MLI",
  NG="NGA", CM="CMR", TD="TCD", NE="NER",
  ET="ETH", ER="ERI", DJ="DJI"
)

# Loop one country at a time; Sys.sleep(0.5) avoids API rate-limit errors.
pop_neighbourhood_raw <- purrr::map_dfr(seq_along(neighbourhood_iso2), function(i) {
  code <- neighbourhood_iso2[i]
  cat("  [", i, "/", length(neighbourhood_iso2), "] Downloading:", code, "\n")
  Sys.sleep(0.5)   # rate-limit: 0.5 s pause between WDI API calls
  tryCatch(
    WDI(
      indicator = "SP.POP.TOTL",
      country   = code,
      start     = 1992,   # pre-2004 data needed for PELT changepoint baseline
      end       = 2024,
      extra     = FALSE
    ),
    error = function(e) {
      cat("    ERROR:", conditionMessage(e), "\n")
      NULL
    }
  )
})

population_neighbourhood <- pop_neighbourhood_raw %>%
  as_tibble() %>%
  select(iso2c, country, year, population = SP.POP.TOTL) %>%
  filter(!is.na(population)) %>%
  mutate(
    iso3c  = iso2_to_iso3[iso2c],
    source = "WDI"
  ) %>%
  arrange(country, year)

cat("Neighbourhood population rows:", nrow(population_neighbourhood), "\n")
cat("Countries:", n_distinct(population_neighbourhood$iso2c), "\n")

# ── 3. ANNUAL TOTAL NEIGHBOURHOOD POPULATION ──────────────────────────────────
# Summing across all neighbourhood countries gives a total denominator for
# per-capita fatality calculation in script 01b (robustness variant 2).
annual_neighbourhood_pop <- population_neighbourhood %>%
  group_by(year) %>%
  summarise(
    total_neighbourhood_pop = sum(population, na.rm = TRUE),
    n_countries             = n(),
    .groups                 = "drop"
  )

# ── 4. COMBINED DATASET ───────────────────────────────────────────────────────
population_combined <- bind_rows(
  population_eu %>%
    select(country_code, country_name, year, population, source),
  population_neighbourhood %>%
    rename(country_code = iso3c, country_name = country) %>%
    select(country_code, country_name, year, population, source)
)

# ── Coverage checks ───────────────────────────────────────────────────────────
eu_coverage <- population_eu %>%
  group_by(country_code) %>%
  summarise(
    n_years   = n(),
    min_year  = min(year),
    max_year  = max(year),
    n_missing = sum(is.na(population)),
    .groups   = "drop"
  )

neighbourhood_coverage <- population_neighbourhood %>%
  group_by(iso2c, country) %>%
  summarise(
    n_years   = n(),
    min_year  = min(year),
    max_year  = max(year),
    n_missing = sum(is.na(population)),
    .groups   = "drop"
  )

missing_neighbourhood <- neighbourhood_iso2[
  !neighbourhood_iso2 %in% unique(population_neighbourhood$iso2c)
]

# ── Save outputs ──────────────────────────────────────────────────────────────
write_csv(population_eu,            "data/processed/population_eu.csv")
write_csv(population_neighbourhood, "data/processed/population_neighbourhood.csv")
write_csv(population_combined,      "data/processed/population_combined.csv")
write_csv(annual_neighbourhood_pop, "data/processed/annual_neighbourhood_pop.csv")

cat("Saved: data/processed/population_eu.csv\n")
cat("Saved: data/processed/population_neighbourhood.csv\n")
cat("Saved: data/processed/population_combined.csv\n")
cat("Saved: data/processed/annual_neighbourhood_pop.csv\n")

# ── Report ────────────────────────────────────────────────────────────────────
report <- c(
  "================================================",
  "REPORT: 02d_population.R",
  paste("Run at:", Sys.time()),
  "================================================",
  "",
  "--- EU POPULATION (EUROSTAT) ---",
  paste("Countries:", n_distinct(population_eu$country_code), "/ 27 expected"),
  paste("Year range:", min(population_eu$year), "to", max(population_eu$year)),
  paste("Missing values:", sum(is.na(population_eu$population))),
  "",
  "--- EU COVERAGE ISSUES ---",
  if (any(eu_coverage$n_years < 21))
    capture.output(print(eu_coverage %>% filter(n_years < 21)))
  else "OK: All EU countries have complete 2004-2024 coverage",
  "",
  "--- NEIGHBOURHOOD POPULATION (WDI) ---",
  paste("Countries requested:", length(neighbourhood_iso2)),
  paste("Countries found:", n_distinct(population_neighbourhood$iso2c)),
  paste("Year range:", min(population_neighbourhood$year),
        "to", max(population_neighbourhood$year)),
  "",
  "--- MISSING NEIGHBOURHOOD COUNTRIES ---",
  if (length(missing_neighbourhood) > 0)
    paste("Missing:", paste(missing_neighbourhood, collapse = ", "))
  else "OK: All neighbourhood countries found",
  "",
  "--- ANNUAL NEIGHBOURHOOD POPULATION (selected years) ---",
  capture.output(
    annual_neighbourhood_pop %>%
      filter(year %in% c(1992, 2000, 2010, 2014, 2022, 2024)) %>%
      mutate(total_millions = round(total_neighbourhood_pop / 1e6, 1)) %>%
      select(year, total_millions, n_countries) %>%
      print()
  ),
  "",
  "--- FLAGS ---",
  if (length(missing_neighbourhood) > 0)
    paste("WARNING: Missing neighbourhood countries will have zero weight",
          "in per-capita index:", paste(missing_neighbourhood, collapse = ", "))
  else "OK: No missing neighbourhood countries",
  if (any(eu_coverage$n_missing > 0))
    "WARNING: Some EU population values are missing"
  else "OK: No missing EU population values",
  "",
  "--- OUTPUT FILES ---",
  "data/processed/population_eu.csv",
  "data/processed/population_neighbourhood.csv",
  "data/processed/population_combined.csv",
  "data/processed/annual_neighbourhood_pop.csv",
  "",
  "--- DECISION ---",
  "Review missing neighbourhood countries above.",
  "Countries absent from WDI will have zero weight in per-capita threat index.",
  "Proceed to 02e_sipri_wdi.R"
)

writeLines(report, "report/02d_population_report.txt")
cat("\nReport written to report/02d_population_report.txt\n")
