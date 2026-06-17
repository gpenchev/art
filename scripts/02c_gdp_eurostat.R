# =============================================================================
# 02c_gdp_eurostat.R
# Purpose: Download EU-27 GDP from Eurostat, compute annual GDP weights,
#          and produce simple-average vs GDP-weighted EU trend series for
#          government and opposition defence stance.
#
# Substantive role in the paper
# --------------------------------
# The paper reports EU-average trends in pro-military rhetoric (Fig. 4).
# An unweighted average treats Malta (pop. ~0.5 M) identically to Germany
# (pop. ~84 M).  GDP-weighted averages give larger economies proportionally
# more influence, which better reflects the political centre of gravity of
# European defence policy.  This script checks whether the two averages
# diverge materially; if they do, the weighted version is preferred.
#
# Eurostat note — Greece country code
# ------------------------------------
# Eurostat uses "EL" for Greece (following its Greek-language name
# "Ellas"), not the ISO 3166-1 alpha-2 code "GR" used by the rest of the
# pipeline.  We download with the Eurostat code "EL" and immediately
# recode to "GR" so that subsequent joins work correctly.
#
# Data source
# -----------
# Eurostat API, dataset "nama_10_gdp":
#   Indicator : B1GQ  (Gross Domestic Product at market prices)
#   Unit      : CP_MEUR (current prices, millions of euro)
#   Downloaded automatically via the {eurostat} R package.
#   No manual download required.
#
# Inputs
# ------
#   Eurostat API — fetched automatically
#   data/processed/stance_time_series.csv
#
# Outputs
# -------
#   data/processed/gdp_weights.csv
#       Annual GDP and within-year share (gdp_weight) per country
#   data/processed/eu_trends_gdp_weighted.csv
#       Side-by-side simple-average and GDP-weighted EU trend series
#   report/02c_gdp_eurostat_report.txt
#
# References
# ----------
# Eurostat (2025). National accounts at current prices (nama_10_gdp).
#   https://ec.europa.eu/eurostat/databrowser/view/nama_10_gdp
# =============================================================================

if (!require("tidyverse")) install.packages("tidyverse")
if (!require("eurostat"))  install.packages("eurostat")

library(tidyverse)
library(eurostat)

# ── Country code lists ─────────────────────────────────────────────────────────
# Eurostat-specific codes for the API query (EL, not GR for Greece)
eu27_codes_eurostat <- c(
  "AT","BE","BG","HR","CY","CZ","DK","EE","FI","FR",
  "DE","EL","HU","IE","IT","LV","LT","LU","MT","NL",
  "PL","PT","RO","SK","SI","ES","SE"
)

# ISO-2 → country name lookup (using post-recode "GR" for Greece)
iso2_to_name <- c(
  AT="Austria", BE="Belgium",  BG="Bulgaria",  HR="Croatia",
  CY="Cyprus",  CZ="Czech Republic", DK="Denmark", EE="Estonia",
  FI="Finland", FR="France",   DE="Germany",   GR="Greece",
  HU="Hungary", IE="Ireland",  IT="Italy",     LV="Latvia",
  LT="Lithuania", LU="Luxembourg", MT="Malta",  NL="Netherlands",
  PL="Poland",  PT="Portugal", RO="Romania",   SK="Slovakia",
  SI="Slovenia", ES="Spain",   SE="Sweden"
)

# ── Download GDP from Eurostat ────────────────────────────────────────────────
cat("Downloading GDP data from Eurostat (nama_10_gdp)...\n")
cat("This uses the {eurostat} package API — no manual download needed.\n")

# get_eurostat() caches the result locally; subsequent runs are fast.
# B1GQ = GDP at market prices; CP_MEUR = current prices in million euro.
gdp_raw <- get_eurostat(
  "nama_10_gdp",
  filters = list(
    na_item = "B1GQ",
    unit    = "CP_MEUR",
    geo     = eu27_codes_eurostat
  ),
  time_format = "num",
  cache       = TRUE
)

cat("GDP raw rows:", nrow(gdp_raw), "\n")

# ── Clean and recode ──────────────────────────────────────────────────────────
gdp_clean <- gdp_raw %>%
  select(geo, time, gdp_meur = values) %>%
  rename(country_code = geo, year = time) %>%
  # Recode Eurostat "EL" → ISO "GR" so joins with rest of pipeline work
  mutate(country_code = if_else(country_code == "EL", "GR", country_code)) %>%
  filter(year >= 2004, year <= 2024) %>%
  mutate(
    country_name = iso2_to_name[country_code],
    year         = as.integer(year)
  ) %>%
  filter(!is.na(gdp_meur))

cat("GDP clean rows:", nrow(gdp_clean), "\n")
cat("Countries:", n_distinct(gdp_clean$country_code), "\n")
cat("Year range:", min(gdp_clean$year), "to", max(gdp_clean$year), "\n")

# ── Coverage checks ───────────────────────────────────────────────────────────
eu27_codes_standard <- c(
  "AT","BE","BG","HR","CY","CZ","DK","EE","FI","FR",
  "DE","GR","HU","IE","IT","LV","LT","LU","MT","NL",
  "PL","PT","RO","SK","SI","ES","SE"
)

missing_countries <- eu27_codes_standard[
  !eu27_codes_standard %in% unique(gdp_clean$country_code)
]
cat("Missing countries:", paste(missing_countries, collapse = ", "), "\n")

missing_years <- gdp_clean %>%
  group_by(country_code) %>%
  summarise(
    n_years   = n(),
    min_year  = min(year),
    max_year  = max(year),
    n_missing = sum(is.na(gdp_meur)),
    .groups   = "drop"
  ) %>%
  filter(n_years < 21 | n_missing > 0)

# ── Compute GDP weights ───────────────────────────────────────────────────────
# Within each year, a country's GDP weight = its GDP / total EU-27 GDP.
# These row-normalised weights sum to 1 in each year and allow
# GDP-weighted averages across countries for any variable.
gdp_weights <- gdp_clean %>%
  group_by(year) %>%
  mutate(
    total_eu_gdp = sum(gdp_meur, na.rm = TRUE),
    gdp_weight   = gdp_meur / total_eu_gdp
  ) %>%
  ungroup() %>%
  select(country_code, country_name, year, gdp_meur,
         total_eu_gdp, gdp_weight)

# ── Compute simple vs GDP-weighted EU average defence stance ──────────────────
stance <- read_csv("data/processed/stance_time_series.csv",
                   show_col_types = FALSE)

# Build a name → ISO-2 reverse lookup
name_to_iso2 <- setNames(names(iso2_to_name), iso2_to_name)

stance_with_gdp <- stance %>%
  mutate(country_code = name_to_iso2[country]) %>%
  filter(!is.na(country_code)) %>%
  left_join(gdp_weights %>% select(country_code, year, gdp_weight),
            by = c("country_code", "year"))

# Simple (unweighted) EU average
simple_avg <- stance_with_gdp %>%
  group_by(year) %>%
  summarise(
    gov_stance_simple = mean(gov_stance_locf, na.rm = TRUE),
    opp_stance_simple = mean(opp_stance_locf, na.rm = TRUE),
    n_countries       = sum(!is.na(gov_stance_locf)),
    .groups           = "drop"
  )

# GDP-weighted EU average
gdp_weighted_avg <- stance_with_gdp %>%
  filter(!is.na(gdp_weight)) %>%
  group_by(year) %>%
  summarise(
    gov_stance_gdp_weighted = weighted.mean(gov_stance_locf,
                                             w    = gdp_weight,
                                             na.rm = TRUE),
    opp_stance_gdp_weighted = weighted.mean(opp_stance_locf,
                                             w    = gdp_weight,
                                             na.rm = TRUE),
    n_countries_weighted    = sum(!is.na(gov_stance_locf) &
                                    !is.na(gdp_weight)),
    .groups = "drop"
  )

eu_trends_gdp_weighted <- simple_avg %>%
  left_join(gdp_weighted_avg, by = "year")

# Pearson correlation between the two series — a correlation close to 1
# indicates GDP weighting has negligible substantive effect.
cor_gov <- cor(eu_trends_gdp_weighted$gov_stance_simple,
               eu_trends_gdp_weighted$gov_stance_gdp_weighted,
               use = "complete.obs")
cor_opp <- cor(eu_trends_gdp_weighted$opp_stance_simple,
               eu_trends_gdp_weighted$opp_stance_gdp_weighted,
               use = "complete.obs")

cat("Correlation simple vs GDP-weighted (gov):", round(cor_gov, 3), "\n")
cat("Correlation simple vs GDP-weighted (opp):", round(cor_opp, 3), "\n")

# Top 10 GDP contributors in most recent year (for report)
latest_year <- max(gdp_weights$year)
top_weights <- gdp_weights %>%
  filter(year == latest_year) %>%
  arrange(desc(gdp_weight)) %>%
  head(10)

# ── Save outputs ──────────────────────────────────────────────────────────────
write_csv(gdp_weights,            "data/processed/gdp_weights.csv")
write_csv(eu_trends_gdp_weighted, "data/processed/eu_trends_gdp_weighted.csv")

cat("Saved: data/processed/gdp_weights.csv\n")
cat("Saved: data/processed/eu_trends_gdp_weighted.csv\n")

# ── Report ────────────────────────────────────────────────────────────────────
report <- c(
  "================================================",
  "REPORT: 02c_gdp_eurostat.R",
  paste("Run at:", Sys.time()),
  "================================================",
  "",
  "--- EUROSTAT GDP DOWNLOAD ---",
  "NOTE: Eurostat uses 'EL' for Greece; recoded to 'GR' after download",
  paste("Raw rows:", nrow(gdp_raw)),
  paste("Clean rows:", nrow(gdp_clean)),
  paste("Countries:", n_distinct(gdp_clean$country_code), "/ 27 expected"),
  paste("Year range:", min(gdp_clean$year), "to", max(gdp_clean$year)),
  "",
  "--- MISSING COUNTRIES ---",
  if (length(missing_countries) > 0)
    paste("Missing:", paste(missing_countries, collapse = ", "))
  else "OK: All 27 EU countries present",
  "",
  "--- COVERAGE ISSUES ---",
  if (nrow(missing_years) > 0)
    capture.output(print(missing_years))
  else "OK: All countries have complete year coverage",
  "",
  "--- GDP WEIGHTS (latest year) ---",
  paste("Year:", latest_year),
  capture.output(
    print(top_weights %>%
            select(country_code, country_name, gdp_weight) %>%
            mutate(gdp_weight = round(gdp_weight, 4)))
  ),
  "",
  "--- SIMPLE vs GDP-WEIGHTED COMPARISON ---",
  paste("Correlation gov stance (simple vs weighted):", round(cor_gov, 3)),
  paste("Correlation opp stance (simple vs weighted):", round(cor_opp, 3)),
  "",
  capture.output(
    eu_trends_gdp_weighted %>%
      mutate(
        gov_diff = round(gov_stance_gdp_weighted - gov_stance_simple, 4),
        opp_diff = round(opp_stance_gdp_weighted - opp_stance_simple, 4)
      ) %>%
      select(year, gov_diff, opp_diff) %>%
      print(n = Inf)
  ),
  "",
  "--- FLAGS ---",
  if (length(missing_countries) > 0)
    paste("WARNING: Missing countries will have zero weight in GDP average:",
          paste(missing_countries, collapse = ", "))
  else "OK: No missing countries",
  if (cor_gov < 0.95)
    paste("INFO: GDP weighting materially changes gov stance series",
          "(cor =", round(cor_gov, 3), ") — consider reporting")
  else "OK: GDP weighting has minimal effect on gov stance",
  if (cor_opp < 0.95)
    paste("INFO: GDP weighting materially changes opp stance series",
          "(cor =", round(cor_opp, 3), ") — consider reporting")
  else "OK: GDP weighting has minimal effect on opp stance",
  "",
  "--- OUTPUT FILES ---",
  "data/processed/gdp_weights.csv",
  "data/processed/eu_trends_gdp_weighted.csv",
  "",
  "--- DECISION ---",
  "If correlation < 0.95: update Fig 4 with the GDP-weighted series.",
  "If correlation > 0.95: note in paper that weighting has minimal effect.",
  "Proceed to 02d_population.R"
)

writeLines(report, "report/02c_gdp_eurostat_report.txt")
cat("\nReport written to report/02c_gdp_eurostat_report.txt\n")
