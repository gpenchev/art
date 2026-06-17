# =============================================================================
# 01b_threat_robustness.R
#
# Purpose:
#   Build three alternative variants of the Regional Threat Index for
#   robustness checks reported in the paper's online appendix:
#     1. Per-capita index  – fatalities normalised by neighbourhood population
#     2. Distance-weighted index – each country's fatalities weighted by
#        inverse great-circle distance from EU capitals (Bartik-style shift-share)
#     3. Country-specific index – per-EU-country distance-weighted exposure
#   Then computes correlations between all variants to assess robustness.
#
# Paper section: Appendix – "Robustness of Threat Measure"
#
# Data sources:
#   (A) Processed threat index – output of 01_threat_index.R (no download needed)
#   (B) Neighbourhood population – downloaded automatically from World Bank WDI API
#       WDI indicator: SP.POP.TOTL (total population)
#       No registration required; requires internet connection.
#   (C) GED data – see 01_threat_index.R header for download instructions
#       Save as: data/raw/ged251.csv
#
# Input:
#   data/processed/regional_threat_index.csv   (from 01_threat_index.R)
#   data/raw/ged251.csv                        (manual download – see above)
#   WDI API (SP.POP.TOTL)                      (downloaded automatically)
#
# Output:
#   data/processed/threat_index_variants.csv        – all three variants, monthly
#   data/processed/population_neighbourhood.csv     – WDI population by country-year
#   data/processed/distance_weights.csv             – inverse-distance weight matrix
#   data/processed/threat_index_country_specific.csv – per-EU-country monthly series
#   report/01b_threat_robustness_report.txt
#
# R packages required:
#   tidyverse, lubridate, WDI, geosphere, purrr
# =============================================================================

if (!require("tidyverse"))  install.packages("tidyverse")
if (!require("lubridate"))  install.packages("lubridate")
if (!require("WDI"))        install.packages("WDI")
if (!require("geosphere"))  install.packages("geosphere")

library(tidyverse)
library(lubridate)
library(WDI)         # World Bank API wrapper
library(geosphere)   # great-circle distances (Vincenty / distGeo)
library(purrr)

# ── 1. LOAD BASE THREAT INDEX ─────────────────────────────────────────────────
cat("Loading base threat index...\n")
rti <- read_csv("data/processed/regional_threat_index.csv",
                show_col_types = FALSE) %>%
  mutate(year = year(month))

# ── 2. NEIGHBOURHOOD COUNTRY CODES ───────────────────────────────────────────
# ISO2 codes for the same 40 neighbourhood countries used in 01_threat_index.R.
# Used to query WDI population data.

neighbourhood_iso2 <- c(
  "AL", "BA", "MK", "ME", "RS",            # Balkans
  "AM", "AZ", "BY", "GE", "MD", "UA",      # Eastern Partnership
  "DZ", "EG", "LY", "MA", "TN",            # North Africa
  "IL", "JO", "LB", "PS", "SY",            # Levant
  "IQ", "YE", "SA", "IR",                  # Middle East
  "RU", "TR", "AF", "PK",                  # Near neighbourhood & Central Asia
  "SD", "SS", "SO", "ML",                  # Sahel / Horn
  "NG", "CM", "TD", "NE",
  "ET", "ER", "DJ"
)

# ISO2 → ISO3 lookup for joining with GED data (which uses ISO3)
iso2_to_iso3 <- c(
  AL = "ALB", BA = "BIH", MK = "MKD", ME = "MNE", RS = "SRB",
  AM = "ARM", AZ = "AZE", BY = "BLR", GE = "GEO", MD = "MDA", UA = "UKR",
  DZ = "DZA", EG = "EGY", LY = "LBY", MA = "MAR", TN = "TUN",
  IL = "ISR", JO = "JOR", LB = "LBN", PS = "PSE", SY = "SYR",
  IQ = "IRQ", YE = "YEM", SA = "SAU", IR = "IRN",
  RU = "RUS", TR = "TUR", AF = "AFG", PK = "PAK",
  SD = "SDN", SS = "SSD", SO = "SOM", ML = "MLI",
  NG = "NGA", CM = "CMR", TD = "TCD", NE = "NER",
  ET = "ETH", ER = "ERI", DJ = "DJI"
)

# ── 3. DOWNLOAD NEIGHBOURHOOD POPULATION FROM WDI ────────────────────────────
# We query countries one-at-a-time to avoid API pagination failures.
# Sys.sleep(0.5) adds a 0.5-second pause between requests to stay within
# World Bank API rate limits and avoid connection errors on slow networks.

cat("Downloading neighbourhood population from WDI (one country at a time)...\n")

pop_raw <- purrr::map_dfr(seq_along(neighbourhood_iso2), function(i) {
  code <- neighbourhood_iso2[i]
  cat("  [", i, "/", length(neighbourhood_iso2), "] Downloading:", code, "\n")
  Sys.sleep(0.5)   # rate-limit: respect World Bank API
  tryCatch(
    WDI(
      indicator = "SP.POP.TOTL",  # WDI code: total population
      country   = code,
      start     = 1992,
      end       = 2024,
      extra     = FALSE
    ),
    error = function(e) {
      cat("    ERROR:", conditionMessage(e), "\n")
      NULL
    }
  )
})

population_neighbourhood <- pop_raw %>%
  as_tibble() %>%
  select(iso2c, country, year, population = SP.POP.TOTL) %>%
  filter(!is.na(population)) %>%
  mutate(iso3c = iso2_to_iso3[iso2c]) %>%
  arrange(country, year)

cat("Population data rows:", nrow(population_neighbourhood), "\n")
cat("Countries with population data:",
    n_distinct(population_neighbourhood$iso2c), "\n")

# Annual totals for the neighbourhood as a whole (denominator for per-capita)
annual_neighbourhood_pop <- population_neighbourhood %>%
  group_by(year) %>%
  summarise(
    total_neighbourhood_pop = sum(population, na.rm = TRUE),
    n_countries_pop         = n(),
    .groups = "drop"
  )

# ── 4. VARIANT 1: PER-CAPITA THREAT INDEX ─────────────────────────────────────
# Divides monthly fatalities by total neighbourhood population.
# Expressed as fatalities per million people to aid interpretation.
# Rationale: accounts for population growth in the neighbourhood over 1992-2024.

cat("Computing per-capita threat index...\n")

rti_annual <- rti %>%
  group_by(year) %>%
  summarise(
    annual_fatalities = sum(total_fatalities, na.rm = TRUE),
    .groups = "drop"
  )

per_capita_annual <- rti_annual %>%
  left_join(annual_neighbourhood_pop, by = "year") %>%
  mutate(
    fatalities_per_million = (annual_fatalities / total_neighbourhood_pop) * 1e6
  )

rti_with_percap <- rti %>%
  left_join(per_capita_annual %>%
              select(year, total_neighbourhood_pop, fatalities_per_million),
            by = "year") %>%
  mutate(
    monthly_per_million = (total_fatalities / total_neighbourhood_pop) * 1e6,
    log_per_million     = log(monthly_per_million + 1)  # log+1 to handle zeros
  )

# ── 5. VARIANT 2: DISTANCE-WEIGHTED THREAT INDEX ─────────────────────────────
# Weights each conflict country's fatalities by its inverse great-circle distance
# from EU capital cities. Countries closer to the EU receive higher weight.
#
# Rationale (Bartik shift-share logic):
#   The threat a given conflict country poses to EU member states is partly a
#   function of geographic proximity. A war in Ukraine is more salient to Poland
#   or Estonia than a war in Mali. The distance-weighted index lets EU-average
#   threat reflect this spatial heterogeneity.
#
# Distance calculation:
#   Uses distGeo() from {geosphere}: geodesic distance on the WGS-84 ellipsoid,
#   i.e. the shortest path over the Earth's surface (in metres, converted to km).
#
# Capital city coordinates: manually verified from standard geographic sources.

cat("Computing distance-weighted threat index...\n")

eu_capitals <- tribble(
  ~country_code, ~capital,           ~lat,     ~lon,
  "AUT", "Vienna",                   48.2092,  16.3728,
  "BEL", "Brussels",                 50.8503,   4.3517,
  "BGR", "Sofia",                    42.6977,  23.3219,
  "HRV", "Zagreb",                   45.8150,  15.9819,
  "CYP", "Nicosia",                  35.1856,  33.3823,
  "CZE", "Prague",                   50.0755,  14.4378,
  "DNK", "Copenhagen",               55.6761,  12.5683,
  "EST", "Tallinn",                  59.4370,  24.7536,
  "FIN", "Helsinki",                 60.1699,  24.9384,
  "FRA", "Paris",                    48.8566,   2.3522,
  "DEU", "Berlin",                   52.5200,  13.4050,
  "GRC", "Athens",                   37.9838,  23.7275,
  "HUN", "Budapest",                 47.4979,  19.0402,
  "IRL", "Dublin",                   53.3498,  -6.2603,
  "ITA", "Rome",                     41.9028,  12.4964,
  "LVA", "Riga",                     56.9460,  24.1059,
  "LTU", "Vilnius",                  54.6872,  25.2797,
  "LUX", "Luxembourg City",          49.6116,   6.1319,
  "MLT", "Valletta",                 35.8997,  14.5147,
  "NLD", "Amsterdam",                52.3676,   4.9041,
  "POL", "Warsaw",                   52.2297,  21.0122,
  "PRT", "Lisbon",                   38.7223,  -9.1393,
  "ROU", "Bucharest",                44.4268,  26.1025,
  "SVK", "Bratislava",               48.1486,  17.1077,
  "SVN", "Ljubljana",                46.0569,  14.5058,
  "ESP", "Madrid",                   40.4168,  -3.7038,
  "SWE", "Stockholm",                59.3293,  18.0686,
  "GBR", "London",                   51.5074,  -0.1278
)

# Geographic centroids of conflict countries (approximate country centres)
conflict_centroids <- tribble(
  ~country,             ~iso3c,  ~lat,     ~lon,
  "Albania",            "ALB",   41.1533,  20.1683,
  "Bosnia-Herzegovina", "BIH",   43.9159,  17.6791,
  "North Macedonia",    "MKD",   41.6086,  21.7453,
  "Montenegro",         "MNE",   42.7087,  19.3744,
  "Serbia",             "SRB",   44.0165,  21.0059,
  "Armenia",            "ARM",   40.0691,  45.0382,
  "Azerbaijan",         "AZE",   40.1431,  47.5769,
  "Belarus",            "BLR",   53.7098,  27.9534,
  "Georgia",            "GEO",   42.3154,  43.3569,
  "Moldova",            "MDA",   47.4116,  28.3699,
  "Ukraine",            "UKR",   48.3794,  31.1656,
  "Algeria",            "DZA",   28.0339,   1.6596,
  "Egypt",              "EGY",   26.8206,  30.8025,
  "Libya",              "LBY",   26.3351,  17.2283,
  "Morocco",            "MAR",   31.7917,  -7.0926,
  "Tunisia",            "TUN",   33.8869,   9.5375,
  "Israel",             "ISR",   31.0461,  34.8516,
  "Jordan",             "JOR",   30.5852,  36.2384,
  "Lebanon",            "LBN",   33.8547,  35.8623,
  "Palestine",          "PSE",   31.9522,  35.2332,
  "Syria",              "SYR",   34.8021,  38.9968,
  "Iraq",               "IRQ",   33.2232,  43.6793,
  "Yemen",              "YEM",   15.5527,  48.5164,
  "Saudi Arabia",       "SAU",   23.8859,  45.0792,
  "Iran",               "IRN",   32.4279,  53.6880,
  "Russia",             "RUS",   61.5240, 105.3188,
  "Turkey",             "TUR",   38.9637,  35.2433,
  "Afghanistan",        "AFG",   33.9391,  67.7100,
  "Pakistan",           "PAK",   30.3753,  69.3451,
  "Sudan",              "SDN",   12.8628,  30.2176,
  "South Sudan",        "SSD",    6.8770,  31.3070,
  "Somalia",            "SOM",    5.1521,  46.1996,
  "Mali",               "MLI",   17.5707,  -3.9962,
  "Nigeria",            "NGA",    9.0820,   8.6753,
  "Cameroon",           "CMR",    3.8480,  11.5021,
  "Chad",               "TCD",   15.4542,  18.7322,
  "Niger",              "NER",   17.6078,   8.0817,
  "Ethiopia",           "ETH",    9.1450,  40.4897,
  "Eritrea",            "ERI",   15.1794,  39.7823,
  "Djibouti",           "DJI",   11.8251,  42.5903
)

cat("Computing distance matrix",
    nrow(eu_capitals), "x", nrow(conflict_centroids), "...\n")

# Compute pairwise geodesic distances (km) between every EU capital
# and every conflict country centroid
dist_matrix <- matrix(NA,
                      nrow = nrow(eu_capitals),
                      ncol = nrow(conflict_centroids))

for (i in seq_len(nrow(eu_capitals))) {
  for (j in seq_len(nrow(conflict_centroids))) {
    dist_matrix[i, j] <- distGeo(
      c(eu_capitals$lon[i], eu_capitals$lat[i]),
      c(conflict_centroids$lon[j], conflict_centroids$lat[j])
    ) / 1000   # convert metres → kilometres
  }
}

rownames(dist_matrix) <- eu_capitals$country_code
colnames(dist_matrix) <- conflict_centroids$iso3c

# Inverse-distance weights: closer countries receive higher weight.
# Row-normalise so each EU country's weights sum to 1.
inv_dist_matrix <- 1 / dist_matrix
inv_dist_norm   <- inv_dist_matrix / rowSums(inv_dist_matrix)

distance_weights <- as_tibble(inv_dist_norm, rownames = "eu_country") %>%
  pivot_longer(-eu_country,
               names_to  = "conflict_country",
               values_to = "inv_dist_weight")

write_csv(distance_weights, "data/processed/distance_weights.csv")
cat("Saved: data/processed/distance_weights.csv\n")

# ── 6. RELOAD GED FOR COUNTRY-LEVEL MONTHLY SERIES ───────────────────────────
# We need per-country monthly fatalities to apply distance weights.
# The GED data must already be at data/raw/ged251.csv (see 01_threat_index.R).

cat("Loading GED for country-level aggregation...\n")
ged_raw <- read_csv("data/raw/ged251.csv", show_col_types = FALSE) %>%
  mutate(country = case_when(
    country == "Russia (Soviet Union)" ~ "Russia",
    country == "Serbia (Yugoslavia)"   ~ "Serbia",
    country == "Yemen (North Yemen)"   ~ "Yemen",
    TRUE ~ country
  ))

country_iso_map <- conflict_centroids %>%
  select(country, iso3c)

ged_monthly_country <- ged_raw %>%
  mutate(
    event_date = as_date(date_start),
    month      = floor_date(event_date, "month"),
    year       = year(event_date)
  ) %>%
  filter(year >= 1992, year <= 2024) %>%
  left_join(country_iso_map, by = "country") %>%
  filter(!is.na(iso3c)) %>%
  group_by(month, iso3c) %>%
  summarise(fatalities = sum(best, na.rm = TRUE), .groups = "drop")

# Expand to all month × country combinations so zeros are explicit
all_months <- tibble(
  month = seq(as_date("1992-01-01"), as_date("2024-12-01"), by = "month")
)
all_combos <- expand_grid(
  month = all_months$month,
  iso3c = conflict_centroids$iso3c
)

ged_complete <- all_combos %>%
  left_join(ged_monthly_country, by = c("month", "iso3c")) %>%
  mutate(fatalities = replace_na(fatalities, 0))

# ── 7. COMPUTE COUNTRY-SPECIFIC DISTANCE-WEIGHTED INDICES ────────────────────
# For each EU capital, compute a weighted sum of neighbourhood fatalities
# where weights are the row-normalised inverse distances computed above.
# This produces a monthly threat series specific to each EU member state.

cat("Computing country-specific distance-weighted threat indices...\n")

dist_weighted_list <- purrr::map(eu_capitals$country_code, function(eu_cc) {
  weights <- inv_dist_norm[eu_cc, ]
  ged_complete %>%
    mutate(weight = unname(weights[as.character(iso3c)])) %>%
    group_by(month) %>%
    summarise(
      dist_weighted_fatalities = sum(fatalities * weight, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      eu_country        = eu_cc,
      log_dist_weighted = log(dist_weighted_fatalities + 1)
    )
})

dist_weighted_all <- bind_rows(dist_weighted_list)

# EU-average of the distance-weighted series (for use as a single EU index)
dist_weighted_eu_avg <- dist_weighted_all %>%
  group_by(month) %>%
  summarise(
    dist_weighted_eu_avg     = mean(dist_weighted_fatalities, na.rm = TRUE),
    log_dist_weighted_eu_avg = mean(log_dist_weighted, na.rm = TRUE),
    .groups = "drop"
  )

# ── 8. COMBINE ALL VARIANTS ───────────────────────────────────────────────────
# Normalise each variant to [0, 1] for direct comparison.
# Min-max normalisation: (x - min) / (max - min).

threat_variants <- rti %>%
  select(month, year,
         raw_fatalities = total_fatalities,
         log_fatalities) %>%
  left_join(
    rti_with_percap %>% select(month, monthly_per_million, log_per_million),
    by = "month"
  ) %>%
  left_join(dist_weighted_eu_avg, by = "month") %>%
  mutate(
    raw_norm    = (raw_fatalities - min(raw_fatalities)) /
                  (max(raw_fatalities) - min(raw_fatalities)),
    log_norm    = (log_fatalities - min(log_fatalities)) /
                  (max(log_fatalities) - min(log_fatalities)),
    percap_norm = (monthly_per_million - min(monthly_per_million, na.rm = TRUE)) /
                  (max(monthly_per_million, na.rm = TRUE) -
                   min(monthly_per_million, na.rm = TRUE)),
    dist_norm   = (dist_weighted_eu_avg - min(dist_weighted_eu_avg, na.rm = TRUE)) /
                  (max(dist_weighted_eu_avg, na.rm = TRUE) -
                   min(dist_weighted_eu_avg, na.rm = TRUE))
  )

# ── 9. SAVE OUTPUTS ───────────────────────────────────────────────────────────
write_csv(threat_variants,          "data/processed/threat_index_variants.csv")
write_csv(population_neighbourhood, "data/processed/population_neighbourhood.csv")
write_csv(dist_weighted_all,        "data/processed/threat_index_country_specific.csv")

cat("Saved: data/processed/threat_index_variants.csv\n")
cat("Saved: data/processed/population_neighbourhood.csv\n")
cat("Saved: data/processed/threat_index_country_specific.csv\n")

# ── 10. CORRELATION BETWEEN VARIANTS ─────────────────────────────────────────
# High correlations (> 0.8) indicate results are robust across measurement
# choices. Divergence suggests the variants capture different phenomena.

cor_data <- threat_variants %>%
  select(log_fatalities, log_per_million, log_dist_weighted_eu_avg) %>%
  filter(complete.cases(.))

cor_matrix <- cor(cor_data, use = "complete.obs")

# ── 11. WRITE REPORT ──────────────────────────────────────────────────────────
dir.create("report", showWarnings = FALSE)

report <- c(
  "================================================",
  "REPORT: 01b_threat_robustness.R",
  paste("Run at:", Sys.time()),
  "================================================",
  "",
  "--- BASE INDEX ---",
  paste("Months:", nrow(rti)),
  paste("Year range: 1992-2024"),
  "",
  "--- NEIGHBOURHOOD POPULATION ---",
  paste("Countries with WDI population data:",
        n_distinct(population_neighbourhood$iso2c)),
  paste("Year range:",
        min(population_neighbourhood$year), "to",
        max(population_neighbourhood$year)),
  paste("Countries missing population data:",
        paste(neighbourhood_iso2[
          !neighbourhood_iso2 %in% unique(population_neighbourhood$iso2c)],
          collapse = ", ")),
  "",
  "--- DISTANCE MATRIX ---",
  paste("EU countries:", nrow(eu_capitals)),
  paste("Conflict countries:", nrow(conflict_centroids)),
  paste("Distance range (km):",
        round(min(dist_matrix), 0), "to",
        round(max(dist_matrix), 0)),
  "",
  "--- CLOSEST CONFLICT COUNTRIES TO EU CENTROID ---",
  capture.output(
    tibble(
      conflict_country = conflict_centroids$country,
      avg_dist_to_eu   = colMeans(dist_matrix)
    ) %>%
      arrange(avg_dist_to_eu) %>%
      head(10) %>%
      print()
  ),
  "",
  "--- CORRELATION BETWEEN INDEX VARIANTS ---",
  capture.output(round(cor_matrix, 3)),
  "",
  "--- VARIANT SUMMARIES ---",
  "Log fatalities (main index):",
  capture.output(summary(threat_variants$log_fatalities)),
  "Per million (per-capita variant):",
  capture.output(summary(threat_variants$log_per_million)),
  "Distance-weighted (EU average):",
  capture.output(summary(threat_variants$log_dist_weighted_eu_avg)),
  "",
  "--- FLAGS ---",
  if (any(cor_matrix[lower.tri(cor_matrix)] < 0.7))
    "WARNING: Some variants have correlation < 0.7 - results may not be robust"
  else "OK: All variants highly correlated (> 0.7)",
  if (any(is.na(threat_variants$log_per_million)))
    paste("WARNING: Missing per-capita values:",
          sum(is.na(threat_variants$log_per_million)))
  else "OK: No missing per-capita values",
  "",
  "--- OUTPUT FILES ---",
  "data/processed/threat_index_variants.csv",
  "data/processed/population_neighbourhood.csv",
  "data/processed/distance_weights.csv",
  "data/processed/threat_index_country_specific.csv",
  "",
  "--- DECISION ---",
  "Review correlation matrix.",
  "If all variants correlated > 0.8: main results are robust.",
  "If variants diverge: note in paper which countries are sensitive.",
  "Proceed to 01c_gpr_comparison.R"
)

writeLines(report, "report/01b_threat_robustness_report.txt")
cat("\nReport written to report/01b_threat_robustness_report.txt\n")
