# =============================================================================
# 06_app_data.R
# Purpose: Read all pipeline outputs and reshape them into a single
#          app_data.rda file consumed by the Shiny application.
#
# This script must be run AFTER the full pipeline (run_all.R) completes.
# It reads from data/processed/ and writes to app/data/app_data.rda.
#
# Design principles
# -----------------
# • All joins, type conversions, normalisations and PCA computations are
#   done here ONCE — not at Shiny reactive time.
# • The .rda contains named R objects loaded by app/global.R via load().
# • Column types are set explicitly (Date, factor, numeric) so the app
#   never needs to coerce types at runtime.
# • Country lists are pre-sorted vectors used to populate selectizeInputs.
#
# Run with:
#   setwd("path/to/prepare")
#   source("scripts/06_app_data.R")
# =============================================================================

library(tidyverse)
library(lubridate)

cat("=== 06_app_data.R: Building app data ===\n\n")

# ── Helper: min-max normalise to [0, 1] ───────────────────────────────────────
norm01 <- function(x) {
  rng <- max(x, na.rm = TRUE) - min(x, na.rm = TRUE)
  if (rng == 0) return(rep(0, length(x)))
  (x - min(x, na.rm = TRUE)) / rng
}

# =============================================================================
# 1. THREAT INDEX
# =============================================================================
cat("Loading threat data...\n")

rti_raw <- read_csv("data/processed/regional_threat_index.csv",
                    show_col_types = FALSE) %>%
  mutate(month = as_date(month))

regimes_raw <- read_csv("data/processed/security_regimes.csv",
                        show_col_types = FALSE) %>%
  mutate(start_date = as_date(start_date),
         end_date   = as_date(end_date),
         # Alternating fill for regime band shading
         fill_col   = if_else(row_number() %% 2 == 0, "#f0f0f0", "#ffffff"),
         # Human-readable label with key events
         label = case_when(
           regime_id == 1  ~ "Post-Cold War conflicts\n(1992–1995)",
           regime_id == 2  ~ "Post-Dayton lull\n(1995–1998)",
           regime_id == 3  ~ "Kosovo & Caucasus\n(1998–2000)",
           regime_id == 4  ~ "Relative stability\n(2000–2006)",
           regime_id == 5  ~ "Africa & Middle East\n(2006–2008)",
           regime_id == 6  ~ "Georgia & Arab tensions\n(2008–2011)",
           regime_id == 7  ~ "Arab Spring & Syria\n(2012–2016)",
           regime_id == 8  ~ "Syria peak & Sahel\n(2016–2018)",
           regime_id == 9  ~ "Ebbing conflicts\n(2018–2020)",
           regime_id == 10 ~ "Ethiopia & Ukraine onset\n(2020–2022)",
           regime_id == 11 ~ "Ukraine war\n(2022–2024)",
           TRUE ~ paste("Regime", regime_id)
         )
  )

# Join regime_id to each monthly row
threat_eu <- rti_raw %>%
  mutate(
    regime_id = map_int(month, function(m) {
      idx <- which(regimes_raw$start_date <= m & regimes_raw$end_date >= m)
      if (length(idx) == 0L) NA_integer_ else regimes_raw$regime_id[idx[1]]
    }),
    norm_fat  = norm01(total_fatalities),
    norm_log  = norm01(log_fatalities)
  )

regimes <- regimes_raw

# Key conflict events (from export_figures.R — hardcoded)
key_events <- tribble(
  ~label_text,              ~start_date,    ~end_date,      ~label_date,   ~label_row,
  # label_date = where the annotation text box is placed on the x-axis
  # label_row  = 1 (top, y=0.97) or 2 (lower, y=0.84) to avoid overlap
  "Bosnian War\n(1992–95)", "1992-04-06",   "1995-12-14",   "1993-10-01",  1L,
  "Kosovo War\n(1998–99)",  "1998-02-28",   "1999-06-11",   "1998-09-01",  1L,
  "Libyan War\n(2011)",     "2011-02-15",   "2011-10-23",   "2011-05-01",  1L,
  "Syrian War\n(2011– )",   "2011-03-15",   "2025-01-01",   "2013-06-01",  2L,  # staggered low: starts same year as Libya
  "Crimea\n(2014)",         "2014-02-20",   "2025-01-01",   "2014-06-01",  1L,  # fixed: was wrongly at 2018-01-01
  "Ukraine\n(2022– )",      "2022-02-24",   "2025-01-01",   "2022-09-01",  1L
) %>%
  mutate(across(c(start_date, end_date, label_date), as_date))

# Country-specific distance-weighted threat
threat_country_raw <- read_csv("data/processed/threat_index_country_specific.csv",
                               show_col_types = FALSE) %>%
  mutate(month = as_date(month))

# ISO3 → country name lookup
iso3_to_name <- c(
  AUT="Austria",       BEL="Belgium",       BGR="Bulgaria",      CYP="Cyprus",
  CZE="Czech Republic",DEU="Germany",        DNK="Denmark",       ESP="Spain",
  EST="Estonia",       FIN="Finland",        FRA="France",        GBR="United Kingdom",
  GRC="Greece",        HRV="Croatia",        HUN="Hungary",       IRL="Ireland",
  ITA="Italy",         LTU="Lithuania",      LUX="Luxembourg",    LVA="Latvia",
  MLT="Malta",         NLD="Netherlands",    POL="Poland",        PRT="Portugal",
  ROU="Romania",       SVK="Slovakia",       SVN="Slovenia",      SWE="Sweden"
)

threat_country <- threat_country_raw %>%
  mutate(country = iso3_to_name[eu_country]) %>%
  filter(!is.na(country)) %>%
  group_by(eu_country, country) %>%
  mutate(norm_dist = norm01(dist_weighted_fatalities)) %>%
  ungroup()

# GPR vs UCDP comparison (optional — graceful fallback if absent)
gpr_file <- "data/processed/gpr_ucdp_comparison.csv"
if (file.exists(gpr_file)) {
  gpr_comparison <- read_csv(gpr_file, show_col_types = FALSE) %>%
    mutate(month = as_date(month)) %>%
    select(month, ucdp_norm, gpr_dist_norm)
  has_gpr <- TRUE
  cat("  GPR comparison data loaded\n")
} else {
  gpr_comparison <- tibble(month = as_date(character()),
                           ucdp_norm = numeric(), gpr_dist_norm = numeric())
  has_gpr <- FALSE
  cat("  GPR comparison data not found — GPR toggle will be disabled\n")
}

cat("  threat_eu:", nrow(threat_eu), "rows\n")
cat("  threat_country:", nrow(threat_country), "rows\n")

# =============================================================================
# 2. STANCE TIME SERIES
# =============================================================================
cat("Loading stance data...\n")

stance <- read_csv("data/processed/stance_time_series.csv",
                   show_col_types = FALSE) %>%
  mutate(year = as.integer(year))

cabinet_lr <- read_csv("data/processed/cabinet_rightleft.csv",
                       show_col_types = FALSE) %>%
  mutate(year             = as.integer(year),
         is_right_leaning = as.logical(is_right_leaning))

cat("  stance:", nrow(stance), "rows,", n_distinct(stance$country), "countries\n")

# =============================================================================
# 3. EU-LEVEL TREND SERIES  (Fig 4 and Fig 6 data)
# =============================================================================
cat("Building EU trend series...\n")

# Annual EU-average threat (for normalised overlay)
rti_annual <- threat_eu %>%
  mutate(year = year(month)) %>%
  group_by(year) %>%
  summarise(log_threat = mean(log_fatalities, na.rm = TRUE), .groups = "drop")

# Simple EU average stance (all 27 countries, unweighted)
eu_simple <- stance %>%
  group_by(year) %>%
  summarise(
    gov_simple = mean(gov_stance_locf, na.rm = TRUE),
    opp_simple = mean(opp_stance_locf, na.rm = TRUE),
    .groups    = "drop"
  )

gdp_w      <- read_csv("data/processed/eu_trends_gdp_weighted.csv",
                       show_col_types = FALSE) %>%
  mutate(year = as.integer(year))

def_trends <- read_csv("data/processed/defence_eu_trends.csv",
                       show_col_types = FALSE) %>%
  mutate(year = as.integer(year))

sipri_avg  <- read_csv("data/processed/sipri_eu_average.csv",
                       show_col_types = FALSE) %>%
  mutate(year = as.integer(year))

# Fig 4 data: stance vs threat (all normalised 0-1)
eu_trends <- eu_simple %>%
  left_join(gdp_w %>% select(year,
                              gov_gdpw = gov_stance_gdp_weighted,
                              opp_gdpw = opp_stance_gdp_weighted),
            by = "year") %>%
  left_join(def_trends %>% select(year, def_pct_gdp = eu_avg_defence_pct_gdp),
            by = "year") %>%
  left_join(rti_annual, by = "year") %>%
  filter(year >= 2004, year <= 2024) %>%
  mutate(
    gov_norm        = norm01(gov_simple),
    opp_norm        = norm01(opp_simple),
    gov_gdpw_norm   = norm01(gov_gdpw),
    opp_gdpw_norm   = norm01(opp_gdpw),
    def_norm        = norm01(def_pct_gdp),
    threat_norm     = norm01(log_threat)
  )

# Fig 6 data: stance vs spending (COFOG + SIPRI, all normalised)
eu_spending_trends <- eu_simple %>%
  left_join(gdp_w %>% select(year,
                              gov_gdpw = gov_stance_gdp_weighted,
                              opp_gdpw = opp_stance_gdp_weighted),
            by = "year") %>%
  left_join(def_trends %>% select(year, def_pct_gdp = eu_avg_defence_pct_gdp),
            by = "year") %>%
  left_join(sipri_avg %>% select(year, sipri_pct_gdp = eu_avg_pct_gdp),
            by = "year") %>%
  filter(year >= 2004, year <= 2024) %>%
  mutate(
    gov_norm      = norm01(gov_simple),
    opp_norm      = norm01(opp_simple),
    gov_gdpw_norm = norm01(gov_gdpw),
    opp_gdpw_norm = norm01(opp_gdpw),
    def_norm      = norm01(def_pct_gdp),
    sipri_norm    = norm01(sipri_pct_gdp)
  )

# Key years for event marker lines
event_years <- tibble(
  year  = c(2011, 2014, 2022),
  label = c("Arab Spring", "Crimea", "Ukraine")
)

cat("  eu_trends:", nrow(eu_trends), "rows\n")

# =============================================================================
# 4. DTW METRICS + CLUSTER ASSIGNMENTS
# =============================================================================
cat("Loading DTW and cluster data...\n")

dtw_threat_raw  <- read_csv("data/processed/dtw_threat_metrics.csv",
                             show_col_types = FALSE)
dtw_spend_raw   <- read_csv("data/processed/dtw_spending_metrics.csv",
                             show_col_types = FALSE)
final_table     <- read_csv("data/processed/final_comparison_table.csv",
                             show_col_types = FALSE)
cluster_rob     <- read_csv("data/processed/cluster_robustness.csv",
                             show_col_types = FALSE)

# Build full 28-country DTW table
# 22 have full cluster assignments; 6 have gov_responsiveness only
dtw_all <- dtw_threat_raw %>%
  left_join(dtw_spend_raw  %>% select(country, gov_spending_similarity,
                                       opp_spending_similarity),
            by = "country") %>%
  left_join(final_table    %>% select(country, cluster_threat, cluster,
                                       threat_cluster_type, spending_cluster_type),
            by = "country") %>%
  left_join(cluster_rob    %>% select(country, is_stable, pct_stable),
            by = "country") %>%
  mutate(
    threat_cluster_type  = factor(threat_cluster_type,
                                   levels = c("Polarised Reactors", "Disengaged",
                                              "Quiet Reactors",
                                              "Vocal but Unresponsive")),
    spending_cluster_type = factor(spending_cluster_type,
                                    levels = c("Stable Allocators",
                                               "Policy Converters")),
    is_clustered = !is.na(debate_intensity)  # FALSE for the 6 partial countries
  )

# Robustness data
dtw_robustness <- read_csv("data/processed/dtw_metrics_robustness.csv",
                            show_col_types = FALSE) %>%
  mutate(variant_label = case_when(
    variant == "log_norm"    ~ "Log fatalities (main)",
    variant == "percap_norm" ~ "Per-capita fatalities",
    variant == "dist_norm"   ~ "Distance-weighted",
    TRUE ~ variant
  ))

robustness_cors <- read_csv("data/processed/dtw_robustness_correlations.csv",
                             show_col_types = FALSE) %>%
  mutate(
    variant_label = case_when(
      variant == "percap_norm" ~ "Per-capita",
      variant == "dist_norm"   ~ "Distance-weighted",
      TRUE ~ variant
    ),
    metric_label = case_when(
      metric == "debate_intensity"   ~ "Debate intensity",
      metric == "gov_responsiveness" ~ "Gov. responsiveness",
      metric == "opp_responsiveness" ~ "Opp. responsiveness",
      TRUE ~ metric
    )
  )

k_sil <- read_csv("data/processed/k_silhouette_scores.csv",
                  show_col_types = FALSE)

cat("  dtw_all:", nrow(dtw_all), "rows\n")

# =============================================================================
# 5. PCA FOR CLUSTER SCATTER PLOTS
# =============================================================================
cat("Computing PCA...\n")

# Threat cluster PCA (22 countries with full DTW data)
dtw_threat_pca <- dtw_all %>%
  filter(is_clustered) %>%
  select(country, debate_intensity, gov_responsiveness, opp_responsiveness,
         threat_cluster_type)

pca_threat_mat  <- dtw_threat_pca %>%
  select(debate_intensity, gov_responsiveness, opp_responsiveness) %>%
  scale()

pca_threat_fit  <- prcomp(pca_threat_mat, scale. = FALSE)
pca_var_threat  <- round(100 * pca_threat_fit$sdev^2 /
                           sum(pca_threat_fit$sdev^2), 1)

pca_threat <- dtw_threat_pca %>%
  mutate(PC1 = pca_threat_fit$x[, 1],
         PC2 = pca_threat_fit$x[, 2])

# Spending cluster PCA (21 countries — excl. UK which has no COFOG)
dtw_spend_pca <- dtw_all %>%
  filter(!is.na(gov_spending_similarity), !is.na(spending_cluster_type)) %>%
  select(country, gov_spending_similarity, opp_spending_similarity,
         spending_cluster_type)

pca_spend_mat  <- dtw_spend_pca %>%
  select(gov_spending_similarity, opp_spending_similarity) %>%
  scale()

pca_spend_fit  <- prcomp(pca_spend_mat, scale. = FALSE)
pca_var_spend  <- round(100 * pca_spend_fit$sdev^2 /
                          sum(pca_spend_fit$sdev^2), 1)

pca_spending <- dtw_spend_pca %>%
  mutate(PC1 = pca_spend_fit$x[, 1],
         PC2 = pca_spend_fit$x[, 2])

cat("  PCA threat: PC1 explains", pca_var_threat[1], "%\n")
cat("  PCA spending: PC1 explains", pca_var_spend[1], "%\n")

# =============================================================================
# 6. COUNTRY SPENDING SERIES
# =============================================================================
cat("Loading spending data...\n")

cofog <- read_csv("data/processed/defence_gdp_share.csv",
                  show_col_types = FALSE) %>%
  mutate(year   = as.integer(year),
         source = "COFOG (Eurostat)") %>%
  select(country = country_name, year,
         pct_gdp  = defence_pct_gdp,
         pct_govt = defence_pct_govt,
         source)

sipri <- read_csv("data/processed/sipri_spending.csv",
                  show_col_types = FALSE) %>%
  mutate(year   = as.integer(year),
         source = "SIPRI (WDI)") %>%
  select(country = country_name, year,
         pct_gdp  = sipri_pct_gdp,
         pct_govt = sipri_pct_govt,
         source)

# Stack and join cluster label
spending_country <- bind_rows(cofog, sipri) %>%
  left_join(dtw_all %>% select(country, threat_cluster_type,
                                spending_cluster_type),
            by = "country") %>%
  filter(!is.na(country))

spending_eu <- def_trends %>%
  left_join(sipri_avg %>% select(year, sipri_avg_pct_gdp = eu_avg_pct_gdp),
            by = "year") %>%
  select(year,
         cofog_pct_gdp  = eu_avg_defence_pct_gdp,
         cofog_pct_govt = eu_avg_defence_pct_govt,
         sipri_pct_gdp  = sipri_avg_pct_gdp)

cat("  spending_country:", nrow(spending_country), "rows\n")

# =============================================================================
# 7. COLOUR PALETTE + COUNTRY LISTS
# =============================================================================
cluster_palette <- c(
  "Polarised Reactors"     = "#1a6faf",
  "Disengaged"             = "#e07b00",
  "Quiet Reactors"         = "#2a9d2a",
  "Vocal but Unresponsive" = "#c0392b",
  "Stable Allocators"      = "#555555",
  "Policy Converters"      = "#aaaaaa",
  "No data"                = "#e8e8e8"
)

country_list_28 <- sort(unique(stance$country))
country_list_22 <- sort(dtw_all$country[dtw_all$is_clustered])

# Grouped list for selectizeInput optgroup (countries grouped by threat cluster)
country_groups <- dtw_all %>%
  filter(is_clustered) %>%
  arrange(threat_cluster_type, country) %>%
  select(country, threat_cluster_type) %>%
  mutate(threat_cluster_type = as.character(threat_cluster_type))

cat("  country_list_28:", length(country_list_28), "countries\n")
cat("  country_list_22:", length(country_list_22), "countries\n")

# =============================================================================
# 8. SAVE
# =============================================================================
out_path <- "app/data/app_data.rda"
dir.create(dirname(out_path), showWarnings = FALSE, recursive = TRUE)

save(
  # Threat
  threat_eu, regimes, key_events, threat_country, gpr_comparison, has_gpr,
  # Stance
  stance, cabinet_lr, eu_trends, eu_spending_trends, event_years,
  # DTW
  dtw_all, dtw_robustness, robustness_cors, k_sil,
  # PCA
  pca_threat, pca_spending, pca_var_threat, pca_var_spend,
  # Spending
  spending_country, spending_eu,
  # Lookups
  cluster_palette, country_list_28, country_list_22, country_groups,
  iso3_to_name,
  file = out_path,
  compress = "xz"   # xz gives best size reduction for tabular data
)

size_kb <- round(file.size(out_path) / 1024, 1)
cat("\nSaved:", out_path, "(", size_kb, "KB)\n")
cat("\nObjects in app_data.rda:\n")
cat(" threat_eu, regimes, key_events, threat_country, gpr_comparison, has_gpr\n")
cat(" stance, cabinet_lr, eu_trends, eu_spending_trends, event_years\n")
cat(" dtw_all, dtw_robustness, robustness_cors, k_sil\n")
cat(" pca_threat, pca_spending, pca_var_threat, pca_var_spend\n")
cat(" spending_country, spending_eu\n")
cat(" cluster_palette, country_list_28, country_list_22, country_groups\n")
cat(" iso3_to_name\n")
cat("\n=== 06_app_data.R complete ===\n")
