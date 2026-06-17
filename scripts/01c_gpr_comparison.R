# =============================================================================
# 01c_gpr_comparison.R
#
# Purpose:
#   Download the Caldara-Iacoviello Geopolitical Risk (GPR) index and compare
#   it with the UCDP-based Regional Threat Index. Implements a distance-weighted
#   GPR composite as a Bartik-style robustness check.
#
#   The comparison addresses Reviewer 1's recommendation to validate the UCDP
#   conflict-fatalities measure against an alternative, perception-based
#   threat indicator.
#
# Conceptual note:
#   GPR measures media coverage of geopolitical tensions (Caldara & Iacoviello
#   2022); UCDP GED measures actual conflict fatalities. The two capture
#   complementary dimensions of threat: perceived vs. realised violence.
#   High correlation supports our use of UCDP; divergence (e.g. during
#   Cold-War-style standoffs) would motivate discussing both measures.
#
# Paper section: Appendix – "Alternative Threat Measures"
#
# Data sources:
#   (A) GPR index – downloaded automatically from Iacoviello's website
#       URL:      https://www.matteoiacoviello.com/gpr.htm
#       Format:   Stata .dta file (downloaded via haven::read_dta)
#       Citation: Caldara, D. & Iacoviello, M. (2022). Measuring Geopolitical
#                 Risk. American Economic Review 112(4): 1194-1225.
#       No registration required; requires internet connection.
#   (B) Regional Threat Index    – output of 01_threat_index.R
#   (C) Distance weights matrix  – output of 01b_threat_robustness.R
#
# Input:
#   https://www.matteoiacoviello.com/gpr_files/data_gpr_export.dta (auto-downloaded)
#   data/processed/regional_threat_index.csv
#   data/processed/distance_weights.csv
#
# Output:
#   data/processed/gpr_neighbourhood.csv    – monthly GPR by neighbourhood country
#   data/processed/gpr_weighted_index.csv   – distance-weighted GPR composite
#   data/processed/gpr_ucdp_comparison.csv  – combined UCDP + GPR series
#   data/processed/gpr_annual.csv           – annual averages (2004-2024)
#   report/01c_gpr_comparison_report.txt
#
# R packages required:
#   tidyverse, haven, lubridate, changepoint, purrr
# =============================================================================

if (!require("tidyverse"))   install.packages("tidyverse")
if (!require("haven"))       install.packages("haven")
if (!require("lubridate"))   install.packages("lubridate")
if (!require("changepoint")) install.packages("changepoint")
if (!require("purrr"))       install.packages("purrr")

library(tidyverse)
library(haven)       # reads Stata .dta files
library(lubridate)
library(changepoint)
library(purrr)

# ── 1. DOWNLOAD GPR DATA ──────────────────────────────────────────────────────
# The GPR dataset is a Stata .dta file hosted on Iacoviello's website.
# {haven} can read .dta directly from a URL.
# If the download fails, check internet connectivity and the URL below.

cat("Downloading Caldara-Iacoviello GPR data...\n")

gpr_raw <- tryCatch(
  read_dta("https://www.matteoiacoviello.com/gpr_files/data_gpr_export.dta"),
  error = function(e) {
    cat("ERROR downloading GPR data:", conditionMessage(e), "\n")
    NULL
  }
)

if (is.null(gpr_raw)) stop("GPR data download failed - check internet connection")

cat("GPR raw rows:", nrow(gpr_raw), "\n")
cat("GPR columns:", length(names(gpr_raw)), "\n")

# ── 2. PARSE THE DATE COLUMN ──────────────────────────────────────────────────
# Stata stores dates in its own numeric format. The `month` column can arrive
# in three different forms depending on the Stata version used to save the file:
#   (a) Already parsed as R Date (haven may handle this automatically)
#   (b) Numeric, values < 10000: Stata monthly format – months since Jan 1960
#       Conversion: year = 1960 + floor(x/12), month = (x %% 12) + 1
#   (c) Numeric, values > 10000: Stata daily format – days since 1960-01-01
# We detect which case applies and convert accordingly.

cat("month column class:", class(gpr_raw$month), "\n")
cat("month sample values:", paste(head(gpr_raw$month, 5), collapse = ", "), "\n")

gpr_raw <- gpr_raw %>% as_tibble()

month_sample <- gpr_raw$month[1]
cat("First month value:", month_sample, "\n")

if (inherits(gpr_raw$month, "Date")) {
  # Case (a): haven already parsed it
  gpr_raw <- gpr_raw %>% mutate(month_parsed = month)

} else if (is.numeric(gpr_raw$month) && max(gpr_raw$month, na.rm = TRUE) < 10000) {
  # Case (b): Stata monthly format (months since January 1960)
  gpr_raw <- gpr_raw %>%
    mutate(
      month_parsed = as_date(
        paste(1960 + floor(as.numeric(month) / 12),
              (as.numeric(month) %% 12) + 1,
              "01", sep = "-")
      )
    )

} else if (is.numeric(gpr_raw$month) && max(gpr_raw$month, na.rm = TRUE) > 10000) {
  # Case (c): Stata daily format (days since 1960-01-01)
  gpr_raw <- gpr_raw %>%
    mutate(month_parsed = as_date(as.numeric(month), origin = "1960-01-01"))

} else {
  # Fallback: attempt direct coercion
  gpr_raw <- gpr_raw %>%
    mutate(month_parsed = tryCatch(
      as_date(month),
      error = function(e) as_date(as.numeric(month), origin = "1960-01-01")
    ))
}

cat("Parsed month sample:", paste(head(gpr_raw$month_parsed, 5), collapse = ", "), "\n")
cat("Parsed month range:",
    format(min(gpr_raw$month_parsed, na.rm = TRUE), "%Y-%m"),
    "to",
    format(max(gpr_raw$month_parsed, na.rm = TRUE), "%Y-%m"), "\n")

# ── 3. IDENTIFY AVAILABLE GPR COLUMNS ────────────────────────────────────────
# Column naming convention in the GPR dataset:
#   GPR        – global aggregate index
#   GPRC_XXX   – country-level GPR (constructed from country mentions)
#   GPRHC_XXX  – historical GPR for select countries

gprc_cols  <- names(gpr_raw)[str_detect(names(gpr_raw), "^GPRC_")]
gprhc_cols <- names(gpr_raw)[str_detect(names(gpr_raw), "^GPRHC_")]

cat("GPRC country columns:", length(gprc_cols), "\n")
cat("GPRHC country columns:", length(gprhc_cols), "\n")
cat("Available GPRC countries:",
    paste(str_remove(gprc_cols, "GPRC_"), collapse = ", "), "\n")

# ── 4. MAP GPR COLUMNS TO NEIGHBOURHOOD ISO3 CODES ───────────────────────────
# GPR covers only major economies; most conflict-affected neighbourhood countries
# (Syria, Iraq, Afghanistan, etc.) are not in GPR. We use the 7 available.

neighbourhood_gpr_map <- c(
  "RUS" = "GPRC_RUS",
  "TUR" = "GPRC_TUR",
  "UKR" = "GPRC_UKR",
  "ISR" = "GPRC_ISR",
  "SAU" = "GPRC_SAU",
  "TUN" = "GPRC_TUN",
  "EGY" = "GPRC_EGY"
)

# Keep only those that actually exist in this version of the dataset
neighbourhood_gpr_map <- neighbourhood_gpr_map[
  neighbourhood_gpr_map %in% names(gpr_raw)
]

cat("Neighbourhood countries with GPR data:",
    paste(names(neighbourhood_gpr_map), collapse = ", "), "\n")

# ── 5. CLEAN AND FILTER ───────────────────────────────────────────────────────
has_global_gpr <- "GPR" %in% names(gpr_raw)

gpr_clean <- gpr_raw %>%
  rename(month_date = month_parsed) %>%
  filter(!is.na(month_date)) %>%
  filter(month_date >= as_date("1992-01-01"),
         month_date <= as_date("2024-12-31")) %>%
  select(month = month_date,
         any_of(c("GPR", unname(neighbourhood_gpr_map))))

cat("GPR clean rows:", nrow(gpr_clean), "\n")

if (nrow(gpr_clean) == 0) {
  stop("GPR clean data is empty after date filtering - check date parsing above")
}

# ── 6. RESHAPE TO LONG FORMAT BY COUNTRY ─────────────────────────────────────
gpr_neighbourhood <- gpr_clean %>%
  select(month, any_of(unname(neighbourhood_gpr_map))) %>%
  pivot_longer(
    -month,
    names_to  = "gpr_col",
    values_to = "gpr_value"
  ) %>%
  mutate(
    iso3c = names(neighbourhood_gpr_map)[
      match(gpr_col, neighbourhood_gpr_map)
    ]
  ) %>%
  filter(!is.na(gpr_value), !is.na(iso3c)) %>%
  select(month, iso3c, gpr_value)

cat("Neighbourhood GPR rows:", nrow(gpr_neighbourhood), "\n")

# ── 7. SIMPLE UNWEIGHTED NEIGHBOURHOOD GPR ───────────────────────────────────
# Simple average across available neighbourhood countries (unweighted baseline)
gpr_simple_avg <- gpr_neighbourhood %>%
  group_by(month) %>%
  summarise(
    gpr_simple_mean = mean(gpr_value, na.rm = TRUE),
    gpr_simple_max  = if (any(!is.na(gpr_value)))
                        max(gpr_value, na.rm = TRUE) else NA_real_,
    n_countries     = sum(!is.na(gpr_value)),
    .groups         = "drop"
  )

# ── 8. DISTANCE-WEIGHTED GPR COMPOSITE (BARTIK-STYLE) ───────────────────────
# Applies the inverse-distance weights from 01b_threat_robustness.R to GPR.
# Because GPR is only available for 7 neighbourhood countries, the weights
# are re-normalised to sum to 1 over those 7 countries.
#
# Bartik logic: variation in the composite is driven by idiosyncratic shocks
# in each country (the "shifts"), scaled by structural distance weights (the
# "shares"). This isolates the threat exposure attributable to geography rather
# than political salience.

cat("Computing distance-weighted GPR index...\n")

distance_weights <- read_csv("data/processed/distance_weights.csv",
                              show_col_types = FALSE)

gpr_iso3c_available <- unique(gpr_neighbourhood$iso3c)

eu_avg_weights <- distance_weights %>%
  filter(conflict_country %in% gpr_iso3c_available) %>%
  group_by(conflict_country) %>%
  summarise(
    avg_inv_dist_weight = mean(inv_dist_weight, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(norm_weight = avg_inv_dist_weight / sum(avg_inv_dist_weight))

cat("GPR countries with distance weights:\n")
print(eu_avg_weights)

gpr_dist_weighted <- gpr_neighbourhood %>%
  left_join(eu_avg_weights %>% select(conflict_country, norm_weight),
            by = c("iso3c" = "conflict_country")) %>%
  filter(!is.na(norm_weight)) %>%
  group_by(month) %>%
  summarise(
    gpr_dist_weighted = sum(gpr_value * norm_weight, na.rm = TRUE),
    n_countries_w     = sum(!is.na(gpr_value)),
    .groups           = "drop"
  )

# ── 9. GLOBAL GPR SERIES ─────────────────────────────────────────────────────
# The global GPR (column "GPR") aggregates across all countries in the dataset.
# Used as an additional comparison series.
if (has_global_gpr && "GPR" %in% names(gpr_clean)) {
  gpr_global <- gpr_clean %>%
    select(month, gpr_global = GPR) %>%
    filter(!is.na(gpr_global))
  cat("Global GPR rows:", nrow(gpr_global), "\n")
} else {
  cat("Global GPR column not found - skipping\n")
  gpr_global <- tibble(month = as_date(character()), gpr_global = numeric())
}

# ── 10. LOAD UCDP INDEX ───────────────────────────────────────────────────────
cat("Loading UCDP threat index...\n")
ucdp_index <- read_csv("data/processed/regional_threat_index.csv",
                        show_col_types = FALSE) %>%
  mutate(year = year(month))

# ── 11. COMBINE AND NORMALISE ALL SERIES ─────────────────────────────────────
# Normalise each series to [0, 1] for direct visual and statistical comparison.
norm_01 <- function(x) {
  rng <- max(x, na.rm = TRUE) - min(x, na.rm = TRUE)
  if (!is.finite(rng) || rng == 0) return(rep(0, length(x)))
  (x - min(x, na.rm = TRUE)) / rng
}

gpr_ucdp_comparison <- ucdp_index %>%
  select(month, ucdp_log = log_fatalities) %>%
  left_join(gpr_simple_avg %>% select(month, gpr_simple_mean), by = "month") %>%
  left_join(gpr_dist_weighted %>% select(month, gpr_dist_weighted), by = "month") %>%
  left_join(
    if (nrow(gpr_global) > 0) gpr_global else
      tibble(month = as_date(character()), gpr_global = numeric()),
    by = "month"
  ) %>%
  mutate(
    ucdp_norm       = norm_01(ucdp_log),
    gpr_simple_norm = norm_01(gpr_simple_mean),
    gpr_dist_norm   = norm_01(gpr_dist_weighted),
    gpr_global_norm = if ("gpr_global" %in% names(.))
                        norm_01(gpr_global) else NA_real_
  )

# ── 12. CORRELATIONS ─────────────────────────────────────────────────────────
# Pearson correlation between normalised UCDP and each GPR variant.
# Interpretation thresholds (approximate): > 0.7 strong, 0.5-0.7 moderate, < 0.5 weak.
cor_simple <- tryCatch(
  cor(gpr_ucdp_comparison$ucdp_norm,
      gpr_ucdp_comparison$gpr_simple_norm,
      use = "complete.obs"),
  error = function(e) NA_real_
)
cor_dist <- tryCatch(
  cor(gpr_ucdp_comparison$ucdp_norm,
      gpr_ucdp_comparison$gpr_dist_norm,
      use = "complete.obs"),
  error = function(e) NA_real_
)
cor_global <- tryCatch(
  cor(gpr_ucdp_comparison$ucdp_norm,
      gpr_ucdp_comparison$gpr_global_norm,
      use = "complete.obs"),
  error = function(e) NA_real_
)

cat("Correlation UCDP vs GPR simple:", round(cor_simple, 3), "\n")
cat("Correlation UCDP vs GPR distance-weighted:", round(cor_dist, 3), "\n")
cat("Correlation UCDP vs GPR global:", round(cor_global, 3), "\n")

# ── 13. ANNUAL SERIES (2004-2024, FOR PAPER FIGURES) ─────────────────────────
gpr_annual <- gpr_ucdp_comparison %>%
  mutate(year = year(month)) %>%
  filter(year >= 2004, year <= 2024) %>%
  group_by(year) %>%
  summarise(
    ucdp_annual       = mean(ucdp_log,          na.rm = TRUE),
    gpr_simple_annual = mean(gpr_simple_mean,   na.rm = TRUE),
    gpr_dist_annual   = mean(gpr_dist_weighted, na.rm = TRUE),
    gpr_global_annual = if ("gpr_global" %in% names(gpr_ucdp_comparison))
                          mean(gpr_global,       na.rm = TRUE) else NA_real_,
    .groups           = "drop"
  )

# ── 14. CHANGEPOINT COMPARISON ────────────────────────────────────────────────
# Apply the same PELT changepoint method to the GPR series and compare
# the detected breaks with the UCDP-based security regimes from 01_threat_index.R.
# Aligned breaks support the validity of either measure.

cat("Running changepoint analysis on GPR series...\n")

gpr_series_for_cpt <- gpr_ucdp_comparison %>%
  filter(!is.na(gpr_dist_norm)) %>%
  pull(gpr_dist_norm)

gpr_months_for_cpt <- gpr_ucdp_comparison %>%
  filter(!is.na(gpr_dist_norm)) %>%
  pull(month)

if (length(gpr_series_for_cpt) >= 10) {
  cpt_gpr <- tryCatch(
    cpt.meanvar(gpr_series_for_cpt, method = "PELT", penalty = "BIC"),
    error = function(e) NULL
  )
  if (!is.null(cpt_gpr)) {
    gpr_cpt_positions <- cpts(cpt_gpr)
    gpr_cpt_dates     <- gpr_months_for_cpt[gpr_cpt_positions]
    cat("GPR changepoints:",
        paste(format(gpr_cpt_dates, "%Y-%m"), collapse = ", "), "\n")
  } else {
    gpr_cpt_dates <- as_date(character())
    cat("GPR changepoint analysis failed\n")
  }
} else {
  gpr_cpt_dates <- as_date(character())
  cat("Insufficient data for GPR changepoint analysis\n")
}

ucdp_series_for_cpt <- ucdp_index %>%
  filter(!is.na(log_fatalities)) %>%
  pull(log_fatalities)
ucdp_months_for_cpt <- ucdp_index %>%
  filter(!is.na(log_fatalities)) %>%
  pull(month)

cpt_ucdp       <- cpt.meanvar(ucdp_series_for_cpt, method = "PELT", penalty = "BIC")
ucdp_cpt_pos   <- cpts(cpt_ucdp)
ucdp_cpt_dates <- ucdp_months_for_cpt[ucdp_cpt_pos]

# ── 15. SAVE OUTPUTS ──────────────────────────────────────────────────────────
write_csv(gpr_neighbourhood,   "data/processed/gpr_neighbourhood.csv")
write_csv(gpr_dist_weighted,   "data/processed/gpr_weighted_index.csv")
write_csv(gpr_ucdp_comparison, "data/processed/gpr_ucdp_comparison.csv")
write_csv(gpr_annual,          "data/processed/gpr_annual.csv")

cat("Saved: data/processed/gpr_neighbourhood.csv\n")
cat("Saved: data/processed/gpr_weighted_index.csv\n")
cat("Saved: data/processed/gpr_ucdp_comparison.csv\n")
cat("Saved: data/processed/gpr_annual.csv\n")

# ── 16. WRITE REPORT ──────────────────────────────────────────────────────────
dir.create("report", showWarnings = FALSE)

report <- c(
  "================================================",
  "REPORT: 01c_gpr_comparison.R",
  paste("Run at:", Sys.time()),
  "================================================",
  "",
  "--- DATA SOURCE ---",
  "Caldara, D. and Iacoviello, M. (2022)",
  "Measuring Geopolitical Risk. American Economic Review 112(4): 1194-1225.",
  "Data: https://www.matteoiacoviello.com/gpr.htm",
  "",
  "--- GPR DATA ---",
  paste("Raw rows:", nrow(gpr_raw)),
  paste("Clean rows (1992-2024):", nrow(gpr_clean)),
  paste("GPRC country columns available:", length(gprc_cols)),
  paste("Date range:",
        format(min(gpr_clean$month, na.rm = TRUE), "%Y-%m"),
        "to",
        format(max(gpr_clean$month, na.rm = TRUE), "%Y-%m")),
  "",
  "--- NEIGHBOURHOOD COVERAGE ---",
  paste("Neighbourhood countries with GPR data:",
        paste(names(neighbourhood_gpr_map), collapse = ", ")),
  paste("Countries available:", length(neighbourhood_gpr_map),
        "/ 40 in neighbourhood"),
  "NOTE: GPR covers major economies only - most conflict-affected",
  "neighbourhood countries (Syria, Iraq, Afghanistan etc.) are not covered.",
  "",
  "--- DISTANCE WEIGHTS FOR GPR COUNTRIES ---",
  capture.output(print(eu_avg_weights)),
  "",
  "--- CORRELATIONS: UCDP vs GPR ---",
  paste("UCDP vs GPR simple (unweighted):",
        if (!is.na(cor_simple)) round(cor_simple, 3) else "NA"),
  paste("UCDP vs GPR distance-weighted:",
        if (!is.na(cor_dist)) round(cor_dist, 3) else "NA"),
  paste("UCDP vs GPR global:",
        if (!is.na(cor_global)) round(cor_global, 3) else "NA"),
  "",
  "--- ANNUAL SERIES (2004-2024) ---",
  capture.output(
    gpr_annual %>%
      mutate(across(where(is.numeric), ~round(., 3))) %>%
      print(n = Inf)
  ),
  "",
  "--- CHANGEPOINT COMPARISON ---",
  paste("UCDP changepoints:",
        paste(format(ucdp_cpt_dates, "%Y-%m"), collapse = ", ")),
  paste("GPR changepoints:",
        if (length(gpr_cpt_dates) > 0)
          paste(format(gpr_cpt_dates, "%Y-%m"), collapse = ", ")
        else "None detected or insufficient data"),
  "",
  "--- FLAGS ---",
  if (!is.na(cor_dist) && cor_dist > 0.7)
    paste("OK: UCDP and distance-weighted GPR correlated at",
          round(cor_dist, 3), "- supports robustness")
  else if (!is.na(cor_dist) && cor_dist > 0.5)
    paste("INFO: Moderate correlation UCDP vs GPR:",
          round(cor_dist, 3),
          "- discuss methodological complementarity in paper")
  else if (!is.na(cor_dist))
    paste("WARNING: Low correlation UCDP vs GPR:",
          round(cor_dist, 3),
          "- GPR (media salience) and UCDP (events) capture different things")
  else
    "WARNING: GPR correlation could not be computed - check date parsing",
  paste("INFO: GPR covers only", length(neighbourhood_gpr_map),
        "of 40 neighbourhood countries - weighted index reflects major powers only"),
  "",
  "--- PAPER GUIDANCE ---",
  "If cor > 0.7: cite Caldara-Iacoviello; state results robust to GPR.",
  "If cor 0.5-0.7: note complementarity - GPR captures perception, UCDP captures events.",
  "If cor < 0.5: discuss divergence - media coverage vs. actual conflict intensity.",
  "In all cases note GPR coverage limitation (7/40 neighbourhood countries).",
  "Cite: Caldara, D. & Iacoviello, M. (2022), AER 112(4): 1194-1225.",
  "",
  "--- OUTPUT FILES ---",
  "data/processed/gpr_neighbourhood.csv",
  "data/processed/gpr_weighted_index.csv",
  "data/processed/gpr_ucdp_comparison.csv",
  "data/processed/gpr_annual.csv",
  "",
  "--- DECISION ---",
  "Review correlation and changepoint alignment.",
  "Add GPR comparison paragraph to methodology section.",
  "Proceed to 02_manifesto_parlgov.R"
)

writeLines(report, "report/01c_gpr_comparison_report.txt")
cat("\nReport written to report/01c_gpr_comparison_report.txt\n")
