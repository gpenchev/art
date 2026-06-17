# =============================================================================
# 02b_parlgov_rightleft.R
# Purpose: Build an annual cabinet left-right position time series for
#          EU28 + UK (2004-2024) from ParlGov data, then measure its
#          correlation with government and opposition defence stance.
#
# Substantive role in the paper
# --------------------------------
# This script asks whether the ideological composition of governments
# (left vs. right) predicts the level of pro-military rhetoric measured
# by the Manifesto `per104` variable.  The cabinet left-right score is
# a seat-weighted average of individual party left-right scores sourced
# from ParlGov's `view_cabinet` table; gaps are filled hierarchically
# from the `view_party` table and then from party-family medians
# established in the comparative politics literature (see Döring &
# Manow, 2024).  The script is a *descriptive/correlational* auxiliary;
# it does not directly enter the DTW analysis.
#
# Data sources (all already present from script 02)
# ------------------------------------------------
# • ParlGov (stable release 2024) — view_cabinet.csv, view_party.csv
#   → Download: https://www.parlgov.org/data/parlgov-stable.csv.zip
#     or via the database export at https://www.parlgov.org/static/data/
#     Extract and save the two CSV files as:
#       data/raw/view_cabinet.csv
#       data/raw/view_party.csv
# • stance_time_series.csv — produced by script 02_manifesto_parlgov.R
#
# Inputs
# ------
#   data/raw/view_cabinet.csv
#   data/raw/view_party.csv
#   data/processed/stance_time_series.csv
#
# Outputs
# -------
#   data/processed/cabinet_rightleft.csv
#       Annual cabinet left-right score per country (seat-weighted)
#   data/processed/rightleft_stance_correlation.csv
#       Country-level Pearson r between cabinet LR and defence stance
#   data/processed/stance_rightleft_combined.csv
#       Merged panel: stance + cabinet LR (used for visualisation)
#   report/02b_parlgov_rightleft_report.txt
#       Diagnostic report with fill-source audit and correlation summary
#
# References
# ----------
# Döring, H. & Manow, P. (2024). ParlGov: Parliaments and Governments
#   Database, stable version. https://www.parlgov.org/
# =============================================================================

# ── Packages ──────────────────────────────────────────────────────────────────
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("lubridate")) install.packages("lubridate")

library(tidyverse)
library(lubridate)

# ── Country list ──────────────────────────────────────────────────────────────
# 27 EU member states plus the UK (retained for the full 2004-2024 span,
# as the UK was a member for the majority of the study period and left
# the EU at end-2020).
eu28_uk <- c(
  "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus",
  "Czech Republic", "Denmark", "Estonia", "Finland", "France",
  "Germany", "Greece", "Hungary", "Ireland", "Italy",
  "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands",
  "Poland", "Portugal", "Romania", "Slovakia", "Slovenia",
  "Spain", "Sweden", "United Kingdom"
)

# ── Load data ─────────────────────────────────────────────────────────────────
cat("Loading ParlGov files...\n")
cabinet <- read_csv("data/raw/view_cabinet.csv",  show_col_types = FALSE)
party   <- read_csv("data/raw/view_party.csv",    show_col_types = FALSE)
stance  <- read_csv("data/processed/stance_time_series.csv",
                    show_col_types = FALSE)

# Filter to our study countries and parse date column
cabinet_eu <- cabinet %>%
  filter(country_name %in% eu28_uk) %>%
  mutate(
    start_date = as_date(start_date),
    start_year = year(start_date)
  )

party_eu <- party %>%
  filter(country_name %in% eu28_uk)

cat("Cabinet rows:", nrow(cabinet_eu), "\n")
cat("Party rows:",   nrow(party_eu),   "\n")

# ── Fill missing left_right scores ────────────────────────────────────────────
# ParlGov provides a 0-10 left-right scale at both the cabinet-party and
# party-table level.  Coverage is incomplete, so we apply a three-tier
# imputation strategy:
#   1. Use `left_right` from view_cabinet if available (most specific).
#   2. Fall back to `left_right` from view_party via party_id.
#   3. Fall back to the party-family median from the literature
#      (Döring & Manow, 2024; values below are widely-used reference
#      scores for European party families on the 0-10 CMP scale).
family_lr_defaults <- tribble(
  ~family_name,              ~family_lr,
  "Communist/Socialist",      1.5,
  "Social democracy",         3.5,
  "Green/Ecologist",          3.0,
  "Liberal",                  5.5,
  "Christian democracy",      6.5,
  "Conservative",             7.0,
  "National conservatism",    7.5,
  "Right-wing",               8.5,
  "Agrarian",                 5.0,
  "Regionalist",              5.0,
  "Special issue",            5.0,
  "No family",                5.0   # neutral centre value
)

# Build a party-level lookup: party_id → best available LR score
party_lr <- party_eu %>%
  select(party_id, family_name, left_right_party = left_right) %>%
  left_join(family_lr_defaults, by = "family_name") %>%
  mutate(
    # coalesce() picks the first non-NA: party score > family default
    lr_filled = coalesce(left_right_party, family_lr, 5.0)
  )

# Apply three-tier fill to the cabinet table
cabinet_filled <- cabinet_eu %>%
  left_join(party_lr %>% select(party_id, lr_filled),
            by = "party_id") %>%
  mutate(
    left_right_final = coalesce(left_right, lr_filled),
    fill_source = case_when(
      !is.na(left_right) ~ "cabinet",        # ParlGov cabinet record
      !is.na(lr_filled)  ~ "party_table",    # ParlGov party record
      TRUE               ~ "family_default"  # literature median
    )
  )

cat("Left-right fill sources:\n")
print(table(cabinet_filled$fill_source))

# ── Annual cabinet left-right position ────────────────────────────────────────
# For each country-year we identify the *active* cabinet (the most recent
# formation with start_year <= study year) and take a *seat-weighted*
# average of governing-party LR scores.  Seat weighting better reflects
# the distribution of power within a coalition than a simple average.
# If seat data are missing for all coalition members, an unweighted mean
# is used as a fallback.
years <- 2004:2024

cabinet_annual <- map_dfr(years, function(yr) {
  map_dfr(eu28_uk, function(cname) {

    # Identify governing parties in the most recent cabinet formed by year yr
    active <- cabinet_filled %>%
      filter(country_name == cname,
             start_year   <= yr,
             cabinet_party == 1) %>%       # cabinet_party==1: governing party
      filter(start_date == max(start_date)) # most recent cabinet formation

    if (nrow(active) == 0) {
      return(tibble(
        country             = cname, year = yr,
        cabinet_lr_weighted = NA_real_,
        cabinet_lr_simple   = NA_real_,
        n_gov_parties       = 0L,
        is_right_leaning    = NA
      ))
    }

    # Seat-weighted mean; fall back to simple mean if all seats are NA/zero
    lr_weighted <- if (sum(active$seats, na.rm = TRUE) > 0) {
      weighted.mean(active$left_right_final,
                    w    = active$seats,
                    na.rm = TRUE)
    } else {
      mean(active$left_right_final, na.rm = TRUE)
    }

    tibble(
      country             = cname,
      year                = yr,
      cabinet_lr_weighted = lr_weighted,
      cabinet_lr_simple   = mean(active$left_right_final, na.rm = TRUE),
      n_gov_parties       = nrow(active),
      # Conventional midpoint: >5 = right-leaning on 0-10 scale
      is_right_leaning    = lr_weighted > 5.0
    )
  })
})

cat("Annual cabinet LR rows:", nrow(cabinet_annual), "\n")

# ── Correlation with defence stance ──────────────────────────────────────────
# Join the LR time series with the government/opposition stance series
# produced by 02_manifesto_parlgov.R and compute Pearson correlations.
combined <- stance %>%
  left_join(cabinet_annual, by = c("country", "year")) %>%
  filter(!is.na(gov_stance_locf), !is.na(cabinet_lr_weighted))

# Overall (pooled) correlation across all country-years
cor_gov_lr <- cor(combined$gov_stance_locf,
                  combined$cabinet_lr_weighted,
                  use = "complete.obs")
cor_opp_lr <- cor(combined$opp_stance_locf,
                  combined$cabinet_lr_weighted,
                  use = "complete.obs")

cat("Correlation gov_stance vs cabinet_lr:", round(cor_gov_lr, 3), "\n")
cat("Correlation opp_stance vs cabinet_lr:", round(cor_opp_lr, 3), "\n")

# Country-level correlations (time-series within each country)
country_cors <- combined %>%
  group_by(country) %>%
  summarise(
    cor_gov_lr = tryCatch(
      cor(gov_stance_locf, cabinet_lr_weighted, use = "complete.obs"),
      error = function(e) NA_real_
    ),
    cor_opp_lr = tryCatch(
      cor(opp_stance_locf, cabinet_lr_weighted, use = "complete.obs"),
      error = function(e) NA_real_
    ),
    n_obs   = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(abs(cor_gov_lr)))

# Mean stance by government orientation (right vs. left)
right_left_comparison <- combined %>%
  filter(!is.na(is_right_leaning)) %>%
  group_by(is_right_leaning) %>%
  summarise(
    mean_gov_stance = mean(gov_stance_locf, na.rm = TRUE),
    mean_opp_stance = mean(opp_stance_locf, na.rm = TRUE),
    n_obs           = n(),
    .groups         = "drop"
  )

# ── Save outputs ──────────────────────────────────────────────────────────────
write_csv(cabinet_annual, "data/processed/cabinet_rightleft.csv")
write_csv(country_cors,   "data/processed/rightleft_stance_correlation.csv")
write_csv(combined,       "data/processed/stance_rightleft_combined.csv")

cat("Saved: data/processed/cabinet_rightleft.csv\n")
cat("Saved: data/processed/rightleft_stance_correlation.csv\n")
cat("Saved: data/processed/stance_rightleft_combined.csv\n")

# ── Report ────────────────────────────────────────────────────────────────────
report <- c(
  "================================================",
  "REPORT: 02b_parlgov_rightleft.R",
  paste("Run at:", Sys.time()),
  "================================================",
  "",
  "--- LEFT-RIGHT FILL SOURCES ---",
  capture.output(print(table(cabinet_filled$fill_source))),
  paste("Missing after fill:",
        sum(is.na(cabinet_filled$left_right_final))),
  "",
  "--- ANNUAL CABINET LR COVERAGE ---",
  paste("Total country-year rows:", nrow(cabinet_annual)),
  paste("Missing LR (no cabinet data):",
        sum(is.na(cabinet_annual$cabinet_lr_weighted))),
  paste("Right-leaning country-years:",
        sum(cabinet_annual$is_right_leaning,  na.rm = TRUE)),
  paste("Left-leaning country-years:",
        sum(!cabinet_annual$is_right_leaning, na.rm = TRUE)),
  "",
  "--- OVERALL CORRELATIONS (pooled) ---",
  paste("Gov defence stance vs cabinet LR:", round(cor_gov_lr, 3)),
  paste("Opp defence stance vs cabinet LR:", round(cor_opp_lr, 3)),
  "",
  "--- RIGHT vs LEFT GOVERNMENT DEFENCE STANCE ---",
  capture.output(print(right_left_comparison)),
  "",
  "--- COUNTRY-LEVEL CORRELATIONS (top 10 by |r|) ---",
  capture.output(print(head(country_cors, 10))),
  "",
  "--- FLAGS ---",
  if (abs(cor_gov_lr) < 0.1)
    "INFO: Weak overall correlation between LR position and defence stance"
  else if (abs(cor_gov_lr) > 0.5)
    "INFO: Strong correlation - right-wing governments show markedly different defence stance"
  else
    "INFO: Moderate correlation between LR position and defence stance",
  "",
  "--- OUTPUT FILES ---",
  "data/processed/cabinet_rightleft.csv",
  "data/processed/rightleft_stance_correlation.csv",
  "data/processed/stance_rightleft_combined.csv",
  "",
  "--- DECISION ---",
  "If |cor| > 0.3: consider reporting LR-stance association in paper Section 5.",
  "Proceed to 02c_gdp_eurostat.R"
)

writeLines(report, "report/02b_parlgov_rightleft_report.txt")
cat("\nReport written to report/02b_parlgov_rightleft_report.txt\n")
