# =============================================================================
# 02_manifesto_parlgov.R
#
# Purpose:
#   Build an annual time series (2004-2024) of government and opposition
#   defence policy stances for all EU28 + UK countries.
#
#   Stance is measured using Manifesto Project variable per104 ("Military:
#   Positive"), which captures the share of a party's manifesto quasi-sentences
#   devoted to positive statements about military spending, defence capability,
#   and NATO commitments. Higher per104 values indicate a more pro-defence stance.
#
#   Government vs. opposition classification is based on ParlGov cabinet data.
#   Stances are seat-weighted within each government or opposition bloc.
#   Last-observation-carried-forward (LOCF) imputation fills gaps between
#   elections so that every country-year has a value.
#
# Paper section: Section 4 – "Measuring Defence Policy Stance"
#
# Data sources (MANUAL DOWNLOAD REQUIRED):
#
#   (A) Manifesto Project Dataset MPDS2025a
#       URL:      https://manifesto-project.wzb.eu/
#       Navigate: "Datasets" → "Manifesto Corpus" → "MPDS2025a" → Download CSV
#       Save as:  data/raw/MPDataset_MPDS2025a.csv
#       Licence:  Free with registration (academic use)
#       Citation: Volkens, A. et al. (2025). The Manifesto Project Dataset
#                 (MPDS2025a). WZB Berlin Social Science Center.
#                 https://doi.org/10.25522/manifesto.mpds.2025a
#
#   (B) ParlGov "stable" release (2024)
#       URL:      https://parlgov.org/data/
#       Navigate: "Stable release" → Download ZIP → extract files
#       Save as:  data/raw/view_cabinet.csv
#                 data/raw/view_election.csv
#                 data/raw/view_party.csv
#       Licence:  CC0 (public domain)
#       Citation: Döring, H. & Manow, P. (2024). Parliaments and governments
#                 database (ParlGov). parlgov.org
#
# Input:
#   data/raw/MPDataset_MPDS2025a.csv
#   data/raw/view_cabinet.csv
#   data/raw/view_election.csv
#   data/raw/view_party.csv
#
# Output:
#   data/processed/stance_time_series.csv  – annual gov/opp stance by country
#   data/processed/party_stance_annual.csv – party-level annual stance
#   report/02_manifesto_parlgov_report.txt
#
# R packages required:
#   tidyverse, lubridate, zoo
# =============================================================================

if (!require("tidyverse")) install.packages("tidyverse")
if (!require("lubridate")) install.packages("lubridate")
if (!require("zoo"))       install.packages("zoo")

library(tidyverse)
library(lubridate)
library(zoo)   # na.locf(): last-observation-carried-forward imputation

# ── 1. COUNTRY LIST ───────────────────────────────────────────────────────────
# EU28 + UK: all member states as of 2020 (pre-Brexit membership list).
# UK is included because it was an EU member for part of the study period.

eu28_uk <- c(
  "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus",
  "Czech Republic", "Denmark", "Estonia", "Finland", "France",
  "Germany", "Greece", "Hungary", "Ireland", "Italy",
  "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands",
  "Poland", "Portugal", "Romania", "Slovakia", "Slovenia",
  "Spain", "Sweden", "United Kingdom"
)

# ISO2 ↔ country name lookup (used to add ISO2 codes to the output)
iso2_map <- c(
  AUT = "Austria",        BEL = "Belgium",      BGR = "Bulgaria",
  HRV = "Croatia",        CYP = "Cyprus",        CZE = "Czech Republic",
  DNK = "Denmark",        EST = "Estonia",       FIN = "Finland",
  FRA = "France",         DEU = "Germany",       GRC = "Greece",
  HUN = "Hungary",        IRL = "Ireland",       ITA = "Italy",
  LVA = "Latvia",         LTU = "Lithuania",     LUX = "Luxembourg",
  MLT = "Malta",          NLD = "Netherlands",   POL = "Poland",
  PRT = "Portugal",       ROU = "Romania",       SVK = "Slovakia",
  SVN = "Slovenia",       ESP = "Spain",         SWE = "Sweden",
  GBR = "United Kingdom"
)

# ── 2. HELPER FUNCTIONS ───────────────────────────────────────────────────────

# Normalise a string for fuzzy matching: lowercase, strip punctuation and
# extra whitespace. This prevents mismatches due to accents, hyphens, etc.
clean_str <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("[[:punct:]]", "") %>%
    str_replace_all("\\s+", " ") %>%
    str_trim()
}

# Fuzzy party name match:
#   Tests whether `needle` (a Manifesto party name) matches any string in
#   `haystack_vec` (ParlGov party names), using three progressively looser
#   criteria: (1) exact clean match, (2) substring containment, (3) token overlap
#   on words ≥ 4 characters.
#
#   A fallback (fallback_pervote) kicks in when no match is found; this uses
#   the largest parties by vote share as a proxy for the government coalition.
#   The fallback rate is reported and should be < 30% for reliable results.

fuzzy_match <- function(needle, haystack_vec) {
  if (is.na(needle) || length(haystack_vec) == 0) return(FALSE)
  haystack_vec  <- haystack_vec[!is.na(haystack_vec)]
  if (length(haystack_vec) == 0) return(FALSE)
  needle_clean  <- clean_str(needle)
  if (is.na(needle_clean) || nchar(needle_clean) == 0) return(FALSE)
  haystack_clean <- clean_str(haystack_vec)
  haystack_clean <- haystack_clean[!is.na(haystack_clean) & nchar(haystack_clean) > 0]
  if (length(haystack_clean) == 0) return(FALSE)
  if (needle_clean %in% haystack_clean) return(TRUE)
  if (isTRUE(any(str_detect(haystack_clean, fixed(needle_clean))))) return(TRUE)
  if (isTRUE(any(str_detect(needle_clean,   fixed(haystack_clean))))) return(TRUE)
  needle_tokens <- str_split(needle_clean, " ")[[1]]
  needle_tokens <- needle_tokens[nchar(needle_tokens) >= 4]
  if (length(needle_tokens) == 0) return(FALSE)
  haystack_tokens <- unlist(str_split(haystack_clean, " "))
  any(needle_tokens %in% haystack_tokens)
}

# ── 3. LOAD RAW DATA ──────────────────────────────────────────────────────────
# See file header for download instructions for each of these files.

cat("Loading Manifesto dataset...\n")
mp_raw <- read_csv("data/raw/MPDataset_MPDS2025a.csv",
                   col_types = cols(edate = col_character()),
                   show_col_types = FALSE)

# edate is the election date stored as "dd/mm/yyyy" in MPDS; parse with dmy()
mp_raw <- mp_raw %>%
  mutate(edate = dmy(edate, quiet = TRUE))

cat("Loading ParlGov files...\n")
cabinet  <- read_csv("data/raw/view_cabinet.csv",  show_col_types = FALSE)
election <- read_csv("data/raw/view_election.csv", show_col_types = FALSE)
party    <- read_csv("data/raw/view_party.csv",    show_col_types = FALSE)

cat("Manifesto rows:", nrow(mp_raw), "\n")
cat("Cabinet rows:",   nrow(cabinet), "\n")

n_failed <- sum(is.na(mp_raw$edate))
cat("edate parse failures:", n_failed, "/", nrow(mp_raw), "\n")

if (all(is.na(mp_raw$edate))) {
  stop("All edate values failed to parse - check Manifesto edate column format")
}

# ── 4. FILTER MANIFESTO TO EU28+UK ───────────────────────────────────────────
# per104 = "Military: Positive" – quasi-sentence share devoted to positive
# mentions of military spending, defence capacity, NATO, etc. (Volkens et al.)
# This is our primary measure of party defence policy stance.
#
# absseat = absolute seats won (used as weight for govt/opp stance aggregation)
# totseats = total parliamentary seats (for seat-share calculation)
# pervote = vote share (fallback weight when seat data unavailable)

mp_eu <- mp_raw %>%
  filter(countryname %in% eu28_uk) %>%
  mutate(
    elect_year        = year(edate),
    per104            = as.numeric(per104),
    absseat           = as.numeric(absseat),
    totseats          = as.numeric(totseats),
    partyname_clean   = clean_str(coalesce(partyname, "")),
    partyabbrev_clean = clean_str(coalesce(partyabbrev, ""))
  ) %>%
  filter(!is.na(edate)) %>%
  select(countryname, partyname, partyabbrev, party,
         edate, elect_year, per104, absseat, totseats,
         pervote, parfam, partyname_clean, partyabbrev_clean)

cat("Manifesto EU28+UK rows (valid edate):", nrow(mp_eu), "\n")
cat("Countries:", n_distinct(mp_eu$countryname), "\n")
cat("Year range:", min(mp_eu$elect_year, na.rm = TRUE),
    "to", max(mp_eu$elect_year, na.rm = TRUE), "\n")

if (nrow(mp_eu) == 0) {
  stop("mp_eu is empty - check countryname values in Manifesto dataset")
}

# ── 5. FILTER PARLGOV TO EU28+UK ─────────────────────────────────────────────
cabinet_eu <- cabinet %>%
  filter(country_name %in% eu28_uk) %>%
  mutate(
    start_date         = as_date(start_date),
    start_year         = year(start_date),
    party_name_clean   = clean_str(coalesce(party_name, "")),
    party_abbrev_clean = clean_str(coalesce(party_name_short, ""))
  )

cat("Cabinet EU28+UK rows:", nrow(cabinet_eu), "\n")

# ── 6. BUILD ANNUAL GOVERNMENT COMPOSITION ────────────────────────────────────
# For each year, find the most recent cabinet formation per country.
# This implements a "carry-forward" logic: the cabinet in power at the start
# of a given year is used for that entire year.

years <- 2004:2024

govt_annual <- map_dfr(years, function(yr) {
  cabinet_eu %>%
    filter(start_year <= yr) %>%
    group_by(country_name) %>%
    filter(start_date == max(start_date)) %>%
    ungroup() %>%
    mutate(year = yr) %>%
    select(country_name, party_name, party_name_clean,
           party_abbrev_clean, cabinet_party,
           seats, election_seats_total, left_right, year, start_date)
})

cat("Annual government composition rows:", nrow(govt_annual), "\n")

# ── 7. WEIGHTED AVERAGE HELPER ────────────────────────────────────────────────
# Computes seat-weighted average of per104.
# Falls back to simple mean if seat data are missing.
calc_weighted_avg <- function(df) {
  if (nrow(df) == 0 || all(is.na(df$per104))) return(NA_real_)
  if (all(is.na(df$absseat)) || sum(df$absseat, na.rm = TRUE) == 0) {
    return(mean(df$per104, na.rm = TRUE))
  }
  weighted.mean(df$per104, w = replace_na(df$absseat, 0), na.rm = TRUE)
}

# ── 8. BUILD ANNUAL STANCE TIME SERIES ───────────────────────────────────────
# For each country-year:
#   1. Identify the government parties from ParlGov (cabinet_party == 1)
#   2. Match those parties to Manifesto entries using fuzzy_match()
#   3. Use the most recent available Manifesto entry for each party (LOCF)
#   4. Compute seat-weighted average of per104 for government and opposition

match_log <- list()

party_annual_list <- map_dfr(eu28_uk, function(cname) {
  results <- map_dfr(years, function(yr) {

    active_cabinet <- govt_annual %>%
      filter(country_name == cname, year == yr)

    gov_names   <- active_cabinet %>% filter(cabinet_party == 1) %>%
                   pull(party_name_clean)
    gov_abbrevs <- active_cabinet %>% filter(cabinet_party == 1) %>%
                   pull(party_abbrev_clean)
    opp_names   <- active_cabinet %>% filter(cabinet_party == 0) %>%
                   pull(party_name_clean)

    # Use the most recent Manifesto entry available up to year yr for each party
    mp_country <- mp_eu %>%
      filter(countryname == cname, elect_year <= yr) %>%
      group_by(partyname) %>%
      filter(edate == max(edate, na.rm = TRUE)) %>%
      ungroup()

    if (nrow(mp_country) == 0) return(NULL)
    if (length(gov_names) == 0) return(NULL)

    mp_classified <- mp_country %>%
      rowwise() %>%
      mutate(
        is_gov = fuzzy_match(partyname_clean, gov_names) |
                 fuzzy_match(partyabbrev_clean, gov_abbrevs),
        is_opp = fuzzy_match(partyname_clean, opp_names)
      ) %>%
      ungroup()

    gov_parties <- mp_classified %>% filter(is_gov)
    opp_parties <- mp_classified %>% filter(is_opp & !is_gov)

    # Fallback: if no government parties matched, use the top-N parties by vote
    # share as a proxy. This is reported in the report as "fallback_pervote" and
    # should be reviewed if the rate exceeds ~30%.
    method <- "name_match"
    if (nrow(gov_parties) == 0 && nrow(mp_country) > 0) {
      n_gov_approx <- max(1, length(gov_names))
      mp_sorted    <- mp_country %>%
        arrange(desc(replace_na(pervote, 0)))
      gov_parties  <- mp_sorted %>% slice_head(n = n_gov_approx)
      opp_parties  <- mp_sorted %>%
        slice_tail(n = max(1, nrow(mp_sorted) - n_gov_approx))
      method <- "fallback_pervote"
    }

    tibble(
      country       = cname,
      year          = yr,
      gov_stance    = calc_weighted_avg(gov_parties),
      opp_stance    = calc_weighted_avg(opp_parties),
      n_gov_parties = nrow(gov_parties),
      n_opp_parties = nrow(opp_parties),
      match_method  = method
    )
  })
  results
})

cat("Party annual rows:", nrow(party_annual_list), "\n")

if (nrow(party_annual_list) == 0) {
  stop("party_annual_list is empty - check Manifesto/ParlGov country name alignment")
}

# ── 9. LOCF IMPUTATION ────────────────────────────────────────────────────────
# Manifesto data are only available at election years. Between elections,
# we carry the most recent election result forward (LOCF) under the assumption
# that party stances do not change between elections.
# zoo::na.locf() fills NAs by propagating the last non-NA value forward.

stance_time_series <- party_annual_list %>%
  group_by(country) %>%
  arrange(year) %>%
  mutate(
    gov_stance_locf = na.locf(gov_stance, na.rm = FALSE),
    opp_stance_locf = na.locf(opp_stance, na.rm = FALSE)
  ) %>%
  ungroup()

# ── 10. ADD ISO2 CODES ─────────────────────────────────────────────────────────
iso2_lookup <- tibble(
  country      = as.character(iso2_map),
  country_code = names(iso2_map)
)

stance_time_series <- stance_time_series %>%
  left_join(iso2_lookup, by = "country")

# ── 11. COVERAGE CHECK ────────────────────────────────────────────────────────
coverage <- stance_time_series %>%
  group_by(country) %>%
  summarise(
    n_years       = n(),
    n_gov_missing = sum(is.na(gov_stance_locf)),
    n_opp_missing = sum(is.na(opp_stance_locf)),
    n_fallback    = sum(match_method == "fallback_pervote", na.rm = TRUE),
    gov_mean      = round(mean(gov_stance_locf, na.rm = TRUE), 3),
    opp_mean      = round(mean(opp_stance_locf, na.rm = TRUE), 3),
    .groups       = "drop"
  )

# ── 12. SAVE OUTPUTS ──────────────────────────────────────────────────────────
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
write_csv(stance_time_series, "data/processed/stance_time_series.csv")
write_csv(party_annual_list,  "data/processed/party_stance_annual.csv")

cat("Saved: data/processed/stance_time_series.csv\n")
cat("Saved: data/processed/party_stance_annual.csv\n")

# ── 13. WRITE REPORT ──────────────────────────────────────────────────────────
dir.create("report", showWarnings = FALSE)

n_fallback_total <- sum(party_annual_list$match_method == "fallback_pervote",
                        na.rm = TRUE)

report <- c(
  "================================================",
  "REPORT: 02_manifesto_parlgov.R",
  paste("Run at:", Sys.time()),
  "================================================",
  "",
  "--- INPUT DATA ---",
  paste("Manifesto rows (all):", nrow(mp_raw)),
  paste("Manifesto edate parse failures:", n_failed),
  paste("Manifesto rows (EU28+UK, valid edate):", nrow(mp_eu)),
  paste("Cabinet rows (EU28+UK):", nrow(cabinet_eu)),
  "",
  "--- MANIFESTO COVERAGE ---",
  paste("Countries:", n_distinct(mp_eu$countryname)),
  paste("Year range:",
        min(mp_eu$elect_year, na.rm = TRUE), "to",
        max(mp_eu$elect_year, na.rm = TRUE)),
  paste("per104 missing:", sum(is.na(mp_eu$per104)), "/", nrow(mp_eu)),
  paste("absseat missing:", sum(is.na(mp_eu$absseat)), "/", nrow(mp_eu)),
  "",
  "--- MATCHING QUALITY ---",
  paste("Total country-year observations:", nrow(party_annual_list)),
  paste("Name-matched observations:",
        sum(party_annual_list$match_method == "name_match", na.rm = TRUE)),
  paste("Fallback (pervote) observations:", n_fallback_total),
  paste("Fallback rate:",
        round(n_fallback_total / nrow(party_annual_list) * 100, 1), "%"),
  "",
  "--- STANCE TIME SERIES ---",
  paste("Countries:", n_distinct(stance_time_series$country)),
  paste("Years: 2004 to 2024"),
  paste("Total rows:", nrow(stance_time_series)),
  "",
  "--- COVERAGE BY COUNTRY ---",
  capture.output(print(coverage, n = Inf)),
  "",
  "--- FLAGS ---",
  if (n_fallback_total > nrow(party_annual_list) * 0.5)
    paste("WARNING: >50% fallback matching - name matching largely failed.",
          "Review party name alignment between Manifesto and ParlGov.")
  else if (n_fallback_total > nrow(party_annual_list) * 0.2)
    paste("INFO:", round(n_fallback_total / nrow(party_annual_list) * 100, 1),
          "% fallback matching - moderate name mismatch")
  else
    paste("OK: Name matching successful for",
          round((1 - n_fallback_total / nrow(party_annual_list)) * 100, 1),
          "% of observations"),
  if (any(coverage$n_gov_missing > 10))
    paste("WARNING: High missing gov stance:",
          paste(coverage$country[coverage$n_gov_missing > 10], collapse = ", "))
  else "OK: Gov stance coverage acceptable",
  if (any(coverage$n_opp_missing > 10))
    paste("WARNING: High missing opp stance:",
          paste(coverage$country[coverage$n_opp_missing > 10], collapse = ", "))
  else "OK: Opp stance coverage acceptable",
  "",
  "--- OUTPUT FILES ---",
  "data/processed/stance_time_series.csv",
  "data/processed/party_stance_annual.csv",
  "",
  "--- DECISION ---",
  "Review fallback rate and missing values.",
  "If fallback > 50%: consider manual party crosswalk table.",
  "Proceed to 02b_parlgov_rightleft.R"
)

writeLines(report, "report/02_manifesto_parlgov_report.txt")
cat("\nReport written to report/02_manifesto_parlgov_report.txt\n")
