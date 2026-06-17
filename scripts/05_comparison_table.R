# =============================================================================
# 05_comparison_table.R
# Purpose: Assemble the final country-level comparison table that combines
#          cluster assignments, DTW metrics, cluster-type labels, and
#          robustness stability flags into a single publication-ready
#          dataset.
#
# Substantive role in the paper
# --------------------------------
# This is the terminal script in the pipeline.  It produces the master
# comparison table reported in the paper (Table 1 / Appendix) and a
# condensed "paper" version formatted for direct publication.
#
# Cluster label assignment logic
# --------------------------------
# Labels are assigned automatically from the empirical cluster profiles
# by comparing each cluster's centroid against the within-run median:
#
#   THREAT CLUSTERS (four typologies):
#   ┌──────────────────────────────────┬──────────────┬───────────────┐
#   │ Type                             │ debate_int.  │ gov_resp.     │
#   ├──────────────────────────────────┼──────────────┼───────────────┤
#   │ Polarised Reactors               │ ≥ median     │ ≤ median      │
#   │   (high debate, govt tracks)     │              │               │
#   │ Vocal but Unresponsive           │ ≥ median     │ > median      │
#   │   (high debate, govt decoupled)  │              │               │
#   │ Quiet Reactors                   │ < median     │ ≤ median      │
#   │   (low debate, govt tracks)      │              │               │
#   │ Disengaged                       │ < median     │ > median      │
#   │   (low debate, govt decoupled)   │              │               │
#   └──────────────────────────────────┴──────────────┴───────────────┘
#   Note: DTW = *distance*; lower gov_responsiveness means govt stance
#   tracks threat *more closely* (more responsive).
#
#   SPENDING CLUSTERS (two typologies):
#   Policy Converters  : lower DTW distance (stance mirrors spending)
#   Stable Allocators  : higher DTW distance (spending decoupled from stance)
#
# NA values — explanation
# -------------------------
# • NA spending cluster: United Kingdom.  Eurostat COFOG data only cover
#   EU member states; the UK left the EU at end-2020 and has no COFOG
#   series for the full 2004-2024 window.
# • NA threat cluster: Countries excluded due to insufficient opposition
#   stance data from the Manifesto Project (e.g., countries with
#   single-dominant parties or no clear parliamentary opposition).
#
# Inputs
# ------
#   data/processed/cluster_assignments_threat.csv   (from script 04)
#   data/processed/cluster_assignments.csv          (from script 04)
#   data/processed/dtw_metrics_all.csv              (from script 03)
#   data/processed/dtw_threat_metrics.csv           (from script 03)
#   data/processed/dtw_spending_metrics.csv         (from script 03)
#   data/processed/cluster_robustness.csv           (from script 04b, optional)
#
# Outputs
# -------
#   data/processed/final_comparison_table.csv
#       Full table with all numeric columns and flags
#   data/processed/final_comparison_table_paper.csv
#       Formatted table for publication (rounded, labelled columns)
#   report/05_final_summary_report.txt
#       Final pipeline summary and all key decisions
# =============================================================================

if (!require("tidyverse")) install.packages("tidyverse")

library(tidyverse)

# ── Load data ─────────────────────────────────────────────────────────────────
cat("Loading cluster assignments...\n")
threat_clusters   <- read_csv("data/processed/cluster_assignments_threat.csv",
                               show_col_types = FALSE)
spending_clusters <- read_csv("data/processed/cluster_assignments.csv",
                               show_col_types = FALSE)
dtw_all           <- read_csv("data/processed/dtw_metrics_all.csv",
                               show_col_types = FALSE)

robustness_file <- "data/processed/cluster_robustness.csv"
if (file.exists(robustness_file)) {
  robustness     <- read_csv(robustness_file, show_col_types = FALSE)
  has_robustness <- TRUE
} else {
  has_robustness <- FALSE
}

# ── Detect k from assignments (sanity check) ──────────────────────────────────
k_threat   <- n_distinct(threat_clusters$cluster_threat, na.rm = TRUE)
k_spending <- n_distinct(spending_clusters$cluster,      na.rm = TRUE)
cat("Detected threat clusters (k):", k_threat, "\n")
cat("Detected spending clusters (k):", k_spending, "\n")

# ── Assign threat cluster labels ─────────────────────────────────────────────
dtw_threat_file <- "data/processed/dtw_threat_metrics.csv"

if (file.exists(dtw_threat_file)) {
  dtw_threat <- read_csv(dtw_threat_file, show_col_types = FALSE)

  # Compute cluster centroids
  cluster_profiles <- threat_clusters %>%
    left_join(dtw_threat %>%
                select(country, debate_intensity,
                       gov_responsiveness, opp_responsiveness),
              by = "country") %>%
    group_by(cluster_threat) %>%
    summarise(
      n           = n(),
      mean_debate = mean(debate_intensity,   na.rm = TRUE),
      mean_gov    = mean(gov_responsiveness, na.rm = TRUE),
      mean_opp    = mean(opp_responsiveness, na.rm = TRUE),
      .groups     = "drop"
    ) %>%
    arrange(cluster_threat)

  cat("Cluster profiles:\n")
  print(cluster_profiles)

  med_debate <- median(cluster_profiles$mean_debate)
  med_gov    <- median(cluster_profiles$mean_gov)

  # Label assignment: cross median thresholds on debate and gov_responsiveness
  # See header for the full 2×2 label matrix and its DTW interpretation
  threat_cluster_labels <- cluster_profiles %>%
    mutate(
      threat_cluster_type = case_when(
        mean_debate >= med_debate & mean_gov <= med_gov ~ "Polarised Reactors",
        mean_debate >= med_debate & mean_gov >  med_gov ~ "Vocal but Unresponsive",
        mean_debate <  med_debate & mean_gov <= med_gov ~ "Quiet Reactors",
        mean_debate <  med_debate & mean_gov >  med_gov ~ "Disengaged",
        TRUE ~ paste("Cluster", cluster_threat)  # fallback if k ≠ 4
      )
    ) %>%
    select(cluster_threat, threat_cluster_type)

} else {
  # Hard-coded fallback labels for k = 4 (based on known profiles from the paper)
  threat_cluster_labels <- tribble(
    ~cluster_threat, ~threat_cluster_type,
    1L, "Polarised Reactors",
    2L, "Disengaged",
    3L, "Quiet Reactors",
    4L, "Engaged Reactors"
  )
}

cat("Threat cluster labels:\n")
print(threat_cluster_labels)

# ── Assign spending cluster labels ────────────────────────────────────────────
dtw_spending_file <- "data/processed/dtw_spending_metrics.csv"

if (file.exists(dtw_spending_file)) {
  dtw_spending <- read_csv(dtw_spending_file, show_col_types = FALSE)

  spending_profiles <- spending_clusters %>%
    left_join(dtw_spending %>%
                select(country, gov_spending_similarity,
                       opp_spending_similarity),
              by = "country") %>%
    group_by(cluster) %>%
    summarise(
      n            = n(),
      mean_gov_sim = mean(gov_spending_similarity, na.rm = TRUE),
      mean_opp_sim = mean(opp_spending_similarity, na.rm = TRUE),
      .groups      = "drop"
    ) %>%
    arrange(cluster)

  cat("Spending cluster profiles:\n")
  print(spending_profiles)

  # Lower DTW = stance tracks spending more closely → "Policy Converters"
  spending_cluster_labels <- spending_profiles %>%
    mutate(
      spending_cluster_type = if_else(
        mean_gov_sim <= median(mean_gov_sim),
        "Policy Converters",
        "Stable Allocators"
      )
    ) %>%
    select(cluster, spending_cluster_type)

} else {
  # Fallback for k = 2
  spending_cluster_labels <- tribble(
    ~cluster, ~spending_cluster_type,
    1L, "Policy Converters",
    2L, "Stable Allocators"
  )
}

cat("Spending cluster labels:\n")
print(spending_cluster_labels)

# ── Build the full comparison table ──────────────────────────────────────────
comparison_table <- threat_clusters %>%
  full_join(spending_clusters, by = "country") %>%
  left_join(
    dtw_all %>%
      select(country, debate_intensity, gov_responsiveness,
             opp_responsiveness, gov_spending_similarity,
             opp_spending_similarity),
    by = "country"
  ) %>%
  left_join(threat_cluster_labels,   by = "cluster_threat") %>%
  left_join(spending_cluster_labels, by = "cluster") %>%
  arrange(cluster_threat, country)

if (has_robustness) {
  comparison_table <- comparison_table %>%
    left_join(
      robustness %>% select(country, is_stable, pct_stable),
      by = "country"
    )
}

# ── NA diagnostics ────────────────────────────────────────────────────────────
# Log which countries are missing from each cluster dimension and why
na_spending <- comparison_table %>%
  filter(is.na(cluster)) %>%
  pull(country)

na_threat <- comparison_table %>%
  filter(is.na(cluster_threat)) %>%
  pull(country)

cat("Countries with NA spending cluster:", paste(na_spending, collapse = ", "), "\n")
cat("Countries with NA threat cluster:",  paste(na_threat,   collapse = ", "), "\n")

# ── Format for publication ────────────────────────────────────────────────────
# Round all DTW metrics to 3 d.p.; replace NA spending cluster with
# a human-readable explanation for the paper footnote.
paper_table <- comparison_table %>%
  mutate(
    across(c(debate_intensity, gov_responsiveness,
             opp_responsiveness, gov_spending_similarity,
             opp_spending_similarity),
           ~round(., 3)),
    spending_cluster_type = if_else(
      is.na(cluster),
      "No COFOG data",    # explains NA for UK in the paper
      spending_cluster_type
    )
  ) %>%
  select(
    Country                        = country,
    `Threat Cluster`               = cluster_threat,
    `Threat Cluster Type`          = threat_cluster_type,
    `Spending Cluster`             = cluster,
    `Spending Cluster Type`        = spending_cluster_type,
    `Debate Intensity`             = debate_intensity,
    `Gov. Threat Responsiveness`   = gov_responsiveness,
    `Opp. Threat Responsiveness`   = opp_responsiveness,
    `Gov. Spending Similarity`     = gov_spending_similarity,
    `Opp. Spending Similarity`     = opp_spending_similarity
  )

if (has_robustness) {
  paper_table <- paper_table %>%
    left_join(
      comparison_table %>%
        select(Country = country, `Cluster Stable` = is_stable),
      by = "Country"
    )
}

# ── Save outputs ──────────────────────────────────────────────────────────────
write_csv(comparison_table, "data/processed/final_comparison_table.csv")
write_csv(paper_table,      "data/processed/final_comparison_table_paper.csv")

cat("Saved: data/processed/final_comparison_table.csv\n")
cat("Saved: data/processed/final_comparison_table_paper.csv\n")

# ── Final report ──────────────────────────────────────────────────────────────
report <- c(
  "================================================",
  "REPORT: 05_comparison_table.R",
  paste("Run at:", Sys.time()),
  "================================================",
  "",
  "--- TABLE SUMMARY ---",
  paste("Total countries:", nrow(comparison_table)),
  paste("Threat clusters (k):", k_threat),
  paste("Spending clusters (k):", k_spending),
  paste("Countries with threat cluster:",
        sum(!is.na(comparison_table$cluster_threat))),
  paste("Countries with spending cluster:",
        sum(!is.na(comparison_table$cluster))),
  paste("Countries with both clusters:",
        sum(!is.na(comparison_table$cluster_threat) &
              !is.na(comparison_table$cluster))),
  "",
  "--- NA EXPLANATION ---",
  paste("NA spending cluster countries:",
        if (length(na_spending) > 0) paste(na_spending, collapse = ", ")
        else "None"),
  "  Reason: No Eurostat COFOG data for non-EU countries (UK left EU 2020)",
  paste("NA threat cluster countries:",
        if (length(na_threat) > 0) paste(na_threat, collapse = ", ")
        else "None"),
  "  Reason: Insufficient Manifesto Project opposition data for some countries",
  "",
  "--- THREAT CLUSTER LABELS ---",
  "Label logic (see script header for full 2x2 matrix):",
  "  Polarised Reactors:     high debate + low gov_resp  (tracks threat)",
  "  Vocal but Unresponsive: high debate + high gov_resp (ignores threat)",
  "  Quiet Reactors:         low debate  + low gov_resp  (tracks threat)",
  "  Disengaged:             low debate  + high gov_resp (ignores threat)",
  capture.output(print(threat_cluster_labels, n = Inf)),
  "",
  "--- SPENDING CLUSTER LABELS ---",
  "  Policy Converters: lower DTW distance — stance closely tracks spending",
  "  Stable Allocators: higher DTW distance — spending decoupled from stance",
  capture.output(print(spending_cluster_labels, n = Inf)),
  "",
  "--- FULL COMPARISON TABLE ---",
  capture.output(print(paper_table, n = Inf)),
  "",
  "--- CLUSTER DISTRIBUTION ---",
  "Threat clusters:",
  capture.output(print(table(comparison_table$cluster_threat))),
  "Spending clusters:",
  capture.output(print(table(comparison_table$cluster))),
  "",
  if (has_robustness) {
    c(
      "--- ROBUSTNESS ---",
      paste("Stable countries:", sum(comparison_table$is_stable, na.rm = TRUE)),
      paste("Unstable countries:", sum(!comparison_table$is_stable, na.rm = TRUE)),
      "Stable country list:",
      paste(
        comparison_table$country[
          !is.na(comparison_table$is_stable) & comparison_table$is_stable],
        collapse = ", "
      )
    )
  } else "--- ROBUSTNESS --- Not available (run 04b first)",
  "",
  "--- FLAGS ---",
  if (length(na_spending) > 0)
    paste("INFO: Add paper footnote explaining NA spending cluster for:",
          paste(na_spending, collapse = ", "))
  else "OK: No NA spending clusters",
  if (k_threat != 4)
    paste("INFO: Threat k =", k_threat,
          "— paper reports k=4. Update paper or justify theoretically.")
  else "OK: Threat k = 4 matches paper",
  if (k_spending != 2)
    paste("INFO: Spending k =", k_spending,
          "— paper reports k=2. Update paper or justify theoretically.")
  else "OK: Spending k = 2 matches paper",
  "",
  "--- OUTPUT FILES ---",
  "data/processed/final_comparison_table.csv",
  "data/processed/final_comparison_table_paper.csv",
  "",
  "================================================",
  "FINAL PIPELINE SUMMARY — ALL PROCESSING COMPLETE",
  "================================================",
  "",
  "Review all report files before finalising the paper revision.",
  "",
  "Key decision checklist:",
  "1. Cluster labels appropriate?   → see report/04_clustering_report.txt",
  "2. Results robust?               → see report/04b_clustering_robustness_report.txt",
  "3. GDP weighting effect?         → see report/02c_gdp_eurostat_report.txt",
  "4. SIPRI vs COFOG agreement?     → see report/02e_sipri_wdi_report.txt",
  "5. DTW robust to threat variants?→ see report/03b_dtw_robustness_report.txt",
  "6. GPR correlated with UCDP?     → see report/01c_gpr_comparison_report.txt",
  "",
  "Script execution order:",
  "01 → 01b → 01c → 02 → 02b → 02c → 02d → 02e → 02f → 03 → 03b → 04 → 04b → 05",
  "Or run the complete pipeline via: source('run_all.R')"
)

writeLines(report, "report/05_final_summary_report.txt")
cat("\nReport written to report/05_final_summary_report.txt\n")
cat("\nAll data processing complete.\n")
