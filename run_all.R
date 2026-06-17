# =============================================================================
# run_all.R
# Purpose: Master script — executes the complete data-processing pipeline
#          in the correct sequential order.
#
# Usage
# -----
# Set your working directory to the `prepare/` folder, then run:
#
#   source("run_all.R")
#
# Or from an R console:
#
#   setwd("path/to/prepare")
#   source("run_all.R")
#
# Prerequisites
# -------------
# Before running, ensure the following files are present in data/raw/:
#   • GEDEvent_v26_1.csv          (UCDP GED v26.1 — manual download required)
#   • MPDataset_MPDS2025a.csv     (Manifesto Project — manual download required)
#   • view_cabinet.csv            (ParlGov — manual download required)
#   • view_election.csv           (ParlGov — manual download required)
#   • view_party.csv              (ParlGov — manual download required)
#   • data_gpr_export.xls         (GPR index — optional, for script 01c only)
#
# See README.md for full download instructions for each file.
# All other data (Eurostat, World Bank WDI) are fetched automatically.
#
# Pipeline structure
# ------------------
# Stage 1 — Threat Index
#   01   : UCDP GED → Regional Threat Index (PELT changepoints)
#   01b  : Three robustness variants + country-specific distance-weighted index
#   01c  : GPR comparison (optional robustness check)
#
# Stage 2 — Stance and Contextual Variables
#   02   : Manifesto + ParlGov → Annual gov/opp defence stance time series
#   02b  : ParlGov left-right positions + correlation with stance
#   02c  : Eurostat GDP weights + GDP-weighted EU average trends
#   02d  : Eurostat + WDI population (EU + neighbourhood)
#   02e  : SIPRI military spending via WDI + COFOG comparison
#   02f  : Eurostat COFOG defence spending (GF02 % GDP, % govt)
#
# Stage 3 — DTW Metrics
#   03   : Core DTW distances (debate intensity, gov/opp responsiveness)
#   03b  : DTW robustness across threat index variants
#
# Stage 4 — Clustering
#   04   : NbClust + k-means on DTW metrics → cluster assignments
#   04b  : Cluster stability (seeds, k-values, variants)
#
# Stage 5 — Final Table
#   05   : Assemble master comparison table + cluster labels
#
# Estimated run time: 30-90 minutes (dominated by API downloads in stage 2)
# =============================================================================

# ── Timing ────────────────────────────────────────────────────────────────────
pipeline_start <- proc.time()
cat("=============================================================\n")
cat("  EU DEFENCE PIPELINE — Starting full run\n")
cat("  Time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("=============================================================\n\n")

# ── Helper: run script with timing and error reporting ────────────────────────
run_script <- function(path, optional = FALSE) {
  script_start <- proc.time()
  cat(sprintf(">>> Running: %s\n", path))
  result <- tryCatch(
    {
      source(path, local = FALSE)
      "OK"
    },
    error = function(e) {
      msg <- conditionMessage(e)
      if (optional) {
        cat(sprintf("    SKIPPED (optional): %s\n", msg))
        "SKIPPED"
      } else {
        cat(sprintf("    FAILED: %s\n", msg))
        stop(sprintf("Pipeline halted at %s: %s", path, msg))
      }
    }
  )
  elapsed <- (proc.time() - script_start)[["elapsed"]]
  cat(sprintf("    [%s] completed in %.1f seconds\n\n", result, elapsed))
  invisible(result)
}

# ── Stage 1: Threat Index ─────────────────────────────────────────────────────
cat("--- STAGE 1: THREAT INDEX ---\n\n")
run_script("scripts/01_threat_index.R")
run_script("scripts/01b_threat_robustness.R")
run_script("scripts/01c_gpr_comparison.R", optional = TRUE)  # GPR is optional

# ── Stage 2: Stance and Contextual Variables ──────────────────────────────────
cat("--- STAGE 2: STANCE AND CONTEXT ---\n\n")
run_script("scripts/02_manifesto_parlgov.R")
run_script("scripts/02b_parlgov_rightleft.R")
run_script("scripts/02c_gdp_eurostat.R")
run_script("scripts/02d_population.R")
run_script("scripts/02e_sipri_wdi.R")
run_script("scripts/02f_defence_gdp_eurostat.R")

# ── Stage 3: DTW Metrics ──────────────────────────────────────────────────────
cat("--- STAGE 3: DTW METRICS ---\n\n")
run_script("scripts/03_dtw_metrics.R")
run_script("scripts/03b_dtw_robustness.R")

# ── Stage 4: Clustering ───────────────────────────────────────────────────────
cat("--- STAGE 4: CLUSTERING ---\n\n")
run_script("scripts/04_clustering.R")
run_script("scripts/04b_clustering_robustness.R")

# ── Stage 5: Final Table ──────────────────────────────────────────────────────
cat("--- STAGE 5: FINAL TABLE ---\n\n")
run_script("scripts/05_comparison_table.R")

# ── Summary ───────────────────────────────────────────────────────────────────
total_elapsed <- (proc.time() - pipeline_start)[["elapsed"]]
cat("=============================================================\n")
cat("  PIPELINE COMPLETE\n")
cat(sprintf("  Total time: %.1f minutes\n", total_elapsed / 60))
cat("  Time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("=============================================================\n\n")
cat("Key outputs:\n")
cat("  data/processed/final_comparison_table_paper.csv  — master table\n")
cat("  data/processed/dtw_metrics.csv                   — DTW metrics\n")
cat("  data/processed/cluster_assignments_threat.csv    — cluster assignments\n")
cat("  report/05_final_summary_report.txt               — pipeline summary\n\n")
cat("Review all report/*.txt files for diagnostics and decision flags.\n")
