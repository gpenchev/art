# =============================================================================
# 04b_clustering_robustness.R
# Purpose: Verify that the cluster assignments from script 04 are stable
#          across three dimensions of variation:
#          1. Different random seeds (initialisation sensitivity)
#          2. Different values of k (sensitivity to cluster number)
#          3. Different threat index variants (sensitivity to input data)
#
# Why robustness testing matters
# --------------------------------
# k-means is sensitive to (a) random initialisation, (b) the choice of k,
# and (c) the input variables.  If cluster assignments change substantially
# when any of these is varied, the typology is unstable and its
# interpretive validity is in doubt.  We quantify stability using the
# Adjusted Rand Index (ARI), a chance-corrected measure of agreement
# between two partitions.
#
# Adjusted Rand Index (ARI) interpretation
# -----------------------------------------
# ARI = 1.0  : perfect agreement between two partitions
# ARI > 0.9  : highly stable (near-identical cluster assignments)
# ARI > 0.7  : moderately stable
# ARI < 0.5  : poor stability — results should be interpreted cautiously
#
# ROBUSTNESS_K
# ------------
# `ROBUSTNESS_K <- 4L` is set to match the optimal k identified by
# NbClust in script 04.  Using the *same* k in robustness tests is
# essential: comparing a 4-cluster solution with a 5-cluster solution
# would always produce a low ARI by construction, regardless of stability.
# If script 04 produces a different optimal k, update ROBUSTNESS_K here.
#
# Stability criterion
# --------------------
# A country is classified as "stable" (is_stable = TRUE) if it lands in
# the same cluster in ≥ 66% of the alternative-variant clusterings.
# Unstable countries are discussed in the paper's robustness section.
#
# Inputs
# ------
#   data/processed/dtw_metrics_robustness.csv   (from script 03b)
#   data/processed/cluster_assignments_threat.csv (from script 04)
#   data/processed/cluster_assignments.csv        (from script 04)
#   data/processed/dtw_threat_metrics.csv         (from script 03)
#
# Outputs
# -------
#   data/processed/cluster_robustness.csv
#       Main cluster table augmented with stability flags
#   data/processed/cluster_stability_summary.csv
#       Per-country stability percentage across variants
#   data/processed/k_silhouette_scores.csv
#       Average silhouette for k = 3 … 7
#   report/04b_clustering_robustness_report.txt
#
# References
# ----------
# Hubert, L. & Arabie, P. (1985). Comparing partitions.
#   Journal of Classification, 2(1), 193-218.
# Vinh, N.X. et al. (2010). Information theoretic measures for clusterings
#   comparison. Journal of Machine Learning Research, 11, 2837-2854.
# =============================================================================

if (!require("tidyverse"))  install.packages("tidyverse")
if (!require("cluster"))    install.packages("cluster")
if (!require("mclust"))     install.packages("mclust")

library(tidyverse)
library(cluster)
library(mclust)   # for adjustedRandIndex()

# ── Load data ─────────────────────────────────────────────────────────────────
cat("Loading data...\n")
dtw_robust    <- read_csv("data/processed/dtw_metrics_robustness.csv",
                          show_col_types = FALSE)
main_threat   <- read_csv("data/processed/cluster_assignments_threat.csv",
                          show_col_types = FALSE)
main_spending <- read_csv("data/processed/cluster_assignments.csv",
                          show_col_types = FALSE)

dtw_threat_main <- read_csv("data/processed/dtw_threat_metrics.csv",
                             show_col_types = FALSE) %>%
  select(country, debate_intensity, gov_responsiveness, opp_responsiveness) %>%
  na.omit()

# ── ROBUSTNESS_K: must match optimal k from script 04 ────────────────────────
# Update this value if NbClust in script 04 recommends a different k.
# All three robustness dimensions (seeds, k-sensitivity, variants) use
# this constant as the reference cluster number.
ROBUSTNESS_K <- 4L

# ── Helper: cluster data and return assignments + silhouette ──────────────────
cluster_data <- function(df, k, seed = 123) {
  mat <- df %>%
    column_to_rownames("country") %>%
    scale()
  set.seed(seed)
  km        <- kmeans(mat, centers = k, nstart = 50)
  sil_score <- mean(silhouette(km$cluster, dist(mat))[, 3])
  tibble(
    country = rownames(mat),
    cluster = km$cluster,
    sil     = sil_score
  )
}

# ── 1. STABILITY ACROSS RANDOM SEEDS ─────────────────────────────────────────
cat("Testing seed stability (k =", ROBUSTNESS_K, ")...\n")
seeds <- c(42, 123, 456, 789, 1234)

seed_results <- purrr::map_dfr(seeds, function(s) {
  res <- cluster_data(dtw_threat_main, k = ROBUSTNESS_K, seed = s)
  res %>% mutate(seed = s)
})

# Compute ARI for every pair of seeds
ari_matrix <- matrix(NA, length(seeds), length(seeds))
for (i in seq_along(seeds)) {
  for (j in seq_along(seeds)) {
    c1 <- seed_results$cluster[seed_results$seed == seeds[i]]
    c2 <- seed_results$cluster[seed_results$seed == seeds[j]]
    ari_matrix[i, j] <- adjustedRandIndex(c1, c2)
  }
}
rownames(ari_matrix) <- colnames(ari_matrix) <- paste0("seed_", seeds)
avg_ari_seeds <- mean(ari_matrix[lower.tri(ari_matrix)])
cat("Average ARI across seeds:", round(avg_ari_seeds, 3), "\n")

# ── 2. SENSITIVITY TO k ───────────────────────────────────────────────────────
# Silhouette scores for k = 3 to 7 help confirm that ROBUSTNESS_K is
# the right choice; a clear peak at ROBUSTNESS_K supports the decision.
cat("Testing k sensitivity (k = 3 to 7)...\n")
k_values <- 3:7

k_results <- purrr::map_dfr(k_values, function(k) {
  res <- cluster_data(dtw_threat_main, k = k)
  res %>% mutate(k = k)
})

k_silhouettes <- k_results %>%
  group_by(k) %>%
  summarise(avg_sil = mean(sil), .groups = "drop")

# ── 3. STABILITY ACROSS THREAT VARIANTS ──────────────────────────────────────
cat("Testing variant stability (k =", ROBUSTNESS_K, ")...\n")
variants <- unique(dtw_robust$variant)

# Reference: ROBUSTNESS_K clustering on the main threat metrics
ref_clustering <- cluster_data(dtw_threat_main, k = ROBUSTNESS_K)

variant_results <- purrr::map_dfr(variants, function(v) {
  variant_data <- dtw_robust %>%
    filter(variant == v) %>%
    select(country, debate_intensity, gov_responsiveness, opp_responsiveness) %>%
    na.omit()

  if (nrow(variant_data) < 5) return(NULL)

  res <- cluster_data(variant_data, k = ROBUSTNESS_K)
  res %>% mutate(variant = v)
})

# ARI between the reference and each variant's clustering
ari_variants <- variant_results %>%
  group_by(variant) %>%
  summarise(
    ari_vs_main = {
      common <- intersect(country, ref_clustering$country)
      c1     <- cluster[match(common, country)]
      c2     <- ref_clustering$cluster[match(common, ref_clustering$country)]
      adjustedRandIndex(c1, c2)
    },
    .groups = "drop"
  )

# ── 4. PER-COUNTRY STABILITY ──────────────────────────────────────────────────
# What proportion of variant runs assign each country to the same cluster
# as the reference (main) solution?
country_stability <- variant_results %>%
  left_join(ref_clustering %>%
              rename(main_cluster = cluster) %>%
              select(country, main_cluster),
            by = "country") %>%
  group_by(country) %>%
  summarise(
    n_variants     = n(),
    n_same_as_main = sum(cluster == main_cluster, na.rm = TRUE),
    pct_stable     = round(n_same_as_main / n_variants * 100, 1),
    .groups        = "drop"
  ) %>%
  arrange(pct_stable)

# Merge stability information into the main cluster table
cluster_robustness <- main_threat %>%
  left_join(country_stability, by = "country") %>%
  mutate(is_stable = coalesce(pct_stable >= 66, FALSE))

# ── Save outputs ──────────────────────────────────────────────────────────────
write_csv(cluster_robustness, "data/processed/cluster_robustness.csv")
write_csv(country_stability,  "data/processed/cluster_stability_summary.csv")
write_csv(k_silhouettes,      "data/processed/k_silhouette_scores.csv")

cat("Saved: data/processed/cluster_robustness.csv\n")
cat("Saved: data/processed/cluster_stability_summary.csv\n")
cat("Saved: data/processed/k_silhouette_scores.csv\n")

# ── Report ────────────────────────────────────────────────────────────────────
report <- c(
  "================================================",
  "REPORT: 04b_clustering_robustness.R",
  paste("Run at:", Sys.time()),
  "================================================",
  "",
  "--- NOTE ON K CHOICE ---",
  paste("Robustness tested at k =", ROBUSTNESS_K,
        "(matches main analysis optimal k from NbClust in script 04)"),
  "Rationale: robustness tests must use the same k as the main analysis",
  "  to produce interpretable ARI comparisons.",
  "",
  "--- SEED STABILITY ---",
  paste("Seeds tested:", paste(seeds, collapse = ", ")),
  paste("k used:", ROBUSTNESS_K),
  paste("Average ARI across all seed pairs:", round(avg_ari_seeds, 3)),
  if (avg_ari_seeds > 0.9)
    "OK: Clustering highly stable across random seeds (ARI > 0.9)"
  else if (avg_ari_seeds > 0.7)
    "INFO: Moderate seed stability (ARI 0.7-0.9)"
  else
    "WARNING: Low seed stability — results may depend on initialisation",
  "",
  "--- K SENSITIVITY (silhouette by k) ---",
  capture.output(print(k_silhouettes)),
  paste("Best k by silhouette:",
        k_silhouettes$k[which.max(k_silhouettes$avg_sil)]),
  "",
  "--- VARIANT STABILITY ---",
  capture.output(print(ari_variants)),
  paste("Mean ARI vs main (k =", ROBUSTNESS_K, "):",
        round(mean(ari_variants$ari_vs_main, na.rm = TRUE), 3)),
  "",
  "--- COUNTRY STABILITY ---",
  "Countries with < 66% stability across variants (unstable):",
  capture.output(
    country_stability %>%
      filter(pct_stable < 66) %>%
      print()
  ),
  "",
  "Stable countries (>= 66%):",
  paste(
    cluster_robustness$country[
      !is.na(cluster_robustness$is_stable) & cluster_robustness$is_stable],
    collapse = ", "
  ),
  "",
  "--- OVERALL ROBUSTNESS ---",
  paste("Stable countries:", sum(cluster_robustness$is_stable, na.rm = TRUE),
        "/", nrow(cluster_robustness)),
  paste("Unstable countries:",
        paste(cluster_robustness$country[
          !cluster_robustness$is_stable], collapse = ", ")),
  "",
  "--- FLAGS ---",
  if (avg_ari_seeds < 0.8)
    "WARNING: Low seed stability — report and investigate"
  else "OK: High seed stability",
  if (mean(ari_variants$ari_vs_main, na.rm = TRUE) < 0.7)
    "WARNING: Results sensitive to threat index choice"
  else "OK: Results robust to threat index variants",
  "",
  "--- OUTPUT FILES ---",
  "data/processed/cluster_robustness.csv",
  "data/processed/cluster_stability_summary.csv",
  "data/processed/k_silhouette_scores.csv",
  "",
  "--- DECISION ---",
  "Report ARI and silhouette scores in paper as robustness evidence.",
  "Note any unstable countries in discussion section.",
  "Proceed to 05_comparison_table.R"
)

writeLines(report, "report/04b_clustering_robustness_report.txt")
cat("\nReport written to report/04b_clustering_robustness_report.txt\n")
