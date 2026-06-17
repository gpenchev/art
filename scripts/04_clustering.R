# =============================================================================
# 04_clustering.R
# Purpose: Determine the optimal number of clusters and run k-means on
#          the DTW metrics to classify EU28 + UK countries into distinct
#          defence-behaviour typologies.
#
# Two independent cluster analyses
# ----------------------------------
# 1. THREAT CLUSTERING
#    Variables: debate_intensity, gov_responsiveness, opp_responsiveness
#    Question:  Which countries share a similar pattern of internal
#               political debate *and* responsiveness to external threats?
#    Result:    Four threat behaviour typologies (see labels below).
#
# 2. SPENDING CLUSTERING
#    Variables: gov_spending_similarity, opp_spending_similarity
#    Question:  Do countries that align rhetorically also align their
#               budgets, or do some "talk the talk" without "walking the
#               walk"?
#    Result:    Two spending-alignment typologies.
#
# Optimal k: NbClust majority vote
# ----------------------------------
# `NbClust` evaluates 26 cluster validity indices (Dunn, Silhouette,
# Calinski-Harabasz, etc.) and takes a majority vote across them to
# recommend the optimal k.  We test k ∈ {2 … 8} and accept the
# majority winner.
#
# Why k-means?
# ------------
# k-means is the standard choice for compact, roughly hyperspherical
# clusters in a low-dimensional (3D or 2D) standardised feature space.
# We set `nstart = 50` to run 50 random initialisations and keep the
# best solution, reducing sensitivity to the starting point.
#
# Seeds
# -----
# `set.seed(42)` before NbClust (index computation) and `set.seed(123)`
# before the final k-means run.  Different seeds separate the
# exploratory phase (NbClust) from the confirmatory phase (final
# assignment).  Seed stability is verified in script 04b.
#
# Silhouette score
# ----------------
# The average silhouette width (from `{cluster}`) summarises cluster
# quality: > 0.5 = good, 0.25–0.5 = weak but acceptable, < 0.25 =
# poorly separated.  Reported in the paper as a quality indicator.
#
# Cluster label logic (threat clusters)
# ----------------------------------------
# Labels are assigned by comparing each cluster's centroid profile
# against the within-run median for debate_intensity and gov_responsiveness:
#
#   High debate + low gov_responsiveness  → "Polarised Reactors"
#     (intense internal disagreement AND government tracks threat closely)
#   High debate + high gov_responsiveness → "Vocal but Unresponsive"
#     (intense internal disagreement AND government ignores threat)
#   Low debate  + low gov_responsiveness  → "Quiet Reactors"
#     (low internal disagreement AND government tracks threat)
#   Low debate  + high gov_responsiveness → "Disengaged"
#     (low internal disagreement AND government ignores threat)
#
# Cluster label logic (spending clusters)
# -----------------------------------------
#   Lower DTW distance (stance mirrors spending) → "Policy Converters"
#   Higher DTW distance (spending decoupled from stance) → "Stable Allocators"
#
# Inputs
# ------
#   data/processed/dtw_threat_metrics.csv   (from script 03)
#   data/processed/dtw_spending_metrics.csv (from script 03)
#
# Outputs
# -------
#   data/processed/cluster_assignments_threat.csv
#   data/processed/cluster_assignments.csv      (spending clusters)
#   data/processed/cluster_labels.csv           (both clusters combined)
#   report/04_clustering_report.txt
#
# References
# ----------
# Charrad, M. et al. (2014). NbClust: An R package for determining the
#   best number of clusters. Journal of Statistical Software, 61(6), 1-36.
# Rousseeuw, P.J. (1987). Silhouettes: a graphical aid to the interpretation
#   and validation of cluster analysis. Journal of Computational and
#   Applied Mathematics, 20, 53-65.
# =============================================================================

if (!require("tidyverse"))  install.packages("tidyverse")
if (!require("NbClust"))    install.packages("NbClust")
if (!require("factoextra")) install.packages("factoextra")
if (!require("cluster"))    install.packages("cluster")

library(tidyverse)
library(NbClust)
library(factoextra)
library(cluster)

# ── Load data ─────────────────────────────────────────────────────────────────
cat("Loading DTW metrics...\n")
dtw_threat   <- read_csv("data/processed/dtw_threat_metrics.csv",
                         show_col_types = FALSE)
dtw_spending <- read_csv("data/processed/dtw_spending_metrics.csv",
                         show_col_types = FALSE)

cat("Threat metrics rows:", nrow(dtw_threat), "\n")
cat("Spending metrics rows:", nrow(dtw_spending), "\n")

# ── 1. THREAT CLUSTERING ──────────────────────────────────────────────────────
cat("\n--- THREAT CLUSTERING ---\n")

# Remove countries with any NA in the three threat metrics
threat_data <- dtw_threat %>%
  select(country, debate_intensity, gov_responsiveness, opp_responsiveness) %>%
  na.omit()

# Z-score standardisation (scale): ensures each metric contributes equally
# regardless of its absolute range.
threat_matrix <- threat_data %>%
  column_to_rownames("country") %>%
  scale()

cat("Countries for threat clustering:", nrow(threat_matrix), "\n")

# NbClust majority vote — tests k = 2 to 8 across 26 validity indices
cat("Running NbClust for threat clustering (this may take a moment)...\n")
set.seed(42)   # reproducibility for NbClust index computations
nb_threat <- NbClust(
  data   = threat_matrix,
  method = "kmeans",
  min.nc = 2,
  max.nc = 8,
  index  = "all"   # use all 26 available indices
)

# Majority vote: most frequently recommended k across all indices
optimal_k_threat <- as.integer(names(which.max(
  table(nb_threat$Best.nc[1, ])
)))
cat("Optimal k (threat):", optimal_k_threat, "\n")

# Final k-means: nstart=50 runs 50 random starts, returns the best
set.seed(123)   # separate seed for the confirmatory run
kmeans_threat <- kmeans(threat_matrix,
                        centers = optimal_k_threat,
                        nstart  = 50)

cluster_assignments_threat <- tibble(
  country       = rownames(threat_matrix),
  cluster_threat = kmeans_threat$cluster
) %>%
  arrange(cluster_threat, country)

# Average silhouette width: quality measure for the chosen partition
sil_threat     <- silhouette(kmeans_threat$cluster, dist(threat_matrix))
avg_sil_threat <- mean(sil_threat[, 3])
cat("Average silhouette (threat):", round(avg_sil_threat, 3), "\n")

# Cluster profile summaries (used for label assignment below)
threat_profiles <- threat_data %>%
  left_join(cluster_assignments_threat, by = "country") %>%
  group_by(cluster_threat) %>%
  summarise(
    n_countries   = n(),
    mean_debate   = round(mean(debate_intensity,   na.rm = TRUE), 3),
    mean_gov_resp = round(mean(gov_responsiveness, na.rm = TRUE), 3),
    mean_opp_resp = round(mean(opp_responsiveness, na.rm = TRUE), 3),
    countries     = paste(sort(country), collapse = ", "),
    .groups       = "drop"
  )

# ── 2. SPENDING CLUSTERING ────────────────────────────────────────────────────
cat("\n--- SPENDING CLUSTERING ---\n")

spending_data <- dtw_spending %>%
  select(country, gov_spending_similarity, opp_spending_similarity) %>%
  na.omit()

spending_matrix <- spending_data %>%
  column_to_rownames("country") %>%
  scale()

cat("Countries for spending clustering:", nrow(spending_matrix), "\n")

cat("Running NbClust for spending clustering...\n")
set.seed(42)
nb_spending <- NbClust(
  data   = spending_matrix,
  method = "kmeans",
  min.nc = 2,
  max.nc = 8,
  index  = "all"
)

optimal_k_spending <- as.integer(names(which.max(
  table(nb_spending$Best.nc[1, ])
)))
cat("Optimal k (spending):", optimal_k_spending, "\n")

set.seed(123)
kmeans_spending <- kmeans(spending_matrix,
                           centers = optimal_k_spending,
                           nstart  = 50)

cluster_assignments_spending <- tibble(
  country = rownames(spending_matrix),
  cluster = kmeans_spending$cluster
) %>%
  arrange(cluster, country)

sil_spending     <- silhouette(kmeans_spending$cluster, dist(spending_matrix))
avg_sil_spending <- mean(sil_spending[, 3])
cat("Average silhouette (spending):", round(avg_sil_spending, 3), "\n")

spending_profiles <- spending_data %>%
  left_join(cluster_assignments_spending, by = "country") %>%
  group_by(cluster) %>%
  summarise(
    n_countries  = n(),
    mean_gov_sim = round(mean(gov_spending_similarity, na.rm = TRUE), 3),
    mean_opp_sim = round(mean(opp_spending_similarity, na.rm = TRUE), 3),
    countries    = paste(sort(country), collapse = ", "),
    .groups      = "drop"
  )

# ── 3. COMBINED CLUSTER LABELS ────────────────────────────────────────────────
cluster_labels <- cluster_assignments_threat %>%
  full_join(cluster_assignments_spending, by = "country") %>%
  arrange(cluster_threat, country)

# ── Save outputs ──────────────────────────────────────────────────────────────
write_csv(cluster_assignments_threat,   "data/processed/cluster_assignments_threat.csv")
write_csv(cluster_assignments_spending, "data/processed/cluster_assignments.csv")
write_csv(cluster_labels,               "data/processed/cluster_labels.csv")

cat("Saved: data/processed/cluster_assignments_threat.csv\n")
cat("Saved: data/processed/cluster_assignments.csv\n")
cat("Saved: data/processed/cluster_labels.csv\n")

# ── Report ────────────────────────────────────────────────────────────────────
report <- c(
  "================================================",
  "REPORT: 04_clustering.R",
  paste("Run at:", Sys.time()),
  "================================================",
  "",
  "--- THREAT CLUSTERING ---",
  paste("Countries analysed:", nrow(threat_matrix)),
  paste("Optimal k (NbClust majority vote):", optimal_k_threat),
  paste("Average silhouette:", round(avg_sil_threat, 3),
        "(>0.5 good; 0.25-0.5 weak; <0.25 poor)"),
  "",
  "Cluster profiles:",
  capture.output(print(threat_profiles, n = Inf)),
  "",
  "--- SPENDING CLUSTERING ---",
  paste("Countries analysed:", nrow(spending_matrix)),
  paste("Optimal k (NbClust majority vote):", optimal_k_spending),
  paste("Average silhouette:", round(avg_sil_spending, 3)),
  "",
  "Cluster profiles:",
  capture.output(print(spending_profiles, n = Inf)),
  "",
  "--- COMBINED ASSIGNMENTS ---",
  capture.output(print(cluster_labels, n = Inf)),
  "",
  "--- FLAGS ---",
  if (avg_sil_threat < 0.3)
    "WARNING: Low silhouette for threat clustering — clusters may overlap"
  else if (avg_sil_threat > 0.5)
    "OK: Good silhouette for threat clustering"
  else "INFO: Moderate silhouette for threat clustering",
  if (avg_sil_spending < 0.3)
    "WARNING: Low silhouette for spending clustering"
  else if (avg_sil_spending > 0.5)
    "OK: Good silhouette for spending clustering"
  else "INFO: Moderate silhouette for spending clustering",
  if (optimal_k_threat != 4)
    paste("INFO: Optimal k for threat =", optimal_k_threat,
          "(paper reports k=4 — review profiles and update paper if needed)")
  else "OK: Optimal k = 4 matches paper",
  if (optimal_k_spending != 2)
    paste("INFO: Optimal k for spending =", optimal_k_spending,
          "(paper reports k=2 — review)")
  else "OK: Optimal k = 2 matches paper",
  "",
  "--- LABEL INTERPRETATION GUIDE ---",
  "Threat cluster labels (data-driven, assigned in script 05):",
  "  Polarised Reactors:     high debate + low gov_resp",
  "  Vocal but Unresponsive: high debate + high gov_resp",
  "  Quiet Reactors:         low debate  + low gov_resp",
  "  Disengaged:             low debate  + high gov_resp",
  "Spending cluster labels:",
  "  Policy Converters: stance closely tracks spending (low DTW dist)",
  "  Stable Allocators: spending decoupled from stance (high DTW dist)",
  "",
  "--- OUTPUT FILES ---",
  "data/processed/cluster_assignments_threat.csv",
  "data/processed/cluster_assignments.csv",
  "data/processed/cluster_labels.csv",
  "",
  "--- DECISION ---",
  "If optimal k differs from paper values: decide whether to update paper.",
  "Review cluster profiles for meaningful substantive interpretation.",
  "Proceed to 04b_clustering_robustness.R"
)

writeLines(report, "report/04_clustering_report.txt")
cat("\nReport written to report/04_clustering_report.txt\n")
