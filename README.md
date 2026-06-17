# Replication Repository

**Political Debate as a Filter: Defence Policy Dynamics in EU Member States, 2004–2024**
*Eastern Journal of European Studies (EJES), under review*

------------------------------------------------------------------------

## Overview

This repository contains the complete data-processing pipeline for the paper. The pipeline:

1.  Builds a **Regional Threat Index** from UCDP GED conflict fatalities using structural break detection (PELT algorithm).
2.  Extracts annual **government and opposition defence stance** time series from Manifesto Project data combined with ParlGov cabinet information.
3.  Computes **Dynamic Time Warping (DTW)** distances between stance and threat series as the primary analytical metrics.
4.  Classifies 28 EU+UK countries into **behavioural typologies** via NbClust + k-means clustering.
5.  Performs robustness checks on all three stages.

All scripts are written in **R** and follow a sequential pipeline: `01 → 01b → 01c → 02 → … → 05`. The complete pipeline can be run with a single command (see below).

------------------------------------------------------------------------

## Repository Structure

```         
prepare/
├── README.md               This file
├── run_all.R               Master script — runs the complete pipeline
├── .gitignore
├── renv.lock               Package version lockfile
├── scripts/                R processing scripts (15 files)
│   ├── 01_threat_index.R
│   ├── 01b_threat_robustness.R
│   ├── 01c_gpr_comparison.R    ← optional GPR robustness check
│   ├── 02_manifesto_parlgov.R
│   ├── 02b_parlgov_rightleft.R
│   ├── 02c_gdp_eurostat.R
│   ├── 02d_population.R
│   ├── 02e_sipri_wdi.R
│   ├── 02f_defence_gdp_eurostat.R
│   ├── 03_dtw_metrics.R
│   ├── 03b_dtw_robustness.R
│   ├── 04_clustering.R
│   ├── 04b_clustering_robustness.R
│   ├── 05_comparison_table.R
│   └── 06_app_data.R           ← builds app/data/app_data.rda (run after 01–05)
├── data/
│   ├── raw/                Raw input data (see download instructions below)
│   │   └── README.md
│   └── processed/          Auto-generated outputs (created by scripts)
├── report/                 Auto-generated diagnostic reports (one per script)
└── app/                    Interactive Shiny application
    ├── global.R
    ├── ui.R
    ├── server.R
    ├── data/
    │   ├── app_data.rda        ← built by 06_app_data.R
    │   └── eu_nuts0.geojson
    └── modules/
        ├── mod_overview.R
        ├── mod_threat.R
        ├── mod_stance.R
        ├── mod_dtw.R
        └── mod_typologies.R
```

------------------------------------------------------------------------

## Requirements

### R version

R ≥ 4.2.0 is recommended.

### R packages

Install all required packages at once:

``` r
install.packages(c(
  "tidyverse", "lubridate", "zoo",
  "changepoint",
  "dtw",
  "eurostat", "WDI", "haven",
  "NbClust", "factoextra", "cluster", "mclust",
  "geosphere"
))
```

For fully reproducible package versions, restore the environment from `renv.lock`:

``` r
install.packages("renv")
renv::restore()
```

------------------------------------------------------------------------

## Data: What to Download Manually

Two datasets **cannot** be downloaded automatically (licence or size constraints) and must be placed in `data/raw/` before running the pipeline. All other data are fetched automatically via API.

------------------------------------------------------------------------

### 1. UCDP Georeferenced Event Dataset (GED) — **required for scripts 01, 01b, 01c**

| Field        | Value                                             |
|--------------|---------------------------------------------------|
| Source       | Uppsala Conflict Data Program                     |
| Version      | GED Global v26.1 (2026 release, covers 1989–2025) |
| URL          | https://ucdp.uu.se/downloads/ged/ged261-csv.zip   |
| ZIP contains | `ged261-csv/GEDEvent_v26_1.csv` (≈ 250 MB)        |
| Save as      | `data/raw/GEDEvent_v26_1.csv`                     |

**Download and prepare (shell commands):**

``` bash
cd data/raw
curl -L -o ged261-csv.zip https://ucdp.uu.se/downloads/ged/ged261-csv.zip
unzip -j ged261-csv.zip
rm ged261-csv.zip
cd ../..
```

Or download manually: 1. Go to https://ucdp.uu.se/downloads/ged/ged261-csv.zip 2. Unzip the archive with the `-j` flag (flat extract — no subdirectory created): `unzip -j ged261-csv.zip` 3. `GEDEvent_v26_1.csv` will appear directly in `data/raw/` 4. Delete the ZIP file

> **Note:** The file is large (\~250 MB). No registration required (CC BY 4.0 licence).

------------------------------------------------------------------------

### 2. Manifesto Project Dataset — **required for script 02**

| Field            | Value                                     |
|------------------|-------------------------------------------|
| Source           | Manifesto Project (WZB Berlin)            |
| Version          | MPDS2025a                                 |
| URL              | https://manifesto-project.wzb.eu/datasets |
| File to download | `MPDataset_MPDS2025a.csv`                 |
| Save as          | `data/raw/MPDataset_MPDS2025a.csv`        |

**Download steps:** 1. Register for a free account at https://manifesto-project.wzb.eu/ 2. Go to **Datasets → Main Dataset** 3. Download version **MPDS2025a** as CSV 4. Save the file as `data/raw/MPDataset_MPDS2025a.csv`

> **Note:** Free registration is required. The dataset includes the `per104` variable (Military: Positive) used as the pro-military rhetoric indicator.

------------------------------------------------------------------------

### 3. ParlGov Data — **required for scripts 02, 02b**

| Field | Value |
|------------------------------------|------------------------------------|
| Source | ParlGov (Harvard Dataverse) |
| Download page | https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/2VZ5ZC |

**Download steps (repeat for each of the three files):**

1.  Go to https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/2VZ5ZC
2.  In the file list on the right, locate `view_cabinet.tab` and click the **download arrow** (↓) next to it
3.  Choose **"Comma Separated Values (Original file converted)"**
4.  Save as `data/raw/view_cabinet.csv`
5.  Repeat for `view_election.tab` → save as `data/raw/view_election.csv`
6.  Repeat for `view_party.tab` → save as `data/raw/view_party.csv`

------------------------------------------------------------------------

### 4. GPR (Geopolitical Risk Index) — **required for script 01c** *(optional robustness check)*

| Field | Value |
|------------------------------------|------------------------------------|
| Source | Caldara & Iacoviello (2022) |
| Main page | https://www.matteoiacoviello.com/gpr.htm |
| Direct download | https://www.matteoiacoviello.com/gpr_files/data_gpr_export.xls |
| Save as | `data/raw/data_gpr_export.xls` |

**Download steps:** 1. Direct link: https://www.matteoiacoviello.com/gpr_files/data_gpr_export.xls — click to download 2. Save as `data/raw/data_gpr_export.xls`

*Alternatively:* go to https://www.matteoiacoviello.com/gpr.htm and download the Excel data file from the data section.

> Script `01c_gpr_comparison.R` is an *optional* robustness check comparing UCDP-based threat with the GPR index. The main pipeline (scripts 01–05) runs without it.

------------------------------------------------------------------------

### Automatically Downloaded Data (no action required)

The following datasets are fetched automatically when the scripts run:

| Dataset | Script | Package | Notes |
|------------------|------------------|------------------|------------------|
| Eurostat GDP (`nama_10_gdp`) | 02c | `{eurostat}` | Cached locally after first run |
| Eurostat Population (`demo_pjan`) | 02d | `{eurostat}` | Cached locally |
| Eurostat COFOG Defence (`gov_10a_exp`) | 02f | `{eurostat}` | Cached locally |
| World Bank Population (`SP.POP.TOTL`) | 02d | `{WDI}` | Per-country loop, rate-limited |
| World Bank SIPRI Spending | 02e | `{WDI}` | Per-country loop, rate-limited |

------------------------------------------------------------------------

## Running the Pipeline

### Option A — Run everything at once

``` r
setwd("path/to/prepare")
source("run_all.R")
```

### Option B — Run scripts individually (in order)

``` r
setwd("path/to/prepare")
source("scripts/01_threat_index.R")
source("scripts/01b_threat_robustness.R")
source("scripts/01c_gpr_comparison.R")   # optional GPR robustness check
source("scripts/02_manifesto_parlgov.R")
source("scripts/02b_parlgov_rightleft.R")
source("scripts/02c_gdp_eurostat.R")
source("scripts/02d_population.R")
source("scripts/02e_sipri_wdi.R")
source("scripts/02f_defence_gdp_eurostat.R")
source("scripts/03_dtw_metrics.R")
source("scripts/03b_dtw_robustness.R")
source("scripts/04_clustering.R")
source("scripts/04b_clustering_robustness.R")
source("scripts/05_comparison_table.R")
source("scripts/06_app_data.R")          # builds app_data.rda for the Shiny app
```

> All scripts must be run from the `prepare/` directory so that relative paths (`data/raw/`, `data/processed/`, `report/`) resolve correctly.

> **`06_app_data.R`** reads all outputs from scripts 01–05 and writes `app/data/app_data.rda`. Run it after the main pipeline completes. Estimated run time: \~1 minute.

------------------------------------------------------------------------

## Outputs

After the pipeline completes, `data/processed/` will contain:

| File | Produced by | Description |
|------------------------|------------------------|------------------------|
| `regional_threat_index.csv` | 01 | Monthly EU regional threat index |
| `security_regimes.csv` | 01 | PELT-detected security regime periods |
| `threat_index_variants.csv` | 01b | Three alternative threat index variants |
| `threat_index_country_specific.csv` | 01b | Distance-weighted threat per EU country |
| `distance_weights.csv` | 01b | Geographic distance weights per country pair |
| `gpr_neighbourhood.csv` | 01c | GPR index aggregated for EU neighbourhood *(optional)* |
| `gpr_ucdp_comparison.csv` | 01c | GPR vs UCDP correlation *(optional)* |
| `stance_time_series.csv` | 02 | Annual gov/opp defence stance per country (with LOCF) |
| `party_stance_annual.csv` | 02 | Party-level annual stance before aggregation |
| `cabinet_rightleft.csv` | 02b | Annual cabinet left-right score per country |
| `rightleft_stance_correlation.csv` | 02b | Country-level Pearson r: LR position vs stance |
| `eu_trends_gdp_weighted.csv` | 02c | GDP-weighted EU average stance series |
| `gdp_weights.csv` | 02c | Annual GDP weights (EU-27) |
| `population_combined.csv` | 02d | EU + neighbourhood population |
| `population_eu.csv` | 02d | EU population only |
| `sipri_spending.csv` | 02e | SIPRI military expenditure % GDP per country |
| `sipri_eu_average.csv` | 02e | SIPRI EU average series |
| `defence_gdp_share.csv` | 02f | COFOG defence % GDP and % govt per country |
| `defence_eu_trends.csv` | 02f | COFOG EU average defence trend series |
| `dtw_metrics.csv` | 03 | Core DTW metrics (debate intensity, gov/opp responsiveness) |
| `dtw_threat_metrics.csv` | 03 | DTW threat-response metrics only |
| `dtw_spending_metrics.csv` | 03 | DTW spending-alignment metrics only |
| `dtw_metrics_all.csv` | 03 | Combined threat + spending DTW metrics |
| `dtw_metrics_robustness.csv` | 03b | DTW metrics for alternative threat variants |
| `dtw_robustness_correlations.csv` | 03b | Pearson r between main and variant DTW metrics |
| `cluster_assignments_threat.csv` | 04 | Threat cluster assignments (k=4) |
| `cluster_assignments.csv` | 04 | Spending cluster assignments |
| `cluster_labels.csv` | 04 | Named cluster type labels |
| `k_silhouette_scores.csv` | 04 | Average silhouette width for k=3–7 |
| `cluster_robustness.csv` | 04b | Stability flags per country |
| `cluster_stability_summary.csv` | 04b | Stability summary statistics |
| `cluster_sensitivity.csv` | 04b | Sensitivity analysis results |
| `final_comparison_table.csv` | 05 | **Master comparison table** (all metrics + clusters) |
| `final_comparison_table_paper.csv` | 05 | Publication-formatted version |

Diagnostic reports are written to `report/` (one `.txt` file per script).

------------------------------------------------------------------------

## Shiny Application

An interactive application for exploring all results is included in `app/`. It provides five tabs: Overview, Regional Threat Index, Debate & Spending, DTW Metrics, and Country Typologies (interactive map, PCA scatter, Sankey flow diagram, data table).

### Prerequisites

Run `scripts/06_app_data.R` (or `run_all.R` which includes it as Stage 6) to build `app/data/app_data.rda` before launching the app.

### Running the app

``` r
# From the prepare/ directory:
shiny::runApp("app")
```

### App structure

```         
app/
├── global.R          # loads app_data.rda, sources modules, shared helpers & theme
├── ui.R              # page_navbar with 5 tabs
├── server.R          # delegates to module servers
├── data/
│   ├── app_data.rda        # pre-processed data (built by 06_app_data.R)
│   └── eu_nuts0.geojson    # EU country polygons for leaflet map
└── modules/
    ├── mod_overview.R      # Tab 1: About & data sources
    ├── mod_threat.R        # Tab 2: Regional Threat Index (interactive, GPR overlay)
    ├── mod_stance.R        # Tab 3: Debate & Spending — EU trends + country series
    ├── mod_dtw.R           # Tab 4: DTW metrics, scatter, alignment chart, robustness
    └── mod_typologies.R    # Tab 5: Map, PCA scatter, Sankey flow, data table
```

### Required packages (app only)

``` r
install.packages(c("shiny", "bslib", "bsicons", "tidyverse", "lubridate",
                   "plotly", "leaflet", "DT", "networkD3",
                   "sf", "RColorBrewer", "htmltools", "jsonlite"))
```

> `jsonlite` is used for building the D3 colour scale in the interactive Sankey diagram. It is also a transitive dependency of `shiny` and `plotly`, so it is likely already installed.

### Deploying to Shiny Server

Copy the entire `app/` directory to your Shiny Server apps folder. The app is self-contained: `app_data.rda` and `eu_nuts0.geojson` are bundled in `app/data/` and no network access is required at runtime.

------------------------------------------------------------------------

## Citation

If you use these scripts, please cite the paper:

> Gpenchev, G. (2026). Political Debate as a Filter: Defence Policy Dynamics
> in EU Member States, 2004–2024. *Eastern Journal of European Studies (EJES)*, under review.

And the underlying datasets as listed in the individual script headers.

------------------------------------------------------------------------

## Licence

Scripts: MIT Licence (see `LICENSE`). Data: subject to the terms of each data provider (see download instructions above).

------------------------------------------------------------------------

## Acknowledgements

The R processing scripts and the Shiny interactive application in this repository were written with the assistance of **Claude Sonnet 4.6** (Anthropic), accessed via [**AiderDesk**](https://github.com/hotovo/aider-desk) **v0.70.0**.

All research concepts, analytical decisions, theoretical framework, interpretation of results, and overall supervision of the work are solely the author's.
