# Replication Repository

**EU Defence Policy Responses to External Security Threats, 2004–2024**
*Measuring Alignment Between Parliamentary Debate and Conflict Threat via Dynamic Time Warping*

---

## Overview

This repository contains the complete data-processing pipeline for the paper. The pipeline:

1. Builds a **Regional Threat Index** from UCDP GED conflict fatalities using structural break detection (PELT algorithm).
2. Extracts annual **government and opposition defence stance** time series from Manifesto Project data combined with ParlGov cabinet information.
3. Computes **Dynamic Time Warping (DTW)** distances between stance and threat series as the primary analytical metrics.
4. Classifies 28 EU+UK countries into **behavioural typologies** via NbClust + k-means clustering.
5. Performs robustness checks on all three stages.

All scripts are written in **R** and follow a sequential pipeline: `01 → 01b → 01c → 02 → … → 05`. The complete pipeline can be run with a single command (see below).

---

## Repository Structure

```
prepare/
├── README.md               This file
├── run_all.R               Master script — runs the complete pipeline
├── .gitignore
├── scripts/                R processing scripts (14 files)
│   ├── 01_threat_index.R
│   ├── 01b_threat_robustness.R
│   ├── 01c_gpr_comparison.R
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
│   └── 05_comparison_table.R
├── data/
│   ├── raw/                Raw input data (see download instructions below)
│   │   └── README.md
│   └── processed/          Auto-generated outputs (created by scripts)
├── report/                 Auto-generated diagnostic reports (one per script)
└── app/                    Shiny application (planned)
```

---

## Requirements

### R version

R ≥ 4.2.0 is recommended.

### R packages

Install all required packages at once:

```r
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

```r
install.packages("renv")
renv::restore()
```

---

## Data: What to Download Manually

Two datasets **cannot** be downloaded automatically (licence or size constraints) and must be placed in `data/raw/` before running the pipeline. All other data are fetched automatically via API.

---

### 1. UCDP Georeferenced Event Dataset (GED) — **required for scripts 01, 01b, 01c**

| Field | Value |
|---|---|
| Source | Uppsala Conflict Data Program |
| Version | GED Global v26.1 (2026 release, covers 1989–2025) |
| URL | https://ucdp.uu.se/downloads/ged/ged261-csv.zip |
| ZIP contains | `ged261-csv/GEDEvent_v26_1.csv` (≈ 250 MB) |
| Save as | `data/raw/GEDEvent_v26_1.csv` |

**Download and prepare (shell commands):**

```bash
cd data/raw
curl -L -o ged261-csv.zip https://ucdp.uu.se/downloads/ged/ged261-csv.zip
unzip -j ged261-csv.zip
rm ged261-csv.zip
cd ../..
```

Or download manually:
1. Go to https://ucdp.uu.se/downloads/ged/ged261-csv.zip
2. Unzip the archive with the `-j` flag (flat extract — no subdirectory created): `unzip -j ged261-csv.zip`
3. `GEDEvent_v26_1.csv` will appear directly in `data/raw/`
4. Delete the ZIP file

> **Note:** The file is large (~250 MB). No registration required (CC BY 4.0 licence).

---

### 2. Manifesto Project Dataset — **required for script 02**

| Field | Value |
|---|---|
| Source | Manifesto Project (WZB Berlin) |
| Version | MPDS2025a |
| URL | https://manifesto-project.wzb.eu/datasets |
| File to download | `MPDataset_MPDS2025a.csv` |
| Save as | `data/raw/MPDataset_MPDS2025a.csv` |

**Download steps:**
1. Register for a free account at https://manifesto-project.wzb.eu/
2. Go to **Datasets → Main Dataset**
3. Download version **MPDS2025a** as CSV
4. Save the file as `data/raw/MPDataset_MPDS2025a.csv`

> **Note:** Free registration is required. The dataset includes the `per104` variable (Military: Positive) used as the pro-military rhetoric indicator.

---

### 3. ParlGov Data — **required for scripts 02, 02b**

| Field | Value |
|---|---|
| Source | ParlGov (Harvard Dataverse) |
| Download page | https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/2VZ5ZC |

**Download steps (repeat for each of the three files):**

1. Go to https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/2VZ5ZC
2. In the file list on the right, locate `view_cabinet.tab` and click the **download arrow** (↓) next to it
3. Choose **"Comma Separated Values (Original file converted)"**
4. Save as `data/raw/view_cabinet.csv`
5. Repeat for `view_election.tab` → save as `data/raw/view_election.csv`
6. Repeat for `view_party.tab` → save as `data/raw/view_party.csv`

---

### 4. GPR (Geopolitical Risk Index) — **required for script 01c** *(optional robustness check)*

| Field | Value |
|---|---|
| Source | Caldara & Iacoviello (2022) |
| Main page | https://www.matteoiacoviello.com/gpr.htm |
| Direct download | https://www.matteoiacoviello.com/gpr_files/data_gpr_export.xls |
| Save as | `data/raw/data_gpr_export.xls` |

**Download steps:**
1. Direct link: https://www.matteoiacoviello.com/gpr_files/data_gpr_export.xls — click to download
2. Save as `data/raw/data_gpr_export.xls`

   *Alternatively:* go to https://www.matteoiacoviello.com/gpr.htm and download the Excel data file from the data section.

> Script `01c_gpr_comparison.R` is an *optional* robustness check comparing UCDP-based threat with the GPR index. The main pipeline (scripts 01–05) runs without it.

---

### Automatically Downloaded Data (no action required)

The following datasets are fetched automatically when the scripts run:

| Dataset | Script | Package | Notes |
|---|---|---|---|
| Eurostat GDP (`nama_10_gdp`) | 02c | `{eurostat}` | Cached locally after first run |
| Eurostat Population (`demo_pjan`) | 02d | `{eurostat}` | Cached locally |
| Eurostat COFOG Defence (`gov_10a_exp`) | 02f | `{eurostat}` | Cached locally |
| World Bank Population (`SP.POP.TOTL`) | 02d | `{WDI}` | Per-country loop, rate-limited |
| World Bank SIPRI Spending | 02e | `{WDI}` | Per-country loop, rate-limited |

---

## Running the Pipeline

### Option A — Run everything at once

```r
setwd("path/to/prepare")
source("run_all.R")
```

### Option B — Run scripts individually (in order)

```r
setwd("path/to/prepare")
source("scripts/01_threat_index.R")
source("scripts/01b_threat_robustness.R")
source("scripts/01c_gpr_comparison.R")   # optional
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
```

> All scripts must be run from the `prepare/` directory so that relative paths (`data/raw/`, `data/processed/`, `report/`) resolve correctly.

---

## Outputs

After the pipeline completes, `data/processed/` will contain:

| File | Produced by | Description |
|---|---|---|
| `regional_threat_index.csv` | 01 | Monthly EU regional threat index |
| `threat_index_variants.csv` | 01b | Three alternative threat index variants |
| `threat_index_country_specific.csv` | 01b | Distance-weighted threat per EU country |
| `stance_time_series.csv` | 02 | Annual gov/opp defence stance per country |
| `cabinet_rightleft.csv` | 02b | Annual cabinet left-right score |
| `gdp_weights.csv` | 02c | Annual GDP weights (EU-27) |
| `population_combined.csv` | 02d | EU + neighbourhood population |
| `sipri_spending.csv` | 02e | SIPRI military expenditure |
| `defence_gdp_share.csv` | 02f | COFOG defence % GDP and % govt |
| `dtw_metrics.csv` | 03 | Core DTW metrics (3 per country) |
| `dtw_metrics_robustness.csv` | 03b | DTW metrics for alternative variants |
| `cluster_assignments_threat.csv` | 04 | Threat cluster assignments |
| `cluster_assignments.csv` | 04 | Spending cluster assignments |
| `cluster_robustness.csv` | 04b | Stability flags per country |
| `final_comparison_table.csv` | 05 | **Master comparison table** |
| `final_comparison_table_paper.csv` | 05 | Publication-formatted table |

Diagnostic reports are written to `report/` (one `.txt` file per script).

---

## Citation

If you use these scripts, please cite the paper:

> [Author(s)] ([Year]). [Title]. *[Journal]*, [Volume]([Issue]), [Pages]. DOI: [DOI]

And the underlying datasets as listed in the individual script headers.

---

## Licence

Scripts: MIT Licence (see `LICENSE`).
Data: subject to the terms of each data provider (see download instructions above).
