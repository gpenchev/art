# Raw Data — Download Instructions

Place all files in this directory (`data/raw/`) before running the pipeline.

Files listed as **Auto (API)** are downloaded automatically by the scripts.
Files listed as **Manual** must be downloaded by the user.

---

## Manual Downloads

### 1. UCDP GED v25.1 — `ged251.csv`

- **Used by:** `scripts/01_threat_index.R`, `01b_threat_robustness.R`, `01c_gpr_comparison.R`
- **URL:** https://ucdp.uu.se/downloads/
- **Steps:**
  1. Navigate to "UCDP Georeferenced Event Dataset (GED) Global"
  2. Select version **25.1**
  3. Download the **CSV** format (~239 MB)
  4. Save as `data/raw/ged251.csv`
- **Registration:** may be required

---

### 2. Manifesto Project Dataset — `MPDataset_MPDS2025a.csv`

- **Used by:** `scripts/02_manifesto_parlgov.R`
- **URL:** https://manifesto-project.wzb.eu/datasets
- **Steps:**
  1. Register for a free account
  2. Go to **Datasets → Manifesto Corpus Main Dataset**
  3. Download version **MPDS2025a** as CSV
  4. Save as `data/raw/MPDataset_MPDS2025a.csv`
- **Key variable used:** `per104` (Military: Positive)

---

### 3. ParlGov Data — three CSV files

- **Used by:** `scripts/02_manifesto_parlgov.R`, `02b_parlgov_rightleft.R`
- **URL:** https://www.parlgov.org/data/
- **Steps:**
  1. Download the stable (2024) release CSV export
  2. Save the following files:

| Filename | Save as |
|---|---|
| Cabinet view | `data/raw/view_cabinet.csv` |
| Election view | `data/raw/view_election.csv` |
| Party view | `data/raw/view_party.csv` |

---

### 4. GPR Index — `data_gpr_export.xls` *(optional)*

- **Used by:** `scripts/01c_gpr_comparison.R` (optional robustness check only)
- **URL:** https://www.matteoiacoviello.com/gpr.htm
- **Steps:**
  1. Scroll to the data download section
  2. Download the Excel or Stata (.dta) file
  3. Save as `data/raw/data_gpr_export.xls`
- **Note:** Script `01c` is optional; the main pipeline (01 → 05) runs without it.

---

## Automatic Downloads (API)

No manual action needed for these:

| Dataset | Script | Package | Notes |
|---|---|---|---|
| Eurostat GDP (`nama_10_gdp`) | 02c | `{eurostat}` | Cached after first download |
| Eurostat Population (`demo_pjan`) | 02d | `{eurostat}` | Cached after first download |
| Eurostat COFOG Defence (`gov_10a_exp`, `gov_10a_main`) | 02f | `{eurostat}` | Cached |
| World Bank Population (`SP.POP.TOTL`) | 02d | `{WDI}` | One country at a time, rate-limited |
| World Bank SIPRI Spending | 02e | `{WDI}` | One country at a time, rate-limited |
