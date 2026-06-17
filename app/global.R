# =============================================================================
# global.R
# EU Defence Policy App — Global setup
# Runs once when the app starts; all objects become available to ui.R
# and server.R (and all modules via their parent session).
# =============================================================================

# ── Packages ──────────────────────────────────────────────────────────────────
library(shiny)
library(bslib)
library(tidyverse)
library(lubridate)
library(plotly)
library(leaflet)
library(DT)
library(networkD3)     # interactive Sankey flow diagram (replaces ggalluvial)
library(sf)            # GeoJSON reading via GDAL (replaces geojsonio, no V8 required)

# ── Load pre-processed app data ───────────────────────────────────────────────
# All objects are created by prepare/scripts/06_app_data.R
# They are loaded into the global environment and accessible to all modules.
load("data/app_data.rda")

# ── Load module files ─────────────────────────────────────────────────────────
source("modules/mod_overview.R")
source("modules/mod_threat.R")
source("modules/mod_stance.R")
source("modules/mod_dtw.R")
source("modules/mod_typologies.R")

# ── App-wide theme ────────────────────────────────────────────────────────────
# Bootstrap 5 with EU blue as primary colour.
# Clean academic aesthetic: white background, minimal chrome.
app_theme <- bs_theme(
  version    = 5,
  primary    = "#1a5e9f",   # EU institutional blue
  secondary  = "#6c757d",
  success    = "#2a9d2a",
  warning    = "#e07b00",
  danger     = "#c0392b",
  bg         = "#ffffff",
  fg         = "#212529",
  base_font  = font_google("Source Sans Pro"),
  heading_font = font_google("Source Serif Pro"),
  code_font  = font_google("Source Code Pro"),
  "navbar-bg"           = "#1a5e9f",
  "navbar-light-color"  = "#ffffff",
  "navbar-dark-color"   = "#ffffff",
  "navbar-light-brand-color" = "#ffffff",
  "navbar-dark-brand-color"  = "#ffffff",
  "card-border-color"   = "#dee2e6",
  "card-cap-bg"         = "#f8f9fa"
)

# ── Shared plotly layout defaults ─────────────────────────────────────────────
# Applied to every plotly chart via layout() for visual consistency.
plotly_layout_defaults <- list(
  font       = list(family = "Source Sans Pro, sans-serif", size = 12),
  paper_bgcolor = "rgba(0,0,0,0)",
  plot_bgcolor  = "rgba(0,0,0,0)",
  margin     = list(l = 50, r = 20, t = 40, b = 50),
  legend     = list(orientation = "h", x = 0, y = -0.15,
                    bgcolor = "rgba(255,255,255,0.8)",
                    bordercolor = "#dee2e6", borderwidth = 1),
  xaxis      = list(showgrid = TRUE, gridcolor = "#e9ecef",
                    zeroline = FALSE, linecolor = "#dee2e6"),
  yaxis      = list(showgrid = TRUE, gridcolor = "#e9ecef",
                    zeroline = FALSE, linecolor = "#dee2e6")
)

# ── Shared helper: min-max normalise to [0, 1] ────────────────────────────────
# Used by mod_threat (GPR overlay) and mod_dtw (scatter sizing / alignment).
# Returns NA vector (silently) when all inputs are NA — avoids -Inf/Inf warnings.
norm01 <- function(x) {
  if (all(is.na(x))) return(rep(NA_real_, length(x)))
  rng <- max(x, na.rm = TRUE) - min(x, na.rm = TRUE)
  if (rng == 0) return(rep(0, length(x)))
  (x - min(x, na.rm = TRUE)) / rng
}

# ── Shared helper: apply plotly defaults ──────────────────────────────────────
apply_plotly_defaults <- function(p) {
  p %>% layout(
    font          = plotly_layout_defaults$font,
    paper_bgcolor = plotly_layout_defaults$paper_bgcolor,
    plot_bgcolor  = plotly_layout_defaults$plot_bgcolor,
    margin        = plotly_layout_defaults$margin,
    legend        = plotly_layout_defaults$legend,
    xaxis         = plotly_layout_defaults$xaxis,
    yaxis         = plotly_layout_defaults$yaxis
  )
}

# ── Shared helper: regime background shapes for plotly ────────────────────────
# Returns a list of plotly shape objects (alternating grey/white bands)
# for overlaying security regime periods on any time-series chart.
regime_shapes <- function(x_type = "date") {
  purrr::map(seq_len(nrow(regimes)), function(i) {
    list(
      type    = "rect",
      xref    = "x", yref = "paper",
      x0      = if (x_type == "date")
                  as.character(regimes$start_date[i])
                else year(regimes$start_date[i]),
      x1      = if (x_type == "date")
                  as.character(regimes$end_date[i])
                else year(regimes$end_date[i]),
      y0      = 0, y1 = 1,
      fillcolor = regimes$fill_col[i],
      opacity = 0.5,
      line    = list(width = 0)
    )
  })
}

# ── Shared helper: event marker lines for annual charts ───────────────────────
event_vlines <- function() {
  purrr::map(seq_len(nrow(event_years)), function(i) {
    list(
      type      = "line",
      xref      = "x", yref = "paper",
      x0        = event_years$year[i], x1 = event_years$year[i],
      y0        = 0, y1 = 1,
      line      = list(color = "#555555", width = 1, dash = "dot")
    )
  })
}

event_annotations <- function(y_pos = 1.05) {
  purrr::map(seq_len(nrow(event_years)), function(i) {
    list(
      xref      = "x", yref = "paper",
      x         = event_years$year[i],
      y         = y_pos,
      text      = event_years$label[i],
      showarrow = FALSE,
      font      = list(size = 10, color = "#555555"),
      textangle = -45,
      xanchor   = "left"
    )
  })
}
