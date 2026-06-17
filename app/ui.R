# =============================================================================
# ui.R — EU Defence Policy App
# =============================================================================

ui <- page_navbar(
  title = span(
    bsicons::bs_icon("shield", style = "margin-right:6px; vertical-align:middle;"),
    "EU Defence Debate & Threat Alignment"
  ),
  theme    = app_theme,
  id       = "main_nav",
  bg       = "#1a5e9f",
  inverse  = TRUE,
  fillable = FALSE,

  # ── Tab 1: About ────────────────────────────────────────────────────────────
  nav_panel(
    title = tagList(bsicons::bs_icon("info-circle"), "About"),
    value = "tab_about",
    overviewUI("overview")
  ),

  # ── Tab 2: Threat Index ─────────────────────────────────────────────────────
  nav_panel(
    title = tagList(bsicons::bs_icon("graph-up-arrow"), "Threat Index"),
    value = "tab_threat",
    threatUI("threat")
  ),

  # ── Tab 3: Debate & Spending ────────────────────────────────────────────────
  nav_panel(
    title = tagList(bsicons::bs_icon("chat-square-text"), "Debate & Spending"),
    value = "tab_stance",
    stanceUI("stance")
  ),

  # ── Tab 4: DTW Metrics ──────────────────────────────────────────────────────
  nav_panel(
    title = tagList(bsicons::bs_icon("rulers"), "DTW Metrics"),
    value = "tab_dtw",
    dtwUI("dtw")
  ),

  # ── Tab 5: Country Typologies ───────────────────────────────────────────────
  nav_panel(
    title = tagList(bsicons::bs_icon("map"), "Country Typologies"),
    value = "tab_typologies",
    typologiesUI("typologies")
  ),

  # ── Footer spacer ───────────────────────────────────────────────────────────
  nav_spacer(),
  nav_item(
    tags$small(
      style = "color:rgba(255,255,255,0.7); padding-right:8px;",
      "EJES 2026 | Data: UCDP GED v26.1 · Manifesto MPDS2025a"
    )
  )
)
