# =============================================================================
# mod_overview.R — Tab 1: About
# Static informational panel. No reactive computation.
# =============================================================================

overviewUI <- function(id) {
  ns <- NS(id)

  tagList(
    # ── KPI value boxes ───────────────────────────────────────────────────────
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      value_box(
        title    = "Countries analysed",
        value    = "28",
        showcase = bsicons::bs_icon("flag"),
        theme    = "primary"
      ),
      value_box(
        title    = "Study period",
        value    = "2004 – 2024",
        showcase = bsicons::bs_icon("calendar-range"),
        theme    = "secondary"
      ),
      value_box(
        title    = "Threat typologies",
        value    = "4 clusters",
        showcase = bsicons::bs_icon("diagram-3"),
        theme    = "success"
      ),
      value_box(
        title    = "Security regimes",
        value    = "11 periods",
        showcase = bsicons::bs_icon("shield-exclamation"),
        theme    = "warning"
      )
    ),

    # ── Main content row ──────────────────────────────────────────────────────
    layout_columns(
      col_widths = c(7, 5),

      # Left: abstract
      card(
        card_header("About this application"),
        card_body(
          p(strong("Research question:"),
            "Do EU member states adjust their parliamentary defence rhetoric
            in response to external security threats, and do they differ
            in how responsive they are?"),
          p("This application presents the interactive replication of:"),
          tags$blockquote(
            style = "border-left: 4px solid #1a5e9f; padding-left: 1em;
                     color: #444; font-style: italic;",
            "Penchev, G. (2026). Political Debate as a Filter: Defence
             Policy Dynamics in EU Member States, 2004–2024.",
            tags$em("Eastern Journal of European Studies (EJES), under review.")
          ),
          hr(),
          p(strong("Method:"), "The monthly",
            strong("Regional Threat Index"), "is built from UCDP GED conflict
            fatality data (1992–2024), detect structural breaks using the
            PELT algorithm, and classify 11 security regimes. We extract
            annual government and opposition", strong("defence stance"),
            "scores from Manifesto Project party manifestos (variable",
            code("per104"), "— Military: Positive) combined with ParlGov
            cabinet data. We then compute",
            strong("Dynamic Time Warping (DTW)"), "distances between
            stance and threat series to measure how closely each country's
            political debate tracks external threat dynamics."),
          p(strong("Clusters:"), "NbClust majority-vote k-means clustering
            on the three DTW metrics identifies four threat-response
            typologies and two spending-alignment typologies."),
          hr(),
          p(strong("Navigate using the tabs above:"),
            tags$ul(
              tags$li(bsicons::bs_icon("graph-up-arrow"), " ",
                      strong("Threat Index"), " — UCDP conflict fatalities,
                      security regimes, country-specific exposure"),
              tags$li(bsicons::bs_icon("chat-square-text"), " ",
                      strong("Debate & Spending"), " — EU trends in defence
                      rhetoric and actual spending"),
              tags$li(bsicons::bs_icon("rulers"), " ",
                      strong("DTW Metrics"), " — per-country alignment
                      scores and robustness checks"),
              tags$li(bsicons::bs_icon("map"), " ",
                      strong("Country Typologies"), " — interactive map,
                      cluster profiles, and alluvial flow")
            )
          )
        )
      ),

      # Right: data sources + pipeline
      tagList(
        card(
          card_header("Data sources"),
          card_body(
            tags$table(
              class = "table table-sm table-borderless",
              style = "font-size: 0.9em;",
              tags$thead(
                tags$tr(
                  tags$th("Dataset"), tags$th("Variable"), tags$th("Source")
                )
              ),
              tags$tbody(
                tags$tr(
                  tags$td(strong("UCDP GED v26.1")),
                  tags$td("Conflict fatalities"),
                  tags$td(tags$a("ucdp.uu.se", href = "https://ucdp.uu.se/downloads/",
                                 target = "_blank"))
                ),
                tags$tr(
                  tags$td(strong("Manifesto MPDS2025a")),
                  tags$td(code("per104")),
                  tags$td(tags$a("manifesto-project.wzb.eu",
                                 href = "https://manifesto-project.wzb.eu/",
                                 target = "_blank"))
                ),
                tags$tr(
                  tags$td(strong("ParlGov")),
                  tags$td("Cabinet composition"),
                  tags$td(tags$a("Harvard Dataverse",
                                 href = "https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/2VZ5ZC",
                                 target = "_blank"))
                ),
                tags$tr(
                  tags$td(strong("Eurostat")),
                  tags$td("GDP, COFOG defence"),
                  tags$td(tags$a("ec.europa.eu/eurostat",
                                 href = "https://ec.europa.eu/eurostat",
                                 target = "_blank"))
                ),
                tags$tr(
                  tags$td(strong("SIPRI via WDI")),
                  tags$td("Military spending"),
                  tags$td(tags$a("data.worldbank.org",
                                 href = "https://data.worldbank.org/indicator/MS.MIL.XPND.GD.ZS",
                                 target = "_blank"))
                )
              )
            )
          )
        ),
        card(
          card_header("Replication"),
          card_body(
            p("All scripts and instructions are available at:"),
            tags$a(
              bsicons::bs_icon("github"), " gpenchev/art",
              href   = "https://github.com/gpenchev/art",
              target = "_blank",
              class  = "btn btn-outline-primary btn-sm"
            ),
            p(style = "margin-top: 0.8em;",
              "Pipeline: R · tidyverse · changepoint · dtw · NbClust"),
            p(tags$small(style = "color: #888;",
                         "UCDP GED data © Uppsala University (CC BY 4.0). ",
                         "Manifesto data © WZB Berlin. ",
                         "App: MIT Licence."))
          )
        )
      )
    )
  )
}

overviewServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # No reactive computation — fully static tab
  })
}
