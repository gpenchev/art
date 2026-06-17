# =============================================================================
# mod_stance.R — Tab 3: Debate & Spending
# Sub-panel A: EU Trends (Figs 4 & 6 combined with toggle)
# Sub-panel B: Country time series (per-country stance over time)
# =============================================================================

stanceUI <- function(id) {
  ns <- NS(id)

  tagList(
    navset_card_tab(
      id = ns("stance_tabs"),

      # ── Sub-panel A: EU Trends ───────────────────────────────────────────────
      nav_panel(
        title = "EU Trends",
        card_body(
          # Controls
          layout_columns(
            col_widths = c(3, 3, 3, 3),
            radioButtons(
              ns("view_mode"),
              label    = "Overlay on stance:",
              choices  = c("Threat index" = "threat",
                           "Defence spending" = "spending"),
              selected = "threat",
              inline   = TRUE
            ),
            checkboxInput(ns("show_gdpw"),
                          "GDP-weighted averages", value = FALSE),
            conditionalPanel(
              condition = sprintf("input['%s'] == 'spending'", ns("view_mode")),
              checkboxInput(ns("show_sipri"),
                            "SIPRI spending (vs COFOG)", value = TRUE)
            ),
            checkboxInput(ns("show_events_eu"),
                          "Event markers", value = TRUE)
          ),
          # Chart
          plotlyOutput(ns("eu_trends_chart"), height = "420px"),
          tags$small(
            style = "color:#888; display:block; margin-top:4px;",
            "All series normalised to [0–1] for comparability. ",
            "Manifesto Project per104 (Military: Positive) averaged across
             all EU-27 member states."
          )
        )
      ),

      # ── Sub-panel B: Country Time Series ─────────────────────────────────────
      nav_panel(
        title = "Country Time Series",
        card_body(
          layout_columns(
            col_widths = c(8, 4),
            selectizeInput(
              ns("countries_ts"),
              label    = "Countries (max 8)",
              choices  = NULL,    # populated in server with optgroup
              multiple = TRUE,
              selected = c("Estonia", "Germany", "France", "Poland"),
              options  = list(
                maxItems    = 8,
                placeholder = "Select countries...",
                plugins     = list("remove_button")
              )
            ),
            radioButtons(
              ns("stance_type"),
              label   = "Show:",
              choices = c("Government"  = "gov",
                          "Opposition"  = "opp",
                          "Both"        = "both"),
              selected = "both"
            )
          ),
          plotlyOutput(ns("country_ts_chart"), height = "420px"),
          tags$small(
            style = "color:#888; display:block; margin-top:4px;",
            "Stance = Manifesto per104 (% quasi-sentences on military
             positively). LOCF imputation applied between elections.
             Countries grouped by threat cluster type in selector."
          )
        )
      )
    )
  )
}

stanceServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Populate country selector with optgroups ──────────────────────────────
    observe({
      grouped <- split(country_groups$country,
                       country_groups$threat_cluster_type)
      updateSelectizeInput(
        session, "countries_ts",
        choices  = grouped,
        selected = c("Estonia", "Germany", "France", "Poland"),
        server   = TRUE
      )
    })

    # ── Sub-panel A: EU Trends chart ──────────────────────────────────────────
    output$eu_trends_chart <- renderPlotly({

      use_spending <- input$view_mode == "spending"
      show_gdpw    <- input$show_gdpw
      show_sipri   <- isTRUE(input$show_sipri) && use_spending

      data_src <- if (use_spending) eu_spending_trends else eu_trends

      p <- plot_ly()

      # Government stance (always shown)
      p <- p %>% add_trace(
        data = data_src, x = ~year, y = ~gov_norm,
        type = "scatter", mode = "lines+markers",
        name = "Gov. stance (simple avg.)",
        line = list(color = "#212529", width = 2),
        marker = list(size = 5),
        hovertemplate = "Gov (simple): %{y:.3f}<extra></extra>"
      )

      # Opposition stance (always shown)
      p <- p %>% add_trace(
        data = data_src, x = ~year, y = ~opp_norm,
        type = "scatter", mode = "lines+markers",
        name = "Opp. stance (simple avg.)",
        line = list(color = "#6c757d", width = 2),
        marker = list(size = 5),
        hovertemplate = "Opp (simple): %{y:.3f}<extra></extra>"
      )

      # GDP-weighted versions (optional)
      if (show_gdpw) {
        p <- p %>%
          add_trace(
            data = data_src, x = ~year, y = ~gov_gdpw_norm,
            type = "scatter", mode = "lines",
            name = "Gov. stance (GDP-weighted)",
            line = list(color = "#212529", width = 1.5, dash = "dash"),
            hovertemplate = "Gov (GDP-w): %{y:.3f}<extra></extra>"
          ) %>%
          add_trace(
            data = data_src, x = ~year, y = ~opp_gdpw_norm,
            type = "scatter", mode = "lines",
            name = "Opp. stance (GDP-weighted)",
            line = list(color = "#6c757d", width = 1.5, dash = "dash"),
            hovertemplate = "Opp (GDP-w): %{y:.3f}<extra></extra>"
          )
      }

      # Overlay: threat or COFOG defence spending
      if (!use_spending) {
        p <- p %>% add_trace(
          data = eu_trends, x = ~year, y = ~threat_norm,
          type = "scatter", mode = "lines",
          name = "Threat index",
          line = list(color = "#c0392b", width = 1.5, dash = "dot"),
          hovertemplate = "Threat: %{y:.3f}<extra></extra>"
        )
      } else {
        p <- p %>% add_trace(
          data = eu_spending_trends, x = ~year, y = ~def_norm,
          type = "scatter", mode = "lines",
          name = "Defence % GDP (COFOG)",
          line = list(color = "#1a6faf", width = 1.5, dash = "dot"),
          hovertemplate = "COFOG % GDP: %{y:.3f}<extra></extra>"
        )
        if (show_sipri) {
          p <- p %>% add_trace(
            data = eu_spending_trends, x = ~year, y = ~sipri_norm,
            type = "scatter", mode = "lines",
            name = "Defence % GDP (SIPRI)",
            line = list(color = "#2a9d2a", width = 1.5, dash = "longdash"),
            hovertemplate = "SIPRI % GDP: %{y:.3f}<extra></extra>"
          )
        }
      }

      # Event markers
      ann <- if (input$show_events_eu) event_annotations(y_pos = 1.08) else list()
      vl  <- if (input$show_events_eu) event_vlines() else list()

      p %>%
        layout(
          shapes      = vl,
          annotations = ann,
          xaxis = list(title = "", tickmode = "linear", dtick = 2,
                       range = list(2003.5, 2024.5), showgrid = FALSE),
          yaxis = list(title = "Normalised index [0–1]",
                       range = list(0, 1.15), gridcolor = "#e9ecef"),
          hovermode = "x unified"
        ) %>%
        apply_plotly_defaults() %>%
        config(displayModeBar = FALSE)
    })

    # ── Sub-panel B: Country time series ──────────────────────────────────────
    output$country_ts_chart <- renderPlotly({
      req(input$countries_ts)

      sel       <- input$countries_ts
      show_type <- input$stance_type

      # Palette: one colour per country from a qualitative set
      country_colors <- setNames(
        RColorBrewer::brewer.pal(max(3, min(8, length(sel))), "Set2")[seq_along(sel)],
        sel
      )

      p <- plot_ly()

      for (cname in sel) {
        cdata <- stance %>%
          filter(country == cname) %>%
          arrange(year)

        col <- country_colors[cname]

        if (show_type %in% c("gov", "both")) {
          p <- p %>% add_trace(
            data      = cdata,
            x         = ~year, y = ~gov_stance_locf,
            type      = "scatter", mode = "lines+markers",
            name      = if (show_type == "both") paste(cname, "(gov)")
                        else cname,
            line      = list(color = col, width = 2),
            marker    = list(size = 5, color = col),
            legendgroup = cname,
            hovertemplate = paste0("<b>", cname, " gov</b><br>",
                                   "Year: %{x}<br>Stance: %{y:.3f}",
                                   "<extra></extra>")
          )
        }
        if (show_type %in% c("opp", "both")) {
          p <- p %>% add_trace(
            data      = cdata,
            x         = ~year, y = ~opp_stance_locf,
            type      = "scatter", mode = "lines+markers",
            name      = if (show_type == "both") paste(cname, "(opp)")
                        else cname,
            line      = list(color = col, width = 1.5, dash = "dash"),
            marker    = list(size = 4, color = col, symbol = "circle-open"),
            legendgroup = cname,
            showlegend  = show_type != "gov",   # show opp in legend for "opp" and "both"
            hovertemplate = paste0("<b>", cname, " opp</b><br>",
                                   "Year: %{x}<br>Stance: %{y:.3f}",
                                   "<extra></extra>")
          )
        }
      }

      p %>%
        layout(
          xaxis     = list(title = "", tickmode = "linear", dtick = 2,
                           range = list(2003.5, 2024.5), showgrid = FALSE),
          yaxis     = list(title = "Defence stance (per104, %)",
                           gridcolor = "#e9ecef"),
          hovermode = "x unified"
        ) %>%
        apply_plotly_defaults() %>%
        config(displayModeBar = FALSE)
    })

  })
}
