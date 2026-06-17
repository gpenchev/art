# =============================================================================
# mod_threat.R — Tab 2: Regional Threat Index
# Replicates paper Figures 1 & 2 interactively.
# Adds: GPR overlay, country-specific exposure comparison.
# =============================================================================

threatUI <- function(id) {
  ns <- NS(id)

  tagList(
    # ── Controls row ──────────────────────────────────────────────────────────
    card(
      card_body(
        class = "py-2",
        # Row 1: scale radio (inline so label + buttons stay on one line)
        tags$div(
          class = "d-flex align-items-center gap-4 flex-wrap mb-1",
          tags$div(
            class = "d-flex align-items-center gap-2",
            tags$small(tags$b("Y-axis scale:"),
                       style = "white-space:nowrap; color:#495057;"),
            radioButtons(
              ns("scale"), label = NULL, inline = TRUE,
              choices = c(
                "Raw (000s)"   = "raw",
                "Log"          = "log",
                "Index [0–1]"  = "norm"
              ),
              selected = "log"
            )
          )
        ),
        # Row 2: overlay toggles
        tags$div(
          class = "d-flex align-items-center gap-3 flex-wrap",
          checkboxInput(ns("show_regimes"), "Regime bands",  value = TRUE),
          checkboxInput(ns("show_cpts"),    "Changepoints",  value = TRUE),
          checkboxInput(ns("show_events"),  "Key events",    value = TRUE),
          checkboxInput(ns("show_gpr"),     "GPR overlay",   value = FALSE)
        )
      )
    ),

    # ── Main threat time series ───────────────────────────────────────────────
    card(
      card_header("Regional Threat Index — Europe's neighbourhood (1992–2024)"),
      card_body(
        plotlyOutput(ns("threat_main"), height = "380px")
      )
    ),

    # ── Bottom row: regime table + country-specific ───────────────────────────
    layout_columns(
      col_widths = c(5, 7),

      card(
        card_header("Security regimes"),
        card_body(
          DTOutput(ns("regime_table")),
          tags$small(style = "color:#888; margin-top:4px; display:block;",
                     "11 structural breaks detected by PELT algorithm (BIC penalty,
                     min. segment = 24 months). Click a row to highlight on chart.")
        )
      ),

      card(
        card_header("Country-specific threat exposure"),
        card_body(
          layout_columns(
            col_widths = c(6, 6),
            selectizeInput(
              ns("country_threat"),
              label   = "Select EU country",
              choices = country_list_28,
              selected = "Poland",
              options = list(placeholder = "Type to search...")
            ),
            tags$div(
              style = "padding-top: 1.8em; font-size: 0.85em; color: #555;",
              "Distance-weighted fatalities from conflicts in Europe's
               neighbourhood, weighted by inverse great-circle distance
               from the country's capital."
            )
          ),
          plotlyOutput(ns("threat_country_chart"), height = "260px")
        )
      )
    )
  )
}

threatServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    # ── Reactive: y-axis variable and label ───────────────────────────────────
    y_var <- reactive({
      switch(input$scale,
             raw  = "total_fatalities",   # raw casualty count (divided /1000 below)
             log  = "log_fatalities",     # log(fatalities + 1) — emphasises relative change
             norm = "norm_fat")           # min-max of raw fatalities — linear [0,1] index
    })

    y_label <- reactive({
      switch(input$scale,
             raw  = "Casualties (thousands)",
             log  = "Log(fatalities + 1)",
             norm = "Threat index [0–1]")
    })

    y_transform <- reactive({
      if (input$scale == "raw") function(x) x / 1000 else identity
    })

    # ── Main chart ─────────────────────────────────────────────────────────────
    output$threat_main <- renderPlotly({

      y  <- threat_eu[[y_var()]]
      yt <- y_transform()
      yv <- yt(y)

      p <- plot_ly() %>%
        add_trace(
          data      = threat_eu,
          x         = ~month, y = yv,
          type      = "scatter", mode = "lines",
          name      = "Threat index",
          line      = list(color = "#212529", width = 1.5),
          hovertemplate = paste0(
            "<b>%{x|%b %Y}</b><br>",
            y_label(), ": %{y:.2f}<br>",
            "<extra></extra>"
          )
        )

      # Regime bands
      shapes <- list()
      if (input$show_regimes) {
        shapes <- regime_shapes("date")
      }

      # Changepoint vertical lines
      if (input$show_cpts) {
        cpt_dates <- regimes$start_date[-1]  # all but the first regime start
        cpt_shapes <- purrr::map(cpt_dates, function(d) {
          list(type  = "line", xref = "x", yref = "paper",
               x0 = as.character(d), x1 = as.character(d),
               y0 = 0, y1 = 1,
               line = list(color = "#444", width = 0.8, dash = "dot"))
        })
        shapes <- c(shapes, cpt_shapes)
      }

      # Key conflict event annotations
      # label_row 1 → top (y=0.97), label_row 2 → lower (y=0.84) to stagger
      # events that start close together (Libya 2011 / Syria 2011, etc.)
      annotations <- list()
      if (input$show_events) {
        annotations <- purrr::map(seq_len(nrow(key_events)), function(i) {
          y_pos <- if (!is.null(key_events$label_row) && key_events$label_row[i] == 2L) 0.84 else 0.97
          list(
            xref  = "x", yref = "paper",
            x     = as.character(key_events$label_date[i]),
            y     = y_pos,
            text  = gsub("\n", "<br>", key_events$label_text[i]),
            showarrow = FALSE,
            font  = list(size = 9, color = "#555"),
            xanchor = "center", yanchor = "top",
            bgcolor = "rgba(255,255,255,0.75)",
            bordercolor = "#ccc", borderwidth = 1, borderpad = 2
          )
        })
      }

      # GPR overlay — both series normalised to [0,1] on a secondary y-axis
      # so they are visually comparable regardless of the primary scale chosen.
      if (input$show_gpr && has_gpr && nrow(gpr_comparison) > 0) {
        p <- p %>%
          add_trace(
            # Threat re-plotted normalised on yaxis2 (dashed, blue)
            x    = threat_eu$month, y = threat_eu$norm_fat,
            type = "scatter", mode = "lines",
            name = "Threat [0–1]",
            yaxis = "y2",
            line  = list(color = "#1a5e9f", width = 1, dash = "dot"),
            showlegend = TRUE,
            hovertemplate = "Threat [0–1]: %{y:.3f}<extra></extra>"
          ) %>%
          add_trace(
            # GPR index normalised [0,1] on same yaxis2 (dashed, red)
            data = gpr_comparison,
            x    = ~month, y = ~gpr_dist_norm,
            type = "scatter", mode = "lines",
            name = "GPR (Caldara & Iacoviello 2022)",
            yaxis = "y2",
            line  = list(color = "#c0392b", width = 1, dash = "dash"),
            hovertemplate = "GPR [0–1]: %{y:.3f}<extra></extra>"
          )
      }

      # Secondary y-axis only rendered when GPR overlay is active
      yaxis2_def <- if (input$show_gpr && has_gpr && nrow(gpr_comparison) > 0) {
        list(title = "Normalised [0–1]", overlaying = "y", side = "right",
             showgrid = FALSE, range = c(0, 1),
             tickfont = list(size = 10), titlefont = list(size = 11))
      } else {
        list(visible = FALSE)
      }

      p %>%
        layout(
          shapes      = shapes,
          annotations = annotations,
          xaxis  = list(title = "", type = "date",
                        showgrid = FALSE, linecolor = "#dee2e6"),
          yaxis  = list(title = y_label(), gridcolor = "#e9ecef"),
          yaxis2 = yaxis2_def,
          hovermode   = "x unified",
          legend      = list(orientation = "h", x = 0, y = -0.12)
        ) %>%
        apply_plotly_defaults() %>%
        config(displayModeBar = TRUE,
               modeBarButtonsToRemove = c("lasso2d", "select2d"),
               displaylogo = FALSE)
    })

    # ── Regime table ───────────────────────────────────────────────────────────
    output$regime_table <- renderDT({
      regimes %>%
        select(
          `#`        = regime_id,
          Start      = period_start,
          End        = period_end,
          `Mean log` = mean_log_fatalities,
          `Total fat.` = total_fatalities,
          Description  = label
        ) %>%
        mutate(
          `Mean log`   = round(`Mean log`, 2),
          `Total fat.` = formatC(`Total fat.`, format = "d", big.mark = ",")
        ) %>%
        datatable(
          options  = list(
            pageLength = 11, dom = "t",
            columnDefs = list(list(className = "dt-center",
                                   targets    = c(0, 3, 4)))
          ),
          rownames   = FALSE,
          selection  = "single",
          class      = "table-sm table-striped table-hover"
        )
    })

    # ── Country-specific chart ─────────────────────────────────────────────────
    output$threat_country_chart <- renderPlotly({
      req(input$country_threat)

      # Country-specific series (distance-weighted)
      country_data <- threat_country %>%
        filter(country == input$country_threat) %>%
        arrange(month)

      # EU average — use norm_fat (linear [0,1]) to match the main chart index
      eu_avg <- threat_eu %>%
        select(month, norm_fat) %>%
        arrange(month)

      if (nrow(country_data) == 0) {
        return(plot_ly() %>%
                 layout(title = "No country-specific data available"))
      }

      plot_ly() %>%
        add_trace(
          data = country_data,
          x    = ~month, y = ~norm_dist,
          type = "scatter", mode = "lines",
          name = input$country_threat,
          line = list(color = "#1a5e9f", width = 1.5),
          hovertemplate = paste0(input$country_threat,
                                 " (dist-weighted norm): %{y:.3f}<extra></extra>")
        ) %>%
        add_trace(
          data = eu_avg,
          x    = ~month, y = ~norm_fat,
          type = "scatter", mode = "lines",
          name = "EU average",
          line = list(color = "#aaaaaa", width = 1, dash = "dash"),
          hovertemplate = "EU avg [0–1]: %{y:.3f}<extra></extra>"
        ) %>%
        layout(
          xaxis     = list(title = "", showgrid = FALSE),
          yaxis     = list(title = "Normalised threat [0–1]",
                           gridcolor = "#e9ecef"),
          hovermode = "x unified",
          legend    = list(orientation = "h", x = 0, y = -0.2)
        ) %>%
        apply_plotly_defaults() %>%
        config(displayModeBar = FALSE)
    })

  })
}
