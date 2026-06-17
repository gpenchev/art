# =============================================================================
# mod_dtw.R — Tab 4: DTW Metrics
# Sub-panel A: Scatter + alignment chart + horizontal bar chart (Fig 5)
# Sub-panel B: Robustness checks (variant correlations, silhouette)
# =============================================================================

dtwUI <- function(id) {
  ns <- NS(id)

  tagList(
    navset_card_tab(
      id = ns("dtw_tabs"),

      # ── Sub-panel A: Country Profiles ────────────────────────────────────────
      nav_panel(
        title = "Country Profiles",
        card_body(
          layout_columns(
            col_widths = c(4, 4, 4),
            selectizeInput(
              ns("highlight_country"),
              label    = "Highlight country",
              choices  = country_list_28,
              selected = "Estonia",
              options  = list(placeholder = "Type to search...")
            ),
            checkboxInput(ns("clustered_only"),
                          "Clustered countries only (22)",
                          value = TRUE),
            radioButtons(
              ns("bar_sort"),
              label   = "Sort bar chart by:",
              choices = c("Debate intensity" = "debate_intensity",
                          "Gov. responsiveness" = "gov_responsiveness",
                          "Cluster" = "cluster"),
              selected = "debate_intensity",
              inline   = TRUE
            )
          ),

          # Scatter + alignment side by side
          layout_columns(
            col_widths = c(6, 6),
            card(
              card_header(
                "DTW metric space",
                tooltip(
                  bsicons::bs_icon("info-circle"),
                  "X-axis: Debate Intensity — how different are government
                   and opposition trajectories? Higher = more polarised.
                   Y-axis: Gov. Responsiveness — how closely does
                   government rhetoric track the threat index? Lower = more
                   responsive. Point size = Opposition Responsiveness.",
                  placement = "right"
                )
              ),
              plotlyOutput(ns("dtw_scatter"), height = "320px")
            ),
            card(
              card_header(
                "Stance vs Threat alignment",
                tooltip(
                  bsicons::bs_icon("info-circle"),
                  "Time series for the selected country. DTW measures
                   the similarity in shape between these two trajectories,
                   allowing for temporal warping (lags and compressions).",
                  placement = "left"
                )
              ),
              plotlyOutput(ns("alignment_chart"), height = "320px")
            )
          ),

          # Full-width bar chart (Fig 5)
          card(
            card_header("DTW metrics by country (Fig 5 replica)"),
            plotlyOutput(ns("dtw_bars"), height = "360px")
          )
        )
      ),

      # ── Sub-panel B: Robustness ───────────────────────────────────────────────
      nav_panel(
        title = "Robustness",
        card_body(
          layout_columns(
            col_widths = c(6, 6),

            card(
              card_header(
                "Correlation with main analysis",
                tooltip(
                  bsicons::bs_icon("info-circle"),
                  "Pearson r between DTW metrics computed with the main
                   (log fatalities) threat index and two alternatives:
                   per-capita fatalities and distance-weighted fatalities.
                   r > 0.9 = highly robust.",
                  placement = "right"
                )
              ),
              card_body(
                DTOutput(ns("robustness_table")),
                tags$small(
                  style = "color:#888; margin-top:6px; display:block;",
                  "Debate intensity is perfectly stable (r = 1.0) — it
                   depends only on gov vs. opp stance, not on the threat
                   index choice."
                )
              )
            ),

            card(
              card_header(
                "Silhouette score by k",
                tooltip(
                  bsicons::bs_icon("info-circle"),
                  "Average silhouette width for k = 3 to 7 threat clusters.
                   Higher = better-separated clusters. NbClust majority
                   vote selected k = 4.",
                  placement = "left"
                )
              ),
              plotlyOutput(ns("silhouette_chart"), height = "280px")
            )
          )
        )
      )
    )
  )
}

dtwServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    # ── Reactive: filtered data ───────────────────────────────────────────────
    dtw_data <- reactive({
      if (input$clustered_only) {
        dtw_all %>% filter(is_clustered)
      } else {
        dtw_all
      }
    })

    # ── Scatter plot ──────────────────────────────────────────────────────────
    output$dtw_scatter <- renderPlotly({
      df   <- dtw_data()
      high <- input$highlight_country

      # Quadrant lines at median of clustered countries
      med_debate <- median(df$debate_intensity[df$is_clustered], na.rm = TRUE)
      med_gov    <- median(df$gov_responsiveness[df$is_clustered], na.rm = TRUE)

      # Colour: cluster type or grey for non-clustered
      df <- df %>%
        mutate(
          colour = if_else(!is.na(threat_cluster_type),
                           as.character(threat_cluster_type),
                           "No data"),
          size   = if_else(is.na(opp_responsiveness), 5,
                           5 + 12 * norm01(opp_responsiveness)),
          alpha  = if_else(country == high, 1, 0.7)
        )

      p <- plot_ly(df,
                   source     = "dtw_scatter",   # required for event_data() click
                   x          = ~debate_intensity,
                   y          = ~gov_responsiveness,
                   color      = ~colour,
                   colors     = cluster_palette,
                   size       = ~size,
                   text       = ~paste0("<b>", country, "</b><br>",
                                        "Debate: ", round(debate_intensity, 3),
                                        "<br>Gov resp.: ",
                                        round(gov_responsiveness, 3),
                                        "<br>Opp resp.: ",
                                        round(opp_responsiveness, 3)),
                   hoverinfo  = "text",
                   type       = "scatter",
                   mode       = "markers"
                   # Note: marker$line intentionally omitted — passing line.width
                   # as a scalar inside a vectorised size= call triggers a plotly
                   # warning. Cluster colours already distinguish points clearly.
      ) %>%
        # Country labels — explicit mode="text" prevents marker inheritance warning
        add_text(
          data         = df %>% filter(is_clustered | country == high),
          x            = ~debate_intensity,
          y            = ~gov_responsiveness,
          text         = ~country,
          type         = "scatter",
          mode         = "text",
          textposition = "top center",
          showlegend   = FALSE,
          textfont     = list(size = 9, color = "#444"),
          hoverinfo    = "skip",
          inherit      = FALSE
        ) %>%
        # Quadrant lines — inherit=FALSE prevents marker spec propagation
        add_segments(
          x = med_debate, xend = med_debate,
          y = 0, yend = max(df$gov_responsiveness, na.rm = TRUE) * 1.05,
          line = list(color = "#bbb", width = 1, dash = "dot"),
          showlegend = FALSE, hoverinfo = "skip", inherit = FALSE
        ) %>%
        add_segments(
          x = 0, xend = max(df$debate_intensity, na.rm = TRUE) * 1.05,
          y = med_gov, yend = med_gov,
          line = list(color = "#bbb", width = 1, dash = "dot"),
          showlegend = FALSE, hoverinfo = "skip", inherit = FALSE
        ) %>%
        layout(
          xaxis = list(title = "Debate Intensity (Gov vs Opp DTW)",
                       zeroline = FALSE, gridcolor = "#e9ecef"),
          yaxis = list(title = "Gov. Responsiveness (Gov vs Threat DTW)",
                       zeroline = FALSE, gridcolor = "#e9ecef"),
          legend = list(title = list(text = "Cluster type"),
                        orientation = "v", x = 1.02, y = 0.5)
        ) %>%
        apply_plotly_defaults() %>%
        config(displayModeBar = FALSE) %>%
        event_register("plotly_click")   # must be last in chain — return value is the registered plot
    })

    # ── Update highlight country on scatter click ─────────────────────────────
    observeEvent(event_data("plotly_click", source = "dtw_scatter"), {
      click <- event_data("plotly_click", source = "dtw_scatter")
      if (!is.null(click)) {
        clicked_country <- dtw_data()$country[
          which.min(abs(dtw_data()$debate_intensity - click$x) +
                      abs(dtw_data()$gov_responsiveness - click$y))
        ]
        updateSelectizeInput(session, "highlight_country",
                             selected = clicked_country[1])
      }
    })

    # ── Alignment chart ────────────────────────────────────────────────────────
    output$alignment_chart <- renderPlotly({
      req(input$highlight_country)
      cname <- input$highlight_country

      cdata <- stance %>%
        filter(country == cname) %>%
        arrange(year)

      # Get country-specific threat series
      iso3 <- names(iso3_to_name)[iso3_to_name == cname]

      threat_annual_c <- threat_country %>%
        filter(eu_country == iso3) %>%
        mutate(year = year(month)) %>%
        group_by(year) %>%
        summarise(threat_val = sum(dist_weighted_fatalities, na.rm = TRUE),
                  .groups = "drop") %>%
        filter(year >= 2004, year <= 2024) %>%
        mutate(threat_norm = norm01(log(threat_val + 1)))

      # Normalise stance — norm01 returns NA vector (no warning) if all values NA
      gov_norm <- norm01(cdata$gov_stance_locf)
      opp_norm <- norm01(cdata$opp_stance_locf)

      # Determine opp data availability for subtitle note
      has_opp   <- !all(is.na(cdata$opp_stance_locf))
      first_opp <- if (has_opp) min(cdata$year[!is.na(cdata$opp_stance_locf)]) else NA_integer_
      opp_note  <- if (!has_opp) {
        "Opposition data: not available for this country"
      } else if (first_opp > 2004L) {
        paste0("Opposition data available from ", first_opp, " only")
      } else {
        NULL
      }

      # Look up DTW distance for annotation
      dtw_row <- dtw_all %>% filter(country == cname)
      gov_dtw  <- round(dtw_row$gov_responsiveness, 3)
      deb_dtw  <- round(dtw_row$debate_intensity,   3)

      p <- plot_ly() %>%
        add_trace(
          x    = cdata$year, y = gov_norm,
          type = "scatter", mode = "lines+markers",
          name = "Gov. stance (norm.)",
          line = list(color = "#212529", width = 2),
          marker = list(size = 5),
          hovertemplate = "Gov: %{y:.3f}<extra></extra>"
        )

      # Only add opp trace if at least some data exists
      if (has_opp) {
        p <- p %>% add_trace(
          x    = cdata$year, y = opp_norm,
          type = "scatter", mode = "lines+markers",
          name = if (!is.null(opp_note))
                   paste0("Opp. stance (norm., from ", first_opp, ")")
                 else
                   "Opp. stance (norm.)",
          line = list(color = "#888", width = 1.5, dash = "dash"),
          marker = list(size = 4, symbol = "circle-open"),
          hovertemplate = "Opp: %{y:.3f}<extra></extra>"
        )
      }

      p <- p %>%
        add_trace(
          data = threat_annual_c,
          x    = ~year, y = ~threat_norm,
          type = "scatter", mode = "lines",
          name = "Threat (dist-weighted, norm.)",
          line = list(color = "#c0392b", width = 1.5, dash = "dot"),
          hovertemplate = "Threat: %{y:.3f}<extra></extra>"
        )

      # Build title: include opp availability note if relevant
      title_text <- paste0(
        "<b>", cname, "</b>  |  ",
        "Gov-Threat DTW: ", gov_dtw, "  |  Debate DTW: ", deb_dtw,
        if (!is.null(opp_note)) paste0("<br><span style='font-size:10px;color:#888;'>",
                                       opp_note, "</span>") else ""
      )

      p %>%
        layout(
          title = list(text = title_text, font = list(size = 12), x = 0),
          xaxis     = list(title = "", tickmode = "linear", dtick = 2,
                           showgrid = FALSE),
          yaxis     = list(title = "Normalised [0–1]",
                           range = list(-0.05, 1.1), gridcolor = "#e9ecef"),
          hovermode = "x unified"
        ) %>%
        apply_plotly_defaults() %>%
        config(displayModeBar = FALSE)
    })

    # ── Bar chart (Fig 5 replica) ──────────────────────────────────────────────
    output$dtw_bars <- renderPlotly({

      df <- dtw_data() %>%
        filter(!is.na(debate_intensity)) %>%
        {
          if (input$bar_sort == "cluster") {
            arrange(., threat_cluster_type, desc(debate_intensity))
          } else {
            arrange(., desc(.data[[input$bar_sort]]))
          }
        } %>%
        mutate(country = factor(country, levels = rev(unique(country))))

      plot_ly(df) %>%
        add_bars(
          x    = ~debate_intensity, y = ~country,
          name = "Debate Intensity",
          marker = list(color = "#212529"),
          orientation = "h",
          hovertemplate = "Debate: %{x:.3f}<extra></extra>"
        ) %>%
        add_bars(
          x    = ~gov_responsiveness, y = ~country,
          name = "Gov. Responsiveness",
          marker = list(color = "#888"),
          orientation = "h",
          hovertemplate = "Gov resp: %{x:.3f}<extra></extra>"
        ) %>%
        add_bars(
          x    = ~opp_responsiveness, y = ~country,
          name = "Opp. Responsiveness",
          marker = list(color = "#ccc"),
          orientation = "h",
          hovertemplate = "Opp resp: %{x:.3f}<extra></extra>"
        ) %>%
        layout(
          barmode = "group",
          xaxis   = list(title = "DTW distance (lower = more similar)",
                         gridcolor = "#e9ecef"),
          yaxis   = list(title = "", automargin = TRUE),
          legend  = list(orientation = "h", x = 0.3, y = -0.12)
        ) %>%
        apply_plotly_defaults() %>%
        config(displayModeBar = FALSE)
    })

    # ── Robustness correlation table ──────────────────────────────────────────
    output$robustness_table <- renderDT({
      robustness_cors %>%
        select(Variant    = variant_label,
               Metric     = metric_label,
               `Pearson r` = correlation,
               `Mean |diff|` = mean_diff) %>%
        mutate(
          `Pearson r`   = round(`Pearson r`, 3),
          `Mean |diff|` = round(`Mean |diff|`, 4)
        ) %>%
        datatable(
          options  = list(pageLength = 6, dom = "t"),
          rownames = FALSE,
          class    = "table-sm table-striped"
        ) %>%
        formatStyle(
          "Pearson r",
          backgroundColor = styleInterval(
            c(0.7, 0.9),
            c("#f8d7da", "#fff3cd", "#d1e7dd")
          )
        )
    })

    # ── Silhouette chart ──────────────────────────────────────────────────────
    output$silhouette_chart <- renderPlotly({
      optimal_k <- k_sil$k[which.max(k_sil$avg_sil)]

      plot_ly(k_sil,
              x    = ~k, y = ~avg_sil,
              type = "bar",
              marker = list(
                color = if_else(k_sil$k == 4, "#1a5e9f", "#adb5bd")
              ),
              hovertemplate = "k = %{x}<br>Avg silhouette: %{y:.3f}<extra></extra>"
      ) %>%
        layout(
          xaxis = list(title = "Number of clusters (k)",
                       tickmode = "linear", dtick = 1,
                       showgrid = FALSE),
          yaxis = list(title = "Average silhouette width",
                       range = list(0, max(k_sil$avg_sil) * 1.1),
                       gridcolor = "#e9ecef"),
          annotations = list(list(
            xref = "x", yref = "y",
            x    = 4, y = k_sil$avg_sil[k_sil$k == 4] + 0.005,
            text = "NbClust\noptimal k = 4",
            showarrow = TRUE, arrowhead = 2, arrowsize = 0.8,
            ax = 30, ay = -30,
            font = list(size = 10, color = "#1a5e9f")
          ))
        ) %>%
        apply_plotly_defaults() %>%
        config(displayModeBar = FALSE)
    })

  })
}
