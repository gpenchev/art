# =============================================================================
# mod_typologies.R — Tab 5: Country Typologies
# Sub-panel A: Map + Cluster Profile card
# Sub-panel B: PCA scatter (Figs 8 & 9)
# Sub-panel C: Alluvial flow (Fig 10) + Data table
# =============================================================================

typologiesUI <- function(id) {
  ns <- NS(id)

  tagList(
    navset_card_tab(
      id = ns("typo_tabs"),

      # ── Sub-panel A: Map & Profiles ──────────────────────────────────────────
      nav_panel(
        title = "Map",
        card_body(
          layout_columns(
            col_widths = c(3, 9),

            # Controls + profile card
            tagList(
              card(
                card_header("Display"),
                card_body(
                  radioButtons(
                    ns("map_var"),
                    label   = NULL,
                    choices = c(
                      "Threat cluster"    = "threat",
                      "Spending cluster"  = "spending",
                      "Cluster stability" = "stability"
                    ),
                    selected = "threat"
                  )
                )
              ),
              uiOutput(ns("cluster_profile_card"))
            ),

            # Map
            card(
              card_header(
                "Click a country to see its profile",
                tooltip(
                  bsicons::bs_icon("info-circle"),
                  "Grey = no data (Cyprus, Greece, Hungary, Luxembourg, Malta,
                   Sweden — insufficient opposition stance data for full DTW
                   analysis). UK shown but has no COFOG spending data.",
                  placement = "right"
                )
              ),
              leafletOutput(ns("eu_map"), height = "480px")
            )
          )
        )
      ),

      # ── Sub-panel B: PCA Scatter ──────────────────────────────────────────────
      nav_panel(
        title = "Cluster Scatter",
        card_body(
          layout_columns(
            col_widths = c(3, 9),
            card(
              card_body(
                radioButtons(
                  ns("pca_view"),
                  label   = "Cluster dimension:",
                  choices = c(
                    "Threat response (Fig 9)" = "threat",
                    "Spending alignment (Fig 8)" = "spending"
                  ),
                  selected = "threat"
                ),
                hr(),
                uiOutput(ns("pca_variance_note"))
              )
            ),
            card(
              card_header("PCA of DTW metrics — cluster separation"),
              plotlyOutput(ns("pca_scatter"), height = "440px")
            )
          )
        )
      ),

      # ── Sub-panel C: Alluvial + Table ─────────────────────────────────────────
      nav_panel(
        title = "Flow & Table",
        card_body(
          layout_columns(
            col_widths = c(6, 6),

            card(
              card_header(
                "Threat → Spending cluster flow (Fig 10)",
                tooltip(
                  bsicons::bs_icon("info-circle"),
                  "Shows how countries in each threat-response typology
                   distribute across the two spending-alignment typologies.
                   Width of flow = number of countries.",
                  placement = "right"
                )
              ),
              sankeyNetworkOutput(ns("alluvial_plot"), height = "400px")
            ),

            card(
              card_header("Country typology summary"),
              card_body(
                selectizeInput(
                  ns("table_cluster_filter"),
                  label    = "Filter by threat cluster:",
                  choices  = c("All clusters" = "",
                               "Polarised Reactors",
                               "Disengaged",
                               "Quiet Reactors",
                               "Vocal but Unresponsive"),
                  selected = "",
                  options  = list(placeholder = "All clusters")
                ),
                DTOutput(ns("typology_table"))
              )
            )
          )
        )
      )
    )
  )
}

typologiesServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Load GeoJSON ──────────────────────────────────────────────────────────
    eu_geo <- sf::read_sf("data/eu_nuts0.geojson")   # sf reads GeoJSON via GDAL, no V8 needed

    # ── Reactive: map colour data ─────────────────────────────────────────────
    map_data <- reactive({
      var <- input$map_var
      # Join on NAME (matches dtw_all$country full names) not ISO2 (which are
      # two-letter codes — joining on those produced all-NA and no map colours).
      df  <- sf::st_drop_geometry(eu_geo) %>%
        left_join(
          dtw_all %>% select(NAME = country, threat_cluster_type,
                              spending_cluster_type, is_stable, pct_stable,
                              debate_intensity, gov_responsiveness,
                              opp_responsiveness),
          by = "NAME"
        )

      if (var == "threat") {
        df$fill_label <- as.character(df$threat_cluster_type)   # NA stays NA
      } else if (var == "spending") {
        df$fill_label <- as.character(df$spending_cluster_type) # NA stays NA
      } else {
        df$fill_label <- case_when(
          is.na(df$pct_stable)  ~ NA_character_,     # no data → transparent
          df$pct_stable >= 66   ~ "Stable (≥66%)",
          df$pct_stable >= 34   ~ "Moderate (34–65%)",
          TRUE                  ~ "Unstable (<34%)"
        )
      }
      # fill_label intentionally left as NA for countries outside the analysis —
      # those polygons will be rendered transparent (fillOpacity = 0)
      df
    })

    # ── Leaflet map ────────────────────────────────────────────────────────────
    output$eu_map <- renderLeaflet({
      leaflet(eu_geo,
              options = leafletOptions(minZoom = 3, maxZoom = 8)) %>%
        setView(lng = 15, lat = 54, zoom = 4) %>%
        addTiles(options = tileOptions(opacity = 0)) %>%
        addProviderTiles(providers$CartoDB.PositronNoLabels)
    })

    # ── Update map colours on variable change ─────────────────────────────────
    observe({
      df  <- map_data()
      var <- input$map_var

      # Palettes: NA handled via na.color = "transparent" so no-data countries
      # render as invisible polygons — "No data" removed from domain and legend.
      if (var == "threat") {
        palette <- colorFactor(
          palette  = unname(cluster_palette[c(
            "Polarised Reactors", "Disengaged",
            "Quiet Reactors", "Vocal but Unresponsive")]),
          domain   = c("Polarised Reactors", "Disengaged",
                       "Quiet Reactors", "Vocal but Unresponsive"),
          na.color = "transparent"
        )
      } else if (var == "spending") {
        palette <- colorFactor(
          palette  = unname(cluster_palette[c(
            "Stable Allocators", "Policy Converters")]),
          domain   = c("Stable Allocators", "Policy Converters"),
          na.color = "transparent"
        )
      } else {
        palette <- colorFactor(
          palette  = c("#1a5e9f", "#f0c040", "#c0392b"),
          domain   = c("Stable (≥66%)", "Moderate (34–65%)", "Unstable (<34%)"),
          na.color = "transparent"
        )
      }

      # Per-polygon opacity: 0 for no-data, 0.8 for clustered countries
      fill_opacity <- ifelse(is.na(df$fill_label), 0, 0.8)

      # Tooltip: context-aware label for the current map variable
      var_label <- switch(var,
                          threat    = "Threat cluster",
                          spending  = "Spending cluster",
                          stability = "Stability")
      tooltip_text <- paste0(
        "<b>", eu_geo$NAME, "</b><br>",
        ifelse(is.na(df$fill_label),
               "No data",
               paste0(var_label, ": <b>", df$fill_label, "</b><br>",
                      ifelse(!is.na(df$debate_intensity),
                             paste0("Debate intensity: ",
                                    round(df$debate_intensity, 3), "<br>",
                                    "Gov. responsiveness: ",
                                    round(df$gov_responsiveness, 3)),
                             "")))
      )

      leafletProxy("eu_map") %>%
        clearShapes() %>%
        clearControls() %>%    # remove previous legend before adding new one
        addPolygons(
          data        = eu_geo,
          fillColor   = palette(df$fill_label),
          fillOpacity = fill_opacity,
          color       = "#cccccc",   # light border for all polygons incl. transparent
          weight      = 0.6,
          layerId     = eu_geo$ISO2,
          label       = lapply(tooltip_text, htmltools::HTML),
          labelOptions = labelOptions(
            style     = list("font-size" = "12px"),
            direction = "auto"
          ),
          highlightOptions = highlightOptions(
            weight      = 2,
            color       = "#1a5e9f",
            fillOpacity = 0.95,
            bringToFront = TRUE
          )
        ) %>%
        addLegend(
          position = "bottomright",
          pal      = palette,
          values   = df$fill_label[!is.na(df$fill_label)],   # NA excluded → no "No data" row
          title    = switch(var,
                            threat    = "Threat cluster",
                            spending  = "Spending cluster",
                            stability = "Cluster stability"),
          opacity  = 0.9,
          layerId  = "map_legend"
        )
    })

    # ── Clicked country → profile card ───────────────────────────────────────
    clicked_country <- reactiveVal(NULL)

    observeEvent(input$eu_map_shape_click, {
      iso2  <- input$eu_map_shape_click$id
      cname <- eu_geo$NAME[eu_geo$ISO2 == iso2]
      if (length(cname) > 0) clicked_country(cname[1])
    })

    output$cluster_profile_card <- renderUI({
      cname <- clicked_country()
      if (is.null(cname)) {
        return(card(
          card_body(
            style = "color:#888; font-size:0.9em;",
            bsicons::bs_icon("cursor"), " Click a country on the map
            to see its profile."
          )
        ))
      }

      row <- dtw_all %>% filter(country == cname)

      if (nrow(row) == 0) {
        return(card(card_body(p(em("No data for"), strong(cname)))))
      }

      card(
        card_header(strong(cname)),
        card_body(
          style = "font-size: 0.88em;",

          # ── Threat-response cluster (based on DTW of stance vs threat) ───────
          tags$div(
            style = "margin-bottom: 4px;",
            tags$span(style = "color:#666; font-size:0.82em; display:block;",
                      "Threat-response cluster:"),
            if (!is.na(row$threat_cluster_type)) {
              tags$span(
                class = "badge",
                style = paste0("background-color:",
                               cluster_palette[as.character(row$threat_cluster_type)],
                               "; color:white; font-size:0.82em;"),
                as.character(row$threat_cluster_type)
              )
            } else {
              tags$span(class = "badge bg-secondary",
                        style = "font-size:0.82em;", "No data")
            }
          ),

          # ── Spending-alignment cluster (based on DTW of stance vs spending) ──
          tags$div(
            style = "margin-bottom: 6px;",
            tags$span(style = "color:#666; font-size:0.82em; display:block;",
                      "Spending-alignment cluster:"),
            if (!is.na(row$spending_cluster_type)) {
              tags$span(
                class = "badge",
                style = paste0("background-color:",
                               cluster_palette[as.character(row$spending_cluster_type)],
                               "; color:white; font-size:0.82em;"),
                as.character(row$spending_cluster_type)
              )
            } else {
              tags$span(class = "badge bg-secondary",
                        style = "font-size:0.82em;", "No data")
            }
          ),

          hr(style = "margin: 4px 0 6px 0;"),

          # ── DTW metrics ──────────────────────────────────────────────────────
          tags$table(
            class = "table table-sm table-borderless mb-0",
            tags$tbody(
              tags$tr(
                tags$td(tooltip(
                  tags$span("Debate intensity ",
                            bsicons::bs_icon("info-circle",
                              style = "color:#aaa; font-size:0.8em;")),
                  "DTW distance gov. vs opp. stance. Higher = more polarised.",
                  placement = "right"
                )),
                tags$td(strong(round(row$debate_intensity, 3)))
              ),
              tags$tr(
                tags$td(tooltip(
                  tags$span("Gov. responsiveness ",
                            bsicons::bs_icon("info-circle",
                              style = "color:#aaa; font-size:0.8em;")),
                  "DTW distance gov. stance vs threat index.
                   Lower = more responsive.",
                  placement = "right"
                )),
                tags$td(strong(round(row$gov_responsiveness, 3)))
              ),
              tags$tr(
                tags$td(tooltip(
                  tags$span("Opp. responsiveness ",
                            bsicons::bs_icon("info-circle",
                              style = "color:#aaa; font-size:0.8em;")),
                  "DTW distance opp. stance vs threat index.
                   N/A = no opposition Manifesto data.",
                  placement = "right"
                )),
                tags$td(strong(
                  if (is.na(row$opp_responsiveness)) "N/A"
                  else round(row$opp_responsiveness, 3)
                ))
              ),
              if (!is.na(row$pct_stable))
                tags$tr(
                  tags$td(tooltip(
                    tags$span("Cluster stability ",
                              bsicons::bs_icon("info-circle",
                                style = "color:#aaa; font-size:0.8em;")),
                    "% of robustness variants placing this country
                     in the same cluster. >=66% = stable.",
                    placement = "right"
                  )),
                  tags$td(strong(paste0(row$pct_stable, "%")))
                )
            )
          )
        )
      )
    })

    # ── PCA scatter (Figs 8 & 9) ──────────────────────────────────────────────
    output$pca_variance_note <- renderUI({
      if (input$pca_view == "threat") {
        tags$p(style = "font-size:0.85em; color:#555;",
               "PC1: ", strong(pca_var_threat[1], "%"), " variance",
               tags$br(),
               "PC2: ", strong(pca_var_threat[2], "%"), " variance",
               tags$br(), tags$br(),
               tags$em("PC1 aligns with gov. responsiveness. PC2 with
                        debate intensity."))
      } else {
        tags$p(style = "font-size:0.85em; color:#555;",
               "PC1: ", strong(pca_var_spend[1], "%"), " variance",
               tags$br(),
               "PC2: ", strong(pca_var_spend[2], "%"), " variance",
               tags$br(), tags$br(),
               tags$em("Both PCs capture gov vs opp spending similarity."))
      }
    })

    output$pca_scatter <- renderPlotly({
      use_threat <- input$pca_view == "threat"
      pca_df     <- if (use_threat) pca_threat else pca_spending
      cl_col     <- if (use_threat) "threat_cluster_type" else "spending_cluster_type"
      xlab       <- if (use_threat)
        paste0("PC1 (", pca_var_threat[1], "% var) — Gov. responsiveness")
      else
        paste0("PC1 (", pca_var_spend[1], "% var)")
      ylab       <- if (use_threat)
        paste0("PC2 (", pca_var_threat[2], "% var) — Debate intensity")
      else
        paste0("PC2 (", pca_var_spend[2], "% var)")

      pca_df <- pca_df %>%
        mutate(cluster_col = as.character(.data[[cl_col]]))

      # Compute 90% confidence ellipses per cluster
      ellipses <- purrr::map(unique(pca_df$cluster_col), function(cl) {
        sub <- pca_df %>% filter(cluster_col == cl)
        if (nrow(sub) < 3) return(NULL)
        # Parametric ellipse from covariance matrix
        mu  <- colMeans(sub[, c("PC1", "PC2")])
        cov_m <- cov(sub[, c("PC1", "PC2")])
        theta <- seq(0, 2 * pi, length.out = 100)
        pts   <- t(mu + 1.645 * t(chol(cov_m)) %*% rbind(cos(theta), sin(theta)))
        list(
          type = "scatter", mode = "lines",
          x    = pts[, 1], y = pts[, 2],
          line = list(color = cluster_palette[cl], width = 1, dash = "dot"),
          fill = "toself",
          fillcolor = paste0(
            substr(cluster_palette[cl], 1, 7),
            "20"   # 12% opacity hex suffix
          ),
          showlegend = FALSE,
          hoverinfo  = "skip",
          name       = cl
        )
      }) %>% purrr::compact()

      p <- plot_ly()

      # Add ellipses first (behind points)
      for (ell in ellipses) {
        p <- p %>% add_trace(
          type       = ell$type, mode = ell$mode,
          x          = ell$x,   y    = ell$y,
          line       = ell$line, fill = ell$fill,
          fillcolor  = ell$fillcolor,
          showlegend = FALSE, hoverinfo = "skip"
        )
      }

      # Points
      p <- p %>%
        add_trace(
          data       = pca_df,
          x          = ~PC1, y = ~PC2,
          color      = ~cluster_col,
          colors     = cluster_palette,
          type       = "scatter", mode = "markers+text",
          text       = ~country,
          textposition = "top center",
          textfont   = list(size = 9, color = "#444"),
          marker     = list(size = 9),   # line removed: scalar line.width warns with vector color=
          hovertemplate = paste0(
            "<b>%{text}</b><br>PC1: %{x:.3f}<br>PC2: %{y:.3f}<extra></extra>"
          )
        ) %>%
        layout(
          xaxis = list(title = xlab, zeroline = TRUE,
                       zerolinecolor = "#ddd", gridcolor = "#e9ecef"),
          yaxis = list(title = ylab, zeroline = TRUE,
                       zerolinecolor = "#ddd", gridcolor = "#e9ecef"),
          legend = list(title = list(text = "Cluster"),
                        orientation = "h", x = 0, y = -0.15)
        ) %>%
        apply_plotly_defaults() %>%
        config(displayModeBar = FALSE)

      p
    })

    # ── Sankey flow diagram (Fig 10) — networkD3 ─────────────────────────────
    # Rendered ONCE — selection highlighting is applied via sendCustomMessage
    # (JS mutation of the live SVG) rather than re-rendering, because
    # htmlwidgets::onRender fires only on the initial render; subsequent
    # Shiny re-renders wipe and redraw the SVG without re-running onRender.
    output$alluvial_plot <- renderSankeyNetwork({

      threat_levels   <- c("Polarised Reactors", "Disengaged",
                           "Quiet Reactors",     "Vocal but Unresponsive")
      spending_levels <- c("Stable Allocators", "Policy Converters")
      node_names      <- c(threat_levels, spending_levels)

      nodes <- data.frame(name = node_names, stringsAsFactors = FALSE)

      links <- dtw_all %>%
        filter(!is.na(threat_cluster_type), !is.na(spending_cluster_type)) %>%
        group_by(threat_cluster_type, spending_cluster_type) %>%
        summarise(
          value     = n(),
          countries = paste(sort(country), collapse = ", "),
          .groups   = "drop"
        ) %>%
        mutate(
          source = match(as.character(threat_cluster_type),   node_names) - 1L,
          target = match(as.character(spending_cluster_type), node_names) - 1L
        ) %>%
        select(source, target, value, countries) %>%
        as.data.frame()

      node_colours <- unname(c(
        cluster_palette[threat_levels],
        cluster_palette[spending_levels]
      ))
      colour_js <- networkD3::JS(sprintf(
        'd3.scaleOrdinal().domain(%s).range(%s)',
        jsonlite::toJSON(node_names),
        jsonlite::toJSON(node_colours)
      ))

      input_id <- ns("table_cluster_filter")

      widget <- sankeyNetwork(
        Links       = links,
        Nodes       = nodes,
        Source      = "source",
        Target      = "target",
        Value       = "value",
        NodeID      = "name",
        colourScale = colour_js,
        fontSize    = 12,
        fontFamily  = "Source Sans Pro, sans-serif",
        nodeWidth   = 22,
        nodePadding = 18,
        sinksRight  = TRUE,
        iterations  = 64
      )

      htmlwidgets::onRender(widget, sprintf('
        function(el, x) {
          var DIM_NODE = 0.12, DIM_LINK = 0.04, FULL_LINK = 0.45, REST_LINK = 0.2;

          // ── Helper: apply highlight for a given selected cluster name ─────
          // selected = "" means no filter (reset all to full opacity).
          function highlight(sel) {
            d3.select(el).selectAll(".node rect, .node text")
              .style("opacity", function(d) {
                if (!sel) return 1;
                return (d.node < 4 && d.name !== sel) ? DIM_NODE : 1;
              });
            d3.select(el).selectAll(".link")
              .style("stroke-opacity", function(d) {
                if (!sel) return REST_LINK;
                return d.source.name === sel ? FULL_LINK : DIM_LINK;
              });
          }

          // ── Register custom message handler (selector → Sankey) ───────────
          // R calls session$sendCustomMessage("sankey_select", list(selected="..."))
          Shiny.addCustomMessageHandler("sankey_select", function(msg) {
            highlight(msg.selected || "");
          });

          // ── Floating tooltip ──────────────────────────────────────────────
          var tip = d3.select(el).append("div")
            .style("position",       "absolute")
            .style("background",     "rgba(255,255,255,0.97)")
            .style("border",         "1px solid #ccc")
            .style("border-radius",  "4px")
            .style("padding",        "7px 10px")
            .style("font-size",      "12px")
            .style("pointer-events", "none")
            .style("box-shadow",     "0 2px 6px rgba(0,0,0,0.15)")
            .style("max-width",      "220px")
            .style("line-height",    "1.5")
            .style("display",        "none");

          // ── Link hover ────────────────────────────────────────────────────
          d3.select(el).selectAll(".link")
            .on("mousemove.tip", function(d) {
              var n = d.value;
              tip.style("display", "block")
                .style("left", (d3.event.pageX - el.getBoundingClientRect().left + 12) + "px")
                .style("top",  (d3.event.pageY - el.getBoundingClientRect().top  - 28) + "px")
                .html("<b>" + d.source.name + " \u2192 " + d.target.name + "</b>" +
                      "<br><span style=\'color:#555\'>" + n +
                      (n === 1 ? " country" : " countries") + "</span>" +
                      "<br><span style=\'color:#333\'>" + (d.countries || "") + "</span>");
            })
            .on("mouseout.tip", function() { tip.style("display", "none"); });

          // ── Node hover ────────────────────────────────────────────────────
          d3.select(el).selectAll(".node")
            .on("mousemove.tip", function(d) {
              tip.style("display", "block")
                .style("left", (d3.event.pageX - el.getBoundingClientRect().left + 12) + "px")
                .style("top",  (d3.event.pageY - el.getBoundingClientRect().top  - 28) + "px")
                .html("<b>" + d.name + "</b>" +
                      "<br><span style=\'color:#555\'>" + Math.round(d.value) + " countries</span>" +
                      (d.node < 4
                        ? "<br><span style=\'color:#888;font-size:11px\'>Click to filter table</span>"
                        : ""));
            })
            .on("mouseout.tip", function() { tip.style("display", "none"); });

          // ── Threat node click → update Shiny selector (Sankey → table) ───
          var inputId = "%s";
          d3.select(el).selectAll(".node")
            .on("click", function(d) {
              if (d.node >= 4) return;
              tip.style("display", "none");
              var current = Shiny.shinyapp.$inputValues[inputId] || "";
              var next    = (current === d.name) ? "" : d.name;
              Shiny.setInputValue(inputId, next, {priority: "event"});
            })
            .style("cursor", function(d) {
              return d.node < 4 ? "pointer" : "default";
            });
        }
      ', input_id))
    })

    # ── Push selector state to Sankey via custom message (table → Sankey) ────
    # Fires whenever the selector changes; JS handler applies opacity without
    # re-rendering the widget.
    observe({
      sel <- input$table_cluster_filter
      if (is.null(sel)) sel <- ""
      session$sendCustomMessage("sankey_select", list(selected = sel))
    })

    # ── Data table ────────────────────────────────────────────────────────────
    output$typology_table <- renderDT({
      df <- dtw_all

      # Cluster filter
      if (!is.null(input$table_cluster_filter) &&
          nzchar(input$table_cluster_filter)) {
        df <- df %>%
          filter(as.character(threat_cluster_type) ==
                   input$table_cluster_filter)
      }

      df %>%
        arrange(threat_cluster_type, country) %>%
        select(
          Country         = country,
          `Threat cluster` = threat_cluster_type,
          `Spending cluster` = spending_cluster_type,
          `Debate`        = debate_intensity,
          `Gov resp.`     = gov_responsiveness,
          `Opp resp.`     = opp_responsiveness,
          `Stable`        = is_stable
        ) %>%
        mutate(
          across(c(Debate, `Gov resp.`, `Opp resp.`), ~round(., 3)),
          Stable = if_else(is.na(Stable), "—",
                           if_else(Stable, "✓", "✗"))
        ) %>%
        datatable(
          options  = list(
            pageLength  = 12,
            dom         = "ftp",
            order       = list(list(1, "asc"), list(0, "asc")),
            columnDefs  = list(
              list(className = "dt-center", targets = c(3, 4, 5, 6))
            )
          ),
          rownames   = FALSE,
          class      = "table-sm table-striped table-hover"
        )
    })

  })
}
