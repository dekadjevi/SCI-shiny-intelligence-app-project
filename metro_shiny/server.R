# ============================================================
#  METRO AG SCI Project — server.R
#  Strategic & Competitive Intelligence Shiny App
# ============================================================
#
#  Every renderPlot / renderDT below corresponds to an output
#  ID declared in ui.R. Replace the placeholder ggplot2 code
#  with your actual analysis charts.
#
#  To add your real data:
#  1. Put CSV/RDS files in a /data subfolder
#  2. Load them at the top of this file:
#     df_patents <- readRDS("data/patents.rds")
#     df_reviews <- read.csv("data/reviews.csv")
# ============================================================

library(shiny)
library(ggplot2)
library(dplyr)
library(DT)

# ── METRO color palette ────────────────────────────────────
metro_blue   <- "#003DA5"
metro_yellow <- "#FFD100"
metro_dark   <- "#001F52"
metro_gray   <- "#8896A8"
metro_light  <- "#E8EEF8"

# ── shared ggplot2 theme (apply to every chart) ────────────
theme_metro <- function() {
  theme_minimal(base_size = 12) +
  theme(
    plot.background    = element_rect(fill = "white", color = NA),
    panel.background   = element_rect(fill = "white", color = NA),
    panel.grid.major   = element_line(color = "#E8EEF8", linewidth = 0.4),
    panel.grid.minor   = element_blank(),
    axis.text          = element_text(color = "#5a5a56", size = 10),
    axis.title         = element_text(color = "#5a5a56", size = 10),
    plot.title         = element_text(color = metro_dark, size = 12, face = "bold"),
    plot.subtitle      = element_text(color = metro_gray, size = 10),
    legend.position    = "bottom",
    legend.text        = element_text(color = "#5a5a56", size = 9),
    strip.text         = element_text(color = metro_dark, face = "bold")
  )
}

# ============================================================
#  SERVER
# ============================================================

server <- function(input, output, session) {

  # ── KIT 1 dashboard: selected KIQ ─────────────────────────
  kit1_sel <- reactiveVal("1.4")   # start on the punchline KIQ
  kit1_chart <- reactiveVal(1)     # layer tabs for KIQs in KIT1_TABS (e.g. 1.3)
  lapply(c("1.1","1.2","1.3","1.4"), function(k) {
    id <- paste0("k1nav_", gsub("\\.", "", k))
    observeEvent(input[[id]], { kit1_sel(k); kit1_chart(1) })
  })
  observeEvent(input$k1chart1, kit1_chart(1))
  observeEvent(input$k1chart2, kit1_chart(2))
  observeEvent(input$k1chart3, kit1_chart(3))


  # ── BUSINESS CONTEXT ──────────────────────────────────────

  output$plot_score_strategy <- renderPlot({
    # TODO: replace with your sCore strategy infographic
    # Suggestion: a simple ggplot2 bar or lollipop showing
    # 2021 baseline vs 2030 targets for digital sales %, FSD, etc.
    df <- data.frame(
      metric  = c("Digital sales", "Own-brand share", "FSD growth"),
      current = c(11, 25, 100),
      target  = c(40, 35, 300)
    )
    df_long <- tidyr::pivot_longer(df, -metric,
                                   names_to = "period", values_to = "value")
    ggplot(df_long, aes(x = metric, y = value, fill = period)) +
      geom_col(position = "dodge", width = 0.6) +
      scale_fill_manual(values = c(current = metro_gray, target = metro_blue),
                        labels = c("Current", "2030 target")) +
      labs(title = "sCore 2030 — key targets", x = NULL, y = "%  /  index",
           fill = NULL) +
      theme_metro()
  })

  # ── SCOPE ─────────────────────────────────────────────────

  output$plot_kiq_map <- renderPlot({
    # TODO: replace with your scope → KIT → KIQ diagram
    # Suggestion: use ggplot2 with geom_label or the
    # ggraph package for a tree layout
    df <- data.frame(
      x     = c(2, 1, 2, 3, 0.5, 1, 1.5, 2, 2.5, 3, 3.5),
      y     = c(3, 1, 1, 1, 0,   0, 0,   0, 0,   0, 0),
      label = c("Scope",
                "KIT 1", "KIT 2", "KIT 3",
                "1.1","1.2","1.3","2.1","2.2","3.1","3.2"),
      level = c("scope","kit","kit","kit",
                "kiq","kiq","kiq","kiq","kiq","kiq","kiq")
    )
    ggplot(df, aes(x, y, label = label, color = level)) +
      geom_label(size = 3, fontface = "bold") +
      scale_color_manual(values = c(
        scope = metro_dark, kit = metro_blue, kiq = metro_gray)) +
      theme_void() +
      theme(legend.position = "none")
  })

  # ── KIT 1 — Intelligence dashboard ────────────────────────

  output$kit1_kpis <- renderUI({
    sel <- kit1_sel()
    # The METRO app rating / sentiment KPIs come from KIQ 1.4 (app reviews).
    # Showing them on other KIQs would be misleading, so they appear only on 1.4.
    if (sel == "1.4") {
      k <- KIT1_KPIS
      div(class = "kit1-kpis",
        div(class = "kit1-kpi", div(class="kk-v", paste0(k$avg, "\u2605")), div(class="kk-l","METRO app rating")),
        div(class = "kit1-kpi", div(class="kk-v", k$neg),  div(class="kk-l","Negative reviews")),
        div(class = "kit1-kpi", div(class="kk-v", k$auth), div(class="kk-l","Auth / login complaints")),
        div(class = "kit1-kpi", div(class="kk-v", k$n),    div(class="kk-l","Reviews analysed"))
      )
    } else {
      # Context KPIs per KIQ (method / corpus), not the app rating
      ctx <- switch(sel,
        "1.1" = list(c("SCOPUS","Evidence base"), c("2 layers","Capability framework"),
                     c("Both","Layers covered by METRO"), c("Personalization","Identified gap")),
        "1.2" = list(c("Espacenet","Evidence base"), c("~250","Patents analysed"),
                     c("2022\u201326","Window"), c("G06Q","Dominant IPC")),
        "1.3" = list(c("SCOPUS","Evidence base"), c("Co-occurrence","Method"),
                     c("Satisfaction","Structural centre"), c("Periphery","Where tech sits")),
        list(c("","")))
      div(class = "kit1-kpis",
        lapply(ctx, function(p)
          div(class = "kit1-kpi kit1-kpi-ctx",
            div(class="kk-v", p[1]), div(class="kk-l", p[2])))
      )
    }
  })

  output$kit1_rail <- renderUI({
    cur <- kit1_sel()
    tagList(lapply(c("1.1","1.2","1.3","1.4"), function(k) {
      m <- KIT1_META[[k]]; id <- paste0("k1nav_", gsub("\\.", "", k))
      cls <- paste("kit1-railbtn", if (k == cur) "active" else "")
      tags$button(class = cls,
        onclick = sprintf("Shiny.setInputValue('%s', Math.random())", id),
        m$tag, tags$span(class = "rb-t", m$title))
    }))
  })

  output$kit1_panel <- renderUI({
    k <- kit1_sel(); m <- KIT1_META[[k]]
    paths <- KIT1_IMAGES[[k]]
    # keep only images that actually exist (a not-yet-added screenshot leaves no gap)
    uris <- Filter(Negate(is.null), lapply(paths, img_uri))
    labels <- KIT1_TABS[[k]]

    # tabbed layer view (e.g. KIQ 1.3: co-occurrence vs occurrence vs word cloud)
    if (!is.null(labels) && length(uris) >= 2) {
      sel_chart <- max(1, min(kit1_chart(), length(uris)))
      chart_tabs <- div(class = "step-toggle",
        lapply(seq_along(uris), function(i)
          tags$button(class = paste("step-btn", if (i == sel_chart) "active" else ""),
            onclick = sprintf("Shiny.setInputValue('k1chart%d', Math.random())", i),
            labels[i]))
      )
      return(tagList(
        div(class = "kit1-ptag", m$tag),
        div(class = "kit1-ptitle", m$title),
        div(class = "kit1-pq", m$q),
        chart_tabs,
        div(class = "kit1-charts solo",
          div(class = "kit1-cell", tags$img(src = uris[[sel_chart]]))),
        div(class = "kit1-ftr",
          div(class = "kit1-ins", tags$b("Insight \u2014 "), m$insight),
          div(class = "kit1-dec", tags$b("Decision \u2014 "), m$decision)
        )
      ))
    }

    # default grid view
    cells <- if (length(uris) == 0)
      list(div(class = "kit1-cell empty", "chart not found"))
    else lapply(uris, function(u) div(class = "kit1-cell", tags$img(src = u)))
    grid_cls <- if (length(uris) >= 2) "kit1-charts two" else "kit1-charts one"
    tagList(
      div(class = "kit1-ptag", m$tag),
      div(class = "kit1-ptitle", m$title),
      div(class = "kit1-pq", m$q),
      div(class = grid_cls, cells),
      div(class = "kit1-ftr",
        div(class = "kit1-ins", tags$b("Insight \u2014 "), m$insight),
        div(class = "kit1-dec", tags$b("Decision \u2014 "), m$decision)
      )
    )
  })

  # ── KIT 2 — Early-warning console (2.1, 2.2, 2.3 wired) ──
  kit2_sel  <- reactiveVal("2.3")    # all three indicators wired
  kit2_step <- reactiveVal("step1")  # two-step narrative for 2.3
  kit2_chart <- reactiveVal(1)       # one-chart-per-tab for 2.1 / 2.2
  lapply(c("2.1","2.2","2.3"), function(k) {
    id <- paste0("k2nav_", gsub("\\.", "", k))
    observeEvent(input[[id]], { kit2_sel(k); kit2_step("step1"); kit2_chart(1) })
  })
  observeEvent(input$k2step1, kit2_step("step1"))
  observeEvent(input$k2step2, kit2_step("step2"))
  observeEvent(input$k2chart1, kit2_chart(1))
  observeEvent(input$k2chart2, kit2_chart(2))
  observeEvent(input$k2chart3, kit2_chart(3))

  output$kit2_kpis <- renderUI({
    kpis <- switch(kit2_sel(),
                   "2.1" = KIT2_KIQ21_KPIS,
                   "2.2" = KIT2_KIQ22_KPIS,
                   "2.3" = KIT2_KIQ23_KPIS,
                   NULL)
    if (is.null(kpis)) return(NULL)
    div(class = "kit1-kpis",
      lapply(kpis, function(p)
        div(class = "kit1-kpi",
          div(class = "kk-v", p[[1]]), div(class = "kk-l", p[[2]])))
    )
  })

  output$kit2_rail <- renderUI({
    cur <- kit2_sel()
    items <- list(c("2.1","Tech trends",""),
                  c("2.2","Competitor IP",""),
                  c("2.3","Customer expectations",""))
    tagList(lapply(items, function(it) {
      k <- it[1]; id <- paste0("k2nav_", gsub("\\.", "", k))
      disabled <- nzchar(it[3])
      cls <- paste("kit1-railbtn", if (k == cur) "active" else "",
                   if (disabled) "disabled" else "")
      tags$button(class = cls,
        onclick = if (disabled) NULL else sprintf("Shiny.setInputValue('%s', Math.random())", id),
        disabled = if (disabled) "disabled" else NULL,
        paste0("KIQ ", k),
        tags$span(class = "rb-t", it[2], if (disabled) " · soon" else ""))
    }))
  })

  output$kit2_panel <- renderUI({
    sel <- kit2_sel()

    # ── KIQ 2.3 — two-step narrative ──
    if (sel == "2.3") {
      step <- kit2_step(); d <- KIT2_KIQ23[[step]]
      uris <- Filter(Negate(is.null), lapply(d$imgs, img_uri))
      stack <- identical(step, "step1")
      cells <- if (length(uris) == 0)
        list(div(class = "kit1-cell empty", "chart not found"))
      else lapply(uris, function(u) div(class = "kit1-cell", tags$img(src = u)))
      grid_cls <- if (stack) "kit1-charts stack"
                  else if (length(uris) >= 3) "kit1-charts three"
                  else if (length(uris) == 2) "kit1-charts two"
                  else "kit1-charts one"
      return(tagList(
        div(class = "kit1-ptag", "KIQ 2.3 \u00b7 Customer expectations"),
        div(class = "kit1-ptitle", "How are customer expectations evolving?"),
        div(class = "kit1-pq", "BERTopic + K-means on the SCOPUS customer-experience corpus"),
        div(class = "step-toggle",
          tags$button(class = paste("step-btn", if (step=="step1") "active" else ""),
            onclick = "Shiny.setInputValue('k2step1', Math.random())", KIT2_KIQ23$step1$label),
          tags$button(class = paste("step-btn", if (step=="step2") "active" else ""),
            onclick = "Shiny.setInputValue('k2step2', Math.random())", KIT2_KIQ23$step2$label)
        ),
        div(class = "step-blurb", d$blurb),
        div(class = grid_cls, cells),
        div(class = "kit1-ftr",
          div(class = "kit1-ins", tags$b("Insight \u2014 "), d$insight),
          div(class = "kit1-dec", tags$b("Decision \u2014 "), d$decision)
        )
      ))
    }

    # ── KIQ 2.1 / 2.2 — one chart per tab, takeaway pinned below ──
    d <- switch(sel, "2.1" = KIT2_KIQ21, "2.2" = KIT2_KIQ22, NULL)
    if (is.null(d))
      return(div(class = "viz-missing", "This indicator is being finalised."))
    uris <- Filter(Negate(is.null), lapply(d$imgs, img_uri))
    if (length(uris) == 0)
      return(div(class = "kit1-cell empty", "chart not found"))
    sel_chart <- max(1, min(kit2_chart(), length(uris)))
    chart_tabs <- div(class = "step-toggle",
      lapply(seq_along(uris), function(i)
        tags$button(class = paste("step-btn", if (i == sel_chart) "active" else ""),
          onclick = sprintf("Shiny.setInputValue('k2chart%d', Math.random())", i),
          d$chart_labels[i]))
    )
    tagList(
      div(class = "kit1-ptag", d$ptag),
      div(class = "kit1-ptitle", d$ptitle),
      div(class = "kit1-pq", d$pq),
      chart_tabs,
      div(class = "kit1-charts solo",
        div(class = "kit1-cell", tags$img(src = uris[[sel_chart]]))),
      div(class = "kit1-ftr",
        div(class = "kit1-ins", tags$b("Insight \u2014 "), d$insight),
        div(class = "kit1-dec", tags$b("Decision \u2014 "), d$decision)
      )
    )
  })

  # ── KIT 3 — Key players & positioning ────────
  kit3_sel   <- reactiveVal("3.1")
  kit3_chart <- reactiveVal(1)
  lapply(c("3.1","3.2","3.3"), function(k) {
    id <- paste0("k3nav_", gsub("\\.", "", k))
    observeEvent(input[[id]], { kit3_sel(k); kit3_chart(1) })
  })
  observeEvent(input$k3chart1, kit3_chart(1))
  observeEvent(input$k3chart2, kit3_chart(2))
  observeEvent(input$k3chart3, kit3_chart(3))
  observeEvent(input$k3chart4, kit3_chart(4))

  output$kit3_kpis <- renderUI({
    kpis <- switch(kit3_sel(), "3.1" = KIT3_KIQ31_KPIS,
                   "3.2" = KIT3_KIQ32_KPIS, "3.3" = KIT3_KIQ33_KPIS, NULL)
    if (is.null(kpis)) return(NULL)
    div(class = "kit1-kpis",
      lapply(kpis, function(pp)
        div(class = "kit1-kpi",
          div(class = "kk-v", pp[[1]]), div(class = "kk-l", pp[[2]]))))
  })

  output$kit3_rail <- renderUI({
    cur <- kit3_sel()
    items <- list(c("3.1","Main competitors",""),
                  c("3.2","Feature comparison",""),
                  c("3.3","Customer perception",""))
    tagList(lapply(items, function(it) {
      k <- it[1]; id <- paste0("k3nav_", gsub("\\.", "", k))
      disabled <- nzchar(it[3])
      cls <- paste("kit1-railbtn", if (k == cur) "active" else "",
                   if (disabled) "disabled" else "")
      tags$button(class = cls,
        onclick = if (disabled) NULL else sprintf("Shiny.setInputValue('%s', Math.random())", id),
        disabled = if (disabled) "disabled" else NULL,
        paste0("KIQ ", k),
        tags$span(class = "rb-t", it[2], if (disabled) " \u00b7 soon" else ""))
    }))
  })

  output$kit3_panel <- renderUI({
    d <- switch(kit3_sel(), "3.1" = KIT3_KIQ31, "3.2" = KIT3_KIQ32,
                "3.3" = KIT3_KIQ33, NULL)
    if (is.null(d))
      return(div(class = "viz-missing", "This indicator is being finalised."))
    layers <- d$layers; n <- length(layers)
    selc <- max(1, min(kit3_chart(), n))
    tabs <- div(class = "step-toggle",
      lapply(seq_len(n), function(i)
        tags$button(class = paste("step-btn", if (i == selc) "active" else ""),
          onclick = sprintf("Shiny.setInputValue('k3chart%d', Math.random())", i),
          layers[[i]]$label)))
    tagList(
      div(class = "kit1-ptag", d$ptag),
      div(class = "kit1-ptitle", d$ptitle),
      div(class = "kit1-pq", d$pq),
      tabs,
      kit3_render_layer(d, layers[[selc]])
    )
  })

  # ── KIQ 3.3 interactive Plotly charts (fallback to PNG if plotly absent) ──
  if (requireNamespace("plotly", quietly = TRUE)) {
    .k3col <- c("METRO app" = "#14365c", "Choco" = "#2E7D5B", "REKKI" = "#7E3F92")

    output$kit3_pl_feat <- plotly::renderPlotly({
      df <- readr::read_csv(kpath("KIQ_3_3", "kiq33_feature_mentions.csv"), show_col_types = FALSE)
      feats <- setdiff(names(df), "competitor")
      pp <- plotly::plot_ly()
      for (e in df$competitor) {
        vals <- unlist(df[df$competitor == e, feats], use.names = FALSE)
        pp <- plotly::add_trace(pp, type = "bar", orientation = "h", name = e,
          y = feats, x = vals, marker = list(color = .k3col[[e]]),
          hovertemplate = paste0("<b>", e, "</b><br>%{y}<br>%{x}% of reviews<extra></extra>"))
      }
      plotly::layout(pp, barmode = "group", margin = list(l = 175, r = 20, t = 10, b = 40),
        xaxis = list(title = "Share of reviews mentioning the feature (%)"),
        yaxis = list(autorange = "reversed"),
        legend = list(orientation = "h", y = -0.18, x = 0.5, xanchor = "center"),
        font = list(family = "Montserrat, sans-serif"))
    })

    output$kit3_pl_avg <- plotly::renderPlotly({
      ent  <- c("METRO app","Choco","REKKI")
      base <- list(avg = c(2.46, 4.25, 4.06), nn = c(749, 239, 115))
      d <- tryCatch({
        rv <- readr::read_csv(dpath("KIQ_3_3", "raw", "kiq33_reviews_raw.csv"), show_col_types = FALSE)
        rv <- rv[!is.na(rv$rating), ]
        rv$entity <- ifelse(grepl("^METRO", rv$competitor), "METRO app", rv$competitor)
        list(avg = sapply(ent, function(e) round(mean(rv$rating[rv$entity == e]), 2)),
             nn  = sapply(ent, function(e) sum(rv$entity == e)))
      }, error = function(e) base)
      if (any(!is.finite(unlist(d$avg)))) d <- base
      pp <- plotly::plot_ly(x = as.numeric(d$avg), y = ent, type = "bar", orientation = "h",
        marker = list(color = unname(.k3col[ent])),
        text = paste0(as.numeric(d$avg), "★ (n=", as.numeric(d$nn), ")"), textposition = "outside",
        hovertemplate = paste0("%{y}<br>%{x}★ (n=", as.numeric(d$nn), ")<extra></extra>"))
      plotly::layout(pp, margin = list(l = 110, r = 60, t = 10, b = 40),
        xaxis = list(title = "Average rating (1–5 stars)", range = c(0, 5)),
        yaxis = list(autorange = "reversed"),
        shapes = list(list(type = "line", x0 = 3, x1 = 3, y0 = -0.5, y1 = 2.5,
          line = list(dash = "dot", color = "#999"))),
        showlegend = FALSE, font = list(family = "Montserrat, sans-serif"))
    })

    output$kit3_pl_sent <- plotly::renderPlotly({
      ent  <- c("METRO app","Choco","REKKI")
      base <- list(pos = c(32, 80, 77), neu = c(8.5, 2, 0), neg = c(59.5, 18, 23))
      d <- tryCatch({
        rv <- readr::read_csv(dpath("KIQ_3_3", "raw", "kiq33_reviews_raw.csv"), show_col_types = FALSE)
        rv <- rv[!is.na(rv$rating), ]
        rv$entity <- ifelse(grepl("^METRO", rv$competitor), "METRO app", rv$competitor)
        f <- function(e) { r <- rv$rating[rv$entity == e]; n <- length(r)
          c(pos = 100 * sum(r >= 4) / n, neu = 100 * sum(r == 3) / n, neg = 100 * sum(r <= 2) / n) }
        m <- sapply(ent, f)
        list(pos = round(as.numeric(m["pos", ]), 1),
             neu = round(as.numeric(m["neu", ]), 1),
             neg = round(as.numeric(m["neg", ]), 1))
      }, error = function(e) base)
      if (any(!is.finite(d$pos))) d <- base
      pp <- plotly::plot_ly()
      pp <- plotly::add_trace(pp, type="bar", orientation="h", name="Positive", y=ent, x=d$pos, marker=list(color="#2E7D5B"))
      pp <- plotly::add_trace(pp, type="bar", orientation="h", name="Neutral",  y=ent, x=d$neu, marker=list(color="#b9b9b4"))
      pp <- plotly::add_trace(pp, type="bar", orientation="h", name="Negative", y=ent, x=d$neg, marker=list(color="#b5292f"))
      plotly::layout(pp, barmode="stack", margin=list(l=110, r=20, t=10, b=40),
        xaxis=list(title="Share of reviews (%)", range=c(0,100)),
        yaxis=list(autorange="reversed"),
        legend=list(orientation="h", y=-0.18, x=0.5, xanchor="center"),
        font=list(family="Montserrat, sans-serif"))
    })

    output$kit3_pl_pain <- plotly::renderPlotly({
      df <- readr::read_csv(kpath("KIQ_3_3", "kiq33_painpoints_summary.csv"), show_col_types = FALSE)
      themes <- setdiff(names(df), c("competitor", "n"))
      pp <- plotly::plot_ly()
      for (e in df$competitor) {
        vals <- unlist(df[df$competitor == e, themes], use.names = FALSE)
        pp <- plotly::add_trace(pp, type="bar", orientation="h", name=e,
          y=themes, x=vals, marker=list(color=.k3col[[e]]),
          hovertemplate=paste0("<b>", e, "</b><br>%{y}<br>%{x}% of negative reviews<extra></extra>"))
      }
      plotly::layout(pp, barmode="group", margin=list(l=205, r=20, t=10, b=40),
        xaxis=list(title="Share of negative reviews touching the theme (%)"),
        yaxis=list(autorange="reversed"),
        legend=list(orientation="h", y=-0.2, x=0.5, xanchor="center"),
        font=list(family="Montserrat, sans-serif"))
    })

    output$kit3_pl_temp <- plotly::renderPlotly({
      df <- readr::read_csv(kpath("KIQ_3_3", "kiq33_temporal_trend.csv"), show_col_types = FALSE)
      df$yq_dt <- as.Date(substr(as.character(df$yq_dt), 1, 10))
      pp <- plotly::plot_ly()
      for (e in unique(df$competitor)) {
        d <- df[df$competitor == e, ]; d <- d[order(d$yq_dt), ]
        pp <- plotly::add_trace(pp, type="scatter", mode="lines+markers", name=e,
          x=d$yq_dt, y=d$avg_rating, line=list(color=.k3col[[e]]), marker=list(color=.k3col[[e]]),
          hovertemplate=paste0("<b>", e, "</b><br>%{x|%Y-%m}<br>%{y:.2f}★ (n=", d$n, ")<extra></extra>"))
      }
      plotly::layout(pp, margin=list(l=55, r=20, t=10, b=40),
        xaxis=list(title="Quarter"),
        yaxis=list(title="Average rating (1–5 stars)", range=c(1,5)),
        shapes=list(list(type="line", x0=min(df$yq_dt), x1=max(df$yq_dt), y0=3, y1=3,
          line=list(dash="dot", color="#999"))),
        legend=list(orientation="h", y=-0.2, x=0.5, xanchor="center"),
        font=list(family="Montserrat, sans-serif"))
    })
  }
}
