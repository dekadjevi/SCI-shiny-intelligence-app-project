# ============================================================
#  METRO AG SCI Project — global.R
#  Loaded once at app start; shared by ui.R and server.R.
#  Builds the KIT 1 data frames from the real CSVs in data/.
# ============================================================

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(tidyr); library(stringr)
})

# ---- Locate the data/ folder ---------------------------------------------
# The app lives in  <project>/metro_shiny/  and the data in <project>/data/.
# We look one level up first, then in the working directory.
.find_data <- function() {
  cands <- c(file.path("..", "data"), "data", file.path(getwd(), "data"))
  hit <- cands[dir.exists(cands)]
  if (length(hit)) normalizePath(hit[1]) else cands[1]
}
DATA_DIR <- .find_data()
dpath <- function(...) file.path(DATA_DIR, ...)

# ---- Helper: SCOPUS keyword occurrence -----------------------------------
scopus_occurrence <- function(path, top_n = 20) {
  if (!file.exists(path)) return(NULL)
  df <- suppressWarnings(readr::read_csv(path, show_col_types = FALSE))
  kw <- intersect(c("Author Keywords", "Index Keywords"), names(df))
  if (!length(kw)) return(NULL)
  df %>%
    select(all_of(kw)) %>%
    pivot_longer(everything(), values_to = "text") %>%
    filter(!is.na(text)) %>%
    separate_rows(text, sep = ";") %>%
    mutate(term = str_squish(str_to_lower(text))) %>%
    filter(term != "", str_length(term) > 2) %>%
    count(term, sort = TRUE) %>%
    slice_head(n = top_n)
}

# ---- Helper: patent IPC occurrence (Espacenet ';' export, header @ "No") --
patent_ipc <- function(path, top_n = 12) {
  if (!file.exists(path)) return(NULL)
  raw <- readLines(path, warn = FALSE)
  hdr <- grep('^"No"', raw); hdr <- if (length(hdr)) hdr[1] else 8
  tmp <- tempfile(fileext = ".csv"); writeLines(raw[hdr:length(raw)], tmp)
  df <- suppressWarnings(readr::read_delim(tmp, delim = ";",
        show_col_types = FALSE, escape_double = TRUE, trim_ws = TRUE))
  if (!"IPC" %in% names(df)) return(NULL)
  df %>%
    filter(!is.na(IPC)) %>%
    separate_rows(IPC, sep = "[\n,]") %>%
    mutate(ipc4 = str_sub(str_squish(IPC), 1, 4)) %>%
    filter(str_detect(ipc4, "^[A-H]\\d")) %>%
    count(ipc4, sort = TRUE) %>%
    slice_head(n = top_n)
}

# ---- Helper: patent publication-year trend -------------------------------
patent_trend <- function(path) {
  if (!file.exists(path)) return(NULL)
  raw <- readLines(path, warn = FALSE)
  hdr <- grep('^"No"', raw); hdr <- if (length(hdr)) hdr[1] else 8
  tmp <- tempfile(fileext = ".csv"); writeLines(raw[hdr:length(raw)], tmp)
  df <- suppressWarnings(readr::read_delim(tmp, delim = ";",
        show_col_types = FALSE, trim_ws = TRUE))
  dcol <- intersect(c("Publication date", "Earliest publication"), names(df))[1]
  if (is.na(dcol)) return(NULL)
  df %>%
    mutate(year = suppressWarnings(as.integer(str_sub(.data[[dcol]], 1, 4)))) %>%
    filter(!is.na(year), year >= 2018, year <= 2026) %>%
    count(year)
}

# ============================================================
#  PRE-LOAD KIT 1 DATA  (computed once)
# ============================================================

# KIQ 1.1 — SCOPUS keyword occurrence (with the same merges as the script)
kiq11_data <- local({
  d <- scopus_occurrence(dpath("KIQ_1_1", "raw", "new.csv"), top_n = 200)
  if (is.null(d)) return(NULL)
  merge_term <- function(k) dplyr::case_when(
    k == "machine-learning" ~ "machine learning",
    k %in% c("recommendation","recommendation algorithms","product recommendation",
             "personalized recommendation","e-commerce recommendations") ~ "recommender systems",
    k %in% c("customer experience","users' experiences","user interfaces") ~ "user experience",
    k == "marketplaces" ~ "marketplace",
    k %in% c("commerce platforms","e-commerce","electronic commerce",
             "e- commerces","e-commerce platform") ~ "commerce platform",
    TRUE ~ k)
  drop <- c("websites","commerce platform","e-commerce websites","adversarial machine learning",
            "contrastive learning","natural language processing","sales","online shopping",
            "purchasing","electronic money")
  d %>% mutate(term = merge_term(term)) %>%
    filter(!term %in% drop) %>%
    group_by(term) %>% summarise(n = sum(n), .groups = "drop") %>%
    arrange(desc(n)) %>% slice_head(n = 20)
})

# KIQ 1.2 — patent IPC + trend
kiq12_ipc   <- patent_ipc(dpath("KIQ_1_2", "raw", "KIQ_1_2_patents_raw.csv"), top_n = 12)
kiq12_trend <- patent_trend(dpath("KIQ_1_2", "raw", "KIQ_1_2_patents_raw.csv"))

# KIQ 1.3 — SCOPUS retention keyword occurrence
kiq13_data <- scopus_occurrence(dpath("KIQ_1_3", "raw", "scopus_KIQ_1_3.csv"), top_n = 18)

# KIQ 1.4 — strengths / weaknesses summary CSVs
kiq14_strengths <- local({
  p <- dpath("KIQ_1_4", "outputs", "kiq14_strengths.csv")
  if (file.exists(p)) readr::read_csv(p, show_col_types = FALSE) %>% mutate(kind = "Strength") else NULL
})
kiq14_weaknesses <- local({
  p <- dpath("KIQ_1_4", "outputs", "kiq14_weaknesses.csv")
  if (file.exists(p)) readr::read_csv(p, show_col_types = FALSE) %>% mutate(kind = "Weakness") else NULL
})

# ============================================================
#  SAVED PNG PLOTS  (preferred for KIT 1 — reflect the real
#  analysis exactly as exported, and render reliably)
# ============================================================
# base64-encode so images render without a www/ copy step.
img_uri <- function(path) {
  if (!file.exists(path)) return(NULL)
  tryCatch(base64enc::dataURI(file = path, mime = "image/png"),
           error = function(e) NULL)
}

# resilient path: tries KIQ folder with both 'output' and 'outputs'
kpath <- function(kiq, file) {
  for (sub in c("output", "outputs")) {
    p <- dpath(kiq, sub, file)
    if (file.exists(p)) return(p)
  }
  dpath(kiq, "outputs", file)   # default (for the not-found message)
}

KIT1_IMAGES <- list(
  "1.1" = c(kpath("KIQ_1_1", "KIQ_1_1_occurrence.png"),
            kpath("KIQ_1_1", "metro_ux.png")),
  "1.2" = c(kpath("KIQ_1_2", "Yoy_Rplot.png"),
            kpath("KIQ_1_2", "Occurrency_plot.png")),
  "1.3" = c(kpath("KIQ_1_3", "Rplot_occ_KIQ1_3.png"),
            kpath("KIQ_1_3", "Rwordcloud_KIQ1_3.png"),
            kpath("KIQ_1_3", "Rplot_cooc_KIQ1_3.png")),
  "1.4" = c(kpath("KIQ_1_4", "kiq14_companion_vs_shop.png"),
            kpath("KIQ_1_4", "kiq14_by_market.png"))
)

# KIQs rendered as tabbed layers instead of a grid (1.3 builds to the co-occurrence network)
KIT1_TABS <- list(
  "1.3" = c("Occurrence", "Word cloud", "Co-occurrence network")
)

# ---- KIT 1 dashboard: live KPI numbers (from KIQ 1.4 CSVs) ----
KIT1_KPIS <- local({
  out <- list(avg = "2.46", neg = "59.5%", auth = "32%", n = "749")
  corp <- dpath("KIQ_1_4", "raw", "kiq14_metro_app_corpus.csv")
  if (file.exists(corp)) {
    d <- suppressWarnings(readr::read_csv(corp, show_col_types = FALSE))
    if ("rating" %in% names(d)) {
      out$avg <- sprintf("%.2f", mean(d$rating, na.rm = TRUE))
      out$neg <- sprintf("%.0f%%", mean(d$rating <= 2, na.rm = TRUE) * 100)
      out$n   <- format(nrow(d), big.mark = ",")
    }
  }
  w <- dpath("KIQ_1_4", "outputs", "kiq14_weaknesses.csv")
  if (file.exists(w)) {
    wd <- suppressWarnings(readr::read_csv(w, show_col_types = FALSE))
    a <- wd$pct[grepl("Authentication", wd$category)]
    if (length(a)) out$auth <- sprintf("%.0f%%", a[1])
  }
  out
})

# ---- KIT 1 per-KIQ metadata (question, insight, decision) ----
KIT1_META <- list(
  "1.1" = list(tag = "KIQ 1.1", title = "Functionalities",
    q = "What functionalities does METRO's platform offer?",
    insight  = "Two capability layers dominate the literature (discovery/interaction + ordering/fulfilment). METRO covers both; personalization is the thin spot.",
    decision = "Treat core functionality as parity; invest at the personalization gap, not on rebuilding covered features."),
  "1.2" = list(tag = "KIQ 1.2", title = "Channel Evolution",
    q = "How are digital ordering channels evolving (3–5 yr)?",
    insight  = "Patent filings rose 2022→2025; G06Q (e-commerce) dominates with AI/ML rising — the frontier is intelligent, marketplace-centric ordering.",
    decision = "Plan the roadmap toward AI-assisted ordering; decide build-vs-partner while the field is still open."),
  "1.3" = list(tag = "KIQ 1.3", title = "Retention",
    q = "How do digital channels drive customer retention?",
    insight  = "Customer satisfaction is the structural centre of retention, linking service quality & trust to loyalty and repurchase. Tech sits at the periphery as enabler.",
    decision = "Anchor retention on satisfaction fundamentals (reliability, ease, trust) — not peripheral tech for its own sake."),
  "1.4" = list(tag = "KIQ 1.4", title = "Strengths / Weaknesses",
    q = "How do real customers rate METRO's app?",
    insight  = "The digital card is the most-loved feature AND the most-broken (32% authentication, 23% crashes). Only ~2% ask for missing features; Shop (ordering) is worse than Companion.",
    decision = "Harden the digital-card / login flow and update QA before any new feature — the highest-ROI digital action.")
)

# ============================================================
#  KIT 2 — KIQ 2.3 (Customer expectations · BERTopic + K-means)
#  Two-step narrative:
#   Step 1 — BERTopic result      (16 topics discovered)
#   Step 2 — K-means on BERTopic  (k=2 macro-clusters)
# ============================================================
KIT2_KIQ23 <- list(
  step1 = list(
    label = "Step 1 · BERTopic result",
    blurb = "Topic modelling (BERTopic) on the SCOPUS corpus discovered 16 distinct topics. The bar charts show each topic's defining terms; the hierarchical plot confirms they collapse into two big families.",
    imgs  = c(kpath("KIQ_2_3", "topic_barcharts.png"),
              kpath("KIQ_2_3", "hierarchical_plot.png")),
    insight  = "BERTopic resolves the customer-expectations literature into 16 topics, but the hierarchical clustering already points to two clear super-groups — an experience/service family and a smaller operational/transactional family.",
    decision = "Treat the 16 raw topics as too granular to steer strategy; collapse them into macro-themes (next step) before drawing any conclusion."
  ),
  step2 = list(
    label = "Step 2 · K-means on BERTopic",
    blurb = "Following the two-family signal, K-means (k=2) clusters the topics into macro-themes. The distance map recolours by cluster; the macro-term bars show what each cluster is about; the trend tracks academic attention to each over 2018–2026.",
    imgs  = c(kpath("KIQ_2_3", "inter_topic_after_K_means.png"),
              kpath("KIQ_2_3", "kiq23_macro_terms_topic_After_Kmeans.png"),
              kpath("KIQ_2_3", "kiq23_macro_term_trend.png")),
    insight  = "The literature splits into Experience Economy (835 papers — customer, hospitality, service, digital, experience, personalization, robots) and a much smaller Operational/Transactional cluster (123 papers — food, ordering, price, supply, delivery). Academic attention to the Experience cluster grew ~10× (25→242 papers, 2018→2025) while the Operational cluster stayed essentially flat (~5→28). Customer expectations are migrating decisively from \u2018can I order efficiently\u2019 to \u2018is the whole experience smart, personal and engaging\u2019.",
    decision = "Anchor METRO's 3–5 yr digital roadmap to the experience layer (personalization, smart/assisted ordering, engagement), not just transactional ordering mechanics — the operational basics are necessary but no longer where expectations are moving. This corroborates the KIT 1 personalization gap from the demand side."
  )
)

# KPI tiles for KIQ 2.3 (evidence framing, not app metrics)
KIT2_KIQ23_KPIS <- list(
  list("16",  "Topics discovered (BERTopic)"),
  list("2",   "Macro-clusters (K-means)"),
  list("835", "Experience-economy papers"),
  list("~10\u00d7", "Growth in experience focus, 2018\u201325")
)

# ── KIQ 2.1 — Tech trends (single panel) ──────────────────
KIT2_KIQ21 <- list(
  ptag   = "KIQ 2.1 \u00b7 Tech trends",
  ptitle = "What emerging digital trends are shaping ordering?",
  pq     = "IPC, applicant and n-gram analysis on the Espacenet digital-ordering corpus (n = 874)",
  blurb  = "Patents lead academic publishing by 1\u20133 years, so they are KIT 2's earliest signal. The trend shows how fast the field is moving; the IPC chart shows the layered technology stack being built; the applicant ranking shows who owns the IP.",
  imgs   = c(kpath("KIQ_2_1", "Rplot_trend_KIQ2_1.png"),
             kpath("KIQ_2_1", "Rplot_ipc_KIQ2_1.png"),
             kpath("KIQ_2_1", "Rplot_applicants_KIQ2_1.png")),
  chart_labels = c("Filing trend", "Technology stack", "Top filers"),
  insight  = "Filing volume grew 2.6\u00d7 from 2022 to 2025 (111 \u2192 288) \u2014 the technologies that will reshape ordering are being built right now. They form a layered stack: commerce logic (G06Q), computing (G06F) and AI/ML (G06N) dominate, while voice (G10L) and IoT (G16Y) surface as smaller but unmistakable 2024\u20132026 leading edges, and the title bigrams reveal generative AI, intelligent agents and recommendation engines beneath the headline 'AI' signal. But the IP is externally held and concentrated \u2014 Baidu alone files 101 patents, with the rest US- and Asian-headquartered tech and fintech players (Tencent, eBay, Rakuten, Mastercard); European presence is essentially absent.",
  decision = "METRO's question is no longer whether to adopt but which layers to build, partner for or acquire. No top filer is HoReCa-specific and the technologies are sector-neutral \u2014 a deployment window is still open to embed them into HoReCa ordering before consolidation hardens."
)
KIT2_KIQ21_KPIS <- list(
  list("874",  "Digital-ordering patents"),
  list("2.6\u00d7", "Filing growth, 2022\u21922025"),
  list("5",    "Layers in the tech stack"),
  list("0",    "European firms in top filers")
)

# ── KIQ 2.2 — Competitor IP (single panel) ────────────────
KIT2_KIQ22 <- list(
  ptag   = "KIQ 2.2 \u00b7 Competitor IP",
  ptitle = "How are competitors evolving their digital ordering?",
  pq     = "Espacenet B2B / wholesale corpus, 2018\u20132025 (n = 1,219), complemented by SCOPUS",
  blurb  = "Named competitors file almost no patents \u2014 a negative result that is itself a finding. Read as a proxy for the competitive environment: the trend shows whether B2B / wholesale IP is accelerating; the applicant ranking shows how concentrated it is; the IPC mix shows whether the frontier has arrived.",
  imgs   = c(kpath("KIQ_2_2", "01_trend.png"),
             kpath("KIQ_2_2", "02_applicants.png"),
             kpath("KIQ_2_2", "03_ipc_classes.png")),
  chart_labels = c("Filing trend", "Top filers", "IPC mix"),
  insight  = "Unlike KIQ 2.1's accelerating field, B2B / wholesale filings are stable at ~120\u2013150 per year since 2021 (the 2018 spike is broad 'platform' terminology) \u2014 this IP territory is mature, not growing. It is also extremely fragmented: no applicant exceeds ~1.1% of the corpus, and the top filers are Korean and Chinese e-commerce SMEs, Asian universities and individual inventors; the closest analog to METRO's set is Foodpang [KR]. European incumbents, digital-native HoReCa entrants and Western tech giants are all absent. The stack is commerce-pure \u2014 G06Q is ~60% of all classifications while AI (G06N) is marginal and voice/IoT do not appear \u2014 and the semantic anchor is supply chain and agricultural wholesale.",
  decision = "Competitors evolve through deployment of externally-supplied classical-commerce technology, not invention \u2014 and the frontier capabilities accelerating in KIQ 2.1 have not yet entered the layer where METRO competes. The deployment window is wider than 2.1 alone suggested: METRO can be first to bring voice, IoT replenishment, intelligent agents and AI personalization into B2B / wholesale HoReCa ordering."
)
KIT2_KIQ22_KPIS <- list(
  list("1,219", "B2B / wholesale patents"),
  list("~150",  "Annual filings, stable since 2021"),
  list("~60%",  "Corpus classified G06Q"),
  list("1.1%",  "Share held by the top filer")
)

# ── KIT 3 — Key players & positioning ────────
KIT3_GROUP_COL <- list(metro = "#1f4e79", incumbent = "#8B2E2E",
                       digital = "#2E7D5B", adjacent = "#E08A1E")
KIT3_GROUP_LBL <- list(metro = "METRO", incumbent = "Incumbent",
                       digital = "Digital-native", adjacent = "Adjacent")
KIT3_COMPETITORS <- list(
  list("METRO M|Shop","metro","Companion app \u00b7 single-supplier own catalog","Europe (~30 countries)","2017"),
  list("METRO MARKETS","metro","Hybrid B2B marketplace (catalog + 3rd-party)","Germany + EU rollout","2019"),
  list("Sysco Shop / MySysco","incumbent","Own-catalog e-commerce + mobile app","US-led \u00b7 UK/EU via Brakes & Davigel","2014"),
  list("Sysco Marketplace","incumbent","Mirakl hybrid marketplace (15k+ SKUs)","US (rolling out)","2024"),
  list("Bidfood (Bidcorp)","incumbent","eBidfood / Bidfood Direct \u00b7 single-supplier","UK + 35+ countries","2015"),
  list("Transgourmet B2B app","incumbent","React Native app \u00b7 single-supplier","DE/FR/CH/AT/PL/RO","2021"),
  list("Choco","digital","Mobile-first AI ordering \u00b7 pure aggregator \u00b7 free","DE/FR/ES/AT/BE/UK/US","2018"),
  list("REKKI","digital","Mobile ordering + UK marketplace \u00b7 pure aggregator","UK/NL/PT/US","2016"),
  list("Amazon Business","adjacent","Hybrid marketplace + 300 procurement integrations","US + Europe + global","2015")
)
kit3_competitor_table <- function() {
  hdr <- shiny::tags$tr(
    shiny::tags$th("Competitor"), shiny::tags$th("Group"),
    shiny::tags$th("Platform & model"), shiny::tags$th("Geography"),
    shiny::tags$th(class = "cnum", "Since"))
  rows <- lapply(KIT3_COMPETITORS, function(co) {
    g <- co[[2]]
    shiny::tags$tr(class = if (g == "metro") "crow mclass" else "crow",
      shiny::tags$td(class = "cname", co[[1]]),
      shiny::tags$td(shiny::tags$span(class = "dot",
        style = paste0("background:", KIT3_GROUP_COL[[g]])), KIT3_GROUP_LBL[[g]]),
      shiny::tags$td(class = "cplat", co[[3]]),
      shiny::tags$td(co[[4]]),
      shiny::tags$td(class = "cnum", co[[5]]))
  })
  shiny::tags$table(class = "ctbl",
    shiny::tags$thead(hdr), do.call(shiny::tags$tbody, rows))
}
# ── KIT 3 feature heatmap data (KIQ 3.2) ──
KIT3_FEAT_COLS <- list(
  list("METRO","metro"), list("Sysco Mkt","incumbent"), list("Bidfood","incumbent"),
  list("Transgourmet","incumbent"), list("Choco","digital"), list("REKKI","digital"),
  list("Amazon Biz","adjacent"))
KIT3_FEAT_ROWS <- list(
  list("Multi-vendor marketplace",               c("Y","Y","N","N","Y","Y","Y")),
  list("Broad own catalog / range",              c("Y","Y","Y","Y","N","N","Y")),
  list("Owned fulfilment / delivery",            c("Y","Y","Y","Y","N","N","Y")),
  list("Native mobile ordering app",             c("P","P","N","Y","Y","Y","Y")),
  list("Punchout / procurement integration",     c("P","Y","Y","Y","N","N","Y")),
  list("ERP integration",                        c("Y","Y","Y","Y","P","P","Y")),
  list("Approval workflows / company accounts",  c("Y","Y","Y","Y","P","P","Y")),
  list("Quote & tiered / negotiated pricing",    c("Y","Y","Y","Y","N","N","P")),
  list("AI order capture (voice / multimodal)",  c("N","N","N","N","Y","P","N")),
  list("AI recommendation / personalization",    c("P","P","P","P","Y","P","Y")))
KIT3_FEAT_CELL <- list(Y = c("\u25cf","cy"), P = c("\u25d0","cp"), N = c("\u25cb","cn"))

kit3_feature_heatmap <- function() {
  ths <- list(shiny::tags$th(class = "rowhdr", "Capability"))
  for (cc in KIT3_FEAT_COLS) {
    mcol <- if (cc[[2]] == "metro") "cch mcol" else "cch"
    ths[[length(ths) + 1]] <- shiny::tags$th(class = mcol,
      shiny::tags$span(class = "cdot", style = paste0("background:", KIT3_GROUP_COL[[cc[[2]]]])),
      shiny::tags$br(), cc[[1]])
  }
  rows <- lapply(KIT3_FEAT_ROWS, function(rw) {
    tds <- list(shiny::tags$td(class = "rlab", rw[[1]]))
    vals <- rw[[2]]
    for (i in seq_along(KIT3_FEAT_COLS)) {
      cc <- KIT3_FEAT_COLS[[i]]; cell <- KIT3_FEAT_CELL[[ vals[i] ]]
      cls <- if (cc[[2]] == "metro") paste("hc", cell[2], "mcol") else paste("hc", cell[2])
      tds[[length(tds) + 1]] <- shiny::tags$td(class = cls,
        shiny::tags$span(class = "sym", cell[1]))
    }
    do.call(shiny::tags$tr, tds)
  })
  legend <- shiny::div(class = "hm-legend",
    shiny::tags$span(class = "lg", shiny::tags$span(class = "sw cy"), "Yes"),
    shiny::tags$span(class = "lg", shiny::tags$span(class = "sw cp"), "Partial / emerging"),
    shiny::tags$span(class = "lg", shiny::tags$span(class = "sw cn"), "Not present"))
  shiny::tagList(
    shiny::tags$table(class = "hm",
      shiny::tags$thead(do.call(shiny::tags$tr, ths)),
      do.call(shiny::tags$tbody, rows)),
    legend)
}

# render one layer (tab) of a KIT 3 KIQ: content + its own footer
kit3_render_layer <- function(d, layer) {
  content <- if (identical(layer$kind, "comp_table")) {
      kit3_competitor_table()
    } else if (identical(layer$kind, "heatmap")) {
      kit3_feature_heatmap()
    } else if (identical(layer$kind, "plotly")) {
      if (requireNamespace("plotly", quietly = TRUE)) {
        shiny::div(style = "width:100%",
          plotly::plotlyOutput(layer$pid, height = "440px"))
      } else {
        uri <- img_uri(layer$fallback)
        if (is.null(uri)) shiny::div(class = "kit1-cell empty", "chart not found")
        else shiny::div(class = "kit1-charts solo",
               shiny::div(class = "kit1-cell", shiny::tags$img(src = uri)))
      }
    } else {
      uri <- img_uri(layer$src)
      if (is.null(uri)) shiny::div(class = "kit1-cell empty", "chart not found")
      else shiny::div(class = "kit1-charts solo",
             shiny::div(class = "kit1-cell", shiny::tags$img(src = uri)))
    }
  footer <- if (identical(layer$footer, "insightdec")) {
      shiny::div(class = "kit1-ftr",
        shiny::div(class = "kit1-ins", shiny::tags$b("Insight \u2014 "), d$insight),
        shiny::div(class = "kit1-dec", shiny::tags$b("Decision \u2014 "), d$decision))
    } else if (identical(layer$footer, "context")) {
      shiny::div(class = "ctx-box",
        shiny::div(class = "ctx-lead", layer$ctx_lead),
        shiny::div(class = "ctx-sub",  layer$ctx_sub))
    } else NULL
  shiny::tagList(content, footer)
}

# ── KIQ 3.1 ──
KIT3_KIQ31 <- list(
  ptag   = "KIQ 3.1 \u00b7 Main competitors",
  ptitle = "Who competes for HoReCa digital ordering in Europe?",
  pq     = "Structured web research on 7 competitors, encoded to a positioning matrix on two axes",
  insight  = "Two structural axes \u2014 catalog model (single-supplier \u2192 hybrid \u2192 pure aggregator) and business DNA (incumbent vs digital-native) \u2014 split the field into clear strategic groups. METRO already spans both incumbent quadrants (M|Shop single-supplier and METRO MARKETS hybrid), so it is not losing the catalog war. The contested space is the upper-right: Choco and REKKI, the digital-native pure aggregators, are the experience-setters training HoReCa buyers on UX, speed and AI personalization, with Amazon Business the latent adjacent threat.",
  decision = "METRO's strategic challenge is the experience war, not the catalog war. The next two indicators test it \u2014 3.2 compares the platforms feature-by-feature, 3.3 surfaces how customers actually perceive them \u2014 together locating where METRO must close the gap the pure aggregators have opened.",
  layers = list(
    list(label = "Competitor landscape", kind = "comp_table", footer = "context",
      ctx_lead = "Not a long list \u2014 a deliberate set: two players from each of the three strategic groups that define HoReCa digital ordering.",
      ctx_sub  = "Incumbent foodservice (Sysco, Bidfood, Transgourmet), digital-native aggregators (Choco, REKKI) and the adjacent B2C giant (Amazon Business) together span the full catalog gradient \u2014 single-supplier \u2192 hybrid \u2192 pure marketplace \u2014 and both business-DNA types. Regional long-tail players would add rows, not strategic insight."),
    list(label = "Positioning matrix", kind = "image",
      src = kpath("KIQ_3_1", "kiq31_positioning_matrix.png"), footer = "insightdec")
  )
)
KIT3_KIQ31_KPIS <- list(
  list("7", "Competitors mapped"),
  list("4", "Strategic quadrants"),
  list("3", "Marketplace models"),
  list("2", "Quadrants METRO spans")
)

# ── KIQ 3.2 ──
KIT3_KIQ32 <- list(
  ptag   = "KIQ 3.2 \u00b7 Feature comparison",
  ptitle = "How does METRO compare on features and usability?",
  pq     = "Capability heatmap across 7 platforms (evidenced from each documented stack), validated against app-store feature mentions",
  insight  = "Across ten user-facing capabilities the field splits three ways. METRO and the incumbents (Sysco Marketplace on Mirakl, Bidfood on Adobe Commerce, Transgourmet on Commercetools) are strong on the B2B fundamentals \u2014 catalog breadth, procurement/ERP integration, approval workflows, quoting and owned fulfilment. The digital-natives (Choco, REKKI) win decisively on mobile-first UX and, for Choco, AI order capture, but carry no owned catalog or fulfilment. METRO matches or leads everyone on breadth, ecosystem and fulfilment; its one real gap is AI-driven personalization and capture, where Choco and Amazon's B2C-grade engine are ahead.",
  decision = "METRO doesn't have a feature problem \u2014 it has an intelligence problem. The build priority is the personalization / AI-capture layer (smart reorder, recommendation, assisted/voice ordering) on top of an already-broad platform, not catching up on B2B basics it already has. This is the functional evidence behind the experience-war framing from 3.1, and 3.3 shows the perception cost of leaving the gap open.",
  layers = list(
    list(label = "Feature heatmap", kind = "heatmap", footer = "insightdec"),
    list(label = "Reviewed features", kind = "plotly", pid = "kit3_pl_feat",
      fallback = kpath("KIQ_3_3", "kiq33_feature_mentions.png"), footer = "context",
      ctx_lead = "App-store reviews independently corroborate the heatmap.",
      ctx_sub  = "Across the three reviewable apps, customers mention exactly the capabilities the heatmap attributes to each platform \u2014 ordering and supplier discovery dominate the digital-natives, scan and search dominate METRO, and AI/voice barely registers for anyone, the same personalization gap the heatmap flags. The four incumbents have no public app for customers to mention features in at all \u2014 which is itself the KIQ 3.3 corpus-asymmetry finding.")
  )
)
KIT3_KIQ32_KPIS <- list(
  list("7",  "Platforms compared"),
  list("10", "Capabilities assessed"),
  list("3",  "Pillars METRO leads"),
  list("1",  "Structural capability gap")
)

# ── KIQ 3.3 ──
KIT3_KIQ33 <- list(
  ptag   = "KIQ 3.3 \u00b7 Customer perception",
  ptitle = "How do customers perceive METRO vs competitors?",
  pq     = "Sentiment, pain-point and feature analysis of 1,103 app-store reviews (METRO, Choco, REKKI; multilingual)",
  insight  = "1,103 app-store reviews split the field by a 1.6\u20131.8\u2605 structural gap: METRO averages 2.46\u2605 (59.5% of reviews 1\u20132\u2605) against Choco 4.25\u2605 and REKKI 4.06\u2605 (77\u201380% positive). But METRO's problem is operational, not functional \u2014 authentication/card-login (33%), crashes (24%) and update regressions (9%) dominate the negative reviews, while only ~2% request missing features. The digital-natives are praised for workflow transformation, and only three competitors have public apps at all \u2014 so METRO is judged publicly against the digital-natives, not its B2B incumbents.",
  decision = "METRO's public-perception competitor is Choco and REKKI, not Sysco \u2014 and the fix is execution, not strategy. ~57% of complaints are operational, so fixing authentication / card-login reliability is the single highest-ROI digital move. Then decide whether to extend M|Companion into a true ordering experience or build a dedicated M|Shop / METRO MARKETS front-end, where the digital-natives now set buyer expectations. The window is closing as the 1\u2605 reviews compound on future buyers.",
  layers = list(
    list(label = "Average rating",  kind = "plotly", pid = "kit3_pl_avg", fallback = kpath("KIQ_3_3", "kiq33_avg_rating.png"),            footer = "insightdec"),
    list(label = "Sentiment split", kind = "plotly", pid = "kit3_pl_sent", fallback = kpath("KIQ_3_3", "kiq33_sentiment_from_ratings.png"), footer = "insightdec"),
    list(label = "Pain points",     kind = "plotly", pid = "kit3_pl_pain", fallback = kpath("KIQ_3_3", "kiq33_painpoints.png"),             footer = "insightdec"),
    list(label = "Temporal trend",  kind = "plotly", pid = "kit3_pl_temp", fallback = kpath("KIQ_3_3", "kiq33_temporal_trend.png"),         footer = "insightdec")
  )
)
KIT3_KIQ33_KPIS <- list(
  list("2.46\u2605",      "METRO app rating"),
  list("1.6\u20131.8\u2605", "Gap to digital-natives"),
  list("59.5%",        "METRO reviews 1\u20132\u2605"),
  list("~57%",         "Complaints operational")
)

# Render a stack of <img> tags for a KIQ (used by server renderUI)
kit1_image_block <- function(key) {
  paths <- KIT1_IMAGES[[key]]
  imgs <- Filter(Negate(is.null), lapply(paths, function(p) {
    uri <- img_uri(p)
    if (is.null(uri)) NULL else
      shiny::tags$img(src = uri, class = "kiq-plot",
                      style = "max-width:100%;margin:6px 0;border:0.5px solid var(--metro-border);border-radius:6px;")
  }))
  if (!length(imgs))
    return(shiny::div(class = "viz-missing",
      "Plot not found — run the KIQ script to export the PNG into its outputs/ folder."))
  do.call(shiny::tagList, imgs)
}
