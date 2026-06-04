# ============================================================
#  METRO AG SCI Project — ui.R
#  Strategic & Competitive Intelligence Shiny App
# ============================================================
#
#  FOLDER STRUCTURE expected:
#  metro_shiny/
#  ├── ui.R
#  ├── server.R
#  └── www/
#      └── styles.css
#
#  Install required packages once:
#  install.packages(c("shiny","shinydashboard","DT","plotly","ggplot2","dplyr"))
# ============================================================

library(shiny)
library(DT)
library(plotly)

# ── helper: section label ──────────────────────────────────
sec <- function(txt) tags$p(class = "section-label", txt)

# ── helper: stat card ──────────────────────────────────────
stat_card <- function(label, value, sub = NULL, border_color = NULL) {
  style <- if (!is.null(border_color))
    paste0("border-left: 3px solid ", border_color, ";")
  else ""
  div(class = "card",
    style = style,
    div(class = "card-label", label),
    div(class = "card-value", value),
    if (!is.null(sub)) div(class = "card-sub", sub)
  )
}

# ── helper: finding block ──────────────────────────────────
finding <- function(n, text, kit = "kit1") {
  div(class = paste("finding", kit),
    div(class = "finding-num", paste("Finding", n)),
    div(class = "finding-text", text)
  )
}

# ── helper: implication box ───────────────────────────────
note_box <- function(...) div(class = "note", ...)

# ── helper: viz placeholder (swap with plotOutput) ─────────
viz_placeholder <- function(label, sub = NULL, height = "160px") {
  div(class = "viz-box", style = paste0("min-height:", height, ";"),
    tags$strong(label),
    if (!is.null(sub)) tags$span(sub)
  )
}

# ── helper: source chip ────────────────────────────────────
chip <- function(text) tags$span(class = "chip", text)

# ── helper: chart rationale / insight / decision strip ─────
# Implements the Data-Viz Lab structure: every chart is framed by
# WHY this chart (rationale), the EXPECTED INSIGHT, and the
# POTENTIAL DECISION it supports.
viz_meta <- function(chart_type, rationale, insight, decision) {
  div(class = "viz-meta",
    div(class = "vm-row",
      tags$span(class = "vm-key", "Chart"),
      tags$span(class = "vm-val", chart_type)),
    div(class = "vm-row",
      tags$span(class = "vm-key", "Why"),
      tags$span(class = "vm-val", rationale)),
    div(class = "vm-row vm-insight",
      tags$span(class = "vm-key", "Insight"),
      tags$span(class = "vm-val", insight)),
    div(class = "vm-row vm-decision",
      tags$span(class = "vm-key", "Decision"),
      tags$span(class = "vm-val", decision))
  )
}

# ── helper: narrative step (for the KIT story path) ────────
nstep <- function(n, title, body) {
  div(class = "nstep",
    tags$span(class = "nstep-num", n),
    div(div(class = "nstep-title", title),
        div(class = "nstep-body", body)))
}

# ============================================================
#  UI
# ============================================================

ui <- navbarPage(

  title = div(class = "navbar-brand-custom",
    tags$span(class = "brand-metro", "METRO AG"),
    tags$span(class = "brand-sub", "SCI Project")
  ),

  id = "main_nav",
  windowTitle = "METRO AG — Digital Intelligence",
  header = includeCSS("www/styles.css"),

  # ── 1. HOME ───────────────────────────────────────────────
  tabPanel("Home",
    div(class = "page-wrap home-wrap",

      # hero banner (inspired by the title slide)
      div(class = "hero",
        div(class = "hero-accent"),
        div(class = "hero-body",
          tags$span(class = "hero-brand", "METRO AG"),
          tags$span(class = "hero-title", "Digitalization of Customer-Facing Services"),
          tags$span(class = "hero-sub", "Strategic & Competitive Intelligence Project"),
          div(class = "hero-rule"),
          tags$span(class = "hero-meta", "European HoReCa · M|Shop · M|Companion · METRO MARKETS")
        )
      ),

      # ── the central question ──
      div(class = "bigq",
        tags$span(class = "bigq-kicker", "The question this dashboard answers"),
        tags$span(class = "bigq-text",
          "Are METRO's digital channels a real competitive advantage — ",
          tags$span(class = "bigq-em", "or just an assumption?"))
      ),

      # ── hero numbers ──
      div(class = "bignums",
        div(class = "bignum",
          div(class = "bn-v", "€31B"), div(class = "bn-l", "Group sales FY23/24")),
        div(class = "bignum",
          div(class = "bn-v", "30+"), div(class = "bn-l", "Countries")),
        div(class = "bignum",
          div(class = "bn-v", "15–17M"), div(class = "bn-l", "Professional customers")),
        div(class = "bignum hl",
          div(class = "bn-v", "11% → 40%"), div(class = "bn-l", "Digital sales target by 2030"))
      ),

      # ── one honest tension line ──
      div(class = "tension",
        tags$p(
          "METRO is Europe's leading B2B wholesaler for HoReCa, betting its ",
          tags$strong("sCore 2030"), " future on digital. Its channels are ",
          tags$em("assumed"), " to drive retention and stay competitive — ",
          tags$strong("this project tests whether the evidence agrees."))
      ),

      note_box(tags$strong("Start here:"),
        " head to ", tags$strong("Business Context"), " for the company snapshot, then ",
        tags$strong("Scope & KITs"), " for the scope statement that frames every question that follows.")
    )
  ),

  # ── 2. BUSINESS CONTEXT ───────────────────────────────────
  tabPanel("Business Context",
    div(class = "page-wrap bc-tight",

      div(class = "page-header",
        tags$span(class = "page-title", "Business Context"),
        tags$span(class = "page-sub", "Phase 1 — company snapshot & strategy")
      ),

      div(class = "bc-byline",
        "624 wholesale stores · ~90k employees · ~30 countries — METRO operates one of Europe's largest B2B foodservice footprints."),

      div(class = "two-col",
        div(
          sec("Three business channels"),
          div(class = "card", style = "margin-bottom:8px;",
            div(class = "card-title", "Cash & Carry"),
            div(class = "card-desc", "Stores → Multichannel Fulfilment Centres")),
          div(class = "card", style = "margin-bottom:8px;",
            div(class = "card-title", "FSD Delivery"),
            div(class = "card-desc", "€7.9B · fastest growing channel")),
          div(class = "card",
            div(class = "card-title", "Digital Platforms"),
            div(class = "card-desc", "M|Shop · M|Companion · METRO MARKETS"))
        ),
        div(
          sec("sCore 2030 strategy"),
          plotOutput("plot_score_strategy", height = "200px")
        )
      ),

      # ── customer segments ──
      sec("Customer segments — exclusively professional (B2B)"),
      div(class = "three-col",
        stat_card("HoReCa", "Strategic priority",
                  "Hotels, restaurants, caterers — the sCore focus segment", "#185FA5"),
        stat_card("Traders", "Independent retailers",
                  "Kiosks, convenience stores — served with resale assortments", "#854F0B"),
        stat_card("SCO", "Service / offices",
                  "Offices & non-HoReCa buyers — complementary segment", "#3B6D11")
      ),
      div(class = "note", style = "margin-top:0;",
        tags$strong("HoReCa + Traders = 74% of sales"),
        " (FY 22/23), targeted to reach 80% by 2030 — the strategic core this project's scope follows."),

      # ── sCore three pillars ──
      sec("sCore 2030 — three strategic pillars"),
      div(class = "three-col",
        div(class = "card",
          div(class = "card-title", "1 · Sharpened focus"),
          div(class = "card-desc", "Exit non-core retail (Real divested); concentrate on HoReCa & Traders; own-brand 25% → 35%+.")),
        div(class = "card",
          div(class = "card-title", "2 · Multichannel expansion"),
          div(class = "card-desc", "Stores → fulfilment centres; FSD scaled; METRO MARKETS toward €3B marketplace sales by 2030.")),
        div(class = "card",
          div(class = "card-title", "3 · Digital transformation"),
          div(class = "card-desc", "Digital sales 11% → 40% by 2030 — positioned as a retention & partnership lever, not just a channel."))
      ),

      # ── digital portfolio ──
      sec("Digital portfolio — the customer-facing layer this project studies"),
      div(class = "four-col",
        stat_card("M|Shop", "Ordering platform", "Catalog, search, reorder, click & collect"),
        stat_card("M|Companion", "Mobile / in-store", "Digital card, scan, loyalty, invoices"),
        stat_card("METRO MARKETS", "Hybrid marketplace", "3rd-party assortment on own catalog"),
        stat_card("DISH", "Hospitality tools", "POS & restaurant ops (ecosystem lock-in)")
      ),

      div(class = "note",
        tags$strong("Source: "), "METRO AG Annual Report 2023/24 and sCore strategy communications. ",
        tags$em("The customer-facing digital layer above is exactly what the project scope isolates for analysis."))
    )
  ),

  # ── 3. FRAMEWORK ──────────────────────────────────────────
  tabPanel("Framework",
    div(class = "page-wrap",

      div(class = "page-header",
        tags$span(class = "page-title", "Scope Framework — 5Ws & 2Hs"),
        tags$span(class = "page-sub", "Deriving the scope statement · negative analysis")
      ),

      sec("5Ws & 2Hs — from business goal to operational scope"),
      tags$table(class = "ws-table ws-full",
        tags$tr(tags$td(class = "w-label", "Why"),
                tags$td(class = "w-answer", "METRO must defend and grow its position as the leading European B2B wholesaler for HoReCa amid digital disruption. Customer retention and competitive positioning in digital channels are decisive for the sCore 2030 targets (40% digital sales).")),
        tags$tr(tags$td(class = "w-label", "Who"),
                tags$td(class = "w-answer", "Professional HoReCa customers in Europe (restaurants, hotels, caterers); indirectly, METRO's strategy and digital teams who consume the intelligence output.")),
        tags$tr(tags$td(class = "w-label", "What"),
                tags$td(class = "w-answer", "Customer-facing digital services — specifically the e-commerce ordering platform and the mobile commerce app. Internal tools, logistics and supplier-side software are excluded.")),
        tags$tr(tags$td(class = "w-label", "Where"),
                tags$td(class = "w-answer", "Europe (METRO's principal HoReCa market).")),
        tags$tr(tags$td(class = "w-label", "When"),
                tags$td(class = "w-answer", "A 3–5 year forward horizon, with retrospective context from publicly available data up to the present.")),
        tags$tr(tags$td(class = "w-label", "How"),
                tags$td(class = "w-answer", "Digital ordering capabilities act as retention and convenience drivers; competitor evolution and emerging technologies shape what is competitively necessary over a 3–5 year horizon.")),
        tags$tr(tags$td(class = "w-label", "How much"),
                tags$td(class = "w-answer", "Bounded by course resources: open-source data (SCOPUS, Espacenet), no paid data, no web scraping, R-based analysis only."))
      ),

      sec("Negative analysis — what is deliberately out of scope"),
      tags$p(class = "neg-intro",
        "Negative analysis sharpens the scope boundary by stating explicitly what is excluded and why. Each exclusion is a deliberate choice, not an oversight."),
      div(class = "neg-grid",
        div(class = "neg-card",
          div(class = "neg-x", "Internal digital tools"),
          div(class = "neg-r", "CRM, ERP, employee-facing systems — not customer-facing; do not directly affect customer retention or perception.")),
        div(class = "neg-card",
          div(class = "neg-x", "Logistics, transport & warehouse"),
          div(class = "neg-r", "Back-office infrastructure; relevant to operational efficiency but outside the customer-experience focus.")),
        div(class = "neg-card",
          div(class = "neg-x", "Supplier-side & procurement software"),
          div(class = "neg-r", "Upstream, not downstream; outside the HoReCa-customer relationship.")),
        div(class = "neg-card",
          div(class = "neg-x", "Credit, invoicing & billing"),
          div(class = "neg-r", "Financial back-office processes; not part of the ordering experience.")),
        div(class = "neg-card",
          div(class = "neg-x", "Non-European markets (METRO Asia)"),
          div(class = "neg-r", "Different competitive and regulatory contexts; would dilute focus.")),
        div(class = "neg-card",
          div(class = "neg-x", "Real retail / hypermarket legacy"),
          div(class = "neg-r", "Divested business; not part of METRO's current strategic identity."))
      ),
      note_box(tags$strong("Note on geographic exclusion: "),
        "the exclusion of non-European markets refers to the market being studied. Evidence whose origin happens to be outside Europe (international patent databases, global literature) remains admissible — the distinction is between the geography studied and the geography of the source.")
    )
  ),

  # ── 4. SCOPE & KITS ───────────────────────────────────────
  tabPanel("Scope & KITs",
    div(class = "page-wrap",

      div(class = "page-header",
        tags$span(class = "page-title", "Scope & KITs"),
        tags$span(class = "page-sub", "The three KITs and how they fit together")
      ),

      # ── scope statement banner (also shown on Framework) ──
      div(class = "scope-stmt",
        tags$span(class = "scope-stmt-label", "Scope statement"),
        tags$span(class = "scope-stmt-body",
          "This project examines METRO AG's customer-facing digital services for professional HoReCa customers in Europe, with a focus on its e-commerce ordering platform and mobile commerce app, in order to assess how these capabilities contribute to customer retention and competitive positioning over a 3–5 year horizon.")
      ),

      div(class = "three-col",
        stat_card("KIT 1 · Strategic decisions", "Current capabilities",
                  "4 KIQs · SCOPUS · Patents · App reviews", "#185FA5"),
        stat_card("KIT 2 · Early warning", "Emerging trends",
                  "3 KIQs · Patents · SCOPUS literature", "#854F0B"),
        stat_card("KIT 3 · Key players", "Competitive position",
                  "3 KIQs · Competitor matrix · Reviews", "#3B6D11")
      ),

      div(class = "four-col",
        stat_card("Total KIQs", "10"),
        stat_card("Data sources", "4", "Espacenet · SCOPUS · App stores · Web"),
        stat_card("Patents analysed", "2,507"),
        stat_card("Scope horizon", "3–5 yr")
      ),

      note_box(tags$strong("Reading order: "),
        "strategic decisions in KIT 1 are informed by early warnings in KIT 2 and constrained by positioning in KIT 3 — none stands alone. The Framework tab frames the scope; the KIT tabs answer it; Synthesis combines them."),

      sec("Scope → KIT → KIQ map"),
      div(class = "kiqmap",
        div(class = "km-scope", "PROJECT SCOPE"),
        div(class = "km-kits",
          div(class = "km-kit",
            div(class = "km-kit-h kit1", "KIT 1 · Strategic decisions"),
            div(class = "km-kiqs",
              tags$span(class = "km-kiq", "1.1 Functionalities"),
              tags$span(class = "km-kiq", "1.2 Evolution"),
              tags$span(class = "km-kiq", "1.3 Retention"),
              tags$span(class = "km-kiq", "1.4 Strengths / weaknesses"))
          ),
          div(class = "km-kit",
            div(class = "km-kit-h kit2", "KIT 2 · Early warning"),
            div(class = "km-kiqs",
              tags$span(class = "km-kiq", "2.1 Tech trends"),
              tags$span(class = "km-kiq", "2.2 Competitor IP"),
              tags$span(class = "km-kiq", "2.3 Customer expectations"))
          ),
          div(class = "km-kit",
            div(class = "km-kit-h kit3", "KIT 3 · Key players"),
            div(class = "km-kiqs",
              tags$span(class = "km-kiq", "3.1 Main competitors"),
              tags$span(class = "km-kiq", "3.2 Feature comparison"),
              tags$span(class = "km-kiq", "3.3 Customer perception"))
          )
        )
      )
    )
  ),

  # ── 5. KIT 1 ──────────────────────────────────────────────
  tabPanel("KIT 1",
    div(class = "page-wrap kit1-dash",

      div(class = "page-header",
        tags$span(class = "page-title", "KIT 1 — Strategic Decisions",
          tags$span(class = "badge badge-blue", "Current capabilities")),
        tags$span(class = "page-sub", "Intelligence console · KPIs · Data → Insight → Decision")
      ),

      # ── live KPI strip ──
      uiOutput("kit1_kpis"),

      # ── rail (indicators) + main panel ──
      div(class = "kit1-grid",
        div(class = "kit1-rail",
          div(class = "kit1-rail-h", "Indicators"),
          uiOutput("kit1_rail")
        ),
        div(class = "kit1-main",
          uiOutput("kit1_panel")
        )
      )
    )
  ),

  # ── 5. KIT 2 ──────────────────────────────────────────────
  tabPanel("KIT 2",
    div(class = "page-wrap kit1-dash",

      div(class = "page-header",
        tags$span(class = "page-title", "KIT 2 — Early Warning",
          tags$span(class = "badge badge-amber", "Emerging trends")),
        tags$span(class = "page-sub", "Where customer expectations and the technology field are heading (3–5 yr)")
      ),

      uiOutput("kit2_kpis"),

      div(class = "kit1-grid",
        div(class = "kit1-rail",
          div(class = "kit1-rail-h", "Indicators"),
          uiOutput("kit2_rail")
        ),
        div(class = "kit1-main",
          uiOutput("kit2_panel")
        )
      )
    )
  ),

  tabPanel("KIT 3",
    div(class = "page-wrap kit1-dash",

      div(class = "page-header",
        tags$span(class = "page-title", "KIT 3 — Key Players",
          tags$span(class = "badge badge-green", "Positioning")),
        tags$span(class = "page-sub", "Where METRO sits versus traditional and digital-native players in European HoReCa")
      ),

      uiOutput("kit3_kpis"),

      div(class = "kit1-grid",
        div(class = "kit1-rail",
          div(class = "kit1-rail-h", "Indicators"),
          uiOutput("kit3_rail")
        ),
        div(class = "kit1-main",
          uiOutput("kit3_panel")
        )
      )
    )
  ),

  # ── 7. SYNTHESIS ──────────────────────────────────────────
  tabPanel("Synthesis",
    div(class = "page-wrap",

      div(class = "page-header",
        tags$span(class = "page-title", "Strategic Synthesis & Recommendations"),
        tags$span(class = "page-sub", "Intelligence dashboard · Actionable output")
      ),

      div(class = "two-col",
        div(
          sec("Intelligence dashboard"),
          div(class = "card", style = "padding:12px 16px;",
            # ── traffic light rows ──
            div(class = "traffic-row",
              div(class = "dot dot-g"),
              div(div(class = "traffic-area", "Digital maturity"),
                  div(class = "traffic-note", "Strong platform portfolio, growing MAU")),
              div(class = "traffic-level", style = "color:#3B6D11;", "HIGH")),
            div(class = "traffic-row",
              div(class = "dot dot-g"),
              div(div(class = "traffic-area", "Customer retention"),
                  div(class = "traffic-note", "Loyalty, reorder, DISH ecosystem stickiness")),
              div(class = "traffic-level", style = "color:#3B6D11;", "HIGH")),
            div(class = "traffic-row",
              div(class = "dot dot-a"),
              div(div(class = "traffic-area", "Future readiness"),
                  div(class = "traffic-note", "AI/voice/IoT not yet integrated — window open")),
              div(class = "traffic-level", style = "color:#854F0B;", "MEDIUM")),
            div(class = "traffic-row",
              div(class = "dot dot-o"),
              div(div(class = "traffic-area", "Competitive pressure"),
                  div(class = "traffic-note", "Digital-natives closing UX gap")),
              div(class = "traffic-level", style = "color:#D85A30;", "MEDIUM")),
            div(class = "traffic-row",
              div(class = "dot dot-g"),
              div(div(class = "traffic-area", "Long-term positioning"),
                  div(class = "traffic-note", "sCore 2030 targets credible, first-mover available")),
              div(class = "traffic-level", style = "color:#3B6D11;", "HIGH"))
          )
        ),
        div(
          sec("Strategic recommendations"),
          div(class = "rec-row",
            tags$span(class = "rec-num", "R1"),
            div(div(class = "card-title", "Enhance personalisation"),
                div(class = "card-desc", "Integrate AI-driven recommendations into M|Shop/M|Companion."))),
          div(class = "rec-row",
            tags$span(class = "rec-num", "R2"),
            div(div(class = "card-title", "Strengthen mobile-first"),
                div(class = "card-desc", "Address friction in mobile ordering journey from app store reviews."))),
          div(class = "rec-row",
            tags$span(class = "rec-num", "R3"),
            div(div(class = "card-title", "Monitor frontier tech"),
                div(class = "card-desc", "Track voice (G10L) and IoT replenishment (G16Y) — niche now, mainstream in 3–5yr."))),
          div(class = "rec-row",
            tags$span(class = "rec-num", "R4"),
            div(div(class = "card-title", "Benchmark continuously"),
                div(class = "card-desc", "Quarterly benchmarking against Choco, Rekki, and digital-native HoReCa entrants.")))
        )
      ),

      note_box(
        tags$strong("KIT 1 + KIT 2 + KIT 3 combined signal:"),
        " METRO's digital ordering capabilities are solid and defensible today.
          The strategic horizon risk is a UX simplicity gap vs digital-native entrants,
          amplified by an accelerating AI/voice/IoT technology wave that METRO has not
          yet deployed. The deployment window is still open — no European competitor
          has staked out the frontier IP."
      )
    )
  )
)
