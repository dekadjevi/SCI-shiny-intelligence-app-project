# =============================================================
# KIQ 2.1 — Title text mining (occurrence)
# Input : cleaned title tokens file
# Output: occurrence plot (PNG) in outputs/
# =============================================================

install.packages(c("readr", "dplyr", "ggplot2"))

library(readr)
library(dplyr)
library(ggplot2)

# ---- Paths ----
tokens_path <- "/Users/flaurent/Desktop/SCI-shiny-intelligence-app-project/data/KIQ_2_1/processed/KIQ_2_1_title_tokens.csv"
plot_path   <- "/Users/flaurent/Desktop/SCI-shiny-intelligence-app-project/data/KIQ_2_1/outputs/Rplot_titles_KIQ2_1.png"

dir.create(dirname(plot_path), recursive = TRUE, showWarnings = FALSE)

# ---- Load tokens ----
title_tokens <- read_csv(tokens_path, show_col_types = FALSE)

# ---- Extra cleaning specific to patent titles ----
# These are generic / boilerplate / overly broad terms that survived
# the first cleaning pass but add no analytical value here.
extra_stopwords <- c(
  "method", "methods", "system", "systems", "based", "using",
  "apparatus", "device", "devices", "data", "information",
  "model", "models", "process", "processing",
  "computer", "electronic", "digital",
  "application", "applications", "service", "services",
  "platform", "platforms", "technology", "technologies",
  "control", "management", "analysis",
  "network", "networks", "module", "unit",
  "present", "invention", "providing", "provide",
  "method", "thereof", "comprising"
)

tokens_clean <- title_tokens %>%
  filter(!word %in% extra_stopwords)

# ---- Occurrence table ----
occurrence <- tokens_clean %>%
  count(word, name = "n", sort = TRUE)

cat("\nUnique terms:", nrow(occurrence), "\n")
cat("Top 30:\n")
print(head(occurrence, 30))

# ---- Tag KIQ 2.1 signal terms ----
# Words that map directly onto the KIQ 2.1 keywords used in the Espacenet query.
signal_terms <- c(
  "artificial", "intelligence",                   # AI
  "learning", "machine", "neural", "deep",        # ML
  "recommendation", "recommender",                # recommender systems
  "voice", "speech", "conversational",            # voice ordering
  "predictive", "prediction", "forecasting",      # predictive ordering
  "autonomous", "automated", "automatic",         # autonomous replenishment
  "chatbot",                                      # chatbots
  "marketplace",                                  # marketplaces
  "commerce", "shopping", "ordering", "order",    # commerce verbs
  "personalization", "personalized",              # personalization
  "blockchain", "smart" ,"intelligent","online","shopping"                          # blockchain / smart procurement
)

# ---- Top 25 plot ----
top25 <- occurrence %>%
  slice_max(n, n = 25, with_ties = FALSE) %>%
  mutate(is_signal = word %in% signal_terms) %>%
  arrange(n)

p <- ggplot(top25, aes(x = n, y = reorder(word, n))) +
  geom_col(aes(fill = is_signal), width = 0.7) +
  geom_text(aes(label = n), hjust = -0.25, size = 3.8,
            fontface = "bold", color = "grey20") +
  scale_fill_manual(
    values = c("TRUE" = "#0A2540", "FALSE" = "#9AAEC2"),
    labels = c("TRUE" = "KIQ 2.1 signal term", "FALSE" = "Other"),
    name   = NULL
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "KIQ 2.1 — Patent Title Term Frequency",
    subtitle = "Most frequent words in patent titles, 2022–2026 (Espacenet, n = 874)",
    x        = "Frequency in patent titles",
    y        = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title          = element_text(face = "bold", size = 15),
    plot.subtitle       = element_text(size = 10, color = "grey40",
                                       margin = margin(b = 12)),
    plot.title.position = "plot",
    panel.grid.major.y  = element_blank(),
    panel.grid.minor    = element_blank(),
    axis.text.y         = element_text(size = 10),
    legend.position     = "bottom",
    plot.margin         = margin(15, 20, 15, 15)
  )

p

ggsave(
  filename = plot_path,
  plot     = p,
  width    = 11,
  height   = 7.5,
  dpi      = 200,
  bg       = "white"
)

