# =============================================================
# KIQ 2.1 — Title bigram frequency
# Input : cleaned patents file (re-tokenize for bigrams)
# Output: bigram plot (PNG) in outputs/
# =============================================================

install.packages(c("readr", "dplyr", "tidyr", "stringr", "tidytext", "ggplot2"))

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(tidytext)
library(ggplot2)

# ---- Paths ----
patents_path <- "/Users/flaurent/Desktop/SCI-shiny-intelligence-app-project/data/KIQ_2_1/processed/KIQ_2_1_patents.csv"
plot_path    <- "/Users/flaurent/Desktop/SCI-shiny-intelligence-app-project/data/KIQ_2_1/outputs/Rplot_bigrams_KIQ2_1.png"

dir.create(dirname(plot_path), recursive = TRUE, showWarnings = FALSE)

# ---- Load patents (re-tokenize titles into bigrams) ----
patents <- read_csv(patents_path, show_col_types = FALSE)

# Same stopwords as the unigram analysis
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
  "thereof", "comprising", "medium", "equipment", "storage",
  "generation", "user", "product"
)

# ---- Build bigrams ----
bigrams <- patents %>%
  select(doc_id, title) %>%
  filter(!is.na(title)) %>%
  unnest_tokens(bigram, title, token = "ngrams", n = 2) %>%
  filter(!is.na(bigram)) %>%
  separate(bigram, into = c("w1", "w2"), sep = " ", remove = FALSE) %>%
  # drop bigrams where either word is a stopword or boilerplate
  filter(!w1 %in% stop_words$word, !w2 %in% stop_words$word) %>%
  filter(!w1 %in% extra_stopwords, !w2 %in% extra_stopwords) %>%
  # drop bigrams with non-letter tokens (numbers, codes)
  filter(str_detect(w1, "^[a-z]+$"), str_detect(w2, "^[a-z]+$")) %>%
  filter(nchar(w1) >= 3, nchar(w2) >= 3)

# ---- Count bigrams ----
bigram_counts <- bigrams %>%
  count(bigram, name = "n", sort = TRUE)

cat("\nUnique bigrams:", nrow(bigram_counts), "\n")
cat("Top 30:\n")
print(head(bigram_counts, 30))

# ---- Tag KIQ 2.1 signal bigrams ----
signal_bigrams <- c(
  "artificial intelligence",
  "machine learning",
  "deep learning",
  "neural network",
  "recommendation system",
  "recommendation engine",
  "recommender system",
  "voice ordering",
  "voice recognition",
  "speech recognition",
  "predictive ordering",
  "autonomous replenishment",
  "smart procurement",
  "smart contract",
  "conversational commerce",
  "digital marketplace",
  "online shopping",
  "online ordering",
  "e commerce",
  "cross border",
  "supply chain",
  "augmented reality",
  "virtual reality",
  "commodity recommendation",
  "intelligent recommendation",
  "resource recommendation",
  "intelligent agent",
  "generative artificial",
  "intelligence internet",
  "commerce learning",
  "commerce intelligent"
)

# ---- Top 20 plot ----
top20 <- bigram_counts %>%
  slice_max(n, n = 20, with_ties = FALSE) %>%
  mutate(is_signal = bigram %in% signal_bigrams) %>%
  arrange(n)

p <- ggplot(top20, aes(x = n, y = reorder(bigram, n))) +
  geom_col(aes(fill = is_signal), width = 0.7) +
  geom_text(aes(label = n), hjust = -0.25, size = 3.8,
            fontface = "bold", color = "grey20") +
  scale_fill_manual(
    values = c("TRUE" = "#0A2540", "FALSE" = "#9AAEC2"),
    labels = c("TRUE" = "KIQ 2.1 signal bigram", "FALSE" = "Other"),
    name   = NULL
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(
    title    = "KIQ 2.1 — Patent Title Bigram Frequency",
    subtitle = "Most frequent two-word phrases in patent titles, 2022–2026 (Espacenet, n = 874)",
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

p
