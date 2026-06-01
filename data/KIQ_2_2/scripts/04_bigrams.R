# KIQ 2.2 — Script 04: Title bigrams analysis
# Prerequisite: run 01_clean_and_trend.R first

# ---- 0. Setup ----
pkgs <- c("tidyverse", "tidytext", "scales")
new_pkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(new_pkgs)) install.packages(new_pkgs)

library(tidyverse)
library(tidytext)
library(scales)

# Paths
processed_dir <- file.path("/Users/flaurent/Desktop/SCI-shiny-intelligence-app-project/data/KIQ_2_2/processed")
outputs_dir   <- file.path("/Users/flaurent/Desktop/SCI-shiny-intelligence-app-project/data/KIQ_2_2/outputs")

# Style
bar_color <- "#003366"
text_size <- 3.5


# ---- 1. Load cleaned data ----
patents <- readRDS(file.path(processed_dir, "patents_clean.rds"))
cat("Loaded", nrow(patents), "patents.\n")


# ---- 2. Title bigrams analysis ----
# Moderate stopword filter — strips patent-language noise (system,
# method, apparatus, etc.) but keeps domain-meaningful terms
# (platform, marketplace, AI, etc.)

custom_stopwords <- tibble(word = c(
  "system", "systems", "method", "methods", "apparatus", "device",
  "based", "computer", "program", "thereof", "thereon",
  "wherein", "comprises", "comprising", "using"
))

bigrams_data <- patents %>%
  filter(!is.na(Title), Title != "") %>%
  select(Title) %>%
  mutate(Title = str_to_lower(Title)) %>%
  unnest_tokens(bigram, Title, token = "ngrams", n = 2) %>%
  filter(!is.na(bigram)) %>%
  separate(bigram, into = c("w1", "w2"), sep = " ") %>%
  filter(
    !w1 %in% stop_words$word,
    !w2 %in% stop_words$word,
    !w1 %in% custom_stopwords$word,
    !w2 %in% custom_stopwords$word,
    # Restrict to English alphabetic tokens (drops Asian characters & digits)
    str_detect(w1, "^[a-z]+$"),
    str_detect(w2, "^[a-z]+$")
  ) %>%
  unite(bigram, w1, w2, sep = " ") %>%
  count(bigram, sort = TRUE, name = "occurrences") %>%
  slice_head(n = 20)

write_csv(bigrams_data, file.path(processed_dir, "top_bigrams.csv"))


# ---- 3. Chart ----
p_bigrams <- ggplot(bigrams_data,
                    aes(x = reorder(bigram, occurrences), y = occurrences)) +
  geom_col(fill = bar_color, width = 0.7) +
  geom_text(aes(label = occurrences), hjust = -0.2, size = text_size) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(
    title    = "Top 20 title bigrams — B2B / wholesale digital ordering",
    subtitle = "Semantic content of the corpus (stopwords removed, English tokens only)",
    x = NULL, y = "Occurrences"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title         = element_text(face = "bold"),
    plot.subtitle      = element_text(color = "grey40"),
    panel.grid.major.y = element_blank()
  )


p_bigrams

ggsave(file.path(outputs_dir, "04_bigrams.png"), p_bigrams,
       width = 9, height = 7, dpi = 300, bg = "white")

cat("Script 04 complete. Chart saved to:", file.path(outputs_dir, "04_bigrams.png"), "\n")
