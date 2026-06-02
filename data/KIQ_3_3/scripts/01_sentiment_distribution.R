# =============================================================================
# 01_sentiment_distribution.R  —  Sentiment distribution per entity
# =============================================================================
# Input :  processed/kiq33_reviews_merged.csv
# Output:  outputs/kiq33_sentiment_from_ratings.png
#
# Run (from Data/KIQ_3_3/):  Rscript scripts/01_sentiment_distribution.R
# =============================================================================

source("scripts/_helpers.R")
df <- load_corpus()
dir.create(DIR_OUT, showWarnings = FALSE)

pol <- df %>%
  count(competitor, polarity) %>%
  group_by(competitor) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ungroup() %>%
  mutate(polarity = factor(polarity, levels = c("negative","neutral","positive")))

# order entities by positive share (best at top)
order_lvl <- pol %>% filter(polarity == "positive") %>% arrange(pct) %>% pull(competitor)
pol$competitor <- factor(pol$competitor, levels = order_lvl)

p <- ggplot(pol, aes(x = pct, y = competitor, fill = polarity)) +
  geom_col(width = 0.7, color = "white") +
  geom_text(aes(label = ifelse(pct >= 4, paste0(round(pct), "%"), "")),
            position = position_stack(vjust = 0.5),
            color = "white", fontface = "bold", size = 4) +
  scale_fill_manual(values = c("negative" = "#a31515",
                                "neutral"  = "#aaaaaa",
                                "positive" = "#1e7a4d"),
                    breaks = c("positive","neutral","negative"), name = NULL) +
  labs(title = "KIQ 3.3 - Sentiment distribution per entity",
       subtitle = "From star ratings (1-2 = negative, 3 = neutral, 4-5 = positive)",
       x = "Share of reviews (%)", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "bottom",
        panel.grid.major.y = element_blank())

ggsave(file.path(DIR_OUT, "kiq33_sentiment_from_ratings.png"),
       p, width = 11, height = 4.5, dpi = 200, bg = "white")
cat("Saved", file.path(DIR_OUT, "kiq33_sentiment_from_ratings.png"), "\n")

# Console summary
cat("\n=== Polarity % per entity ===\n")
print(pol %>% select(competitor, polarity, pct) %>%
        pivot_wider(names_from = polarity, values_from = pct) %>%
        mutate(across(where(is.numeric), ~round(.,1))))
