# =============================================================================
# 02_companion_vs_shop.R  —  METRO Companion vs METRO Shop sentiment
# =============================================================================
# Input :  raw/kiq14_metro_app_corpus.csv
# Output:  outputs/kiq14_companion_vs_shop.png
#
# Uses the app_name column to compare the two METRO apps (different products:
# Companion = digital card/companion; Shop = ordering).
#
# Run (from Data/KIQ_1_4/):  Rscript scripts/02_companion_vs_shop.R
# =============================================================================

source("scripts/_helpers.R")
df <- load_metro()
dir.create(DIR_OUT, showWarnings = FALSE)

split <- df %>%
  count(app_name, polarity) %>%
  group_by(app_name) %>%
  mutate(pct = n / sum(n) * 100, total = sum(n)) %>%
  ungroup() %>%
  mutate(polarity = factor(polarity, levels = c("negative","neutral","positive")))

cat("\n=== Companion vs Shop (% per polarity) ===\n")
print(split %>% select(app_name, polarity, pct) %>%
        pivot_wider(names_from = polarity, values_from = pct) %>%
        mutate(across(where(is.numeric), ~round(.,1))))

labels <- split %>% distinct(app_name, total)

p <- ggplot(split, aes(x = pct, y = app_name, fill = polarity)) +
  geom_col(width = 0.7, color = "white") +
  geom_text(aes(label = ifelse(pct >= 5, paste0(round(pct), "%"), "")),
            position = position_stack(vjust = 0.5),
            color = "white", fontface = "bold", size = 4) +
  geom_text(data = labels, aes(x = 103, y = app_name, label = paste0("n=", total)),
            inherit.aes = FALSE, hjust = 0, size = 3.3, color = "grey40", fontface = "bold") +
  scale_fill_manual(values = c("negative" = "#a31515",
                                "neutral"  = "#aaaaaa",
                                "positive" = "#1e7a4d"),
                    breaks = c("positive","neutral","negative"), name = NULL) +
  scale_x_continuous(limits = c(0, 112)) +
  labs(title = "KIQ 1.4 - METRO Companion vs METRO Shop sentiment",
       x = "Share of reviews (%)", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "bottom",
        panel.grid.major.y = element_blank())

ggsave(file.path(DIR_OUT, "kiq14_companion_vs_shop.png"),
       p, width = 9, height = 3.8, dpi = 200, bg = "white")
cat("\nSaved", file.path(DIR_OUT, "kiq14_companion_vs_shop.png"), "\n")
