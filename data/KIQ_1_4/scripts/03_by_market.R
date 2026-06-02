# =============================================================================
# 03_by_market.R  —  METRO app average rating by market (language proxy)
# =============================================================================
# Input :  raw/kiq14_metro_app_corpus.csv
# Output:  outputs/kiq14_by_market.png
#
# Run (from Data/KIQ_1_4/):  Rscript scripts/03_by_market.R
# =============================================================================

source("scripts/_helpers.R")
df <- load_metro()
dir.create(DIR_OUT, showWarnings = FALSE)

langmap <- c(de = "Germany (DE)", fr = "France (FR)",
             en = "English (EN)", it = "Italy (IT)")

mkt <- df %>%
  filter(lang_detected %in% names(langmap)) %>%
  group_by(lang_detected) %>%
  summarise(mean_r = mean(rating), n = n(), .groups = "drop") %>%
  mutate(market = langmap[lang_detected]) %>%
  arrange(mean_r)

cat("\n=== METRO app rating by market ===\n"); print(mkt %>% mutate(mean_r = round(mean_r,2)))

p <- ggplot(mkt, aes(x = mean_r, y = reorder(market, mean_r))) +
  geom_col(fill = "#003366", width = 0.65) +
  geom_text(aes(label = sprintf("%.2f * (n=%d)", mean_r, n)),
            hjust = -0.05, fontface = "bold", color = "#003366", size = 3.6) +
  geom_vline(xintercept = 3, linetype = "dashed", color = "grey60") +
  scale_x_continuous(limits = c(0, 5.5)) +
  labs(title = "KIQ 1.4 - METRO app rating by market",
       subtitle = "Average star rating per detected review language",
       x = "Average rating (1-5 stars)", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"),
        panel.grid.major.y = element_blank())

ggsave(file.path(DIR_OUT, "kiq14_by_market.png"),
       p, width = 9, height = 3.8, dpi = 200, bg = "white")
cat("\nSaved", file.path(DIR_OUT, "kiq14_by_market.png"), "\n")
