# =============================================================================
# 02_average_rating.R  —  Average app-store rating per entity
# =============================================================================
# Input :  processed/kiq33_reviews_merged.csv
# Output:  outputs/kiq33_avg_rating.png
#
# Run (from Data/KIQ_3_3/):  Rscript scripts/02_average_rating.R
# =============================================================================

source("scripts/_helpers.R")
df <- load_corpus()
dir.create(DIR_OUT, showWarnings = FALSE)

avg <- df %>%
  group_by(competitor) %>%
  summarise(mean_r = mean(rating), n = n(), .groups = "drop") %>%
  arrange(mean_r)

p <- ggplot(avg, aes(x = mean_r, y = reorder(competitor, mean_r), fill = competitor)) +
  geom_col(width = 0.65) +
  geom_text(aes(label = sprintf("%.2f * (n=%d)", mean_r, n)),
            hjust = -0.05, fontface = "bold", size = 4) +
  geom_vline(xintercept = 3, linetype = "dashed", color = "grey60") +
  scale_fill_manual(values = comp_colors) +
  scale_x_continuous(limits = c(0, 5.5)) +
  labs(title = "KIQ 3.3 - Average app-store rating per entity",
       x = "Average rating (1-5 stars)", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "none",
        panel.grid.major.y = element_blank())

ggsave(file.path(DIR_OUT, "kiq33_avg_rating.png"),
       p, width = 10, height = 4, dpi = 200, bg = "white")
cat("Saved", file.path(DIR_OUT, "kiq33_avg_rating.png"), "\n")

cat("\n=== Average rating per entity ===\n")
print(avg %>% mutate(mean_r = round(mean_r, 2)))
