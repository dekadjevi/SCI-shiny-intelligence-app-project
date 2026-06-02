# =============================================================================
# 04_temporal_trend.R  —  Temporal sentiment trend (quarterly average)
# =============================================================================
# Input :  processed/kiq33_reviews_merged.csv
# Output:  outputs/kiq33_temporal_trend.png
#          outputs/kiq33_temporal_trend.csv
#
# Run (from Data/KIQ_3_3/):  Rscript scripts/04_temporal_trend.R
# =============================================================================

source("scripts/_helpers.R")
df <- load_corpus()
dir.create(DIR_OUT, showWarnings = FALSE)

trend <- df %>%
  filter(!is.na(dt), dt >= as.Date("2020-01-01")) %>%
  mutate(yq_dt = floor_date(dt, "quarter")) %>%
  group_by(competitor, yq_dt) %>%
  summarise(avg_rating = mean(rating), n = n(), .groups = "drop") %>%
  filter(n >= 5)                       # need >=5 reviews in a quarter to plot

write_csv(trend, file.path(DIR_OUT, "kiq33_temporal_trend.csv"))

cat("\n=== Trend: first vs last quarter ===\n")
print(trend %>% group_by(competitor) %>%
        summarise(first_q = first(yq_dt), first_r = round(first(avg_rating), 2),
                  last_q  = last(yq_dt),  last_r  = round(last(avg_rating), 2),
                  delta   = round(last(avg_rating) - first(avg_rating), 2),
                  .groups = "drop"))

last_pts <- trend %>% group_by(competitor) %>% slice_max(yq_dt, n = 1) %>%
  mutate(lbl = sprintf("  %.2f*", avg_rating))

p <- ggplot(trend, aes(x = yq_dt, y = avg_rating, color = competitor)) +
  geom_hline(yintercept = 3, linetype = "dashed", color = "grey70") +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.5) +
  geom_text(data = last_pts, aes(label = lbl), hjust = 0,
            fontface = "bold", size = 3.8, show.legend = FALSE) +
  scale_color_manual(values = comp_colors) +
  scale_y_continuous(limits = c(1, 5.3), breaks = 1:5) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "KIQ 3.3 - Temporal trend of app-store sentiment per entity",
       subtitle = "Quarterly average rating, min n=5 reviews",
       x = NULL, y = "Average rating per quarter (1-5 stars)", color = NULL) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"),
        legend.position = c(0.02, 0.5), legend.justification = c(0, 0.5),
        legend.background = element_rect(fill = alpha("white", 0.7), color = NA))

ggsave(file.path(DIR_OUT, "kiq33_temporal_trend.png"),
       p, width = 12, height = 5, dpi = 200, bg = "white")
cat("Saved", file.path(DIR_OUT, "kiq33_temporal_trend.png"), "\n")
