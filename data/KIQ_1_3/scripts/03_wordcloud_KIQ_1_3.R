# =============================================================
# KIQ 1.3 — Wordcloud script (ggwordcloud version)
# Input : cleaned keyword file
# Output: wordcloud (PNG) in outputs/
# =============================================================
install.packages(c(
  "readr", "dplyr", "tidyr", "stringr","ggwordcloud",
  "tidytext", "ggplot2", "wordcloud", "RColorBrewer"
))

library(readr)
library(dplyr)
library(ggplot2)
library(ggwordcloud)

# ---- Paths ----
processed_path <- "C:/Users/flaurent/Desktop/SCI-shiny-intelligence-app-project/data/KIQs_3/processed/KIQ_1_3_cleaned.csv"
plot_path      <- "C:/Users/flaurent/Desktop/SCI-shiny-intelligence-app-project/data/KIQs_3/outputs/Rwordcloud_KIQ1_3.png"

dir.create(dirname(plot_path), recursive = TRUE, showWarnings = FALSE)

# ---- Load cleaned data ----
keywords_clean <- read_csv(processed_path, show_col_types = FALSE)

# ---- Build frequency table ----
occurrence_table <- keywords_clean %>%
  count(keyword, sort = TRUE) %>%
  filter(n >= 10) %>%
  slice_max(n, n = 80)

set.seed(42)

p <- ggplot(occurrence_table,
            aes(label = keyword, size = n, color = n)) +
  geom_text_wordcloud(
    rm_outside = FALSE,        # nothing gets dropped silently
    eccentricity = 1,          # roundish cloud, not stretched
    shape = "circle"
  ) +
  scale_size_area(max_size = 18) +
  scale_color_viridis_c(option = "mako", direction = -1)+
  labs(
    title    = "KIQ 1.3 — Keyword Wordcloud",
    subtitle = "Customer retention, loyalty and convenience signals"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    plot.margin   = margin(10, 10, 10, 10)
  )

ggsave(
  filename = plot_path,
  plot     = p,
  width    = 12,
  height   = 7,
  dpi      = 200,
  bg       = "white"
)

message("Wordcloud saved to: ", plot_path)
