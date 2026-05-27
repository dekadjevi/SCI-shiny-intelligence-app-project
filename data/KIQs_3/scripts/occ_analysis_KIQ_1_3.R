# =============================================================
# KIQ 1.3 — Analysis script
# Input : cleaned keyword file
# Output: occurrence plot (PNG) in outputs/
# =============================================================

install.packages(c(
  "readr", "dplyr", "tidyr", "ggplot2"
))

library(readr)
library(dplyr)
library(ggplot2)

# ---- Paths ----
processed_path <- "C:/Users/flaurent/Desktop/SCI-shiny-intelligence-app-project/data/KIQs_3/processed/KIQ_1_3_cleaned.csv"
plot_path      <- "C:/Users/flaurent/Desktop/SCI-shiny-intelligence-app-project/data/KIQs_3/outputs/Rplot_occ_KIQ1_3.png"

# Make sure the outputs directory exists
dir.create(dirname(plot_path), recursive = TRUE, showWarnings = FALSE)

# ---- Load cleaned data ----
keywords_clean <- read_csv(processed_path, show_col_types = FALSE)

# ---- Occurrence table ----
occurrence_table <- keywords_clean %>%
  count(keyword, sort = TRUE)

head(occurrence_table, 30)

# ---- Top 20 plot ----
top20 <- occurrence_table %>%
  slice_max(n, n = 20) %>%
  arrange(n)

p <- ggplot(top20, aes(x = n, y = reorder(keyword, n))) +
  geom_col(fill = "grey30") +
  geom_text(aes(label = n), hjust = -0.2, size = 3.5) +
  labs(
    title    = "Top Keyword Occurrences - KIQ 1.3",
    subtitle = "Customer retention, loyalty and convenience signals",
    x        = "Frequency",
    y        = "Keyword"
  ) +
  xlim(0, max(top20$n) + 30) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title          = element_text(face = "bold", size = 14, margin = margin(b = 4)),
    plot.subtitle       = element_text(size = 11, margin = margin(b = 10)),
    plot.title.position = "plot",
    plot.margin         = margin(10, 20, 10, 10)
  )

ggsave(
  filename = plot_path,
  plot     = p,
  width    = 10,
  height   = 6,
  dpi      = 150,
  bg       = "white"
)

p
