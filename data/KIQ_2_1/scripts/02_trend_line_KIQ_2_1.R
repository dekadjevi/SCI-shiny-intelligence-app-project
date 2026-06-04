# =============================================================
# KIQ 2.1 — Patent trend by year
# Input : cleaned patents file
# Output: trend plot (PNG) in outputs/
# =============================================================
install.packages(c("readr", "dplyr", "ggplot2"))
library(readr)
library(dplyr)
library(ggplot2)

# ---- Paths ----
patents_path <- "/Users/flaurent/Desktop/SCI-shiny-intelligence-app-project/data/KIQ_2_1/processed/KIQ_2_1_patents.csv"
plot_path    <- "/Users/flaurent/Desktop/SCI-shiny-intelligence-app-project/data/KIQ_2_1/outputs/Rplot_trend_KIQ2_1.png"
dir.create(dirname(plot_path), recursive = TRUE, showWarnings = FALSE)

# ---- Load cleaned data ----
patents <- read_csv(patents_path, show_col_types = FALSE)

# ---- Count patents per year ----
trend <- patents %>%
  filter(!is.na(pub_year)) %>%
  count(pub_year, name = "n_patents") %>%
  arrange(pub_year) %>%
  # Flag incomplete year (current year publishes only partially due to lag)
  mutate(is_partial = pub_year == max(pub_year))

# ---- Split for solid (complete) vs dashed (transition to partial) segments ----
n_rows   <- nrow(trend)
solid_df  <- if (n_rows >= 2) trend[1:(n_rows - 1), ] else trend   # complete years
dashed_df <- if (n_rows >= 2) trend[(n_rows - 1):n_rows, ] else trend[0, ]  # last segment

# ---- Plot ----
p <- ggplot(trend, aes(x = pub_year, y = n_patents)) +
  # solid line through the complete years
  geom_line(data = solid_df, color = "#0A2540", linewidth = 1) +
  # dashed segment into the partial (incomplete) year
  geom_line(data = dashed_df, color = "#9AAEC2", linewidth = 1, linetype = "dashed") +
  # points, coloured complete vs partial
  geom_point(aes(color = is_partial), size = 3.2) +
  geom_text(aes(label = n_patents), vjust = -1, size = 4, fontface = "bold",
            color = "grey20") +
  scale_color_manual(
    values = c("FALSE" = "#0A2540", "TRUE" = "#9AAEC2"),
    labels = c("FALSE" = "Complete year", "TRUE" = "Partial year (publication lag)"),
    name   = NULL
  ) +
  scale_x_continuous(breaks = trend$pub_year) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  labs(
    title    = "KIQ 2.1 — Patent Filing Trend",
    subtitle = "Emerging digital ordering technology patents, 2022–2026 (Espacenet, n = 874)",
    x        = NULL,
    y        = "Number of patents published"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title          = element_text(face = "bold", size = 15),
    plot.subtitle       = element_text(size = 10, color = "grey40",
                                       margin = margin(b = 12)),
    plot.title.position = "plot",
    panel.grid.minor    = element_blank(),
    legend.position     = "bottom",
    plot.margin         = margin(15, 15, 15, 15)
  )
p

ggsave(
  filename = plot_path,
  plot     = p,
  width    = 10,
  height   = 6,
  dpi      = 200,
  bg       = "white"
)

# Quick stats for the console
cat("\n=== Patent trend summary ===\n")
print(trend)
cat("\nGrowth 2022 -> 2025 (complete years):  ",
    round(trend$n_patents[trend$pub_year == 2025] /
            trend$n_patents[trend$pub_year == 2022], 2), "x\n")

