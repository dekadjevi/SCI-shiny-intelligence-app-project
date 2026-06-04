# =============================================================================
# KIQ 3.1 — Competitor positioning matrix for METRO digital-ordering analysis
# =============================================================================
# Reproducible pipeline:
#   1. Load competitor inventory from CSV (gathered via structured web research:
#      competitor websites, industry reports, app-store listings, analyst coverage).
#   2. Plot the 2x2 strategic positioning matrix:
#        X-axis: Catalog model (single-supplier own catalog <-> multi-supplier marketplace)
#        Y-axis: Business DNA (incumbent <-> digital-native)
#   3. Save as kiq31_positioning_matrix.png.
#
# Run:  Rscript kiq31_plot.R
# =============================================================================

# ---- Packages ----
install.packages(c("tidyverse", "ggrepel"))
library(tidyverse)
library(ggrepel)

# ---- 1. Load competitor inventory ----
setwd("/Users/flaurent/Downloads/KIQ_3_1")
df <- read_csv("KIQ_3_1_competitors.csv", show_col_types = FALSE)

cat("Loaded", nrow(df), "competitor platforms across",
    n_distinct(df$group), "strategic groups:\n")
print(df %>% select(name, group), n = Inf)

# ---- 2. Color mapping (must match Python version for consistency) ----
color_map <- c(
  "METRO (reference)"             = "#003366",
  "Incumbent B2B foodservice"     = "#7a1f1f",
  "Digital-native B2B HoReCa"     = "#1e7a4d",
  "Adjacent giant (B2C-to-B2B)"   = "#d97706"
)

# ---- 3. Build figure ----
p <- ggplot(df, aes(x = x_pos, y = y_pos, color = group)) +
  
  # Quadrant background shading
  annotate("rect", xmin = 0,    xmax = 1.1,  ymin = 0,    ymax = 1.1,
           fill = "#1e7a4d", alpha = 0.05) +
  annotate("rect", xmin = -1.1, xmax = 0,    ymin = -1.1, ymax = 0,
           fill = "#7a1f1f", alpha = 0.05) +
  
  # Reference axes
  geom_hline(yintercept = 0, color = "#555555", linewidth = 0.5) +
  geom_vline(xintercept = 0, color = "#555555", linewidth = 0.5) +
  
  # Competitor bubbles
  geom_point(size = 8, alpha = 0.85, stroke = 1.5, shape = 21,
             aes(fill = group), color = "white") +
  
  # Labels with auto-placement (no overlap)
  geom_text_repel(aes(label = name),
                  size = 3.6, fontface = "bold",
                  box.padding = 0.6, point.padding = 0.4,
                  segment.color = "grey60", segment.size = 0.3,
                  max.overlaps = Inf, seed = 42) +
  
  # Quadrant labels
  annotate("text", x = -0.95, y =  1.00,
           label = "Digital-native\nsingle-supplier\n(rare in market)",
           color = "#888888", fontface = "italic", hjust = 0, vjust = 1, size = 3.3) +
  annotate("text", x =  0.95, y =  1.05,
           label = "Digital-native\nmarketplace / aggregator",
           color = "#1e7a4d", fontface = "bold.italic", hjust = 1, vjust = 1, size = 3.8) +
  annotate("text", x = -0.95, y = -1.00,
           label = "Incumbent\nsingle-supplier e-commerce",
           color = "#7a1f1f", fontface = "bold.italic", hjust = 0, vjust = 0, size = 3.8) +
  annotate("text", x =  0.95, y = -1.00,
           label = "Incumbent\nmarketplace (emerging)",
           color = "#7a1f1f", fontface = "bold.italic", hjust = 1, vjust = 0, size = 3.8) +
  
  # Colors
  scale_color_manual(values = color_map) +
  scale_fill_manual(values  = color_map) +
  
  # Axes
  coord_cartesian(xlim = c(-1.15, 1.15), ylim = c(-1.15, 1.15)) +
  
  labs(
    title    = "KIQ 3.1 — Competitive positioning of digital ordering platforms for European HoReCa",
    x        = "← Single-supplier (own catalog)        Catalog model        Multi-supplier marketplace →",
    y        = "← Incumbent        Business DNA        Digital-native →",
    color    = NULL,
    fill     = NULL
  ) +
  
  theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 13, hjust = 0),
    axis.text        = element_blank(),
    axis.ticks       = element_blank(),
    axis.title.x     = element_text(face = "bold", margin = margin(t = 10)),
    axis.title.y     = element_text(face = "bold", margin = margin(r = 10)),
    panel.grid       = element_blank(),
    legend.position  = "bottom",
    legend.text      = element_text(size = 9),
    legend.key       = element_blank()
  ) +
  
  guides(color = guide_legend(nrow = 1, override.aes = list(size = 5)),
         fill  = "none")

# ---- 4. Save ----
ggsave("kiq31_positioning_matrix.png", plot = p,
       width = 13, height = 9, dpi = 300, bg = "white")

cat("\nSaved: kiq31_positioning_matrix.png\n")
