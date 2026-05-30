# =============================================================
# KIQ 1.3 — Co-occurrence analysis
# Input : cleaned keyword file (with doc_id)
# Output: co-occurrence network plot (PNG) in outputs

install.packages(c(
  "readr",
  "dplyr",
  "tidyr",
  "igraph",
  "ggraph",
  "ggplot2"
))

library(readr)
library(dplyr)
library(tidyr)
library(igraph)
library(ggraph)
library(ggplot2)

# ---- Paths ----
processed_path <- "C:/Users/flaurent/Desktop/SCI-shiny-intelligence-app-project/data/KIQs_3/processed/KIQ_1_3_cleaned.csv"
edges_path     <- "C:/Users/flaurent/Desktop/SCI-shiny-intelligence-app-project/data/KIQs_3/processed/KIQ_1_3_cooccurrence_edges.csv"
plot_path      <- "C:/Users/flaurent/Desktop/SCI-shiny-intelligence-app-project/data/KIQs_3/outputs/Rplot_cooc_KIQ1_3.png"

dir.create(dirname(plot_path), recursive = TRUE, showWarnings = FALSE)

# ---- Load cleaned data ----
keywords_clean <- read_csv(processed_path, show_col_types = FALSE)

# ---- Restrict to meaningful terms ----
top_terms <- keywords_clean %>%
  count(keyword, sort = TRUE) %>%
  filter(n >= 20) %>%
  pull(keyword)

kw_filtered <- keywords_clean %>%
  filter(keyword %in% top_terms) %>%
  distinct(doc_id, keyword)

# ---- Build co-occurrence edge list ----
edges <- kw_filtered %>%
  inner_join(kw_filtered, by = "doc_id", relationship = "many-to-many") %>%
  filter(keyword.x < keyword.y) %>%
  count(keyword.x, keyword.y, name = "weight", sort = TRUE) %>%
  filter(weight >= 8)               # raised from 5 → drops weakest edges

write_csv(edges, edges_path)

# ---- Node frequencies ----
node_freq <- keywords_clean %>%
  filter(keyword %in% unique(c(edges$keyword.x, edges$keyword.y))) %>%
  count(keyword, name = "freq")

# ---- Tag the retention core (for color emphasis) ----
core_terms <- c(
  "customer satisfaction", "customer loyalty", "service quality",
  "repurchase intention", "trust", "customer experience"
)

node_freq <- node_freq %>%
  mutate(group = if_else(keyword %in% core_terms, "Retention core", "Other"))

# ---- Build igraph object ----
g <- graph_from_data_frame(
  d        = edges,
  vertices = node_freq,
  directed = FALSE
)

# ---- Plot ----
set.seed(42)

p <- ggraph(g, layout = "fr") +
  geom_edge_link(
    aes(width = weight, alpha = weight),
    color = "grey55"
  ) +
  geom_node_point(
    aes(size = freq, color = group)
  ) +
  geom_node_text(
    aes(label = name, size = freq),
    repel        = TRUE,
    family       = "sans",
    fontface     = "bold",
    size         = 3.8 ,
    color        = "grey15",
    bg.color     = "white",
    bg.r         = 0.15,
    max.overlaps = 20,
    point.padding = 0.4
  ) +
  scale_edge_width(range = c(0.2, 3.5)) +
  scale_edge_alpha(range = c(0.25, 0.9)) +
  scale_size(range = c(3, 14), guide = "none") +
  scale_color_manual(
    values = c("Retention core" = "#0A2540", "Other" = "#9AAEC2"),
    name   = NULL
  ) +
  labs(
    title    = "KIQ 1.3 — Keyword Co-occurrence Network",
    subtitle = "Edges = co-mentions across articles; node size = term frequency; dark nodes = retention core"
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title      = element_text(face = "bold", size = 15, hjust = 0),
    plot.subtitle   = element_text(size = 10, hjust = 0, color = "grey40",
                                   margin = margin(b = 10)),
    legend.position = "bottom",
    legend.text     = element_text(size = 10),
    plot.margin     = margin(15, 15, 15, 15)
  )

ggsave(
  filename = plot_path,
  plot     = p,
  width    = 12,
  height   = 9,
  dpi      = 200,
  bg       = "white"
)

message("Co-occurrence edges saved to: ", edges_path)
message("Co-occurrence plot saved to:  ", plot_path)

p
