
# KIQ 2.2 — Script 03: Top IPC classes analysis
# Prerequisite: run 01_clean_and_trend.R first
# ---- 0. Setup ----
pkgs <- c("tidyverse", "scales")
new_pkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(new_pkgs)) install.packages(new_pkgs)

library(tidyverse)
library(scales)

# Paths
processed_dir <- file.path("/Users/flaurent/Desktop/SCI-shiny-intelligence-app-project/data/KIQ_2_2/processed")
outputs_dir   <- file.path("/Users/flaurent/Desktop/SCI-shiny-intelligence-app-project/data/KIQ_2_2/outputs")

# Style
bar_color <- "#003366"
text_size <- 3.5


# ---- 1. Load cleaned data ----
patents <- readRDS(file.path(processed_dir, "patents_clean.rds"))
cat("Loaded", nrow(patents), "patents.\n")


# ---- 2. IPC analysis ----
# Aggregated to IPC class level (4 chars, e.g. G06Q) for direct
# comparison with the KIQ 2.1 chart. Each patent may carry multiple
# IPC codes — split into one row per code.

ipc_data <- patents %>%
  filter(!is.na(IPC)) %>%
  separate_rows(IPC, sep = "[\r\n]+") %>%
  mutate(
    IPC       = str_squish(IPC),
    ipc_class = str_extract(IPC, "^[A-Z][0-9]{2}[A-Z]")  # e.g. G06Q
  ) %>%
  filter(!is.na(ipc_class)) %>%
  count(ipc_class, sort = TRUE, name = "occurrences") %>%
  slice_head(n = 15)

write_csv(ipc_data, file.path(processed_dir, "top_ipc_classes.csv"))


# ---- 3. Chart ----
p_ipc <- ggplot(ipc_data,
                aes(x = reorder(ipc_class, occurrences), y = occurrences)) +
  geom_col(fill = bar_color, width = 0.7) +
  geom_text(aes(label = occurrences), hjust = -0.2, size = text_size) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(
    title    = "Top 15 IPC classes — B2B / wholesale digital ordering",
    subtitle = "G06Q dominates; AI (G06N), voice (G10L), IoT (G16Y) marginal or absent",
    x = NULL, y = "IPC occurrences"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title         = element_text(face = "bold"),
    plot.subtitle      = element_text(color = "grey40"),
    panel.grid.major.y = element_blank()
  )

p_ipc

ggsave(file.path(outputs_dir, "03_ipc_classes.png"), p_ipc,
       width = 9, height = 6, dpi = 300, bg = "white")

cat("Script 03 complete. Chart saved to:", file.path(outputs_dir, "03_ipc_classes.png"), "\n")
