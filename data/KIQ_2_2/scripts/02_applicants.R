# ============================================================
# KIQ 2.2 — Script 02: Top applicants analysis
# Prerequisite: run 01_clean_and_trend.R first
# ============================================================

# ---- 0. Setup ----
pkgs <- c("tidyverse", "scales")
new_pkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(new_pkgs)) install.packages(new_pkgs)

library(tidyverse)
library(scales)

# Paths
processed_dir <- file.path( "/Users/flaurent/Desktop/SCI-shiny-intelligence-app-project/data/KIQ_2_2/processed")
outputs_dir   <- file.path( "/Users/flaurent/Desktop/SCI-shiny-intelligence-app-project/data/KIQ_2_2/outputs")

# Style
bar_color <- "#003366"
text_size <- 3.5


# ---- 1. Load cleaned data ----
patents <- readRDS(file.path(processed_dir, "patents_clean.rds"))
cat("Loaded", nrow(patents), "patents.\n")


# ---- 2. Applicants analysis ----
# Some patents list multiple applicants — split into one row per applicant
applicants_data <- patents %>%
  filter(!is.na(Applicants), Applicants != "") %>%
  separate_rows(Applicants, sep = "[\r\n]+") %>%
  mutate(Applicants = str_squish(Applicants)) %>%
  filter(Applicants != "") %>%
  count(Applicants, sort = TRUE, name = "patents") %>%
  slice_head(n = 20)

write_csv(applicants_data, file.path(processed_dir, "top_applicants.csv"))


# ---- 3. Chart ----
p_applicants <- ggplot(applicants_data,
                       aes(x = reorder(Applicants, patents), y = patents)) +
  geom_col(fill = bar_color, width = 0.7) +
  geom_text(aes(label = patents), hjust = -0.2, size = text_size) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(
    title    = "Top 20 applicants — B2B / wholesale digital ordering",
    subtitle = "No applicant exceeds ~1.1% of the 1,219-patent corpus — extreme fragmentation",
    x = NULL, y = "Patents"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title         = element_text(face = "bold"),
    plot.subtitle      = element_text(color = "grey40"),
    panel.grid.major.y = element_blank()
  )

p_applicants

ggsave(file.path(outputs_dir, "02_applicants.png"), p_applicants,
       width = 10, height = 7, dpi = 300, bg = "white")

cat("Script 02 complete. Chart saved to:", file.path(outputs_dir, "02_applicants.png"), "\n")
