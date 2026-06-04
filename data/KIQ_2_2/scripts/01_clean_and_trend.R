# KIQ 2.2 — Script 01: Data cleaning + Trend analysis
# METRO AG SCI Project — B2B / wholesale digital ordering
# Espacenet corpus, 2018–2026, n = 1,219

# ---- 0. Setup ----
# Install missing packages (runs only if not already installed)
pkgs <- c("tidyverse", "scales")
new_pkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(new_pkgs)) install.packages(new_pkgs)

library(tidyverse)
library(scales)


# Paths — adjust project_root to your local path

raw_path      <- file.path( "/Users/flaurent/Desktop/SCI-shiny-intelligence-app-project/data/KIQ_2_2/raw/Espacenet_KIQ_2_2_result.csv")
processed_dir <- file.path( "/Users/flaurent/Desktop/SCI-shiny-intelligence-app-project/data/KIQ_2_2/processed")
outputs_dir   <- file.path( "/Users/flaurent/Desktop/SCI-shiny-intelligence-app-project/data/KIQ_2_2/outputs")

dir.create(processed_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(outputs_dir,   showWarnings = FALSE, recursive = TRUE)

# Style constants (consistent with KIQ 2.1)
bar_color <- "#003366"
text_size <- 3.5


# ---- 1. Load raw data ----
# Espacenet exports: 7 metadata rows before header, ';' separator
patents_raw <- read_delim(
  raw_path,
  delim = ";",
  skip  = 7,
  show_col_types = FALSE
) %>%
  select(-last_col())  # drop trailing empty column


# ---- 2. Clean data ----
patents <- patents_raw %>%
  mutate(
    earliest_priority = as.Date(`Earliest priority`),
    publication_date  = as.Date(`Publication date`),
    priority_year     = year(earliest_priority),
    publication_year  = year(publication_date)
  ) %>%
  filter(publication_year >= 2018, publication_year <= 2025)

# Save processed dataset for use by scripts 02–04
saveRDS(patents, file.path(processed_dir, "patents_clean.rds"))
write_csv(patents, file.path(processed_dir, "patents_clean.csv"))

cat("Loaded and cleaned", nrow(patents), "patents.\n")


# ---- 3. Trend analysis ----
# Using PUBLICATION date (not priority) to match the query window exactly
# and avoid publication-lag artefacts in 2025–2026.
# Consistent with how KIQ 2.1 framed the trend.

trend_data <- patents %>%
  count(publication_year, name = "filings") %>%
  arrange(publication_year)

write_csv(trend_data, file.path(processed_dir, "trend_data.csv"))

p_trend <- ggplot(trend_data, aes(x = publication_year, y = filings)) +
  geom_line(color = bar_color, linewidth = 1.2) +
  geom_point(color = bar_color, size = 3) +
  geom_text(aes(label = filings), vjust = -1.2, size = text_size, color = bar_color) +
  scale_x_continuous(breaks = 2018:2025) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  labs(
    title    = "B2B / wholesale digital ordering patents — annual filings",
    subtitle = "Espacenet, publication date, 2018–2025 (2026 excluded — partial year)",
    x = NULL, y = "Patents published"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold"),
    plot.subtitle    = element_text(color = "grey40"),
    panel.grid.minor = element_blank()
  )

p_trend

