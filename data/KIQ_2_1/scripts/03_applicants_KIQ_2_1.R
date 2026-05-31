# =============================================================
# KIQ 2.1 — Top patent applicants
# Input : cleaned patents file
# Output: applicants plot (PNG) in outputs/
# =============================================================

install.packages(c("readr", "dplyr", "tidyr", "stringr", "ggplot2"))

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

# ---- Paths ----
patents_path <- "/Users/flaurent/Desktop/SCI-shiny-intelligence-app-project/data/KIQ_2_1/processed/KIQ_2_1_patents.csv"
plot_path    <- "/Users/flaurent/Desktop/SCI-shiny-intelligence-app-project/data/KIQ_2_1/outputs/Rplot_applicants_KIQ2_1.png"

dir.create(dirname(plot_path), recursive = TRUE, showWarnings = FALSE)

# ---- Load cleaned data ----
patents <- read_csv(patents_path, show_col_types = FALSE)

# ---- Split multi-applicant patents into long format ----
# Some patents have several co-applicants separated by " | "
applicants_long <- patents %>%
  select(doc_id, applicants_clean) %>%
  filter(!is.na(applicants_clean)) %>%
  separate_rows(applicants_clean, sep = "\\s*\\|\\s*") %>%
  mutate(applicant = str_trim(applicants_clean)) %>%
  filter(applicant != "") %>%
  distinct(doc_id, applicant)   # avoid double-counting if same applicant appears twice on a patent

# ---- Count and pick top 15 ----
top_applicants <- applicants_long %>%
  count(applicant, name = "n_patents", sort = TRUE) %>%
  slice_max(n_patents, n = 15, with_ties = FALSE) %>%
  arrange(n_patents)

# ---- Plot ----
p <- ggplot(top_applicants,
            aes(x = n_patents, y = reorder(applicant, n_patents))) +
  geom_col(fill = "#0A2540", width = 0.7) +
  geom_text(aes(label = n_patents), hjust = -0.25, size = 3.8,
            fontface = "bold", color = "grey20") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(
    title    = "KIQ 2.1 — Top 15 Patent Applicants",
    subtitle = "Most active filers in digital ordering technologies, 2022–2026 (Espacenet, n = 874)",
    x        = "Number of patents",
    y        = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title          = element_text(face = "bold", size = 15),
    plot.subtitle       = element_text(size = 10, color = "grey40",
                                       margin = margin(b = 12)),
    plot.title.position = "plot",
    panel.grid.major.y  = element_blank(),
    panel.grid.minor    = element_blank(),
    axis.text.y         = element_text(size = 10),
    plot.margin         = margin(15, 20, 15, 15)
  )

p

ggsave(
  filename = plot_path,
  plot     = p,
  width    = 11,
  height   = 7,
  dpi      = 200,
  bg       = "white"
)

# ---- Quick stats for the console ----
cat("\n=== Top 15 applicants ===\n")
print(top_applicants %>% arrange(desc(n_patents)))

cat("\nTotal unique applicants in corpus:",
    n_distinct(applicants_long$applicant), "\n")
cat("Share of corpus covered by top 15:",
    round(100 * sum(top_applicants$n_patents) / n_distinct(applicants_long$doc_id), 1),
    "%\n")
