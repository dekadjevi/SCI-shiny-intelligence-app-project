# =============================================================
# KIQ 2.1 — IPC technology domain analysis
# Input : cleaned IPC long-format file
# Output: IPC plot (PNG) in outputs/
# =============================================================

install.packages(c("readr", "dplyr", "ggplot2"))

library(readr)
library(dplyr)
library(ggplot2)

# ---- Paths ----
ipc_path  <- "/Users/flaurent/Desktop/SCI-shiny-intelligence-app-project/data/KIQ_2_1/processed/KIQ_2_1_ipc_long.csv"
plot_path <- "/Users/flaurent/Desktop/SCI-shiny-intelligence-app-project/data/KIQ_2_1/outputs/Rplot_ipc_KIQ2_1.png"

dir.create(dirname(plot_path), recursive = TRUE, showWarnings = FALSE)

# ---- Load cleaned data ----
ipc_long <- read_csv(ipc_path, show_col_types = FALSE)

# ---- IPC main group labels (the canonical meanings) ----
# These are standard WIPO / EPO classifications for the codes most likely
# to appear in a digital ordering / AI commerce corpus.
ipc_labels <- tibble::tribble(
  ~ipc_main, ~label,
  "G06Q",    "Business methods (commerce, admin, finance)",
  "G06F",    "Electric digital data processing",
  "G06N",    "AI / machine learning / neural networks",
  "H04N",    "Pictorial communication (e.g. video)",
  "G06V",    "Image / video recognition",
  "H04L",    "Digital information transmission",
  "G06T",    "Image data processing",
  "G10L",    "Speech / audio analysis (voice ordering)",
  "G16H",    "Healthcare informatics",
  "H04W",    "Wireless communication networks",
  "G16Y",    "Internet of Things (IoT)",
  "G06K",    "Data recognition / coding (QR, OCR)",
  "G05D",    "Control / regulating systems",
  "A61B",    "Diagnosis / surgery (medical)",
  "G05B",    "Control / regulating systems (general)"
)

# ---- Count IPC main groups, take top 12 ----
top_ipc <- ipc_long %>%
  filter(!is.na(ipc_main)) %>%
  count(ipc_main, name = "n_codes", sort = TRUE) %>%
  slice_max(n_codes, n = 12, with_ties = FALSE) %>%
  left_join(ipc_labels, by = "ipc_main") %>%
  mutate(
    label = ifelse(is.na(label), ipc_main, label),
    display = paste0(ipc_main, " — ", label)
  ) %>%
  arrange(n_codes)
top_ipc
# ---- Highlight the "core KIT 2" domains ----
core_ipc <- c("G06Q", "G06F", "G06N", "G10L", "G16Y")
top_ipc <- top_ipc %>%
  mutate(is_core = ipc_main %in% core_ipc)

# ---- Plot ----
p <- ggplot(top_ipc,
            aes(x = n_codes, y = reorder(display, n_codes))) +
  geom_col(aes(fill = is_core), width = 0.7) +
  geom_text(aes(label = n_codes), hjust = -0.25, size = 3.8,
            fontface = "bold", color = "grey20") +
  scale_fill_manual(
    values = c("TRUE" = "#0A2540", "FALSE" = "#9AAEC2"),
    labels = c("TRUE" = "KIT 2 core domain", "FALSE" = "Other"),
    name   = NULL
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(
    title    = "KIQ 2.1 — Top Technology Domains (IPC)",
    subtitle = "Emerging digital ordering technologies, 2022–2026 (Espacenet, n = 874)",
    x        = "Number of IPC code occurrences",
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
    legend.position     = "bottom",
    plot.margin         = margin(15, 20, 15, 15)
  )

p

ggsave(
  filename = plot_path,
  plot     = p,
  width    = 12,
  height   = 7,
  dpi      = 200,
  bg       = "white"
)

# ---- Quick stats ----
cat("\n=== Top 12 IPC main groups ===\n")
print(top_ipc %>% select(ipc_main, label, n_codes, is_core) %>%
        arrange(desc(n_codes)))

p
