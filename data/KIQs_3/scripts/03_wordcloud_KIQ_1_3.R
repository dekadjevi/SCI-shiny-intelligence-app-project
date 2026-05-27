# =============================================================
# KIQ 1.3 — Wordcloud script
# Input : cleaned keyword file
# Output: wordcloud (PNG) in outputs/
# =============================================================
install.packages(c(
  "readr", "dplyr", "wordcloud", "RColorBrewer"
))

library(readr)
library(dplyr)
library(wordcloud)
library(RColorBrewer)

# ---- Paths ----
processed_path <- "C:/Users/flaurent/Desktop/SCI-shiny-intelligence-app-project/data/KIQs_3/processed/KIQ_1_3_cleaned.csv"
plot_path      <- "C:/Users/flaurent/Desktop/SCI-shiny-intelligence-app-project/data/KIQs_3/outputs/Rwordcloud_KIQ1_3.png"

# Make sure the outputs directory exists
dir.create(dirname(plot_path), recursive = TRUE, showWarnings = FALSE)

# ---- Load cleaned data ----
keywords_clean <- read_csv(processed_path, show_col_types = FALSE)

# ---- Build frequency table ----
occurrence_table <- keywords_clean %>%
  count(keyword, sort = TRUE) %>%
  filter(n >= 5)   # drop very rare terms to keep the cloud readable

# ---- Reproducibility (wordcloud layout is randomized) ----
set.seed(42)

# ---- Save wordcloud as PNG ----
png(
  filename = plot_path,
  width    = 2400,
  height   = 1400,
  res      = 200,
  bg       = "white"
)

# Bigger margins help; the default par() leaves almost none for wordcloud
par(mar = c(1, 1, 3, 1))

wordcloud(
  words        = occurrence_table$keyword,
  freq         = occurrence_table$n,
  min.freq     = 5,
  max.words    = 80,                 # fewer words = more room for big ones
  random.order = FALSE,
  rot.per      = 0.0,                # no rotation — long strings stay horizontal
  scale        = c(2.8, 0.5),        # cap the largest size
  colors       = brewer.pal(8, "Dark2")
)

title(
  main     = "KIQ 1.3 — Keyword Wordcloud",
  sub      = "Customer retention, loyalty and convenience signals",
  cex.main = 1.2,
  cex.sub  = 0.9
)
wordcloud

dev.off()


message("Wordcloud saved to: ", plot_path)

