# =============================================================================
# _helpers.R  —  shared setup for KIQ 1.4 (METRO app strengths & weaknesses)
# =============================================================================
# ASSUMPTION: working directory is  Data/KIQ_1_4/
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
  library(stringr)
})

# ---- Paths (relative to Data/KIQ_1_4/) ----
PATH_RAW <- "raw/kiq14_metro_app_corpus.csv"   # METRO app only (Companion + Shop)
DIR_OUT  <- "outputs"

# ---- Load METRO corpus + derive polarity ----
load_metro <- function() {
  read_csv(PATH_RAW, show_col_types = FALSE) %>%
    mutate(
      polarity = case_when(rating <= 2 ~ "negative",
                           rating == 3 ~ "neutral",
                           rating >= 4 ~ "positive"),
      text_lc  = tolower(text)
    )
}

# ---- Keyword classifier helper: % of rows matching each category ----
classify_pct <- function(data, dict) {
  map_dbl(dict, function(pats) {
    pat <- paste(pats, collapse = "|")
    round(mean(str_detect(data$text_lc, regex(pat, ignore_case = TRUE))) * 100, 1)
  })
}
