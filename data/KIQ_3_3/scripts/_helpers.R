# =============================================================================
# _helpers.R  —  shared setup for all KIQ 3.3 analysis scripts
# =============================================================================
# Sourced by every 01..05 script. Defines libraries, paths, colours, and the
# load_corpus() function (load merged CSV, apply n>=50 threshold, derive
# polarity + parsed date).
#
# ASSUMPTION: working directory is  Data/KIQ_3_3/
#   (so that "raw/...", "processed/...", "outputs/..." resolve correctly)
# In RStudio: Session > Set Working Directory > To Source File Location,
#   then setwd("..") if your script sits in scripts/.
# Or from a terminal:  cd Data/KIQ_3_3 && Rscript scripts/01_sentiment_distribution.R
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(scales)
  library(stringr)
})

# ---- Paths (relative to Data/KIQ_3_3/) ----
PATH_RAW    <- "raw/kiq33_reviews_raw.csv"
PATH_MERGED <- "processed/kiq33_reviews_merged.csv"
PATH_METRO  <- "processed/kiq14_metro_app_corpus.csv"
DIR_OUT     <- "outputs"

# ---- Shared colours ----
comp_colors <- c("METRO app" = "#003366",
                 "Choco"     = "#1e7a4d",
                 "REKKI"     = "#7a3a8c")

# ---- Load merged corpus, threshold, derive fields ----
load_corpus <- function(threshold = 50) {
  df <- read_csv(PATH_MERGED, show_col_types = FALSE) %>%
    mutate(competitor = entity)            # group by unified entity

  counts     <- df %>% count(competitor)
  qualifying <- counts %>% filter(n >= threshold) %>% pull(competitor)
  message("Qualifying entities (n>=", threshold, "): ",
          paste(qualifying, collapse = ", "))

  df %>%
    filter(competitor %in% qualifying) %>%
    mutate(
      polarity = case_when(rating <= 2 ~ "negative",
                           rating == 3 ~ "neutral",
                           rating >= 4 ~ "positive"),
      text_lc  = tolower(text),
      dt       = parse_date_time(date, orders = c("YmdHMS","Ymd","YmdHMSz","ymd HMS"))
    )
}
