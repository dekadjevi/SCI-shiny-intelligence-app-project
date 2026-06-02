# =============================================================================
# 00_merge.R  —  Build merged dual-label corpus (raw -> processed)
# =============================================================================
# Input :  raw/kiq33_reviews_raw.csv          (Colab scrape output)
# Output:  processed/kiq33_reviews_merged.csv  (entity + app_name dual labels)
#          processed/kiq14_metro_app_corpus.csv (METRO app only, for KIQ 1.4)
#
# Run (from Data/KIQ_3_3/):  Rscript scripts/00_merge.R
# =============================================================================

source("scripts/_helpers.R")

# ---- 1. Load raw scraped reviews ----
df <- read_csv(PATH_RAW, show_col_types = FALSE)
cat("Raw corpus loaded:\n"); print(df %>% count(competitor, name = "n_reviews"))

# ---- 2. Create dual labels ----
metro_apps <- c("METRO Companion", "METRO Shop")
df <- df %>%
  mutate(
    app_name = competitor,                                   # preserve product name
    entity   = if_else(competitor %in% metro_apps, "METRO app", competitor)
  )

# ---- 3. Reorder columns ----
preferred <- c("entity", "app_name", "group", "source", "country",
               "lang_query", "rating", "text", "date",
               "thumbs_up", "app_version", "lang_detected")
df <- df %>% select(any_of(preferred))

# ---- 4. Save merged corpus ----
dir.create("processed", showWarnings = FALSE)
write_csv(df, PATH_MERGED)
cat("\nSaved", PATH_MERGED, "\n")

# ---- 5. Save METRO-only corpus for KIQ 1.4 ----
metro <- df %>% filter(entity == "METRO app")
write_csv(metro, PATH_METRO)
cat("Saved", PATH_METRO, "(METRO app only)\n")

# ---- 6. Sanity checks ----
cat("\n=== Corpus by entity ===\n")
print(df %>% count(entity, name = "n_reviews") %>% arrange(desc(n_reviews)))
cat("\n=== METRO app split by app_name ===\n")
print(metro %>% count(app_name, name = "n_reviews"))
cat(sprintf("\nMETRO app: %d reviews | with date: %d | missing: %d\n",
            nrow(metro), sum(!is.na(metro$date)), sum(is.na(metro$date))))
