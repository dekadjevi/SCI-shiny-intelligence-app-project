# =============================================================================
# run_all.R  —  Run the full KIQ 3.3 pipeline in order
# =============================================================================
# Run (from Data/KIQ_3_3/):  Rscript scripts/run_all.R
# =============================================================================

scripts <- c(
  "scripts/00_merge.R",
  "scripts/01_sentiment_distribution.R",
  "scripts/02_average_rating.R",
  "scripts/03_painpoints.R",
  "scripts/04_temporal_trend.R",
  "scripts/05_feature_mentions.R"
)

for (s in scripts) {
  cat("\n=====================================================\n")
  cat(">>> Running", s, "\n")
  cat("=====================================================\n")
  source(s, local = new.env())
}

cat("\nPipeline complete. Charts in outputs/, processed CSVs in processed/.\n")
