# =============================================================================
# run_all.R  —  Run the KIQ 1.4 pipeline
# =============================================================================
# Run (from Data/KIQ_1_4/):  Rscript scripts/run_all.R
# =============================================================================

scripts <- c(
  "scripts/01_strengths_weaknesses.R",
  "scripts/02_companion_vs_shop.R",
  "scripts/03_by_market.R",
  "scripts/04_wordcloud.R"          # optional/decorative; needs 'wordcloud' pkg
)

for (s in scripts) {
  cat("\n==============================\n>>> Running", s, "\n==============================\n")
  tryCatch(source(s, local = new.env()),
           error = function(e) cat("  (skipped:", conditionMessage(e), ")\n"))
}
cat("\nDone. Charts + CSVs in outputs/.\n")
