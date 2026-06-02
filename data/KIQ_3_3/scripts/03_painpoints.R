# =============================================================================
# 03_painpoints.R  —  Pain-point categorization of NEGATIVE reviews
# =============================================================================
# Input :  processed/kiq33_reviews_merged.csv
# Output:  outputs/kiq33_painpoints.png
#          outputs/kiq33_painpoints_summary.csv
#
# Multilingual keyword classification (EN/DE/FR/IT/ES) of negative reviews
# into pain-point categories.
#
# Run (from Data/KIQ_3_3/):  Rscript scripts/03_painpoints.R
# =============================================================================

source("scripts/_helpers.R")
df <- load_corpus()
dir.create(DIR_OUT, showWarnings = FALSE)

# ---- Category keyword dictionaries (multilingual) ----
cats <- list(
  "Authentication / Card / Login" = c(
    "\\blogin", "\\banmeld", "\\bkarte\\b", "\\bcard\\b", "\\bcarte\\b",
    "passw", "einlog", "log in", "log on", "sign in", "\\bcompte\\b",
    "\\baccount\\b", "\\bkonto\\b", "\\bcuenta\\b", "authenticat"),
  "Crashes / Errors / Bugs" = c(
    "\\bcrash", "\\bst\u00fcrzt", "\\babst\u00fcrz", "\\bfehler", "\\berror", "\\bbug",
    "freeze", "einfrier", "\\bplante", "\\bplanta", "doesn.{0,3}work",
    "funktioniert nicht", "fonctionne pas", "ne marche pas",
    "no funciona", "non funziona"),
  "Update regressions" = c(
    "\\bupdate", "\\bversion\\b", "aktualisier", "mise \u00e0 jour",
    "seit (dem|der)", "depuis (la|le)", "after (the|last) update",
    "nach dem update", "apres la mise", "desde la actualiz"),
  "Performance / Slow / Loading" = c(
    "\\bslow\\b", "\\blangsam", "\\blent\\b", "\\blenta", "\\blento",
    "loading", "\\bl\u00e4dt", "\\bcharge", "\\bwait\\b", "warten"),
  "Useless / Bad" = c(
    "\\buseless", "\\bunusable", "\\bschrott", "\\bm\u00fcll", "rubbish",
    "horrible", "terrible", "pessim", "sehr schlecht", "tres mauvais",
    "\\bawful", "\\binut"),
  "Missing features / Requests" = c(
    "\\bshould", "\\bsollte", "\\bdevrait", "\\bwish", "\\bw\u00fcnsch", "would like",
    "\\bdeber\u00eda", "\\bdovrebbe", "\\bmanque", "\\blacking", "\\bfehlt")
)

neg <- df %>% filter(polarity == "negative")
for (cat in names(cats)) {
  pat <- paste(cats[[cat]], collapse = "|")
  neg[[cat]] <- as.integer(str_detect(neg$text_lc, regex(pat, ignore_case = TRUE)))
}
neg[["Other (unclassified)"]] <- as.integer(rowSums(neg[, names(cats)]) == 0)

pain_pct <- neg %>%
  group_by(competitor) %>%
  summarise(across(all_of(c(names(cats), "Other (unclassified)")),
                   ~ round(mean(.) * 100, 1)),
            n = n(), .groups = "drop")

write_csv(pain_pct, file.path(DIR_OUT, "kiq33_painpoints_summary.csv"))
cat("\n=== Pain-points (% of NEGATIVE reviews per entity) ===\n"); print(pain_pct)

pain_long <- pain_pct %>%
  pivot_longer(-c(competitor, n), names_to = "category", values_to = "pct")
metro_order <- pain_pct %>% filter(competitor == "METRO app") %>%
  select(-competitor, -n) %>% pivot_longer(everything()) %>%
  arrange(desc(value)) %>% pull(name)
pain_long$category <- factor(pain_long$category, levels = rev(metro_order))

p <- ggplot(pain_long, aes(x = pct, y = category, fill = competitor)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = comp_colors, breaks = c("METRO app", "Choco", "REKKI")) +
  labs(title = "KIQ 3.3 - Pain-point categorization of negative reviews per entity",
       subtitle = "Multilingual keyword classification (EN / DE / FR / IT / ES)",
       x = "Share of negative reviews touching the theme (%)", y = NULL, fill = NULL) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"), legend.position = "bottom")

ggsave(file.path(DIR_OUT, "kiq33_painpoints.png"),
       p, width = 12, height = 5, dpi = 200, bg = "white")
cat("Saved", file.path(DIR_OUT, "kiq33_painpoints.png"), "\n")
