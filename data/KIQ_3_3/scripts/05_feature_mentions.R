# =============================================================================
# 05_feature_mentions.R  —  Feature-mention frequency (bridge to KIQ 3.2)
# =============================================================================
# Input :  processed/kiq33_reviews_merged.csv
# Output:  outputs/kiq33_feature_mentions.png
#          outputs/kiq33_feature_mentions.csv
#
# Counts the share of each entity's reviews that mention each feature theme,
# providing an empirical cross-check against the KIQ 3.2 feature heatmap.
#
# Run (from Data/KIQ_3_3/):  Rscript scripts/05_feature_mentions.R
# =============================================================================

source("scripts/_helpers.R")
df <- load_corpus()
dir.create(DIR_OUT, showWarnings = FALSE)

# ---- Feature keyword dictionaries (multilingual) ----
features <- list(
  "Chat / messaging"           = c("\\bchat", "\\bmessag", "\\bnachricht", "\\bsms\\b"),
  "Supplier discovery"         = c("supplier", "fournisseur", "lieferant", "proveedor", "fornitor"),
  "Order / ordering"           = c("\\border", "\\bbestell", "\\bcomand", "\\bpedid", "\\borden"),
  "Scan / barcode"             = c("\\bscan", "\\bbarcode", "code-bar"),
  "Search / find"              = c("\\bsearch", "\\bsuche", "\\brecherch", "\\bbuscar", "\\bcerca"),
  "AI / voice"                 = c("\\bai\\b", "\\bvoice", "\\bsprach", "\\bvocal", "\\bvoz"),
  "Speed / fast / quick"       = c("\\bfast\\b", "\\bquick", "\\bschnell", "\\brapid", "\\bvelo"),
  "Recommendation / suggest"   = c("\\brecommend", "\\bempfehl", "\\bsuggest", "\\brecomend"),
  "Customer service / support" = c("\\bsupport", "\\bservice", "\\bhelp", "\\bhilfe", "\\bayuda", "\\baide")
)

for (f in names(features)) {
  pat <- paste(features[[f]], collapse = "|")
  df[[f]] <- as.integer(str_detect(df$text_lc, regex(pat, ignore_case = TRUE)))
}

feat_pct <- df %>%
  group_by(competitor) %>%
  summarise(across(all_of(names(features)), ~ round(mean(.) * 100, 1)), .groups = "drop")

write_csv(feat_pct, file.path(DIR_OUT, "kiq33_feature_mentions.csv"))
cat("\n=== Feature mentions (% of all reviews per entity) ===\n"); print(feat_pct)

feat_plot <- feat_pct %>% pivot_longer(-competitor, names_to = "feature", values_to = "pct")
order_feat <- feat_plot %>% group_by(feature) %>% summarise(m = mean(pct)) %>%
  arrange(m) %>% pull(feature)
feat_plot$feature <- factor(feat_plot$feature, levels = order_feat)

p <- ggplot(feat_plot, aes(x = pct, y = feature, fill = competitor)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = comp_colors, breaks = c("METRO app", "Choco", "REKKI")) +
  labs(title = "KIQ 3.3 - Feature mentions in app-store reviews per entity",
       subtitle = "Empirical bridge to the KIQ 3.2 feature heatmap",
       x = "Share of reviews mentioning the feature (%)", y = NULL, fill = NULL) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"), legend.position = "bottom")

ggsave(file.path(DIR_OUT, "kiq33_feature_mentions.png"),
       p, width = 12, height = 5.5, dpi = 200, bg = "white")
cat("Saved", file.path(DIR_OUT, "kiq33_feature_mentions.png"), "\n")
