# =============================================================================
# 01_strengths_weaknesses.R  —  METRO app strengths vs weaknesses
# =============================================================================
# Input :  raw/kiq14_metro_app_corpus.csv
# Output:  outputs/kiq14_strengths_weaknesses.png
#          outputs/kiq14_strengths.csv, outputs/kiq14_weaknesses.csv
#
# Run (from Data/KIQ_1_4/):  Rscript scripts/01_strengths_weaknesses.R
# =============================================================================

source("scripts/_helpers.R")
df <- load_metro()
dir.create(DIR_OUT, showWarnings = FALSE)

# ---- Weakness categories (multilingual) ----
weak_dict <- list(
  "Authentication / Card / Login" = c("\\blogin","\\banmeld","\\bkarte\\b","\\bcard\\b","\\bcarte\\b",
      "passw","einlog","log in","sign in","\\bkonto\\b","\\bcompte\\b","authenticat"),
  "Crashes / Errors / Bugs" = c("\\bcrash","\\bst\u00fcrzt","\\babst\u00fcrz","\\bfehler","\\berror",
      "\\bbug","freeze","funktioniert nicht","fonctionne pas","ne marche pas","non funziona"),
  "Update regressions" = c("\\bupdate","\\bversion\\b","aktualisier","seit dem","depuis",
      "after.{0,6}update","nach dem update"),
  "Performance / Slow" = c("\\bslow\\b","\\blangsam","\\blent","loading","\\bl\u00e4dt","warten"),
  "Access / blocked" = c("gesperrt","blocked","\\baccess","zugang","zugriff")
)
# ---- Strength categories (multilingual) ----
strong_dict <- list(
  "Digital card / convenience" = c("\\bkarte\\b","\\bcard\\b","\\bcarte\\b","digital","praktisch",
      "pratique","handy","bequem","practical","convenient"),
  "Scan / inventory check" = c("\\bscan","barcode","verf\u00fcgbar","bestand","availab","lager"),
  "Clear / easy to use" = c("\u00fcbersichtlich","einfach","\\beasy","\\bsimple","intuitiv",
      "clair","\\bclear","user friendly","benutzerfreundlich"),
  "Works well / satisfied" = c("zufrieden","funktioniert","bestens","\\btop\\b","\\bsuper",
      "\\bgut\\b","\\bgreat","works well","perfetto","parfait"),
  "Time-saving / fast" = c("\\bzeit","\\btemps","\\btime","schnell","\\bfast\\b","\\bquick","rapid"),
  "Promotions / offers / prices" = c("angebot","prospekt","\\boffer","promo","rabatt","preise","price")
)

neg <- df %>% filter(polarity == "negative")
pos <- df %>% filter(polarity == "positive")

weak   <- tibble(category = names(weak_dict),   pct = classify_pct(neg, weak_dict))   %>% arrange(pct)
strong <- tibble(category = names(strong_dict), pct = classify_pct(pos, strong_dict)) %>% arrange(pct)

write_csv(strong, file.path(DIR_OUT, "kiq14_strengths.csv"))
write_csv(weak,   file.path(DIR_OUT, "kiq14_weaknesses.csv"))

cat(sprintf("\nSTRENGTHS (%% of %d positive reviews):\n", nrow(pos))); print(strong)
cat(sprintf("\nWEAKNESSES (%% of %d negative reviews):\n", nrow(neg))); print(weak)

# ---- Plot: two panels ----
library(patchwork)   # if unavailable, see fallback note below

p_strong <- ggplot(strong, aes(x = pct, y = reorder(category, pct))) +
  geom_col(fill = "#1e7a4d", width = 0.7) +
  geom_text(aes(label = paste0(round(pct), "%")), hjust = -0.15,
            fontface = "bold", color = "#1e7a4d", size = 3.5) +
  scale_x_continuous(limits = c(0, max(strong$pct) * 1.2)) +
  labs(title = sprintf("STRENGTHS (%% of %d positive reviews)", nrow(pos)),
       x = NULL, y = NULL) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", color = "#1e7a4d"),
        panel.grid.major.y = element_blank())

p_weak <- ggplot(weak, aes(x = pct, y = reorder(category, pct))) +
  geom_col(fill = "#a31515", width = 0.7) +
  geom_text(aes(label = paste0(round(pct), "%")), hjust = -0.15,
            fontface = "bold", color = "#a31515", size = 3.5) +
  scale_x_continuous(limits = c(0, max(weak$pct) * 1.2)) +
  labs(title = sprintf("WEAKNESSES (%% of %d negative reviews)", nrow(neg)),
       x = NULL, y = NULL) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", color = "#a31515"),
        panel.grid.major.y = element_blank())

combined <- p_strong + p_weak +
  plot_annotation(title = "KIQ 1.4 - METRO app: strengths vs. weaknesses",
                  subtitle = "Multilingual keyword classification (DE / FR / IT / EN / ES)",
                  theme = theme(plot.title = element_text(face = "bold", size = 13)))

ggsave(file.path(DIR_OUT, "kiq14_strengths_weaknesses.png"),
       combined, width = 14, height = 5, dpi = 200, bg = "white")
cat("\nSaved", file.path(DIR_OUT, "kiq14_strengths_weaknesses.png"), "\n")

# Fallback if 'patchwork' is not installed: save the two panels separately
# ggsave(file.path(DIR_OUT,"kiq14_strengths.png"), p_strong, width=7,height=4,dpi=200,bg="white")
# ggsave(file.path(DIR_OUT,"kiq14_weaknesses.png"), p_weak,  width=7,height=4,dpi=200,bg="white")
