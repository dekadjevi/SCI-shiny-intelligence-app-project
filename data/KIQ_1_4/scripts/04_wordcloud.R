# =============================================================================
# 04_wordcloud.R  —  Decorative TF-weighted word clouds (positive / negative)
# =============================================================================
# Input :  raw/kiq14_metro_app_corpus.csv
# Output:  outputs/kiq14_wordcloud_positive.png
#          outputs/kiq14_wordcloud_negative.png
#
# NOTE: word clouds are a presentation garnish, not a core analytical exhibit.
# The strengths/weaknesses categorization (script 01) is the defensible answer.
#
# Needs: wordcloud, RColorBrewer   install.packages(c("wordcloud","RColorBrewer"))
# Run (from Data/KIQ_1_4/):  Rscript scripts/04_wordcloud.R
# =============================================================================

source("scripts/_helpers.R")
suppressPackageStartupMessages({ library(wordcloud); library(RColorBrewer) })
df <- load_metro()
dir.create(DIR_OUT, showWarnings = FALSE)

# Multilingual stopwords + app-domain noise
stop_words <- c(
  "the","a","an","and","or","but","if","is","are","was","to","of","in","on","for","with","it","this","that","i","you","my","me",
  "der","die","das","und","ist","ich","nicht","auch","ein","eine","mit","f\u00fcr","den","im","zu","auf","man","sich","sehr","aber","wie","noch","nur","es","so",
  "le","la","les","un","une","de","et","est","pour","pas","plus","je","ne","que","des","du","au",
  "il","lo","gli","di","che","per","non","con","una","sono",
  "el","los","las","que","con","por","para","una",
  "app","metro","application","aplikation"
)

make_cloud <- function(sub, file, palette) {
  words <- sub$text_lc %>%
    str_replace_all("[^a-z\u00e4\u00f6\u00fc\u00df\u00e0\u00e9\u00e8\u00ea ]", " ") %>%
    str_split("\\s+") %>% unlist()
  words <- words[nchar(words) >= 3 & !words %in% stop_words]
  freq  <- sort(table(words), decreasing = TRUE)
  freq  <- freq[freq >= 3]
  png(file, width = 1400, height = 1000, res = 200)
  set.seed(42)
  wordcloud(names(freq), as.integer(freq), min.freq = 3, max.words = 80,
            random.order = FALSE, rot.per = 0.2,
            colors = brewer.pal(8, palette), scale = c(4, 0.7))
  dev.off()
  cat("Saved", file, "\n")
}

make_cloud(df %>% filter(polarity == "positive"),
           file.path(DIR_OUT, "kiq14_wordcloud_positive.png"), "Greens")
make_cloud(df %>% filter(polarity == "negative"),
           file.path(DIR_OUT, "kiq14_wordcloud_negative.png"), "Reds")
