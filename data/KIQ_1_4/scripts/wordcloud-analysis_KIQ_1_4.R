# ==============================================================================
# KIQ 1.4 - WORDCLOUD ANALYSIS (COMPLET)
# ==============================================================================
library(tidyverse)
library(tidytext)
library(tm)
library(wordcloud)
library(RColorBrewer)

# 1. Lecture directe du fichier téléchargé
raw_path <- "data/KIQ_1_4/raw/scopus_KIQ_1_4.csv"

if (!file.exists(raw_path)) {
  stop("Erreur : Le fichier 'scopus_KIQ_1_4.csv' est introuvable dans data/KIQ_1_4/raw/")
}

raw_data <- read_csv(raw_path, col_types = cols())
colnames(raw_data) <- str_to_title(colnames(raw_data))        

# 2. Nettoyage rapide des données
clean_data <- raw_data %>% 
  filter(!is.na(Abstract)) %>% 
  distinct(Title, .keep_all = TRUE)

# 3. Liste des mots inutiles à éliminer (Stopwords + mots vides du domaine)
custom_stop_words <- tibble(word = c(
  stopwords("en"), "system", "method", "paper", "platform", "data", "application", 
  "apps", "channels", "study", "analysis", "results", "b2b", "wholesale", 
  "distribution", "companies", "research", "framework", "model", "using", "based"
))

# 4. Extraction et comptage de TOUS les mots
words_freq <- clean_data %>%
  unnest_tokens(word, Abstract) %>%
  anti_join(custom_stop_words, by = "word") %>%
  filter(!str_detect(word, "^[0-9]+$")) %>% # Supprime les chiffres
  filter(str_length(word) > 2) %>%           # Supprime les mots de moins de 3 lettres
  count(word, sort = TRUE)

# 5. Configuration de la sauvegarde en PNG
png_path <- "data/KIQ_1_4/output/wordcloud_kiq_1_4.png"
png(png_path, width = 800, height = 800, res = 150)

# 6. Génération du Wordcloud (s'affiche à l'écran ET s'enregistre)
set.seed(123) # Pour garder toujours la même disposition des mots
wordcloud(
  words = words_freq$word, 
  freq = words_freq$n, 
  min.freq = 2,           # Mot présent au moins 2 fois
  max_words = 100,         # Top 100 des mots les plus fréquents
  random.order = FALSE,   # Met les plus gros mots au centre
  rot.per = 0.35,         # 35% de mots verticaux pour le style
  colors = brewer.pal(8, "Dark2") # Palette de couleurs pros
)

# Fermeture du fichier PNG
dev.off()

# Réaffichage dans RStudio pour que tu puisses le voir direct
set.seed(123)
wordcloud(
  words = words_freq$word, 
  freq = words_freq$n, 
  min.freq = 2, 
  max_words = 100, s''
  random.order = FALSE, 
  rot.per = 0.35, 
  colors = brewer.pal(8, "Dark2")
)

message("✓ Wordcloud généré à l'écran et enregistré dans data/KIQ_1_4/output/wordcloud_kiq_1_4.png !")