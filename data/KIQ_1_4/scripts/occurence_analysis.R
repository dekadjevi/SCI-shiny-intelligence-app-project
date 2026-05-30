# ==============================================================================
# KIQ 1.4 - OCCURRENCE ANALYSIS ONLY (AVEC AFFICHAGE)
# ==============================================================================
library(tidyverse)
library(tidytext)
library(tm)

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

# 3. Liste des mots inutiles à éliminer
custom_stop_words <- tibble(word = c(
  stopwords("en"), "system", "method", "paper", "platform", "data", "application", 
  "apps", "channels", "study", "analysis", "results", "b2b", "wholesale", 
  "distribution", "companies", "research", "framework", "model", "using", "based"
))

# 4. Extraction et comptage des termes (Unigrams)
unigrams_data <- clean_data %>%
  unnest_tokens(word, Abstract) %>%
  anti_join(custom_stop_words, by = "word") %>%
  filter(!str_detect(word, "^[0-9]+$")) %>% 
  count(word, sort = TRUE) %>%
  head(15)

# 5. Génération du graphique
plot_occurrence <- ggplot(unigrams_data, aes(x = reorder(word, n), y = n)) +
  geom_col(fill = "#00a65a", width = 0.7) + 
  coord_flip() +
  labs(
    title = "KIQ 1.4 - Top 15 des Concepts Clés (Forces & Faiblesses)",
    subtitle = "Fréquence des termes associés à l'utilisabilité et aux retours utilisateurs",
    x = "Concepts", y = "Fréquence"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

# --- AJOUTS POUR FORCER L'AFFICHAGE ET LA SAUVEGARDE ---
print(plot_occurrence) # Force l'apparition dans l'onglet Plots

ggsave("data/KIQ_1_4/output/plot_occurrence_kiq_1_4.png", plot = plot_occurrence, width = 8, height = 6)
message("✓ Graphique d'occurrences affiché à l'écran et enregistré dans data/KIQ_1_4/output/ !")
