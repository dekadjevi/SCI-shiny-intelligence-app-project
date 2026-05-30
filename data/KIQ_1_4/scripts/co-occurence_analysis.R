# ==============================================================================
# KIQ 1.4 - CO-OCCURRENCE ANALYSIS (AVEC AFFICHAGE)
# ==============================================================================
library(tidyverse)
library(tidytext)
library(tm)
library(igraph)
library(ggraph)

# 1. Lecture directe du fichier téléchargé
raw_path <- "data/KIQ_1_4/raw/scopus_KIQ_1_4.csv"

if (!file.exists(raw_path)) {
  stop("Erreur : Le fichier 'scopus_KIQ_1_4.csv' est introuvable dans data/KIQ_1_4/raw/")
}

raw_data <- read_csv(raw_path, col_types = cols())
colnames(raw_data) <- str_to_title(colnames(raw_data))

# 2. Nettoyage des données
clean_data <- raw_data %>% 
  filter(!is.na(Abstract)) %>% 
  distinct(Title, .keep_all = TRUE)

# 3. Liste des mots inutiles à éliminer
custom_stop_words <- tibble(word = c(
  stopwords("en"), "system", "method", "paper", "platform", "data", "application", 
  "apps", "channels", "study", "analysis", "results", "b2b", "wholesale", 
  "distribution", "companies", "research", "framework", "model", "using", "based"
))

# 4. Extraction des paires de mots (Bigrams)
bigrams_data <- clean_data %>%
  unnest_tokens(bigram, Abstract, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% custom_stop_words$word, !word2 %in% custom_stop_words$word) %>%
  filter(!str_detect(word1, "^[0-9]+$"), !str_detect(word2, "^[0-9]+$")) %>%
  drop_na() %>%
  count(word1, word2, sort = TRUE)

# Sélection du top 50 des liaisons
top_links <- head(bigrams_data, 50)

# 5. Génération du réseau sémantique sécurisé
if (nrow(top_links) > 0) {
  g <- graph_from_data_frame(top_links)
  V(g)$centrality <- degree(g)
  
  set.seed(42)
  plot_cooccurrence <- ggraph(g, layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), edge_colour = "darkgray", show.legend = FALSE) +
    geom_node_point(aes(size = centrality), color = "#dd4b39") + 
    geom_node_text(aes(label = name), vjust = 1.8, hjust = 0.5, fontface = "bold", size = 3.5) +
    labs(
      title = "KIQ 1.4 - Réseau Sémantique des Attributs Qualitatifs",
      subtitle = "Cartographie de proximité des forces et faiblesses perçues"
    ) +
    theme_void() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 13),
      plot.subtitle = element_text(hjust = 0.5, size = 10)
    )
  
  # --- AJOUTS POUR FORCER L'AFFICHAGE ET LA SAUVEGARDE ---
  print(plot_cooccurrence) # Force l'apparition dans l'onglet Plots
  
  ggsave("data/KIQ_1_4/output/plot_cooccurrence_kiq_1_4.png", plot = plot_cooccurrence, width = 9, height = 7)
  message("✓ Réseau de co-occurrences affiché à l'écran et enregistré dans data/KIQ_1_4/output/ !")
  
} else {
  message("⚠️ Pas assez de liaisons trouvées pour générer un graphique de réseau.")
}
