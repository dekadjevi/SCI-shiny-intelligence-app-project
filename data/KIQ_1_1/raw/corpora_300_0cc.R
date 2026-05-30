install.packages(c(
  "readr", "dplyr", "tidyr", "stringr",
  "tidytext", "ggplot2", "wordcloud", "RColorBrewer"
))

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(tidytext)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)

setwd("/Users/kodjoflaurent/SCI-shiny-intelligence-app/data/KIQs_1_1/raw")
# Load data
df_new <- read_csv("new_scopus_latest_1.csv", show_col_types = FALSE)

# Merge Author Keywords and Index Keywords
keywords_df <- df_new %>%
  select(`Author Keywords`,'Index Keywords') %>%
  pivot_longer(
    cols = everything(),
    names_to = "keyword_type",
    values_to = "text"
  ) %>%
  filter(!is.na(text))

# Tokenize keywords using tidytext
keywords_tokens <- keywords_df %>%
  unnest_tokens(
    output = keyword,
    input = text,
    token = "regex",
    pattern = ";"
  ) %>%
  mutate(keyword = str_trim(keyword)) %>%
  filter(keyword != "")

keywords_clean <- keywords_tokens %>%
  mutate(keyword = str_to_lower(str_trim(keyword))) %>%
  
  mutate(keyword = case_when(
    
    # Merge variants
    keyword %in% c("machine-learning") ~ "machine learning",
    
    keyword %in% c(
      "recommendation",
      "recommendation algorithms",
      "product recommendation",
      "personalized recommendation",
      "e-commerce recommendations"
    ) ~ "recommender systems",
    
    keyword %in% c(
      "customer experience",
      "users' experiences",
      "user interfaces"
    ) ~ "user experience",
    
    keyword %in% c(
      "marketplaces"
    ) ~ "marketplace",
    
    keyword %in% c(
      "commerce platforms",
      "e-commerce",
      "electronic commerce",
      "e- commerces",
      "e-commerce platform"
    ) ~ "commerce platform",
    
    TRUE ~ keyword
  )) %>%
  
  # Remove overly generic / technical noise
  filter(!keyword %in% c(
    "websites",
    "commerce platform",
    "e-commerce websites",
    "adversarial machine learning",
    "contrastive learning",
    "natural language processing",
    "sales",
    "online shopping",
    "purchasing",
    "electronic money"
  ))

# Occurrence analysis
occurrence_table <- keywords_clean %>%
  count(keyword, sort = TRUE)

# View top 20
head(occurrence_table, 35)

# Top 15 bar chart
top15 <- occurrence_table %>%
  slice_max(n, n = 30) %>%
  arrange(n)

ggplot(top15, aes(x = n, y = reorder(keyword, n))) +
  geom_col() +
  geom_text(aes(label = n), hjust = -0.2, size = 3.5) +
  labs(
    title = "Top 35 Keyword Occurrences Without Scope Terms",
    x = "Frequency",
    y = "Keyword"
  ) +
  xlim(0, max(top15$n) + 2) +
  theme_minimal(base_size = 12)

# Word cloud
set.seed(123)

wordcloud(
  words = occurrence_table$keyword,
  freq = occurrence_table$n,
  max.words = 60,
  random.order = FALSE,
  colors = brewer.pal(8, "Dark2")
)
