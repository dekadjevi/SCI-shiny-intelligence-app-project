
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

setwd("/Users/flaurent/Desktop/SCI-shiny-intelligence-app-project/data/KIQ_1_1/raw")
# Load data
df <- read_csv("new.csv", show_col_types = FALSE)

# Merge Author Keywords and Index Keywords
keywords_df <- df %>%
  select(`Author Keywords`,`Index Keywords`) %>%
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

# Remove scope/background terms
remove_terms <- c(
  "e- commerces", "e-commerce", "electronic commerce","commerce platforms",
  "commerce", "e commerce", "mobile commerce", "online commerce",
  "information systems", "information use", "learning systems",
  "decision making", "sales","na","user experience"
)

keywords_clean <- keywords_tokens %>%
  mutate(keyword = str_to_lower(str_trim(keyword))) %>%
  mutate(keyword = case_when(
    keyword %in% c("personalizations") ~ "personalization",
    keyword %in% c("recommender system", "recommendation system", "recommender systems") ~ "recommender systems",
    keyword %in% c("users' experiences", "user experiences") ~ "user experience",
    TRUE ~ keyword
  )) %>%
  filter(!keyword %in% remove_terms)

# Occurrence analysis
occurrence_table <- keywords_clean %>%
  count(keyword, sort = TRUE)

# View top 20
head(occurrence_table, 20)

# Top 20 bar chart
top15 <- occurrence_table %>%
  slice_max(n, n = 15) %>%
  arrange(n)

ggplot(top15, aes(x = n, y = reorder(keyword, n))) +
  geom_col() +
  geom_text(aes(label = n), hjust = -0.2, size = 3.5) +
  labs(
    title = "Top 15 Keyword Occurrences Without Scope Terms",
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

# ---- Top-N bar chart (display-fixed) ----
top_n_val <- 20                       # 20 reads cleaner than 30 (tail is all 2s)

topN <- occurrence_table %>%
  slice_max(n, n = top_n_val, with_ties = FALSE) %>%   # exactly N, no tie explosion
  arrange(n)

p_occ <- ggplot(topN, aes(x = n, y = reorder(keyword, n))) +
  geom_col(fill = "#003366") +
  geom_text(aes(label = n), hjust = -0.3, size = 3.5) +
  labs(
    title = paste0("Top ", top_n_val, " Keyword Occurrences (scope terms removed)"),
    x = "Frequency", y = "Keyword"
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +  # headroom for labels (replaces xlim)
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

print(p_occ)

# ---- Export so the Shiny image-app can display it ----
ggsave("../output/KIQ_1_1_occurrence.png", p_occ,
       width = 8, height = 7, dpi = 200, bg = "white")   # tall enough for N bars

# ---- Word cloud (unchanged core; minor guard) ----
set.seed(123)
p <- wordcloud(
  words = occurrence_table$keyword,
  freq  = occurrence_table$n,
  max.words = 60,
  scale = c(3.2, 0.5),          # prevents the big term from overflowing the canvas
  random.order = FALSE,
  colors = brewer.pal(8, "Dark2")
)

p

