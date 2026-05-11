
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

# Load data
df <- read_csv("KIQ_1_1_scopus_export.csv", show_col_types = FALSE)

# Merge Author Keywords and Index Keywords
keywords_df <- df %>%
  select(`Author Keywords`, `Index Keywords`) %>%
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
  "e-commerces",
  "e-commerce",
  "electronic commerce",
  "commerce",
  "e commerce",
  "mobile commerce",
  "online commerce"
)

keywords_clean <- keywords_tokens %>%
  filter(!keyword %in% remove_terms)

# Occurrence analysis
occurrence_table <- keywords_clean %>%
  count(keyword, sort = TRUE)

# View top 20
head(occurrence_table, 20)

# Top 15 bar chart
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
