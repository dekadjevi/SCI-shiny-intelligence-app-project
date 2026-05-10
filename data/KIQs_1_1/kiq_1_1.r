
install.packages(c("readr", "dplyr", "tidyr", "stringr", "wordcloud", "RColorBrewer"))

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(wordcloud)
library(RColorBrewer)

# Load data
data <- read_csv("KIQ_1_1_scopus_export.csv", show_col_types = FALSE)

# Merge keyword columns
all_keywords <- c(data$`Author Keywords`, data$`Index Keywords`)
all_keywords <- all_keywords[!is.na(all_keywords)]

# Split and clean keywords
keywords_tokens <- data.frame(text = all_keywords) %>%
  separate_rows(text, sep = ";") %>%
  mutate(text = str_trim(text),
         text = str_to_lower(text)) %>%
  filter(text != "")

# Remove scope/background terms
remove_terms <- c(
  "e- commerces",
  "e-commerce",
  "electronic commerce",
  "commerce",
  "e commerce",
  "mobile commerce",
  "online commerce"
)

keywords_clean <- keywords_tokens %>%
  filter(!text %in% remove_terms)

# Occurrence analysis
occurrence_table <- keywords_clean %>%
  count(text, sort = TRUE)

# View top 20
head(occurrence_table, 20)

##

install.packages("ggplot2")
library(ggplot2)

top15 <- occurrence_table %>%
  slice_max(n, n = 15) %>%
  arrange(n)

ggplot(top15, aes(x = n, y = reorder(text, n))) +
  geom_col() +
  labs(
    title = "Top Keyword Occurrences Without Scope Terms",
    x = "Frequency",
    y = "Keyword"
  ) +
  theme_minimal(base_size = 12)


#
ggplot(top15, aes(x = n, y = reorder(text, n))) +
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
  words = occurrence_table$text,
  freq = occurrence_table$n,
  max.words = 60,
  random.order = FALSE,
  colors = brewer.pal(8, "Dark2")
)

