# CO-OCCURRENCE ANALYSIS

# Install if needed
install.packages(c("widyr", "igraph", "ggraph", "tidygraph"))
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(widyr)
library(igraph)
library(ggraph)
library(tidygraph)
library(ggplot2)

# LOAD DATA

setwd("/Users/kodjoflaurent/SCI-shiny-intelligence-app/data/KIQs_1_1/raw")
data <- read_csv(
  "new.csv",
  show_col_types = FALSE
)

# MERGE KEYWORD COLUMNS

all_keywords <- data.frame(
  id = 1:nrow(data),
  keywords = paste(
    data$`Author Keywords`,
    data$`Index Keywords`,
    sep = ";"
  )
)

# SPLIT KEYWORDS

keywords <- all_keywords %>%
  separate_rows(keywords, sep = ";") %>%
  mutate(
    keywords = str_trim(str_to_lower(keywords))
  ) %>%
  filter(keywords != "")

# MERGE SYNONYMS

keywords <- keywords %>%
  mutate(
    keywords = case_when(
      keywords %in% c("user experience", "users' experiences") ~ "user experience",
      keywords %in% c("search engine", "search engines") ~ "search engine",
      keywords %in% c("user interface", "user interfaces") ~ "user interface",
      keywords %in% c("recommender systems", "recommendation systems") ~ "recommender system",
      TRUE ~ keywords
    )
  )

# REMOVE VERY GENERIC TERMS


remove_terms <- c(
  "e- commerces",
  "e-commerce",
  "electronic commerce",
  "commerce",
  "mobile commerce",
  "commerce platforms",
  "na"
)

keywords <- keywords %>%
  filter(!keywords %in% remove_terms)


# CO-OCCURRENCE MATRIX

cooc <- keywords %>%
  pairwise_count(
    item = keywords,
    feature = id,
    sort = TRUE
  )

# View strongest associations
head(cooc, 20)


# FILTER STRONG LINKS

cooc_filtered <- cooc %>%
  filter(n >= 3)

# CREATE NETWORK GRAPH

graph <- graph_from_data_frame(cooc_filtered)

ggraph(graph, layout = "fr") +
  geom_edge_link(alpha = 0.5) +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void() +
  ggtitle("Keyword Co-occurrence Network")

##

# Plot improved network
ggraph(graph, layout = "fr") +
  
  # edges
  geom_edge_link(
    aes(width = n),
    alpha = 0.5,
    color = "gray60"
  ) +
  
  # nodes
  geom_node_point(
    aes(size = degree(graph)),
    color = "steelblue"
  ) +
  
  # labels
  geom_node_text(
    aes(label = name),
    repel = TRUE,
    size = 4
  ) +
  
  scale_edge_width(range = c(0.5, 2)) +
  
  theme_void() +
  
  ggtitle("Keyword Co-occurrence Network") +
  
  theme(
    plot.title = element_text(
      size = 16,
      face = "bold"
    )
  )

