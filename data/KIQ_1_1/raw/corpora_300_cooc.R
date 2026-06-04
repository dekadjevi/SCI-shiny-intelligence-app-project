install.packages(c("widyr", "igraph", "tidygraph", "ggraph"))

library(widyr)
library(igraph)
library(tidygraph)
library(ggraph)

setwd("/Users/flaurent/Desktop/SCI-shiny-intelligence-app-project/data/KIQ_1_1/raw")
# Load data
df_new <- read_csv("new_scopus_latest_1.csv", show_col_types = FALSE)


# Build keyword table with document ID
keywords_doc <- df_new %>%
  mutate(doc_id = row_number()) %>%
  select(doc_id, `Author Keywords`, `Index Keywords`) %>%
  pivot_longer(
    cols = c(`Author Keywords`, `Index Keywords`),
    names_to = "keyword_type",
    values_to = "text"
  ) %>%
  filter(!is.na(text)) %>%
  unnest_tokens(
    output = keyword,
    input = text,
    token = "regex",
    pattern = ";"
  ) %>%
  mutate(keyword = str_to_lower(str_trim(keyword))) %>%
  filter(keyword != "") %>%
  
  mutate(keyword = case_when(
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
    
    keyword %in% c("marketplaces") ~ "marketplace",
    
    keyword %in% c(
      "commerce platforms",
      "e-commerce",
      "electronic commerce",
      "e- commerces",
      "e-commerce platform"
    ) ~ "commerce platform",
    
    TRUE ~ keyword
  )) %>%
  
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
  )) %>%
  
  distinct(doc_id, keyword)

# Keyword co-occurrence
keyword_pairs <- keywords_doc %>%
  pairwise_count(
    item = keyword,
    feature = doc_id,
    sort = TRUE,
    upper = FALSE
  )

# Keep strongest links only
keyword_pairs_filtered <- keyword_pairs %>%
  filter(n >= 4) %>%        # adjust to 2 or 4 if graph is too empty/dense
  slice_max(n, n = 40)

# Build graph
keyword_graph <- keyword_pairs_filtered %>%
  as_tbl_graph(directed = FALSE) %>%
  mutate(degree = centrality_degree())

# Plot network
set.seed(123)

ggraph(keyword_graph, layout = "fr") +
  geom_edge_link(aes(width = n), alpha = 0.5, colour = "grey60") +
  geom_node_point(aes(size = degree), colour = "steelblue") +
  geom_node_text(aes(label = name), repel = TRUE, size = 4) +
  scale_edge_width(range = c(0.3, 2.5)) +
  theme_void() +
  labs(
    title = "Keyword Co-occurrence Network - Corpus B",
    subtitle = "Ordering & Fulfillment Layer"
  )

