


# KIQ 1.3 — Cleaning script
# Input : raw SCOPUS export
# Output: tidy long-format CSV of normalized keywords (with doc_id)

install.packages(c(
  "readr", "dplyr", "tidyr", "stringr",
  "tidytext"
))

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(tidytext)

# ---- Paths ----
raw_path       <- "C:/Users/flaurent/Desktop/SCI-shiny-intelligence-app-project/data/KIQs_3/raw/scopus_KIQ_1_3.csv"
processed_path <- "C:/Users/flaurent/Desktop/SCI-shiny-intelligence-app-project/data/KIQs_3/processed/KIQ_1_3_cleaned.csv"

# Make sure the processed directory exists
dir.create(dirname(processed_path), recursive = TRUE, showWarnings = FALSE)

# ---- Load raw data ----
df <- read_csv(raw_path, show_col_types = FALSE)

# Add doc_id so we can later trace each keyword back to its source document
# (needed for co-occurrence analysis down the line)
df <- df %>% mutate(doc_id = row_number())

# ---- Merge Author Keywords and Index Keywords ----
keywords_df <- df %>%
  select(doc_id, `Author Keywords`, `Index Keywords`) %>%
  pivot_longer(
    cols      = c(`Author Keywords`, `Index Keywords`),
    names_to  = "keyword_type",
    values_to = "text"
  ) %>%
  filter(!is.na(text))

# ---- Tokenize on ';' ----
keywords_tokens <- keywords_df %>%
  unnest_tokens(
    output  = keyword,
    input   = text,
    token   = "regex",
    pattern = ";"
  ) %>%
  mutate(keyword = str_trim(keyword)) %>%
  filter(keyword != "")

# ---- Normalize variants ----
keywords_clean <- keywords_tokens %>%
  mutate(keyword = str_to_lower(str_trim(keyword))) %>%
  mutate(keyword = case_when(
    
    # Satisfaction variants (now includes bare "satisfaction")
    keyword %in% c(
      "customer satisfaction",
      "customers' satisfaction",
      "e-satisfaction",
      "online satisfaction",
      "consumer satisfaction",
      "satisfaction"
    ) ~ "customer satisfaction",
    
    # Loyalty variants
    keyword %in% c(
      "customer loyalty",
      "e-loyalty",
      "loyalty",
      "online loyalty"
    ) ~ "customer loyalty",
    
    # Repurchase / repeat use variants
    keyword %in% c(
      "repurchase intention",
      "online repurchase intention",
      "repeat purchase",
      "repeat purchase intention",
      "continuance intention"
    ) ~ "repurchase intention",
    
    # Experience variants
    keyword %in% c(
      "customer experience",
      "user experience",
      "users' experiences",
      "user experiences",
      "online customer experience",
      "online experience"
    ) ~ "customer experience",
    
    # Service quality variants
    keyword %in% c(
      "service quality",
      "e-service quality",
      "quality of service",
      "electronic service quality",
      "online service quality"
    ) ~ "service quality",
    
    # Trust variants
    keyword %in% c(
      "trust",
      "e-trust",
      "online trust",
      "consumer trust"
    ) ~ "trust",
    
    # Engagement variants
    keyword %in% c(
      "customer engagement",
      "online customer engagement",
      "consumer engagement",
      "user engagement"
    ) ~ "customer engagement",
    
    TRUE ~ keyword
  )) %>%
  
  # Remove generic domain / scope terms and geographic artifacts
  filter(!keyword %in% c(
    "e-commerce",
    "electronic commerce",
    "e- commerces",
    "e commerce",
    "mobile commerce",
    "online shopping",
    "sales",
    "commerce",
    "internet",
    "websites",
    "indonesia"
  ))

# ---- Save tidy long-format output ----
write_csv(keywords_clean, processed_path)

message("Cleaned file saved to: ", processed_path)
message("Rows: ", nrow(keywords_clean),
        " | Unique keywords: ", n_distinct(keywords_clean$keyword),
        " | Unique documents: ", n_distinct(keywords_clean$doc_id))
