# =============================================================
# KIQ 2.1 — Cleaning script (Espacenet patent data)
# Input : raw Espacenet CSV (My Patents export)
# Output: 3 tidy datasets in processed/
#   - KIQ_2_1_patents.csv       : one row per patent (wide format)
#   - KIQ_2_1_ipc_long.csv      : one row per (patent, IPC code)
#   - KIQ_2_1_title_tokens.csv  : one row per (patent, title token)
# =============================================================

install.packages(c(
  "readr", "dplyr", "tidyr", "stringr",
  "tidytext", "lubridate"
))

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(tidytext)
library(lubridate)

# ---- Paths ----
raw_path <- "/Users/flaurent/Desktop/SCI-shiny-intelligence-app-project/data/KIQ_2_1/raw/My_patents_KIQ_2_1.csv"

out_dir       <- "/Users/flaurent/Desktop/SCI-shiny-intelligence-app-project/data/KIQ_2_1/processed"
patents_path  <- file.path(out_dir, "KIQ_2_1_patents.csv")
ipc_path      <- file.path(out_dir, "KIQ_2_1_ipc_long.csv")
tokens_path   <- file.path(out_dir, "KIQ_2_1_title_tokens.csv")

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ---- Load raw data ----
# Espacenet CSV: 7 metadata rows, then header, semicolon-delimited.
# Multi-value cells (IPC, CPC, publication number, etc.) use \r\n internally.
df_raw <- read_delim(
  raw_path,
  delim          = ";",
  skip           = 7,
  show_col_types = FALSE,
  trim_ws        = TRUE,
  na             = c("", "NA")
)

message("Raw rows loaded: ", nrow(df_raw))

df_raw

# ---- Wide patent table (one row per patent) ----
patents <- df_raw %>%
  mutate(doc_id = row_number()) %>%
  transmute(
    doc_id,
    publication_number = `Publication number`,
    title              = Title,
    inventors          = Inventors,
    applicants         = Applicants,
    earliest_priority  = `Earliest priority`,
    ipc_raw            = IPC,
    cpc_raw            = CPC,
    publication_date   = `Publication date`,
    earliest_pub       = `Earliest publication`,
    family_number      = `Family number`
  ) %>%
  mutate(
    pub_date_first = str_extract(publication_date, "\\d{4}-\\d{2}-\\d{2}"),
    pub_date_first = ymd(pub_date_first),
    pub_year       = year(pub_date_first)
  )

# ---- Long IPC table (one row per IPC code per patent) ----
ipc_long <- patents %>%
  select(doc_id, ipc_raw) %>%
  filter(!is.na(ipc_raw)) %>%
  separate_rows(ipc_raw, sep = "\r?\n|\\|") %>%
  mutate(ipc_code = str_trim(ipc_raw)) %>%
  filter(ipc_code != "") %>%
  mutate(
    ipc_main  = str_extract(ipc_code, "^[A-Z]\\d{2}[A-Z]"),  # e.g. G06Q
    ipc_class = str_extract(ipc_code, "^[A-Z]\\d{2}")        # e.g. G06
  ) %>%
  select(doc_id, ipc_code, ipc_main, ipc_class)

# ---- Title tokens (one row per word per patent) ----
patent_stopwords <- c(
  "method", "methods", "system", "systems", "apparatus", "device", "devices",
  "based", "using", "thereof", "comprising", "providing", "provide",
  "data", "information"
)

title_tokens <- patents %>%
  select(doc_id, title) %>%
  filter(!is.na(title)) %>%
  unnest_tokens(word, title) %>%
  filter(!word %in% stop_words$word) %>%
  filter(!word %in% patent_stopwords) %>%
  filter(str_detect(word, "^[a-z]+$")) %>%
  filter(nchar(word) >= 3)

# ---- Applicant cleaning ----
applicant_clean <- function(x) {
  x <- str_replace_all(x, "\\s*\\[[A-Z]+\\]", "")  # strip country codes
  x <- str_replace_all(x, "\r?\n", " | ")          # multi-value separator
  x <- str_trim(x)
  x <- str_to_upper(x)
  x <- str_replace_all(x, ".*BAIDU.*",  "BAIDU")
  x <- str_replace_all(x, ".*TENCENT.*", "TENCENT")
  x
}

patents <- patents %>%
  mutate(applicants_clean = applicant_clean(applicants))

# ---- Save outputs ----
write_csv(patents,      patents_path)
write_csv(ipc_long,     ipc_path)
write_csv(title_tokens, tokens_path)

message("=== Cleaning complete ===")
message("Patents:      ", nrow(patents),      " rows -> ", patents_path)
message("IPC long:     ", nrow(ipc_long),     " rows -> ", ipc_path)
message("Title tokens: ", nrow(title_tokens), " rows -> ", tokens_path)
message("Year range:   ", min(patents$pub_year, na.rm = TRUE), " - ",
        max(patents$pub_year, na.rm = TRUE))
