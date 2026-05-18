library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(stringr)

# Read Espacenet CSV with semicolon separator
patents <- read_delim("/Users/kodjoflaurent/SCI-shiny-intelligence-app/data/KIQ_1_2/raw/KIQ_1_2_patents_raw.csv",
                      delim = ";",
                      skip = 7,
                      show_col_types = FALSE
)

# Check column names
names(patents)

# 

patents %>%
  mutate(pub_raw = as.character(`Publication date`)) %>%
  distinct(pub_raw) %>%
  head(20)

#

patent_trend <- patents %>%
  mutate(
    pub_raw = as.character(`Publication date`),
    pub_first = str_extract(pub_raw, "^\\d{4}-\\d{2}-\\d{2}"),
    publication_date = ymd(pub_first),
    year = year(publication_date)
  ) %>%
  filter(!is.na(year)) %>%
  count(year, sort = FALSE)


# Plot trend
ggplot(patent_trend, aes(x = year, y = n)) +
  geom_col() +
  geom_text(aes(label = n), vjust = -0.3, size = 3.5) +
  labs(
    title = "Patent Publication Trend - KIQ 1.2",
    subtitle = "Digital ordering and e-commerce capability patents",
    x = "Publication Year",
    y = "Number of Patents"
  ) +
  theme_minimal(base_size = 12)






# Save table for Shiny
write_csv(
  patent_trend,
  "/Users/kodjoflaurent/SCI-shiny-intelligence-app/data/KIQs_1_2/processed/patent_trend_by_year.csv"
)