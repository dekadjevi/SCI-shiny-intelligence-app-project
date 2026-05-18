
install.packages(c(
  "readr", "dplyr", "tidyr", "stringr",
  "tidytext", "ggplot2", "wordcloud", "RColorBrewer"
))


library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

#setwd("/Users/kodjoflaurent/SCI-shiny-intelligence-app/data/KIQs_1_2/raw")

# Load patent dataset
#patents <- read_csv("/Users/kodjoflaurent/SCI-shiny-intelligence-app/data/KIQ_1_2/raw/KIQ_1_2_patents_raw.csv", show_col_types = FALSE)

# Read Espacenet CSV with semicolon separator
patents <- read_delim("/Users/kodjoflaurent/SCI-shiny-intelligence-app/data/KIQ_1_2/raw/KIQ_1_2_patents_raw.csv",
  delim = ";",
  skip = 7,
  show_col_types = FALSE
)

# Check column names
names(patents)


# IPC occurrence analysis
ipc_tokens <- patents %>%
  select(IPC) %>%
  filter(!is.na(IPC)) %>%
  separate_rows(IPC, sep = "\\n|;|,") %>%
  mutate(IPC = str_trim(IPC)) %>%
  filter(IPC != "")

ipc_occurrence <- ipc_tokens %>%
  count(IPC, sort = TRUE)

# View top IPC classes
head(ipc_occurrence, 20)

# Plot top 15 IPC classes
top15_ipc <- ipc_occurrence %>%
  slice_max(n, n = 15) %>%
  arrange(n)

ggplot(top15_ipc, aes(x = n, y = reorder(IPC, n))) +
  geom_col() +
  geom_text(aes(label = n), hjust = -0.2, size = 3.5) +
  labs(
    title = "Top 15 IPC Occurrences - KIQ 1.2 Patent Corpus",
    x = "Frequency",
    y = "IPC Class"
  ) +
  xlim(0, max(top15_ipc$n) + 2) +
  theme_minimal(base_size = 12)






# Save result for Shiny
#write_csv(
#  ipc_occurrence,
#  "/Users/kodjoflaurent/SCI-shiny-intelligence-app/data/KIQ_1_2/processed/ipc_occurrence.csv"
#)
