# KIQ 3.3 — App-store review analysis (R scripts)

Modular R pipeline for the KIQ 3.3 sentiment/perception analysis.
Also produces the cleaned corpus reused by KIQ 1.4.

## Folder layout (Data/KIQ_3_3/)

```
Data/KIQ_3_3/
├── raw/
│   └── kiq33_reviews_raw.csv          # input: Colab scrape output
├── scripts/
│   ├── _helpers.R                     # shared: libs, paths, colours, load_corpus()
│   ├── 00_merge.R                     # raw -> processed (dual-label corpus)
│   ├── 01_sentiment_distribution.R
│   ├── 02_average_rating.R
│   ├── 03_painpoints.R
│   ├── 04_temporal_trend.R
│   ├── 05_feature_mentions.R
│   └── run_all.R                      # runs 00..05 in order
├── processed/
│   ├── kiq33_reviews_merged.csv       # created by 00_merge.R
│   └── kiq14_metro_app_corpus.csv     # created by 00_merge.R (for KIQ 1.4)
└── outputs/
    ├── kiq33_sentiment_from_ratings.png
    ├── kiq33_avg_rating.png
    ├── kiq33_painpoints.png   (+ _summary.csv)
    ├── kiq33_temporal_trend.png  (+ .csv)
    └── kiq33_feature_mentions.png  (+ .csv)
```

## How to run

Set the working directory to `Data/KIQ_3_3/` (NOT `scripts/`), so the
`raw/`, `processed/`, `outputs/` paths resolve.

**RStudio:** open any script, then `setwd()` to `Data/KIQ_3_3/`
(or use an .Rproj at that level).

**Terminal:**
```bash
cd Data/KIQ_3_3
Rscript scripts/run_all.R          # whole pipeline
# or individually:
Rscript scripts/00_merge.R         # must run first (creates processed/)
Rscript scripts/01_sentiment_distribution.R
Rscript scripts/03_painpoints.R
# ...etc
```

## Dependencies

```r
install.packages(c("tidyverse", "lubridate", "scales"))
```

## Run order

`00_merge.R` **must run first** — it creates `processed/kiq33_reviews_merged.csv`
that scripts 01–05 read. Scripts 01–05 are independent of each other and can run
in any order once the merge exists.

## Method notes

- **Sentiment** is derived from star ratings (1–2 = negative, 3 = neutral,
  4–5 = positive) — the user's own encoded judgment, no model bias, fully
  reproducible across DE/FR/IT/EN/ES.
- **METRO app** = METRO Companion + METRO Shop merged (n=749). The `app_name`
  column preserves the per-app split for product-type analysis.
- **Threshold** n>=50 per entity. Choco (239), REKKI (115), METRO app (749)
  qualify; the three login-walled incumbents (Sysco/Brakes, Bidfood,
  Transgourmet) have no public EU app and are absent by design.
- **Pain-points / features** use multilingual keyword dictionaries (regex,
  case-insensitive) — transparent and reproducible.
