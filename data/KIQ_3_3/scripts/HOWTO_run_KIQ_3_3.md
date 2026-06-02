# KIQ 3.3 — How to run

Analysis of app-store reviews (sentiment, pain-points, temporal trend, feature
mentions) for METRO app vs Choco vs REKKI. Also produces the cleaned corpus
reused by KIQ 1.4.

## Folder layout

```
KIQ_3_3/
├── raw/        kiq33_reviews_raw.csv          # input (Colab scrape)
├── scripts/    _helpers.R, 00_merge.R … 05_*  # the code
├── processed/  merged corpus + KIQ 1.4 corpus # created by 00_merge.R
└── outputs/    charts (.png) + summary CSVs   # created by 01–05
```

## First time only — install R packages

```r
install.packages(c("tidyverse", "lubridate", "scales"))
```

---

## ▶ Recommended: run in RStudio

1. Open the project: **File → Open Project** (or **New Project → Existing Directory**)
   and select the `KIQ_3_3` folder. This pins the working directory to `KIQ_3_3/`.
2. In the Console:

   ```r
   source("scripts/run_all.R")        # whole pipeline
   ```

   Or run a single analysis:

   ```r
   source("scripts/00_merge.R")       # MUST run first (creates processed/)
   source("scripts/01_sentiment_distribution.R")
   source("scripts/03_painpoints.R")
   # …01–05 are independent once 00_merge has run
   ```

3. Check the working directory anytime — it must end in `/KIQ_3_3`:

   ```r
   getwd()
   ```

> ⚠️ Don't use RStudio's **Source** button on a script opened from `scripts/` —
> it can switch the working directory to `scripts/` and break the
> `raw/ processed/ outputs/` paths. Use `source("scripts/…")` from the Console instead.

---

## ▶ Alternative: run from Terminal

```bash
cd "/Users/flaurent/Desktop/SCI-shiny-intelligence-app-project/data/KIQ_3_3"
Rscript scripts/run_all.R          # whole pipeline
# or individually:
Rscript scripts/00_merge.R         # must be first
Rscript scripts/03_painpoints.R
```

---

## Run order

`00_merge.R` **must run first** — it builds `processed/kiq33_reviews_merged.csv`
that scripts 01–05 read, plus `processed/kiq14_metro_app_corpus.csv` for KIQ 1.4.
Scripts 01–05 are independent of each other and can run in any order afterwards.

## What each script makes

| Script | Output |
|--------|--------|
| `00_merge.R` | `processed/kiq33_reviews_merged.csv`, `processed/kiq14_metro_app_corpus.csv` |
| `01_sentiment_distribution.R` | `outputs/kiq33_sentiment_from_ratings.png` |
| `02_average_rating.R` | `outputs/kiq33_avg_rating.png` |
| `03_painpoints.R` | `outputs/kiq33_painpoints.png` + summary CSV |
| `04_temporal_trend.R` | `outputs/kiq33_temporal_trend.png` + CSV |
| `05_feature_mentions.R` | `outputs/kiq33_feature_mentions.png` + CSV |

## Method notes

- **Sentiment** from star ratings (1–2 = negative, 3 = neutral, 4–5 = positive) —
  reproducible across DE/FR/IT/EN/ES, no model bias.
- **METRO app** = METRO Companion + METRO Shop merged (n=749); `app_name` column
  keeps the per-app split.
- **Threshold** n ≥ 50 per entity → Choco, REKKI, METRO app qualify.
- **Pain-points / features** via multilingual keyword dictionaries (regex).
