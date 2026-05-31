# NFL 4th Down Intelligence

Play-by-play analysis of NFL 4th down decision-making across 16 seasons, plus an interactive Shiny coaching dashboard.

## What's in here

| Path | Description |
|------|-------------|
| `analysis.R` | Master script — runs the full pipeline |
| `R/01_load_and_parse.R` | Ingests season CSVs, reconstructs play-by-play score |
| `R/02_feature_engineering.R` | Decision classification, win flag, score differential buckets |
| `R/03_eda.R` | 8 EDA plots: decisions, FG vs. go-for-it, win rates |
| `R/04_model.R` | Logistic regression: conversion probability + win probability |
| `R/05_coach_analysis.R` | Team aggressiveness, FG reliance, year-over-year stability |
| `dashboard/app.R` | Shiny coaching dashboard |
| `data/2010-2025_scores.csv` | Game outcomes used for win flag |

## Key findings

- The league-wide go-for-it rate has nearly doubled: **14.5% in 2010 → 27.1% in 2025**
- **4th & 1 conversion rate: 69.1%** across 4,216 attempts
- FG make rate is stable at **84–89%** from 30–50 yards, dropping sharply beyond 55
- A situation-controlled logistic model shows going for it has a **higher association with winning** than punting across most short-yardage situations in opponent territory
- Punt rate fell **below 50% for the first time** in 2025

## Setup

### Analysis

```r
# Install dependencies
install.packages(c(
  "tidyverse", "glue", "scales", "broom",
  "pROC", "ggridges", "patchwork",
  "janitor", "lubridate", "ggrepel"
))
```

Place season play CSVs (`2010_plays.csv` … `2025_plays.csv`) in `data/`. These are not included in the repo due to file size — download from [Kaggle](https://www.kaggle.com).

Then run:

```r
source("analysis.R")
```

Outputs write to `output/`.

### Dashboard

```r
install.packages(c("shiny","bslib","ggplot2","dplyr","tidyr","scales","DT","plotly","bsicons"))
shiny::runApp("dashboard/app.R")
```

No data files required — all historical data is embedded in the app.

## Data source

Kaggle NFL play-by-play dataset (2010–2025). Season CSV files are excluded from this repo per file size constraints.

## Dashboard preview

The coaching dashboard has four tabs:

- **Decision Assistant** — enter yards to go, field position, score differential, and quarter for an immediate Go / FG / Punt recommendation with historical probabilities
- **League Trends** — interactive charts of how the league has shifted since 2010
- **Team Intelligence** — 2024 team comparison table and scatter plots
- **Decision Guide** — conversion probability heatmap and printable reference



# NFL 4th Down Intelligence: 2010–2025

Play-by-play analysis of NFL 4th down decision-making across 16 seasons, plus an interactive Shiny coaching dashboard.

## What's in here

| Path | Description |
|------|-------------|
| `analysis.R` | Master script — runs the full pipeline |
| `R/01_load_and_parse.R` | Ingests season CSVs, reconstructs play-by-play score |
| `R/02_feature_engineering.R` | Decision classification, win flag, score differential buckets |
| `R/03_eda.R` | 8 EDA plots: decisions, FG vs. go-for-it, win rates |
| `R/04_model.R` | Logistic regression: conversion probability + win probability |
| `R/05_coach_analysis.R` | Team aggressiveness, FG reliance, year-over-year stability |
| `dashboard/app.R` | Shiny coaching dashboard |
| `data/2010-2025_scores.csv` | Game outcomes used for win flag |

## Key findings

- The league-wide go-for-it rate has nearly doubled: **14.5% in 2010 → 27.1% in 2025**
- **4th & 1 conversion rate: 69.1%** across 4,216 attempts
- FG make rate is stable at **84–89%** from 30–50 yards, dropping sharply beyond 55
- A situation-controlled logistic model shows going for it has a **higher association with winning** than punting across most short-yardage situations in opponent territory
- Punt rate fell **below 50% for the first time** in 2025

## Setup

### Analysis

```r
# Install dependencies
install.packages(c(
  "tidyverse", "glue", "scales", "broom",
  "pROC", "ggridges", "patchwork",
  "janitor", "lubridate", "ggrepel"
))
```

Place season play CSVs (`2010_plays.csv` … `2025_plays.csv`) in `data/`. These are not included in the repo due to file size — download from [Kaggle](https://www.kaggle.com).

Then run:

```r
source("analysis.R")
```

Outputs write to `output/`.

### Dashboard

```r
install.packages(c("shiny","bslib","ggplot2","dplyr","tidyr","scales","DT","plotly","bsicons"))
shiny::runApp("dashboard/app.R")
```

No data files required — all historical data is embedded in the app.

## Data source

Kaggle NFL play-by-play dataset (2010–2025). Season CSV files are excluded from this repo per file size constraints.

## Dashboard preview

The coaching dashboard has four tabs:

- **Decision Assistant** — enter yards to go, field position, score differential, and quarter for an immediate Go / FG / Punt recommendation with historical probabilities
- **League Trends** — interactive charts of how the league has shifted since 2010
- **Team Intelligence** — 2024 team comparison table and scatter plots
- **Decision Guide** — conversion probability heatmap and printable reference



# NFL 4th Down Efficiency Analysis (v2)

## Overview

Play-by-play analysis of NFL 4th down decisions across 16 seasons (2010–2025),
with focus on field goal vs. go-for-it tradeoffs and their association with
winning, controlled for score differential.

## Project Structure

```
nfl_4th_down/
├── nfl_4th_down.Rproj
├── README.md
├── analysis.R                    # Run everything in sequence
├── data/
│   ├── 2010_plays.csv            # One per season (required)
│   ├── ...
│   └── 2010-2025_scores.csv      # Game outcomes (required for win_flag)
├── R/
│   ├── 00_packages.R
│   ├── 01_load_and_parse.R       # Ingest + score reconstruction + 4th down filter
│   ├── 02_feature_engineering.R  # Decision classification, win_flag, score_diff_bucket
│   ├── 03_eda.R                  # 8 plots covering decisions, FG vs. go, win rates
│   ├── 04_model.R                # Two logistic models (conversion + win probability)
│   └── 05_coach_analysis.R       # Team aggressiveness, FG reliance, win rate scatter
└── output/                       # Generated plots + CSVs
```

## Setup

1. Open `nfl_4th_down.Rproj` in RStudio.
2. Run `R/00_packages.R` to install dependencies.
3. Place season CSVs and `2010-2025_scores.csv` in `data/`.
4. Source `analysis.R` or run scripts 00–05 in order.

## Score Differential

Score at the time of each 4th down play is reconstructed from `PlayOutcome`:

| Outcome              | Points | Notes                          |
|----------------------|--------|--------------------------------|
| Touchdown            | 6      | Possessing team                |
| Extra Point          | 1      | Possessing team                |
| 2 Point Conversion   | 2      | Possessing team                |
| Field Goal           | 3      | Possessing team                |
| Safety               | 2      | Opposing team                  |

`IsScoringPlay` is NOT used for scoring (it misses 2-pt conversions and safeties).
Score differential = possessing team's cumulative score minus opponent's, just
before the snap.

## Outputs

### EDA plots (03_eda.R)
| File | Description |
|------|-------------|
| 01_decision_rates_by_season | Go / Punt / FG rate trends 2010–2025 |
| 02_fg_vs_conversion_by_distance | FG make rate vs. go-for-it conversion rate |
| 03_win_rate_by_decision | Raw win rate by decision type |
| 04_win_rate_fg_vs_go_by_distance | Win rate: FG vs. go by distance |
| 05_decision_by_score_diff | Decision mix across score situations |
| 06_win_rate_by_diff_and_decision | Win rate by decision x score differential |
| 07_go_rate_heatmap | Go-for-it rate: distance × field zone |
| 08_redzone_fg_vs_go_win_rate | Red zone FG vs. go win rate by distance |

### Model plots (04_model.R)
| File | Description |
|------|-------------|
| 09_conversion_model_coefs | Conversion model odds ratios (incl. score_diff) |
| 10_conversion_roc | ROC curve |
| 11_conversion_prob_by_diff | Predicted conversion prob across score situations |
| 12_win_model_coefs | Win model odds ratios — FG and go vs. punt baseline |
| 13_win_prob_by_decision_and_diff | Predicted win prob by decision across score range |

### Team analysis plots (05_coach_analysis.R)
| File | Description |
|------|-------------|
| 14_team_aggressiveness_all_time | Top/bottom 10 franchises by aggr. index |
| 15_fg_reliance_vs_win_rate | FG rate vs. win rate scatter by franchise |
| 16_go_rate_vs_win_rate | Go-for-it rate vs. win rate scatter |
| 17_close_game_aggressiveness | Close-game go-rate trends over time |
| 18_go_rate_stability | Year-over-year go-rate correlation |

## Interpretation Notes

Win rates in raw plots (03, 04, 05) reflect game context — teams going for it
on 4th down are often losing and need points, which artificially lowers their
raw win rate. The Model B win probability (04_model.R) controls for score
differential, field position, distance, and quarter, giving a fairer read of
each decision's association with winning.

##	Author
Zach Laher
B.S. Data Science, Merrimack College, 2025
linkedin.com/in/zachlaher · zachlaher

