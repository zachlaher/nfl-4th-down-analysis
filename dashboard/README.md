# NFL 4th Down Intelligence Dashboard

A Shiny dashboard for coaching staff. All data is pre-aggregated from 64,000+
4th down plays across 16 NFL seasons (2010–2025). No raw data files required to run.

## Setup

```r
# Install dependencies
install.packages(c(
  "shiny",
  "bslib",
  "ggplot2",
  "dplyr",
  "tidyr",
  "scales",
  "DT",
  "plotly",
  "bsicons"
))

# Run
shiny::runApp("app.R")
```

## Tabs

### Decision Assistant
Enter the current game situation (yards to go, field position, score
differential, quarter) and get an immediate recommendation with:
- Go / FG / Punt verdict with contextual note
- Historical conversion probability at that distance and score margin
- Historical FG make rate at that field position
- League-average decision mix at that distance

### League Trends
- Go-for-it, FG, and punt rates 2010–2025 (interactive)
- Conversion rate and FG make rate trends over 16 seasons
- Conversion probability by distance for both decisions
- Conversion rate by field zone

### Team Intelligence
- 2024 team scatter: go-for-it rate vs. conversion rate
- Full sortable table of all 32 teams (go rate, FG rate, punt rate,
  conversion %, FG make %)
- Stacked bar: decision mix for all 32 teams

### Decision Guide
- Conversion probability heatmap (yards to go × score differential)
- Historical decision mix by distance
- Key statistical reference cards

## Notes

The recommendation logic uses a simplified expected value model derived from
the logistic regression in the full analysis project (nfl_4th_down_project_v2).
It does not account for opponent quality, weather, timeout availability, or
remaining time. Use as one input among several.

FG range threshold is ~57 yards. Beyond that, the dashboard defaults to
evaluating Go vs. Punt only.
