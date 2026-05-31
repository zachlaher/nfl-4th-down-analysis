# =============================================================================
# 02_feature_engineering.R
# Derive analytical columns for EDA, win-rate analysis, and modeling.
#
# Key additions vs. v1:
#   - score_diff_bucket: situational buckets (blowout down, close, blowout up)
#   - win_flag: did the possessing team WIN the game?
#   - decision classification refined for FG vs. Go analysis
#
# Depends on: fourth_down (from 01_load_and_parse.R)
# Outputs:    fourth_down_fe, game_outcomes
# =============================================================================

if (!exists("fourth_down")) source("R/01_load_and_parse.R")

message("[02] Engineering features...")

# -----------------------------------------------------------------------------
# 1. Game outcomes from scores file
#    Used to compute win_flag for each 4th down play
# -----------------------------------------------------------------------------

scores_path <- "data/2010-2025_scores.csv"

if (!file.exists(scores_path)) {
  # Fall back: scores file might be in data/ directly
  scores_path <- list.files("data", pattern = "scores\\.csv$", full.names = TRUE)[1]
}

if (!is.na(scores_path) && file.exists(scores_path)) {
  game_outcomes_raw <- read_csv(scores_path, show_col_types = FALSE) |>
    janitor::clean_names() |>
    filter(!is.na(away_score), !is.na(home_score)) |>
    mutate(
      game_key    = paste(season, week, away_team, home_team, sep = "|"),
      away_win    = away_score > home_score,
      home_win    = home_score > away_score,
      is_tie      = away_score == home_score
    )
  message("[02] Game outcomes loaded: ", nrow(game_outcomes_raw), " games")
} else {
  warning("[02] Scores file not found — win_flag will be NA")
  game_outcomes_raw <- tibble(game_key = character(), away_win = logical(),
                              home_win = logical(), is_tie = logical())
}

# -----------------------------------------------------------------------------
# 2. Feature engineering
# -----------------------------------------------------------------------------

fourth_down_fe <- fourth_down |>
  # Join win outcomes
  left_join(
    game_outcomes_raw |> select(game_key, away_win, home_win, is_tie),
    by = "game_key"
  ) |>
  mutate(
    # Did the possessing team win?
    win_flag = case_when(
      team_abbr == away_team & away_win ~ TRUE,
      team_abbr == home_team & home_win ~ TRUE,
      is_tie                            ~ NA,
      TRUE                              ~ FALSE
    ),

    # Decision classification
    decision = case_when(
      str_detect(play_outcome, regex("^Punt",        ignore_case = TRUE)) ~ "Punt",
      str_detect(play_outcome, regex("Field Goal",   ignore_case = TRUE)) ~ "Field Goal",
      str_detect(play_outcome, regex("^Timeout|^Penalty|^Spike|^Kneel",
                                     ignore_case = TRUE))                 ~ "Special/Admin",
      TRUE                                                                  ~ "Go For It"
    ),

    # FG outcome
    fg_made = case_when(
      decision == "Field Goal" & !str_detect(play_outcome, "No Good") ~ TRUE,
      decision == "Field Goal"                                         ~ FALSE,
      TRUE                                                             ~ NA
    ),

    # Conversion: did the offense gain the yards needed?
    converted = case_when(
      decision != "Go For It" ~ NA,
      str_detect(play_outcome,
        regex("Turnover|Incomplete|Interception|Fumble|No Good|Sack|Turnover on Downs",
              ignore_case = TRUE))           ~ FALSE,
      str_detect(play_outcome,
        regex("\\d+ Yard|Touchdown",
              ignore_case = TRUE))           ~ TRUE,
      play_outcome == "Turnover on Downs"    ~ FALSE,
      TRUE                                   ~ NA
    ),

    # Distance buckets
    distance_bucket = case_when(
      yards_to_go == 1  ~ "4th & 1",
      yards_to_go == 2  ~ "4th & 2",
      yards_to_go == 3  ~ "4th & 3",
      yards_to_go <= 5  ~ "4th & 4-5",
      yards_to_go <= 10 ~ "4th & 6-10",
      TRUE              ~ "4th & 11+"
    ),
    distance_bucket = factor(distance_bucket, levels = c(
      "4th & 1","4th & 2","4th & 3","4th & 4-5","4th & 6-10","4th & 11+"
    )),

    # Score differential buckets (possessing team's perspective)
    score_diff_bucket = case_when(
      score_diff <= -14          ~ "Down 14+",
      score_diff <= -7           ~ "Down 7-13",
      score_diff <= -1           ~ "Down 1-6",
      score_diff == 0            ~ "Tied",
      score_diff <= 6            ~ "Up 1-6",
      score_diff <= 13           ~ "Up 7-13",
      TRUE                       ~ "Up 14+"
    ),
    score_diff_bucket = factor(score_diff_bucket, levels = c(
      "Down 14+","Down 7-13","Down 1-6","Tied","Up 1-6","Up 7-13","Up 14+"
    )),

    # Late-game close situation (Q4 or OT, within one score)
    late_close = str_detect(quarter, regex("4th|OT|Over", ignore_case = TRUE)) &
                 abs(score_diff) <= 8,

    # Quarter clean
    quarter_clean = case_when(
      quarter %in% c("1","Q1","1st","1st Quarter") ~ "Q1",
      quarter %in% c("2","Q2","2nd","2nd Quarter") ~ "Q2",
      quarter %in% c("3","Q3","3rd","3rd Quarter") ~ "Q3",
      quarter %in% c("4","Q4","4th","4th Quarter") ~ "Q4",
      str_detect(quarter, regex("OT|Over", ignore_case = TRUE)) ~ "OT",
      TRUE ~ quarter
    ),

    # Game type
    is_playoff = str_detect(week,
      regex("WILD|DIVISIONAL|CONFERENCE|SUPER BOWL|CHAMPIONSHIP", ignore_case = TRUE)),
    game_type = if_else(is_playoff, "Playoffs", "Regular Season"),

    # Era
    era = case_when(
      season <= 2014 ~ "2010-2014",
      season <= 2019 ~ "2015-2019",
      season <= 2022 ~ "2020-2022",
      TRUE           ~ "2023-2025"
    ),
    era = factor(era, levels = c("2010-2014","2015-2019","2020-2022","2023-2025")),

    season_c = season - 2010L
  )

# -----------------------------------------------------------------------------
# 3. Validation
# -----------------------------------------------------------------------------

total    <- nrow(fourth_down_fe)
go_n     <- sum(fourth_down_fe$decision == "Go For It",  na.rm = TRUE)
punt_n   <- sum(fourth_down_fe$decision == "Punt",        na.rm = TRUE)
fg_n     <- sum(fourth_down_fe$decision == "Field Goal",  na.rm = TRUE)
win_n    <- sum(!is.na(fourth_down_fe$win_flag))

message(glue::glue(
  "[02] Total 4th downs : {total}
  [02]   Go For It      : {go_n}
  [02]   Punt           : {punt_n}
  [02]   Field Goal     : {fg_n}
  [02]   Win flag avail : {win_n}"
))

message("[02] Done.\n")
