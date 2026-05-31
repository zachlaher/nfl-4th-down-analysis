# =============================================================================
# 01_load_and_parse.R
# Ingest all season CSVs, combine, parse fields, reconstruct running score,
# and compute score differential at the time of each 4th down play.
#
# Score reconstruction approach:
#   - Drive scoring off PlayOutcome directly (not IsScoringPlay — that flag
#     misses 2-point conversions and safeties).
#   - Game key: Season + Week + AwayTeam + HomeTeam (all abbreviations).
#   - Ordering: plays are already in game sequence within each CSV.
#   - Point values: TD=6, XP=1, 2PT=2, FG=3, Safety=2 (to opponent).
#
# Outputs:
#   plays_raw       - combined raw data (all seasons)
#   fourth_down     - 4th down plays with parsed columns + score_diff
# =============================================================================

source("R/00_packages.R")

DATA_DIR <- "data"

# -----------------------------------------------------------------------------
# 1. Ingest
# -----------------------------------------------------------------------------

csv_files <- list.files(DATA_DIR, pattern = "^\\d{4}_plays\\.csv$", full.names = TRUE)

if (length(csv_files) == 0) {
  stop("[01] No season CSVs found in '", DATA_DIR, "/'.")
}

message("[01] Loading ", length(csv_files), " season files...")

plays_raw <- map_dfr(
  csv_files,
  \(f) {
    tryCatch(
      {
        df <- read_csv(f,
          col_types = cols(
            Season             = col_integer(),
            Week               = col_character(),
            GameSlot           = col_character(),
            Date               = col_character(),
            AwayTeam           = col_character(),
            HomeTeam           = col_character(),
            Quarter            = col_character(),
            DriveNumber        = col_integer(),
            TeamWithPossession = col_character(),
            IsScoringDrive     = col_integer(),
            PlayNumberInDrive  = col_integer(),
            IsScoringPlay      = col_integer(),
            PlayOutcome        = col_character(),
            PlayStart          = col_character(),
            PlayTimeFormation  = col_character(),
            PlayDescription    = col_character()
          ),
          show_col_types = FALSE
        )
        message("  Loaded: ", basename(f), " (", nrow(df), " rows)")
        df
      },
      error = function(e) {
        warning("[01] Failed to load ", f, ": ", conditionMessage(e))
        NULL
      }
    )
  }
) |>
  janitor::clean_names()

message("[01] Combined dataset: ", nrow(plays_raw), " rows")

# -----------------------------------------------------------------------------
# 2. Build team full-name -> abbreviation lookup
#    Derived from data: AwayTeam/HomeTeam are abbrevs; TeamWithPossession is full.
# -----------------------------------------------------------------------------

team_abbr_lookup <- plays_raw |>
  filter(!is.na(team_with_possession), !is.na(away_team)) |>
  pivot_longer(c(away_team, home_team), names_to = "side", values_to = "team_abbr") |>
  count(team_with_possession, team_abbr, sort = TRUE) |>
  group_by(team_with_possession) |>
  slice_max(n, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(team_with_possession, team_abbr)

message("[01] Team lookup entries: ", nrow(team_abbr_lookup))

# -----------------------------------------------------------------------------
# 3. Reconstruct running score per game
#    A "game" is uniquely identified by season + week + away_team + home_team.
# -----------------------------------------------------------------------------

# Define which PlayOutcome values score points and how many
score_points <- function(play_outcome) {
  # Returns list(team = "possessing"|"opponent", points = N)
  # "opponent" only for Safety
  case_when(
    play_outcome == "Touchdown"          ~ 6L,
    play_outcome == "Extra Point"        ~ 1L,
    play_outcome == "2 Point Conversion" ~ 2L,
    play_outcome == "Field Goal"         ~ 3L,
    play_outcome == "Safety"             ~ 2L,    # 2 pts to opposing team
    TRUE                                 ~ 0L
  )
}

plays_scored <- plays_raw |>
  left_join(team_abbr_lookup, by = "team_with_possession") |>
  mutate(
    game_key    = paste(season, week, away_team, home_team, sep = "|"),
    pts_scored  = score_points(play_outcome),
    # For safety, points go to the OPPONENT of the possessing team
    is_safety   = play_outcome == "Safety",
    pts_away    = case_when(
      is_safety & team_abbr == home_team ~ pts_scored,   # home team forced safety -> away gets pts
      !is_safety & team_abbr == away_team ~ pts_scored,
      TRUE ~ 0L
    ),
    pts_home    = case_when(
      is_safety & team_abbr == away_team ~ pts_scored,   # away team forced safety -> home gets pts
      !is_safety & team_abbr == home_team ~ pts_scored,
      TRUE ~ 0L
    )
  ) |>
  group_by(game_key) |>
  mutate(
    # Cumulative score BEFORE this play (lag so the play sees pre-snap state)
    cum_away_score = lag(cumsum(pts_away), default = 0L),
    cum_home_score = lag(cumsum(pts_home), default = 0L),
    # Score differential from possessing team's perspective: positive = leading
    score_diff = if_else(
      team_abbr == away_team,
      cum_away_score - cum_home_score,
      cum_home_score - cum_away_score
    )
  ) |>
  ungroup()

message("[01] Score differential computed")

# -----------------------------------------------------------------------------
# 4. Filter to 4th down and parse PlayStart
#    Format: "4th & <yards_to_go> at <territory_abbr> <yardline>"
# -----------------------------------------------------------------------------

fourth_down <- plays_scored |>
  filter(str_starts(play_start, "4th")) |>
  mutate(
    yards_to_go    = as.integer(str_extract(play_start, "(?<=4th & )\\d+")),
    territory_abbr = str_extract(play_start, "(?<=at )([A-Z]+)(?= \\d)"),
    yardline_raw   = as.integer(str_extract(play_start, "\\d+$")),
    in_own_territory = !is.na(team_abbr) & !is.na(territory_abbr) &
                       (territory_abbr == team_abbr),
    abs_yardline   = if_else(in_own_territory, yardline_raw, 100L - yardline_raw),
    field_zone     = case_when(
      abs_yardline <= 25 ~ "Own 1-25 (deep)",
      abs_yardline <= 40 ~ "Own 26-40",
      abs_yardline <= 49 ~ "Own 41-49 (mid)",
      abs_yardline <= 60 ~ "Opp 40-49 (mid)",
      abs_yardline <= 75 ~ "Opp 25-39",
      TRUE               ~ "Opp 1-24 (red zone)"
    ),
    field_zone = factor(field_zone, levels = c(
      "Own 1-25 (deep)", "Own 26-40", "Own 41-49 (mid)",
      "Opp 40-49 (mid)", "Opp 25-39", "Opp 1-24 (red zone)"
    ))
  )

message("[01] 4th down plays: ", nrow(fourth_down))
message("[01] Score diff range: ", round(min(fourth_down$score_diff, na.rm=TRUE)),
        " to ", round(max(fourth_down$score_diff, na.rm=TRUE)))
message("[01] Done.\n")
