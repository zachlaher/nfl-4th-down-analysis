# =============================================================================
# 05_coach_analysis.R
# Per-team aggressiveness analysis with score-differential context.
#
# New vs. v1:
#   - Aggressiveness index broken out by score situation (close games only)
#   - Win rate by team x decision type (controlled for era/situation)
#   - FG reliance index: does heavy FG use correlate with winning?
# =============================================================================

if (!exists("fourth_down_fe")) source("R/02_feature_engineering.R")

OUTPUT_DIR <- "output"
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR)

theme_nfl <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.title    = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(color = "grey40", size = 10),
      panel.grid.minor = element_blank(),
      legend.position  = "bottom"
    )
}

save_plot <- function(p, name, w = 11, h = 7) {
  path <- file.path(OUTPUT_DIR, paste0(name, ".png"))
  ggsave(path, p, width = w, height = h, dpi = 150, bg = "white")
  message("  Saved: ", path)
  invisible(p)
}

message("[05] Running team aggressiveness analysis...")

# ─────────────────────────────────────────────────────────────────────────────
# 1. Team-season base stats
# ─────────────────────────────────────────────────────────────────────────────

team_season <- fourth_down_fe |>
  filter(
    decision %in% c("Go For It","Punt","Field Goal"),
    game_type == "Regular Season",
    !is.na(team_with_possession)
  ) |>
  group_by(team_with_possession, season) |>
  summarise(
    total_4th      = n(),
    go_n           = sum(decision == "Go For It"),
    fg_n           = sum(decision == "Field Goal"),
    punt_n         = sum(decision == "Punt"),
    go_rate        = go_n / total_4th,
    fg_rate        = fg_n / total_4th,
    # Subset: close games only (within 8 pts, Q3-Q4)
    close_total    = sum(abs(score_diff) <= 8 &
                         str_detect(quarter_clean, "Q3|Q4"), na.rm = TRUE),
    close_go_n     = sum(decision == "Go For It" &
                         abs(score_diff) <= 8 &
                         str_detect(quarter_clean, "Q3|Q4"), na.rm = TRUE),
    close_go_rate  = if_else(close_total >= 5, close_go_n / close_total, NA_real_),
    # Win rate (games in which this team made at least one 4th down decision)
    win_rate       = mean(win_flag, na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(total_4th >= 20)

# League averages per season
league_avg <- team_season |>
  group_by(season) |>
  summarise(
    league_go_rate = weighted.mean(go_rate, total_4th),
    league_fg_rate = weighted.mean(fg_rate, total_4th),
    .groups = "drop"
  )

team_season <- team_season |>
  left_join(league_avg, by = "season") |>
  mutate(
    agg_index = go_rate   - league_go_rate,
    fg_index  = fg_rate   - league_fg_rate
  )

# ─────────────────────────────────────────────────────────────────────────────
# 2. All-time aggressiveness + FG reliance rankings
# ─────────────────────────────────────────────────────────────────────────────

all_time <- team_season |>
  group_by(team_with_possession) |>
  summarise(
    seasons       = n(),
    avg_go_rate   = weighted.mean(go_rate,  total_4th),
    avg_fg_rate   = weighted.mean(fg_rate,  total_4th),
    avg_agg_index = weighted.mean(agg_index, total_4th),
    avg_fg_index  = weighted.mean(fg_index,  total_4th),
    avg_win_rate  = weighted.mean(win_rate,  total_4th, na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(seasons >= 3) |>
  arrange(desc(avg_agg_index))

# Plot 14: All-time aggressiveness (top 10 / bottom 10)
top10    <- head(all_time, 10) |> mutate(grp = "Most Aggressive")
bottom10 <- tail(all_time, 10) |> mutate(grp = "Least Aggressive")

p14 <- bind_rows(top10, bottom10) |>
  ggplot(aes(avg_agg_index, reorder(team_with_possession, avg_agg_index), fill = grp)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  geom_col(width = 0.7, alpha = 0.85) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 0.1),
                     expand = expansion(mult = c(0.15, 0.15))) +
  scale_fill_manual(values = c("Most Aggressive"="#c0392b","Least Aggressive"="#2980b9")) +
  labs(title = "All-Time 4th Down Aggressiveness by Team",
       subtitle = "Go-for-it rate minus league average that season (2010–2025, min. 3 seasons)",
       x = "Aggressiveness index", y = NULL, fill = NULL,
       caption = "Source: Kaggle NFL play-by-play") +
  theme_nfl() + theme(legend.position = "top")

save_plot(p14, "14_team_aggressiveness_all_time", h = 8)

# ─────────────────────────────────────────────────────────────────────────────
# 3. FG reliance vs. win rate scatter
# ─────────────────────────────────────────────────────────────────────────────

p15 <- all_time |>
  filter(seasons >= 6) |>
  ggplot(aes(avg_fg_rate, avg_win_rate)) +
  geom_point(aes(size = seasons), alpha = 0.7, color = "#27ae60") +
  geom_smooth(method = "lm", se = TRUE, color = "#c0392b", linewidth = 1) +
  ggrepel::geom_text_repel(
    aes(label = str_extract(team_with_possession, "\\S+$")),
    size = 3, color = "grey30", max.overlaps = 15
  ) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_size_continuous(range = c(3, 7), guide = "none") +
  labs(title = "Field Goal Reliance vs. Win Rate by Team",
       subtitle = "Each point = franchise average over seasons present (min. 6 seasons)",
       x = "Avg. FG attempt rate on 4th down", y = "Avg. win rate",
       caption = "Source: Kaggle NFL play-by-play | Association, not causation") +
  theme_nfl()

save_plot(p15, "15_fg_reliance_vs_win_rate")

# ─────────────────────────────────────────────────────────────────────────────
# 4. Go-for-it rate vs. win rate scatter
# ─────────────────────────────────────────────────────────────────────────────

p16 <- all_time |>
  filter(seasons >= 6) |>
  ggplot(aes(avg_go_rate, avg_win_rate)) +
  geom_point(aes(size = seasons), alpha = 0.7, color = "#1a6faf") +
  geom_smooth(method = "lm", se = TRUE, color = "#c0392b", linewidth = 1) +
  ggrepel::geom_text_repel(
    aes(label = str_extract(team_with_possession, "\\S+$")),
    size = 3, color = "grey30", max.overlaps = 15
  ) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_size_continuous(range = c(3, 7), guide = "none") +
  labs(title = "Go-For-It Rate vs. Win Rate by Team",
       subtitle = "Each point = franchise average over seasons present (min. 6 seasons)",
       x = "Avg. go-for-it rate on 4th down", y = "Avg. win rate",
       caption = "Source: Kaggle NFL play-by-play | Association, not causation") +
  theme_nfl()

save_plot(p16, "16_go_rate_vs_win_rate")

# ─────────────────────────────────────────────────────────────────────────────
# 5. Close-game aggressiveness over time (top 5 / bottom 5 by avg close go-rate)
# ─────────────────────────────────────────────────────────────────────────────

close_game_teams <- team_season |>
  filter(!is.na(close_go_rate)) |>
  group_by(team_with_possession) |>
  summarise(
    n_seasons       = n(),
    avg_close_go    = mean(close_go_rate, na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(n_seasons >= 8) |>
  arrange(desc(avg_close_go))

top5_close    <- head(close_game_teams, 5)$team_with_possession
bottom5_close <- tail(close_game_teams, 5)$team_with_possession

p17 <- team_season |>
  filter(team_with_possession %in% c(top5_close, bottom5_close),
         !is.na(close_go_rate)) |>
  mutate(grp = if_else(team_with_possession %in% top5_close,
                       "Most Aggressive in Close Games",
                       "Least Aggressive in Close Games")) |>
  ggplot(aes(season, close_go_rate,
             color = team_with_possession, group = team_with_possession)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_line(linewidth = 0.9, alpha = 0.85) + geom_point(size = 2) +
  facet_wrap(~grp, ncol = 1) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = seq(2010, 2025, 2)) +
  labs(title = "Go-For-It Rate in Close Games (Q3-Q4, within 8 pts)",
       subtitle = "Top 5 and bottom 5 teams by average close-game aggressiveness",
       x = NULL, y = "Go-for-it rate", color = NULL,
       caption = "Source: Kaggle NFL play-by-play") +
  theme_nfl() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

save_plot(p17, "17_close_game_aggressiveness", h = 10)

# ─────────────────────────────────────────────────────────────────────────────
# 6. Year-over-year stability
# ─────────────────────────────────────────────────────────────────────────────

stability <- team_season |>
  arrange(team_with_possession, season) |>
  group_by(team_with_possession) |>
  mutate(go_rate_lag = lag(go_rate)) |>
  ungroup() |>
  filter(!is.na(go_rate_lag))

r_val <- cor(stability$go_rate_lag, stability$go_rate, use = "complete.obs") |>
  round(3)

p18 <- stability |>
  ggplot(aes(go_rate_lag, go_rate)) +
  geom_point(alpha = 0.25, size = 1.5, color = "#2980b9") +
  geom_smooth(method = "lm", se = TRUE, color = "#c0392b", linewidth = 1.1) +
  annotate("text", x = 0.05, y = 0.55,
           label = paste0("r = ", r_val),
           size = 5, color = "#c0392b", fontface = "bold") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Year-Over-Year Go-Rate Stability",
       subtitle = "Does a team's 4th down tendency persist season to season?",
       x = "Go-for-it rate (season N)", y = "Go-for-it rate (season N+1)",
       caption = "Source: Kaggle NFL play-by-play") +
  theme_nfl()

save_plot(p18, "18_go_rate_stability")

# Write summary CSV
write_csv(all_time |> arrange(desc(avg_agg_index)),
          "output/team_aggressiveness_summary.csv")
message("  Saved: output/team_aggressiveness_summary.csv")

message("[05] Coach analysis complete.\n")
