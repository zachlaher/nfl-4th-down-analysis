# =============================================================================
# 03_eda.R
# EDA: 4th down decisions, FG vs. go-for-it tradeoffs, and winning context.
#
# Plots:
#   01 - Decision rates by season (Go / Punt / FG trend)
#   02 - FG make rate vs. go-for-it conversion rate by distance
#   03 - Win rate by decision type and distance
#   04 - Go-for-it rate heatmap: distance x field zone
#   05 - Decision rates by score differential bucket
#   06 - Win rate by decision x score differential
#   07 - Conversion rate trend by season
#   08 - FG vs. go-for-it: win rate comparison at red zone distances
# =============================================================================

if (!exists("fourth_down_fe")) source("R/02_feature_engineering.R")

OUTPUT_DIR <- "output"
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR)

message("[03] Generating EDA plots...")

theme_nfl <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.title       = element_text(face = "bold", size = 15),
      plot.subtitle    = element_text(color = "grey40", size = 11),
      plot.caption     = element_text(color = "grey55", size = 9),
      panel.grid.minor = element_blank(),
      legend.position  = "bottom"
    )
}

save_plot <- function(p, name, w = 10, h = 6) {
  path <- file.path(OUTPUT_DIR, paste0(name, ".png"))
  ggsave(path, p, width = w, height = h, dpi = 150, bg = "white")
  message("  Saved: ", path)
  invisible(p)
}

# Helper: binomial CI
binom_ci <- function(x, n, z = 1.96) {
  p <- x / n
  se <- sqrt(p * (1 - p) / n)
  list(lo = pmax(0, p - z * se), hi = pmin(1, p + z * se))
}

# ─────────────────────────────────────────────────────────────────────────────
# Plot 01: Decision rates by season
# ─────────────────────────────────────────────────────────────────────────────

decision_season <- fourth_down_fe |>
  filter(decision %in% c("Go For It","Punt","Field Goal"),
         game_type == "Regular Season") |>
  group_by(season) |>
  summarise(
    go_rate = mean(decision == "Go For It"),
    fg_rate = mean(decision == "Field Goal"),
    punt_rate = mean(decision == "Punt"),
    n = n(), .groups = "drop"
  )

p01 <- decision_season |>
  pivot_longer(c(go_rate, fg_rate, punt_rate), names_to = "type", values_to = "rate") |>
  mutate(type = recode(type,
    go_rate = "Go For It", fg_rate = "Field Goal", punt_rate = "Punt"
  )) |>
  ggplot(aes(season, rate, color = type, group = type)) +
  geom_line(linewidth = 1.1) + geom_point(size = 2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = 2010:2025) +
  scale_color_manual(values = c("Go For It"="#1a6faf","Punt"="#c0392b","Field Goal"="#27ae60")) +
  labs(title = "4th Down Decision Rates by Season",
       subtitle = "Regular season, 2010–2025",
       x = NULL, y = "Share of 4th down plays", color = NULL,
       caption = "Source: Kaggle NFL play-by-play") +
  theme_nfl() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

save_plot(p01, "01_decision_rates_by_season")

# ─────────────────────────────────────────────────────────────────────────────
# Plot 02: FG make rate vs. go-for-it conversion rate by distance
# ─────────────────────────────────────────────────────────────────────────────

fg_conv_dist <- bind_rows(
  # FG make rate by distance bucket
  fourth_down_fe |>
    filter(decision == "Field Goal", !is.na(fg_made), !is.na(distance_bucket)) |>
    group_by(distance_bucket) |>
    summarise(n = n(), successes = sum(fg_made), rate = mean(fg_made), .groups = "drop") |>
    mutate(type = "Field Goal Make Rate"),

  # Conversion rate by distance bucket
  fourth_down_fe |>
    filter(decision == "Go For It", !is.na(converted), !is.na(distance_bucket)) |>
    group_by(distance_bucket) |>
    summarise(n = n(), successes = sum(converted), rate = mean(converted), .groups = "drop") |>
    mutate(type = "Go-For-It Conversion Rate")
) |>
  mutate(
    se  = sqrt(rate * (1 - rate) / n),
    lo  = pmax(0, rate - 1.96 * se),
    hi  = pmin(1, rate + 1.96 * se)
  )

p02 <- fg_conv_dist |>
  ggplot(aes(distance_bucket, rate, color = type, group = type)) +
  geom_line(linewidth = 1.1) + geom_point(size = 3) +
  geom_ribbon(aes(ymin = lo, ymax = hi, fill = type), alpha = 0.12, color = NA) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  scale_color_manual(values = c("Field Goal Make Rate"="#27ae60","Go-For-It Conversion Rate"="#1a6faf")) +
  scale_fill_manual(values  = c("Field Goal Make Rate"="#27ae60","Go-For-It Conversion Rate"="#1a6faf")) +
  labs(title = "Field Goal Make Rate vs. Go-For-It Conversion Rate",
       subtitle = "By distance to go, 2010–2025 (ribbon = 95% CI)",
       x = "Distance", y = "Success rate", color = NULL, fill = NULL,
       caption = "Source: Kaggle NFL play-by-play") +
  theme_nfl()

save_plot(p02, "02_fg_vs_conversion_by_distance")

# ─────────────────────────────────────────────────────────────────────────────
# Plot 03: Win rate by decision type (all distances combined)
# ─────────────────────────────────────────────────────────────────────────────

win_by_decision <- fourth_down_fe |>
  filter(decision %in% c("Go For It","Field Goal","Punt"),
         !is.na(win_flag), game_type == "Regular Season") |>
  group_by(decision) |>
  summarise(
    n       = n(),
    wins    = sum(win_flag),
    win_rate = mean(win_flag),
    se      = sqrt(win_rate * (1 - win_rate) / n),
    .groups = "drop"
  )

p03 <- win_by_decision |>
  ggplot(aes(decision, win_rate, fill = decision)) +
  geom_col(width = 0.6, alpha = 0.85) +
  geom_errorbar(aes(ymin = win_rate - 1.96*se, ymax = win_rate + 1.96*se),
                width = 0.2, color = "grey30") +
  geom_text(aes(label = paste0(round(win_rate*100, 1), "%\n(n=", scales::comma(n), ")")),
            vjust = -0.4, size = 3.5) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 0.75), expand = c(0, 0)) +
  scale_fill_manual(values = c("Go For It"="#1a6faf","Punt"="#c0392b","Field Goal"="#27ae60")) +
  labs(title = "Win Rate by 4th Down Decision",
       subtitle = "Regular season 2010–2025 — note: win rate reflects game context, not causation",
       x = NULL, y = "Win rate", fill = NULL,
       caption = "Source: Kaggle NFL play-by-play") +
  theme_nfl() + theme(legend.position = "none")

save_plot(p03, "03_win_rate_by_decision")

# ─────────────────────────────────────────────────────────────────────────────
# Plot 04: Win rate by decision x distance bucket
# ─────────────────────────────────────────────────────────────────────────────

win_decision_dist <- fourth_down_fe |>
  filter(decision %in% c("Go For It","Field Goal"),
         !is.na(win_flag), !is.na(distance_bucket),
         game_type == "Regular Season") |>
  group_by(decision, distance_bucket) |>
  summarise(
    n        = n(),
    win_rate = mean(win_flag),
    se       = sqrt(win_rate * (1 - win_rate) / n),
    .groups  = "drop"
  ) |>
  filter(n >= 30)

p04 <- win_decision_dist |>
  ggplot(aes(distance_bucket, win_rate, color = decision, group = decision)) +
  geom_line(linewidth = 1.1) + geom_point(size = 3) +
  geom_ribbon(aes(ymin = win_rate - 1.96*se, ymax = win_rate + 1.96*se,
                  fill = decision), alpha = 0.12, color = NA) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0.3, 0.85)) +
  scale_color_manual(values = c("Field Goal"="#27ae60","Go For It"="#1a6faf")) +
  scale_fill_manual(values  = c("Field Goal"="#27ae60","Go For It"="#1a6faf")) +
  labs(title = "Win Rate: Field Goal vs. Go For It by Distance",
       subtitle = "Regular season 2010–2025 (ribbon = 95% CI) — context not controlled",
       x = "Distance to go", y = "Win rate", color = NULL, fill = NULL,
       caption = "Source: Kaggle NFL play-by-play") +
  theme_nfl()

save_plot(p04, "04_win_rate_fg_vs_go_by_distance")

# ─────────────────────────────────────────────────────────────────────────────
# Plot 05: Decision rates by score differential bucket
# ─────────────────────────────────────────────────────────────────────────────

decision_by_diff <- fourth_down_fe |>
  filter(decision %in% c("Go For It","Punt","Field Goal"),
         !is.na(score_diff_bucket), game_type == "Regular Season") |>
  group_by(score_diff_bucket) |>
  summarise(
    go_rate   = mean(decision == "Go For It"),
    fg_rate   = mean(decision == "Field Goal"),
    punt_rate = mean(decision == "Punt"),
    n         = n(),
    .groups   = "drop"
  )

p05 <- decision_by_diff |>
  pivot_longer(c(go_rate, fg_rate, punt_rate), names_to = "type", values_to = "rate") |>
  mutate(type = recode(type,
    go_rate = "Go For It", fg_rate = "Field Goal", punt_rate = "Punt"
  )) |>
  ggplot(aes(score_diff_bucket, rate, fill = type)) +
  geom_col(position = "stack") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("Go For It"="#1a6faf","Punt"="#c0392b","Field Goal"="#27ae60")) +
  labs(title = "4th Down Decision Mix by Score Differential",
       subtitle = "Regular season 2010–2025 — score differential at time of play",
       x = "Score differential (possessing team)", y = "Share of 4th downs",
       fill = NULL, caption = "Source: Kaggle NFL play-by-play") +
  theme_nfl() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

save_plot(p05, "05_decision_by_score_diff")

# ─────────────────────────────────────────────────────────────────────────────
# Plot 06: Win rate by decision x score differential bucket
# ─────────────────────────────────────────────────────────────────────────────

win_diff_decision <- fourth_down_fe |>
  filter(decision %in% c("Go For It","Field Goal","Punt"),
         !is.na(win_flag), !is.na(score_diff_bucket),
         game_type == "Regular Season") |>
  group_by(score_diff_bucket, decision) |>
  summarise(
    n        = n(),
    win_rate = mean(win_flag),
    se       = sqrt(win_rate * (1 - win_rate) / n),
    .groups  = "drop"
  ) |>
  filter(n >= 20)

p06 <- win_diff_decision |>
  ggplot(aes(score_diff_bucket, win_rate, color = decision, group = decision)) +
  geom_line(linewidth = 1) + geom_point(size = 2.5) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_manual(values = c("Go For It"="#1a6faf","Punt"="#c0392b","Field Goal"="#27ae60")) +
  labs(title = "Win Rate by Decision Type and Score Differential",
       subtitle = "Regular season 2010–2025 — each point = team-plays in that bucket",
       x = "Score differential", y = "Win rate", color = NULL,
       caption = "Source: Kaggle NFL play-by-play") +
  theme_nfl() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

save_plot(p06, "06_win_rate_by_diff_and_decision")

# ─────────────────────────────────────────────────────────────────────────────
# Plot 07: Go-for-it rate heatmap — distance x field zone
# ─────────────────────────────────────────────────────────────────────────────

go_heatmap <- fourth_down_fe |>
  filter(decision %in% c("Go For It","Punt","Field Goal"),
         !is.na(field_zone), !is.na(distance_bucket)) |>
  group_by(distance_bucket, field_zone) |>
  summarise(go_rate = mean(decision == "Go For It"), n = n(), .groups = "drop") |>
  filter(n >= 20)

p07 <- go_heatmap |>
  ggplot(aes(field_zone, distance_bucket, fill = go_rate)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = paste0(round(go_rate*100), "%")),
            size = 3.5, color = "white", fontface = "bold") +
  scale_fill_gradient2(low = "#2c3e50", mid = "#2980b9", high = "#e74c3c",
                       midpoint = 0.15, labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Go-For-It Rate: Distance vs. Field Position",
       subtitle = "NFL 2010–2025 — redder = more aggressive",
       x = "Field zone", y = "Distance to go", fill = "Go rate",
       caption = "Source: Kaggle NFL play-by-play") +
  theme_nfl() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

save_plot(p07, "07_go_rate_heatmap", w = 11, h = 7)

# ─────────────────────────────────────────────────────────────────────────────
# Plot 08: FG vs. go-for-it win rate in red zone (Opp 1-24), by distance
# ─────────────────────────────────────────────────────────────────────────────

rz_win <- fourth_down_fe |>
  filter(
    field_zone == "Opp 1-24 (red zone)",
    decision %in% c("Go For It","Field Goal"),
    !is.na(win_flag), !is.na(yards_to_go),
    game_type == "Regular Season",
    yards_to_go <= 10
  ) |>
  group_by(yards_to_go, decision) |>
  summarise(n = n(), win_rate = mean(win_flag),
            se = sqrt(win_rate*(1-win_rate)/n), .groups = "drop") |>
  filter(n >= 15)

p08 <- rz_win |>
  ggplot(aes(yards_to_go, win_rate, color = decision, group = decision)) +
  geom_ribbon(aes(ymin = win_rate - 1.96*se, ymax = win_rate + 1.96*se,
                  fill = decision), alpha = 0.12, color = NA) +
  geom_line(linewidth = 1.2) + geom_point(aes(size = n)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0.3, 1.0)) +
  scale_x_continuous(breaks = 1:10) +
  scale_size_continuous(range = c(2, 6), guide = "none") +
  scale_color_manual(values = c("Field Goal"="#27ae60","Go For It"="#1a6faf")) +
  scale_fill_manual(values  = c("Field Goal"="#27ae60","Go For It"="#1a6faf")) +
  labs(title = "Red Zone Win Rate: Field Goal vs. Go For It",
       subtitle = "Opponent's 1-24 yard line — point size = sample size — context not controlled",
       x = "Yards to go", y = "Win rate", color = NULL, fill = NULL,
       caption = "Source: Kaggle NFL play-by-play") +
  theme_nfl()

save_plot(p08, "08_redzone_fg_vs_go_win_rate")

message("[03] EDA complete.\n")
