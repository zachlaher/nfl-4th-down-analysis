# =============================================================================
# 04_model.R
# Two models:
#
#   Model A — Conversion probability (logistic):
#     Predicts whether a go-for-it attempt converts.
#     Features: yards_to_go, abs_yardline, score_diff, quarter_clean,
#               game_type, season_c
#
#   Model B — Win probability by decision (logistic):
#     Predicts game win from the decision made on 4th down.
#     Features: decision, yards_to_go, abs_yardline, score_diff,
#               quarter_clean, game_type, season_c
#     This isolates the decision's association with winning after
#     controlling for situation (distance, field position, score margin).
#
# Outputs:
#   09_conversion_model_coefs.png
#   10_conversion_roc.png
#   11_conversion_prob_surface.png
#   12_win_model_coefs.png
#   13_win_prob_by_decision_and_diff.png
# =============================================================================

if (!exists("fourth_down_fe")) source("R/02_feature_engineering.R")

library(broom)
library(pROC)

OUTPUT_DIR <- "output"
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR)

theme_nfl <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.title       = element_text(face = "bold", size = 15),
      plot.subtitle    = element_text(color = "grey40", size = 11),
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

# ─────────────────────────────────────────────────────────────────────────────
# Model A: Conversion probability
# ─────────────────────────────────────────────────────────────────────────────

message("[04] Fitting Model A: conversion probability...")

conv_data <- fourth_down_fe |>
  filter(
    decision == "Go For It",
    !is.na(converted), !is.na(yards_to_go), !is.na(abs_yardline),
    !is.na(score_diff), !is.na(quarter_clean), quarter_clean != "Unknown",
    !is.na(season_c)
  ) |>
  mutate(
    converted_int = as.integer(converted),
    quarter_f     = factor(quarter_clean, levels = c("Q1","Q2","Q3","Q4","OT")),
    game_type_f   = factor(game_type, levels = c("Regular Season","Playoffs"))
  )

stopifnot(
  sum(is.na(conv_data$converted_int)) == 0,
  sum(is.na(conv_data$yards_to_go))   == 0,
  sum(is.na(conv_data$score_diff))    == 0
)

model_conv <- glm(
  converted_int ~
    yards_to_go + abs_yardline + score_diff +
    quarter_f + game_type_f + season_c,
  data   = conv_data,
  family = binomial(link = "logit")
)

conv_tidy <- tidy(model_conv, exponentiate = TRUE, conf.int = TRUE) |>
  mutate(significant = p.value < 0.05)

conv_aug  <- augment(model_conv, type.predict = "response")
roc_conv  <- roc(conv_aug$converted_int, conv_aug$.fitted, quiet = TRUE)

message("[04] Model A AIC: ", round(AIC(model_conv), 1),
        " | AUC: ", round(auc(roc_conv), 3))

# Plot 09: coefficient forest plot
p09 <- conv_tidy |>
  filter(term != "(Intercept)") |>
  mutate(term = str_replace_all(term, c(
    "yards_to_go"        = "Yards to go",
    "abs_yardline"       = "Field position",
    "score_diff"         = "Score differential",
    "quarter_fQ2"        = "Quarter: Q2",
    "quarter_fQ3"        = "Quarter: Q3",
    "quarter_fQ4"        = "Quarter: Q4",
    "quarter_fOT"        = "Quarter: OT",
    "game_type_fPlayoffs"= "Playoffs",
    "season_c"           = "Season trend"
  ))) |>
  ggplot(aes(estimate, reorder(term, estimate), color = significant)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.3) +
  geom_point(size = 3.5) +
  scale_color_manual(values = c("TRUE"="#c0392b","FALSE"="grey60"),
                     labels = c("TRUE"="p < 0.05","FALSE"="p ≥ 0.05")) +
  scale_x_log10() +
  labs(title = "Conversion Model: Odds Ratios",
       subtitle = "Go-for-it attempts only — values > 1 increase conversion odds",
       x = "Odds ratio (log scale)", y = NULL, color = NULL,
       caption = "Source: Kaggle NFL play-by-play") +
  theme_nfl()

save_plot(p09, "09_conversion_model_coefs")

# Plot 10: ROC
roc_df <- data.frame(fpr = 1 - roc_conv$specificities, tpr = roc_conv$sensitivities)
p10 <- ggplot(roc_df, aes(fpr, tpr)) +
  geom_abline(linetype = "dashed", color = "grey60") +
  geom_line(color = "#1a6faf", linewidth = 1.2) +
  annotate("text", x = 0.65, y = 0.15,
           label = paste0("AUC = ", round(auc(roc_conv), 3)),
           size = 5, color = "#1a6faf", fontface = "bold") +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "ROC Curve: Conversion Probability Model",
       x = "False positive rate", y = "True positive rate",
       caption = "Source: Kaggle NFL play-by-play") +
  theme_nfl()

save_plot(p10, "10_conversion_roc")

# Plot 11: predicted conversion probability surface (score_diff x yards_to_go)
grid_conv <- expand.grid(
  yards_to_go  = 1:12,
  score_diff   = c(-14, -7, 0, 7, 14),
  abs_yardline = 50L,
  quarter_f    = factor("Q4", levels = levels(conv_data$quarter_f)),
  game_type_f  = factor("Regular Season", levels = levels(conv_data$game_type_f)),
  season_c     = as.integer(median(conv_data$season_c))
) |>
  mutate(pred = predict(model_conv, newdata = cur_data(), type = "response"),
         diff_label = paste0(ifelse(score_diff >= 0, "+", ""), score_diff))

p11 <- grid_conv |>
  ggplot(aes(yards_to_go, pred, color = factor(score_diff), group = factor(score_diff))) +
  geom_line(linewidth = 1.1) + geom_point(size = 2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  scale_x_continuous(breaks = 1:12) +
  scale_color_viridis_d(name = "Score diff", option = "plasma") +
  labs(title = "Predicted Conversion Probability by Score Differential",
       subtitle = "Q4, midfield, median season — yards to go on x-axis",
       x = "Yards to go", y = "Predicted conversion probability",
       caption = "Source: Kaggle NFL play-by-play | Logistic model") +
  theme_nfl()

save_plot(p11, "11_conversion_prob_by_diff")

# ─────────────────────────────────────────────────────────────────────────────
# Model B: Win probability by decision (situation-controlled)
# ─────────────────────────────────────────────────────────────────────────────

message("[04] Fitting Model B: win probability by decision...")

win_data <- fourth_down_fe |>
  filter(
    decision %in% c("Go For It","Field Goal","Punt"),
    !is.na(win_flag), !is.na(yards_to_go), !is.na(abs_yardline),
    !is.na(score_diff), !is.na(quarter_clean), quarter_clean != "Unknown",
    game_type == "Regular Season"
  ) |>
  mutate(
    win_int    = as.integer(win_flag),
    decision_f = factor(decision, levels = c("Punt","Field Goal","Go For It")),
    quarter_f  = factor(quarter_clean, levels = c("Q1","Q2","Q3","Q4","OT")),
    season_c   = as.integer(season_c)
  )

model_win <- glm(
  win_int ~
    decision_f + yards_to_go + abs_yardline + score_diff +
    quarter_f + season_c,
  data   = win_data,
  family = binomial(link = "logit")
)

win_tidy <- tidy(model_win, exponentiate = TRUE, conf.int = TRUE) |>
  mutate(significant = p.value < 0.05)

win_aug  <- augment(model_win, type.predict = "response")
roc_win  <- roc(win_aug$win_int, win_aug$.fitted, quiet = TRUE)

message("[04] Model B AIC: ", round(AIC(model_win), 1),
        " | AUC: ", round(auc(roc_win), 3))

# Plot 12: win model coefficient forest
p12 <- win_tidy |>
  filter(term != "(Intercept)") |>
  mutate(term = str_replace_all(term, c(
    "decision_fField Goal" = "Decision: Field Goal (vs. Punt)",
    "decision_fGo For It"  = "Decision: Go For It (vs. Punt)",
    "yards_to_go"          = "Yards to go",
    "abs_yardline"         = "Field position",
    "score_diff"           = "Score differential",
    "quarter_fQ2"          = "Quarter: Q2",
    "quarter_fQ3"          = "Quarter: Q3",
    "quarter_fQ4"          = "Quarter: Q4",
    "quarter_fOT"          = "Quarter: OT",
    "season_c"             = "Season trend"
  ))) |>
  ggplot(aes(estimate, reorder(term, estimate), color = significant)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.3) +
  geom_point(size = 3.5) +
  scale_color_manual(values = c("TRUE"="#c0392b","FALSE"="grey60"),
                     labels = c("TRUE"="p < 0.05","FALSE"="p ≥ 0.05")) +
  scale_x_log10() +
  labs(title = "Win Probability Model: Odds Ratios",
       subtitle = "Reference level = Punt — controls for distance, field position, score, quarter",
       x = "Odds ratio (log scale)", y = NULL, color = NULL,
       caption = "Source: Kaggle NFL play-by-play | Regular season 2010–2025") +
  theme_nfl()

save_plot(p12, "12_win_model_coefs")

# Plot 13: predicted win probability by decision x score differential
grid_win <- expand.grid(
  decision_f   = factor(c("Punt","Field Goal","Go For It"),
                        levels = levels(win_data$decision_f)),
  score_diff   = seq(-21, 21, by = 3),
  yards_to_go  = 3L,
  abs_yardline = 55L,
  quarter_f    = factor("Q4", levels = levels(win_data$quarter_f)),
  season_c     = as.integer(median(win_data$season_c))
) |>
  mutate(pred_win = predict(model_win, newdata = cur_data(), type = "response"))

p13 <- grid_win |>
  ggplot(aes(score_diff, pred_win, color = decision_f, group = decision_f)) +
  geom_line(linewidth = 1.2) + geom_point(size = 2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  scale_x_continuous(breaks = seq(-21, 21, by = 7)) +
  scale_color_manual(values = c("Punt"="#c0392b","Field Goal"="#27ae60","Go For It"="#1a6faf")) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
  labs(title = "Predicted Win Probability by Decision and Score Differential",
       subtitle = "Q4, 4th & 3, opp ~45yd line — situation-controlled logistic model",
       x = "Score differential (possessing team)", y = "Predicted win probability",
       color = NULL,
       caption = "Source: Kaggle NFL play-by-play | Regular season 2010–2025") +
  theme_nfl()

save_plot(p13, "13_win_prob_by_decision_and_diff")

message("[04] Modeling complete.\n")
