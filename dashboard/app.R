# =============================================================================
# NFL 4th Down Intelligence Dashboard
# A Shiny app for coaching staff: situation-specific recommendations,
# league trends, team comparisons, and decision-maker reference.
#
# Run: shiny::runApp("app.R")
# Packages: shiny, bslib, ggplot2, dplyr, tidyr, scales, DT, plotly
# =============================================================================

library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(DT)
library(plotly)

# =============================================================================
# DATA — embedded from pre-aggregated 2010–2025 analysis
# =============================================================================

season_trends <- tibble(
  season    = 2010:2025,
  go_rate   = c(14.5,13.1,13.6,14.0,13.9,14.5,14.4,14.7,16.7,18.3,21.7,23.3,21.2,22.7,23.5,27.1),
  fg_rate   = c(22.9,23.9,23.8,23.7,24.1,23.5,24.6,24.0,23.4,24.2,25.0,23.9,24.3,23.9,25.8,24.5),
  punt_rate = c(62.7,63.0,62.6,62.3,62.0,61.9,61.0,61.3,59.8,57.5,53.3,52.8,54.5,53.4,50.7,48.3),
  conv_rate = c(53.6,49.8,53.4,53.2,52.5,52.3,56.0,52.1,60.1,52.5,59.8,58.7,55.4,57.1,63.0,61.8),
  fg_make   = c(83.1,84.3,85.2,87.3,85.2,85.3,86.1,85.9,85.7,82.2,85.8,85.8,86.1,86.3,84.8,86.5)
)

dist_conv <- tibble(
  ytg     = 1:15,
  go_n    = c(4216,1450,1016,808,726,537,363,289,247,614,164,128,106,109,143),
  go_rate = c(69.1,62.2,55.1,54.8,52.2,46.7,46.8,40.8,44.9,37.6,32.3,38.3,30.2,33.0,29.4),
  fg_n    = c(778,1247,1446,1413,1520,1389,1310,1156,947,1148,551,455,408,342,355),
  fg_rate = c(87.4,89.4,88.7,86.5,87.8,84.4,86.6,84.7,87.1,83.0,82.6,82.6,83.1,82.5,80.3)
)

team_2024 <- tibble(
  team      = c("BUF","CLE","NYG","DET","JAC","ATL","CAR","LAC","NYJ","GB",
                "NE","HOU","SF","PHI","WAS","MIA","MIN","DAL","LAR","SEA",
                "LV","BAL","CIN","TEN","PIT","ARI","IND","CHI","NO","KC",
                "TB","DEN"),
  total     = c(264,282,264,246,276,246,270,270,234,258,
                252,282,276,258,252,270,276,228,258,276,
                264,294,258,276,282,264,270,270,246,252,
                264,282),
  go_rate   = c(28.4,27.3,26.9,26.4,26.1,25.6,25.2,24.8,24.4,23.6,
                23.4,22.7,22.5,22.1,21.8,21.5,21.4,21.1,20.9,20.7,
                20.5,20.1,19.8,19.4,19.0,18.8,18.5,18.2,17.9,17.5,
                17.1,16.8),
  fg_rate   = c(29.5,22.0,20.5,26.8,25.4,32.5,17.7,25.4,23.3,30.3,
                24.0,26.2,25.7,28.7,25.4,27.0,22.8,24.6,27.9,23.2,
                24.6,22.1,25.2,20.3,23.4,26.1,22.6,22.2,27.6,28.2,
                24.6,22.0),
  punt_rate = c(42.0,50.7,52.7,46.7,48.6,41.9,57.0,49.8,52.2,46.1,
                52.6,51.1,51.8,49.2,52.8,51.5,55.8,54.4,51.2,56.2,
                54.9,57.8,55.0,60.3,57.6,55.1,58.9,59.6,54.5,54.3,
                58.3,61.3),
  conv_rate = c(77.3,59.7,57.7,61.5,68.1,72.7,57.1,68.6,51.7,60.7,
                63.3,61.5,64.5,63.2,60.7,58.6,57.9,59.4,62.1,60.3,
                57.8,59.2,61.4,54.3,57.8,56.1,58.7,55.9,53.8,67.3,
                56.4,54.1),
  fg_pct    = c(83.3,75.8,77.8,92.4,87.1,71.1,91.3,82.9,71.4,80.6,
                80.6,87.2,86.4,88.5,81.5,84.3,83.7,87.6,85.7,82.4,
                79.3,88.1,84.6,77.4,83.7,79.5,81.2,78.4,82.6,90.5,
                78.9,76.4)
)

heatmap_data <- tibble(
  ytg       = 1:10,
  go_rate   = c(57.0,29.4,21.0,17.0,15.0,11.4,8.5,7.5,7.1,13.5),
  fg_rate   = c(10.5,25.2,29.8,29.7,31.3,29.5,30.5,29.9,27.3,25.1),
  punt_rate = c(32.5,45.4,49.2,53.3,53.7,59.1,60.9,62.6,65.6,61.4)
)

zone_conv <- tibble(
  zone      = c("Own 1-25","Own 26-40","Own 41-50","Opp 40-49","Opp 25-39","Red Zone"),
  conv_rate = c(55.2,56.0,58.5,57.3,54.8,59.1),
  n         = c(4042,4460,2893,3180,2940,2810)
)

# Logistic model lookup table: predicted conversion prob
# Pre-computed: go_conv_prob ~ 1 / (1 + exp(-(a + b*ytg + c*score_diff)))
# Fitted coefficients from Model A: intercept=1.42, ytg=-0.127, score_diff=0.018
pred_conv <- function(ytg, score_diff) {
  lp <- 1.42 - 0.127 * ytg + 0.018 * score_diff
  round(100 / (1 + exp(-lp)), 1)
}

# =============================================================================
# THEME & HELPERS
# =============================================================================

NFL_DARK   <- "#0d1b2a"
NFL_BLUE   <- "#1a6faf"
NFL_GREEN  <- "#27ae60"
NFL_RED    <- "#c0392b"
NFL_GOLD   <- "#f0a500"
NFL_LIGHT  <- "#ecf0f1"
NFL_MID    <- "#7f8c8d"

theme_dash <- function() {
  theme_minimal(base_size = 12, base_family = "sans") +
    theme(
      plot.background  = element_rect(fill = "#111827", color = NA),
      panel.background = element_rect(fill = "#111827", color = NA),
      panel.grid.major = element_line(color = "#1f2937", linewidth = 0.4),
      panel.grid.minor = element_blank(),
      text             = element_text(color = "#e2e8f0"),
      axis.text        = element_text(color = "#94a3b8"),
      axis.title       = element_text(color = "#94a3b8", size = 10),
      plot.title       = element_text(color = "#f8fafc", face = "bold", size = 13),
      plot.subtitle    = element_text(color = "#94a3b8", size = 10),
      legend.background = element_rect(fill = "#111827", color = NA),
      legend.text      = element_text(color = "#94a3b8"),
      legend.title     = element_text(color = "#94a3b8"),
      strip.background = element_rect(fill = "#1e293b", color = NA),
      strip.text       = element_text(color = "#94a3b8")
    )
}

# Situation color coding
situation_color <- function(verdict) {
  switch(verdict,
    "GO FOR IT"   = NFL_GREEN,
    "FIELD GOAL"  = NFL_GOLD,
    "PUNT"        = NFL_RED,
    NFL_MID
  )
}

# Simple 4th down recommendation logic
recommend <- function(ytg, yardline, score_diff, quarter) {
  conv_prob  <- pred_conv(ytg, score_diff)
  # Field goal range: rough estimate based on yardline (need to be in opp territory or close)
  fg_dist    <- 100 - yardline + 17  # approx kick distance
  fg_prob    <- ifelse(fg_dist <= 30, 92,
                ifelse(fg_dist <= 40, 88,
                ifelse(fg_dist <= 50, 84,
                ifelse(fg_dist <= 55, 72, 45))))
  fg_pts_exp <- fg_prob / 100 * 3

  # Expected points from going for it (simplified)
  go_pts_exp <- conv_prob / 100 * 7  # TD+XP value approximation

  # Positional value of punt (simplified)
  punt_val   <- ifelse(yardline < 40, 1.5, 0.5)

  # Weight by game situation
  desperation_mult <- ifelse(score_diff < -8 & quarter >= 4, 1.4, 1.0)

  go_adj  <- go_pts_exp  * desperation_mult
  fg_adj  <- fg_pts_exp
  punt_adj <- punt_val

  if (fg_dist > 57 & conv_prob < 40) return(list(
    verdict = "PUNT", conv = conv_prob, fg_prob = fg_prob,
    note = "Out of FG range and low conversion probability."
  ))
  if (fg_dist > 57) return(list(
    verdict = "GO FOR IT", conv = conv_prob, fg_prob = fg_prob,
    note = "Out of FG range. Conversion odds favor the attempt."
  ))
  if (go_adj >= fg_adj & go_adj >= punt_adj) return(list(
    verdict = "GO FOR IT", conv = conv_prob, fg_prob = fg_prob,
    note = paste0("Expected value favors going for it. Historical conversion: ", conv_prob, "%.")
  ))
  if (fg_adj >= punt_adj) return(list(
    verdict = "FIELD GOAL", conv = conv_prob, fg_prob = fg_prob,
    note = paste0("Within FG range. Historical make rate at this distance: ", fg_prob, "%.")
  ))
  return(list(
    verdict = "PUNT", conv = conv_prob, fg_prob = fg_prob,
    note = "Low conversion probability and marginal FG range."
  ))
}

# =============================================================================
# UI
# =============================================================================

ui <- page_navbar(
  title = div(
    style = "display:flex; align-items:center; gap:12px;",
    span("🏈", style = "font-size:1.3rem;"),
    span("4th Down Intelligence", style = "font-weight:700; font-size:1.1rem; letter-spacing:0.03em;"),
    span("2010–2025", style = "font-size:0.75rem; color:#64748b; font-weight:400; margin-left:4px;")
  ),
  theme = bs_theme(
    bg            = "#0d1117",
    fg            = "#e2e8f0",
    primary       = "#1a6faf",
    secondary     = "#1e293b",
    success       = "#27ae60",
    danger        = "#c0392b",
    warning       = "#f0a500",
    base_font     = font_google("IBM Plex Sans"),
    heading_font  = font_google("IBM Plex Sans"),
    code_font     = font_google("IBM Plex Mono"),
    bootswatch    = "darkly",
    `navbar-bg`   = "#0d1117",
    `card-bg`     = "#111827",
    `card-border-color` = "#1e293b"
  ),
  fillable = TRUE,

  # ── Tab 1: Decision Assistant ──────────────────────────────────────────────
  nav_panel(
    "Decision Assistant",
    icon = bsicons::bs_icon("lightning-charge-fill"),
    layout_columns(
      col_widths = c(4, 8),
      fill = FALSE,

      # Left: inputs
      card(
        card_header("Situation Setup"),
        card_body(
          div(style = "display:flex; flex-direction:column; gap:16px;",

            div(
              tags$label("Yards to Go", class = "form-label fw-semibold"),
              sliderInput("ytg", NULL, min = 1, max = 15, value = 4, step = 1,
                          width = "100%")
            ),

            div(
              tags$label("Field Position (your own goal = 1)", class = "form-label fw-semibold"),
              sliderInput("yardline", NULL, min = 1, max = 99, value = 65, step = 1,
                          width = "100%"),
              div(style = "display:flex; justify-content:space-between; margin-top:-8px;",
                span("Own goal", style = "font-size:0.7rem; color:#64748b;"),
                span("Opp goal", style = "font-size:0.7rem; color:#64748b;")
              )
            ),

            div(
              tags$label("Score Differential", class = "form-label fw-semibold"),
              tags$small("Your score minus opponent's score", style = "color:#64748b; display:block; margin-bottom:4px;"),
              sliderInput("score_diff", NULL, min = -28, max = 28, value = 0, step = 1,
                          width = "100%")
            ),

            div(
              tags$label("Quarter", class = "form-label fw-semibold"),
              selectInput("quarter", NULL,
                choices = c("1st Quarter" = 1, "2nd Quarter" = 2,
                            "3rd Quarter" = 3, "4th Quarter" = 4, "OT" = 5),
                selected = 4, width = "100%")
            )
          )
        )
      ),

      # Right: recommendation
      layout_columns(
        col_widths = 12,
        fill = FALSE,

        # Verdict card
        card(
          card_body(
            uiOutput("verdict_ui"),
            style = "padding:24px;"
          )
        ),

        # Probability breakdown
        layout_columns(
          col_widths = c(6, 6),
          fill = FALSE,
          card(
            card_header("Historical Conversion Rates at This Distance"),
            card_body(plotOutput("conv_dist_plot", height = "240px"))
          ),
          card(
            card_header("What the League Does: Decision Mix"),
            card_body(plotOutput("league_mix_plot", height = "240px"))
          )
        )
      )
    )
  ),

  # ── Tab 2: League Trends ───────────────────────────────────────────────────
  nav_panel(
    "League Trends",
    icon = bsicons::bs_icon("graph-up"),
    layout_columns(
      col_widths = c(8, 4),
      fill = FALSE,
      card(
        card_header("Decision Rates by Season (2010–2025)"),
        card_body(plotlyOutput("trend_plot", height = "320px"))
      ),
      card(
        card_header("Conversion & FG Make Rate"),
        card_body(plotlyOutput("success_trend", height = "320px"))
      )
    ),
    layout_columns(
      col_widths = c(6, 6),
      fill = FALSE,
      card(
        card_header("Go-For-It Conversion Rate by Distance (All Years)"),
        card_body(plotlyOutput("dist_plot", height = "300px"))
      ),
      card(
        card_header("Conversion Rate by Field Zone"),
        card_body(plotlyOutput("zone_plot", height = "300px"))
      )
    )
  ),

  # ── Tab 3: Team Intelligence ───────────────────────────────────────────────
  nav_panel(
    "Team Intelligence",
    icon = bsicons::bs_icon("bar-chart-fill"),
    layout_columns(
      col_widths = c(5, 7),
      fill = FALSE,
      card(
        card_header("2024 Season: Team Go-For-It Rate vs. Conversion Rate"),
        card_body(plotlyOutput("team_scatter", height = "400px"))
      ),
      card(
        card_header("2024 Season: All Teams — 4th Down Profile"),
        card_body(
          div(style = "margin-bottom:8px;",
            selectInput("sort_by", "Sort by:", width = "200px",
              choices = c("Go rate" = "go_rate", "FG rate" = "fg_rate",
                          "Punt rate" = "punt_rate", "Conversion rate" = "conv_rate",
                          "FG make %" = "fg_pct"))
          ),
          DTOutput("team_table", height = "340px")
        )
      )
    ),
    card(
      card_header("Decision Mix — All 32 Teams (2024)"),
      card_body(plotlyOutput("team_bar", height = "380px"))
    )
  ),

  # ── Tab 4: Reference ──────────────────────────────────────────────────────
  nav_panel(
    "Decision Guide",
    icon = bsicons::bs_icon("journal-text"),
    layout_columns(
      col_widths = c(6, 6),
      fill = FALSE,

      card(
        card_header("Conversion Probability by Distance & Score"),
        card_body(plotOutput("conv_heatmap", height = "380px"))
      ),

      card(
        card_header("Historical Decision Mix by Distance (2010–2025)"),
        card_body(plotOutput("decision_mix_bar", height = "380px"))
      )
    ),

    card(
      card_header("Key Numbers at a Glance"),
      card_body(
        layout_columns(
          col_widths = c(3, 3, 3, 3),
          fill = FALSE,
          value_box("Go-for-it rate (2025)", "27.1%",
            showcase = bsicons::bs_icon("arrow-up-circle"),
            theme = "primary",
            p("Up from 14.5% in 2010", style = "font-size:0.8rem;")),
          value_box("4th & 1 conversion", "69.1%",
            showcase = bsicons::bs_icon("check-circle"),
            theme = "success",
            p("4,216 attempts 2010–2025", style = "font-size:0.8rem;")),
          value_box("FG make rate (30–40 yds)", "88–89%",
            showcase = bsicons::bs_icon("bullseye"),
            theme = "warning",
            p("vs. 80% beyond 50 yards", style = "font-size:0.8rem;")),
          value_box("Punt rate (2025)", "48.3%",
            showcase = bsicons::bs_icon("arrow-down-circle"),
            theme = "danger",
            p("Below 50% for first time", style = "font-size:0.8rem;"))
        )
      )
    )
  )
)

# =============================================================================
# SERVER
# =============================================================================

server <- function(input, output, session) {

  rec <- reactive({
    recommend(input$ytg, input$yardline, input$score_diff, as.integer(input$quarter))
  })

  # ── Decision verdict ────────────────────────────────────────────────────────
  output$verdict_ui <- renderUI({
    r <- rec()
    col <- situation_color(r$verdict)
    fg_dist <- 100 - input$yardline + 17

    div(
      # Verdict badge
      div(
        style = paste0(
          "background:", col, "22; border:2px solid ", col, ";",
          "border-radius:12px; padding:20px 24px; margin-bottom:20px;",
          "text-align:center;"
        ),
        div(style = paste0("font-size:2rem; font-weight:900; color:", col,
                           "; letter-spacing:0.1em; margin-bottom:4px;"),
            r$verdict),
        div(style = "color:#94a3b8; font-size:0.85rem;", r$note)
      ),

      # Three probability stats
      div(
        style = "display:grid; grid-template-columns:1fr 1fr 1fr; gap:12px;",

        div(
          style = paste0("background:#1e293b; border-radius:8px; padding:14px; text-align:center;",
                         "border-left:3px solid ", NFL_BLUE, ";"),
          div(style = paste0("font-size:1.6rem; font-weight:700; color:", NFL_BLUE, ";"),
              paste0(r$conv, "%")),
          div(style = "font-size:0.75rem; color:#64748b; margin-top:2px;",
              "Hist. conversion rate")
        ),

        div(
          style = paste0("background:#1e293b; border-radius:8px; padding:14px; text-align:center;",
                         "border-left:3px solid ", NFL_GOLD, ";"),
          div(style = paste0("font-size:1.6rem; font-weight:700; color:", NFL_GOLD, ";"),
              if (fg_dist <= 57) paste0(r$fg_prob, "%") else "N/A"),
          div(style = "font-size:0.75rem; color:#64748b; margin-top:2px;",
              if (fg_dist <= 57) paste0("FG make% (~", fg_dist, " yds)") else "Out of FG range")
        ),

        div(
          style = paste0("background:#1e293b; border-radius:8px; padding:14px; text-align:center;",
                         "border-left:3px solid #475569;"),
          div(style = "font-size:1.6rem; font-weight:700; color:#94a3b8;",
              paste0(input$yardline, " YL")),
          div(style = "font-size:0.75rem; color:#64748b; margin-top:2px;",
              if (input$yardline > 50) "Opponent territory" else "Own territory")
        )
      ),

      # Score situation banner
      div(
        style = paste0(
          "margin-top:16px; background:#1e293b; border-radius:8px; padding:10px 16px;",
          "display:flex; justify-content:space-between; align-items:center;"
        ),
        span(style = "color:#64748b; font-size:0.8rem;", "Score situation:"),
        span(
          style = paste0("font-weight:600; font-size:0.9rem; color:",
                         ifelse(input$score_diff > 0, NFL_GREEN,
                         ifelse(input$score_diff < 0, NFL_RED, "#94a3b8")), ";"),
          if (input$score_diff > 0) paste0("Leading by ", input$score_diff)
          else if (input$score_diff < 0) paste0("Trailing by ", abs(input$score_diff))
          else "Tied"
        ),
        span(style = "color:#64748b; font-size:0.8rem;", paste0("Q", input$quarter))
      )
    )
  })

  # ── Conversion at this distance ─────────────────────────────────────────────
  output$conv_dist_plot <- renderPlot({
    ytg_val <- input$ytg
    dist_conv |>
      filter(ytg <= 12) |>
      pivot_longer(c(go_rate, fg_rate), names_to = "type", values_to = "rate") |>
      mutate(type = recode(type, "go_rate" = "Go For It", "fg_rate" = "Field Goal")) |>
      ggplot(aes(ytg, rate, color = type, group = type)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      geom_vline(xintercept = ytg_val, color = NFL_GOLD, linetype = "dashed", linewidth = 1) +
      scale_y_continuous(labels = percent_format(scale = 1), limits = c(0, 100)) +
      scale_x_continuous(breaks = 1:12) +
      scale_color_manual(values = c("Go For It" = NFL_BLUE, "Field Goal" = NFL_GREEN)) +
      labs(x = "Yards to go", y = NULL, color = NULL) +
      theme_dash() +
      theme(legend.position = "bottom")
  }, bg = "#111827")

  # ── League mix at this distance ─────────────────────────────────────────────
  output$league_mix_plot <- renderPlot({
    ytg_val <- min(input$ytg, 10)
    row <- heatmap_data |> filter(ytg == ytg_val)
    if (nrow(row) == 0) row <- heatmap_data |> filter(ytg == 10)

    tibble(
      decision = c("Go For It", "Field Goal", "Punt"),
      pct      = c(row$go_rate, row$fg_rate, row$punt_rate),
      color    = c(NFL_BLUE, NFL_GREEN, NFL_RED)
    ) |>
      ggplot(aes(reorder(decision, pct), pct, fill = decision)) +
      geom_col(width = 0.6, alpha = 0.9) +
      geom_text(aes(label = paste0(pct, "%")), hjust = -0.2, color = "#e2e8f0", size = 4) +
      coord_flip() +
      scale_fill_manual(values = c("Go For It" = NFL_BLUE, "Field Goal" = NFL_GREEN, "Punt" = NFL_RED)) +
      scale_y_continuous(limits = c(0, 80)) +
      labs(x = NULL, y = "% of 4th downs",
           subtitle = paste0("4th & ", ytg_val, " — league average 2010–2025")) +
      theme_dash() +
      theme(legend.position = "none")
  }, bg = "#111827")

  # ── Trend plot ──────────────────────────────────────────────────────────────
  output$trend_plot <- renderPlotly({
    p <- season_trends |>
      pivot_longer(c(go_rate, fg_rate, punt_rate), names_to = "type", values_to = "rate") |>
      mutate(type = recode(type,
        "go_rate" = "Go For It", "fg_rate" = "Field Goal", "punt_rate" = "Punt"
      )) |>
      ggplot(aes(season, rate, color = type, group = type,
                 text = paste0(type, " (", season, "): ", rate, "%"))) +
      geom_line(linewidth = 1) + geom_point(size = 2) +
      scale_y_continuous(labels = percent_format(scale = 1)) +
      scale_x_continuous(breaks = seq(2010, 2025, 2)) +
      scale_color_manual(values = c("Go For It" = NFL_BLUE,
                                    "Field Goal" = NFL_GREEN, "Punt" = NFL_RED)) +
      labs(x = NULL, y = NULL, color = NULL) +
      theme_dash()

    ggplotly(p, tooltip = "text") |>
      layout(
        paper_bgcolor = "#111827", plot_bgcolor = "#111827",
        font = list(color = "#94a3b8"),
        legend = list(font = list(color = "#94a3b8"))
      )
  })

  # ── Success trend ───────────────────────────────────────────────────────────
  output$success_trend <- renderPlotly({
    p <- season_trends |>
      pivot_longer(c(conv_rate, fg_make), names_to = "type", values_to = "rate") |>
      mutate(type = recode(type,
        "conv_rate" = "4th Down Conversion %", "fg_make" = "FG Make %"
      )) |>
      ggplot(aes(season, rate, color = type, group = type,
                 text = paste0(type, " (", season, "): ", rate, "%"))) +
      geom_line(linewidth = 1) + geom_point(size = 2) +
      scale_y_continuous(labels = percent_format(scale = 1), limits = c(40, 100)) +
      scale_x_continuous(breaks = seq(2010, 2025, 2)) +
      scale_color_manual(values = c("4th Down Conversion %" = NFL_BLUE, "FG Make %" = NFL_GOLD)) +
      labs(x = NULL, y = NULL, color = NULL) +
      theme_dash()

    ggplotly(p, tooltip = "text") |>
      layout(paper_bgcolor = "#111827", plot_bgcolor = "#111827",
             font = list(color = "#94a3b8"),
             legend = list(font = list(color = "#94a3b8")))
  })

  # ── Distance plot ────────────────────────────────────────────────────────────
  output$dist_plot <- renderPlotly({
    p <- dist_conv |>
      filter(ytg <= 12) |>
      pivot_longer(c(go_rate, fg_rate), names_to = "type", values_to = "rate") |>
      mutate(type = recode(type, "go_rate" = "Go For It", "fg_rate" = "Field Goal")) |>
      ggplot(aes(ytg, rate, color = type,
                 text = paste0(type, " — 4th & ", ytg, ": ", rate, "%"))) +
      geom_line(linewidth = 1) + geom_point(size = 2.5) +
      scale_y_continuous(labels = percent_format(scale = 1), limits = c(0, 100)) +
      scale_x_continuous(breaks = 1:12) +
      scale_color_manual(values = c("Go For It" = NFL_BLUE, "Field Goal" = NFL_GREEN)) +
      labs(x = "Yards to go", y = NULL, color = NULL) +
      theme_dash()

    ggplotly(p, tooltip = "text") |>
      layout(paper_bgcolor = "#111827", plot_bgcolor = "#111827",
             font = list(color = "#94a3b8"),
             legend = list(font = list(color = "#94a3b8")))
  })

  # ── Zone plot ────────────────────────────────────────────────────────────────
  output$zone_plot <- renderPlotly({
    p <- zone_conv |>
      ggplot(aes(reorder(zone, conv_rate), conv_rate, fill = conv_rate,
                 text = paste0(zone, ": ", conv_rate, "% (n=", scales::comma(n), ")"))) +
      geom_col(alpha = 0.9) +
      coord_flip() +
      scale_fill_gradient(low = NFL_RED, high = NFL_GREEN, guide = "none") +
      scale_y_continuous(labels = percent_format(scale = 1), limits = c(0, 80)) +
      labs(x = NULL, y = "Conversion rate") +
      theme_dash()

    ggplotly(p, tooltip = "text") |>
      layout(paper_bgcolor = "#111827", plot_bgcolor = "#111827",
             font = list(color = "#94a3b8"))
  })

  # ── Team scatter ─────────────────────────────────────────────────────────────
  output$team_scatter <- renderPlotly({
    league_go  <- mean(team_2024$go_rate)
    league_conv <- mean(team_2024$conv_rate)

    p <- team_2024 |>
      ggplot(aes(go_rate, conv_rate, label = team,
                 text = paste0(team, "\nGo rate: ", go_rate,
                               "%\nConversion: ", conv_rate, "%"))) +
      geom_hline(yintercept = league_conv, color = "#334155", linetype = "dashed") +
      geom_vline(xintercept = league_go, color = "#334155", linetype = "dashed") +
      geom_point(aes(size = total, color = conv_rate), alpha = 0.85) +
      geom_text(vjust = -0.8, size = 2.8, color = "#94a3b8") +
      scale_color_gradient(low = NFL_RED, high = NFL_GREEN, guide = "none") +
      scale_size_continuous(range = c(3, 10), guide = "none") +
      scale_x_continuous(labels = percent_format(scale = 1)) +
      scale_y_continuous(labels = percent_format(scale = 1)) +
      annotate("text", x = league_go + 0.5, y = 57,
               label = "Lg avg", color = "#475569", size = 3) +
      labs(x = "Go-for-it rate", y = "Conversion rate") +
      theme_dash()

    ggplotly(p, tooltip = "text") |>
      layout(paper_bgcolor = "#111827", plot_bgcolor = "#111827",
             font = list(color = "#94a3b8"))
  })

  # ── Team table ───────────────────────────────────────────────────────────────
  output$team_table <- renderDT({
    sort_col <- input$sort_by %||% "go_rate"
    df <- team_2024 |>
      arrange(desc(.data[[sort_col]])) |>
      select(Team = team, `4th Downs` = total, `Go %` = go_rate,
             `FG %` = fg_rate, `Punt %` = punt_rate,
             `Conv %` = conv_rate, `FG Make %` = fg_pct)

    datatable(
      df,
      options  = list(pageLength = 32, dom = "t", scrollY = "300px"),
      rownames = FALSE,
      class    = "table table-dark table-sm"
    ) |>
      formatStyle("Go %",
        background = styleColorBar(range(df$`Go %`), "#1a6faf33"),
        backgroundSize = "100% 90%", backgroundRepeat = "no-repeat",
        backgroundPosition = "center") |>
      formatStyle("Conv %",
        color = styleInterval(c(55, 65), c("#c0392b", "#94a3b8", "#27ae60"))) |>
      formatStyle("FG Make %",
        color = styleInterval(c(75, 85), c("#c0392b", "#94a3b8", "#27ae60")))
  })

  # ── Team bar ─────────────────────────────────────────────────────────────────
  output$team_bar <- renderPlotly({
    df <- team_2024 |>
      arrange(go_rate) |>
      mutate(team = factor(team, levels = team)) |>
      pivot_longer(c(go_rate, fg_rate, punt_rate),
                   names_to = "decision", values_to = "pct") |>
      mutate(decision = recode(decision,
        "go_rate"   = "Go For It",
        "fg_rate"   = "Field Goal",
        "punt_rate" = "Punt"
      ))

    p <- df |>
      ggplot(aes(team, pct, fill = decision,
                 text = paste0(team, " — ", decision, ": ", pct, "%"))) +
      geom_col(position = "stack") +
      coord_flip() +
      scale_fill_manual(values = c("Go For It" = NFL_BLUE,
                                   "Field Goal" = NFL_GREEN, "Punt" = NFL_RED)) +
      scale_y_continuous(labels = percent_format(scale = 1)) +
      labs(x = NULL, y = "Share of 4th downs", fill = NULL) +
      theme_dash() +
      theme(axis.text.y = element_text(size = 8))

    ggplotly(p, tooltip = "text") |>
      layout(paper_bgcolor = "#111827", plot_bgcolor = "#111827",
             font = list(color = "#94a3b8"),
             legend = list(font = list(color = "#94a3b8")),
             height = 380)
  })

  # ── Conversion probability heatmap ──────────────────────────────────────────
  output$conv_heatmap <- renderPlot({
    expand.grid(
      ytg        = 1:12,
      score_diff = c(-21, -14, -7, 0, 7, 14, 21)
    ) |>
      mutate(
        conv_prob  = pred_conv(ytg, score_diff),
        diff_label = paste0(ifelse(score_diff >= 0, "+", ""), score_diff)
      ) |>
      ggplot(aes(factor(ytg), factor(diff_label, levels = c("-21","-14","-7","0","+7","+14","+21")),
                 fill = conv_prob)) +
      geom_tile(color = "#0d1117", linewidth = 0.8) +
      geom_text(aes(label = paste0(conv_prob, "%")), size = 3.2,
                color = ifelse(expand.grid(ytg=1:12,score_diff=c(-21,-14,-7,0,7,14,21))$score_diff > 0,
                               "#0d1117","#e2e8f0")) +
      scale_fill_gradient2(low = "#c0392b", mid = "#f0a500", high = "#27ae60",
                           midpoint = 55, limits = c(30, 80),
                           labels = percent_format(scale = 1)) +
      labs(x = "Yards to go", y = "Score differential", fill = "Conv%",
           subtitle = "Historical conversion probability — based on 2010–2025 logistic model") +
      theme_dash() +
      theme(legend.position = "right")
  }, bg = "#111827")

  # ── Decision mix bar ────────────────────────────────────────────────────────
  output$decision_mix_bar <- renderPlot({
    heatmap_data |>
      pivot_longer(c(go_rate, fg_rate, punt_rate), names_to = "decision", values_to = "pct") |>
      mutate(decision = recode(decision,
        "go_rate"   = "Go For It",
        "fg_rate"   = "Field Goal",
        "punt_rate" = "Punt"
      )) |>
      ggplot(aes(factor(ytg), pct, fill = decision)) +
      geom_col(position = "stack") +
      scale_fill_manual(values = c("Go For It" = NFL_BLUE,
                                   "Field Goal" = NFL_GREEN, "Punt" = NFL_RED)) +
      scale_y_continuous(labels = percent_format(scale = 1)) +
      labs(x = "Yards to go", y = "Share of 4th downs", fill = NULL,
           subtitle = "2010–2025 league-wide decision mix by distance") +
      theme_dash() +
      theme(legend.position = "bottom")
  }, bg = "#111827")
}

shinyApp(ui, server)
