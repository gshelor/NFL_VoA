##### NFL Vortex of Accuracy Model Evaluation #####

##### loading packages, reading in data #####
library(pacman)
# fmt: skip
p_load(
  tidyverse,
  gt,
  nflverse,
  here,
  gtExtras,
  ModelMetrics,
  ggpubr,
  webshot2,
  RColorBrewer,
  Metrics,
  arrow
)

### identifying season and week of season
season <- readline("What season is it? ")
nfl_week <- readline("What week just occurred? ")

### strings I want to preserve
nfl_text <- "NFL"
week_text <- "Week"

### reading in previous week's data
PrevWeekVoA_Proj <- read_csv(here(
  "Data",
  paste0("VoA", season),
  "VoP",
  paste0(season, nfl_text, week_text, nfl_week, "VoP.csv")
)) |>
  select(game_id, Proj_Margin)

LastWeekGames <- load_schedules(as.numeric(season)) |>
  filter(week == as.numeric(nfl_week)) |>
  select(
    game_id,
    season,
    week,
    away_team,
    away_score,
    home_team,
    home_score,
    result,
    total,
    overtime,
    spread_line
  )

LastWeekGames <- full_join(LastWeekGames, PrevWeekVoA_Proj, by = "game_id")

LastWeekGames <- LastWeekGames |>
  mutate(
    VoA_ae = Metrics::ae(result, Proj_Margin),
    vegas_ae = Metrics::ae(result, spread_line),
    VoA_se = Metrics::se(result, Proj_Margin),
    vegas_se = Metrics::se(result, spread_line),
    straight_up_win = case_when(
      result >= 0 & Proj_Margin >= 0 ~ 1,
      result <= 0 & Proj_Margin <= 0 ~ 1,
      TRUE ~ 0
    ),
    vegas_straight_up_win = case_when(
      result >= 0 & spread_line >= 0 ~ 1,
      result <= 0 & spread_line <= 0 ~ 1,
      TRUE ~ 0
    ),
    ATS_win = case_when(
      result > spread_line & Proj_Margin > spread_line ~ 1,
      result < spread_line & Proj_Margin < spread_line ~ 1,
      TRUE ~ 0
    ),
    AE_ATS_win = case_when(abs_error < vegas_abs_error ~ 1, TRUE ~ 0)
  )

WeekMeanAccuracyMetrics <- data.frame(
  week = as.numeric(nfl_week),
  games = nrow(LastWeekGames),
  VoA_mae = mean(LastWeekGames$VoA_ae),
  vegas_mae = mean(LastWeekGames$vegas_ae),
  VoA_mse = mean(LastWeekGames$VoA_se),
  vegas_mse = mean(LastWeekGames$vegas_se),
  VoA_rmse = rmse(LastWeekGames$result, LastWeekGames$Proj_Margin),
  vegas_rmse = rmse(LastWeekGames$result, LastWeekGames$spread_line),
  straight_up_win_pct = sum(LastWeekGames$straight_up_win) /
    nrow(LastWeekGames),
  vegas_straight_up_win_pct = sum(LastWeekGames$vegas_straight_up_win) /
    nrow(LastWeekGames),
  ATS_win_pct = sum(LastWeekGames$ATS_win) / nrow(LastWeekGames),
  AE_ATS_win_pct = sum(LastWeekGames$AE_ATS_win) / nrow(LastWeekGames)
)


if (as.numeric(nfl_week) == 1) {
  ### writing csv with individual games + accuracy metrics
  write_parquet(
    LastWeekGames,
    here(
      "Data",
      paste0("VoA", season),
      "AccuracyMetrics",
      "Games",
      paste0(
        nfl_text,
        "VoA",
        season,
        week_text,
        "1",
        week_text,
        nfl_week,
        "GameAccuracyMetrics.parquet"
      )
    )
  )
  ### writing csv with just weekly average calculated for accuracy metrics
  write_parquet(
    WeekMeanAccuracyMetrics,
    here(
      "Data",
      paste0("VoA", season),
      "AccuracyMetrics",
      paste0(
        nfl_text,
        "VoA",
        season,
        week_text,
        "1",
        week_text,
        nfl_week,
        "WeekAccuracyMetrics.parquet"
      )
    )
  )
} else if (as.numeric(nfl_week) >= 2) {
  ### reading in csv of previous games with error calculated, binding current week's games to that
  PrevWeekGameAccuracyMetrics <- read_parquet(here(
    "Data",
    paste0("VoA", season),
    "AccuracyMetrics",
    "Games",
    paste0(
      nfl_text,
      "VoA",
      season,
      week_text,
      "1",
      week_text,
      as.character(as.numeric(nfl_week) - 1),
      "GameAccuracyMetrics.parquet"
    )
  ))
  # if (is.na(colnames(PrevWeekGameAccuracyMetrics)[which(colnames(PrevWeekGameAccuracyMetrics) == "proj_margin")])){
  #   print("colnames all good")
  # } else{
  #   colnames(PrevWeekGameAccuracyMetrics)[which(colnames(PrevWeekGameAccuracyMetrics) == "proj_margin")] = "Proj_Margin"
  # }
  CompletedGames <- rbind(PrevWeekGameAccuracyMetrics, LastWeekGames)

  ### writing csv with individual games + accuracy metrics
  write_parquet(
    CompletedGames,
    here(
      "Data",
      paste0("VoA", season),
      "AccuracyMetrics",
      "Games",
      paste0(
        nfl_text,
        "VoA",
        season,
        week_text,
        "1",
        week_text,
        nfl_week,
        "GameAccuracyMetrics.parquet"
      )
    )
  )

  ### reading in csv of weekly average, binding current week to it
  PrevWeeklyAvgAccuracyMetrics <- read_parquet(here(
    "Data",
    paste0("VoA", season),
    "AccuracyMetrics",
    paste0(
      nfl_text,
      "VoA",
      season,
      week_text,
      "1",
      week_text,
      as.character(as.numeric(nfl_week) - 1),
      "WeekAccuracyMetrics.parquet"
    )
  ))
  CompletedWeeks <- rbind(PrevWeeklyAvgAccuracyMetrics, WeekMeanAccuracyMetrics)

  ### writing csv with just weekly average calculated for accuracy metrics
  write_parquet(
    CompletedWeeks,
    here(
      "Data",
      paste0("VoA", season),
      "AccuracyMetrics",
      paste0(
        nfl_text,
        "VoA",
        season,
        week_text,
        "1",
        week_text,
        nfl_week,
        "WeekAccuracyMetrics.parquet"
      )
    )
  )
} else {
  print("week not properly entered")
}


if (as.numeric(nfl_week) >= 6) {
  SeasonMetrics <- CompletedGames |>
    group_by(season) |>
    summarize(
      games = nrow(CompletedGames),
      VoA_mae = mean(VoA_ae),
      vegas_mae = mean(vegas_ae),
      VoA_mse = mean(VoA_se),
      vegas_mse = mean(vegas_se),
      VoA_rmse = rmse(result, Proj_Margin),
      vegas_rmse = rmse(result, spread_line),
      straight_up_win_pct = sum(straight_up_win) / nrow(CompletedGames),
      vegas_straight_up_win_pct = sum(vegas_straight_up_win) /
        nrow(CompletedGames),
      ATS_win_pct = sum(ATS_win) / nrow(CompletedGames),
      AE_ATS_win_pct = sum(AE_ATS_win) / nrow(CompletedGames)
    )

  write_csv(
    SeasonMetrics,
    here(
      "Data",
      paste0("VoA", season),
      "AccuracyMetrics",
      paste0(nfl_text, "VoA", season, "SeasonAccuracyMetrics.csv")
    )
  )
} else {
  print("no season metrics calculated yet")
}


if (as.numeric(nfl_week) >= 2) {
  VegasRMSECompPlot <- ggplot(CompletedWeeks, mapping = aes(x = week)) +
    theme_bw() +
    geom_point(mapping = aes(y = RMSE)) +
    geom_point(mapping = aes(y = vegas_RMSE), colour = 'blue') +
    geom_line(mapping = aes(y = RMSE)) +
    geom_line(mapping = aes(y = vegas_RMSE), colour = 'blue') +
    ggtitle(label = "RMSE (Vegas in blue) by Week") +
    xlab("Week") +
    theme(plot.title = element_text(hjust = 0.5))

  VegasRMSECompPlot

  VegasWinPctCompPlot <- ggplot(CompletedWeeks, mapping = aes(x = week)) +
    theme_bw() +
    geom_point(mapping = aes(y = straight_up_win_pct)) +
    geom_point(mapping = aes(y = vegas_straight_up_win_pct), colour = 'blue') +
    geom_line(mapping = aes(y = straight_up_win_pct)) +
    geom_line(mapping = aes(y = vegas_straight_up_win_pct), colour = 'blue') +
    ggtitle(label = "Correct Winner % (Vegas in blue) by Week") +
    xlab("Week") +
    ylab("Correct Win (%)") +
    theme(plot.title = element_text(hjust = 0.5))

  VegasWinPctCompPlot
}
# plot(CompletedWeeks$week, CompletedWeeks$RMSE, type = "l")
# plot(CompletedWeeks$vegas_RMSE, type = "l", add = T)
