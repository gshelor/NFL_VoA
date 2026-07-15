##### NFL Vortex of Accuracy Version 1.3 #####
### Supremely Excellent Yet Salaciously Godlike And Infallibly Magnificent Vortex of Accuracy
### This script is for rating NFL teams by unit (offense, defense, special teams) in order to create an overall Vortex of Accuracy rating, which is intended to represent the amount of points a given team would beat the hypothetical average NFL team by on a neutral field

##### loading packages #####
StartTime <- Sys.time()
library(pacman)
# fmt: skip
p_load(tidyverse, gt, nflfastR, nflverse, here, gtExtras, cmdstanr, ggpubr, webshot2, parallel, RColorBrewer, fastDummies, glmnet, data.table, arrow, lme4)

### running script which reads in functions used in data cleaning/model prep
source(here("Scripts", "R", "NFL_VoAFuncs.R"))

### Creating week and season strings
season <- readline(prompt = "What season is it? ")
nfl_week <- readline(prompt = "What week just occurred? ")


##### setting strings for table titles, file pathways, unintelligible charts #####
`%nin%` <- Negate(`%in%`)
output_dir <- here("Outputs", "RVoA", paste0("VoA", season))
VoP_output_dir <- here("Outputs", "RVoA", paste0("VoA", season), "VoP")
data_dir <- here("Data", paste("VoA", season, sep = ""))
tracking_chart_dir <- here("Data", paste0("VoA", season), "TrackingChartCSVs")
PY_data_dir <- here("Data", paste0("VoA", season), "PYData")
VoP_data_dir <- here("Data", paste0("VoA", season), "VoP")
Accuracy_data_dir <- here("Data", paste0("VoA", season), "AccuracyMetrics")
preseason_text <- "Preseason"
VoAString <- "VoA.csv"
week_text <- "Week"
fulltable_png <- "VoAFullTable.png"
VoA_text <- "Vortex of Accuracy"
Postseason_text <- "Postseason"
nfl_text <- "NFL"
Rating_text <- "_Ratings_Chart.png"
Ranking_text <- "_Rankings_Chart.png"
Histogram_text <- "_RatingHist.png"
Output_Rating_Plot_text <- "VoA Outputs vs VoA Ratings"
OffDef_Rating_Plot_text <- "VoA Off Rating vs VoA Def Rating"
Output_Rating_Plot_png <- "Output_Rating.png"
OffDef_Rating_Plot_png <- "OffDef_Rating.png"

hist_title <- paste(season, week_text, nfl_week, nfl_text, VoA_text, "Ratings")
# fmt: skip
Output_Rating_Plot_title <- paste(season, week_text, nfl_week, Output_Rating_Plot_text)
OffDef_Rating_Plot_title <- paste(
  season,
  week_text,
  nfl_week,
  OffDef_Rating_Plot_text
)
table_file_pathway <- paste(
  season,
  week_text,
  nfl_week,
  "_",
  fulltable_png,
  sep = ""
)
Output_filename <- paste(
  season,
  week_text,
  nfl_week,
  nfl_text,
  Rating_text,
  sep = ""
)
Ranking_filename <- paste(
  season,
  week_text,
  nfl_week,
  nfl_text,
  Ranking_text,
  sep = ""
)
hist_filename <- paste(
  season,
  week_text,
  nfl_week,
  "_",
  nfl_text,
  Histogram_text,
  sep = ""
)
Output_Rating_Plot_filename <- paste(
  season,
  week_text,
  nfl_week,
  "_",
  Output_Rating_Plot_png,
  sep = ""
)
OffDef_Rating_Plot_filename <- paste(
  season,
  week_text,
  nfl_week,
  "_",
  OffDef_Rating_Plot_png,
  sep = ""
)
### setting gt title based on whether it's after a playoff week or not
if (as.numeric(nfl_week) == 19) {
  gt_title <- paste(season, nfl_text, "Wildcard Round", VoA_text)
} else if (as.numeric(nfl_week) == 20) {
  gt_title <- paste(season, nfl_text, "Divisional Round", VoA_text)
} else if (as.numeric(nfl_week) == 21) {
  gt_title <- paste(season, nfl_text, "Conference Championship", VoA_text)
} else if (as.numeric(nfl_week) == 22) {
  gt_title <- paste(season, nfl_text, "Super Bowl", VoA_text)
} else if (as.numeric(nfl_week) == 0) {
  gt_title <- paste(season, preseason_text, nfl_text, VoA_text)
} else {
  gt_title <- paste(season, week_text, nfl_week, nfl_text, VoA_text)
}
### creating string for csv spreadsheet pathway
file_pathway <- paste(
  data_dir,
  "/",
  season,
  week_text,
  nfl_week,
  "_",
  VoAString,
  sep = ""
)
### creating directories that don't exist
# fmt: skip
for (i in c(data_dir, output_dir, tracking_chart_dir, PY_data_dir, VoP_data_dir, Accuracy_data_dir, VoP_output_dir)) {
  if (dir.exists(i) == FALSE) {
    dir.create(i, recursive = TRUE)
  }
}

### storing PYx numbers here whether I need them or not because for some reason the filter only works with specific numbers, not "functions" like "as.numeric(season) - 1" or something
### kinda annoying honestly
PY1 <- as.numeric(season) - 1
PY2 <- as.numeric(season) - 2
PY3 <- as.numeric(season) - 3
PY4 <- as.numeric(season) - 4
PY5 <- as.numeric(season) - 5

##### reading in data #####
if (as.numeric(nfl_week) == 0) {
  ##### Week 0 (Preseason) Data Pull #####
  ### reading in PBP data
  PY_PBP <- nflfastR::load_pbp(
    (as.numeric(season) - 5):(as.numeric(season) - 1)
  ) |>
    filter(
      play_type_nfl != "GAME_START" &
        play_type_nfl != "TIMEOUT" &
        play_type_nfl != "END_QUARTER" &
        play_type_nfl != "END_GAME"
    )
  ### extracting PY data by season
  PBP_PY1 <- PY_PBP |>
    filter(season == PY1)
  PBP_PY2 <- PY_PBP |>
    filter(season == PY2)
  PBP_PY3 <- PY_PBP |>
    filter(season == PY3)
  PBP_PY4 <- PY_PBP |>
    filter(season == PY4)
  PBP_PY5 <- PY_PBP |>
    filter(season == PY5)

  ### offensive and defensive plays
  PY1_rushpass_plays <- PBP_PY1 |>
    filter(play_type %in% c("run", "pass")) |>
    drop_na(epa) |>
    drop_na(yards_gained)
  PY1_success_plays <- PY1_rushpass_plays |>
    filter(play_type_nfl != "INTERCEPTION") |>
    filter(
      (down == 1 & (yards_gained >= (ydstogo / 2))) |
        (down == 2 & (yards_gained >= (ydstogo * 0.7))) |
        (down > 2 & (yards_gained >= ydstogo))
    )
  PY1_TDs <- PY1_rushpass_plays |>
    filter(
      touchdown == 1 &
        play_type_nfl != "INTERCEPTION" &
        play_type_nfl != "FUMBLE_RECOVERED_BY_OPPONENT" &
        fumble_lost == 0 &
        interception == 0
    )
  PY1_3rdDowns <- PY1_rushpass_plays |>
    filter(down == 3)
  PY1_4thDowns <- PY1_rushpass_plays |>
    filter(down == 4)
  PY1_passplays <- PY1_rushpass_plays |>
    filter(play_type == "pass")
  PY1_rushplays <- PY1_rushpass_plays |>
    filter(play_type == "run")
  PY1_scoringopp_plays <- PBP_PY1 |>
    filter(ydstogo <= 40) |>
    drop_na(drive)
  PY1_Turnovers <- PBP_PY1 |>
    filter(interception == 1 | fumble_lost == 1)
  PY1_2pts <- PBP_PY1 |>
    filter(play_type_nfl == "PAT2")
  ### special teams plays
  PY1_XPts <- PBP_PY1 |>
    filter(play_type == "extra_point")
  PY1_FGs <- PBP_PY1 |>
    filter(play_type == "field_goal")
  PY1_kickoffs <- PBP_PY1 |>
    filter(play_type == "kickoff")
  PY1_punts <- PBP_PY1 |>
    filter(play_type == "punt")

  ### PY2
  ### offensive and defensive plays
  PY2_rushpass_plays <- PBP_PY2 |>
    filter(play_type %in% c("run", "pass")) |>
    drop_na(epa) |>
    drop_na(yards_gained)
  PY2_success_plays <- PY2_rushpass_plays |>
    filter(play_type_nfl != "INTERCEPTION") |>
    filter(
      (down == 1 & (yards_gained >= (ydstogo / 2))) |
        (down == 2 & (yards_gained >= (ydstogo * 0.7))) |
        (down > 2 & (yards_gained >= ydstogo))
    )
  PY2_TDs <- PY2_rushpass_plays |>
    filter(
      touchdown == 1 &
        play_type_nfl != "INTERCEPTION" &
        play_type_nfl != "FUMBLE_RECOVERED_BY_OPPONENT" &
        fumble_lost == 0 &
        interception == 0
    )
  PY2_3rdDowns <- PY2_rushpass_plays |>
    filter(down == 3)
  PY2_4thDowns <- PY2_rushpass_plays |>
    filter(down == 4)
  PY2_passplays <- PY2_rushpass_plays |>
    filter(play_type == "pass")
  PY2_rushplays <- PY2_rushpass_plays |>
    filter(play_type == "run")
  PY2_scoringopp_plays <- PBP_PY2 |>
    filter(ydstogo <= 40) |>
    drop_na(drive)
  PY2_Turnovers <- PBP_PY2 |>
    filter(interception == 1 | fumble_lost == 1)
  PY2_2pts <- PBP_PY2 |>
    filter(play_type_nfl == "PAT2")
  ### special teams plays
  PY2_XPts <- PBP_PY2 |>
    filter(play_type == "extra_point")
  PY2_FGs <- PBP_PY2 |>
    filter(play_type == "field_goal")
  PY2_kickoffs <- PBP_PY2 |>
    filter(play_type == "kickoff")
  PY2_punts <- PBP_PY2 |>
    filter(play_type == "punt")

  ### PY3
  ### offensive and defensive plays
  PY3_rushpass_plays <- PBP_PY3 |>
    filter(play_type %in% c("run", "pass")) |>
    drop_na(epa) |>
    drop_na(yards_gained)
  PY3_success_plays <- PY3_rushpass_plays |>
    filter(play_type_nfl != "INTERCEPTION") |>
    filter(
      (down == 1 & (yards_gained >= (ydstogo / 2))) |
        (down == 2 & (yards_gained >= (ydstogo * 0.7))) |
        (down > 2 & (yards_gained >= ydstogo))
    )
  PY3_TDs <- PY3_rushpass_plays |>
    filter(
      touchdown == 1 &
        play_type_nfl != "INTERCEPTION" &
        play_type_nfl != "FUMBLE_RECOVERED_BY_OPPONENT" &
        fumble_lost == 0 &
        interception == 0
    )
  PY3_3rdDowns <- PY3_rushpass_plays |>
    filter(down == 3)
  PY3_4thDowns <- PY3_rushpass_plays |>
    filter(down == 4)
  PY3_passplays <- PY3_rushpass_plays |>
    filter(play_type == "pass")
  PY3_rushplays <- PY3_rushpass_plays |>
    filter(play_type == "run")
  PY3_scoringopp_plays <- PBP_PY3 |>
    filter(ydstogo <= 40) |>
    drop_na(drive)
  PY3_Turnovers <- PBP_PY3 |>
    filter(interception == 1 | fumble_lost == 1)
  PY3_2pts <- PBP_PY3 |>
    filter(play_type_nfl == "PAT2")
  ### special teams plays
  PY3_XPts <- PBP_PY3 |>
    filter(play_type == "extra_point")
  PY3_FGs <- PBP_PY3 |>
    filter(play_type == "field_goal")
  PY3_kickoffs <- PBP_PY3 |>
    filter(play_type == "kickoff")
  PY3_punts <- PBP_PY3 |>
    filter(play_type == "punt")

  ### PY4
  ### offensive and defensive plays
  PY4_rushpass_plays <- PBP_PY4 |>
    filter(play_type %in% c("run", "pass")) |>
    drop_na(epa) |>
    drop_na(yards_gained)
  PY4_success_plays <- PY4_rushpass_plays |>
    filter(play_type_nfl != "INTERCEPTION") |>
    filter(
      (down == 1 & (yards_gained >= (ydstogo / 2))) |
        (down == 2 & (yards_gained >= (ydstogo * 0.7))) |
        (down > 2 & (yards_gained >= ydstogo))
    )
  PY4_TDs <- PY4_rushpass_plays |>
    filter(
      touchdown == 1 &
        play_type_nfl != "INTERCEPTION" &
        play_type_nfl != "FUMBLE_RECOVERED_BY_OPPONENT" &
        fumble_lost == 0 &
        interception == 0
    )
  PY4_3rdDowns <- PY4_rushpass_plays |>
    filter(down == 3)
  PY4_4thDowns <- PY4_rushpass_plays |>
    filter(down == 4)
  PY4_passplays <- PY4_rushpass_plays |>
    filter(play_type == "pass")
  PY4_rushplays <- PY4_rushpass_plays |>
    filter(play_type == "run")
  PY4_scoringopp_plays <- PBP_PY4 |>
    filter(ydstogo <= 40) |>
    drop_na(drive)
  PY4_Turnovers <- PBP_PY4 |>
    filter(interception == 1 | fumble_lost == 1)
  PY4_2pts <- PBP_PY4 |>
    filter(play_type_nfl == "PAT2")
  ### special teams plays
  PY4_XPts <- PBP_PY4 |>
    filter(play_type == "extra_point")
  PY4_FGs <- PBP_PY4 |>
    filter(play_type == "field_goal")
  PY4_kickoffs <- PBP_PY4 |>
    filter(play_type == "kickoff")
  PY4_punts <- PBP_PY4 |>
    filter(play_type == "punt")

  ### PY5
  ### offensive and defensive plays
  PY5_rushpass_plays <- PBP_PY5 |>
    filter(play_type %in% c("run", "pass")) |>
    drop_na(epa) |>
    drop_na(yards_gained)
  PY5_success_plays <- PY5_rushpass_plays |>
    filter(play_type_nfl != "INTERCEPTION") |>
    filter(
      (down == 1 & (yards_gained >= (ydstogo / 2))) |
        (down == 2 & (yards_gained >= (ydstogo * 0.7))) |
        (down > 2 & (yards_gained >= ydstogo))
    )
  PY5_TDs <- PY5_rushpass_plays |>
    filter(
      touchdown == 1 &
        play_type_nfl != "INTERCEPTION" &
        play_type_nfl != "FUMBLE_RECOVERED_BY_OPPONENT" &
        fumble_lost == 0 &
        interception == 0
    )
  PY5_3rdDowns <- PY5_rushpass_plays |>
    filter(down == 3)
  PY5_4thDowns <- PY5_rushpass_plays |>
    filter(down == 4)
  PY5_passplays <- PY5_rushpass_plays |>
    filter(play_type == "pass")
  PY5_rushplays <- PY5_rushpass_plays |>
    filter(play_type == "run")
  PY5_scoringopp_plays <- PBP_PY5 |>
    filter(ydstogo <= 40) |>
    drop_na(drive)
  PY5_Turnovers <- PBP_PY5 |>
    filter(interception == 1 | fumble_lost == 1)
  PY5_2pts <- PBP_PY5 |>
    filter(play_type_nfl == "PAT2")
  ### special teams plays
  PY5_XPts <- PBP_PY5 |>
    filter(play_type == "extra_point")
  PY5_FGs <- PBP_PY5 |>
    filter(play_type == "field_goal")
  PY5_kickoffs <- PBP_PY5 |>
    filter(play_type == "kickoff")
  PY5_punts <- PBP_PY5 |>
    filter(play_type == "punt")

  ### creating dataframe to eventually store VoA Variables and ratings
  VoAVariables <- create_voa_vars(as.integer(nfl_week))
  ### creating dataframes which will be combined row-wise to make larger dataset for model training
  VoAVariablesTrain_PY1 <- create_voa_vars_train(PY1)
  VoAVariablesTrain_PY2 <- create_voa_vars_train(PY2)
  VoAVariablesTrain_PY3 <- create_voa_vars_train(PY3)
  VoAVariablesTrain_PY4 <- create_voa_vars_train(PY4)
  VoAVariablesTrain_PY5 <- create_voa_vars_train(PY5)
  print("temp break")
} else if (as.numeric(nfl_week) <= 2) {
  ##### Weeks 1-2 Data Pull #####
  ### reading in PY data saved in week 0
  PY_VoAVars <- read_parquet(here(
    "Data",
    paste0("VoA", season),
    "PYData",
    "PYData.parquet"
  )) |>
    select(
      team,
      ends_with("PY1"),
      ends_with("PY2")
    )

  ### reading in PBP data
  PBP <- nflfastR::load_pbp(as.numeric(season)) |>
    filter(
      play_type_nfl != "GAME_START" &
        play_type_nfl != "TIMEOUT" &
        play_type_nfl != "END_QUARTER" &
        play_type_nfl != "END_GAME"
    )

  ### separting PBP into categories for stat extraction later
  ### offensive and defensive plays
  rushpass_plays <- PBP |>
    filter(play_type %in% c("run", "pass")) |>
    drop_na(epa) |>
    drop_na(yards_gained)
  success_plays <- rushpass_plays |>
    filter(play_type_nfl != "INTERCEPTION") |>
    filter(
      (down == 1 & (yards_gained >= (ydstogo / 2))) |
        (down == 2 & (yards_gained >= (ydstogo * 0.7))) |
        (down > 2 & (yards_gained >= ydstogo))
    )
  TDs <- rushpass_plays |>
    filter(
      touchdown == 1 &
        play_type_nfl != "INTERCEPTION" &
        play_type_nfl != "FUMBLE_RECOVERED_BY_OPPONENT" &
        fumble_lost == 0 &
        interception == 0
    )
  ThirdDowns <- PBP |>
    filter(down == 3)
  FourthDowns <- PBP |>
    filter(down == 4)
  passplays <- rushpass_plays |>
    filter(play_type == "pass")
  rushplays <- rushpass_plays |>
    filter(play_type == "run")
  scoringopp_plays <- PBP |>
    filter(ydstogo <= 40) |>
    drop_na(drive)
  turnovers <- PBP |>
    filter(interception == 1 | fumble_lost == 1)
  TwoPts <- PBP |>
    filter(play_type_nfl == "PAT2")
  ### special teams plays
  XPts <- PBP |>
    filter(play_type == "extra_point")
  FGs <- PBP |>
    filter(play_type == "field_goal")
  Kickoffs <- PBP |>
    filter(play_type == "kickoff")
  Punts <- PBP |>
    filter(play_type == "punt")

  ### creating dataframe to eventually store VoA Variables and ratings
  VoAVariables <- create_voa_vars(as.integer(nfl_week))

  ### reading in completed games for error calculation
  # CompletedGames <- get_clean_games()
} else if (as.numeric(nfl_week) <= 10) {
  ##### Weeks 3-10 Data Pull #####
  ### reading in PY data saved in week 0
  PY_VoAVars <- read_csv(here(
    "Data",
    paste0("VoA", season),
    "PYData",
    "PYData.csv"
  )) |>
    select(team, ends_with("PY1"))

  ### reading in PBP data
  PBP <- nflfastR::load_pbp(as.numeric(season)) |>
    filter(
      play_type_nfl != "GAME_START" &
        play_type_nfl != "TIMEOUT" &
        play_type_nfl != "END_QUARTER" &
        play_type_nfl != "END_GAME"
    )

  ### separting PBP into categories for stat extraction later
  ### offensive and defensive plays
  rushpass_plays <- PBP |>
    filter(play_type %in% c("run", "pass")) |>
    drop_na(epa) |>
    drop_na(yards_gained)
  success_plays <- rushpass_plays |>
    filter(play_type_nfl != "INTERCEPTION") |>
    filter(
      (down == 1 & (yards_gained >= (ydstogo / 2))) |
        (down == 2 & (yards_gained >= (ydstogo * 0.7))) |
        (down > 2 & (yards_gained >= ydstogo))
    )
  TDs <- rushpass_plays |>
    filter(
      touchdown == 1 &
        play_type_nfl != "INTERCEPTION" &
        play_type_nfl != "FUMBLE_RECOVERED_BY_OPPONENT" &
        fumble_lost == 0 &
        interception == 0
    )
  ThirdDowns <- PBP |>
    filter(down == 3)
  FourthDowns <- PBP |>
    filter(down == 4)
  passplays <- rushpass_plays |>
    filter(play_type == "pass")
  rushplays <- rushpass_plays |>
    filter(play_type == "run")
  scoringopp_plays <- PBP |>
    filter(ydstogo <= 40) |>
    drop_na(drive)
  turnovers <- PBP |>
    filter(interception == 1 | fumble_lost == 1)
  TwoPts <- PBP |>
    filter(play_type_nfl == "PAT2")
  ### special teams plays
  XPts <- PBP |>
    filter(play_type == "extra_point")
  FGs <- PBP |>
    filter(play_type == "field_goal")
  Kickoffs <- PBP |>
    filter(play_type == "kickoff")
  Punts <- PBP |>
    filter(play_type == "punt")

  ### creating dataframe to eventually store VoA Variables and ratings
  VoAVariables <- create_voa_vars(as.integer(nfl_week))

  ### reading in completed games for error calculation
  # CompletedGames <- get_clean_games()
} else {
  ##### Week 11 - End of Season Data Pull #####
  ### reading in PBP data
  PBP <- nflfastR::load_pbp(as.numeric(season)) |>
    filter(
      play_type_nfl != "GAME_START" &
        play_type_nfl != "TIMEOUT" &
        play_type_nfl != "END_QUARTER" &
        play_type_nfl != "END_GAME"
    )

  ### separting PBP into categories for stat extraction later
  ### offensive and defensive plays
  rushpass_plays <- PBP |>
    filter(play_type %in% c("run", "pass")) |>
    drop_na(epa) |>
    drop_na(yards_gained)
  success_plays <- rushpass_plays |>
    filter(play_type_nfl != "INTERCEPTION") |>
    filter(
      (down == 1 & (yards_gained >= (ydstogo / 2))) |
        (down == 2 & (yards_gained >= (ydstogo * 0.7))) |
        (down > 2 & (yards_gained >= ydstogo))
    )
  TDs <- rushpass_plays |>
    filter(
      touchdown == 1 &
        play_type_nfl != "INTERCEPTION" &
        play_type_nfl != "FUMBLE_RECOVERED_BY_OPPONENT" &
        fumble_lost == 0 &
        interception == 0
    )
  ThirdDowns <- PBP |>
    filter(down == 3)
  FourthDowns <- PBP |>
    filter(down == 4)
  passplays <- rushpass_plays |>
    filter(play_type == "pass")
  rushplays <- rushpass_plays |>
    filter(play_type == "run")
  scoringopp_plays <- PBP |>
    filter(ydstogo <= 40) |>
    drop_na(drive)
  turnovers <- PBP |>
    filter(interception == 1 | fumble_lost == 1)
  TwoPts <- PBP |>
    filter(play_type_nfl == "PAT2")
  ### special teams plays
  XPts <- PBP |>
    filter(play_type == "extra_point")
  FGs <- PBP |>
    filter(play_type == "field_goal")
  Kickoffs <- PBP |>
    filter(play_type == "kickoff")
  Punts <- PBP |>
    filter(play_type == "punt")

  ### creating dataframe to eventually store VoA Variables and ratings
  VoAVariables <- create_voa_vars()

  ### reading in completed games for error calculation
  # CompletedGames <- get_clean_games()
}


##### Extracting Relevant Stats from PBP data #####
if (as.numeric(nfl_week) == 0) {
  ##### Week 0 (preseason) stat collection #####
  ### extracting stats for model training dfs (which will eventually be combined)
  ### function args: VoA_df,rushpass_plays, success_plays, ThirdDowns, FourthDowns, passplays, rushplays, scoringopp_plays, turnovers, TDs, TwoPts, FGs, Punts, Kickoffs, XPts
  VoAVariablesTrain_PY1 <- extract_pbp_stats(
    VoA_df = VoAVariablesTrain_PY1,
    rushpass_plays = PY1_rushpass_plays,
    success_plays = PY1_success_plays,
    ThirdDowns = PY1_3rdDowns,
    FourthDowns = PY1_4thDowns,
    passplays = PY1_passplays,
    rushplays = PY1_rushplays,
    scoringopp_plays = PY1_scoringopp_plays,
    turnovers = PY1_Turnovers,
    TDs = PY1_TDs,
    TwoPts = PY1_2pts,
    FGs = PY1_FGs,
    Punts = PY1_punts,
    Kickoffs = PY1_kickoffs,
    XPts = PY1_XPts
  )
  ### PY2
  VoAVariablesTrain_PY2 <- extract_pbp_stats(
    VoA_df = VoAVariablesTrain_PY2,
    rushpass_plays = PY2_rushpass_plays,
    success_plays = PY2_success_plays,
    ThirdDowns = PY2_3rdDowns,
    FourthDowns = PY2_4thDowns,
    passplays = PY2_passplays,
    rushplays = PY2_rushplays,
    scoringopp_plays = PY2_scoringopp_plays,
    turnovers = PY2_Turnovers,
    TDs = PY2_TDs,
    TwoPts = PY2_2pts,
    FGs = PY2_FGs,
    Punts = PY2_punts,
    Kickoffs = PY2_kickoffs,
    XPts = PY2_XPts
  )
  ### PY3
  VoAVariablesTrain_PY3 <- extract_pbp_stats(
    VoA_df = VoAVariablesTrain_PY3,
    rushpass_plays = PY3_rushpass_plays,
    success_plays = PY3_success_plays,
    ThirdDowns = PY3_3rdDowns,
    FourthDowns = PY3_4thDowns,
    passplays = PY3_passplays,
    rushplays = PY3_rushplays,
    scoringopp_plays = PY3_scoringopp_plays,
    turnovers = PY3_Turnovers,
    TDs = PY3_TDs,
    TwoPts = PY3_2pts,
    FGs = PY3_FGs,
    Punts = PY3_punts,
    Kickoffs = PY3_kickoffs,
    XPts = PY3_XPts
  )
  ### PY4
  VoAVariablesTrain_PY4 <- extract_pbp_stats(
    VoA_df = VoAVariablesTrain_PY4,
    rushpass_plays = PY4_rushpass_plays,
    success_plays = PY4_success_plays,
    ThirdDowns = PY4_3rdDowns,
    FourthDowns = PY4_4thDowns,
    passplays = PY4_passplays,
    rushplays = PY4_rushplays,
    scoringopp_plays = PY4_scoringopp_plays,
    turnovers = PY4_Turnovers,
    TDs = PY4_TDs,
    TwoPts = PY4_2pts,
    FGs = PY4_FGs,
    Punts = PY4_punts,
    Kickoffs = PY4_kickoffs,
    XPts = PY4_XPts
  )
  ### PY5
  VoAVariablesTrain_PY5 <- extract_pbp_stats(
    VoA_df = VoAVariablesTrain_PY5,
    rushpass_plays = PY5_rushpass_plays,
    success_plays = PY5_success_plays,
    ThirdDowns = PY5_3rdDowns,
    FourthDowns = PY5_4thDowns,
    passplays = PY5_passplays,
    rushplays = PY5_rushplays,
    scoringopp_plays = PY5_scoringopp_plays,
    turnovers = PY5_Turnovers,
    TDs = PY5_TDs,
    TwoPts = PY5_2pts,
    FGs = PY5_FGs,
    Punts = PY5_punts,
    Kickoffs = PY5_kickoffs,
    XPts = PY5_XPts
  )
  ### extracting stats for dataset which will be used for inference/creating current ratings
  VoAVariables <- extract_VoAVars_pbp_stats(
    VoA_df = VoAVariables,
    PY1_rushpass_plays = PY1_rushpass_plays,
    PY1_success_plays = PY1_success_plays,
    PY1_3rdDowns = PY1_3rdDowns,
    PY1_4thDowns = PY1_4thDowns,
    PY1_passplays = PY1_passplays,
    PY1_rushplays = PY1_rushplays,
    PY1_scoringopp_plays = PY1_scoringopp_plays,
    PY1_turnovers = PY1_Turnovers,
    PY1_TDs = PY1_TDs,
    PY1_2pts = PY1_2pts,
    PY1_FGs = PY1_FGs,
    PY1_Punts = PY1_punts,
    PY1_Kickoffs = PY1_kickoffs,
    PY1_XPts = PY1_XPts,
    ### PY2 PBP args
    PY2_rushpass_plays = PY2_rushpass_plays,
    PY2_success_plays = PY2_success_plays,
    PY2_3rdDowns = PY2_3rdDowns,
    PY2_4thDowns = PY2_4thDowns,
    PY2_passplays = PY2_passplays,
    PY2_rushplays = PY2_rushplays,
    PY2_scoringopp_plays = PY2_scoringopp_plays,
    PY2_turnovers = PY2_Turnovers,
    PY2_TDs = PY2_TDs,
    PY2_2pts = PY2_2pts,
    PY2_FGs = PY2_FGs,
    PY2_Punts = PY2_punts,
    PY2_Kickoffs = PY2_kickoffs,
    PY2_XPts = PY2_XPts,
    ### PY3 PBP args
    PY3_rushpass_plays = PY3_rushpass_plays,
    PY3_success_plays = PY3_success_plays,
    PY3_3rdDowns = PY3_3rdDowns,
    PY3_4thDowns = PY3_4thDowns,
    PY3_passplays = PY3_4thDowns,
    PY3_rushplays = PY3_rushplays,
    PY3_scoringopp_plays = PY3_scoringopp_plays,
    PY3_turnovers = PY3_Turnovers,
    PY3_TDs = PY3_TDs,
    PY3_2pts = PY3_2pts,
    PY3_FGs = PY3_FGs,
    PY3_Punts = PY3_punts,
    PY3_Kickoffs = PY3_kickoffs,
    PY3_XPts = PY3_XPts
  )

  ### writing parquet of PY data so that I can read it in for future weeks without needing to recreate it all
  write_parquet(
    VoAVariables,
    here("Data", paste0("VoA", season), "PYData", "PYData.parquet")
  )

  ### removing temp objects
  rm(list = ls(pattern = "^temp_"))
} else if (as.numeric(nfl_week) <= 10) {
  ##### Weeks 1-10 Stat Collection #####
  ### binding csv of PY data to VoA Variables, which should only contain current season data at this point
  VoA_Vars_dfs <- list(VoAVariables, PY_VoAVars)
  VoAVariables <- VoA_Vars_dfs |>
    reduce(full_join, by = "team") #|>

  ### Creating opponent-adjusted stats
  VoAVariables <- extract_pbp_stats(
    VoA_df = VoAVariables,
    rushpass_plays = rushpass_plays,
    success_plays = success_plays,
    ThirdDowns = ThirdDowns,
    FourthDowns = FourthDowns,
    passplays = passplays,
    rushplays = rushplays,
    scoringopp_plays = scoringopp_plays,
    turnovers = Turnovers,
    TDs = TDs,
    TwoPts = TwoPts,
    FGs = FGs,
    Punts = punts,
    Kickoffs = kickoffs,
    XPts = XPts
  )

  ### removing temp objects
  rm(list = ls(pattern = "^temp_"))
} else {
  ##### Week 11 - End of Season Stat Collection #####
  ### extracting pbp stats and calculating opponent-adjusted stats
  ### function args: VoA_df,rushpass_plays, success_plays, ThirdDowns, FourthDowns, passplays, rushplays, scoringopp_plays, turnovers, TDs, TwoPts, FGs, Punts, Kickoffs, XPts
  ### binding csv of PY data to VoA Variables, which should only contain current season data at this point
  VoAVariables <- extract_pbp_stats(
    VoA_df = VoAVariables,
    rushpass_plays = rushpass_plays,
    success_plays = success_plays,
    ThirdDowns = ThirdDowns,
    FourthDowns = FourthDowns,
    passplays = passplays,
    rushplays = rushplays,
    scoringopp_plays = scoringopp_plays,
    turnovers = Turnovers,
    TDs = TDs,
    TwoPts = TwoPts,
    FGs = FGs,
    Punts = punts,
    Kickoffs = kickoffs,
    XPts = XPts
  )

  ### removing temp objects
  rm(list = ls(pattern = "^temp_"))
}


##### Calculating Weighted Variables #####
if (as.numeric(nfl_week) == 0) {
  ##### Week 0 (Preseason) weighted variables calculation #####
  ### adding weighted variables to be used in Stan model later
  VoAVariables <- VoAVariables |>
    ### adding weighted variables (offense first)
    mutate(
      weighted_off_ypp = (adj_off_ypp_PY1 * 0.7) +
        (adj_off_ypp_PY2 * 0.25) +
        (adj_off_ypp_PY3 * 0.05),
      weighted_off_epa = (adj_off_epa_PY1 * 0.7) +
        (adj_off_epa_PY2 * 0.25) +
        (adj_off_epa_PY3 * 0.05),
      weighted_off_success_rt = (off_success_rt_PY1 * 0.7) +
        (off_success_rt_PY2 * 0.25) +
        (off_success_rt_PY3 * 0.05),
      weighted_off_explosiveness = (adj_off_explosiveness_PY1 * 0.7) +
        (adj_off_explosiveness_PY2 * 0.25) +
        (adj_off_explosiveness_PY3 * 0.05),
      weighted_off_third_conv_rate = (off_third_conv_rate_PY1 * 0.7) +
        (off_third_conv_rate_PY2 * 0.25) +
        (off_third_conv_rate_PY3 * 0.05),
      weighted_off_fourth_conv_rate = (off_fourth_conv_rate_PY1 * 0.7) +
        (off_fourth_conv_rate_PY2 * 0.25) +
        (off_fourth_conv_rate_PY3 * 0.05),
      weighted_off_pass_ypa = (off_pass_ypa_PY1 * 0.7) +
        (off_pass_ypa_PY2 * 0.25) +
        (off_pass_ypa_PY3 * 0.05),
      weighted_off_pass_ypc = (off_pass_ypc_PY1 * 0.7) +
        (off_pass_ypc_PY2 * 0.25) +
        (off_pass_ypc_PY3 * 0.05),
      weighted_off_rush_ypa = (off_rush_ypa_PY1 * 0.7) +
        (off_rush_ypa_PY2 * 0.25) +
        (off_rush_ypa_PY3 * 0.05),
      weighted_off_pts_per_opp = (off_pts_per_opp_PY1 * 0.7) +
        (off_pts_per_opp_PY2 * 0.25) +
        (off_pts_per_opp_PY3 * 0.05),
      weighted_off_turnovers = (off_turnovers_PY1 * 0.7) +
        (off_turnovers_PY2 * 0.25) +
        (off_turnovers_PY3 * 0.05),
      weighted_off_plays_pg = (off_plays_pg_PY1 * 0.7) +
        (off_plays_pg_PY2 * 0.25) +
        (off_plays_pg_PY3 * 0.05),
      weighted_off_ppg = (adj_off_ppg_PY1 * 0.7) +
        (adj_off_ppg_PY2 * 0.25) +
        (adj_off_ppg_PY3 * 0.05),
      ### weighted defensive stats now
      weighted_def_ypp = (adj_def_ypp_PY1 * 0.7) +
        (adj_def_ypp_PY2 * 0.25) +
        (adj_def_ypp_PY3 * 0.05),
      weighted_def_epa = (adj_def_epa_PY1 * 0.7) +
        (adj_def_epa_PY2 * 0.25) +
        (adj_def_epa_PY3 * 0.05),
      weighted_def_success_rt = (def_success_rt_PY1 * 0.7) +
        (def_success_rt_PY2 * 0.25) +
        (def_success_rt_PY3 * 0.05),
      weighted_def_explosiveness = (adj_def_explosiveness_PY1 * 0.7) +
        (adj_def_explosiveness_PY2 * 0.25) +
        (adj_def_explosiveness_PY3 * 0.05),
      weighted_def_third_conv_rate = (def_third_conv_rate_PY1 * 0.7) +
        (def_third_conv_rate_PY2 * 0.25) +
        (def_third_conv_rate_PY3 * 0.05),
      weighted_def_fourth_conv_rate = (def_fourth_conv_rate_PY1 * 0.7) +
        (def_fourth_conv_rate_PY2 * 0.25) +
        (def_fourth_conv_rate_PY3 * 0.05),
      weighted_def_pass_ypa = (def_pass_ypa_PY1 * 0.7) +
        (def_pass_ypa_PY2 * 0.25) +
        (def_pass_ypa_PY3 * 0.05),
      weighted_def_pass_ypc = (def_pass_ypc_PY1 * 0.7) +
        (def_pass_ypc_PY2 * 0.25) +
        (def_pass_ypc_PY3 * 0.05),
      weighted_def_rush_ypa = (def_rush_ypa_PY1 * 0.7) +
        (def_rush_ypa_PY2 * 0.25) +
        (def_rush_ypa_PY3 * 0.05),
      weighted_def_pts_per_opp = (def_pts_per_opp_PY1 * 0.7) +
        (def_pts_per_opp_PY2 * 0.25) +
        (def_pts_per_opp_PY3 * 0.05),
      weighted_def_turnovers = (def_turnovers_PY1 * 0.7) +
        (def_turnovers_PY2 * 0.25) +
        (def_turnovers_PY3 * 0.05),
      weighted_def_plays_pg = (def_plays_pg_PY1 * 0.7) +
        (def_plays_pg_PY2 * 0.25) +
        (def_plays_pg_PY3 * 0.05),
      weighted_def_ppg = (adj_def_ppg_PY1 * 0.7) +
        (adj_def_ppg_PY2 * 0.25) +
        (adj_def_ppg_PY3 * 0.05),
      ### weighted special teams stats now
      weighted_net_st_epa = (st_net_epa_PY1 * 0.7) +
        (st_net_epa_PY2 * 0.25) +
        (st_net_epa_PY3 * 0.25),
      weighted_net_punt_return_yds = ((st_punt_return_yds_PY1 -
        st_punt_return_yds_allowed_PY1) *
        0.7) +
        ((st_punt_return_yds_PY2 - st_punt_return_yds_allowed_PY2) * 0.25) +
        ((st_punt_return_yds_PY3 - st_punt_return_yds_allowed_PY3) * 0.05),
      weighted_net_kick_return_yds = ((st_kick_return_yds_PY1 -
        st_kick_return_yds_allowed_PY1) *
        0.7) +
        ((st_kick_return_yds_PY2 - st_kick_return_yds_allowed_PY2) * 0.25) +
        ((st_kick_return_yds_PY3 - st_kick_return_yds_allowed_PY3) * 0.05),
      weighted_net_punt_return_TDs = ((st_punt_return_TDs_PY1 -
        st_punt_return_TDs_allowed_PY1) *
        0.7) +
        ((st_punt_return_TDs_PY2 - st_punt_return_TDs_allowed_PY2) * 0.25) +
        ((st_punt_return_TDs_PY3 - st_punt_return_TDs_allowed_PY3) * 0.05),
      weighted_net_kick_return_TDs = ((st_kick_return_TDs_PY1 -
        st_kick_return_TDs_allowed_PY1) *
        0.7) +
        ((st_kick_return_TDs_PY2 - st_kick_return_TDs_allowed_PY2) * 0.25) +
        ((st_kick_return_TDs_PY3 - st_kick_return_TDs_allowed_PY3) * 0.05),
      weighted_net_fg_rate = ((fg_rate_PY1 - fg_rate_allowed_PY1) * 0.7) +
        ((fg_rate_PY2 - fg_rate_allowed_PY2) * 0.25) +
        ((fg_rate_PY3 - fg_rate_allowed_PY3) * 0.05),
      weighted_net_fg_made_pg = ((fg_made_pg_PY1 - fg_made_pg_allowed_PY1) *
        0.7) +
        ((fg_made_pg_PY2 - fg_made_pg_allowed_PY2) * 0.25) +
        ((fg_made_pg_PY3 - fg_made_pg_allowed_PY3) * 0.05),
      weighted_net_xp_rate = ((xp_rate_PY1 - xp_rate_allowed_PY1) * 0.7) +
        ((xp_rate_PY2 - xp_rate_allowed_PY2) * 0.25) +
        ((xp_rate_PY3 - xp_rate_allowed_PY3) * 0.05),
      weighted_net_xp_made_pg = ((xp_made_pg_PY1 - xp_made_pg_allowed_PY1) *
        0.7) +
        ((xp_made_pg_PY2 - xp_made_pg_allowed_PY2) * 0.25) +
        ((xp_made_pg_PY3 - xp_made_pg_allowed_PY3) * 0.05),
      weighted_net_st_ppg = (net_st_ppg_PY1 * 0.7) +
        (net_st_ppg_PY2 * 0.25) +
        (net_st_ppg_PY3 * 0.05),
      off_ppg_aboveavg = weighted_off_ppg - mean(weighted_off_ppg),
      def_ppg_aboveavg = weighted_def_ppg - mean(weighted_def_ppg)
    ) #,
  # off_ppg_adj = case_when(weighted_off_ppg > quantile(weighted_off_ppg, 0.8) ~ weighted_off_ppg + (off_ppg_aboveavg / 2),
  #                         weighted_off_ppg > mean(weighted_off_ppg) ~ weighted_off_ppg + (off_ppg_aboveavg / 5),
  #                         TRUE ~ weighted_off_ppg),
  # def_ppg_adj = case_when(weighted_def_ppg > quantile(weighted_def_ppg, 0.8) ~ weighted_def_ppg + (def_ppg_aboveavg / 2),
  #                         weighted_def_ppg > mean(weighted_def_ppg) ~ weighted_def_ppg + (def_ppg_aboveavg / 5),
  #                         TRUE ~ weighted_def_ppg)))
} else if (as.numeric(nfl_week) <= 2) {
  ##### Weeks 1-2 weighted variable calculation #####
  ### Adding columns of variables weighted by season
  ### adding weighted variables (offense first)
  VoAVariables <- VoAVariables |>
    mutate(
      weighted_off_ypp = (adj_off_ypp_PY1 * 0.6) +
        (adj_off_ypp_PY2 * 0.05) +
        (adj_off_ypp * 0.35),
      weighted_off_epa = (adj_off_epa_PY1 * 0.6) +
        (adj_off_epa_PY2 * 0.05) +
        (adj_off_epa * 0.35),
      weighted_off_success_rt = (off_success_rt_PY1 * 0.6) +
        (off_success_rt_PY2 * 0.05) +
        (off_success_rt * 0.35),
      weighted_off_explosiveness = (adj_off_explosiveness_PY1 * 0.6) +
        (adj_off_explosiveness_PY2 * 0.05) +
        (adj_off_explosiveness * 0.35),
      weighted_off_third_conv_rate = (off_third_conv_rate_PY1 * 0.6) +
        (off_third_conv_rate_PY2 * 0.05) +
        (off_third_conv_rate * 0.35),
      weighted_off_fourth_conv_rate = (off_fourth_conv_rate_PY1 * 0.6) +
        (off_fourth_conv_rate_PY2 * 0.05) +
        (off_fourth_conv_rate * 0.35),
      weighted_off_pass_ypa = (off_pass_ypa_PY1 * 0.6) +
        (off_pass_ypa_PY2 * 0.05) +
        (off_pass_ypa * 0.35),
      weighted_off_pass_ypc = (off_pass_ypc_PY1 * 0.6) +
        (off_pass_ypc_PY2 * 0.05) +
        (off_pass_ypc * 0.35),
      weighted_off_rush_ypa = (off_rush_ypa_PY1 * 0.6) +
        (off_rush_ypa_PY2 * 0.05) +
        (off_rush_ypa * 0.35),
      weighted_off_pts_per_opp = (off_pts_per_opp_PY1 * 0.6) +
        (off_pts_per_opp_PY2 * 0.05) +
        (off_pts_per_opp * 0.35),
      weighted_off_turnovers = (off_turnovers_PY1 * 0.6) +
        (off_turnovers_PY2 * 0.05) +
        (off_turnovers * 0.35),
      weighted_off_plays_pg = (off_plays_pg_PY1 * 0.6) +
        (off_plays_pg_PY2 * 0.05) +
        (off_plays_pg * 0.35),
      weighted_off_ppg = (adj_off_ppg_PY1 * 0.6) +
        (adj_off_ppg_PY2 * 0.05) +
        (adj_off_ppg * 0.35),
      ### weighted defensive stats now
      weighted_def_ypp = (adj_def_ypp_PY1 * 0.6) +
        (adj_def_ypp_PY2 * 0.05) +
        (adj_def_ypp * 0.35),
      weighted_def_epa = (adj_def_epa_PY1 * 0.6) +
        (adj_def_epa_PY2 * 0.05) +
        (adj_def_epa * 0.35),
      weighted_def_success_rt = (def_success_rt_PY1 * 0.6) +
        (def_success_rt_PY2 * 0.05) +
        (def_success_rt * 0.35),
      weighted_def_explosiveness = (adj_def_explosiveness_PY1 * 0.6) +
        (adj_def_explosiveness_PY2 * 0.05) +
        (adj_def_explosiveness * 0.35),
      weighted_def_third_conv_rate = (def_third_conv_rate_PY1 * 0.6) +
        (def_third_conv_rate_PY2 * 0.05) +
        (def_third_conv_rate * 0.35),
      weighted_def_fourth_conv_rate = (def_fourth_conv_rate_PY1 * 0.6) +
        (def_fourth_conv_rate_PY2 * 0.05) +
        (def_fourth_conv_rate * 0.35),
      weighted_def_pass_ypa = (def_pass_ypa_PY1 * 0.6) +
        (def_pass_ypa_PY2 * 0.05) +
        (def_pass_ypa * 0.35),
      weighted_def_pass_ypc = (def_pass_ypc_PY1 * 0.6) +
        (def_pass_ypc_PY2 * 0.05) +
        (def_pass_ypc * 0.35),
      weighted_def_rush_ypa = (def_rush_ypa_PY1 * 0.6) +
        (def_rush_ypa_PY2 * 0.05) +
        (def_rush_ypa * 0.35),
      weighted_def_pts_per_opp = (def_pts_per_opp_PY1 * 0.6) +
        (def_pts_per_opp_PY2 * 0.05) +
        (def_pts_per_opp * 0.35),
      weighted_def_turnovers = (def_turnovers_PY1 * 0.6) +
        (def_turnovers_PY2 * 0.05) +
        (def_turnovers * 0.35),
      weighted_def_plays_pg = (def_plays_pg_PY1 * 0.6) +
        (def_plays_pg_PY2 * 0.05) +
        (def_plays_pg * 0.35),
      weighted_def_ppg = (adj_def_ppg_PY1 * 0.6) +
        (adj_def_ppg_PY2 * 0.05) +
        (adj_def_ppg * 0.35),
      ### weighted special teams stats now
      weighted_net_st_epa = (st_net_epa_PY1 * 0.6) +
        (st_net_epa_PY2 * 0.05) +
        (st_net_epa * 0.35),
      weighted_net_punt_return_yds = ((st_punt_return_yds_PY1 -
        st_punt_return_yds_allowed_PY1) *
        0.6) +
        ((st_punt_return_yds_PY2 - st_punt_return_yds_allowed_PY2) * 0.05) +
        ((st_punt_return_yds - st_punt_return_yds_allowed) * 0.35),
      weighted_net_kick_return_yds = ((st_kick_return_yds_PY1 -
        st_kick_return_yds_allowed_PY1) *
        0.6) +
        ((st_kick_return_yds_PY2 - st_kick_return_yds_allowed_PY2) * 0.05) +
        ((st_kick_return_yds - st_kick_return_yds_allowed) * 0.35),
      weighted_net_punt_return_TDs = ((st_punt_return_TDs_PY1 -
        st_punt_return_TDs_allowed_PY1) *
        0.6) +
        ((st_punt_return_TDs_PY2 - st_punt_return_TDs_allowed_PY2) * 0.05) +
        ((st_punt_return_TDs - st_punt_return_TDs_allowed) * 0.35),
      weighted_net_kick_return_TDs = ((st_kick_return_TDs_PY1 -
        st_kick_return_TDs_allowed_PY1) *
        0.6) +
        ((st_kick_return_TDs_PY2 - st_kick_return_TDs_allowed_PY2) * 0.05) +
        ((st_kick_return_TDs - st_kick_return_TDs_allowed) * 0.35),
      weighted_net_fg_rate = ((fg_rate_PY1 - fg_rate_allowed_PY1) * 0.6) +
        ((fg_rate_PY2 - fg_rate_allowed_PY2) * 0.05) +
        ((fg_rate - fg_rate_allowed) * 0.35),
      weighted_net_fg_made_pg = ((fg_made_pg_PY1 - fg_made_pg_allowed_PY1) *
        0.6) +
        ((fg_made_pg_PY2 - fg_made_pg_allowed_PY2) * 0.05) +
        ((fg_made_pg - fg_made_pg_allowed) * 0.35),
      weighted_net_xp_rate = ((xp_rate_PY1 - xp_rate_allowed_PY1) * 0.6) +
        ((xp_rate_PY2 - xp_rate_allowed_PY2) * 0.05) +
        ((xp_rate - xp_rate_allowed) * 0.35),
      weighted_net_xp_made_pg = ((xp_made_pg_PY1 - xp_made_pg_allowed_PY1) *
        0.6) +
        ((xp_made_pg_PY2 - xp_made_pg_allowed_PY2) * 0.05) +
        ((xp_made_pg - xp_made_pg_allowed) * 0.35),
      weighted_net_st_ppg = (net_st_ppg_PY1 * 0.6) +
        (net_st_ppg_PY2 * 0.05) +
        (net_st_ppg * 0.35),
      off_ppg_aboveavg = weighted_off_ppg - mean(weighted_off_ppg),
      def_ppg_aboveavg = weighted_def_ppg - mean(weighted_def_ppg)
    ) #,
  # off_ppg_adj = case_when(weighted_off_ppg > quantile(weighted_off_ppg, 0.8) ~ weighted_off_ppg + (off_ppg_aboveavg / 2),
  #                         weighted_off_ppg > mean(weighted_off_ppg) ~ weighted_off_ppg + (off_ppg_aboveavg / 5),
  #                         TRUE ~ weighted_off_ppg),
  # def_ppg_adj = case_when(weighted_def_ppg > quantile(weighted_def_ppg, 0.8) ~ weighted_def_ppg + (def_ppg_aboveavg / 2),
  #                         weighted_def_ppg > mean(weighted_def_ppg) ~ weighted_def_ppg + (def_ppg_aboveavg / 5),
  #                         TRUE ~ weighted_def_ppg)))
} else if (as.numeric(nfl_week) <= 4) {
  ##### Weeks 3-4 weighted variable calculation #####
  ### Adding columns of variables weighted by season
  ### adding weighted variables (offense first)
  VoAVariables <- VoAVariables |>
    mutate(
      weighted_off_ypp = (adj_off_ypp_PY1 * 0.6) + (adj_off_ypp * 0.4),
      weighted_off_epa = (adj_off_epa_PY1 * 0.6) + (adj_off_epa * 0.4),
      weighted_off_success_rt = (off_success_rt_PY1 * 0.6) +
        (off_success_rt * 0.4),
      weighted_off_explosiveness = (adj_off_explosiveness_PY1 * 0.6) +
        (adj_off_explosiveness * 0.4),
      weighted_off_third_conv_rate = (off_third_conv_rate_PY1 * 0.6) +
        (off_third_conv_rate * 0.4),
      weighted_off_fourth_conv_rate = (off_fourth_conv_rate_PY1 * 0.6) +
        (off_fourth_conv_rate * 0.4),
      weighted_off_pass_ypa = (off_pass_ypa_PY1 * 0.6) + (off_pass_ypa * 0.4),
      weighted_off_pass_ypc = (off_pass_ypc_PY1 * 0.6) + (off_pass_ypc * 0.4),
      weighted_off_rush_ypa = (off_rush_ypa_PY1 * 0.6) + (off_rush_ypa * 0.4),
      weighted_off_pts_per_opp = (off_pts_per_opp_PY1 * 0.6) +
        (off_pts_per_opp * 0.4),
      weighted_off_turnovers = (off_turnovers_PY1 * 0.6) +
        (off_turnovers * 0.4),
      weighted_off_plays_pg = (off_plays_pg_PY1 * 0.6) + (off_plays_pg * 0.4),
      weighted_off_ppg = (adj_off_ppg_PY1 * 0.6) + (adj_off_ppg * 0.4),
      ### weighted defensive stats now
      weighted_def_ypp = (adj_def_ypp_PY1 * 0.6) + (adj_def_ypp * 0.4),
      weighted_def_epa = (adj_def_epa_PY1 * 0.6) + (adj_def_epa * 0.4),
      weighted_def_success_rt = (def_success_rt_PY1 * 0.6) +
        (def_success_rt * 0.4),
      weighted_def_explosiveness = (adj_def_explosiveness_PY1 * 0.6) +
        (adj_def_explosiveness * 0.4),
      weighted_def_third_conv_rate = (def_third_conv_rate_PY1 * 0.6) +
        (def_third_conv_rate * 0.4),
      weighted_def_fourth_conv_rate = (def_fourth_conv_rate_PY1 * 0.6) +
        (def_fourth_conv_rate * 0.4),
      weighted_def_pass_ypa = (def_pass_ypa_PY1 * 0.6) + (def_pass_ypa * 0.4),
      weighted_def_pass_ypc = (def_pass_ypc_PY1 * 0.6) + (def_pass_ypc * 0.4),
      weighted_def_rush_ypa = (def_rush_ypa_PY1 * 0.6) + (def_rush_ypa * 0.4),
      weighted_def_pts_per_opp = (def_pts_per_opp_PY1 * 0.6) +
        (def_pts_per_opp * 0.4),
      weighted_def_turnovers = (def_turnovers_PY1 * 0.6) +
        (def_turnovers * 0.4),
      weighted_def_plays_pg = (def_plays_pg_PY1 * 0.6) + (def_plays_pg * 0.4),
      weighted_def_ppg = (adj_def_ppg_PY1 * 0.6) + (adj_def_ppg * 0.4),
      ### weighted special teams stats now
      weighted_net_st_epa = (st_net_epa_PY1 * 0.6) + (st_net_epa * 0.4),
      weighted_net_punt_return_yds = ((st_punt_return_yds_PY1 -
        st_punt_return_yds_allowed_PY1) *
        0.6) +
        ((st_punt_return_yds - st_punt_return_yds_allowed) * 0.4),
      weighted_net_kick_return_yds = ((st_kick_return_yds_PY1 -
        st_kick_return_yds_allowed_PY1) *
        0.6) +
        ((st_kick_return_yds - st_kick_return_yds_allowed) * 0.4),
      weighted_net_punt_return_TDs = ((st_punt_return_TDs_PY1 -
        st_punt_return_TDs_allowed_PY1) *
        0.6) +
        ((st_punt_return_TDs - st_punt_return_TDs_allowed) * 0.4),
      weighted_net_kick_return_TDs = ((st_kick_return_TDs_PY1 -
        st_kick_return_TDs_allowed_PY1) *
        0.6) +
        ((st_kick_return_TDs - st_kick_return_TDs_allowed) * 0.4),
      weighted_net_fg_rate = ((fg_rate_PY1 - fg_rate_allowed_PY1) * 0.6) +
        ((fg_rate - fg_rate_allowed) * 0.4),
      weighted_net_fg_made_pg = ((fg_made_pg_PY1 - fg_made_pg_allowed_PY1) *
        0.6) +
        ((fg_made_pg - fg_made_pg_allowed) * 0.4),
      weighted_net_xp_rate = ((xp_rate_PY1 - xp_rate_allowed_PY1) * 0.6) +
        ((xp_rate - xp_rate_allowed) * 0.4),
      weighted_net_xp_made_pg = ((xp_made_pg_PY1 - xp_made_pg_allowed_PY1) *
        0.6) +
        ((xp_made_pg - xp_made_pg_allowed) * 0.4),
      weighted_net_st_ppg = (net_st_ppg_PY1 * 0.6) + (net_st_ppg * 0.4),
      off_ppg_aboveavg = weighted_off_ppg - mean(weighted_off_ppg),
      def_ppg_aboveavg = weighted_def_ppg - mean(weighted_def_ppg)
    ) #,
  # off_ppg_adj = case_when(weighted_off_ppg > quantile(weighted_off_ppg, 0.8) ~ weighted_off_ppg + (off_ppg_aboveavg / 2),
  #                         weighted_off_ppg > mean(weighted_off_ppg) ~ weighted_off_ppg + (off_ppg_aboveavg / 5),
  #                         TRUE ~ weighted_off_ppg),
  # def_ppg_adj = case_when(weighted_def_ppg > quantile(weighted_def_ppg, 0.8) ~ weighted_def_ppg + (def_ppg_aboveavg / 2),
  #                         weighted_def_ppg > mean(weighted_def_ppg) ~ weighted_def_ppg + (def_ppg_aboveavg / 5),
  #                         TRUE ~ weighted_def_ppg)))
} else if (as.numeric(nfl_week) == 5) {
  ##### Week 5 weighted Variable calculation #####
  ### Adding columns of variables weighted by season
  ### adding weighted variables (offense first)
  VoAVariables <- VoAVariables |>
    mutate(
      weighted_off_ypp = (adj_off_ypp_PY1 * 0.5) + (adj_off_ypp * 0.5),
      weighted_off_epa = (adj_off_epa_PY1 * 0.5) + (adj_off_epa * 0.5),
      weighted_off_success_rt = (off_success_rt_PY1 * 0.5) +
        (off_success_rt * 0.5),
      weighted_off_explosiveness = (adj_off_explosiveness_PY1 * 0.5) +
        (adj_off_explosiveness * 0.5),
      weighted_off_third_conv_rate = (off_third_conv_rate_PY1 * 0.5) +
        (off_third_conv_rate * 0.5),
      weighted_off_fourth_conv_rate = (off_fourth_conv_rate_PY1 * 0.5) +
        (off_fourth_conv_rate * 0.5),
      weighted_off_pass_ypa = (off_pass_ypa_PY1 * 0.5) + (off_pass_ypa * 0.5),
      weighted_off_pass_ypc = (off_pass_ypc_PY1 * 0.5) + (off_pass_ypc * 0.5),
      weighted_off_rush_ypa = (off_rush_ypa_PY1 * 0.5) + (off_rush_ypa * 0.5),
      weighted_off_pts_per_opp = (off_pts_per_opp_PY1 * 0.5) +
        (off_pts_per_opp * 0.5),
      weighted_off_turnovers = (off_turnovers_PY1 * 0.5) +
        (off_turnovers * 0.5),
      weighted_off_plays_pg = (off_plays_pg_PY1 * 0.5) + (off_plays_pg * 0.5),
      weighted_off_ppg = (adj_off_ppg_PY1 * 0.5) + (adj_off_ppg * 0.5),
      ### weighted defensive stats now
      weighted_def_ypp = (adj_def_ypp_PY1 * 0.5) + (adj_def_ypp * 0.5),
      weighted_def_epa = (adj_def_epa_PY1 * 0.5) + (adj_def_epa * 0.5),
      weighted_def_success_rt = (def_success_rt_PY1 * 0.5) +
        (def_success_rt * 0.5),
      weighted_def_explosiveness = (adj_def_explosiveness_PY1 * 0.5) +
        (adj_def_explosiveness * 0.5),
      weighted_def_third_conv_rate = (def_third_conv_rate_PY1 * 0.5) +
        (def_third_conv_rate * 0.5),
      weighted_def_fourth_conv_rate = (def_fourth_conv_rate_PY1 * 0.5) +
        (def_fourth_conv_rate * 0.5),
      weighted_def_pass_ypa = (def_pass_ypa_PY1 * 0.5) + (def_pass_ypa * 0.5),
      weighted_def_pass_ypc = (def_pass_ypc_PY1 * 0.5) + (def_pass_ypc * 0.5),
      weighted_def_rush_ypa = (def_rush_ypa_PY1 * 0.5) + (def_rush_ypa * 0.5),
      weighted_def_pts_per_opp = (def_pts_per_opp_PY1 * 0.5) +
        (def_pts_per_opp * 0.5),
      weighted_def_turnovers = (def_turnovers_PY1 * 0.5) +
        (def_turnovers * 0.5),
      weighted_def_plays_pg = (def_plays_pg_PY1 * 0.5) + (def_plays_pg * 0.5),
      weighted_def_ppg = (adj_def_ppg_PY1 * 0.5) + (adj_def_ppg * 0.5),
      ### weighted special teams stats now
      weighted_net_st_epa = (st_net_epa_PY1 * 0.5) + (st_net_epa * 0.5),
      weighted_net_punt_return_yds = ((st_punt_return_yds_PY1 -
        st_punt_return_yds_allowed_PY1) *
        0.5) +
        ((st_punt_return_yds - st_punt_return_yds_allowed) * 0.5),
      weighted_net_kick_return_yds = ((st_kick_return_yds_PY1 -
        st_kick_return_yds_allowed_PY1) *
        0.5) +
        ((st_kick_return_yds - st_kick_return_yds_allowed) * 0.5),
      weighted_net_punt_return_TDs = ((st_punt_return_TDs_PY1 -
        st_punt_return_TDs_allowed_PY1) *
        0.5) +
        ((st_punt_return_TDs - st_punt_return_TDs_allowed) * 0.5),
      weighted_net_kick_return_TDs = ((st_kick_return_TDs_PY1 -
        st_kick_return_TDs_allowed_PY1) *
        0.5) +
        ((st_kick_return_TDs - st_kick_return_TDs_allowed) * 0.5),
      weighted_net_fg_rate = ((fg_rate_PY1 - fg_rate_allowed_PY1) * 0.5) +
        ((fg_rate - fg_rate_allowed) * 0.5),
      weighted_net_fg_made_pg = ((fg_made_pg_PY1 - fg_made_pg_allowed_PY1) *
        0.5) +
        ((fg_made_pg - fg_made_pg_allowed) * 0.5),
      weighted_net_xp_rate = ((xp_rate_PY1 - xp_rate_allowed_PY1) * 0.5) +
        ((xp_rate - xp_rate_allowed) * 0.5),
      weighted_net_xp_made_pg = ((xp_made_pg_PY1 - xp_made_pg_allowed_PY1) *
        0.5) +
        ((xp_made_pg - xp_made_pg_allowed) * 0.5),
      weighted_net_st_ppg = (net_st_ppg_PY1 * 0.5) + (net_st_ppg * 0.5),
      off_ppg_aboveavg = weighted_off_ppg - mean(weighted_off_ppg),
      def_ppg_aboveavg = weighted_def_ppg - mean(weighted_def_ppg)
    ) #,
  # off_ppg_adj = case_when(weighted_off_ppg > quantile(weighted_off_ppg, 0.8) ~ weighted_off_ppg + (off_ppg_aboveavg / 2),
  #                         weighted_off_ppg > mean(weighted_off_ppg) ~ weighted_off_ppg + (off_ppg_aboveavg / 5),
  #                         TRUE ~ weighted_off_ppg),
  # def_ppg_adj = case_when(weighted_def_ppg > quantile(weighted_def_ppg, 0.8) ~ weighted_def_ppg + (def_ppg_aboveavg / 2),
  #                         weighted_def_ppg > mean(weighted_def_ppg) ~ weighted_def_ppg + (def_ppg_aboveavg / 5),
  #                         TRUE ~ weighted_def_ppg)))
} else if (as.numeric(nfl_week) == 6) {
  ##### Week 6 weighted variable calculation #####
  ### Adding columns of variables weighted by season
  ### adding weighted variables (offense first)
  VoAVariables <- VoAVariables |>
    mutate(
      weighted_off_ypp = (adj_off_ypp_PY1 * 0.4) + (adj_off_ypp * 0.6),
      weighted_off_epa = (adj_off_epa_PY1 * 0.4) + (adj_off_epa * 0.6),
      weighted_off_success_rt = (off_success_rt_PY1 * 0.4) +
        (off_success_rt * 0.6),
      weighted_off_explosiveness = (adj_off_explosiveness_PY1 * 0.4) +
        (adj_off_explosiveness * 0.6),
      weighted_off_third_conv_rate = (off_third_conv_rate_PY1 * 0.4) +
        (off_third_conv_rate * 0.6),
      weighted_off_fourth_conv_rate = (off_fourth_conv_rate_PY1 * 0.4) +
        (off_fourth_conv_rate * 0.6),
      weighted_off_pass_ypa = (off_pass_ypa_PY1 * 0.4) + (off_pass_ypa * 0.6),
      weighted_off_pass_ypc = (off_pass_ypc_PY1 * 0.4) + (off_pass_ypc * 0.6),
      weighted_off_rush_ypa = (off_rush_ypa_PY1 * 0.4) + (off_rush_ypa * 0.6),
      weighted_off_pts_per_opp = (off_pts_per_opp_PY1 * 0.4) +
        (off_pts_per_opp * 0.6),
      weighted_off_turnovers = (off_turnovers_PY1 * 0.4) +
        (off_turnovers * 0.6),
      weighted_off_plays_pg = (off_plays_pg_PY1 * 0.4) + (off_plays_pg * 0.6),
      weighted_off_ppg = (adj_off_ppg_PY1 * 0.4) + (adj_off_ppg * 0.6),
      ### weighted defensive stats now
      weighted_def_ypp = (adj_def_ypp_PY1 * 0.4) + (adj_def_ypp * 0.6),
      weighted_def_epa = (adj_def_epa_PY1 * 0.4) + (adj_def_epa * 0.6),
      weighted_def_success_rt = (def_success_rt_PY1 * 0.4) +
        (def_success_rt * 0.6),
      weighted_def_explosiveness = (adj_def_explosiveness_PY1 * 0.4) +
        (adj_def_explosiveness * 0.6),
      weighted_def_third_conv_rate = (def_third_conv_rate_PY1 * 0.4) +
        (def_third_conv_rate * 0.6),
      weighted_def_fourth_conv_rate = (def_fourth_conv_rate_PY1 * 0.4) +
        (def_fourth_conv_rate * 0.6),
      weighted_def_pass_ypa = (def_pass_ypa_PY1 * 0.4) + (def_pass_ypa * 0.6),
      weighted_def_pass_ypc = (def_pass_ypc_PY1 * 0.4) + (def_pass_ypc * 0.6),
      weighted_def_rush_ypa = (def_rush_ypa_PY1 * 0.4) + (def_rush_ypa * 0.6),
      weighted_def_pts_per_opp = (def_pts_per_opp_PY1 * 0.4) +
        (def_pts_per_opp * 0.6),
      weighted_def_turnovers = (def_turnovers_PY1 * 0.4) +
        (def_turnovers * 0.6),
      weighted_def_plays_pg = (def_plays_pg_PY1 * 0.4) + (def_plays_pg * 0.6),
      weighted_def_ppg = (adj_def_ppg_PY1 * 0.4) + (adj_def_ppg * 0.6),
      ### weighted special teams stats now
      weighted_net_st_epa = (st_net_epa_PY1 * 0.4) + (st_net_epa * 0.6),
      weighted_net_punt_return_yds = ((st_punt_return_yds_PY1 -
        st_punt_return_yds_allowed_PY1) *
        0.4) +
        ((st_punt_return_yds - st_punt_return_yds_allowed) * 0.6),
      weighted_net_kick_return_yds = ((st_kick_return_yds_PY1 -
        st_kick_return_yds_allowed_PY1) *
        0.4) +
        ((st_kick_return_yds - st_kick_return_yds_allowed) * 0.6),
      weighted_net_punt_return_TDs = ((st_punt_return_TDs_PY1 -
        st_punt_return_TDs_allowed_PY1) *
        0.4) +
        ((st_punt_return_TDs - st_punt_return_TDs_allowed) * 0.6),
      weighted_net_kick_return_TDs = ((st_kick_return_TDs_PY1 -
        st_kick_return_TDs_allowed_PY1) *
        0.4) +
        ((st_kick_return_TDs - st_kick_return_TDs_allowed) * 0.6),
      weighted_net_fg_rate = ((fg_rate_PY1 - fg_rate_allowed_PY1) * 0.4) +
        ((fg_rate - fg_rate_allowed) * 0.6),
      weighted_net_fg_made_pg = ((fg_made_pg_PY1 - fg_made_pg_allowed_PY1) *
        0.4) +
        ((fg_made_pg - fg_made_pg_allowed) * 0.6),
      weighted_net_xp_rate = ((xp_rate_PY1 - xp_rate_allowed_PY1) * 0.4) +
        ((xp_rate - xp_rate_allowed) * 0.6),
      weighted_net_xp_made_pg = ((xp_made_pg_PY1 - xp_made_pg_allowed_PY1) *
        0.4) +
        ((xp_made_pg - xp_made_pg_allowed) * 0.6),
      weighted_net_st_ppg = (net_st_ppg_PY1 * 0.4) + (net_st_ppg * 0.6),
      off_ppg_aboveavg = weighted_off_ppg - mean(weighted_off_ppg),
      def_ppg_aboveavg = weighted_def_ppg - mean(weighted_def_ppg)
    ) #,
  # off_ppg_adj = case_when(weighted_off_ppg > quantile(weighted_off_ppg, 0.8) ~ weighted_off_ppg + (off_ppg_aboveavg / 2),
  #                         weighted_off_ppg > mean(weighted_off_ppg) ~ weighted_off_ppg + (off_ppg_aboveavg / 5),
  #                         TRUE ~ weighted_off_ppg),
  # def_ppg_adj = case_when(weighted_def_ppg > quantile(weighted_def_ppg, 0.8) ~ weighted_def_ppg + (def_ppg_aboveavg / 2),
  #                         weighted_def_ppg > mean(weighted_def_ppg) ~ weighted_def_ppg + (def_ppg_aboveavg / 5),
  #                         TRUE ~ weighted_def_ppg)))
} else if (as.numeric(nfl_week) == 7) {
  ##### Week 7 weighted variable calculation #####
  ### Adding columns of variables weighted by season
  ### adding weighted variables (offense first)
  VoAVariables <- VoAVariables |>
    mutate(
      weighted_off_ypp = (adj_off_ypp_PY1 * 0.35) + (adj_off_ypp * 0.65),
      weighted_off_epa = (adj_off_epa_PY1 * 0.35) + (adj_off_epa * 0.65),
      weighted_off_success_rt = (off_success_rt_PY1 * 0.35) +
        (off_success_rt * 0.65),
      weighted_off_explosiveness = (adj_off_explosiveness_PY1 * 0.35) +
        (adj_off_explosiveness * 0.65),
      weighted_off_third_conv_rate = (off_third_conv_rate_PY1 * 0.35) +
        (off_third_conv_rate * 0.65),
      weighted_off_fourth_conv_rate = (off_fourth_conv_rate_PY1 * 0.35) +
        (off_fourth_conv_rate * 0.65),
      weighted_off_pass_ypa = (off_pass_ypa_PY1 * 0.35) + (off_pass_ypa * 0.65),
      weighted_off_pass_ypc = (off_pass_ypc_PY1 * 0.35) + (off_pass_ypc * 0.65),
      weighted_off_rush_ypa = (off_rush_ypa_PY1 * 0.35) + (off_rush_ypa * 0.65),
      weighted_off_pts_per_opp = (off_pts_per_opp_PY1 * 0.35) +
        (off_pts_per_opp * 0.65),
      weighted_off_turnovers = (off_turnovers_PY1 * 0.35) +
        (off_turnovers * 0.65),
      weighted_off_plays_pg = (off_plays_pg_PY1 * 0.35) + (off_plays_pg * 0.65),
      weighted_off_ppg = (adj_off_ppg_PY1 * 0.35) + (adj_off_ppg * 0.65),
      ### weighted defensive stats now
      weighted_def_ypp = (adj_def_ypp_PY1 * 0.35) + (adj_def_ypp * 0.65),
      weighted_def_epa = (adj_def_epa_PY1 * 0.35) + (adj_def_epa * 0.65),
      weighted_def_success_rt = (def_success_rt_PY1 * 0.35) +
        (def_success_rt * 0.65),
      weighted_def_explosiveness = (adj_def_explosiveness_PY1 * 0.35) +
        (adj_def_explosiveness * 0.65),
      weighted_def_third_conv_rate = (def_third_conv_rate_PY1 * 0.35) +
        (def_third_conv_rate * 0.65),
      weighted_def_fourth_conv_rate = (def_fourth_conv_rate_PY1 * 0.35) +
        (def_fourth_conv_rate * 0.65),
      weighted_def_pass_ypa = (def_pass_ypa_PY1 * 0.35) + (def_pass_ypa * 0.65),
      weighted_def_pass_ypc = (def_pass_ypc_PY1 * 0.35) + (def_pass_ypc * 0.65),
      weighted_def_rush_ypa = (def_rush_ypa_PY1 * 0.35) + (def_rush_ypa * 0.65),
      weighted_def_pts_per_opp = (def_pts_per_opp_PY1 * 0.35) +
        (def_pts_per_opp * 0.65),
      weighted_def_turnovers = (def_turnovers_PY1 * 0.35) +
        (def_turnovers * 0.65),
      weighted_def_plays_pg = (def_plays_pg_PY1 * 0.35) + (def_plays_pg * 0.65),
      weighted_def_ppg = (adj_def_ppg_PY1 * 0.35) + (adj_def_ppg * 0.65),
      ### weighted special teams stats now
      weighted_net_st_epa = (st_net_epa_PY1 * 0.35) + (st_net_epa * 0.65),
      weighted_net_punt_return_yds = ((st_punt_return_yds_PY1 -
        st_punt_return_yds_allowed_PY1) *
        0.35) +
        ((st_punt_return_yds - st_punt_return_yds_allowed) * 0.65),
      weighted_net_kick_return_yds = ((st_kick_return_yds_PY1 -
        st_kick_return_yds_allowed_PY1) *
        0.35) +
        ((st_kick_return_yds - st_kick_return_yds_allowed) * 0.65),
      weighted_net_punt_return_TDs = ((st_punt_return_TDs_PY1 -
        st_punt_return_TDs_allowed_PY1) *
        0.35) +
        ((st_punt_return_TDs - st_punt_return_TDs_allowed) * 0.65),
      weighted_net_kick_return_TDs = ((st_kick_return_TDs_PY1 -
        st_kick_return_TDs_allowed_PY1) *
        0.35) +
        ((st_kick_return_TDs - st_kick_return_TDs_allowed) * 0.65),
      weighted_net_fg_rate = ((fg_rate_PY1 - fg_rate_allowed_PY1) * 0.35) +
        ((fg_rate - fg_rate_allowed) * 0.65),
      weighted_net_fg_made_pg = ((fg_made_pg_PY1 - fg_made_pg_allowed_PY1) *
        0.35) +
        ((fg_made_pg - fg_made_pg_allowed) * 0.65),
      weighted_net_xp_rate = ((xp_rate_PY1 - xp_rate_allowed_PY1) * 0.35) +
        ((xp_rate - xp_rate_allowed) * 0.65),
      weighted_net_xp_made_pg = ((xp_made_pg_PY1 - xp_made_pg_allowed_PY1) *
        0.35) +
        ((xp_made_pg - xp_made_pg_allowed) * 0.65),
      weighted_net_st_ppg = (net_st_ppg_PY1 * 0.35) + (net_st_ppg * 0.65),
      off_ppg_aboveavg = weighted_off_ppg - mean(weighted_off_ppg),
      def_ppg_aboveavg = weighted_def_ppg - mean(weighted_def_ppg)
    ) #,
  # off_ppg_adj = case_when(weighted_off_ppg > quantile(weighted_off_ppg, 0.8) ~ weighted_off_ppg + (off_ppg_aboveavg / 2),
  #                         weighted_off_ppg > mean(weighted_off_ppg) ~ weighted_off_ppg + (off_ppg_aboveavg / 5),
  #                         TRUE ~ weighted_off_ppg),
  # def_ppg_adj = case_when(weighted_def_ppg > quantile(weighted_def_ppg, 0.8) ~ weighted_def_ppg + (def_ppg_aboveavg / 2),
  #                         weighted_def_ppg > mean(weighted_def_ppg) ~ weighted_def_ppg + (def_ppg_aboveavg / 5),
  #                         TRUE ~ weighted_def_ppg)))
} else if (as.numeric(nfl_week) == 8) {
  ##### Week 8 weighted variable calculation #####
  ### Adding columns of variables weighted by season
  ### adding weighted variables (offense first)
  VoAVariables <- VoAVariables |>
    mutate(
      weighted_off_ypp = (adj_off_ypp_PY1 * 0.3) + (adj_off_ypp * 0.7),
      weighted_off_epa = (adj_off_epa_PY1 * 0.3) + (adj_off_epa * 0.7),
      weighted_off_success_rt = (off_success_rt_PY1 * 0.3) +
        (off_success_rt * 0.7),
      weighted_off_explosiveness = (adj_off_explosiveness_PY1 * 0.3) +
        (adj_off_explosiveness * 0.7),
      weighted_off_third_conv_rate = (off_third_conv_rate_PY1 * 0.3) +
        (off_third_conv_rate * 0.7),
      weighted_off_fourth_conv_rate = (off_fourth_conv_rate_PY1 * 0.3) +
        (off_fourth_conv_rate * 0.7),
      weighted_off_pass_ypa = (off_pass_ypa_PY1 * 0.3) + (off_pass_ypa * 0.7),
      weighted_off_pass_ypc = (off_pass_ypc_PY1 * 0.3) + (off_pass_ypc * 0.7),
      weighted_off_rush_ypa = (off_rush_ypa_PY1 * 0.3) + (off_rush_ypa * 0.7),
      weighted_off_pts_per_opp = (off_pts_per_opp_PY1 * 0.3) +
        (off_pts_per_opp * 0.7),
      weighted_off_turnovers = (off_turnovers_PY1 * 0.3) +
        (off_turnovers * 0.7),
      weighted_off_plays_pg = (off_plays_pg_PY1 * 0.3) + (off_plays_pg * 0.7),
      weighted_off_ppg = (adj_off_ppg_PY1 * 0.3) + (adj_off_ppg * 0.7),
      ### weighted defensive stats now
      weighted_def_ypp = (adj_def_ypp_PY1 * 0.3) + (adj_def_ypp * 0.7),
      weighted_def_epa = (adj_def_epa_PY1 * 0.3) + (adj_def_epa * 0.7),
      weighted_def_success_rt = (def_success_rt_PY1 * 0.3) +
        (def_success_rt * 0.7),
      weighted_def_explosiveness = (adj_def_explosiveness_PY1 * 0.3) +
        (adj_def_explosiveness * 0.7),
      weighted_def_third_conv_rate = (def_third_conv_rate_PY1 * 0.3) +
        (def_third_conv_rate * 0.7),
      weighted_def_fourth_conv_rate = (def_fourth_conv_rate_PY1 * 0.3) +
        (def_fourth_conv_rate * 0.7),
      weighted_def_pass_ypa = (def_pass_ypa_PY1 * 0.3) + (def_pass_ypa * 0.7),
      weighted_def_pass_ypc = (def_pass_ypc_PY1 * 0.3) + (def_pass_ypc * 0.7),
      weighted_def_rush_ypa = (def_rush_ypa_PY1 * 0.3) + (def_rush_ypa * 0.7),
      weighted_def_pts_per_opp = (def_pts_per_opp_PY1 * 0.3) +
        (def_pts_per_opp * 0.7),
      weighted_def_turnovers = (def_turnovers_PY1 * 0.3) +
        (def_turnovers * 0.7),
      weighted_def_plays_pg = (def_plays_pg_PY1 * 0.3) + (def_plays_pg * 0.7),
      weighted_def_ppg = (adj_def_ppg_PY1 * 0.3) + (adj_def_ppg * 0.7),
      ### weighted special teams stats now
      weighted_net_st_epa = (st_net_epa_PY1 * 0.3) + (st_net_epa * 0.7),
      weighted_net_punt_return_yds = ((st_punt_return_yds_PY1 -
        st_punt_return_yds_allowed_PY1) *
        0.3) +
        ((st_punt_return_yds - st_punt_return_yds_allowed) * 0.7),
      weighted_net_kick_return_yds = ((st_kick_return_yds_PY1 -
        st_kick_return_yds_allowed_PY1) *
        0.3) +
        ((st_kick_return_yds - st_kick_return_yds_allowed) * 0.7),
      weighted_net_punt_return_TDs = ((st_punt_return_TDs_PY1 -
        st_punt_return_TDs_allowed_PY1) *
        0.3) +
        ((st_punt_return_TDs - st_punt_return_TDs_allowed) * 0.7),
      weighted_net_kick_return_TDs = ((st_kick_return_TDs_PY1 -
        st_kick_return_TDs_allowed_PY1) *
        0.3) +
        ((st_kick_return_TDs - st_kick_return_TDs_allowed) * 0.7),
      weighted_net_fg_rate = ((fg_rate_PY1 - fg_rate_allowed_PY1) * 0.3) +
        ((fg_rate - fg_rate_allowed) * 0.7),
      weighted_net_fg_made_pg = ((fg_made_pg_PY1 - fg_made_pg_allowed_PY1) *
        0.3) +
        ((fg_made_pg - fg_made_pg_allowed) * 0.7),
      weighted_net_xp_rate = ((xp_rate_PY1 - xp_rate_allowed_PY1) * 0.3) +
        ((xp_rate - xp_rate_allowed) * 0.7),
      weighted_net_xp_made_pg = ((xp_made_pg_PY1 - xp_made_pg_allowed_PY1) *
        0.3) +
        ((xp_made_pg - xp_made_pg_allowed) * 0.7),
      weighted_net_st_ppg = (net_st_ppg_PY1 * 0.3) + (net_st_ppg * 0.7),
      off_ppg_aboveavg = weighted_off_ppg - mean(weighted_off_ppg),
      def_ppg_aboveavg = weighted_def_ppg - mean(weighted_def_ppg)
    ) #,
  # off_ppg_adj = case_when(weighted_off_ppg > quantile(weighted_off_ppg, 0.8) ~ weighted_off_ppg + (off_ppg_aboveavg / 2),
  #                         weighted_off_ppg > mean(weighted_off_ppg) ~ weighted_off_ppg + (off_ppg_aboveavg / 5),
  #                         TRUE ~ weighted_off_ppg),
  # def_ppg_adj = case_when(weighted_def_ppg > quantile(weighted_def_ppg, 0.8) ~ weighted_def_ppg + (def_ppg_aboveavg / 2),
  #                         weighted_def_ppg > mean(weighted_def_ppg) ~ weighted_def_ppg + (def_ppg_aboveavg / 5),
  #                         TRUE ~ weighted_def_ppg)))
} else if (as.numeric(nfl_week) == 9) {
  ##### Week 9 weighted variable calculation #####
  ### Adding columns of variables weighted by season
  ### adding weighted variables (offense first)
  VoAVariables <- VoAVariables |>
    mutate(
      weighted_off_ypp = (adj_off_ypp_PY1 * 0.25) + (adj_off_ypp * 0.75),
      weighted_off_epa = (adj_off_epa_PY1 * 0.25) + (adj_off_epa * 0.75),
      weighted_off_success_rt = (off_success_rt_PY1 * 0.25) +
        (off_success_rt * 0.75),
      weighted_off_explosiveness = (adj_off_explosiveness_PY1 * 0.25) +
        (adj_off_explosiveness * 0.75),
      weighted_off_third_conv_rate = (off_third_conv_rate_PY1 * 0.25) +
        (off_third_conv_rate * 0.75),
      weighted_off_fourth_conv_rate = (off_fourth_conv_rate_PY1 * 0.25) +
        (off_fourth_conv_rate * 0.75),
      weighted_off_pass_ypa = (off_pass_ypa_PY1 * 0.25) + (off_pass_ypa * 0.75),
      weighted_off_pass_ypc = (off_pass_ypc_PY1 * 0.25) + (off_pass_ypc * 0.75),
      weighted_off_rush_ypa = (off_rush_ypa_PY1 * 0.25) + (off_rush_ypa * 0.75),
      weighted_off_pts_per_opp = (off_pts_per_opp_PY1 * 0.25) +
        (off_pts_per_opp * 0.75),
      weighted_off_turnovers = (off_turnovers_PY1 * 0.25) +
        (off_turnovers * 0.75),
      weighted_off_plays_pg = (off_plays_pg_PY1 * 0.25) + (off_plays_pg * 0.75),
      weighted_off_ppg = (adj_off_ppg_PY1 * 0.25) + (adj_off_ppg * 0.75),
      ### weighted defensive stats now
      weighted_def_ypp = (adj_def_ypp_PY1 * 0.25) + (adj_def_ypp * 0.75),
      weighted_def_epa = (adj_def_epa_PY1 * 0.25) + (adj_def_epa * 0.75),
      weighted_def_success_rt = (def_success_rt_PY1 * 0.25) +
        (def_success_rt * 0.75),
      weighted_def_explosiveness = (adj_def_explosiveness_PY1 * 0.25) +
        (adj_def_explosiveness * 0.75),
      weighted_def_third_conv_rate = (def_third_conv_rate_PY1 * 0.25) +
        (def_third_conv_rate * 0.75),
      weighted_def_fourth_conv_rate = (def_fourth_conv_rate_PY1 * 0.25) +
        (def_fourth_conv_rate * 0.75),
      weighted_def_pass_ypa = (def_pass_ypa_PY1 * 0.25) + (def_pass_ypa * 0.75),
      weighted_def_pass_ypc = (def_pass_ypc_PY1 * 0.25) + (def_pass_ypc * 0.75),
      weighted_def_rush_ypa = (def_rush_ypa_PY1 * 0.25) + (def_rush_ypa * 0.75),
      weighted_def_pts_per_opp = (def_pts_per_opp_PY1 * 0.25) +
        (def_pts_per_opp * 0.75),
      weighted_def_turnovers = (def_turnovers_PY1 * 0.25) +
        (def_turnovers * 0.75),
      weighted_def_plays_pg = (def_plays_pg_PY1 * 0.25) + (def_plays_pg * 0.75),
      weighted_def_ppg = (adj_def_ppg_PY1 * 0.25) + (adj_def_ppg * 0.75),
      ### weighted special teams stats now
      weighted_net_st_epa = (st_net_epa_PY1 * 0.25) + (st_net_epa * 0.25),
      weighted_net_punt_return_yds = ((st_punt_return_yds_PY1 -
        st_punt_return_yds_allowed_PY1) *
        0.25) +
        ((st_punt_return_yds - st_punt_return_yds_allowed) * 0.75),
      weighted_net_kick_return_yds = ((st_kick_return_yds_PY1 -
        st_kick_return_yds_allowed_PY1) *
        0.25) +
        ((st_kick_return_yds - st_kick_return_yds_allowed) * 0.75),
      weighted_net_punt_return_TDs = ((st_punt_return_TDs_PY1 -
        st_punt_return_TDs_allowed_PY1) *
        0.25) +
        ((st_punt_return_TDs - st_punt_return_TDs_allowed) * 0.75),
      weighted_net_kick_return_TDs = ((st_kick_return_TDs_PY1 -
        st_kick_return_TDs_allowed_PY1) *
        0.25) +
        ((st_kick_return_TDs - st_kick_return_TDs_allowed) * 0.75),
      weighted_net_fg_rate = ((fg_rate_PY1 - fg_rate_allowed_PY1) * 0.25) +
        ((fg_rate - fg_rate_allowed) * 0.75),
      weighted_net_fg_made_pg = ((fg_made_pg_PY1 - fg_made_pg_allowed_PY1) *
        0.25) +
        ((fg_made_pg - fg_made_pg_allowed) * 0.75),
      weighted_net_xp_rate = ((xp_rate_PY1 - xp_rate_allowed_PY1) * 0.25) +
        ((xp_rate - xp_rate_allowed) * 0.75),
      weighted_net_xp_made_pg = ((xp_made_pg_PY1 - xp_made_pg_allowed_PY1) *
        0.25) +
        ((xp_made_pg - xp_made_pg_allowed) * 0.75),
      weighted_net_st_ppg = (net_st_ppg_PY1 * 0.25) + (net_st_ppg * 0.75),
      off_ppg_aboveavg = weighted_off_ppg - mean(weighted_off_ppg),
      def_ppg_aboveavg = weighted_def_ppg - mean(weighted_def_ppg)
    ) #,
  # off_ppg_adj = case_when(weighted_off_ppg > quantile(weighted_off_ppg, 0.8) ~ weighted_off_ppg + (off_ppg_aboveavg / 2),
  #                         weighted_off_ppg > mean(weighted_off_ppg) ~ weighted_off_ppg + (off_ppg_aboveavg / 5),
  #                         TRUE ~ weighted_off_ppg),
  # def_ppg_adj = case_when(weighted_def_ppg > quantile(weighted_def_ppg, 0.8) ~ weighted_def_ppg + (def_ppg_aboveavg / 2),
  #                         weighted_def_ppg > mean(weighted_def_ppg) ~ weighted_def_ppg + (def_ppg_aboveavg / 5),
  #                         TRUE ~ weighted_def_ppg)))
} else if (as.numeric(nfl_week) == 10) {
  ##### Week 10 weighted variable calculation #####
  ### Adding columns of variables weighted by season
  ### adding weighted variables (offense first)
  VoAVariables <- VoAVariables |>
    mutate(
      weighted_off_ypp = (adj_off_ypp_PY1 * 0.1) + (adj_off_ypp * 0.9),
      weighted_off_epa = (adj_off_epa_PY1 * 0.1) + (adj_off_epa * 0.9),
      weighted_off_success_rt = (off_success_rt_PY1 * 0.1) +
        (off_success_rt * 0.9),
      weighted_off_explosiveness = (adj_off_explosiveness_PY1 * 0.1) +
        (adj_off_explosiveness * 0.9),
      weighted_off_third_conv_rate = (off_third_conv_rate_PY1 * 0.1) +
        (off_third_conv_rate * 0.9),
      weighted_off_fourth_conv_rate = (off_fourth_conv_rate_PY1 * 0.1) +
        (off_fourth_conv_rate * 0.9),
      weighted_off_pass_ypa = (off_pass_ypa_PY1 * 0.1) + (off_pass_ypa * 0.9),
      weighted_off_pass_ypc = (off_pass_ypc_PY1 * 0.1) + (off_pass_ypc * 0.9),
      weighted_off_rush_ypa = (off_rush_ypa_PY1 * 0.1) + (off_rush_ypa * 0.9),
      weighted_off_pts_per_opp = (off_pts_per_opp_PY1 * 0.1) +
        (off_pts_per_opp * 0.9),
      weighted_off_turnovers = (off_turnovers_PY1 * 0.1) +
        (off_turnovers * 0.9),
      weighted_off_plays_pg = (off_plays_pg_PY1 * 0.1) + (off_plays_pg * 0.9),
      weighted_off_ppg = (adj_off_ppg_PY1 * 0.1) + (adj_off_ppg * 0.9),
      ### weighted defensive stats now
      weighted_def_ypp = (adj_def_ypp_PY1 * 0.1) + (adj_def_ypp * 0.9),
      weighted_def_epa = (adj_def_epa_PY1 * 0.1) + (adj_def_epa * 0.9),
      weighted_def_success_rt = (def_success_rt_PY1 * 0.1) +
        (def_success_rt * 0.9),
      weighted_def_explosiveness = (adj_def_explosiveness_PY1 * 0.1) +
        (adj_def_explosiveness * 0.9),
      weighted_def_third_conv_rate = (def_third_conv_rate_PY1 * 0.1) +
        (def_third_conv_rate * 0.9),
      weighted_def_fourth_conv_rate = (def_fourth_conv_rate_PY1 * 0.1) +
        (def_fourth_conv_rate * 0.9),
      weighted_def_pass_ypa = (def_pass_ypa_PY1 * 0.1) + (def_pass_ypa * 0.9),
      weighted_def_pass_ypc = (def_pass_ypc_PY1 * 0.1) + (def_pass_ypc * 0.9),
      weighted_def_rush_ypa = (def_rush_ypa_PY1 * 0.1) + (def_rush_ypa * 0.9),
      weighted_def_pts_per_opp = (def_pts_per_opp_PY1 * 0.1) +
        (def_pts_per_opp * 0.9),
      weighted_def_turnovers = (def_turnovers_PY1 * 0.1) +
        (def_turnovers * 0.9),
      weighted_def_plays_pg = (def_plays_pg_PY1 * 0.1) + (def_plays_pg * 0.9),
      weighted_def_ppg = (adj_def_ppg_PY1 * 0.1) + (adj_def_ppg * 0.9),
      ### weighted special teams stats now
      weighted_net_st_epa = (st_net_epa_PY1 * 0.1) + (st_net_epa * 0.9),
      weighted_net_punt_return_yds = ((st_punt_return_yds_PY1 -
        st_punt_return_yds_allowed_PY1) *
        0.1) +
        ((st_punt_return_yds - st_punt_return_yds_allowed) * 0.9),
      weighted_net_kick_return_yds = ((st_kick_return_yds_PY1 -
        st_kick_return_yds_allowed_PY1) *
        0.1) +
        ((st_kick_return_yds - st_kick_return_yds_allowed) * 0.9),
      weighted_net_punt_return_TDs = ((st_punt_return_TDs_PY1 -
        st_punt_return_TDs_allowed_PY1) *
        0.1) +
        ((st_punt_return_TDs - st_punt_return_TDs_allowed) * 0.9),
      weighted_net_kick_return_TDs = ((st_kick_return_TDs_PY1 -
        st_kick_return_TDs_allowed_PY1) *
        0.1) +
        ((st_kick_return_TDs - st_kick_return_TDs_allowed) * 0.9),
      weighted_net_fg_rate = ((fg_rate_PY1 - fg_rate_allowed_PY1) * 0.1) +
        ((fg_rate - fg_rate_allowed) * 0.9),
      weighted_net_fg_made_pg = ((fg_made_pg_PY1 - fg_made_pg_allowed_PY1) *
        0.1) +
        ((fg_made_pg - fg_made_pg_allowed) * 0.9),
      weighted_net_xp_rate = ((xp_rate_PY1 - xp_rate_allowed_PY1) * 0.1) +
        ((xp_rate - xp_rate_allowed) * 0.9),
      weighted_net_xp_made_pg = ((xp_made_pg_PY1 - xp_made_pg_allowed_PY1) *
        0.1) +
        ((xp_made_pg - xp_made_pg_allowed) * 0.9),
      weighted_net_st_ppg = (net_st_ppg_PY1 * 0.1) + (net_st_ppg * 0.9),
      off_ppg_aboveavg = weighted_off_ppg - mean(weighted_off_ppg),
      def_ppg_aboveavg = weighted_def_ppg - mean(weighted_def_ppg)
    ) #,
  # off_ppg_adj = case_when(weighted_off_ppg > quantile(weighted_off_ppg, 0.8) ~ weighted_off_ppg + (off_ppg_aboveavg / 2),
  #                         weighted_off_ppg > mean(weighted_off_ppg) ~ weighted_off_ppg + (off_ppg_aboveavg / 5),
  #                         TRUE ~ weighted_off_ppg),
  # def_ppg_adj = case_when(weighted_def_ppg > quantile(weighted_def_ppg, 0.8) ~ weighted_def_ppg + (def_ppg_aboveavg / 2),
  #                         weighted_def_ppg > mean(weighted_def_ppg) ~ weighted_def_ppg + (def_ppg_aboveavg / 5),
  #                         TRUE ~ weighted_def_ppg)))
} else {
  print("no more weighted vars, current season only")
}

##### Setting number of Columns to start averaging Rank values At #####
VoA_RankColNum <- ncol(VoAVariables) + 1
if (as.integer(nfl_week) == 0) {
  VoATrain_RankColNum <- ncol(VoAVariablesTrain_PY1) + 1
}

##### Ranking Variables #####
if (as.numeric(nfl_week) <= 10) {
  ##### Weeks 0-10 Variable Ranks #####
  ### since dfs used to fit model don't use PY suffixes in the col names, using the function for creating rank columns on those dfs before they get combined
  VoAVariablesTrain_PY1 <- rank_voa_cols(VoAVariablesTrain_PY1)
  VoAVariablesTrain_PY2 <- rank_voa_cols(VoAVariablesTrain_PY2)
  VoAVariablesTrain_PY3 <- rank_voa_cols(VoAVariablesTrain_PY3)
  VoAVariablesTrain_PY4 <- rank_voa_cols(VoAVariablesTrain_PY4)
  VoAVariablesTrain_PY5 <- rank_voa_cols(VoAVariablesTrain_PY5)
  ### ranking data used for inference/current ratings
  VoAVariables <- VoAVariables |>
    mutate(
      Rank_weighted_off_ypp = dense_rank(desc(weighted_off_ypp)),
      Rank_weighted_off_epa = dense_rank(desc(weighted_off_epa)),
      Rank_weighted_off_success_rt = dense_rank(desc(weighted_off_success_rt)),
      Rank_weighted_off_explosiveness = dense_rank(desc(
        weighted_off_explosiveness
      )),
      Rank_weighted_off_third_conv_rate = dense_rank(desc(
        weighted_off_third_conv_rate
      )),
      Rank_weighted_off_fourth_conv_rate = dense_rank(desc(
        weighted_off_fourth_conv_rate
      )),
      Rank_weighted_off_pass_ypa = dense_rank(desc(weighted_off_pass_ypa)),
      Rank_weighted_off_pass_ypc = dense_rank(desc(weighted_off_pass_ypc)),
      Rank_weighted_off_rush_ypa = dense_rank(desc(weighted_off_rush_ypa)),
      Rank_weighted_off_pts_per_opp = dense_rank(desc(
        weighted_off_pts_per_opp
      )),
      Rank_weighted_off_turnovers = dense_rank(weighted_off_turnovers),
      Rank_weighted_off_ppg = dense_rank(desc(weighted_off_ppg)),
      ### ranking defensive variables now
      Rank_weighted_def_ypp = dense_rank(weighted_def_ypp),
      Rank_weighted_def_epa = dense_rank(weighted_def_epa),
      Rank_weighted_def_success_rt = dense_rank(weighted_def_success_rt),
      Rank_weighted_def_explosiveness = dense_rank(weighted_def_explosiveness),
      Rank_weighted_def_third_conv_rate = dense_rank(
        weighted_def_third_conv_rate
      ),
      Rank_weighted_def_fourth_conv_rate = dense_rank(
        weighted_def_fourth_conv_rate
      ),
      Rank_weighted_def_pass_ypa = dense_rank(weighted_def_pass_ypa),
      Rank_weighted_def_pass_ypc = dense_rank(weighted_def_pass_ypc),
      Rank_weighted_def_rush_ypa = dense_rank(weighted_def_rush_ypa),
      Rank_weighted_def_pts_per_opp = dense_rank(weighted_def_pts_per_opp),
      Rank_weighted_def_turnovers = dense_rank(desc(weighted_def_turnovers)),
      Rank_weighted_def_ppg = dense_rank(weighted_def_ppg),
      ### ranking ST variables now
      Rank_weighted_net_st_epa = dense_rank(desc(weighted_net_st_epa)),
      Rank_weighted_net_punt_return_yds = dense_rank(desc(
        weighted_net_punt_return_yds
      )),
      Rank_weighted_net_punt_return_TDs = dense_rank(desc(
        weighted_net_punt_return_TDs
      )),
      Rank_weighted_net_kick_return_yds = dense_rank(desc(
        weighted_net_kick_return_yds
      )),
      Rank_weighted_net_kick_return_TDs = dense_rank(desc(
        weighted_net_kick_return_TDs
      )),
      Rank_weighted_net_xp_rate = dense_rank(desc(weighted_net_xp_rate)),
      Rank_weighted_net_xp_made_pg = dense_rank(desc(weighted_net_xp_made_pg)),
      Rank_weighted_net_xp_rate = dense_rank(desc(weighted_net_xp_rate)),
      Rank_weighted_net_xp_made_pg = dense_rank(desc(weighted_net_xp_made_pg)),
      Rank_weighted_net_st_ppg = dense_rank(desc(weighted_net_st_ppg))
    )
} else {
  ##### Week 11-End of Season Variable Ranks #####
  ### Ranking variables when only current season data is being used
  VoAVariables <- rank_voa_cols(VoAVariables)
}


##### Calculating VoA Output #####
### for week 0 (preseason), rank columns start at 168
## this may not be true but since I added the VoA_RankColNum thing it doesn't really matter
if (as.numeric(nfl_week) == 0) {
  ### calculating VoA Output for each of the model training dfs
  VoAVariablesTrain_PY1 <- VoAVariablesTrain_PY1 |>
    mutate(
      VoA_Output = rowMeans(VoAVariablesTrain_PY1[,
        VoATrain_RankColNum:ncol(VoAVariablesTrain_PY1)
      ])
    )
  VoAVariablesTrain_PY2 <- VoAVariablesTrain_PY2 |>
    mutate(
      VoA_Output = rowMeans(VoAVariablesTrain_PY2[,
        VoATrain_RankColNum:ncol(VoAVariablesTrain_PY2)
      ])
    )
  VoAVariablesTrain_PY3 <- VoAVariablesTrain_PY3 |>
    mutate(
      VoA_Output = rowMeans(VoAVariablesTrain_PY3[,
        VoATrain_RankColNum:ncol(VoAVariablesTrain_PY3)
      ])
    )
  VoAVariablesTrain_PY4 <- VoAVariablesTrain_PY4 |>
    mutate(
      VoA_Output = rowMeans(VoAVariablesTrain_PY4[,
        VoATrain_RankColNum:ncol(VoAVariablesTrain_PY4)
      ])
    )
  VoAVariablesTrain_PY5 <- VoAVariablesTrain_PY5 |>
    mutate(
      VoA_Output = rowMeans(VoAVariablesTrain_PY5[,
        VoATrain_RankColNum:ncol(VoAVariablesTrain_PY5)
      ])
    )
  ### binding train dfs together now that they have all the columns that go into the VoA
  VoATrain <- rbind(
    VoAVariablesTrain_PY1,
    rbind(
      VoAVariablesTrain_PY2,
      rbind(
        VoAVariablesTrain_PY3,
        rbind(VoAVariablesTrain_PY4, VoAVariablesTrain_PY5)
      )
    )
  )
  ### now VoA Output for inference/current ratings df with weighted values
  VoAVariables <- VoAVariables |>
    mutate(
      VoA_Output = (rowMeans(VoAVariables[,
        VoA_RankColNum:ncol(VoAVariables)
      ]))
    )
} else {
  ### only calculating VoA Output for the ratings df
  VoAVariables <- VoAVariables |>
    mutate(
      VoA_Output = (rowMeans(VoAVariables[,
        VoA_RankColNum:ncol(VoAVariables)
      ]))
    )
}

##### Using Stan Model to create unit/team strength ratings #####
if (as.numeric(nfl_week) <= 10) {
  ##### Week 0-10 Stan Models #####
  ### VoA Offensive Rating Model
  ### making list of data to declare what goes into stan model
  Off_VoA_datalist <- list(
    N = nrow(VoATrain),
    off_ppg = VoATrain$adj_off_ppg,
    off_epa = VoATrain$adj_off_epa,
    off_ypp = VoATrain$adj_off_ypp,
    off_success_rt = VoATrain$off_success_rt,
    off_explosiveness = VoATrain$adj_off_explosiveness,
    third_conv_rate = VoATrain$off_third_conv_rate,
    off_pts_per_opp = VoATrain$off_pts_per_opp,
    off_plays_pg = VoATrain$adj_off_plays_pg,
    VoA_Output = (1 / VoATrain$VoA_Output)
  )

  ### fitting stan model
  set.seed(802)
  options(mc.cores = parallel::detectCores() / 2)
  Off_VoA_model <- cmdstan_model(here("Scripts", "Stan", "Off_VoA.stan"))
  Off_VoA_fit <- Off_VoA_model$sample(
    data = Off_VoA_datalist,
    chains = 3,
    iter_sampling = 10000,
    iter_warmup = 3000,
    seed = 802
  )
  Off_VoA_fit

  ### saving Off_VoA_fit as an RDS file so that I'm not re-compiling and/or re-fitting the model every single week
  ## hoping that using more years of complete season data will help produce a more stable model
  write_rds(
    Off_VoA_fit,
    file = here("Data", "FittedModels", "OffVoAStanFit.rds"),
    compress = "gz"
  )

  ### Print the diagnostics
  print(Off_VoA_fit$cmdstan_diagnose())

  ### Extracting Parameters
  Off_VoA_pars <- Off_VoA_fit$draws(
    variables = c(
      "b0",
      "beta_off_epa",
      "beta_off_ypp",
      "beta_off_success_rt",
      "beta_off_explosiveness",
      "beta_third_conv_rate",
      "beta_off_pts_per_opp",
      "beta_off_plays_pg",
      "beta_VoA_Output",
      "sigma"
    ),
    format = "draws_df"
  )

  ### creating matrix to hold ratings
  ### adding in process uncertainty
  Off_VoA_Ratings <- matrix(NA, length(Off_VoA_pars$b0), nrow(VoAVariables))

  ### creating ratings
  set.seed(802)
  for (p in 1:length(Off_VoA_pars$b0)) {
    for (t in 1:nrow(VoAVariables)) {
      Off_VoA_Rating <- rnorm(
        1,
        mean = Off_VoA_pars$b0[p] +
          Off_VoA_pars$beta_off_epa[p] * VoAVariables$weighted_off_epa[t] +
          Off_VoA_pars$beta_off_ypp[p] * VoAVariables$weighted_off_ypp[t] +
          Off_VoA_pars$beta_off_success_rt[p] *
            VoAVariables$weighted_off_success_rt[t] +
          Off_VoA_pars$beta_off_explosiveness[p] *
            VoAVariables$weighted_off_explosiveness[t] +
          Off_VoA_pars$beta_third_conv_rate[p] *
            VoAVariables$weighted_off_third_conv_rate[t] +
          Off_VoA_pars$beta_off_pts_per_opp[p] *
            VoAVariables$weighted_off_pts_per_opp[t] +
          Off_VoA_pars$beta_off_plays_pg[p] *
            VoAVariables$weighted_off_plays_pg[t] +
          Off_VoA_pars$beta_VoA_Output[p] * (1 / (VoAVariables$VoA_Output[t])),
        sd = Off_VoA_pars$sigma[p]
      )
      Off_VoA_Ratings[p, t] <- Off_VoA_Rating
    }
  }

  ### generating median and mean and quantile ratings
  MeanPred <- apply(Off_VoA_Ratings, 2, mean)
  MedianPred <- apply(Off_VoA_Ratings, 2, median)
  Upper <- apply(Off_VoA_Ratings, 2, quantile, prob = .975)
  Lower <- apply(Off_VoA_Ratings, 2, quantile, prob = .025)

  VoAVariables$OffVoA_MeanRating <- MeanPred
  VoAVariables$OffVoA_MedRating <- MedianPred
  VoAVariables$OffVoA_95PctRating <- Upper
  VoAVariables$OffVoA_05PctRating <- Lower

  ### VoA Defensive Rating Model
  ### making list of data to declare what goes into stan model
  Def_VoA_datalist <- list(
    N = nrow(VoATrain),
    def_ppg = VoATrain$adj_def_ppg,
    def_epa = VoATrain$adj_def_epa,
    def_ypp = VoATrain$adj_def_ypp,
    def_success_rt = VoATrain$def_success_rt,
    def_explosiveness = VoATrain$adj_def_explosiveness,
    def_third_conv_rate = VoATrain$def_third_conv_rate,
    def_pts_per_opp = VoATrain$def_pts_per_opp,
    def_plays_pg = VoATrain$adj_def_plays_pg,
    VoA_Output = VoATrain$VoA_Output
  )

  ### fitting stan model
  set.seed(802)
  # options(mc.cores = parallel::detectCores() / 2)
  Def_VoA_model <- cmdstan_model(here("Scripts", "Stan", "Def_VoA.stan"))
  Def_VoA_fit <- Def_VoA_model$sample(
    data = Def_VoA_datalist,
    chains = 3,
    iter_sampling = 10000,
    iter_warmup = 3000,
    seed = 802
  )
  Def_VoA_fit

  ### saving Off_VoA_fit as an RDS file so that I'm not re-compiling and/or re-fitting the model every single week
  ## hoping that using more years of complete season data will help produce a more stable model
  write_rds(
    Def_VoA_fit,
    file = here("Data", "FittedModels", "DefVoAStanFit.rds"),
    compress = "gz"
  )

  ### Print the diagnostics
  print(Def_VoA_fit$cmdstan_diagnose())

  ### Extracting Parameters
  Def_VoA_pars <- Def_VoA_fit$draws(
    variables = c(
      "b0",
      "beta_def_epa",
      "beta_def_ypp",
      "beta_def_success_rt",
      "beta_def_explosiveness",
      "beta_def_third_conv_rate",
      "beta_def_pts_per_opp",
      "beta_def_plays_pg",
      "beta_VoA_Output",
      "sigma"
    ),
    format = "draws_df"
  )

  ### creating matrix to hold ratings
  ### adding in process uncertainty
  Def_VoA_Ratings <- matrix(NA, length(Def_VoA_pars$b0), nrow(VoAVariables))

  ### creating ratings
  set.seed(802)
  for (p in 1:length(Def_VoA_pars$b0)) {
    for (t in 1:nrow(VoAVariables)) {
      Def_VoA_Rating <- rnorm(
        1,
        mean = Def_VoA_pars$b0[p] +
          Def_VoA_pars$beta_def_epa[p] * VoAVariables$weighted_def_epa[t] +
          Def_VoA_pars$beta_def_ypp[p] * VoAVariables$weighted_def_ypp[t] +
          Def_VoA_pars$beta_def_success_rt[p] *
            VoAVariables$weighted_def_success_rt[t] +
          Def_VoA_pars$beta_def_explosiveness[p] *
            VoAVariables$weighted_def_explosiveness[t] +
          Def_VoA_pars$beta_def_third_conv_rate[p] *
            VoAVariables$weighted_def_third_conv_rate[t] +
          Def_VoA_pars$beta_def_pts_per_opp[p] *
            VoAVariables$weighted_def_pts_per_opp[t] +
          Def_VoA_pars$beta_def_plays_pg[p] *
            VoAVariables$weighted_def_plays_pg[t] +
          Def_VoA_pars$beta_VoA_Output[p] * VoAVariables$VoA_Output[t],
        sd = Def_VoA_pars$sigma[p]
      )
      Def_VoA_Ratings[p, t] <- Def_VoA_Rating
    }
  }

  ### generating median and mean and quantile ratings
  MeanPred <- apply(Def_VoA_Ratings, 2, mean)
  MedianPred <- apply(Def_VoA_Ratings, 2, median)
  Upper <- apply(Def_VoA_Ratings, 2, quantile, prob = 0.975)
  Lower <- apply(Def_VoA_Ratings, 2, quantile, prob = 0.025)

  VoAVariables$DefVoA_MeanRating <- MeanPred
  VoAVariables$DefVoA_MedRating <- MedianPred
  VoAVariables$DefVoA_95PctRating <- Upper
  VoAVariables$DefVoA_05PctRating <- Lower

  ### Special Teams VoA
  ### making list of data to declare what goes into Stan model
  ST_VoA_datalist <- list(
    N = nrow(VoATrain),
    net_st_ppg = VoATrain$net_st_ppg,
    net_st_epa = VoATrain$net_st_epa,
    net_kick_return_avg = VoATrain$net_kick_return_yds,
    net_punt_return_avg = VoATrain$net_punt_return_yds,
    net_fg_rate = VoATrain$net_fg_rate,
    net_xp_rate = VoATrain$net_xp_rate
  )

  ### fitting special teams Stan model
  set.seed(802)
  options(mc.cores = parallel::detectCores() / 2)
  ST_VoA_model <- cmdstan_model(here("Scripts", "Stan", "ST_VoA.stan"))
  ST_VoA_fit <- ST_VoA_model$sample(
    data = ST_VoA_datalist,
    chains = 3,
    iter_sampling = 5000,
    iter_warmup = 2500,
    seed = 802
  )
  ST_VoA_fit

  ### saving Off_VoA_fit as an RDS file so that I'm not re-compiling and/or re-fitting the model every single week
  ## hoping that using more years of complete season data will help produce a more stable model
  write_rds(
    ST_VoA_fit,
    file = here("Data", "FittedModels", "STVoAStanFit.rds"),
    compress = "gz"
  )

  ### Print the diagnostics
  print(ST_VoA_fit$cmdstan_diagnose())

  ### extracting parameters
  ST_VoA_pars <- ST_VoA_fit$draws(
    variables = c(
      "b0",
      "beta_net_st_epa",
      "beta_net_kick_return_avg",
      "beta_net_punt_return_avg",
      "beta_net_fg_rate",
      "beta_net_xp_rate",
      "sigma"
    ),
    format = "draws_df"
  )

  ### creating matrix to store special teams VoA_Ratings
  ST_VoA_Ratings <- matrix(
    NA,
    nrow = length(ST_VoA_pars$b0),
    ncol = nrow(VoAVariables)
  )

  ### creating special teams VoA_Ratings
  set.seed(802)
  for (p in 1:length(ST_VoA_pars$b0)) {
    for (t in 1:nrow(VoAVariables)) {
      ST_VoA_Rating <- rnorm(
        1,
        mean = ST_VoA_pars$b0[p] +
          ST_VoA_pars$beta_net_st_epa[p] *
            VoAVariables$weighted_net_st_epa[t] +
          ST_VoA_pars$beta_net_kick_return_avg[p] *
            VoAVariables$weighted_net_kick_return_yds[t] +
          ST_VoA_pars$beta_net_punt_return_avg[p] *
            VoAVariables$weighted_net_punt_return_yds[t] +
          ST_VoA_pars$beta_net_fg_rate[p] *
            VoAVariables$weighted_net_fg_rate[t] +
          ST_VoA_pars$beta_net_xp_rate[p] *
            VoAVariables$weighted_net_xp_rate[t],
        sd = ST_VoA_pars$sigma[p]
      )
      ST_VoA_Ratings[p, t] <- ST_VoA_Rating
    }
  }

  ### generating median and mean and quantile ratings
  MeanPred <- apply(ST_VoA_Ratings, 2, mean)
  MedianPred <- apply(ST_VoA_Ratings, 2, median)
  Upper <- apply(ST_VoA_Ratings, 2, quantile, prob = 0.975)
  Lower <- apply(ST_VoA_Ratings, 2, quantile, prob = 0.025)

  VoAVariables$STVoA_MeanRating <- MeanPred
  VoAVariables$STVoA_MedRating <- MedianPred
  VoAVariables$STVoA_95PctRating <- Upper
  VoAVariables$STVoA_05PctRating <- Lower
} else {
  ##### Week 11-End of Season Stan Models #####
  ### VoA Offensive Rating Model
  ### making list of data to declare what goes into stan model
  # Off_VoA_datalist <- list(
  #   N = nrow(VoAVariables),
  #   off_ppg = VoAVariables$adj_off_ppg,
  #   off_epa = VoAVariables$adj_off_epa,
  #   off_ypp = VoAVariables$adj_off_ypp,
  #   off_success_rt = VoAVariables$off_success_rt,
  #   off_explosiveness = VoAVariables$adj_off_explosiveness,
  #   third_conv_rate = VoAVariables$off_third_conv_rate,
  #   off_pts_per_opp = VoAVariables$off_pts_per_opp,
  #   off_plays_pg = VoAVariables$off_plays_pg,
  #   VoA_Output = (1 / VoAVariables$VoA_Output)
  # )

  # ### fitting stan model
  # set.seed(802)
  # options(mc.cores = parallel::detectCores() / 2)
  # Off_VoA_model <- cmdstan_model(here("Scripts", "Stan", "Off_VoA.stan"))
  # Off_VoA_fit <- Off_VoA_model$sample(
  #   data = Off_VoA_datalist,
  #   chains = 3,
  #   iter_sampling = 10000,
  #   iter_warmup = 3000,
  #   seed = 802
  # )
  # Off_VoA_fit

  ### loading offensive Stan model
  Off_VoA_fit <- read_rds(here("Data", "FittedModels", "OffVoAStanFit.rds"))

  ### Print the diagnostics
  print(Off_VoA_fit$cmdstan_diagnose())

  ### Extracting Parameters
  Off_VoA_pars <- Off_VoA_fit$draws(
    variables = c(
      "b0",
      "beta_off_epa",
      "beta_off_ypp",
      "beta_off_success_rt",
      "beta_off_explosiveness",
      "beta_third_conv_rate",
      "beta_off_pts_per_opp",
      "beta_off_plays_pg",
      "beta_VoA_Output",
      "sigma"
    ),
    format = "draws_df"
  )

  ### creating matrix to hold ratings
  ### adding in process uncertainty
  Off_VoA_Ratings <- matrix(NA, length(Off_VoA_pars$b0), nrow(VoAVariables))

  ### creating ratings
  set.seed(802)
  for (p in 1:length(Off_VoA_pars$b0)) {
    for (t in 1:nrow(VoAVariables)) {
      Off_VoA_Rating <- rnorm(
        1,
        mean = Off_VoA_pars$b0[p] +
          Off_VoA_pars$beta_off_epa[p] * VoAVariables$adj_off_epa[t] +
          Off_VoA_pars$beta_off_ypp[p] * VoAVariables$adj_off_ypp[t] +
          Off_VoA_pars$beta_off_success_rt[p] *
            VoAVariables$off_success_rt[t] +
          Off_VoA_pars$beta_off_explosiveness[p] *
            VoAVariables$adj_off_explosiveness[t] +
          Off_VoA_pars$beta_third_conv_rate[p] *
            VoAVariables$off_third_conv_rate[t] +
          Off_VoA_pars$beta_off_pts_per_opp[p] *
            VoAVariables$off_pts_per_opp[t] +
          Off_VoA_pars$beta_off_plays_pg[p] * VoAVariables$off_plays_pg[t] +
          Off_VoA_pars$beta_VoA_Output[p] * (1 / (VoAVariables$VoA_Output[t])),
        sd = Off_VoA_pars$sigma[p]
      )
      Off_VoA_Ratings[p, t] <- Off_VoA_Rating
    }
  }

  ### generating median and mean and quantile ratings
  MeanPred <- apply(Off_VoA_Ratings, 2, mean)
  MedianPred <- apply(Off_VoA_Ratings, 2, median)
  Upper <- apply(Off_VoA_Ratings, 2, quantile, prob = 0.975)
  Lower <- apply(Off_VoA_Ratings, 2, quantile, prob = 0.025)

  VoAVariables$OffVoA_MeanRating <- MeanPred
  VoAVariables$OffVoA_MedRating <- MedianPred
  VoAVariables$OffVoA_95PctRating <- Upper
  VoAVariables$OffVoA_05PctRating <- Lower

  ### VoA Defensive Rating Model
  ### making list of data to declare what goes into stan model
  # Def

  ### loading defensive Stan model
  Def_VoA_fit <- read_rds(here("Data", "FittedModels", "DefVoAStanFit.rds"))
  Def_VoA_fit

  ### Print the diagnostics
  print(Def_VoA_fit$cmdstan_diagnose())

  ### Extracting Parameters
  Def_VoA_pars <- Def_VoA_fit$draws(
    variables = c(
      "b0",
      "beta_def_epa",
      "beta_def_ypp",
      "beta_def_success_rt",
      "beta_def_explosiveness",
      "beta_def_third_conv_rate",
      "beta_def_pts_per_opp",
      "beta_def_plays_pg",
      "beta_VoA_Output",
      "sigma"
    ),
    format = "draws_df"
  )

  ### creating matrix to hold ratings
  ### adding in process uncertainty
  Def_VoA_Ratings <- matrix(NA, length(Def_VoA_pars$b0), nrow(VoAVariables))

  ### creating ratings
  set.seed(802)
  for (p in 1:length(Def_VoA_pars$b0)) {
    for (t in 1:nrow(VoAVariables)) {
      Def_VoA_Rating <- rnorm(
        1,
        mean = Def_VoA_pars$b0[p] +
          Def_VoA_pars$beta_def_epa[p] * VoAVariables$adj_def_epa[t] +
          Def_VoA_pars$beta_def_ypp[p] * VoAVariables$adj_def_ypp[t] +
          Def_VoA_pars$beta_def_success_rt[p] *
            VoAVariables$def_success_rt[t] +
          Def_VoA_pars$beta_def_explosiveness[p] *
            VoAVariables$adj_def_explosiveness[t] +
          Def_VoA_pars$beta_def_third_conv_rate[p] *
            VoAVariables$def_third_conv_rate[t] +
          Def_VoA_pars$beta_def_pts_per_opp[p] *
            VoAVariables$def_pts_per_opp[t] +
          Def_VoA_pars$beta_def_plays_pg[p] * VoAVariables$def_plays_pg[t] +
          Def_VoA_pars$beta_VoA_Output[p] * VoAVariables$VoA_Output[t],
        sd = Def_VoA_pars$sigma[p]
      )
      Def_VoA_Ratings[p, t] <- Def_VoA_Rating
    }
  }

  ### generating median and mean and quantile ratings
  MeanPred <- apply(Def_VoA_Ratings, 2, mean)
  MedianPred <- apply(Def_VoA_Ratings, 2, median)
  Upper <- apply(Def_VoA_Ratings, 2, quantile, prob = 0.975)
  Lower <- apply(Def_VoA_Ratings, 2, quantile, prob = 0.025)

  VoAVariables$DefVoA_MeanRating <- MeanPred
  VoAVariables$DefVoA_MedRating <- MedianPred
  VoAVariables$DefVoA_95PctRating <- Upper
  VoAVariables$DefVoA_05PctRating <- Lower

  ### Special Teams VoA
  ### making list of data to declare what goes into Stan model
  # ST_VoA_datalist <- list(
  #   N = nrow(VoAVariables),
  #   net_st_ppg = VoAVariables$net_st_ppg,
  #   net_st_epa = VoAVariables$st_net_epa,
  #   net_kick_return_avg = VoAVariables$net_kick_return_yds,
  #   net_punt_return_avg = VoAVariables$net_punt_return_yds,
  #   net_fg_rate = VoAVariables$net_fg_rate,
  #   net_xp_rate = VoAVariables$net_xp_rate
  # )

  # ### fitting special teams Stan model
  # set.seed(802)
  # ST_VoA_model <- cmdstan_model(here("Scripts", "Stan", "ST_VoA.stan"))
  # ST_VoA_fit <- ST_VoA_model$sample(
  #   data = ST_VoA_datalist,
  #   chains = 3,
  #   iter_sampling = 5000,
  #   iter_warmup = 2500,
  #   seed = 802
  # )

  ### loading offensive Stan model
  ST_VoA_fit <- read_rds(here("Data", "FittedModels", "STVoAStanFit.rds"))
  ST_VoA_fit

  ### Print the diagnostics
  print(ST_VoA_fit$cmdstan_diagnose())

  ### extracting parameters
  ST_VoA_pars <- ST_VoA_fit$draws(
    variables = c(
      "b0",
      "beta_net_st_epa",
      "beta_net_kick_return_avg",
      "beta_net_punt_return_avg",
      "beta_net_fg_rate",
      "beta_net_xp_rate",
      "sigma"
    ),
    format = "draws_df"
  )

  ### creating matrix to store special teams VoA_Ratings
  ST_VoA_Ratings <- matrix(
    NA,
    nrow = length(ST_VoA_pars$b0),
    ncol = nrow(VoAVariables)
  )

  ### creating special teams VoA_Ratings
  set.seed(802)
  for (p in 1:length(ST_VoA_pars$b0)) {
    for (t in 1:nrow(VoAVariables)) {
      ST_VoA_Rating <- rnorm(
        1,
        mean = ST_VoA_pars$b0[p] +
          ST_VoA_pars$beta_net_st_epa[p] * VoAVariables$st_net_epa[t] +
          ST_VoA_pars$beta_net_kick_return_avg[p] *
            VoAVariables$net_kick_return_yds[t] +
          ST_VoA_pars$beta_net_punt_return_avg[p] *
            VoAVariables$net_punt_return_yds[t] +
          ST_VoA_pars$beta_net_fg_rate[p] * VoAVariables$net_fg_rate[t] +
          ST_VoA_pars$beta_net_xp_rate[p] * VoAVariables$net_xp_rate[t],
        sd = ST_VoA_pars$sigma[p]
      )
      ST_VoA_Ratings[p, t] <- ST_VoA_Rating
    }
  }

  ### generating median and mean and quantile ratings
  MeanPred <- apply(ST_VoA_Ratings, 2, mean)
  MedianPred <- apply(ST_VoA_Ratings, 2, median)
  Upper <- apply(ST_VoA_Ratings, 2, quantile, prob = 0.975)
  Lower <- apply(ST_VoA_Ratings, 2, quantile, prob = 0.025)

  VoAVariables$STVoA_MeanRating <- MeanPred
  VoAVariables$STVoA_MedRating <- MedianPred
  VoAVariables$STVoA_95PctRating <- Upper
  VoAVariables$STVoA_05PctRating <- Lower
}

### making sure all values are > 0
for (i in 1:nrow(VoAVariables)) {
  set.seed(802)
  if (VoAVariables$OffVoA_MedRating[i] <= 0) {
    VoAVariables$OffVoA_MedRating[i] <- abs(VoAVariables$OffVoA_MedRating[
      i
    ]) +
      abs(rnorm(1, 1, 1))
  }
  if (VoAVariables$DefVoA_MedRating[i] <= 0) {
    VoAVariables$DefVoA_MedRating[i] <- abs(VoAVariables$DefVoA_MedRating[
      i
    ]) +
      abs(rnorm(1, 1, 1))
  }
}


##### Ranking VoA Rating columns #####
VoAVariables <- VoAVariables |>
  mutate(
    VoA_Rating_Ovr = OffVoA_MedRating - DefVoA_MedRating + STVoA_MedRating,
    VoA_Rating_05Pct = OffVoA_05PctRating -
      DefVoA_05PctRating +
      STVoA_05PctRating,
    VoA_Rating_95Pct = OffVoA_95PctRating -
      DefVoA_95PctRating +
      STVoA_95PctRating,
    VoA_Ranking_Ovr = dense_rank(desc(VoA_Rating_Ovr)),
    OffVoA_Ranking = dense_rank(desc(OffVoA_MedRating)),
    DefVoA_Ranking = dense_rank(DefVoA_MedRating),
    STVoA_Ranking = dense_rank(desc(STVoA_MedRating))
  )


### creating data frame with just team, VoA ratings, VoA Rankings, and VoA output
FinalTable <- VoAVariables |>
  select(
    team,
    week,
    VoA_Output,
    VoA_Rating_Ovr,
    VoA_Ranking_Ovr,
    OffVoA_MedRating,
    OffVoA_Ranking,
    DefVoA_MedRating,
    DefVoA_Ranking,
    STVoA_MedRating,
    STVoA_Ranking
  ) |>
  arrange(VoA_Ranking_Ovr)

##### Creating Table Arranged by VoA Rating #####
if (as.numeric(nfl_week) == 0) {
  ### Full table
  ## adding title and subtitle
  VoA_Table <- FinalTable |>
    gt() |> # use 'gt' to make an awesome table...
    gt_theme_538() |>
    tab_header(
      title = gt_title, # ...with this title
      subtitle = "Supremely Excellent Yet Salaciously Godlike And Infallibly Magnificent NFL Vortex of Accuracy"
    ) |> # and this subtitle
    ##tab_style(style = cell_fill("bisque"),
    ##        locations = cells_body()) |>  # add fill color to table
    fmt_number(
      # A column (numeric data)
      columns = c(VoA_Rating_Ovr), # What column variable?
      decimals = 3 # With four decimal places
    ) |>
    fmt_number(
      # A column (numeric data)
      columns = c(OffVoA_MedRating), # What column variable?
      decimals = 3 # With four decimal places
    ) |>
    fmt_number(
      # A column (numeric data)
      columns = c(DefVoA_MedRating), # What column variable?
      decimals = 3 # With four decimal places
    ) |>
    fmt_number(
      # A column (numeric data)
      columns = c(STVoA_MedRating), # What column variable?
      decimals = 3 # With four decimal places
    ) |>
    fmt_number(
      # Another column (also numeric data)
      columns = c(VoA_Ranking_Ovr), # What column variable? FinalVoATop25$VoA_Ranking
      decimals = 0 # I want this column to have zero decimal places
    ) |>
    data_color(
      # Update cell colors, testing different color palettes
      columns = c(VoA_Rating_Ovr), # ...for dose column
      fn = scales::col_numeric(
        # <- bc it's numeric
        palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    data_color(
      # Update cell colors, testing different color palettes
      columns = c(OffVoA_MedRating), # ...for dose column
      fn = scales::col_numeric(
        # <- bc it's numeric
        palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    data_color(
      # Update cell colors, testing different color palettes
      columns = c(DefVoA_MedRating), # ...for dose column
      fn = scales::col_numeric(
        # <- bc it's numeric
        palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = TRUE
      )
    ) |>
    data_color(
      # Update cell colors, testing different color palettes
      columns = c(STVoA_MedRating), # ...for dose column
      fn = scales::col_numeric(
        # <- bc it's numeric
        palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    gt_nfl_wordmarks(columns = "team") |>
    cols_label(
      VoA_Rating_Ovr = "Overall VoA Rating",
      VoA_Ranking_Ovr = "VoA Ranking",
      OffVoA_MedRating = "Off VoA Rating",
      OffVoA_Ranking = "Off Ranking",
      DefVoA_MedRating = "Def VoA Rating",
      DefVoA_Ranking = "Def Ranking",
      STVoA_MedRating = "ST VoA Rating",
      STVoA_Ranking = "ST Ranking"
    ) |> # Update labels
    # cols_move_to_end(columns = "VoA_Rating_Ovr") |>
    cols_hide(c(week, VoA_Output)) |>
    tab_footnote(
      footnote = "Table by @gshelor, Data from nflfastR"
    )
} else {
  ### Full table
  # adding title and subtitle
  VoA_Table <- FinalTable |>
    gt() |> # use 'gt' to make an awesome table...
    gt_theme_538() |>
    tab_header(
      title = gt_title, # ...with this title
      subtitle = "Supremely Excellent Yet Salaciously Godlike And Infallibly Magnificent Vortex of Accuracy"
    ) |> # and this subtitle
    ##tab_style(style = cell_fill("bisque"),
    ##        locations = cells_body()) |>  # add fill color to table
    fmt_number(
      # A column (numeric data)
      columns = c(VoA_Rating_Ovr), # What column variable? FinalVoATop25$VoA_Rating
      decimals = 3 # With four decimal places
    ) |>
    fmt_number(
      # A column (numeric data)
      columns = c(OffVoA_MedRating), # What column variable? FinalVoATop25$VoA_Rating
      decimals = 3 # With four decimal places
    ) |>
    fmt_number(
      # A column (numeric data)
      columns = c(DefVoA_MedRating), # What column variable? FinalVoATop25$VoA_Rating
      decimals = 3 # With four decimal places
    ) |>
    fmt_number(
      # A column (numeric data)
      columns = c(STVoA_MedRating), # What column variable? FinalVoATop25$VoA_Rating
      decimals = 3 # With four decimal places
    ) |>
    fmt_number(
      # Another column (also numeric data)
      columns = c(VoA_Ranking_Ovr), # What column variable? FinalVoATop25$VoA_Ranking
      decimals = 0 # I want this column to have zero decimal places
    ) |>
    data_color(
      # Update cell colors, testing different color palettes
      columns = c(VoA_Rating_Ovr), # ...for dose column
      fn = scales::col_numeric(
        # <- bc it's numeric
        palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    data_color(
      # Update cell colors, testing different color palettes
      columns = c(OffVoA_MedRating), # ...for dose column
      fn = scales::col_numeric(
        # <- bc it's numeric
        palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    data_color(
      # Update cell colors, testing different color palettes
      columns = c(DefVoA_MedRating), # ...for dose column
      fn = scales::col_numeric(
        # <- bc it's numeric
        palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = TRUE
      )
    ) |>
    data_color(
      # Update cell colors, testing different color palettes
      columns = c(STVoA_MedRating), # ...for dose column
      fn = scales::col_numeric(
        # <- bc it's numeric
        palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    gt_nfl_wordmarks(columns = "team") |>
    cols_label(
      VoA_Rating_Ovr = "Overall VoA Rating",
      VoA_Ranking_Ovr = "VoA Ranking",
      OffVoA_MedRating = "Off VoA Rating",
      OffVoA_Ranking = "Off Ranking",
      DefVoA_MedRating = "Def VoA Rating",
      DefVoA_Ranking = "Def Ranking",
      STVoA_MedRating = "ST VoA Rating",
      STVoA_Ranking = "ST Ranking"
    ) |> # Update labels
    # cols_move_to_end(columns = "VoA_Rating") |>
    cols_hide(c(week, VoA_Output)) |>
    tab_footnote(
      footnote = "Table by @gshelor, Data from nflfastR"
    )
}

VoA_Table
VoA_Table |>
  gtsave(
    table_file_pathway,
    expand = 5,
    path = output_dir
  )


##### Exporting final dataframe as csv #####
write_csv(VoAVariables, file_pathway)


##### Setting up the Unintelligible Charts #####
### Tracks VoA Ratings and Rankings by week
### now reading in and merging VoA rating and ranking data up to current week
### changing FinalTable to only be columns needed for Unintelligible Charts
FinalTable <- FinalTable |>
  select(team, week, VoA_Output, VoA_Ranking_Ovr, VoA_Rating_Ovr)
if (as.numeric(nfl_week) == 3) {
  Week0_VoA <- read_csv(here(
    "Data",
    paste0("VoA", season),
    paste0(season, "Week0_VoA.csv")
  )) |>
    select(team, week, VoA_Output, VoA_Ranking_Ovr, VoA_Rating_Ovr)
  Week1_VoA <- read_csv(here(
    "Data",
    paste0("VoA", season),
    paste0(season, "Week1_VoA.csv")
  )) |>
    select(team, week, VoA_Output, VoA_Ranking_Ovr, VoA_Rating_Ovr)
  Week2_VoA <- read_csv(here(
    "Data",
    paste0("VoA", season),
    paste0(season, "Week2_VoA.csv")
  )) |>
    select(team, week, VoA_Output, VoA_Ranking_Ovr, VoA_Rating_Ovr)
  Ratings_Rks <- rbind(
    Week0_VoA,
    rbind(Week1_VoA, rbind(Week2_VoA, FinalTable))
  )
  write_csv(
    Ratings_Rks,
    paste(
      data_dir,
      "/TrackingChartCSVs",
      "/",
      season,
      week_text,
      "0_3Ratings_Rks.csv",
      sep = ""
    )
  )
} else if (as.numeric(nfl_week) == 4) {
  Ratings_Rks <- read_csv(here(
    "Data",
    paste0("VoA", season),
    "TrackingChartCSVs",
    paste(season, week_text, "0_3Ratings_Rks.csv", sep = "")
  )) |>
    select(team, week, VoA_Output, VoA_Ranking_Ovr, VoA_Rating_Ovr)
  Ratings_Rks <- rbind(Ratings_Rks, FinalTable)
  write_csv(
    Ratings_Rks,
    paste(
      data_dir,
      "/TrackingChartCSVs",
      "/",
      season,
      week_text,
      "0_4Ratings_Rks.csv",
      sep = ""
    )
  )
} else if (as.numeric(nfl_week) == 5) {
  Ratings_Rks <- read_csv(here(
    "Data",
    paste0("VoA", season),
    "TrackingChartCSVs",
    paste(season, week_text, "0_4Ratings_Rks.csv", sep = "")
  )) |>
    select(team, week, VoA_Output, VoA_Ranking_Ovr, VoA_Rating_Ovr)
  Ratings_Rks <- rbind(Ratings_Rks, FinalTable)
  write_csv(
    Ratings_Rks,
    paste(
      data_dir,
      "/TrackingChartCSVs",
      "/",
      season,
      week_text,
      "0_5Ratings_Rks.csv",
      sep = ""
    )
  )
} else if (as.numeric(nfl_week) == 6) {
  Ratings_Rks <- read_csv(here(
    "Data",
    paste0("VoA", season),
    "TrackingChartCSVs",
    paste(season, week_text, "0_5Ratings_Rks.csv", sep = "")
  )) |>
    select(team, week, VoA_Output, VoA_Ranking_Ovr, VoA_Rating_Ovr)
  Ratings_Rks <- rbind(Ratings_Rks, FinalTable)
  write_csv(
    Ratings_Rks,
    paste(
      data_dir,
      "/TrackingChartCSVs",
      "/",
      season,
      week_text,
      "0_6Ratings_Rks.csv",
      sep = ""
    )
  )
} else if (as.numeric(nfl_week) == 7) {
  Ratings_Rks <- read_csv(here(
    "Data",
    paste0("VoA", season),
    "TrackingChartCSVs",
    paste(season, week_text, "0_6Ratings_Rks.csv", sep = "")
  )) |>
    select(team, week, VoA_Output, VoA_Ranking_Ovr, VoA_Rating_Ovr)
  Ratings_Rks <- rbind(Ratings_Rks, FinalTable)
  write_csv(
    Ratings_Rks,
    paste(
      data_dir,
      "/TrackingChartCSVs",
      "/",
      season,
      week_text,
      "0_7Ratings_Rks.csv",
      sep = ""
    )
  )
} else if (as.numeric(nfl_week) == 8) {
  Ratings_Rks <- read_csv(here(
    "Data",
    paste0("VoA", season),
    "TrackingChartCSVs",
    paste(season, week_text, "0_7Ratings_Rks.csv", sep = "")
  )) |>
    select(team, week, VoA_Output, VoA_Ranking_Ovr, VoA_Rating_Ovr)
  Ratings_Rks <- rbind(Ratings_Rks, FinalTable)
  write_csv(
    Ratings_Rks,
    paste(
      data_dir,
      "/TrackingChartCSVs",
      "/",
      season,
      week_text,
      "0_8Ratings_Rks.csv",
      sep = ""
    )
  )
} else if (as.numeric(nfl_week) == 9) {
  Ratings_Rks <- read_csv(here(
    "Data",
    paste0("VoA", season),
    "TrackingChartCSVs",
    paste(season, week_text, "0_8Ratings_Rks.csv", sep = "")
  )) |>
    select(team, week, VoA_Output, VoA_Ranking_Ovr, VoA_Rating_Ovr)
  Ratings_Rks <- rbind(Ratings_Rks, FinalTable)
  write_csv(
    Ratings_Rks,
    paste(
      data_dir,
      "/TrackingChartCSVs",
      "/",
      season,
      week_text,
      "0_9Ratings_Rks.csv",
      sep = ""
    )
  )
} else if (as.numeric(nfl_week) == 10) {
  Ratings_Rks <- read_csv(here(
    "Data",
    paste0("VoA", season),
    "TrackingChartCSVs",
    paste(season, week_text, "0_9Ratings_Rks.csv", sep = "")
  )) |>
    select(team, week, VoA_Output, VoA_Ranking_Ovr, VoA_Rating_Ovr)
  Ratings_Rks <- rbind(Ratings_Rks, FinalTable)
  write_csv(
    Ratings_Rks,
    paste(
      data_dir,
      "/TrackingChartCSVs",
      "/",
      season,
      week_text,
      "0_10Ratings_Rks.csv",
      sep = ""
    )
  )
} else if (as.numeric(nfl_week) == 11) {
  Ratings_Rks <- read_csv(here(
    "Data",
    paste0("VoA", season),
    "TrackingChartCSVs",
    paste(season, week_text, "0_10Ratings_Rks.csv", sep = "")
  )) |>
    select(team, week, VoA_Output, VoA_Ranking_Ovr, VoA_Rating_Ovr)
  Ratings_Rks <- rbind(Ratings_Rks, FinalTable)
  write_csv(
    Ratings_Rks,
    paste(
      data_dir,
      "/TrackingChartCSVs",
      "/",
      season,
      week_text,
      "0_11Ratings_Rks.csv",
      sep = ""
    )
  )
} else if (as.numeric(nfl_week) == 12) {
  Ratings_Rks <- read_csv(here(
    "Data",
    paste0("VoA", season),
    "TrackingChartCSVs",
    paste(season, week_text, "0_11Ratings_Rks.csv", sep = "")
  )) |>
    select(team, week, VoA_Output, VoA_Ranking_Ovr, VoA_Rating_Ovr)
  Ratings_Rks <- rbind(Ratings_Rks, FinalTable)
  write_csv(
    Ratings_Rks,
    paste(
      data_dir,
      "/TrackingChartCSVs",
      "/",
      season,
      week_text,
      "0_12Ratings_Rks.csv",
      sep = ""
    )
  )
} else if (as.numeric(nfl_week) == 13) {
  Ratings_Rks <- read_csv(here(
    "Data",
    paste0("VoA", season),
    "TrackingChartCSVs",
    paste(season, week_text, "0_12Ratings_Rks.csv", sep = "")
  )) |>
    select(team, week, VoA_Output, VoA_Ranking_Ovr, VoA_Rating_Ovr)
  Ratings_Rks <- rbind(Ratings_Rks, FinalTable)
  write_csv(
    Ratings_Rks,
    paste(
      data_dir,
      "/TrackingChartCSVs",
      "/",
      season,
      week_text,
      "0_13Ratings_Rks.csv",
      sep = ""
    )
  )
} else if (as.numeric(nfl_week) == 14) {
  Ratings_Rks <- read_csv(here(
    "Data",
    paste0("VoA", season),
    "TrackingChartCSVs",
    paste(season, week_text, "0_13Ratings_Rks.csv", sep = "")
  )) |>
    select(team, week, VoA_Output, VoA_Ranking_Ovr, VoA_Rating_Ovr)
  Ratings_Rks <- rbind(Ratings_Rks, FinalTable)
  write_csv(
    Ratings_Rks,
    paste(
      data_dir,
      "/TrackingChartCSVs",
      "/",
      season,
      week_text,
      "0_14Ratings_Rks.csv",
      sep = ""
    )
  )
} else if (as.numeric(nfl_week) == 15) {
  Ratings_Rks <- read_csv(here(
    "Data",
    paste0("VoA", season),
    "TrackingChartCSVs",
    paste(season, week_text, "0_14Ratings_Rks.csv", sep = "")
  )) |>
    select(team, week, VoA_Output, VoA_Ranking_Ovr, VoA_Rating_Ovr)
  Ratings_Rks <- rbind(Ratings_Rks, FinalTable)
  write_csv(
    Ratings_Rks,
    paste(
      data_dir,
      "/TrackingChartCSVs",
      "/",
      season,
      week_text,
      "0_15Ratings_Rks.csv",
      sep = ""
    )
  )
} else if (as.numeric(nfl_week) == 16) {
  Ratings_Rks <- read_csv(here(
    "Data",
    paste0("VoA", season),
    "TrackingChartCSVs",
    paste(season, week_text, "0_15Ratings_Rks.csv", sep = "")
  )) |>
    select(team, week, VoA_Output, VoA_Ranking_Ovr, VoA_Rating_Ovr)
  Ratings_Rks <- rbind(Ratings_Rks, FinalTable)
  write_csv(
    Ratings_Rks,
    paste(
      data_dir,
      "/TrackingChartCSVs",
      "/",
      season,
      week_text,
      "0_16Ratings_Rks.csv",
      sep = ""
    )
  )
} else if (as.numeric(nfl_week) == 17) {
  Ratings_Rks <- read_csv(here(
    "Data",
    paste0("VoA", season),
    "TrackingChartCSVs",
    paste(season, week_text, "0_16Ratings_Rks.csv", sep = "")
  )) |>
    select(team, week, VoA_Output, VoA_Ranking_Ovr, VoA_Rating_Ovr)
  Ratings_Rks <- rbind(Ratings_Rks, FinalTable)
  write_csv(
    Ratings_Rks,
    paste(
      data_dir,
      "/TrackingChartCSVs",
      "/",
      season,
      week_text,
      "0_17Ratings_Rks.csv",
      sep = ""
    )
  )
} else if (as.numeric(nfl_week) == 18) {
  Ratings_Rks <- read_csv(here(
    "Data",
    paste0("VoA", season),
    "TrackingChartCSVs",
    paste(season, week_text, "0_17Ratings_Rks.csv", sep = "")
  )) |>
    select(team, week, VoA_Output, VoA_Ranking_Ovr, VoA_Rating_Ovr)
  Ratings_Rks <- rbind(Ratings_Rks, FinalTable)
  write_csv(
    Ratings_Rks,
    paste(
      data_dir,
      "/TrackingChartCSVs",
      "/",
      season,
      week_text,
      "0_18Ratings_Rks.csv",
      sep = ""
    )
  )
} else if (as.numeric(nfl_week) == 19) {
  Ratings_Rks <- read_csv(here(
    "Data",
    paste0("VoA", season),
    "TrackingChartCSVs",
    paste(season, week_text, "0_18Ratings_Rks.csv", sep = "")
  )) |>
    select(team, week, VoA_Output, VoA_Ranking_Ovr, VoA_Rating_Ovr)
  Ratings_Rks <- rbind(Ratings_Rks, FinalTable)
  write_csv(
    Ratings_Rks,
    paste(
      data_dir,
      "/TrackingChartCSVs",
      "/",
      season,
      week_text,
      "0_19Ratings_Rks.csv",
      sep = ""
    )
  )
} else if (as.numeric(nfl_week) == 20) {
  Ratings_Rks <- read_csv(here(
    "Data",
    paste0("VoA", season),
    "TrackingChartCSVs",
    paste(season, week_text, "0_19Ratings_Rks.csv", sep = "")
  )) |>
    select(team, week, VoA_Output, VoA_Ranking_Ovr, VoA_Rating_Ovr)
  Ratings_Rks <- rbind(Ratings_Rks, FinalTable)
  write_csv(
    Ratings_Rks,
    paste(
      data_dir,
      "/TrackingChartCSVs",
      "/",
      season,
      week_text,
      "0_20Ratings_Rks.csv",
      sep = ""
    )
  )
} else if (as.numeric(nfl_week) == 21) {
  Ratings_Rks <- read_csv(here(
    "Data",
    paste0("VoA", season),
    "TrackingChartCSVs",
    paste(season, week_text, "0_20Ratings_Rks.csv", sep = "")
  )) |>
    select(team, week, VoA_Output, VoA_Ranking_Ovr, VoA_Rating_Ovr)
  Ratings_Rks <- rbind(Ratings_Rks, FinalTable)
  write_csv(
    Ratings_Rks,
    paste(
      data_dir,
      "/TrackingChartCSVs",
      "/",
      season,
      week_text,
      "0_21Ratings_Rks.csv",
      sep = ""
    )
  )
} else if (as.numeric(nfl_week) == 22) {
  Ratings_Rks <- read_csv(here(
    "Data",
    paste0("VoA", season),
    "TrackingChartCSVs",
    paste(season, week_text, "0_21Ratings_Rks.csv", sep = "")
  )) |>
    select(team, week, VoA_Output, VoA_Ranking_Ovr, VoA_Rating_Ovr)
  Ratings_Rks <- rbind(Ratings_Rks, FinalTable)
  write_csv(
    Ratings_Rks,
    paste(
      data_dir,
      "/TrackingChartCSVs",
      "/",
      season,
      week_text,
      "0_22Ratings_Rks.csv",
      sep = ""
    )
  )
} else {
  print(
    "No charts until Week 3! or maybe there's another week before the super bowl"
  )
}

##### Creating Charts #####
### charting VoA_Rating and VoA_Ranking for each week from week 3 on
if (as.numeric(nfl_week) >= 3) {
  ### creating rating chart
  VoA_Rating_Chart <- ggplot(
    Ratings_Rks,
    aes(x = week, y = VoA_Rating_Ovr, group = team)
  ) +
    theme_bw() +
    geom_line(linewidth = 1.5) +
    # geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA Overall Rating") +
    labs(caption = "chart by @gshelor, data from nflfastR") +
    ggtitle("Vortex of Accuracy Overall Ratings by Week") +
    expand_limits(
      y = c(
        floor(floor(min(VoAVariables$VoA_Rating_Ovr)) / 10) * 10,
        ceiling((ceiling(max(VoAVariables$VoA_Rating_Ovr)) / 10)) * 10
      )
    ) +
    scale_y_continuous(
      breaks = seq(
        (floor((floor(min(VoAVariables$VoA_Rating_Ovr)) / 10)) * 10),
        (ceiling((ceiling(max(VoAVariables$VoA_Rating_Ovr)) / 10)) * 10),
        by = 5
      )
    ) +
    scale_x_continuous(
      breaks = c(
        0,
        1,
        2,
        3,
        4,
        5,
        6,
        7,
        8,
        9,
        10,
        11,
        12,
        13,
        14,
        15,
        16,
        17,
        18,
        19,
        20,
        21,
        22,
        23,
        24,
        25
      )
    ) +
    geom_nfl_logos(aes(team_abbr = team), width = 0.04) +
    # geom_cfb_logos(aes(team = team, width = 0.035)) +
    theme(
      plot.title = element_text(size = 35, hjust = 0.5),
      axis.text.x = element_text(size = 20),
      axis.text.y = element_text(size = 20),
      axis.title.x = element_text(size = 22),
      axis.title.y = element_text(size = 22),
      legend.text = element_text(size = 20)
    )
  VoA_Rating_Chart
  ggsave(
    Output_filename,
    path = output_dir,
    width = 50,
    height = 40,
    units = 'cm'
  )

  VoA_Ranking_Chart <- ggplot(
    Ratings_Rks,
    aes(x = week, y = VoA_Ranking_Ovr, group = team)
  ) +
    theme_bw() +
    geom_line(linewidth = 1.5) +
    # geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA Ranking") +
    labs(caption = "chart by @gshelor, data from nflfastR") +
    ggtitle("NFL Vortex of Accuracy Rankings by Week") +
    expand_limits(y = c(0, 32)) +
    scale_y_continuous(breaks = c(0, 4, 8, 12, 16, 20, 24, 28, 32)) +
    scale_x_continuous(
      breaks = c(
        0,
        1,
        2,
        3,
        4,
        5,
        6,
        7,
        8,
        9,
        10,
        11,
        12,
        13,
        14,
        15,
        16,
        17,
        18,
        19,
        20,
        21,
        22,
        23,
        24,
        25
      )
    ) +
    geom_nfl_logos(aes(team_abbr = team), width = 0.04) +
    theme(
      plot.title = element_text(size = 35, hjust = 0.5),
      axis.text.x = element_text(size = 20),
      axis.text.y = element_text(size = 20),
      axis.title.x = element_text(size = 22),
      axis.title.y = element_text(size = 22),
      legend.text = element_text(size = 20)
    )
  VoA_Ranking_Chart
  ggsave(
    Ranking_filename,
    path = output_dir,
    width = 50,
    height = 40,
    units = 'cm'
  )
} else {
  print("no charts until week 3!")
}

##### creating histogram of VoA Ratings #####
Rating_histogram <- ggplot(VoAVariables, aes(VoA_Rating_Ovr)) +
  theme_bw() +
  geom_histogram(binwidth = 2, col = "black", fill = "orange") +
  scale_x_continuous(breaks = seq(-40, 40, 5)) +
  scale_y_continuous(breaks = seq(0, 32, 4)) +
  ggtitle(hist_title) +
  xlab("VoA Rating") +
  ylab("Frequency") +
  labs(caption = "chart by @gshelor, data from nflfastR") +
  theme(
    plot.title = element_text(size = 35, hjust = 0.5),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    axis.title.x = element_text(size = 22),
    axis.title.y = element_text(size = 22),
    legend.text = element_text(size = 20)
  )
Rating_histogram
ggsave(hist_filename, path = output_dir, width = 50, height = 40, units = 'cm')


### Creating Scatterplot of VoA_Output vs VoA_Rating
VoA_Output_Rating_plot <- ggplot(
  VoAVariables,
  aes(x = VoA_Output, y = VoA_Rating_Ovr)
) +
  theme_bw() +
  # geom_point(size = 5) +
  geom_nfl_logos(aes(team_abbr = team), width = 0.05) +
  geom_smooth() +
  scale_x_continuous(breaks = seq(0, 32, 2)) +
  scale_y_continuous(breaks = seq(-40, 40, 5)) +
  ggtitle(Output_Rating_Plot_title) +
  xlab("VoA Output") +
  ylab("VoA Overall Rating") +
  labs(caption = "chart by @gshelor, data from nflfastR") +
  theme(
    plot.title = element_text(size = 35, hjust = 0.5),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    axis.title.x = element_text(size = 22),
    axis.title.y = element_text(size = 22),
    legend.text = element_text(size = 20)
  )
VoA_Output_Rating_plot
ggsave(
  Output_Rating_Plot_filename,
  path = output_dir,
  width = 50,
  height = 40,
  units = 'cm'
)

### Creating Scatterplot of VoA Offensive Ratings vs VoA Defensive Ratings
VoA_OffDef_Rating_plot <- ggplot(
  VoAVariables,
  aes(x = OffVoA_MeanRating, y = DefVoA_MeanRating)
) +
  theme_bw() +
  # geom_point(size = 1) +
  geom_nfl_logos(aes(team_abbr = team), width = 0.05) +
  geom_hline(yintercept = median(VoAVariables$DefVoA_MeanRating)) +
  geom_vline(xintercept = median(VoAVariables$OffVoA_MeanRating)) +
  scale_y_reverse() +
  # scale_x_continuous(breaks = seq(0,32,2)) +
  # scale_y_continuous(breaks = seq(-40,40,5)) +
  ggtitle(OffDef_Rating_Plot_title) +
  xlab("VoA Offensive Rating") +
  ylab("VoA Defensive Rating") +
  labs(caption = "chart by @gshelor, data from nflfastR") +
  theme(
    plot.title = element_text(size = 35, hjust = 0.5),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    axis.title.x = element_text(size = 22),
    axis.title.y = element_text(size = 22),
    legend.text = element_text(size = 20)
  )
VoA_OffDef_Rating_plot
ggsave(
  OffDef_Rating_Plot_filename,
  path = output_dir,
  width = 50,
  height = 40,
  units = 'cm'
)


EndTime <- Sys.time()
EndTime - StartTime
##### End of Script #####

##### POOPYPANTS TESTING, PLEASE IGNORE #####
# fmt: skip
poopypants <- read_csv(here("Data", "VoA2025", "2025Week20_VoA.csv"))
# fmt: skip
nfl_adj_stats <- poopypants |>
  select(team, off_epa, off_explosiveness, off_ypp, def_epa, def_explosiveness, def_ypp, starts_with("adj_")
  )

PBP_EPAAdjustment <- PY1_rushpass_plays |>
  select(game_id, home_team, away_team, posteam, defteam, epa, location) |>
  mutate(
    hfa = as.factor(case_when(
      location == "Neutral" ~ 0,
      posteam == home_team ~ 1,
      TRUE ~ -1
    )),
    ### home team on offense
    posteam = as.factor(posteam),
    ### home team on defense
    defteam = as.factor(defteam)
  ) |>
  drop_na()
