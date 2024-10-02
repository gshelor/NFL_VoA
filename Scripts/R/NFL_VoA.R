##### NFL Vortex of Accuracy Version 0.1 #####
### Supremely Excellent Yet Salaciously Godlike And Infallibly Magnificent Vortex of Accuracy
### This script is for rating NFL teams by unit (offense, defense, special teams) in order to create an overall Vortex of Accuracy rating, which is intended to represent the amount of points a given team would beat the hypothetical average NFL team by on a neutral field

##### loading packages #####
StartTime <- Sys.time()
library(pacman)
p_load(tidyverse, gt, nflfastR, nflverse, here, gtExtras, rstan, ggpubr, webshot2, parallel, RColorBrewer)

### Creating week and season strings
season <- readline(prompt = "What season is it? ")
week <- readline(prompt = "What week is it? ")


##### setting strings for table titles, file pathways, unintelligible charts #####
`%nin%` = Negate(`%in%`)
output_dir <- here("Outputs", "RVoA", paste0("VoA", season))
data_dir <- here("Data", paste("VoA", season, sep = ""))
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
Output_Rating_Plot_png <- "Output_Rating.png"

hist_title <- paste(season, week_text, week, nfl_text, VoA_text, "Ratings")
Output_Rating_Plot_title <- paste(season, week_text, week, Output_Rating_Plot_text)
table_file_pathway <- paste(season, week_text, week, "_", fulltable_png, sep = "")
Output_filename <- paste(season, week_text, week, nfl_text, Rating_text, sep = "")
Ranking_filename <- paste(season, week_text, week, nfl_text, Ranking_text, sep = "")
hist_filename <- paste(season, week_text, week, "_", nfl_text, Histogram_text, sep = "")
Output_Rating_Plot_filename <- paste(season, week_text, week, "_", Output_Rating_Plot_png, sep = "")
### creating string for csv spreadsheet pathway
file_pathway <- paste(data_dir, "/", season, week_text, week,"_", VoAString, sep = "")

### storing PYx numbers here whether I need them or not because for some reason the filter only works with specific numbers, not "functions" like "as.numeric(season) - 1" or something
### kinda annoying honestly
PY1 <- as.numeric(season) - 1
PY2 <- as.numeric(season) - 2
PY3 <- as.numeric(season) - 3

##### reading in data #####
if (as.numeric(week) == 0){
  ##### Week 0 (Preseason) Data Pull #####
  ### reading in PBP data
  PY_PBP <- nflfastR::load_pbp((as.numeric(season) - 3):(as.numeric(season) - 1)) |>
    filter(play_type_nfl != "GAME_START" & play_type_nfl != "TIMEOUT" & play_type_nfl != "END_QUARTER" & play_type_nfl != "END_GAME")
  ### extracting PY data by season
  PBP_PY1 <- PY_PBP |>
    filter(season == PY1)
  PBP_PY2 <- PY_PBP |>
    filter(season == PY2)
  PBP_PY3 <- PY_PBP |>
    filter(season == PY3)
  
  
  ### offensive and defensive plays
  PY1_rushpass_plays <- PBP_PY1 |>
    filter(play_type %in% c("run", "pass")) |>
    drop_na(epa) |>
    drop_na(yards_gained)
  PY1_success_plays <- PY1_rushpass_plays |>
    filter(play_type_nfl != "INTERCEPTION") |>
    filter((down == 1 & (yards_gained >= (ydstogo / 2))) | (down == 2 & (yards_gained >= (ydstogo * 0.7))) | (down > 2 & (yards_gained >= ydstogo)))
  PY1_TDs <- PY1_rushpass_plays |>
    filter(touchdown == 1 & play_type_nfl != "INTERCEPTION" & play_type_nfl != "FUMBLE_RECOVERED_BY_OPPONENT" & fumble_lost == 0 & interception == 0)
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
    filter((down == 1 & (yards_gained >= (ydstogo / 2))) | (down == 2 & (yards_gained >= (ydstogo * 0.7))) | (down > 2 & (yards_gained >= ydstogo)))
  PY2_TDs <- PY2_rushpass_plays |>
    filter(touchdown == 1 & play_type_nfl != "INTERCEPTION" & play_type_nfl != "FUMBLE_RECOVERED_BY_OPPONENT" & fumble_lost == 0 & interception == 0)
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
    filter((down == 1 & (yards_gained >= (ydstogo / 2))) | (down == 2 & (yards_gained >= (ydstogo * 0.7))) | (down > 2 & (yards_gained >= ydstogo)))
  PY3_TDs <- PY3_rushpass_plays |>
    filter(touchdown == 1 & play_type_nfl != "INTERCEPTION" & play_type_nfl != "FUMBLE_RECOVERED_BY_OPPONENT" & fumble_lost == 0 & interception == 0)
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
  
  ### creating dataframe to eventually store VoA Variables and ratings
  VoA_Variables <- data.frame(season = rep(as.numeric(season), 32),
                              week = rep(as.numeric(week), 32),
                              team = unique(PY_PBP$home_team),
                              off_ypp_PY1 = -999,
                              off_epa_PY1 = -999,
                              off_success_rt_PY1 = -999,
                              off_explosiveness_PY1 = -999,
                              off_third_conv_rate_PY1 = -999,
                              off_fourth_conv_rate_PY1 = -999,
                              off_pass_ypa_PY1 = -999,
                              off_pass_ypc_PY1 = -999,
                              off_rush_ypa_PY1 = -999,
                              off_pts_per_opp_PY1 = -999,
                              off_turnovers_PY1 = -999,
                              off_plays_pg_PY1 = -999,
                              off_ppg_PY1 = -999,
                              def_ypp_PY1 = -999,
                              def_epa_PY1 = -999,
                              def_success_rt_PY1 = -999,
                              def_explosiveness_PY1 = -999,
                              def_third_conv_rate_PY1 = -999,
                              def_fourth_conv_rate_PY1 = -999,
                              def_pass_ypa_PY1 = -999,
                              def_pass_ypc_PY1 = -999,
                              def_rush_ypa_PY1 = -999,
                              def_pts_per_opp_PY1 = -999,
                              def_plays_pg_PY1 = -999,
                              def_ppg_PY1 = -999,
                              st_net_epa_PY1 = -999,
                              st_punt_return_yds_PY1 = -999,
                              st_kick_return_yds_PY1 = -999,
                              st_kick_return_TDs_PY1 = -999,
                              st_punt_return_TDs_PY1 = -999,
                              fg_rate_PY1 = -999,
                              fg_made_pg_PY1 = -999,
                              xp_rate_PY1 = -999,
                              xp_made_pg_PY1 = -999,
                              st_punt_return_yds_allowed_PY1 = -999,
                              st_kick_return_yds_allowed_PY1 = -999,
                              st_kick_return_TDs_allowed_PY1 = -999,
                              st_punt_return_TDs_allowed_PY1 = -999,
                              fg_rate_allowed_PY1 = -999,
                              fg_made_pg_allowed_PY1 = -999,
                              xp_rate_allowed_PY1 = -999,
                              xp_made_pg_allowed_PY1 = -999,
                              net_st_ppg_PY1 = -999,
                              off_ypp_PY2 = -999,
                              off_epa_PY2 = -999,
                              off_success_rt_PY2 = -999,
                              off_explosiveness_PY2 = -999,
                              off_third_conv_rate_PY2 = -999,
                              off_fourth_conv_rate_PY2 = -999,
                              off_pass_ypa_PY2 = -999,
                              off_pass_ypc_PY2 = -999,
                              off_rush_ypa_PY2 = -999,
                              off_pts_per_opp_PY2 = -999,
                              off_turnovers_PY2 = -999,
                              off_plays_pg_PY2 = -999,
                              off_ppg_PY2 = -999,
                              def_ypp_PY2 = -999,
                              def_epa_PY2 = -999,
                              def_success_rt_PY2 = -999,
                              def_explosiveness_PY2 = -999,
                              def_third_conv_rate_PY2 = -999,
                              def_fourth_conv_rate_PY2 = -999,
                              def_pass_ypa_PY2 = -999,
                              def_pass_ypc_PY2 = -999,
                              def_rush_ypa_PY2 = -999,
                              def_pts_per_opp_PY2 = -999,
                              def_plays_pg_PY2 = -999,
                              def_ppg_PY2 = -999,
                              st_net_epa_PY2 = -999,
                              st_punt_return_yds_PY2 = -999,
                              st_kick_return_yds_PY2 = -999,
                              st_kick_return_TDs_PY2 = -999,
                              st_punt_return_TDs_PY2 = -999,
                              fg_rate_PY2 = -999,
                              fg_made_pg_PY2 = -999,
                              xp_rate_PY2 = -999,
                              xp_made_pg_PY2 = -999,
                              st_punt_return_yds_allowed_PY2 = -999,
                              st_kick_return_yds_allowed_PY2 = -999,
                              st_kick_return_TDs_allowed_PY2 = -999,
                              st_punt_return_TDs_allowed_PY2 = -999,
                              fg_rate_allowed_PY2 = -999,
                              fg_made_pg_allowed_PY2 = -999,
                              xp_rate_allowed_PY2 = -999,
                              xp_made_pg_allowed_PY2 = -999,
                              net_st_ppg_PY2 = -999,
                              off_ypp_PY3 = -999,
                              off_epa_PY3 = -999,
                              off_success_rt_PY3 = -999,
                              off_explosiveness_PY3 = -999,
                              off_third_conv_rate_PY3 = -999,
                              off_fourth_conv_rate_PY3 = -999,
                              off_pass_ypa_PY3 = -999,
                              off_pass_ypc_PY3 = -999,
                              off_rush_ypa_PY3 = -999,
                              off_pts_per_opp_PY3 = -999,
                              off_turnovers_PY3 = -999,
                              off_plays_pg_PY3 = -999,
                              off_ppg_PY3 = -999,
                              def_ypp_PY3 = -999,
                              def_epa_PY3 = -999,
                              def_success_rt_PY3 = -999,
                              def_explosiveness_PY3 = -999,
                              def_third_conv_rate_PY3 = -999,
                              def_fourth_conv_rate_PY3 = -999,
                              def_pass_ypa_PY3 = -999,
                              def_pass_ypc_PY3 = -999,
                              def_rush_ypa_PY3 = -999,
                              def_pts_per_opp_PY3 = -999,
                              def_plays_pg_PY3 = -999,
                              def_ppg_PY3 = -999,
                              st_net_epa_PY3 = -999,
                              st_punt_return_yds_PY3 = -999,
                              st_kick_return_yds_PY3 = -999,
                              st_kick_return_TDs_PY3 = -999,
                              st_punt_return_TDs_PY3 = -999,
                              fg_rate_PY3 = -999,
                              fg_made_pg_PY3 = -999,
                              xp_rate_PY3 = -999,
                              xp_made_pg_PY3 = -999,
                              st_punt_return_yds_allowed_PY3 = -999,
                              st_kick_return_yds_allowed_PY3 = -999,
                              st_kick_return_TDs_allowed_PY3 = -999,
                              st_punt_return_TDs_allowed_PY3 = -999,
                              fg_rate_allowed_PY3 = -999,
                              fg_made_pg_allowed_PY3 = -999,
                              xp_rate_allowed_PY3 = -999,
                              xp_made_pg_allowed_PY3 = -999,
                              net_st_ppg_PY3 = -999)
} else if (as.numeric(week) <= 2){
  ##### Weeks 1-2 Data Pull #####
  ### reading in PY data saved in week 0
  PY_VoAVars <- read_csv(here("Data", paste0("VoA", season), "PYData", "PYData.csv")) |>
    select(team, off_ypp_PY1, off_epa_PY1, off_success_rt_PY1, off_explosiveness_PY1, off_third_conv_rate_PY1, off_fourth_conv_rate_PY1, off_pass_ypa_PY1, off_pass_ypc_PY1, off_rush_ypa_PY1, off_pts_per_opp_PY1, off_turnovers_PY1, off_plays_pg_PY1, off_ppg_PY1, def_ypp_PY1, def_epa_PY1, def_success_rt_PY1, def_explosiveness_PY1, def_third_conv_rate_PY1, def_fourth_conv_rate_PY1, def_pass_ypa_PY1, def_pass_ypc_PY1, def_rush_ypa_PY1, def_pts_per_opp_PY1, def_turnovers_PY1, def_plays_pg_PY1, def_ppg_PY1, st_net_epa_PY1, st_punt_return_yds_PY1, st_kick_return_yds_PY1, st_kick_return_TDs_PY1, st_punt_return_TDs_PY1, fg_rate_PY1, fg_made_pg_PY1, xp_rate_PY1, xp_made_pg_PY1, st_punt_return_yds_allowed_PY1, st_kick_return_yds_allowed_PY1, st_kick_return_TDs_allowed_PY1, st_punt_return_TDs_allowed_PY1, fg_rate_allowed_PY1, fg_made_pg_allowed_PY1, xp_rate_allowed_PY1, xp_made_pg_allowed_PY1, net_st_ppg_PY1, off_ypp_PY2, off_epa_PY2, off_success_rt_PY2, off_explosiveness_PY2, off_third_conv_rate_PY2, off_fourth_conv_rate_PY2, off_pass_ypa_PY2, off_pass_ypc_PY2, off_rush_ypa_PY2, off_pts_per_opp_PY2, off_turnovers_PY2, off_plays_pg_PY2, off_ppg_PY2, def_ypp_PY2, def_epa_PY2, def_success_rt_PY2, def_explosiveness_PY2, def_third_conv_rate_PY2, def_fourth_conv_rate_PY2, def_pass_ypa_PY2, def_pass_ypc_PY2, def_rush_ypa_PY2, def_pts_per_opp_PY2, def_turnovers_PY2, def_plays_pg_PY2, def_ppg_PY2, st_net_epa_PY2, st_punt_return_yds_PY2, st_kick_return_yds_PY2, st_kick_return_TDs_PY2, st_punt_return_TDs_PY2, fg_rate_PY2, fg_made_pg_PY2, xp_rate_PY2, xp_made_pg_PY2, st_punt_return_yds_allowed_PY2, st_kick_return_yds_allowed_PY2, st_kick_return_TDs_allowed_PY2, st_punt_return_TDs_allowed_PY2, fg_rate_allowed_PY2, fg_made_pg_allowed_PY2, xp_rate_allowed_PY2, xp_made_pg_allowed_PY2, net_st_ppg_PY2)
  ### reading in PBP data
  PBP <- nflfastR::load_pbp(as.numeric(season)) |>
    filter(play_type_nfl != "GAME_START" & play_type_nfl != "TIMEOUT" & play_type_nfl != "END_QUARTER" & play_type_nfl != "END_GAME")
  
  
  ### separting PBP into categories for stat extraction later
  ### offensive and defensive plays
  rushpass_plays <- PBP |>
    filter(play_type %in% c("run", "pass")) |>
    drop_na(epa) |>
    drop_na(yards_gained)
  success_plays <- rushpass_plays |>
    filter(play_type_nfl != "INTERCEPTION") |>
    filter((down == 1 & (yards_gained >= (ydstogo / 2))) | (down == 2 & (yards_gained >= (ydstogo * 0.7))) | (down > 2 & (yards_gained >= ydstogo)))
  TDs <- rushpass_plays |>
    filter(touchdown == 1 & play_type_nfl != "INTERCEPTION" & play_type_nfl != "FUMBLE_RECOVERED_BY_OPPONENT" & fumble_lost == 0 & interception == 0)
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
  kickoffs <- PBP |>
    filter(play_type == "kickoff")
  punts <- PBP |>
    filter(play_type == "punt")
  
  ### creating dataframe to eventually store VoA Variables and ratings
  VoA_Variables <- data.frame(season = rep(as.numeric(season), 32),
                              week = rep(as.numeric(week), 32),
                              team = unique(c(PBP$home_team, PBP$away_team)),
                              off_ypp = -999,
                              off_epa = -999,
                              off_success_rt = -999,
                              off_explosiveness = -999,
                              off_third_conv_rate = -999,
                              off_fourth_conv_rate = -999,
                              off_pass_ypa = -999,
                              off_pass_ypc = -999,
                              off_rush_ypa = -999,
                              off_pts_per_opp = -999,
                              off_turnovers = -999,
                              off_plays_pg = -999,
                              off_ppg = -999,
                              def_ypp = -999,
                              def_epa = -999,
                              def_success_rt = -999,
                              def_explosiveness = -999,
                              def_third_conv_rate = -999,
                              def_fourth_conv_rate = -999,
                              def_pass_ypa = -999,
                              def_pass_ypc = -999,
                              def_rush_ypa = -999,
                              def_pts_per_opp = -999,
                              def_plays_pg = -999,
                              def_ppg = -999,
                              st_net_epa = -999,
                              st_punt_return_yds = -999,
                              st_kick_return_yds = -999,
                              st_kick_return_TDs = -999,
                              st_punt_return_TDs = -999,
                              fg_rate = -999,
                              fg_made_pg = -999,
                              xp_rate = -999,
                              xp_made_pg = -999,
                              st_punt_return_yds_allowed = -999,
                              st_kick_return_yds_allowed = -999,
                              st_kick_return_TDs_allowed = -999,
                              st_punt_return_TDs_allowed = -999,
                              fg_rate_allowed = -999,
                              fg_made_pg_allowed = -999,
                              xp_rate_allowed = -999,
                              xp_made_pg_allowed = -999,
                              net_st_ppg = -999)
} else if (as.numeric(week) <= 5){
  ##### Weeks 3-5 Data Pull #####
  ### reading in PY data saved in week 0
  PY_VoAVars <- read_csv(here("Data", paste0("VoA", season), "PYData", "PYData.csv")) |>
    select(team, off_ypp_PY1, off_epa_PY1, off_success_rt_PY1, off_explosiveness_PY1, off_third_conv_rate_PY1, off_fourth_conv_rate_PY1, off_pass_ypa_PY1, off_pass_ypc_PY1, off_rush_ypa_PY1, off_pts_per_opp_PY1, off_turnovers_PY1, off_plays_pg_PY1, off_ppg_PY1, def_ypp_PY1, def_epa_PY1, def_success_rt_PY1, def_explosiveness_PY1, def_third_conv_rate_PY1, def_fourth_conv_rate_PY1, def_pass_ypa_PY1, def_pass_ypc_PY1, def_rush_ypa_PY1, def_pts_per_opp_PY1, def_turnovers_PY1, def_plays_pg_PY1, def_ppg_PY1, st_net_epa_PY1, st_punt_return_yds_PY1, st_kick_return_yds_PY1, st_kick_return_TDs_PY1, st_punt_return_TDs_PY1, fg_rate_PY1, fg_made_pg_PY1, xp_rate_PY1, xp_made_pg_PY1, st_punt_return_yds_allowed_PY1, st_kick_return_yds_allowed_PY1, st_kick_return_TDs_allowed_PY1, st_punt_return_TDs_allowed_PY1, fg_rate_allowed_PY1, fg_made_pg_allowed_PY1, xp_rate_allowed_PY1, xp_made_pg_allowed_PY1, net_st_ppg_PY1)
  
  ### reading in PBP data
  PBP <- nflfastR::load_pbp(as.numeric(season)) |>
    filter(play_type_nfl != "GAME_START" & play_type_nfl != "TIMEOUT" & play_type_nfl != "END_QUARTER" & play_type_nfl != "END_GAME")
  
  ### separting PBP into categories for stat extraction later
  ### offensive and defensive plays
  rushpass_plays <- PBP |>
    filter(play_type %in% c("run", "pass")) |>
    drop_na(epa) |>
    drop_na(yards_gained)
  success_plays <- rushpass_plays |>
    filter(play_type_nfl != "INTERCEPTION") |>
    filter((down == 1 & (yards_gained >= (ydstogo / 2))) | (down == 2 & (yards_gained >= (ydstogo * 0.7))) | (down > 2 & (yards_gained >= ydstogo)))
  TDs <- rushpass_plays |>
    filter(touchdown == 1 & play_type_nfl != "INTERCEPTION" & play_type_nfl != "FUMBLE_RECOVERED_BY_OPPONENT" & fumble_lost == 0 & interception == 0)
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
  kickoffs <- PBP |>
    filter(play_type == "kickoff")
  punts <- PBP |>
    filter(play_type == "punt")
  
  ### creating dataframe to eventually store VoA Variables and ratings
  VoA_Variables <- data.frame(season = rep(as.numeric(season), 32),
                              week = rep(as.numeric(week), 32),
                              team = unique(c(PBP$home_team, PBP$away_team)),
                              off_ypp = -999,
                              off_epa = -999,
                              off_success_rt = -999,
                              off_explosiveness = -999,
                              off_third_conv_rate = -999,
                              off_fourth_conv_rate = -999,
                              off_pass_ypa = -999,
                              off_pass_ypc = -999,
                              off_rush_ypa = -999,
                              off_pts_per_opp = -999,
                              off_turnovers = -999,
                              off_plays_pg = -999,
                              off_ppg = -999,
                              def_ypp = -999,
                              def_epa = -999,
                              def_success_rt = -999,
                              def_explosiveness = -999,
                              def_third_conv_rate = -999,
                              def_fourth_conv_rate = -999,
                              def_pass_ypa = -999,
                              def_pass_ypc = -999,
                              def_rush_ypa = -999,
                              def_pts_per_opp = -999,
                              def_plays_pg = -999,
                              def_ppg = -999,
                              st_net_epa = -999,
                              st_punt_return_yds = -999,
                              st_kick_return_yds = -999,
                              st_kick_return_TDs = -999,
                              st_punt_return_TDs = -999,
                              fg_rate = -999,
                              fg_made_pg = -999,
                              xp_rate = -999,
                              xp_made_pg = -999,
                              st_punt_return_yds_allowed = -999,
                              st_kick_return_yds_allowed = -999,
                              st_kick_return_TDs_allowed = -999,
                              st_punt_return_TDs_allowed = -999,
                              fg_rate_allowed = -999,
                              fg_made_pg_allowed = -999,
                              xp_rate_allowed = -999,
                              xp_made_pg_allowed = -999,
                              net_st_ppg = -999)
} else{
  ##### Week 6 - End of Season Data Pull #####
  ### reading in PBP data
  PBP <- nflfastR::load_pbp(as.numeric(season)) |>
    filter(play_type_nfl != "GAME_START" & play_type_nfl != "TIMEOUT" & play_type_nfl != "END_QUARTER" & play_type_nfl != "END_GAME")
  
  
  ### separting PBP into categories for stat extraction later
  ### offensive and defensive plays
  rushpass_plays <- PBP |>
    filter(play_type %in% c("run", "pass")) |>
    drop_na(epa) |>
    drop_na(yards_gained)
  success_plays <- rushpass_plays |>
    filter(play_type_nfl != "INTERCEPTION") |>
    filter((down == 1 & (yards_gained >= (ydstogo / 2))) | (down == 2 & (yards_gained >= (ydstogo * 0.7))) | (down > 2 & (yards_gained >= ydstogo)))
  TDs <- rushpass_plays |>
    filter(touchdown == 1 & play_type_nfl != "INTERCEPTION" & play_type_nfl != "FUMBLE_RECOVERED_BY_OPPONENT" & fumble_lost == 0 & interception == 0)
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
  kickoffs <- PBP |>
    filter(play_type == "kickoff")
  punts <- PBP |>
    filter(play_type == "punt")
  
  ### creating dataframe to eventually store VoA Variables and ratings
  VoA_Variables <- data.frame(season = rep(as.numeric(season), 32),
                              week = rep(as.numeric(week), 32),
                              team = unique(c(PBP$home_team, PBP$away_team)),
                              off_ypp = -999,
                              off_epa = -999,
                              off_success_rt = -999,
                              off_explosiveness = -999,
                              off_third_conv_rate = -999,
                              off_fourth_conv_rate = -999,
                              off_pass_ypa = -999,
                              off_pass_ypc = -999,
                              off_rush_ypa = -999,
                              off_pts_per_opp = -999,
                              off_turnovers = -999,
                              off_plays_pg = -999,
                              off_ppg = -999,
                              def_ypp = -999,
                              def_epa = -999,
                              def_success_rt = -999,
                              def_explosiveness = -999,
                              def_third_conv_rate = -999,
                              def_fourth_conv_rate = -999,
                              def_pass_ypa = -999,
                              def_pass_ypc = -999,
                              def_rush_ypa = -999,
                              def_pts_per_opp = -999,
                              def_plays_pg = -999,
                              def_ppg = -999,
                              st_net_epa = -999,
                              st_punt_return_yds = -999,
                              st_kick_return_yds = -999,
                              st_kick_return_TDs = -999,
                              st_punt_return_TDs = -999,
                              fg_rate = -999,
                              fg_made_pg = -999,
                              xp_rate = -999,
                              xp_made_pg = -999,
                              st_punt_return_yds_allowed = -999,
                              st_kick_return_yds_allowed = -999,
                              st_kick_return_TDs_allowed = -999,
                              st_punt_return_TDs_allowed = -999,
                              fg_rate_allowed = -999,
                              fg_made_pg_allowed = -999,
                              xp_rate_allowed = -999,
                              xp_made_pg_allowed = -999,
                              net_st_ppg = -999)
}


##### Extracting Relevant Stats from PBP data #####
if (as.numeric(week) == 0){
  ##### Week 0 (preseason) stat collection #####
  for (x in 1:nrow(VoA_Variables)){
    ### PY1 temp dfs
    ### temp PY1 offensive stat dfs
    temp_PY1_offplays <- PY1_rushpass_plays |>
      filter(posteam == VoA_Variables$team[x])
    temp_PY1_offsuccessplays <- PY1_success_plays |>
      filter(posteam == VoA_Variables$team[x])
    temp_PY1_offthirddowns <- PY1_3rdDowns |>
      filter(posteam == VoA_Variables$team[x]) |>
      drop_na(third_down_converted)
    temp_PY1_conv_offthirddowns <- temp_PY1_offthirddowns |>
      filter(third_down_converted == 1)
    temp_PY1_off_fourthdowns <- PY1_4thDowns |>
      filter(posteam == VoA_Variables$team[x]) |>
      drop_na(fourth_down_converted)
    temp_PY1_conv_offfourthdowns <- temp_PY1_off_fourthdowns |>
      filter(fourth_down_converted == 1)
    temp_PY1_off_passplays <- PY1_passplays |>
      filter(posteam == VoA_Variables$team[x])
    temp_PY1_off_comppass <- temp_PY1_off_passplays |>
      filter(complete_pass == 1)
    temp_PY1_off_rushplays <- PY1_rushplays |>
      filter(posteam == VoA_Variables$team[x])
    temp_PY1_off_scoringoppplays <- PY1_scoringopp_plays |>
      filter(posteam == VoA_Variables$team[x]) |>
      drop_na(drive)
    temp_PY1_off_scorringopp_TDs <- temp_PY1_off_scoringoppplays |>
      filter(touchdown == 1)
    temp_PY1_off_scorringopp_FGs <- temp_PY1_off_scoringoppplays |>
      filter(field_goal_result == "made")
    temp_PY1_off_turnovers <- PY1_Turnovers |>
      filter(posteam == VoA_Variables$team[x])
    temp_PY1_off_TDs <- PY1_TDs |>
      filter(posteam == VoA_Variables$team[x])
    temp_PY1_off_2pts <- PY1_2pts |>
      filter(posteam == VoA_Variables$team[x] & two_point_conv_result == "success")
    ### PY1 def stats
    temp_PY1_defplays <- PY1_rushpass_plays |>
      filter(defteam == VoA_Variables$team[x])
    temp_PY1_defsuccessplays <- PY1_success_plays |>
      filter(defteam == VoA_Variables$team[x])
    temp_PY1_defthirddowns <- PY1_3rdDowns |>
      filter(defteam == VoA_Variables$team[x]) |>
      drop_na(third_down_converted)
    temp_PY1_conv_defthirddowns <- temp_PY1_defthirddowns |>
      filter(third_down_converted == 1)
    temp_PY1_def_fourthdowns <- PY1_4thDowns |>
      filter(defteam == VoA_Variables$team[x]) |>
      drop_na(fourth_down_converted)
    temp_PY1_conv_deffourthdowns <- temp_PY1_def_fourthdowns |>
      filter(fourth_down_converted == 1)
    temp_PY1_def_passplays <- PY1_passplays |>
      filter(defteam == VoA_Variables$team[x])
    temp_PY1_def_comppass <- temp_PY1_def_passplays |>
      filter(complete_pass == 1)
    temp_PY1_def_rushplays <- PY1_rushplays |>
      filter(defteam == VoA_Variables$team[x])
    temp_PY1_def_scoringoppplays <- PY1_scoringopp_plays |>
      filter(defteam == VoA_Variables$team[x]) |>
      drop_na(drive)
    temp_PY1_def_scorringopp_TDs <- temp_PY1_def_scoringoppplays |>
      filter(touchdown == 1)
    temp_PY1_def_scorringopp_FGs <- temp_PY1_def_scoringoppplays |>
      filter(field_goal_result == "made")
    temp_PY1_def_turnovers <- PY1_Turnovers |>
      filter(defteam == VoA_Variables$team[x])
    temp_PY1_def_TDs <- PY1_TDs |>
      filter(defteam == VoA_Variables$team[x])
    temp_PY1_def_2pts <- PY1_2pts |>
      filter(defteam == VoA_Variables$team[x] & two_point_conv_result == "success")
    ### temp PY1 special teams dfs
    ## on kickoffs, defteam does kicking
    ## on punts, posteam does punting
    temp_PY1_off_FGs <- PY1_FGs |>
      filter(posteam == VoA_Variables$team[x])
    temp_PY1_off_goodFGs <- temp_PY1_off_FGs |>
      filter(field_goal_result == "made")
    temp_PY1_def_FGs <- PY1_FGs |>
      filter(defteam == VoA_Variables$team[x] & field_goal_result == "made")
    temp_PY1_def_goodFGs <- temp_PY1_def_FGs |>
      filter(field_goal_result == "made")
    temp_PY1_returned_punts <- PY1_punts |>
      filter(defteam == VoA_Variables$team[x])
    temp_PY1_returned_kicks <- PY1_kickoffs |>
      filter(posteam == VoA_Variables$team[x])
    temp_PY1_returned_punt_TDs <- temp_PY1_returned_punts |>
      filter(return_touchdown == 1)
    temp_PY1_returned_kick_TDs <- temp_PY1_returned_kicks |>
      filter(return_touchdown == 1)
    temp_PY1_kicked_punts <- PY1_punts |>
      filter(posteam == VoA_Variables$team[x])
    temp_PY1_kicked_kicks <- PY1_kickoffs |>
      filter(defteam == VoA_Variables$team[x])
    temp_PY1_kicked_punt_TDs <- temp_PY1_kicked_punts |>
      filter(return_touchdown == 1)
    temp_PY1_kicked_kick_TDs <- temp_PY1_kicked_kicks |>
      filter(return_touchdown == 1)
    temp_PY1_off_xps <- PY1_XPts |>
      filter(posteam == VoA_Variables$team[x])
    temp_PY1_def_xps <- PY1_XPts |>
      filter(defteam == VoA_Variables$team[x])
    temp_PY1_off_good_xps <- temp_PY1_off_xps |>
      filter(extra_point_result == "good")
    temp_PY1_def_good_xps <- temp_PY1_def_xps |>
      filter(extra_point_result == "good")
    ### used to get net ST epa/play
    temp_PY1_off_st_plays <- rbind(temp_PY1_off_FGs, temp_PY1_off_xps, temp_PY1_returned_kicks, temp_PY1_returned_punts)
    temp_PY1_def_st_plays <- rbind(temp_PY1_def_FGs, temp_PY1_def_xps, temp_PY1_kicked_kicks, temp_PY1_kicked_punts)
    
    
    ### PY2 temp dfs
    ### temp PY2 offensive stat dfs
    temp_PY2_offplays <- PY2_rushpass_plays |>
      filter(posteam == VoA_Variables$team[x])
    temp_PY2_offsuccessplays <- PY2_success_plays |>
      filter(posteam == VoA_Variables$team[x])
    temp_PY2_offthirddowns <- PY2_3rdDowns |>
      filter(posteam == VoA_Variables$team[x]) |>
      drop_na(third_down_converted)
    temp_PY2_conv_offthirddowns <- temp_PY2_offthirddowns |>
      filter(third_down_converted == 1)
    temp_PY2_off_fourthdowns <- PY2_4thDowns |>
      filter(posteam == VoA_Variables$team[x]) |>
      drop_na(fourth_down_converted)
    temp_PY2_conv_offfourthdowns <- temp_PY2_off_fourthdowns |>
      filter(fourth_down_converted == 1)
    temp_PY2_off_passplays <- PY2_passplays |>
      filter(posteam == VoA_Variables$team[x])
    temp_PY2_off_comppass <- temp_PY2_off_passplays |>
      filter(complete_pass == 1)
    temp_PY2_off_rushplays <- PY2_rushplays |>
      filter(posteam == VoA_Variables$team[x])
    temp_PY2_off_scoringoppplays <- PY2_scoringopp_plays |>
      filter(posteam == VoA_Variables$team[x]) |>
      drop_na(drive)
    temp_PY2_off_scorringopp_TDs <- temp_PY2_off_scoringoppplays |>
      filter(touchdown == 1)
    temp_PY2_off_scorringopp_FGs <- temp_PY2_off_scoringoppplays |>
      filter(field_goal_result == "made")
    temp_PY2_off_turnovers <- PY2_Turnovers |>
      filter(posteam == VoA_Variables$team[x])
    temp_PY2_off_TDs <- PY2_TDs |>
      filter(posteam == VoA_Variables$team[x])
    temp_PY2_off_2pts <- PY2_2pts |>
      filter(posteam == VoA_Variables$team[x] & two_point_conv_result == "success")
    ### PY2 def stats
    temp_PY2_defplays <- PY2_rushpass_plays |>
      filter(defteam == VoA_Variables$team[x])
    temp_PY2_defsuccessplays <- PY2_success_plays |>
      filter(defteam == VoA_Variables$team[x])
    temp_PY2_defthirddowns <- PY2_3rdDowns |>
      filter(defteam == VoA_Variables$team[x]) |>
      drop_na(third_down_converted)
    temp_PY2_conv_defthirddowns <- temp_PY2_defthirddowns |>
      filter(third_down_converted == 1)
    temp_PY2_def_fourthdowns <- PY2_4thDowns |>
      filter(defteam == VoA_Variables$team[x]) |>
      drop_na(fourth_down_converted)
    temp_PY2_conv_deffourthdowns <- temp_PY2_def_fourthdowns |>
      filter(fourth_down_converted == 1)
    temp_PY2_def_passplays <- PY2_passplays |>
      filter(defteam == VoA_Variables$team[x])
    temp_PY2_def_comppass <- temp_PY2_def_passplays |>
      filter(complete_pass == 1)
    temp_PY2_def_rushplays <- PY2_rushplays |>
      filter(defteam == VoA_Variables$team[x])
    temp_PY2_def_scoringoppplays <- PY2_scoringopp_plays |>
      filter(defteam == VoA_Variables$team[x]) |>
      drop_na(drive)
    temp_PY2_def_scorringopp_TDs <- temp_PY2_def_scoringoppplays |>
      filter(touchdown == 1)
    temp_PY2_def_scorringopp_FGs <- temp_PY2_def_scoringoppplays |>
      filter(field_goal_result == "made")
    temp_PY2_def_turnovers <- PY2_Turnovers |>
      filter(defteam == VoA_Variables$team[x])
    temp_PY2_def_TDs <- PY2_TDs |>
      filter(defteam == VoA_Variables$team[x])
    temp_PY2_def_2pts <- PY2_2pts |>
      filter(defteam == VoA_Variables$team[x] & two_point_conv_result == "success")
    ### temp PY2 special teams dfs
    ## on kickoffs, defteam does kicking
    ## on punts, posteam does punting
    temp_PY2_off_FGs <- PY2_FGs |>
      filter(posteam == VoA_Variables$team[x])
    temp_PY2_off_goodFGs <- temp_PY2_off_FGs |>
      filter(field_goal_result == "made")
    temp_PY2_def_FGs <- PY2_FGs |>
      filter(defteam == VoA_Variables$team[x] & field_goal_result == "made")
    temp_PY2_def_goodFGs <- temp_PY2_def_FGs |>
      filter(field_goal_result == "made")
    temp_PY2_returned_punts <- PY2_punts |>
      filter(defteam == VoA_Variables$team[x])
    temp_PY2_returned_kicks <- PY2_kickoffs |>
      filter(posteam == VoA_Variables$team[x])
    temp_PY2_returned_punt_TDs <- temp_PY2_returned_punts |>
      filter(return_touchdown == 1)
    temp_PY2_returned_kick_TDs <- temp_PY2_returned_kicks |>
      filter(return_touchdown == 1)
    temp_PY2_kicked_punts <- PY2_punts |>
      filter(posteam == VoA_Variables$team[x])
    temp_PY2_kicked_kicks <- PY2_kickoffs |>
      filter(defteam == VoA_Variables$team[x])
    temp_PY2_kicked_punt_TDs <- temp_PY2_kicked_punts |>
      filter(return_touchdown == 1)
    temp_PY2_kicked_kick_TDs <- temp_PY2_kicked_kicks |>
      filter(return_touchdown == 1)
    temp_PY2_off_xps <- PY2_XPts |>
      filter(posteam == VoA_Variables$team[x])
    temp_PY2_def_xps <- PY2_XPts |>
      filter(defteam == VoA_Variables$team[x])
    temp_PY2_off_good_xps <- temp_PY2_off_xps |>
      filter(extra_point_result == "good")
    temp_PY2_def_good_xps <- temp_PY2_def_xps |>
      filter(extra_point_result == "good")
    ### used to get net ST epa/play
    temp_PY2_off_st_plays <- rbind(temp_PY2_off_FGs, temp_PY2_off_xps, temp_PY2_returned_kicks, temp_PY2_returned_punts)
    temp_PY2_def_st_plays <- rbind(temp_PY2_def_FGs, temp_PY2_def_xps, temp_PY2_kicked_kicks, temp_PY2_kicked_punts)
    
    
    ### PY3 temp dfs
    ### temp PY3 offensive stat dfs
    temp_PY3_offplays <- PY3_rushpass_plays |>
      filter(posteam == VoA_Variables$team[x])
    temp_PY3_offsuccessplays <- PY3_success_plays |>
      filter(posteam == VoA_Variables$team[x])
    temp_PY3_offthirddowns <- PY3_3rdDowns |>
      filter(posteam == VoA_Variables$team[x]) |>
      drop_na(third_down_converted)
    temp_PY3_conv_offthirddowns <- temp_PY3_offthirddowns |>
      filter(third_down_converted == 1)
    temp_PY3_off_fourthdowns <- PY3_4thDowns |>
      filter(posteam == VoA_Variables$team[x]) |>
      drop_na(fourth_down_converted)
    temp_PY3_conv_offfourthdowns <- temp_PY3_off_fourthdowns |>
      filter(fourth_down_converted == 1)
    temp_PY3_off_passplays <- PY3_passplays |>
      filter(posteam == VoA_Variables$team[x])
    temp_PY3_off_comppass <- temp_PY3_off_passplays |>
      filter(complete_pass == 1)
    temp_PY3_off_rushplays <- PY3_rushplays |>
      filter(posteam == VoA_Variables$team[x])
    temp_PY3_off_scoringoppplays <- PY3_scoringopp_plays |>
      filter(posteam == VoA_Variables$team[x]) |>
      drop_na(drive)
    temp_PY3_off_scorringopp_TDs <- temp_PY3_off_scoringoppplays |>
      filter(touchdown == 1)
    temp_PY3_off_scorringopp_FGs <- temp_PY3_off_scoringoppplays |>
      filter(field_goal_result == "made")
    temp_PY3_off_turnovers <- PY3_Turnovers |>
      filter(posteam == VoA_Variables$team[x])
    temp_PY3_off_TDs <- PY3_TDs |>
      filter(posteam == VoA_Variables$team[x])
    temp_PY3_off_2pts <- PY3_2pts |>
      filter(posteam == VoA_Variables$team[x] & two_point_conv_result == "success")
    ### PY3 def stats
    temp_PY3_defplays <- PY3_rushpass_plays |>
      filter(defteam == VoA_Variables$team[x])
    temp_PY3_defsuccessplays <- PY3_success_plays |>
      filter(defteam == VoA_Variables$team[x])
    temp_PY3_defthirddowns <- PY3_3rdDowns |>
      filter(defteam == VoA_Variables$team[x]) |>
      drop_na(third_down_converted)
    temp_PY3_conv_defthirddowns <- temp_PY1_defthirddowns |>
      filter(third_down_converted == 1)
    temp_PY3_def_fourthdowns <- PY3_4thDowns |>
      filter(defteam == VoA_Variables$team[x]) |>
      drop_na(fourth_down_converted)
    temp_PY3_conv_deffourthdowns <- temp_PY3_off_fourthdowns |>
      filter(fourth_down_converted == 1)
    temp_PY3_def_passplays <- PY3_passplays |>
      filter(defteam == VoA_Variables$team[x])
    temp_PY3_def_comppass <- temp_PY3_def_passplays |>
      filter(complete_pass == 1)
    temp_PY3_def_rushplays <- PY3_rushplays |>
      filter(defteam == VoA_Variables$team[x])
    temp_PY3_def_scoringoppplays <- PY3_scoringopp_plays |>
      filter(defteam == VoA_Variables$team[x]) |>
      drop_na(drive)
    temp_PY3_def_scorringopp_TDs <- temp_PY3_def_scoringoppplays |>
      filter(touchdown == 1)
    temp_PY3_def_scorringopp_FGs <- temp_PY3_def_scoringoppplays |>
      filter(field_goal_result == "made")
    temp_PY3_def_turnovers <- PY3_Turnovers |>
      filter(defteam == VoA_Variables$team[x])
    temp_PY3_def_TDs <- PY3_TDs |>
      filter(defteam == VoA_Variables$team[x])
    temp_PY3_def_2pts <- PY3_2pts |>
      filter(defteam == VoA_Variables$team[x] & two_point_conv_result == "success")
    ### temp PY3 special teams dfs
    ## on kickoffs, defteam does kicking
    ## on punts, posteam does punting
    temp_PY3_off_FGs <- PY3_FGs |>
      filter(posteam == VoA_Variables$team[x])
    temp_PY3_off_goodFGs <- temp_PY3_off_FGs |>
      filter(field_goal_result == "made")
    temp_PY3_def_FGs <- PY3_FGs |>
      filter(defteam == VoA_Variables$team[x] & field_goal_result == "made")
    temp_PY3_def_goodFGs <- temp_PY3_def_FGs |>
      filter(field_goal_result == "made")
    temp_PY3_returned_punts <- PY3_punts |>
      filter(defteam == VoA_Variables$team[x])
    temp_PY3_returned_kicks <- PY3_kickoffs |>
      filter(posteam == VoA_Variables$team[x])
    temp_PY3_returned_punt_TDs <- temp_PY3_returned_punts |>
      filter(return_touchdown == 1)
    temp_PY3_returned_kick_TDs <- temp_PY3_returned_kicks |>
      filter(return_touchdown == 1)
    temp_PY3_kicked_punts <- PY3_punts |>
      filter(posteam == VoA_Variables$team[x])
    temp_PY3_kicked_kicks <- PY3_kickoffs |>
      filter(defteam == VoA_Variables$team[x])
    temp_PY3_kicked_punt_TDs <- temp_PY3_kicked_punts |>
      filter(return_touchdown == 1)
    temp_PY3_kicked_kick_TDs <- temp_PY3_kicked_kicks |>
      filter(return_touchdown == 1)
    temp_PY3_off_xps <- PY3_XPts |>
      filter(posteam == VoA_Variables$team[x])
    temp_PY3_def_xps <- PY3_XPts |>
      filter(defteam == VoA_Variables$team[x])
    temp_PY3_off_good_xps <- temp_PY3_off_xps |>
      filter(extra_point_result == "good")
    temp_PY3_def_good_xps <- temp_PY3_def_xps |>
      filter(extra_point_result == "good")
    ### used to get net ST epa/play
    temp_PY3_off_st_plays <- rbind(temp_PY3_off_FGs, temp_PY3_off_xps, temp_PY3_returned_kicks, temp_PY3_returned_punts)
    temp_PY3_def_st_plays <- rbind(temp_PY3_def_FGs, temp_PY3_def_xps, temp_PY3_kicked_kicks, temp_PY3_kicked_punts)
    
    
    ### deriving stats from temp dfs
    ### PY1 stats
    VoA_Variables$off_ypp_PY1[x] = mean(temp_PY1_offplays$yards_gained)
    VoA_Variables$off_epa_PY1[x] = mean(temp_PY1_offplays$epa)
    VoA_Variables$off_success_rt_PY1[x] = nrow(temp_PY1_offsuccessplays) / nrow(temp_PY1_offplays)
    VoA_Variables$off_explosiveness_PY1[x] = mean(temp_PY1_offsuccessplays$epa)
    VoA_Variables$off_third_conv_rate_PY1[x] = nrow(temp_PY1_conv_offthirddowns) / nrow(temp_PY1_offthirddowns)
    VoA_Variables$off_fourth_conv_rate_PY1[x] = nrow(temp_PY1_conv_offfourthdowns) / nrow(temp_PY1_off_fourthdowns)
    VoA_Variables$off_pass_ypa_PY1[x] = mean(temp_PY1_off_passplays$yards_gained)
    VoA_Variables$off_pass_ypc_PY1[x] = mean(temp_PY1_off_comppass$yards_gained)
    VoA_Variables$off_rush_ypa_PY1[x] = mean(temp_PY1_off_rushplays$yards_gained)
    VoA_Variables$off_pts_per_opp_PY1[x] = ((nrow(temp_PY1_off_scorringopp_TDs) * 6) + (nrow(temp_PY1_off_scorringopp_FGs) * 3)) / length(unique(paste0(temp_PY1_off_scoringoppplays$game_id, temp_PY1_off_scoringoppplays$drive)))
    VoA_Variables$off_turnovers_PY1[x] = nrow(temp_PY1_off_turnovers) / length(unique(temp_PY1_offplays$week))
    VoA_Variables$off_plays_pg_PY1[x] = nrow(temp_PY1_offplays) / length(unique(temp_PY1_offplays$week))
    VoA_Variables$off_ppg_PY1[x] = ((nrow(temp_PY1_off_TDs) * 6) + (nrow(temp_PY1_off_2pts) * 2)) / length(unique(temp_PY1_off_rushplays$week))
    ## PY1 defensive stats now
    VoA_Variables$def_ypp_PY1[x] = mean(temp_PY1_defplays$yards_gained)
    VoA_Variables$def_epa_PY1[x] = mean(temp_PY1_defplays$epa)
    VoA_Variables$def_success_rt_PY1[x] = nrow(temp_PY1_defsuccessplays) / nrow(temp_PY1_defplays)
    VoA_Variables$def_explosiveness_PY1[x] = mean(temp_PY1_defsuccessplays$epa)
    VoA_Variables$def_third_conv_rate_PY1[x] = nrow(temp_PY1_conv_defthirddowns) / nrow(temp_PY1_defthirddowns)
    VoA_Variables$def_fourth_conv_rate_PY1[x] = nrow(temp_PY1_conv_deffourthdowns) / nrow(temp_PY1_def_fourthdowns)
    VoA_Variables$def_pass_ypa_PY1[x] = mean(temp_PY1_def_passplays$yards_gained)
    VoA_Variables$def_pass_ypc_PY1[x] = mean(temp_PY1_def_comppass$yards_gained)
    VoA_Variables$def_rush_ypa_PY1[x] = mean(temp_PY1_def_rushplays$yards_gained)
    VoA_Variables$def_pts_per_opp_PY1[x] = ((nrow(temp_PY1_def_scorringopp_TDs) * 6) + (nrow(temp_PY1_def_scorringopp_FGs) * 3)) / length(unique(paste0(temp_PY1_def_scoringoppplays$game_id, temp_PY1_def_scoringoppplays$drive)))
    VoA_Variables$def_turnovers_PY1[x] = nrow(temp_PY1_def_turnovers) / length(unique(temp_PY1_defplays$week))
    VoA_Variables$def_plays_pg_PY1[x] = nrow(temp_PY1_defplays) / length(unique(temp_PY1_defplays$week))
    VoA_Variables$def_ppg_PY1[x] = ((nrow(temp_PY1_def_TDs) * 6) + (nrow(temp_PY1_def_2pts) * 2)) / length(unique(temp_PY1_def_rushplays$week))
    ## PY1 Special teams stats now
    VoA_Variables$st_net_epa_PY1[x] = mean(temp_PY1_off_st_plays$epa) - mean(temp_PY1_def_st_plays$epa)
    VoA_Variables$st_punt_return_yds_PY1[x] = mean(temp_PY1_returned_punts$return_yards)
    VoA_Variables$st_kick_return_yds_PY1[x] = mean(temp_PY1_returned_kicks$return_yards)
    VoA_Variables$st_kick_return_TDs_PY1[x] = nrow(temp_PY1_returned_kick_TDs) / length(unique(temp_PY1_offplays$week))
    VoA_Variables$st_punt_return_TDs_PY1[x] = nrow(temp_PY1_returned_punt_TDs) / length(unique(temp_PY1_offplays$week))
    VoA_Variables$fg_rate_PY1[x] = nrow(temp_PY1_off_goodFGs) / nrow(temp_PY1_off_FGs)
    VoA_Variables$fg_made_pg_PY1[x] = nrow(temp_PY1_off_goodFGs) / length(unique(temp_PY1_offplays$week))
    VoA_Variables$xp_rate_PY1[x] = nrow(temp_PY1_off_good_xps) / nrow(temp_PY1_off_xps)
    VoA_Variables$xp_made_pg_PY1[x] = nrow(temp_PY1_off_good_xps) / length(unique(temp_PY1_offplays$week))
    VoA_Variables$st_punt_return_yds_allowed_PY1[x] = mean(temp_PY1_kicked_punts$return_yards)
    VoA_Variables$st_kick_return_yds_allowed_PY1[x] = mean(temp_PY1_kicked_kicks$return_yards)
    VoA_Variables$st_kick_return_TDs_allowed_PY1[x] = nrow(temp_PY1_kicked_kick_TDs) / length(unique(temp_PY1_offplays$week))
    VoA_Variables$st_punt_return_TDs_allowed_PY1[x] = nrow(temp_PY1_kicked_punt_TDs) / length(unique(temp_PY1_offplays$week))
    VoA_Variables$fg_rate_allowed_PY1[x] = nrow(temp_PY1_def_goodFGs) / nrow(temp_PY1_def_FGs)
    VoA_Variables$fg_made_pg_allowed_PY1[x] = nrow(temp_PY1_def_goodFGs) / length(unique(temp_PY1_offplays$week))
    VoA_Variables$xp_rate_allowed_PY1[x] = nrow(temp_PY1_def_good_xps) / nrow(temp_PY1_def_xps)
    VoA_Variables$xp_made_pg_allowed_PY1[x] = nrow(temp_PY1_def_good_xps) / length(unique(temp_PY1_offplays$week))
    VoA_Variables$net_st_ppg_PY1[x] = (((nrow(temp_PY1_off_goodFGs) * 3) + (nrow(temp_PY1_returned_punt_TDs) * 6) + (nrow(temp_PY1_returned_kick_TDs) * 6) + nrow(temp_PY1_off_good_xps)) - ((nrow(temp_PY1_def_goodFGs) * 3) + (nrow(temp_PY1_kicked_punt_TDs) * 6) + (nrow(temp_PY1_kicked_kick_TDs) * 6) + nrow(temp_PY1_def_good_xps))) / length(unique(temp_PY1_offplays$week))
    
    ### evaluating PY2 variables
    VoA_Variables$off_ypp_PY2[x] = mean(temp_PY2_offplays$yards_gained)
    VoA_Variables$off_epa_PY2[x] = mean(temp_PY2_offplays$epa)
    VoA_Variables$off_success_rt_PY2[x] = nrow(temp_PY2_offsuccessplays) / nrow(temp_PY2_offplays)
    VoA_Variables$off_explosiveness_PY2[x] = mean(temp_PY2_offsuccessplays$epa)
    VoA_Variables$off_third_conv_rate_PY2[x] = nrow(temp_PY2_conv_offthirddowns) / nrow(temp_PY2_offthirddowns)
    VoA_Variables$off_fourth_conv_rate_PY2[x] = nrow(temp_PY2_conv_offfourthdowns) / nrow(temp_PY2_off_fourthdowns)
    VoA_Variables$off_pass_ypa_PY2[x] = mean(temp_PY2_off_passplays$yards_gained)
    VoA_Variables$off_pass_ypc_PY2[x] = mean(temp_PY2_off_comppass$yards_gained)
    VoA_Variables$off_rush_ypa_PY2[x] = mean(temp_PY2_off_rushplays$yards_gained)
    VoA_Variables$off_pts_per_opp_PY2[x] = ((nrow(temp_PY2_off_scorringopp_TDs) * 6) + (nrow(temp_PY2_off_scorringopp_FGs) * 3)) / length(unique(paste0(temp_PY2_off_scoringoppplays$game_id, temp_PY2_off_scoringoppplays$drive)))
    VoA_Variables$off_turnovers_PY2[x] = nrow(temp_PY2_off_turnovers) / length(unique(temp_PY2_offplays$week))
    VoA_Variables$off_plays_pg_PY2[x] = nrow(temp_PY2_offplays) / length(unique(temp_PY2_offplays$week))
    VoA_Variables$off_ppg_PY2[x] = ((nrow(temp_PY2_off_TDs) * 6) + (nrow(temp_PY2_off_2pts) * 2)) / length(unique(temp_PY2_off_rushplays$week))
    ## PY2 defensive stats now
    VoA_Variables$def_ypp_PY2[x] = mean(temp_PY2_defplays$yards_gained)
    VoA_Variables$def_epa_PY2[x] = mean(temp_PY2_defplays$epa)
    VoA_Variables$def_success_rt_PY2[x] = nrow(temp_PY2_defsuccessplays) / nrow(temp_PY2_defplays)
    VoA_Variables$def_explosiveness_PY2[x] = mean(temp_PY2_defsuccessplays$epa)
    VoA_Variables$def_third_conv_rate_PY2[x] = nrow(temp_PY2_conv_defthirddowns) / nrow(temp_PY2_defthirddowns)
    VoA_Variables$def_fourth_conv_rate_PY2[x] = nrow(temp_PY2_conv_deffourthdowns) / nrow(temp_PY2_def_fourthdowns)
    VoA_Variables$def_pass_ypa_PY2[x] = mean(temp_PY2_def_passplays$yards_gained)
    VoA_Variables$def_pass_ypc_PY2[x] = mean(temp_PY2_def_comppass$yards_gained)
    VoA_Variables$def_rush_ypa_PY2[x] = mean(temp_PY2_def_rushplays$yards_gained)
    VoA_Variables$def_pts_per_opp_PY2[x] = ((nrow(temp_PY2_def_scorringopp_TDs) * 6) + (nrow(temp_PY2_def_scorringopp_FGs) * 3)) / length(unique(paste0(temp_PY2_def_scoringoppplays$game_id, temp_PY2_def_scoringoppplays$drive)))
    VoA_Variables$def_turnovers_PY2[x] = nrow(temp_PY2_def_turnovers) / length(unique(temp_PY2_defplays$week))
    VoA_Variables$def_plays_pg_PY2[x] = nrow(temp_PY2_defplays) / length(unique(temp_PY2_defplays$week))
    VoA_Variables$def_ppg_PY2[x] = ((nrow(temp_PY2_def_TDs) * 6) + (nrow(temp_PY2_def_2pts) * 2)) / length(unique(temp_PY2_def_rushplays$week))
    ## PY2 Special teams stats now
    VoA_Variables$st_net_epa_PY2[x] = mean(temp_PY2_off_st_plays$epa) - mean(temp_PY2_def_st_plays$epa)
    VoA_Variables$st_punt_return_yds_PY2[x] = mean(temp_PY2_returned_punts$return_yards)
    VoA_Variables$st_kick_return_yds_PY2[x] = mean(temp_PY2_returned_kicks$return_yards)
    VoA_Variables$st_kick_return_TDs_PY2[x] = nrow(temp_PY2_returned_kick_TDs) / length(unique(temp_PY2_offplays$week))
    VoA_Variables$st_punt_return_TDs_PY2[x] = nrow(temp_PY2_returned_punt_TDs) / length(unique(temp_PY2_offplays$week))
    VoA_Variables$fg_rate_PY2[x] = nrow(temp_PY2_off_goodFGs) / nrow(temp_PY2_off_FGs)
    VoA_Variables$fg_made_pg_PY2[x] = nrow(temp_PY2_off_goodFGs) / length(unique(temp_PY2_offplays$week))
    VoA_Variables$xp_rate_PY2[x] = nrow(temp_PY2_off_good_xps) / nrow(temp_PY2_off_xps)
    VoA_Variables$xp_made_pg_PY2[x] = nrow(temp_PY2_off_good_xps) / length(unique(temp_PY2_offplays$week))
    VoA_Variables$st_punt_return_yds_allowed_PY2[x] = mean(temp_PY2_kicked_punts$return_yards)
    VoA_Variables$st_kick_return_yds_allowed_PY2[x] = mean(temp_PY2_kicked_kicks$return_yards)
    VoA_Variables$st_kick_return_TDs_allowed_PY2[x] = nrow(temp_PY2_kicked_kick_TDs) / length(unique(temp_PY2_offplays$week))
    VoA_Variables$st_punt_return_TDs_allowed_PY2[x] = nrow(temp_PY2_kicked_punt_TDs) / length(unique(temp_PY2_offplays$week))
    VoA_Variables$fg_rate_allowed_PY2[x] = nrow(temp_PY2_def_goodFGs) / nrow(temp_PY2_def_FGs)
    VoA_Variables$fg_made_pg_allowed_PY2[x] = nrow(temp_PY2_def_goodFGs) / length(unique(temp_PY2_offplays$week))
    VoA_Variables$xp_rate_allowed_PY2[x] = nrow(temp_PY2_def_good_xps) / nrow(temp_PY2_def_xps)
    VoA_Variables$xp_made_pg_allowed_PY2[x] = nrow(temp_PY2_def_good_xps) / length(unique(temp_PY2_offplays$week))
    VoA_Variables$net_st_ppg_PY2[x] = (((nrow(temp_PY2_off_goodFGs) * 3) + (nrow(temp_PY2_returned_punt_TDs) * 6) + (nrow(temp_PY2_returned_kick_TDs) * 6) + nrow(temp_PY2_off_good_xps)) - ((nrow(temp_PY2_def_goodFGs) * 3) + (nrow(temp_PY2_kicked_punt_TDs) * 6) + (nrow(temp_PY2_kicked_kick_TDs) * 6) + nrow(temp_PY2_def_good_xps))) / length(unique(temp_PY2_offplays$week))
    
    
    ### evaluating PY3 variables
    VoA_Variables$off_ypp_PY3[x] = mean(temp_PY3_offplays$yards_gained)
    VoA_Variables$off_epa_PY3[x] = mean(temp_PY3_offplays$epa)
    VoA_Variables$off_success_rt_PY3[x] = nrow(temp_PY3_offsuccessplays) / nrow(temp_PY3_offplays)
    VoA_Variables$off_explosiveness_PY3[x] = mean(temp_PY3_offsuccessplays$epa)
    VoA_Variables$off_third_conv_rate_PY3[x] = nrow(temp_PY3_conv_offthirddowns) / nrow(temp_PY3_offthirddowns)
    VoA_Variables$off_fourth_conv_rate_PY3[x] = nrow(temp_PY3_conv_offfourthdowns) / nrow(temp_PY3_off_fourthdowns)
    VoA_Variables$off_pass_ypa_PY3[x] = mean(temp_PY3_off_passplays$yards_gained)
    VoA_Variables$off_pass_ypc_PY3[x] = mean(temp_PY3_off_comppass$yards_gained)
    VoA_Variables$off_rush_ypa_PY3[x] = mean(temp_PY3_off_rushplays$yards_gained)
    VoA_Variables$off_pts_per_opp_PY3[x] = ((nrow(temp_PY3_off_scorringopp_TDs) * 6) + (nrow(temp_PY3_off_scorringopp_FGs) * 3)) / length(unique(paste0(temp_PY3_off_scoringoppplays$game_id, temp_PY3_off_scoringoppplays$drive)))
    VoA_Variables$off_turnovers_PY3[x] = nrow(temp_PY3_off_turnovers) / length(unique(temp_PY3_offplays$week))
    VoA_Variables$off_plays_pg_PY3[x] = nrow(temp_PY3_offplays) / length(unique(temp_PY3_offplays$week))
    VoA_Variables$off_ppg_PY3[x] = ((nrow(temp_PY3_off_TDs) * 6) + (nrow(temp_PY3_off_2pts) * 2)) / length(unique(temp_PY3_off_rushplays$week))
    ## PY3 defensive stats now
    VoA_Variables$def_ypp_PY3[x] = mean(temp_PY3_defplays$yards_gained)
    VoA_Variables$def_epa_PY3[x] = mean(temp_PY3_defplays$epa)
    VoA_Variables$def_success_rt_PY3[x] = nrow(temp_PY3_defsuccessplays) / nrow(temp_PY3_defplays)
    VoA_Variables$def_explosiveness_PY3[x] = mean(temp_PY3_defsuccessplays$epa)
    VoA_Variables$def_third_conv_rate_PY3[x] = nrow(temp_PY3_conv_defthirddowns) / nrow(temp_PY3_defthirddowns)
    VoA_Variables$def_fourth_conv_rate_PY3[x] = nrow(temp_PY3_conv_deffourthdowns) / nrow(temp_PY3_def_fourthdowns)
    VoA_Variables$def_pass_ypa_PY3[x] = mean(temp_PY3_def_passplays$yards_gained)
    VoA_Variables$def_pass_ypc_PY3[x] = mean(temp_PY3_def_comppass$yards_gained)
    VoA_Variables$def_rush_ypa_PY3[x] = mean(temp_PY3_def_rushplays$yards_gained)
    VoA_Variables$def_pts_per_opp_PY3[x] = ((nrow(temp_PY3_def_scorringopp_TDs) * 6) + (nrow(temp_PY3_def_scorringopp_FGs) * 3)) / length(unique(paste0(temp_PY3_def_scoringoppplays$game_id, temp_PY3_def_scoringoppplays$drive)))
    VoA_Variables$def_turnovers_PY3[x] = nrow(temp_PY3_def_turnovers) / length(unique(temp_PY3_defplays$week))
    VoA_Variables$def_plays_pg_PY3[x] = nrow(temp_PY3_defplays) / length(unique(temp_PY3_defplays$week))
    VoA_Variables$def_ppg_PY3[x] = ((nrow(temp_PY3_def_TDs) * 6) + (nrow(temp_PY3_def_2pts) * 2)) / length(unique(temp_PY3_def_rushplays$week))
    ## PY3 Special teams stats now
    VoA_Variables$st_net_epa_PY3[x] = mean(temp_PY3_off_st_plays$epa) - mean(temp_PY3_def_st_plays$epa)
    VoA_Variables$st_punt_return_yds_PY3[x] = mean(temp_PY3_returned_punts$return_yards)
    VoA_Variables$st_kick_return_yds_PY3[x] = mean(temp_PY3_returned_kicks$return_yards)
    VoA_Variables$st_kick_return_TDs_PY3[x] = nrow(temp_PY3_returned_kick_TDs) / length(unique(temp_PY3_offplays$week))
    VoA_Variables$st_punt_return_TDs_PY3[x] = nrow(temp_PY3_returned_punt_TDs) / length(unique(temp_PY3_offplays$week))
    VoA_Variables$fg_rate_PY3[x] = nrow(temp_PY3_off_goodFGs) / nrow(temp_PY3_off_FGs)
    VoA_Variables$fg_made_pg_PY3[x] = nrow(temp_PY3_off_goodFGs) / length(unique(temp_PY3_offplays$week))
    VoA_Variables$xp_rate_PY3[x] = nrow(temp_PY3_off_good_xps) / nrow(temp_PY3_off_xps)
    VoA_Variables$xp_made_pg_PY3[x] = nrow(temp_PY3_off_good_xps) / length(unique(temp_PY3_offplays$week))
    VoA_Variables$st_punt_return_yds_allowed_PY3[x] = mean(temp_PY3_kicked_punts$return_yards)
    VoA_Variables$st_kick_return_yds_allowed_PY3[x] = mean(temp_PY3_kicked_kicks$return_yards)
    VoA_Variables$st_kick_return_TDs_allowed_PY3[x] = nrow(temp_PY3_kicked_kick_TDs) / length(unique(temp_PY3_offplays$week))
    VoA_Variables$st_punt_return_TDs_allowed_PY3[x] = nrow(temp_PY3_kicked_punt_TDs) / length(unique(temp_PY3_offplays$week))
    VoA_Variables$fg_rate_allowed_PY3[x] = nrow(temp_PY3_def_goodFGs) / nrow(temp_PY3_def_FGs)
    VoA_Variables$fg_made_pg_allowed_PY3[x] = nrow(temp_PY3_def_goodFGs) / length(unique(temp_PY3_offplays$week))
    VoA_Variables$xp_rate_allowed_PY3[x] = nrow(temp_PY3_def_good_xps) / nrow(temp_PY3_def_xps)
    VoA_Variables$xp_made_pg_allowed_PY3[x] = nrow(temp_PY3_def_good_xps) / length(unique(temp_PY3_offplays$week))
    VoA_Variables$net_st_ppg_PY3[x] = (((nrow(temp_PY3_off_goodFGs) * 3) + (nrow(temp_PY3_returned_punt_TDs) * 6) + (nrow(temp_PY3_returned_kick_TDs) * 6) + nrow(temp_PY3_off_good_xps)) - ((nrow(temp_PY3_def_goodFGs) * 3) + (nrow(temp_PY3_kicked_punt_TDs) * 6) + (nrow(temp_PY3_kicked_kick_TDs) * 6) + nrow(temp_PY3_def_good_xps))) / length(unique(temp_PY3_offplays$week))
  }
  
  ### writing csv of PY data so that I can read it in for future weeks without needing to recreate it all
  write_csv(VoA_Variables, here("Data", paste0("VoA", season), "PYData", "PYData.csv"))
  
  ### adding weighted variables to be used in Stan model later
  VoA_Variables <- VoA_Variables |>
    ### adding weighted variables (offense first)
    mutate(weighted_off_ypp = (off_ypp_PY1 * 0.7) + (off_ypp_PY2 * 0.25) + (off_ypp_PY3 * 0.05),
           weighted_off_epa = (off_epa_PY1 * 0.7) + (off_epa_PY2 * 0.25) + (off_epa_PY3 * 0.05),
           weighted_off_success_rt = (off_success_rt_PY1 * 0.7) + (off_success_rt_PY2 * 0.25) + (off_success_rt_PY3 * 0.05),
           weighted_off_explosiveness = (off_explosiveness_PY1 * 0.7) + (off_explosiveness_PY2 * 0.25) + (off_explosiveness_PY3 * 0.05),
           weighted_off_third_conv_rate = (off_third_conv_rate_PY1 * 0.7) + (off_third_conv_rate_PY2 * 0.25) + (off_third_conv_rate_PY3 * 0.05),
           weighted_off_fourth_conv_rate = (off_fourth_conv_rate_PY1 * 0.7) + (off_fourth_conv_rate_PY2 * 0.25) + (off_fourth_conv_rate_PY3 * 0.05),
           weighted_off_pass_ypa = (off_pass_ypa_PY1 * 0.7) + (off_pass_ypa_PY2 * 0.25) + (off_pass_ypa_PY3 * 0.05),
           weighted_off_pass_ypc = (off_pass_ypc_PY1 * 0.7) + (off_pass_ypc_PY2 * 0.25) + (off_pass_ypc_PY3 * 0.05),
           weighted_off_rush_ypa = (off_rush_ypa_PY1 * 0.7) + (off_rush_ypa_PY2 * 0.25) + (off_rush_ypa_PY3 * 0.05),
           weighted_off_pts_per_opp = (off_pts_per_opp_PY1 * 0.7) + (off_pts_per_opp_PY2 * 0.25) + (off_pts_per_opp_PY3 * 0.05),
           weighted_off_turnovers = (off_turnovers_PY1 * 0.7) + (off_turnovers_PY2 * 0.25) + (off_turnovers_PY3 * 0.05),
           weighted_off_plays_pg = (off_plays_pg_PY1 * 0.7) + (off_plays_pg_PY2 * 0.25) + (off_plays_pg_PY3 * 0.05),
           weighted_off_ppg = (off_ppg_PY1 * 0.7) + (off_ppg_PY2 * 0.25) + (off_ppg_PY3 * 0.05),
           ### weighted defensive stats now
           weighted_def_ypp = (def_ypp_PY1 * 0.7) + (def_ypp_PY2 * 0.25) + (def_ypp_PY3 * 0.05),
           weighted_def_epa = (def_epa_PY1 * 0.7) + (def_epa_PY2 * 0.25) + (def_epa_PY3 * 0.05),
           weighted_def_success_rt = (def_success_rt_PY1 * 0.7) + (def_success_rt_PY2 * 0.25) + (def_success_rt_PY3 * 0.05),
           weighted_def_explosiveness = (def_explosiveness_PY1 * 0.7) + (def_explosiveness_PY2 * 0.25) + (def_explosiveness_PY3 * 0.05),
           weighted_def_third_conv_rate = (def_third_conv_rate_PY1 * 0.7) + (def_third_conv_rate_PY2 * 0.25) + (def_third_conv_rate_PY3 * 0.05),
           weighted_def_fourth_conv_rate = (def_fourth_conv_rate_PY1 * 0.7) + (def_fourth_conv_rate_PY2 * 0.25) + (def_fourth_conv_rate_PY3 * 0.05),
           weighted_def_pass_ypa = (def_pass_ypa_PY1 * 0.7) + (def_pass_ypa_PY2 * 0.25) + (def_pass_ypa_PY3 * 0.05),
           weighted_def_pass_ypc = (def_pass_ypc_PY1 * 0.7) + (def_pass_ypc_PY2 * 0.25) + (def_pass_ypc_PY3 * 0.05),
           weighted_def_rush_ypa = (def_rush_ypa_PY1 * 0.7) + (def_rush_ypa_PY2 * 0.25) + (def_rush_ypa_PY3 * 0.05),
           weighted_def_pts_per_opp = (def_pts_per_opp_PY1 * 0.7) + (def_pts_per_opp_PY2 * 0.25) + (def_pts_per_opp_PY3 * 0.05),
           weighted_def_turnovers = (def_turnovers_PY1 * 0.7) + (def_turnovers_PY2 * 0.25) + (def_turnovers_PY3 * 0.05),
           weighted_def_plays_pg = (def_plays_pg_PY1 * 0.7) + (def_plays_pg_PY2 * 0.25) + (def_plays_pg_PY3 * 0.05),
           weighted_def_ppg = (def_ppg_PY1 * 0.7) + (def_ppg_PY2 * 0.25) + (def_ppg_PY3 * 0.05),
           ### weighted special teams stats now
           weighted_net_st_epa = (st_net_epa_PY1 * 0.7) + (st_net_epa_PY2 * 0.25) + (st_net_epa_PY3 * 0.25),
           weighted_net_punt_return_yds = ((st_punt_return_yds_PY1 - st_punt_return_yds_allowed_PY1) * 0.7) + ((st_punt_return_yds_PY2 - st_punt_return_yds_allowed_PY2) * 0.25) + ((st_punt_return_yds_PY3 - st_punt_return_yds_allowed_PY3) * 0.05),
           weighted_net_kick_return_yds = ((st_kick_return_yds_PY1 - st_kick_return_yds_allowed_PY1) * 0.7) + ((st_kick_return_yds_PY2 - st_kick_return_yds_allowed_PY2) * 0.25) + ((st_kick_return_yds_PY3 - st_kick_return_yds_allowed_PY3) * 0.05),
           weighted_net_punt_return_TDs = ((st_punt_return_TDs_PY1 - st_punt_return_TDs_allowed_PY1) * 0.7) + ((st_punt_return_TDs_PY2 - st_punt_return_TDs_allowed_PY2) * 0.25) + ((st_punt_return_TDs_PY3 - st_punt_return_TDs_allowed_PY3) * 0.05),
           weighted_net_kick_return_TDs = ((st_kick_return_TDs_PY1 - st_kick_return_TDs_allowed_PY1) * 0.7) + ((st_kick_return_TDs_PY2 - st_kick_return_TDs_allowed_PY2) * 0.25) + ((st_kick_return_TDs_PY3 - st_kick_return_TDs_allowed_PY3) * 0.05),
           weighted_net_fg_rate = ((fg_rate_PY1 - fg_rate_allowed_PY1) * 0.7) + ((fg_rate_PY2 - fg_rate_allowed_PY2) * 0.25) + ((fg_rate_PY3 - fg_rate_allowed_PY3) * 0.05),
           weighted_net_fg_made_pg = ((fg_made_pg_PY1 - fg_made_pg_allowed_PY1) * 0.7) + ((fg_made_pg_PY2 - fg_made_pg_allowed_PY2) * 0.25) + ((fg_made_pg_PY3 - fg_made_pg_allowed_PY3) * 0.05),
           weighted_net_xp_rate = ((xp_rate_PY1 - xp_rate_allowed_PY1) * 0.7) + ((xp_rate_PY2 - xp_rate_allowed_PY2) * 0.25) + ((xp_rate_PY3 - xp_rate_allowed_PY3) * 0.05),
           weighted_net_xp_made_pg = ((xp_made_pg_PY1 - xp_made_pg_allowed_PY1) * 0.7) + ((xp_made_pg_PY2 - xp_made_pg_allowed_PY2) * 0.25) + ((xp_made_pg_PY3 - xp_made_pg_allowed_PY3) * 0.05),
           weighted_net_st_ppg = (net_st_ppg_PY1 * 0.7) + (net_st_ppg_PY2 * 0.25) + (net_st_ppg_PY3 * 0.05),
           off_ppg_aboveavg = weighted_off_ppg - mean(weighted_off_ppg),
           def_ppg_aboveavg = weighted_def_ppg - mean(weighted_def_ppg),
           off_ppg_adj = case_when(weighted_off_ppg > quantile(weighted_off_ppg, 0.8) ~ weighted_off_ppg + (off_ppg_aboveavg / 2),
                                   weighted_off_ppg > mean(weighted_off_ppg) ~ weighted_off_ppg + (off_ppg_aboveavg / 5),
                                   TRUE ~ weighted_off_ppg),
           def_ppg_adj = case_when(weighted_def_ppg > quantile(weighted_def_ppg, 0.8) ~ weighted_def_ppg + (def_ppg_aboveavg / 2),
                                   weighted_def_ppg > mean(weighted_def_ppg) ~ weighted_def_ppg + (def_ppg_aboveavg / 5),
                                   TRUE ~ weighted_def_ppg))
  
  
  ### removing temp objects
  rm(list = ls(pattern = "^temp_"))
  
} else if (as.numeric(week) <= 2){
  ##### Weeks 1-2 stat collection #####
  for (x in 1:nrow(VoA_Variables)) {
    ### creating temp dfs
    temp_offplays <- rushpass_plays |>
      filter(posteam == VoA_Variables$team[x])
    temp_offsuccessplays <- success_plays |>
      filter(posteam == VoA_Variables$team[x])
    temp_offthirddowns <- ThirdDowns |>
      filter(posteam == VoA_Variables$team[x]) |>
      drop_na(third_down_converted)
    temp_conv_offthirddowns <- temp_offthirddowns |>
      filter(third_down_converted == 1)
    temp_off_fourthdowns <- FourthDowns |>
      filter(posteam == VoA_Variables$team[x]) |>
      drop_na(fourth_down_converted)
    temp_conv_offfourthdowns <- temp_off_fourthdowns |>
      filter(fourth_down_converted == 1)
    temp_off_passplays <- passplays |>
      filter(posteam == VoA_Variables$team[x])
    temp_off_comppass <- temp_off_passplays |>
      filter(complete_pass == 1)
    temp_off_rushplays <- rushplays |>
      filter(posteam == VoA_Variables$team[x])
    temp_off_scoringoppplays <- scoringopp_plays |>
      filter(posteam == VoA_Variables$team[x]) |>
      drop_na(drive)
    temp_off_scorringopp_TDs <- temp_off_scoringoppplays |>
      filter(touchdown == 1)
    temp_off_scorringopp_FGs <- temp_off_scoringoppplays |>
      filter(field_goal_result == "made")
    temp_off_turnovers <- turnovers |>
      filter(posteam == VoA_Variables$team[x])
    temp_off_TDs <- TDs |>
      filter(posteam == VoA_Variables$team[x])
    temp_off_2pts <- TwoPts |>
      filter(posteam == VoA_Variables$team[x] & two_point_conv_result == "success")
    ### PY1 def stats
    temp_defplays <- rushpass_plays |>
      filter(defteam == VoA_Variables$team[x])
    temp_defsuccessplays <- success_plays |>
      filter(defteam == VoA_Variables$team[x])
    temp_defthirddowns <- ThirdDowns |>
      filter(defteam == VoA_Variables$team[x]) |>
      drop_na(third_down_converted)
    temp_conv_defthirddowns <- temp_defthirddowns |>
      filter(third_down_converted == 1)
    temp_def_fourthdowns <- FourthDowns |>
      filter(defteam == VoA_Variables$team[x]) |>
      drop_na(fourth_down_converted)
    temp_conv_deffourthdowns <- temp_def_fourthdowns |>
      filter(fourth_down_converted == 1)
    temp_def_passplays <- passplays |>
      filter(defteam == VoA_Variables$team[x])
    temp_def_comppass <- temp_def_passplays |>
      filter(complete_pass == 1)
    temp_def_rushplays <- rushplays |>
      filter(defteam == VoA_Variables$team[x])
    temp_def_scoringoppplays <- scoringopp_plays |>
      filter(defteam == VoA_Variables$team[x]) |>
      drop_na(drive)
    temp_def_scorringopp_TDs <- temp_def_scoringoppplays |>
      filter(touchdown == 1)
    temp_def_scorringopp_FGs <- temp_def_scoringoppplays |>
      filter(field_goal_result == "made")
    temp_def_turnovers <- turnovers |>
      filter(defteam == VoA_Variables$team[x])
    temp_def_TDs <- TDs |>
      filter(defteam == VoA_Variables$team[x])
    temp_def_2pts <- TwoPts |>
      filter(defteam == VoA_Variables$team[x] & two_point_conv_result == "success")
    ### temp PY1 special teams dfs
    ## on kickoffs, defteam does kicking
    ## on punts, posteam does punting
    temp_off_FGs <- FGs |>
      filter(posteam == VoA_Variables$team[x])
    temp_off_goodFGs <- temp_off_FGs |>
      filter(field_goal_result == "made")
    temp_def_FGs <- FGs |>
      filter(defteam == VoA_Variables$team[x] & field_goal_result == "made")
    temp_def_goodFGs <- temp_def_FGs |>
      filter(field_goal_result == "made")
    temp_returned_punts <- punts |>
      filter(defteam == VoA_Variables$team[x])
    temp_returned_kicks <- kickoffs |>
      filter(posteam == VoA_Variables$team[x])
    temp_returned_punt_TDs <- temp_returned_punts |>
      filter(return_touchdown == 1)
    temp_returned_kick_TDs <- temp_returned_kicks |>
      filter(return_touchdown == 1)
    temp_kicked_punts <- punts |>
      filter(posteam == VoA_Variables$team[x])
    temp_kicked_kicks <- kickoffs |>
      filter(defteam == VoA_Variables$team[x])
    temp_kicked_punt_TDs <- temp_kicked_punts |>
      filter(return_touchdown == 1)
    temp_kicked_kick_TDs <- temp_kicked_kicks |>
      filter(return_touchdown == 1)
    temp_off_xps <- XPts |>
      filter(posteam == VoA_Variables$team[x])
    temp_def_xps <- XPts |>
      filter(defteam == VoA_Variables$team[x])
    temp_off_good_xps <- temp_off_xps |>
      filter(extra_point_result == "good")
    temp_def_good_xps <- temp_def_xps |>
      filter(extra_point_result == "good")
    ### used to get net ST epa/play
    temp_off_st_plays <- rbind(temp_off_FGs, temp_off_xps, temp_returned_kicks, temp_returned_punts)
    temp_def_st_plays <- rbind(temp_def_FGs, temp_def_xps, temp_kicked_kicks, temp_kicked_punts)
    
    ### Evaluating Stats
    VoA_Variables$off_ypp[x] = mean(temp_offplays$yards_gained)
    VoA_Variables$off_epa[x] = mean(temp_offplays$epa)
    VoA_Variables$off_success_rt[x] = nrow(temp_offsuccessplays) / nrow(temp_offplays)
    VoA_Variables$off_explosiveness[x] = mean(temp_offsuccessplays$epa)
    VoA_Variables$off_third_conv_rate[x] = nrow(temp_conv_offthirddowns) / nrow(temp_offthirddowns)
    VoA_Variables$off_fourth_conv_rate[x] = nrow(temp_conv_offfourthdowns) / nrow(temp_off_fourthdowns)
    VoA_Variables$off_pass_ypa[x] = mean(temp_off_passplays$yards_gained)
    VoA_Variables$off_pass_ypc[x] = mean(temp_off_comppass$yards_gained)
    VoA_Variables$off_rush_ypa[x] = mean(temp_off_rushplays$yards_gained)
    VoA_Variables$off_pts_per_opp[x] = ((nrow(temp_off_scorringopp_TDs) * 6) + (nrow(temp_off_scorringopp_FGs) * 3)) / length(unique(paste0(temp_off_scoringoppplays$game_id, temp_off_scoringoppplays$drive)))
    VoA_Variables$off_turnovers[x] = nrow(temp_off_turnovers) / length(unique(temp_offplays$week))
    VoA_Variables$off_plays_pg[x] = nrow(temp_offplays) / length(unique(temp_offplays$week))
    VoA_Variables$off_ppg[x] = ((nrow(temp_off_TDs) * 6) + (nrow(temp_off_2pts) * 2)) / length(unique(temp_off_rushplays$week))
    ## PY1 defensive stats now
    VoA_Variables$def_ypp[x] = mean(temp_defplays$yards_gained)
    VoA_Variables$def_epa[x] = mean(temp_defplays$epa)
    VoA_Variables$def_success_rt[x] = nrow(temp_defsuccessplays) / nrow(temp_defplays)
    VoA_Variables$def_explosiveness[x] = mean(temp_defsuccessplays$epa)
    VoA_Variables$def_third_conv_rate[x] = nrow(temp_conv_defthirddowns) / nrow(temp_defthirddowns)
    VoA_Variables$def_fourth_conv_rate[x] = nrow(temp_conv_deffourthdowns) / nrow(temp_def_fourthdowns)
    VoA_Variables$def_pass_ypa[x] = mean(temp_def_passplays$yards_gained)
    VoA_Variables$def_pass_ypc[x] = mean(temp_def_comppass$yards_gained)
    VoA_Variables$def_rush_ypa[x] = mean(temp_def_rushplays$yards_gained)
    VoA_Variables$def_pts_per_opp[x] = ((nrow(temp_def_scorringopp_TDs) * 6) + (nrow(temp_def_scorringopp_FGs) * 3)) / length(unique(paste0(temp_def_scoringoppplays$game_id, temp_def_scoringoppplays$drive)))
    VoA_Variables$def_turnovers[x] = nrow(temp_def_turnovers) / length(unique(temp_defplays$week))
    VoA_Variables$def_plays_pg[x] = nrow(temp_defplays) / length(unique(temp_defplays$week))
    VoA_Variables$def_ppg[x] = ((nrow(temp_def_TDs) * 6) + (nrow(temp_def_2pts) * 2)) / length(unique(temp_def_rushplays$week))
    ## PY1 Special teams stats now
    VoA_Variables$st_net_epa[x] = mean(temp_off_st_plays$epa) - mean(temp_def_st_plays$epa)
    VoA_Variables$st_punt_return_yds[x] = mean(temp_returned_punts$return_yards)
    VoA_Variables$st_kick_return_yds[x] = mean(temp_returned_kicks$return_yards)
    VoA_Variables$st_kick_return_TDs[x] = nrow(temp_returned_kick_TDs) / length(unique(temp_offplays$week))
    VoA_Variables$st_punt_return_TDs[x] = nrow(temp_returned_punt_TDs) / length(unique(temp_offplays$week))
    if (nrow(temp_off_FGs) == 0 | is.na(nrow(temp_off_FGs))){
      VoA_Variables$fg_rate[x] = 0
    } else{
      VoA_Variables$fg_rate[x] = nrow(temp_off_goodFGs) / nrow(temp_off_FGs)
    }
    VoA_Variables$fg_made_pg[x] = nrow(temp_off_goodFGs) / length(unique(temp_offplays$week))
    if (nrow(temp_off_xps) == 0 | is.na(nrow(temp_off_xps))){
      VoA_Variables$xp_rate[x] = 0
    } else{
      VoA_Variables$xp_rate[x] = nrow(temp_off_good_xps) / nrow(temp_off_xps)
    }
    VoA_Variables$xp_made_pg[x] = nrow(temp_off_good_xps) / length(unique(temp_offplays$week))
    VoA_Variables$st_punt_return_yds_allowed[x] = mean(temp_kicked_punts$return_yards)
    VoA_Variables$st_kick_return_yds_allowed[x] = mean(temp_kicked_kicks$return_yards)
    VoA_Variables$st_kick_return_TDs_allowed[x] = nrow(temp_kicked_kick_TDs) / length(unique(temp_offplays$week))
    VoA_Variables$st_punt_return_TDs_allowed[x] = nrow(temp_kicked_punt_TDs) / length(unique(temp_offplays$week))
    if (nrow(temp_def_FGs) == 0 | is.na(nrow(temp_def_FGs))){
      VoA_Variables$fg_rate_allowed[x] = 0
    } else{
      VoA_Variables$fg_rate_allowed[x] = nrow(temp_def_goodFGs) / nrow(temp_def_FGs)
    }
    VoA_Variables$fg_made_pg_allowed[x] = nrow(temp_def_goodFGs) / length(unique(temp_offplays$week))
    if (nrow(temp_def_xps) == 0 | is.na(nrow(temp_def_xps))){
      VoA_Variables$xp_rate_allowed[x] = 0
    } else{
      VoA_Variables$xp_rate_allowed[x] = nrow(temp_def_good_xps) / nrow(temp_def_xps)
    }
    VoA_Variables$xp_made_pg_allowed[x] = nrow(temp_def_good_xps) / length(unique(temp_offplays$week))
    VoA_Variables$net_st_ppg[x] = (((nrow(temp_off_goodFGs) * 3) + (nrow(temp_returned_punt_TDs) * 6) + (nrow(temp_returned_kick_TDs) * 6) + nrow(temp_off_good_xps)) - ((nrow(temp_def_goodFGs) * 3) + (nrow(temp_kicked_punt_TDs) * 6) + (nrow(temp_kicked_kick_TDs) * 6) + nrow(temp_def_good_xps))) / length(unique(temp_offplays$week))
  }
  
  
  ### binding csv of PY data to VoA Variables, which should only contain current season data at this point
  VoA_Vars_dfs <- list(VoA_Variables, PY_VoAVars)
  VoA_Variables <- VoA_Vars_dfs |>
    reduce(full_join, by = "team") |>
    ### Adding columns of variables weighted by season
    ### adding weighted variables (offense first)
    mutate(weighted_off_ypp = (off_ypp_PY1 * 0.6) + (off_ypp_PY2 * 0.05) + (off_ypp * 0.35),
           weighted_off_epa = (off_epa_PY1 * 0.6) + (off_epa_PY2 * 0.05) + (off_epa * 0.35),
           weighted_off_success_rt = (off_success_rt_PY1 * 0.6) + (off_success_rt_PY2 * 0.05) + (off_success_rt * 0.35),
           weighted_off_explosiveness = (off_explosiveness_PY1 * 0.6) + (off_explosiveness_PY2 * 0.05) + (off_explosiveness * 0.35),
           weighted_off_third_conv_rate = (off_third_conv_rate_PY1 * 0.6) + (off_third_conv_rate_PY2 * 0.05) + (off_third_conv_rate * 0.35),
           weighted_off_fourth_conv_rate = (off_fourth_conv_rate_PY1 * 0.6) + (off_fourth_conv_rate_PY2 * 0.05) + (off_fourth_conv_rate * 0.35),
           weighted_off_pass_ypa = (off_pass_ypa_PY1 * 0.6) + (off_pass_ypa_PY2 * 0.05) + (off_pass_ypa * 0.35),
           weighted_off_pass_ypc = (off_pass_ypc_PY1 * 0.6) + (off_pass_ypc_PY2 * 0.05) + (off_pass_ypc * 0.35),
           weighted_off_rush_ypa = (off_rush_ypa_PY1 * 0.6) + (off_rush_ypa_PY2 * 0.05) + (off_rush_ypa * 0.35),
           weighted_off_pts_per_opp = (off_pts_per_opp_PY1 * 0.6) + (off_pts_per_opp_PY2 * 0.05) + (off_pts_per_opp * 0.35),
           weighted_off_turnovers = (off_turnovers_PY1 * 0.6) + (off_turnovers_PY2 * 0.05) + (off_turnovers * 0.35),
           weighted_off_plays_pg = (off_plays_pg_PY1 * 0.6) + (off_plays_pg_PY2 * 0.05) + (off_plays_pg * 0.35),
           weighted_off_ppg = (off_ppg_PY1 * 0.6) + (off_ppg_PY2 * 0.05) + (off_ppg * 0.35),
           ### weighted defensive stats now
           weighted_def_ypp = (def_ypp_PY1 * 0.6) + (def_ypp_PY2 * 0.05) + (def_ypp * 0.35),
           weighted_def_epa = (def_epa_PY1 * 0.6) + (def_epa_PY2 * 0.05) + (def_epa * 0.35),
           weighted_def_success_rt = (def_success_rt_PY1 * 0.6) + (def_success_rt_PY2 * 0.05) + (def_success_rt * 0.35),
           weighted_def_explosiveness = (def_explosiveness_PY1 * 0.6) + (def_explosiveness_PY2 * 0.05) + (def_explosiveness * 0.35),
           weighted_def_third_conv_rate = (def_third_conv_rate_PY1 * 0.6) + (def_third_conv_rate_PY2 * 0.05) + (def_third_conv_rate * 0.35),
           weighted_def_fourth_conv_rate = (def_fourth_conv_rate_PY1 * 0.6) + (def_fourth_conv_rate_PY2 * 0.05) + (def_fourth_conv_rate * 0.35),
           weighted_def_pass_ypa = (def_pass_ypa_PY1 * 0.6) + (def_pass_ypa_PY2 * 0.05) + (def_pass_ypa * 0.35),
           weighted_def_pass_ypc = (def_pass_ypc_PY1 * 0.6) + (def_pass_ypc_PY2 * 0.05) + (def_pass_ypc * 0.35),
           weighted_def_rush_ypa = (def_rush_ypa_PY1 * 0.6) + (def_rush_ypa_PY2 * 0.05) + (def_rush_ypa * 0.35),
           weighted_def_pts_per_opp = (def_pts_per_opp_PY1 * 0.6) + (def_pts_per_opp_PY2 * 0.05) + (def_pts_per_opp * 0.35),
           weighted_def_turnovers = (def_turnovers_PY1 * 0.6) + (def_turnovers_PY2 * 0.05) + (def_turnovers * 0.35),
           weighted_def_plays_pg = (def_plays_pg_PY1 * 0.6) + (def_plays_pg_PY2 * 0.05) + (def_plays_pg * 0.35),
           weighted_def_ppg = (def_ppg_PY1 * 0.6) + (def_ppg_PY2 * 0.05) + (def_ppg * 0.35),
           ### weighted special teams stats now
           weighted_net_st_epa = (st_net_epa_PY1 * 0.6) + (st_net_epa_PY2 * 0.05) + (st_net_epa * 0.35),
           weighted_net_punt_return_yds = ((st_punt_return_yds_PY1 - st_punt_return_yds_allowed_PY1) * 0.6) + ((st_punt_return_yds_PY2 - st_punt_return_yds_allowed_PY2) * 0.05) + ((st_punt_return_yds - st_punt_return_yds_allowed) * 0.35),
           weighted_net_kick_return_yds = ((st_kick_return_yds_PY1 - st_kick_return_yds_allowed_PY1) * 0.6) + ((st_kick_return_yds_PY2 - st_kick_return_yds_allowed_PY2) * 0.05) + ((st_kick_return_yds - st_kick_return_yds_allowed) * 0.35),
           weighted_net_punt_return_TDs = ((st_punt_return_TDs_PY1 - st_punt_return_TDs_allowed_PY1) * 0.6) + ((st_punt_return_TDs_PY2 - st_punt_return_TDs_allowed_PY2) * 0.05) + ((st_punt_return_TDs - st_punt_return_TDs_allowed) * 0.35),
           weighted_net_kick_return_TDs = ((st_kick_return_TDs_PY1 - st_kick_return_TDs_allowed_PY1) * 0.6) + ((st_kick_return_TDs_PY2 - st_kick_return_TDs_allowed_PY2) * 0.05) + ((st_kick_return_TDs - st_kick_return_TDs_allowed) * 0.35),
           weighted_net_fg_rate = ((fg_rate_PY1 - fg_rate_allowed_PY1) * 0.6) + ((fg_rate_PY2 - fg_rate_allowed_PY2) * 0.05) + ((fg_rate - fg_rate_allowed) * 0.35),
           weighted_net_fg_made_pg = ((fg_made_pg_PY1 - fg_made_pg_allowed_PY1) * 0.6) + ((fg_made_pg_PY2 - fg_made_pg_allowed_PY2) * 0.05) + ((fg_made_pg - fg_made_pg_allowed) * 0.35),
           weighted_net_xp_rate = ((xp_rate_PY1 - xp_rate_allowed_PY1) * 0.6) + ((xp_rate_PY2 - xp_rate_allowed_PY2) * 0.05) + ((xp_rate - xp_rate_allowed) * 0.35),
           weighted_net_xp_made_pg = ((xp_made_pg_PY1 - xp_made_pg_allowed_PY1) * 0.6) + ((xp_made_pg_PY2 - xp_made_pg_allowed_PY2) * 0.05) + ((xp_made_pg - xp_made_pg_allowed) * 0.35),
           weighted_net_st_ppg = (net_st_ppg_PY1 * 0.6) + (net_st_ppg_PY2 * 0.05) + (net_st_ppg * 0.35),
           off_ppg_aboveavg = weighted_off_ppg - mean(weighted_off_ppg),
           def_ppg_aboveavg = weighted_def_ppg - mean(weighted_def_ppg),
           off_ppg_adj = case_when(weighted_off_ppg > quantile(weighted_off_ppg, 0.8) ~ weighted_off_ppg + (off_ppg_aboveavg / 2),
                                   weighted_off_ppg > mean(weighted_off_ppg) ~ weighted_off_ppg + (off_ppg_aboveavg / 5),
                                   TRUE ~ weighted_off_ppg),
           def_ppg_adj = case_when(weighted_def_ppg > quantile(weighted_def_ppg, 0.8) ~ weighted_def_ppg + (def_ppg_aboveavg / 2),
                                   weighted_def_ppg > mean(weighted_def_ppg) ~ weighted_def_ppg + (def_ppg_aboveavg / 5),
                                   TRUE ~ weighted_def_ppg))
  
  
  ### removing temp objects
  rm(list = ls(pattern = "^temp_"))
  
  
} else if (as.numeric(week) <= 5){
  ##### Weeks 3-5 Stat Collection #####
  for (x in 1:nrow(VoA_Variables)){
    ### creating temp dfs
    temp_offplays <- rushpass_plays |>
      filter(posteam == VoA_Variables$team[x])
    temp_offsuccessplays <- success_plays |>
      filter(posteam == VoA_Variables$team[x])
    temp_offthirddowns <- ThirdDowns |>
      filter(posteam == VoA_Variables$team[x]) |>
      drop_na(third_down_converted)
    temp_conv_offthirddowns <- temp_offthirddowns |>
      filter(third_down_converted == 1)
    temp_off_fourthdowns <- FourthDowns |>
      filter(posteam == VoA_Variables$team[x]) |>
      drop_na(fourth_down_converted)
    temp_conv_offfourthdowns <- temp_off_fourthdowns |>
      filter(fourth_down_converted == 1)
    temp_off_passplays <- passplays |>
      filter(posteam == VoA_Variables$team[x])
    temp_off_comppass <- temp_off_passplays |>
      filter(complete_pass == 1)
    temp_off_rushplays <- rushplays |>
      filter(posteam == VoA_Variables$team[x])
    temp_off_scoringoppplays <- scoringopp_plays |>
      filter(posteam == VoA_Variables$team[x]) |>
      drop_na(drive)
    temp_off_scorringopp_TDs <- temp_off_scoringoppplays |>
      filter(touchdown == 1)
    temp_off_scorringopp_FGs <- temp_off_scoringoppplays |>
      filter(field_goal_result == "made")
    temp_off_turnovers <- turnovers |>
      filter(posteam == VoA_Variables$team[x])
    temp_off_TDs <- TDs |>
      filter(posteam == VoA_Variables$team[x])
    temp_off_2pts <- TwoPts |>
      filter(posteam == VoA_Variables$team[x] & two_point_conv_result == "success")
    ### PY1 def stats
    temp_defplays <- rushpass_plays |>
      filter(defteam == VoA_Variables$team[x])
    temp_defsuccessplays <- success_plays |>
      filter(defteam == VoA_Variables$team[x])
    temp_defthirddowns <- ThirdDowns |>
      filter(defteam == VoA_Variables$team[x]) |>
      drop_na(third_down_converted)
    temp_conv_defthirddowns <- temp_defthirddowns |>
      filter(third_down_converted == 1)
    temp_def_fourthdowns <- FourthDowns |>
      filter(defteam == VoA_Variables$team[x]) |>
      drop_na(fourth_down_converted)
    temp_conv_deffourthdowns <- temp_def_fourthdowns |>
      filter(fourth_down_converted == 1)
    temp_def_passplays <- passplays |>
      filter(defteam == VoA_Variables$team[x])
    temp_def_comppass <- temp_def_passplays |>
      filter(complete_pass == 1)
    temp_def_rushplays <- rushplays |>
      filter(defteam == VoA_Variables$team[x])
    temp_def_scoringoppplays <- scoringopp_plays |>
      filter(defteam == VoA_Variables$team[x]) |>
      drop_na(drive)
    temp_def_scorringopp_TDs <- temp_def_scoringoppplays |>
      filter(touchdown == 1)
    temp_def_scorringopp_FGs <- temp_def_scoringoppplays |>
      filter(field_goal_result == "made")
    temp_def_turnovers <- turnovers |>
      filter(defteam == VoA_Variables$team[x])
    temp_def_TDs <- TDs |>
      filter(defteam == VoA_Variables$team[x])
    temp_def_2pts <- TwoPts |>
      filter(defteam == VoA_Variables$team[x] & two_point_conv_result == "success")
    ### temp PY1 special teams dfs
    ## on kickoffs, defteam does kicking
    ## on punts, posteam does punting
    temp_off_FGs <- FGs |>
      filter(posteam == VoA_Variables$team[x])
    temp_off_goodFGs <- temp_off_FGs |>
      filter(field_goal_result == "made")
    temp_def_FGs <- FGs |>
      filter(defteam == VoA_Variables$team[x] & field_goal_result == "made")
    temp_def_goodFGs <- temp_def_FGs |>
      filter(field_goal_result == "made")
    temp_returned_punts <- punts |>
      filter(defteam == VoA_Variables$team[x])
    temp_returned_kicks <- kickoffs |>
      filter(posteam == VoA_Variables$team[x])
    temp_returned_punt_TDs <- temp_returned_punts |>
      filter(return_touchdown == 1)
    temp_returned_kick_TDs <- temp_returned_kicks |>
      filter(return_touchdown == 1)
    temp_kicked_punts <- punts |>
      filter(posteam == VoA_Variables$team[x])
    temp_kicked_kicks <- kickoffs |>
      filter(defteam == VoA_Variables$team[x])
    temp_kicked_punt_TDs <- temp_kicked_punts |>
      filter(return_touchdown == 1)
    temp_kicked_kick_TDs <- temp_kicked_kicks |>
      filter(return_touchdown == 1)
    temp_off_xps <- XPts |>
      filter(posteam == VoA_Variables$team[x])
    temp_def_xps <- XPts |>
      filter(defteam == VoA_Variables$team[x])
    temp_off_good_xps <- temp_off_xps |>
      filter(extra_point_result == "good")
    temp_def_good_xps <- temp_def_xps |>
      filter(extra_point_result == "good")
    ### used to get net ST epa/play
    temp_off_st_plays <- rbind(temp_off_FGs, temp_off_xps, temp_returned_kicks, temp_returned_punts)
    temp_def_st_plays <- rbind(temp_def_FGs, temp_def_xps, temp_kicked_kicks, temp_kicked_punts)
    
    ### Evaluating Stats
    VoA_Variables$off_ypp[x] = mean(temp_offplays$yards_gained)
    VoA_Variables$off_epa[x] = mean(temp_offplays$epa)
    VoA_Variables$off_success_rt[x] = nrow(temp_offsuccessplays) / nrow(temp_offplays)
    VoA_Variables$off_explosiveness[x] = mean(temp_offsuccessplays$epa)
    VoA_Variables$off_third_conv_rate[x] = nrow(temp_conv_offthirddowns) / nrow(temp_offthirddowns)
    VoA_Variables$off_fourth_conv_rate[x] = nrow(temp_conv_offfourthdowns) / nrow(temp_off_fourthdowns)
    VoA_Variables$off_pass_ypa[x] = mean(temp_off_passplays$yards_gained)
    VoA_Variables$off_pass_ypc[x] = mean(temp_off_comppass$yards_gained)
    VoA_Variables$off_rush_ypa[x] = mean(temp_off_rushplays$yards_gained)
    VoA_Variables$off_pts_per_opp[x] = ((nrow(temp_off_scorringopp_TDs) * 6) + (nrow(temp_off_scorringopp_FGs) * 3)) / length(unique(paste0(temp_off_scoringoppplays$game_id, temp_off_scoringoppplays$drive)))
    VoA_Variables$off_turnovers[x] = nrow(temp_off_turnovers) / length(unique(temp_offplays$week))
    VoA_Variables$off_plays_pg[x] = nrow(temp_offplays) / length(unique(temp_offplays$week))
    VoA_Variables$off_ppg[x] = ((nrow(temp_off_TDs) * 6) + (nrow(temp_off_2pts) * 2)) / length(unique(temp_off_rushplays$week))
    ## PY1 defensive stats now
    VoA_Variables$def_ypp[x] = mean(temp_defplays$yards_gained)
    VoA_Variables$def_epa[x] = mean(temp_defplays$epa)
    VoA_Variables$def_success_rt[x] = nrow(temp_defsuccessplays) / nrow(temp_defplays)
    VoA_Variables$def_explosiveness[x] = mean(temp_defsuccessplays$epa)
    VoA_Variables$def_third_conv_rate[x] = nrow(temp_conv_defthirddowns) / nrow(temp_defthirddowns)
    VoA_Variables$def_fourth_conv_rate[x] = nrow(temp_conv_deffourthdowns) / nrow(temp_def_fourthdowns)
    VoA_Variables$def_pass_ypa[x] = mean(temp_def_passplays$yards_gained)
    VoA_Variables$def_pass_ypc[x] = mean(temp_def_comppass$yards_gained)
    VoA_Variables$def_rush_ypa[x] = mean(temp_def_rushplays$yards_gained)
    VoA_Variables$def_pts_per_opp[x] = ((nrow(temp_def_scorringopp_TDs) * 6) + (nrow(temp_def_scorringopp_FGs) * 3)) / length(unique(paste0(temp_def_scoringoppplays$game_id, temp_def_scoringoppplays$drive)))
    VoA_Variables$def_turnovers[x] = nrow(temp_def_turnovers) / length(unique(temp_defplays$week))
    VoA_Variables$def_plays_pg[x] = nrow(temp_defplays) / length(unique(temp_defplays$week))
    VoA_Variables$def_ppg[x] = ((nrow(temp_def_TDs) * 6) + (nrow(temp_def_2pts) * 2)) / length(unique(temp_def_rushplays$week))
    ## PY1 Special teams stats now
    VoA_Variables$st_net_epa[x] = mean(temp_off_st_plays$epa) - mean(temp_def_st_plays$epa)
    VoA_Variables$st_punt_return_yds[x] = mean(temp_returned_punts$return_yards)
    VoA_Variables$st_kick_return_yds[x] = mean(temp_returned_kicks$return_yards)
    VoA_Variables$st_kick_return_TDs[x] = nrow(temp_returned_kick_TDs) / length(unique(temp_offplays$week))
    VoA_Variables$st_punt_return_TDs[x] = nrow(temp_returned_punt_TDs) / length(unique(temp_offplays$week))
    VoA_Variables$fg_rate[x] = nrow(temp_off_goodFGs) / nrow(temp_off_FGs)
    VoA_Variables$fg_made_pg[x] = nrow(temp_off_goodFGs) / length(unique(temp_offplays$week))
    VoA_Variables$xp_rate[x] = nrow(temp_off_good_xps) / nrow(temp_off_xps)
    VoA_Variables$xp_made_pg[x] = nrow(temp_off_good_xps) / length(unique(temp_offplays$week))
    VoA_Variables$st_punt_return_yds_allowed[x] = mean(temp_kicked_punts$return_yards)
    VoA_Variables$st_kick_return_yds_allowed[x] = mean(temp_kicked_kicks$return_yards)
    VoA_Variables$st_kick_return_TDs_allowed[x] = nrow(temp_kicked_kick_TDs) / length(unique(temp_offplays$week))
    VoA_Variables$st_punt_return_TDs_allowed[x] = nrow(temp_kicked_punt_TDs) / length(unique(temp_offplays$week))
    VoA_Variables$fg_rate_allowed[x] = nrow(temp_def_goodFGs) / nrow(temp_def_FGs)
    VoA_Variables$fg_made_pg_allowed[x] = nrow(temp_def_goodFGs) / length(unique(temp_offplays$week))
    VoA_Variables$xp_rate_allowed[x] = nrow(temp_def_good_xps) / nrow(temp_def_xps)
    VoA_Variables$xp_made_pg_allowed[x] = nrow(temp_def_good_xps) / length(unique(temp_offplays$week))
    VoA_Variables$net_st_ppg[x] = (((nrow(temp_off_goodFGs) * 3) + (nrow(temp_returned_punt_TDs) * 6) + (nrow(temp_returned_kick_TDs) * 6) + nrow(temp_off_good_xps)) - ((nrow(temp_def_goodFGs) * 3) + (nrow(temp_kicked_punt_TDs) * 6) + (nrow(temp_kicked_kick_TDs) * 6) + nrow(temp_def_good_xps))) / length(unique(temp_offplays$week))
  }
  
  ### binding csv of PY data to VoA Variables, which should only contain current season data at this point
  VoA_Vars_dfs <- list(VoA_Variables, PY_VoAVars)
  VoA_Variables <- VoA_Vars_dfs |>
    reduce(full_join, by = "team") |>
    ### Adding columns of variables weighted by season
    ### adding weighted variables (offense first)
    mutate(weighted_off_ypp = (off_ypp_PY1 * 0.6) + (off_ypp * 0.35),
           weighted_off_epa = (off_epa_PY1 * 0.6) + (off_epa * 0.35),
           weighted_off_success_rt = (off_success_rt_PY1 * 0.6) + (off_success_rt * 0.35),
           weighted_off_explosiveness = (off_explosiveness_PY1 * 0.6) + (off_explosiveness * 0.35),
           weighted_off_third_conv_rate = (off_third_conv_rate_PY1 * 0.6) + (off_third_conv_rate * 0.35),
           weighted_off_fourth_conv_rate = (off_fourth_conv_rate_PY1 * 0.6) + (off_fourth_conv_rate * 0.35),
           weighted_off_pass_ypa = (off_pass_ypa_PY1 * 0.6) + (off_pass_ypa * 0.35),
           weighted_off_pass_ypc = (off_pass_ypc_PY1 * 0.6) + (off_pass_ypc * 0.35),
           weighted_off_rush_ypa = (off_rush_ypa_PY1 * 0.6) + (off_rush_ypa * 0.35),
           weighted_off_pts_per_opp = (off_pts_per_opp_PY1 * 0.6) + (off_pts_per_opp * 0.35),
           weighted_off_turnovers = (off_turnovers_PY1 * 0.6) + (off_turnovers * 0.35),
           weighted_off_plays_pg = (off_plays_pg_PY1 * 0.6) + (off_plays_pg * 0.35),
           weighted_off_ppg = (off_ppg_PY1 * 0.6) + (off_ppg * 0.35),
           ### weighted defensive stats now
           weighted_def_ypp = (def_ypp_PY1 * 0.6) + (def_ypp * 0.35),
           weighted_def_epa = (def_epa_PY1 * 0.6) + (def_epa * 0.35),
           weighted_def_success_rt = (def_success_rt_PY1 * 0.6) + (def_success_rt * 0.35),
           weighted_def_explosiveness = (def_explosiveness_PY1 * 0.6) + (def_explosiveness * 0.35),
           weighted_def_third_conv_rate = (def_third_conv_rate_PY1 * 0.6) + (def_third_conv_rate * 0.35),
           weighted_def_fourth_conv_rate = (def_fourth_conv_rate_PY1 * 0.6) + (def_fourth_conv_rate * 0.35),
           weighted_def_pass_ypa = (def_pass_ypa_PY1 * 0.6) + (def_pass_ypa * 0.35),
           weighted_def_pass_ypc = (def_pass_ypc_PY1 * 0.6) + (def_pass_ypc * 0.35),
           weighted_def_rush_ypa = (def_rush_ypa_PY1 * 0.6) + (def_rush_ypa * 0.35),
           weighted_def_pts_per_opp = (def_pts_per_opp_PY1 * 0.6) + (def_pts_per_opp * 0.35),
           weighted_def_turnovers = (def_turnovers_PY1 * 0.6) + (def_turnovers * 0.35),
           weighted_def_plays_pg = (def_plays_pg_PY1 * 0.6) + (def_plays_pg * 0.35),
           weighted_def_ppg = (def_ppg_PY1 * 0.6) + (def_ppg * 0.35),
           ### weighted special teams stats now
           weighted_net_st_epa = (st_net_epa_PY1 * 0.6) + (st_net_epa * 0.35),
           weighted_net_punt_return_yds = ((st_punt_return_yds_PY1 - st_punt_return_yds_allowed_PY1) * 0.6) + ((st_punt_return_yds - st_punt_return_yds_allowed) * 0.35),
           weighted_net_kick_return_yds = ((st_kick_return_yds_PY1 - st_kick_return_yds_allowed_PY1) * 0.6) + ((st_kick_return_yds - st_kick_return_yds_allowed) * 0.35),
           weighted_net_punt_return_TDs = ((st_punt_return_TDs_PY1 - st_punt_return_TDs_allowed_PY1) * 0.6) + ((st_punt_return_TDs - st_punt_return_TDs_allowed) * 0.35),
           weighted_net_kick_return_TDs = ((st_kick_return_TDs_PY1 - st_kick_return_TDs_allowed_PY1) * 0.6) + ((st_kick_return_TDs - st_kick_return_TDs_allowed) * 0.35),
           weighted_net_fg_rate = ((fg_rate_PY1 - fg_rate_allowed_PY1) * 0.6) + ((fg_rate - fg_rate_allowed) * 0.35),
           weighted_net_fg_made_pg = ((fg_made_pg_PY1 - fg_made_pg_allowed_PY1) * 0.6) + ((fg_made_pg - fg_made_pg_allowed) * 0.35),
           weighted_net_xp_rate = ((xp_rate_PY1 - xp_rate_allowed_PY1) * 0.6) + ((xp_rate - xp_rate_allowed) * 0.35),
           weighted_net_xp_made_pg = ((xp_made_pg_PY1 - xp_made_pg_allowed_PY1) * 0.6) + ((xp_made_pg - xp_made_pg_allowed) * 0.35),
           weighted_net_st_ppg = (net_st_ppg_PY1 * 0.6) + (net_st_ppg * 0.35),
           off_ppg_aboveavg = weighted_off_ppg - mean(weighted_off_ppg),
           def_ppg_aboveavg = weighted_def_ppg - mean(weighted_def_ppg),
           off_ppg_adj = case_when(weighted_off_ppg > quantile(weighted_off_ppg, 0.8) ~ weighted_off_ppg + (off_ppg_aboveavg / 2),
                                   weighted_off_ppg > mean(weighted_off_ppg) ~ weighted_off_ppg + (off_ppg_aboveavg / 5),
                                   TRUE ~ weighted_off_ppg),
           def_ppg_adj = case_when(weighted_def_ppg > quantile(weighted_def_ppg, 0.8) ~ weighted_def_ppg + (def_ppg_aboveavg / 2),
                                   weighted_def_ppg > mean(weighted_def_ppg) ~ weighted_def_ppg + (def_ppg_aboveavg / 5),
                                   TRUE ~ weighted_def_ppg))
  
  
  ### removing temp objects
  rm(list = ls(pattern = "^temp_"))
  
} else{
  ##### Week 6 - End of Season Stat Collection #####
  for (x in 1:nrow(VoA_Variables)){
    ### creating temp dfs
    temp_offplays <- rushpass_plays |>
      filter(posteam == VoA_Variables$team[x])
    temp_offsuccessplays <- success_plays |>
      filter(posteam == VoA_Variables$team[x])
    temp_offthirddowns <- ThirdDowns |>
      filter(posteam == VoA_Variables$team[x]) |>
      drop_na(third_down_converted)
    temp_conv_offthirddowns <- temp_offthirddowns |>
      filter(third_down_converted == 1)
    temp_off_fourthdowns <- FourthDowns |>
      filter(posteam == VoA_Variables$team[x]) |>
      drop_na(fourth_down_converted)
    temp_conv_offfourthdowns <- temp_off_fourthdowns |>
      filter(fourth_down_converted == 1)
    temp_off_passplays <- passplays |>
      filter(posteam == VoA_Variables$team[x])
    temp_off_comppass <- temp_off_passplays |>
      filter(complete_pass == 1)
    temp_off_rushplays <- rushplays |>
      filter(posteam == VoA_Variables$team[x])
    temp_off_scoringoppplays <- scoringopp_plays |>
      filter(posteam == VoA_Variables$team[x]) |>
      drop_na(drive)
    temp_off_scorringopp_TDs <- temp_off_scoringoppplays |>
      filter(touchdown == 1)
    temp_off_scorringopp_FGs <- temp_off_scoringoppplays |>
      filter(field_goal_result == "made")
    temp_off_turnovers <- turnovers |>
      filter(posteam == VoA_Variables$team[x])
    temp_off_TDs <- TDs |>
      filter(posteam == VoA_Variables$team[x])
    temp_off_2pts <- TwoPts |>
      filter(posteam == VoA_Variables$team[x] & two_point_conv_result == "success")
    ### PY1 def stats
    temp_defplays <- rushpass_plays |>
      filter(defteam == VoA_Variables$team[x])
    temp_defsuccessplays <- success_plays |>
      filter(defteam == VoA_Variables$team[x])
    temp_defthirddowns <- ThirdDowns |>
      filter(defteam == VoA_Variables$team[x]) |>
      drop_na(third_down_converted)
    temp_conv_defthirddowns <- temp_defthirddowns |>
      filter(third_down_converted == 1)
    temp_def_fourthdowns <- FourthDowns |>
      filter(defteam == VoA_Variables$team[x]) |>
      drop_na(fourth_down_converted)
    temp_conv_deffourthdowns <- temp_def_fourthdowns |>
      filter(fourth_down_converted == 1)
    temp_def_passplays <- passplays |>
      filter(defteam == VoA_Variables$team[x])
    temp_def_comppass <- temp_def_passplays |>
      filter(complete_pass == 1)
    temp_def_rushplays <- rushplays |>
      filter(defteam == VoA_Variables$team[x])
    temp_def_scoringoppplays <- scoringopp_plays |>
      filter(defteam == VoA_Variables$team[x]) |>
      drop_na(drive)
    temp_def_scorringopp_TDs <- temp_def_scoringoppplays |>
      filter(touchdown == 1)
    temp_def_scorringopp_FGs <- temp_def_scoringoppplays |>
      filter(field_goal_result == "made")
    temp_def_turnovers <- turnovers |>
      filter(defteam == VoA_Variables$team[x])
    temp_def_TDs <- TDs |>
      filter(defteam == VoA_Variables$team[x])
    temp_def_2pts <- TwoPts |>
      filter(defteam == VoA_Variables$team[x] & two_point_conv_result == "success")
    ### temp PY1 special teams dfs
    ## on kickoffs, defteam does kicking
    ## on punts, posteam does punting
    temp_off_FGs <- FGs |>
      filter(posteam == VoA_Variables$team[x])
    temp_off_goodFGs <- temp_off_FGs |>
      filter(field_goal_result == "made")
    temp_def_FGs <- FGs |>
      filter(defteam == VoA_Variables$team[x] & field_goal_result == "made")
    temp_def_goodFGs <- temp_def_FGs |>
      filter(field_goal_result == "made")
    temp_returned_punts <- punts |>
      filter(defteam == VoA_Variables$team[x])
    temp_returned_kicks <- kickoffs |>
      filter(posteam == VoA_Variables$team[x])
    temp_returned_punt_TDs <- temp_returned_punts |>
      filter(return_touchdown == 1)
    temp_returned_kick_TDs <- temp_returned_kicks |>
      filter(return_touchdown == 1)
    temp_kicked_punts <- punts |>
      filter(posteam == VoA_Variables$team[x])
    temp_kicked_kicks <- kickoffs |>
      filter(defteam == VoA_Variables$team[x])
    temp_kicked_punt_TDs <- temp_kicked_punts |>
      filter(return_touchdown == 1)
    temp_kicked_kick_TDs <- temp_kicked_kicks |>
      filter(return_touchdown == 1)
    temp_off_xps <- XPts |>
      filter(posteam == VoA_Variables$team[x])
    temp_def_xps <- XPts |>
      filter(defteam == VoA_Variables$team[x])
    temp_off_good_xps <- temp_off_xps |>
      filter(extra_point_result == "good")
    temp_def_good_xps <- temp_def_xps |>
      filter(extra_point_result == "good")
    ### used to get net ST epa/play
    temp_off_st_plays <- rbind(temp_off_FGs, temp_off_xps, temp_returned_kicks, temp_returned_punts)
    temp_def_st_plays <- rbind(temp_def_FGs, temp_def_xps, temp_kicked_kicks, temp_kicked_punts)
    
    ### Evaluating Stats
    VoA_Variables$off_ypp[x] = mean(temp_offplays$yards_gained)
    VoA_Variables$off_epa[x] = mean(temp_offplays$epa)
    VoA_Variables$off_success_rt[x] = nrow(temp_offsuccessplays) / nrow(temp_offplays)
    VoA_Variables$off_explosiveness[x] = mean(temp_offsuccessplays$epa)
    VoA_Variables$off_third_conv_rate[x] = nrow(temp_conv_offthirddowns) / nrow(temp_offthirddowns)
    VoA_Variables$off_fourth_conv_rate[x] = nrow(temp_conv_offfourthdowns) / nrow(temp_off_fourthdowns)
    VoA_Variables$off_pass_ypa[x] = mean(temp_off_passplays$yards_gained)
    VoA_Variables$off_pass_ypc[x] = mean(temp_off_comppass$yards_gained)
    VoA_Variables$off_rush_ypa[x] = mean(temp_off_rushplays$yards_gained)
    VoA_Variables$off_pts_per_opp[x] = ((nrow(temp_off_scorringopp_TDs) * 6) + (nrow(temp_off_scorringopp_FGs) * 3)) / length(unique(paste0(temp_off_scoringoppplays$game_id, temp_off_scoringoppplays$drive)))
    VoA_Variables$off_turnovers[x] = nrow(temp_off_turnovers) / length(unique(temp_offplays$week))
    VoA_Variables$off_plays_pg[x] = nrow(temp_offplays) / length(unique(temp_offplays$week))
    VoA_Variables$off_ppg[x] = ((nrow(temp_off_TDs) * 6) + (nrow(temp_off_2pts) * 2)) / length(unique(temp_off_rushplays$week))
    ## PY1 defensive stats now
    VoA_Variables$def_ypp[x] = mean(temp_defplays$yards_gained)
    VoA_Variables$def_epa[x] = mean(temp_defplays$epa)
    VoA_Variables$def_success_rt[x] = nrow(temp_defsuccessplays) / nrow(temp_defplays)
    VoA_Variables$def_explosiveness[x] = mean(temp_defsuccessplays$epa)
    VoA_Variables$def_third_conv_rate[x] = nrow(temp_conv_defthirddowns) / nrow(temp_defthirddowns)
    VoA_Variables$def_fourth_conv_rate[x] = nrow(temp_conv_deffourthdowns) / nrow(temp_def_fourthdowns)
    VoA_Variables$def_pass_ypa[x] = mean(temp_def_passplays$yards_gained)
    VoA_Variables$def_pass_ypc[x] = mean(temp_def_comppass$yards_gained)
    VoA_Variables$def_rush_ypa[x] = mean(temp_def_rushplays$yards_gained)
    VoA_Variables$def_pts_per_opp[x] = ((nrow(temp_def_scorringopp_TDs) * 6) + (nrow(temp_def_scorringopp_FGs) * 3)) / length(unique(paste0(temp_def_scoringoppplays$game_id, temp_def_scoringoppplays$drive)))
    VoA_Variables$def_turnovers[x] = nrow(temp_def_turnovers) / length(unique(temp_defplays))
    VoA_Variables$def_plays_pg[x] = nrow(temp_defplays) / length(unique(temp_defplays$week))
    VoA_Variables$def_ppg[x] = ((nrow(temp_def_TDs) * 6) + (nrow(temp_def_2pts) * 2)) / length(unique(temp_def_rushplays$week))
    ## PY1 Special teams stats now
    VoA_Variables$st_net_epa[x] = mean(temp_off_st_plays$epa) - mean(temp_def_st_plays$epa)
    VoA_Variables$st_punt_return_yds[x] = mean(temp_returned_punts$return_yards)
    VoA_Variables$st_kick_return_yds[x] = mean(temp_returned_kicks$return_yards)
    VoA_Variables$st_kick_return_TDs[x] = nrow(temp_returned_kick_TDs) / length(unique(temp_offplays$week))
    VoA_Variables$st_punt_return_TDs[x] = nrow(temp_returned_punt_TDs) / length(unique(temp_offplays$week))
    VoA_Variables$fg_rate[x] = nrow(temp_off_goodFGs) / nrow(temp_off_FGs)
    VoA_Variables$fg_made_pg[x] = nrow(temp_off_goodFGs) / length(unique(temp_offplays$week))
    VoA_Variables$xp_rate[x] = nrow(temp_off_good_xps) / nrow(temp_off_xps)
    VoA_Variables$xp_made_pg[x] = nrow(temp_off_good_xps) / length(unique(temp_offplays$week))
    VoA_Variables$st_punt_return_yds_allowed[x] = mean(temp_kicked_punts$return_yards)
    VoA_Variables$st_kick_return_yds_allowed[x] = mean(temp_kicked_kicks$return_yards)
    VoA_Variables$st_kick_return_TDs_allowed[x] = nrow(temp_kicked_kick_TDs) / length(unique(temp_offplays$week))
    VoA_Variables$st_punt_return_TDs_allowed[x] = nrow(temp_kicked_punt_TDs) / length(unique(temp_offplays$week))
    VoA_Variables$fg_rate_allowed[x] = nrow(temp_def_goodFGs) / nrow(temp_def_FGs)
    VoA_Variables$fg_made_pg_allowed[x] = nrow(temp_def_goodFGs) / length(unique(temp_offplays$week))
    VoA_Variables$xp_rate_allowed[x] = nrow(temp_def_good_xps) / nrow(temp_def_xps)
    VoA_Variables$xp_made_pg_allowed[x] = nrow(temp_def_good_xps) / length(unique(temp_offplays$week))
    VoA_Variables$net_st_ppg[x] = (((nrow(temp_off_goodFGs) * 3) + (nrow(temp_returned_punt_TDs) * 6) + (nrow(temp_returned_kick_TDs) * 6) + nrow(temp_off_good_xps)) - ((nrow(temp_def_goodFGs) * 3) + (nrow(temp_kicked_punt_TDs) * 6) + (nrow(temp_kicked_kick_TDs) * 6) + nrow(temp_def_good_xps))) / length(unique(temp_offplays$week))
  }
  
  ### binding csv of PY data to VoA Variables, which should only contain current season data at this point
  VoA_Vars_dfs <- list(VoA_Variables, PY_VoAVars)
  VoA_Variables <- VoA_Vars_dfs |>
    reduce(full_join, by = "team") |>
    ### Adding columns of ppg above avg for both offense and defense and adjusting off_ppg and def_ppg
    mutate(off_ppg_aboveavg = off_ppg - mean(off_ppg),
           def_ppg_aboveavg = def_ppg - mean(def_ppg),
           off_ppg_adj = case_when(off_ppg > quantile(off_ppg, 0.8) ~ off_ppg + (off_ppg_aboveavg / 2),
                                   off_ppg > mean(off_ppg) ~ off_ppg + (off_ppg_aboveavg / 5),
                                   TRUE ~ off_ppg),
           def_ppg_adj = case_when(def_ppg > quantile(def_ppg, 0.8) ~ def_ppg + (def_ppg_aboveavg / 2),
                                   def_ppg > mean(def_ppg) ~ def_ppg + (def_ppg_aboveavg / 5),
                                   TRUE ~ def_ppg))
  
  
  ### removing temp objects
  rm(list = ls(pattern = "^temp_"))
}

##### Break point to figure out where Rank columns start #####
if (as.numeric(week) %in% c(0,1,3,6)){
  break
} else{
  print("no new reason to figure out where rank columns start")
}

##### Ranking Variables #####
if (as.numeric(week) <= 5) {
  ##### Weeks 0-5 Variable Ranks #####
  VoA_Variables <- VoA_Variables |>
    mutate(Rank_weighted_off_ypp = dense_rank(desc(weighted_off_ypp)),
           Rank_weighted_off_epa = dense_rank(desc(weighted_off_epa)),
           Rank_weighted_off_success_rt = dense_rank(desc(weighted_off_success_rt)),
           Rank_weighted_off_explosiveness = dense_rank(desc(weighted_off_explosiveness)),
           Rank_weighted_off_third_conv_rate = dense_rank(desc(weighted_off_third_conv_rate)),
           Rank_weighted_off_fourth_conv_rate = dense_rank(desc(weighted_off_fourth_conv_rate)),
           Rank_weighted_off_pass_ypa = dense_rank(desc(weighted_off_pass_ypa)),
           Rank_weighted_off_pass_ypc = dense_rank(desc(weighted_off_pass_ypc)),
           Rank_weighted_off_rush_ypa = dense_rank(desc(weighted_off_rush_ypa)),
           Rank_weighted_off_pts_per_opp = dense_rank(desc(weighted_off_pts_per_opp)),
           Rank_weighted_off_turnovers = dense_rank(weighted_off_turnovers),
           Rank_weighted_off_ppg = dense_rank(desc(weighted_off_ppg)),
           ### ranking defensive variables now
           Rank_weighted_def_ypp = dense_rank(weighted_def_ypp),
           Rank_weighted_def_epa = dense_rank(weighted_def_epa),
           Rank_weighted_def_success_rt = dense_rank(weighted_def_success_rt),
           Rank_weighted_def_explosiveness = dense_rank(weighted_def_explosiveness),
           Rank_weighted_def_third_conv_rate = dense_rank(weighted_def_third_conv_rate),
           Rank_weighted_def_fourth_conv_rate = dense_rank(weighted_def_fourth_conv_rate),
           Rank_weighted_def_pass_ypa = dense_rank(weighted_def_pass_ypa),
           Rank_weighted_def_pass_ypc = dense_rank(weighted_def_pass_ypc),
           Rank_weighted_def_rush_ypa = dense_rank(weighted_def_rush_ypa),
           Rank_weighted_def_pts_per_opp = dense_rank(weighted_def_pts_per_opp),
           Rank_weighted_def_turnovers = dense_rank(desc(weighted_def_turnovers)),
           Rank_weighted_def_ppg = dense_rank(weighted_def_ppg),
           ### ranking ST variables now
           Rank_weighted_net_st_epa = dense_rank(desc(weighted_net_st_epa)),
           Rank_weighted_net_punt_return_yds = dense_rank(desc(weighted_net_punt_return_yds)),
           Rank_weighted_net_punt_return_TDs = dense_rank(desc(weighted_net_punt_return_TDs)),
           Rank_weighted_net_kick_return_yds = dense_rank(desc(weighted_net_kick_return_yds)),
           Rank_weighted_net_kick_return_TDs = dense_rank(desc(weighted_net_kick_return_TDs)),
           Rank_weighted_net_xp_rate = dense_rank(desc(weighted_net_xp_rate)),
           Rank_weighted_net_xp_made_pg = dense_rank(desc(weighted_net_xp_made_pg)),
           Rank_weighted_net_xp_rate = dense_rank(desc(weighted_net_xp_rate)),
           Rank_weighted_net_xp_made_pg = dense_rank(desc(weighted_net_xp_made_pg)),
           Rank_weighted_net_st_ppg = dense_rank(desc(weighted_net_st_ppg)))
} else {
  ### Ranking variables when only current season data is being used
  VoA_Variables <- VoA_Variables |>
    mutate(Rank_off_ypp = dense_rank(desc(off_ypp)),
           Rank_off_epa = dense_rank(desc(off_epa)),
           Rank_off_success_rt = dense_rank(desc(off_success_rt)),
           Rank_off_explosiveness = dense_rank(desc(off_explosiveness)),
           Rank_off_third_conv_rate = dense_rank(desc(off_third_conv_rate)),
           Rank_off_fourth_conv_rate = dense_rank(desc(off_fourth_conv_rate)),
           Rank_off_pass_ypa = dense_rank(desc(off_pass_ypa)),
           Rank_off_pass_ypc = dense_rank(desc(off_pass_ypc)),
           Rank_off_rush_ypa = dense_rank(desc(off_rush_ypa)),
           Rank_off_pts_per_opp = dense_rank(desc(off_pts_per_opp)),
           Rank_off_turnovers = dense_rank(off_turnovers),
           Rank_off_ppg = dense_rank(desc(off_ppg)),
           ### ranking defensive variables now
           Rank_def_ypp = dense_rank(def_ypp),
           Rank_def_epa = dense_rank(def_epa),
           Rank_def_success_rt = dense_rank(def_success_rt),
           Rank_def_explosiveness = dense_rank(def_explosiveness),
           Rank_def_third_conv_rate = dense_rank(def_third_conv_rate),
           Rank_def_fourth_conv_rate = dense_rank(def_fourth_conv_rate),
           Rank_def_pass_ypa = dense_rank(def_pass_ypa),
           Rank_def_pass_ypc = dense_rank(def_pass_ypc),
           Rank_def_rush_ypa = dense_rank(def_rush_ypa),
           Rank_def_pts_per_opp = dense_rank(def_pts_per_opp),
           Rank_def_turnovers = dense_rank(desc(def_turnovers)),
           Rank_def_ppg = dense_rank(def_ppg),
           ### ranking ST variables now
           Rank_net_st_epa = dense_rank(desc(net_st_epa)),
           Rank_net_punt_return_yds = dense_rank(desc(net_punt_return_yds)),
           Rank_net_punt_return_TDs = dense_rank(desc(net_punt_return_TDs)),
           Rank_net_kick_return_yds = dense_rank(desc(net_kick_return_yds)),
           Rank_net_kick_return_TDs = dense_rank(desc(net_kick_return_TDs)),
           Rank_net_xp_rate = dense_rank(desc(net_xp_rate)),
           Rank_net_xp_made_pg = dense_rank(desc(net_xp_made_pg)),
           Rank_net_xp_rate = dense_rank(desc(net_xp_rate)),
           Rank_net_xp_made_pg = dense_rank(desc(net_xp_made_pg)),
           Rank_net_st_ppg = dense_rank(desc(net_st_ppg)))
}


##### Calculating VoA Output #####
### for week 0 (preseason), rank columns start at 168
if (as.numeric(week) == 0){
  VoA_Variables <- VoA_Variables |>
    mutate(VoA_Output = (rowMeans(VoA_Variables[,176:ncol(VoA_Variables)])))
} else if (as.numeric(week) <= 2){
  VoA_Variables <- VoA_Variables |>
    mutate(VoA_Output = (rowMeans(VoA_Variables[,176:ncol(VoA_Variables)])))
} else if (as.numeric(week) <= 5){
  VoA_Variables <- VoA_Variables |>
    mutate(VoA_Output = (rowMeans(VoA_Variables[,132:ncol(VoA_Variables)])))
} else{
  VoA_Variables <- VoA_Variables |>
    mutate(VoA_Output = (rowMeans(VoA_Variables[,168:ncol(VoA_Variables)])))
}

##### Using Stan Model to create unit/team strength ratings #####
if (as.numeric(week) <= 6){
  ##### Week 0-5 Stan Models #####
  ### VoA Offensive Rating Model
  ### making list of data to declare what goes into stan model
  Off_VoA_datalist <- list(N = nrow(VoA_Variables), off_ppg = VoA_Variables$off_ppg_adj, off_epa = VoA_Variables$weighted_off_epa, off_ypp = VoA_Variables$weighted_off_ypp, off_success_rt = VoA_Variables$weighted_off_success_rt, off_explosiveness = VoA_Variables$weighted_off_explosiveness, third_conv_rate = VoA_Variables$weighted_off_third_conv_rate, off_pts_per_opp = VoA_Variables$weighted_off_pts_per_opp, off_plays_pg = VoA_Variables$weighted_off_plays_pg, VoA_Output = (1/VoA_Variables$VoA_Output))
  
  ### fitting stan model
  set.seed(802)
  options(mc.cores = parallel::detectCores())
  Off_VoA_fit <- stan(file=here("Scripts","Stan", "Off_VoA.stan"),data = Off_VoA_datalist, chains = 3, iter = 40000, warmup = 15000, seed = 802)
  Off_VoA_fit
  
  
  ### Extracting Parameters
  Off_VoA_pars <- rstan::extract(Off_VoA_fit, c("b0", "beta_off_epa", "beta_off_ypp", "beta_off_success_rt", "beta_off_explosiveness", "beta_third_conv_rate", "beta_off_pts_per_opp", "beta_off_plays_pg", "beta_VoA_Output", "sigma"))
  
  ### creating matrix to hold ratings
  ### adding in process uncertainty
  Off_VoA_Ratings <- matrix(NA, length(Off_VoA_pars$b0), nrow(VoA_Variables))
  
  ### creating ratings
  set.seed(802)
  for (p in 1:length(Off_VoA_pars$b0)){
    for(t in 1:nrow(VoA_Variables)){
      Off_VoA_Rating <- rnorm(1, mean = Off_VoA_pars$b0[p] + Off_VoA_pars$beta_off_epa[p] * VoA_Variables$weighted_off_epa[t] + Off_VoA_pars$beta_off_ypp[p] * VoA_Variables$weighted_off_ypp[t] + Off_VoA_pars$beta_off_success_rt[p] * VoA_Variables$weighted_off_success_rt[t] + Off_VoA_pars$beta_off_explosiveness[p] * VoA_Variables$weighted_off_explosiveness[t] + Off_VoA_pars$beta_third_conv_rate[p] * VoA_Variables$weighted_off_third_conv_rate[t] + Off_VoA_pars$beta_off_pts_per_opp[p] * VoA_Variables$weighted_off_pts_per_opp[t] + Off_VoA_pars$beta_off_plays_pg[p] * VoA_Variables$weighted_off_plays_pg[t] + Off_VoA_pars$beta_VoA_Output[p] * (1/(VoA_Variables$VoA_Output[t])), sd = Off_VoA_pars$sigma[p])
      Off_VoA_Ratings[p,t] <- Off_VoA_Rating
    }
  }
  
  
  ### generating median and mean and quantile ratings
  MeanPred <- apply(Off_VoA_Ratings,2,mean)
  MedianPred <- apply(Off_VoA_Ratings,2,median)
  Upper <- apply(Off_VoA_Ratings,2,quantile, prob=.95)
  Lower <- apply(Off_VoA_Ratings,2,quantile, prob=.05)
  
  VoA_Variables$OffVoA_MeanRating <- MeanPred
  VoA_Variables$OffVoA_MedRating <- MedianPred
  VoA_Variables$OffVoA_95PctRating <- Upper
  VoA_Variables$OffVoA_05PctRating <- Lower
  
  
  ### VoA Defensive Rating Model
  ### making list of data to declare what goes into stan model
  Def_VoA_datalist <- list(N = nrow(VoA_Variables), def_ppg = VoA_Variables$def_ppg_adj, def_epa = VoA_Variables$weighted_def_epa, def_ypp = VoA_Variables$weighted_def_ypp, def_success_rt = VoA_Variables$weighted_def_success_rt, def_explosiveness = VoA_Variables$weighted_def_explosiveness, def_third_conv_rate = VoA_Variables$weighted_def_third_conv_rate, def_pts_per_opp = VoA_Variables$weighted_def_pts_per_opp, def_plays_pg = VoA_Variables$weighted_def_plays_pg, VoA_Output = VoA_Variables$VoA_Output)
  
  ### fitting stan model
  set.seed(802)
  # options(mc.cores = parallel::detectCores())
  Def_VoA_fit <- stan(file=here("Scripts","Stan", "Def_VoA.stan"),data = Def_VoA_datalist, chains = 3, iter = 40000, warmup = 15000, seed = 802)
  Def_VoA_fit
  
  
  ### Extracting Parameters
  Def_VoA_pars <- rstan::extract(Def_VoA_fit, c("b0", "beta_def_epa", "beta_def_ypp", "beta_def_success_rt", "beta_def_explosiveness", "beta_def_third_conv_rate", "beta_def_pts_per_opp", "beta_def_plays_pg", "beta_VoA_Output", "sigma"))
  
  ### creating matrix to hold ratings
  ### adding in process uncertainty
  Def_VoA_Ratings <- matrix(NA, length(Def_VoA_pars$b0), nrow(VoA_Variables))
  
  ### creating ratings
  set.seed(802)
  for (p in 1:length(Def_VoA_pars$b0)){
    for(t in 1:nrow(VoA_Variables)){
      Def_VoA_Rating <- rnorm(1, mean = Def_VoA_pars$b0[p] + Def_VoA_pars$beta_def_epa[p] * VoA_Variables$weighted_def_epa[t] + Def_VoA_pars$beta_def_ypp[p] * VoA_Variables$weighted_def_ypp[t] + Def_VoA_pars$beta_def_success_rt[p] * VoA_Variables$weighted_def_success_rt[t] + Def_VoA_pars$beta_def_explosiveness[p] * VoA_Variables$weighted_def_explosiveness[t] + Def_VoA_pars$beta_def_third_conv_rate[p] * VoA_Variables$weighted_def_third_conv_rate[t] + Def_VoA_pars$beta_def_pts_per_opp[p] * VoA_Variables$weighted_def_pts_per_opp[t] + Def_VoA_pars$beta_def_plays_pg[p] * VoA_Variables$weighted_def_plays_pg[t] + Def_VoA_pars$beta_VoA_Output[p] * VoA_Variables$VoA_Output[t], sd = Def_VoA_pars$sigma[p])
      Def_VoA_Ratings[p,t] <- Def_VoA_Rating
    }
  }
  
  
  ### generating median and mean and quantile ratings
  MeanPred <- apply(Def_VoA_Ratings,2,mean)
  MedianPred <- apply(Def_VoA_Ratings,2,median)
  Upper <- apply(Def_VoA_Ratings,2,quantile, prob=.95)
  Lower <- apply(Def_VoA_Ratings,2,quantile, prob=.05)
  
  VoA_Variables$DefVoA_MeanRating <- MeanPred
  VoA_Variables$DefVoA_MedRating <- MedianPred
  VoA_Variables$DefVoA_95PctRating <- Upper
  VoA_Variables$DefVoA_05PctRating <- Lower
  
  
  ### Special Teams VoA
  ### making list of data to declare what goes into Stan model
  ST_VoA_datalist <- list(N = nrow(VoA_Variables), net_st_ppg = VoA_Variables$weighted_net_st_ppg, net_kick_return_avg = VoA_Variables$weighted_net_kick_return_yds, net_punt_return_avg = VoA_Variables$weighted_net_punt_return_yds, net_kick_return_TDs = VoA_Variables$weighted_net_kick_return_TDs, net_punt_return_TDs = VoA_Variables$weighted_net_punt_return_TDs, net_fg_rate = VoA_Variables$weighted_net_fg_rate, net_fg_made_pg = VoA_Variables$weighted_net_fg_made_pg, net_xp_rate = VoA_Variables$weighted_net_xp_rate, net_xpts_pg = VoA_Variables$weighted_net_xp_made_pg)
  
  ### fitting special teams Stan model
  set.seed(802)
  options(mc.cores = parallel::detectCores())
  ST_VoA_fit <- stan(file = here("Scripts", "Stan", "ST_VoA.stan"), data = ST_VoA_datalist, chains = 3, iter = 25000, warmup = 10000, seed = 802)
  ST_VoA_fit
  
  ### extracting parameters
  ST_VoA_pars <- rstan::extract(ST_VoA_fit, c("b0", "beta_net_kick_return_avg", "beta_net_punt_return_avg", "beta_net_kick_return_TDs", "beta_net_punt_return_TDs", "beta_net_fg_rate", "beta_net_fg_made_pg", "beta_net_xp_rate", "beta_net_xpts_pg", "sigma"))
  
  ### creating matrix to store special teams VoA_Ratings
  ST_VoA_Ratings <- matrix(NA, nrow = length(ST_VoA_pars$b0), ncol = nrow(VoA_Variables))
  
  ### creating special teams VoA_Ratings
  set.seed(802)
  for (p in 1:length(ST_VoA_pars$b0)){
    for (t in 1:nrow(VoA_Variables)){
      ST_VoA_Rating <- rnorm(1, mean = ST_VoA_pars$b0[p] + ST_VoA_pars$beta_net_kick_return_avg[p] * VoA_Variables$weighted_net_kick_return_yds[t] + ST_VoA_pars$beta_net_punt_return_avg[p] * VoA_Variables$weighted_net_punt_return_yds[t] + ST_VoA_pars$beta_net_kick_return_TDs[p] * VoA_Variables$weighted_net_kick_return_TDs[t] + ST_VoA_pars$beta_net_punt_return_TDs[p] * VoA_Variables$weighted_net_punt_return_TDs[t] + ST_VoA_pars$beta_net_fg_rate[p] * VoA_Variables$weighted_net_fg_rate[t] + ST_VoA_pars$beta_net_fg_made_pg[p] * VoA_Variables$weighted_net_fg_made_pg[t] + ST_VoA_pars$beta_net_xp_rate[p] * VoA_Variables$weighted_net_xp_rate[t] + ST_VoA_pars$beta_net_xpts_pg[p] * VoA_Variables$weighted_net_xp_made_pg[t], sd = ST_VoA_pars$sigma[p])
      ST_VoA_Ratings[p,t] <- ST_VoA_Rating
    }
  }
  
  ### generating median and mean and quantile ratings
  MeanPred <- apply(ST_VoA_Ratings,2,mean)
  MedianPred <- apply(ST_VoA_Ratings,2,median)
  Upper <- apply(ST_VoA_Ratings,2,quantile, prob=.95)
  Lower <- apply(ST_VoA_Ratings,2,quantile, prob=.05)
  
  VoA_Variables$STVoA_MeanRating <- MeanPred
  VoA_Variables$STVoA_MedRating <- MedianPred
  VoA_Variables$STVoA_95PctRating <- Upper
  VoA_Variables$STVoA_05PctRating <- Lower
} else{
  
}


##### Ranking VoA Rating columns #####
VoA_Variables <- VoA_Variables |>
  mutate(VoA_Rating_Ovr = OffVoA_MedRating - DefVoA_MedRating + STVoA_MedRating,
         VoA_Rating_05Pct = OffVoA_05PctRating - DefVoA_05PctRating + STVoA_05PctRating,
         VoA_Rating_95Pct = OffVoA_95PctRating - DefVoA_95PctRating + STVoA_95PctRating,
         VoA_Ranking_Ovr = dense_rank(desc(VoA_Rating_Ovr)),
         OffVoA_Ranking = dense_rank(desc(OffVoA_MedRating)),
         DefVoA_Ranking = dense_rank(DefVoA_MedRating),
         STVoA_Ranking = dense_rank(desc(STVoA_MedRating)))


### creating data frame with just team, VoA ratings, VoA Rankings, and VoA output
FinalTable <- VoA_Variables |>
  select(team, week, VoA_Output, VoA_Rating_Ovr, VoA_Ranking_Ovr, OffVoA_MedRating, OffVoA_Ranking, DefVoA_MedRating, DefVoA_Ranking, STVoA_MedRating, STVoA_Ranking) |>
  arrange(VoA_Ranking_Ovr)

##### Creating Table Arranged by VoA Rating #####
if (as.numeric(week) == 0) {
  ### Full table
  ## adding title and subtitle
  VoA_Table <- FinalTable |>
    gt() |> # use 'gt' to make an awesome table...
    gt_theme_538() |>
    tab_header(
      title = paste(season, preseason_text, nfl_text, VoA_text), # ...with this title
      subtitle = "Supremely Excellent Yet Salaciously Godlike And Infallibly Magnificent NFL Vortex of Accuracy")  |>  # and this subtitle
    ##tab_style(style = cell_fill("bisque"),
    ##        locations = cells_body()) |>  # add fill color to table
    fmt_number( # A column (numeric data)
      columns = c(VoA_Rating_Ovr), # What column variable?
      decimals = 3 # With four decimal places
    ) |>
    fmt_number( # A column (numeric data)
      columns = c(OffVoA_MedRating), # What column variable?
      decimals = 3 # With four decimal places
    ) |>
    fmt_number( # A column (numeric data)
      columns = c(DefVoA_MedRating), # What column variable?
      decimals = 3 # With four decimal places
    ) |>
    fmt_number( # A column (numeric data)
      columns = c(STVoA_MedRating), # What column variable?
      decimals = 3 # With four decimal places
    ) |>
    fmt_number( # Another column (also numeric data)
      columns = c(VoA_Ranking_Ovr), # What column variable? FinalVoATop25$VoA_Ranking
      decimals = 0 # I want this column to have zero decimal places
    ) |> 
    data_color( # Update cell colors, testing different color palettes
      columns = c(VoA_Rating_Ovr), # ...for dose column
      fn = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    data_color( # Update cell colors, testing different color palettes
      columns = c(OffVoA_MedRating), # ...for dose column
      fn = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    data_color( # Update cell colors, testing different color palettes
      columns = c(DefVoA_MedRating), # ...for dose column
      fn = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = TRUE
      )
    ) |>
    data_color( # Update cell colors, testing different color palettes
      columns = c(STVoA_MedRating), # ...for dose column
      fn = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    gt_nfl_wordmarks(columns = "team") |>
    cols_label(VoA_Rating_Ovr = "Overall VoA Rating", VoA_Ranking_Ovr = "VoA Ranking", OffVoA_MedRating = "Off VoA Rating", OffVoA_Ranking = "Off Ranking", DefVoA_MedRating = "Def VoA Rating", DefVoA_Ranking = "Def Ranking", STVoA_MedRating = "ST VoA Rating", STVoA_Ranking = "ST Ranking") |> # Update labels
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
      title = paste(season, week_text, week, nfl_text, VoA_text), # ...with this title
      subtitle = "Supremely Excellent Yet Salaciously Godlike And Infallibly Magnificent Vortex of Accuracy")  |>  # and this subtitle
    ##tab_style(style = cell_fill("bisque"),
    ##        locations = cells_body()) |>  # add fill color to table
    fmt_number( # A column (numeric data)
      columns = c(VoA_Rating_Ovr), # What column variable? FinalVoATop25$VoA_Rating
      decimals = 3 # With four decimal places
    ) |>
    fmt_number( # A column (numeric data)
      columns = c(OffVoA_MedRating), # What column variable? FinalVoATop25$VoA_Rating
      decimals = 3 # With four decimal places
    ) |>
    fmt_number( # A column (numeric data)
      columns = c(DefVoA_MedRating), # What column variable? FinalVoATop25$VoA_Rating
      decimals = 3 # With four decimal places
    ) |>
    fmt_number( # A column (numeric data)
      columns = c(STVoA_MedRating), # What column variable? FinalVoATop25$VoA_Rating
      decimals = 3 # With four decimal places
    ) |>
    fmt_number( # Another column (also numeric data)
      columns = c(VoA_Ranking_Ovr), # What column variable? FinalVoATop25$VoA_Ranking
      decimals = 0 # I want this column to have zero decimal places
    ) |> 
    data_color( # Update cell colors, testing different color palettes
      columns = c(VoA_Rating_Ovr), # ...for dose column
      fn = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    data_color( # Update cell colors, testing different color palettes
      columns = c(OffVoA_MedRating), # ...for dose column
      fn = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    data_color( # Update cell colors, testing different color palettes
      columns = c(DefVoA_MedRating), # ...for dose column
      fn = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = TRUE
      )
    ) |>
    data_color( # Update cell colors, testing different color palettes
      columns = c(STVoA_MedRating), # ...for dose column
      fn = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    gt_nfl_wordmarks(columns = "team") |>
    cols_label(VoA_Rating_Ovr = "Overall VoA Rating", VoA_Ranking_Ovr = "VoA Ranking", OffVoA_MedRating = "Off VoA Rating", OffVoA_Ranking = "Off Ranking", DefVoA_MedRating = "Def VoA Rating", DefVoA_Ranking = "Def Ranking", STVoA_MedRating = "ST VoA Rating", STVoA_Ranking = "ST Ranking") |> # Update labels
    # cols_move_to_end(columns = "VoA_Rating") |>
    cols_hide(c(week, VoA_Output)) |>
    tab_footnote(
      footnote = "Table by @gshelor, Data from nflfastR"
    )
}

VoA_Table
VoA_Table |>
  gtsave(
    table_file_pathway, expand = 5,
    path = output_dir
  )


##### Exporting final dataframe as csv #####
write_csv(VoA_Variables, file_pathway)


##### Setting up the Unintelligible Charts #####
### Tracks VoA Ratings and Rankings by week
### now reading in and merging VoA rating and ranking data up to current week
### changing FinalTable to only be columns needed for Unintelligible Charts
FinalTable <- FinalTable |>
  select(team, week, VoA_Output, VoA_Ranking_Ovr, VoA_Rating_Ovr)
if (as.numeric(week) == 3) {
  Week0_VoA <- read_csv(here("Data", paste0("VoA", season), paste0(season, "Week0_VoA.csv"))) |>
    select(team, week, VoA_Output, VoA_Ranking_Ovr, VoA_Rating_Ovr)
  Week1_VoA <- read_csv(here("Data", paste0("VoA", season), paste0(season, "Week1_VoA.csv"))) |>
    select(team, week, VoA_Output, VoA_Ranking_Ovr, VoA_Rating_Ovr)
  Week2_VoA <- read_csv(here("Data", paste0("VoA", season), paste0(season, "Week2_VoA.csv"))) |>
    select(team, week, VoA_Output, VoA_Ranking_Ovr, VoA_Rating_Ovr)
  Ratings_Rks <- rbind(Week0_VoA, rbind(Week1_VoA, rbind(Week2_VoA, FinalTable)))
  write_csv(Ratings_Rks, paste(data_dir, "/TrackingChartCSVs", "/", season, week_text, "0_3Ratings_Rks.csv", sep = ""))
} else if (as.numeric(week) == 4) {
  Ratings_Rks <- read_csv(here("Data", paste0("VoA", season), "TrackingChartCSVs", paste(season, week_text, "0_3Ratings_Rks.csv", sep = ""))) |>
    select(team, week, VoA_Output, VoA_Ranking_Ovr, VoA_Rating_Ovr)
  Ratings_Rks <- rbind(Ratings_Rks, FinalTable)
  write_csv(Ratings_Rks, paste(data_dir, "/TrackingChartCSVs", "/", season, week_text, "0_4Ratings_Rks.csv", sep = ""))
} else if (as.numeric(week) == 5) {
  Ratings_Rks <- read_csv(here("Data", paste0("VoA", season), "TrackingChartCSVs", paste(season, week_text, "0_4Ratings_Rks.csv", sep = ""))) |>
    select(team, week, VoA_Output, VoA_Ranking_Ovr, VoA_Rating_Ovr)
  Ratings_Rks <- rbind(Ratings_Rks, FinalTable)
  write_csv(Ratings_Rks, paste(data_dir, "/TrackingChartCSVs", "/", season, week_text, "0_5Ratings_Rks.csv", sep = ""))
} else if (as.numeric(week) == 6) {
  Ratings_Rks <- read_csv(here("Data", paste0("VoA", season), "TrackingChartCSVs", paste(season, week_text, "0_5Ratings_Rks.csv", sep = ""))) |>
    select(team, week, VoA_Output, VoA_Ranking_Ovr, VoA_Rating_Ovr)
  Ratings_Rks <- rbind(Ratings_Rks, FinalTable)
  write_csv(Ratings_Rks, paste(data_dir, "/TrackingChartCSVs", "/", season, week_text, "0_6Ratings_Rks.csv", sep = ""))
} else if (as.numeric(week) == 7) {
  Ratings_Rks <- read_csv(here("Data", paste0("VoA", season), "TrackingChartCSVs", paste(season, week_text, "0_6Ratings_Rks.csv", sep = ""))) |>
    select(team, week, VoA_Output, VoA_Ranking_Ovr, VoA_Rating_Ovr)
  Ratings_Rks <- rbind(Ratings_Rks, FinalTable)
  write_csv(Ratings_Rks, paste(data_dir, "/TrackingChartCSVs", "/", season, week_text, "0_7Ratings_Rks.csv", sep = ""))
} else if (as.numeric(week) == 8) {
  Ratings_Rks <- read_csv(here("Data", paste0("VoA", season), "TrackingChartCSVs", paste(season, week_text, "0_7Ratings_Rks.csv", sep = ""))) |>
    select(team, week, VoA_Output, VoA_Ranking_Ovr, VoA_Rating_Ovr)
  Ratings_Rks <- rbind(Ratings_Rks, FinalTable)
  write_csv(Ratings_Rks, paste(data_dir, "/TrackingChartCSVs", "/", season, week_text, "0_8Ratings_Rks.csv", sep = ""))
} else if (as.numeric(week) == 9) {
  Ratings_Rks <- read_csv(here("Data", paste0("VoA", season), "TrackingChartCSVs", paste(season, week_text, "0_8Ratings_Rks.csv", sep = ""))) |>
    select(team, week, VoA_Output, VoA_Ranking_Ovr, VoA_Rating_Ovr)
  Ratings_Rks <- rbind(Ratings_Rks, FinalTable)
  write_csv(Ratings_Rks, paste(data_dir, "/TrackingChartCSVs", "/", season, week_text, "0_9Ratings_Rks.csv", sep = ""))
} else if (as.numeric(week) == 10) {
  Ratings_Rks <- read_csv(here("Data", paste0("VoA", season), "TrackingChartCSVs", paste(season, week_text, "0_9Ratings_Rks.csv", sep = ""))) |>
    select(team, week, VoA_Output, VoA_Ranking_Ovr, VoA_Rating_Ovr)
  Ratings_Rks <- rbind(Ratings_Rks, FinalTable)
  write_csv(Ratings_Rks, paste(data_dir, "/TrackingChartCSVs", "/", season, week_text, "0_10Ratings_Rks.csv", sep = ""))
} else if (as.numeric(week) == 11) {
  Ratings_Rks <- read_csv(here("Data", paste0("VoA", season), "TrackingChartCSVs", paste(season, week_text, "0_10Ratings_Rks.csv", sep = ""))) |>
    select(team, week, VoA_Output, VoA_Ranking_Ovr, VoA_Rating_Ovr)
  Ratings_Rks <- rbind(Ratings_Rks, FinalTable)
  write_csv(Ratings_Rks, paste(data_dir, "/TrackingChartCSVs", "/", season, week_text, "0_11Ratings_Rks.csv", sep = ""))
} else if (as.numeric(week) == 12) {
  Ratings_Rks <- read_csv(here("Data", paste0("VoA", season), "TrackingChartCSVs", paste(season, week_text, "0_11Ratings_Rks.csv", sep = ""))) |>
    select(team, week, VoA_Output, VoA_Ranking_Ovr, VoA_Rating_Ovr)
  Ratings_Rks <- rbind(Ratings_Rks, FinalTable)
  write_csv(Ratings_Rks, paste(data_dir, "/TrackingChartCSVs", "/", season, week_text, "0_12Ratings_Rks.csv", sep = ""))
} else if (as.numeric(week) == 13) {
  Ratings_Rks <- read_csv(here("Data", paste0("VoA", season), "TrackingChartCSVs", paste(season, week_text, "0_12Ratings_Rks.csv", sep = ""))) |>
    select(team, week, VoA_Output, VoA_Ranking_Ovr, VoA_Rating_Ovr)
  Ratings_Rks <- rbind(Ratings_Rks, FinalTable)
  write_csv(Ratings_Rks, paste(data_dir, "/TrackingChartCSVs", "/", season, week_text, "0_13Ratings_Rks.csv", sep = ""))
} else if (as.numeric(week) == 14) {
  Ratings_Rks <- read_csv(here("Data", paste0("VoA", season), "TrackingChartCSVs", paste(season, week_text, "0_13Ratings_Rks.csv", sep = ""))) |>
    select(team, week, VoA_Output, VoA_Ranking_Ovr, VoA_Rating_Ovr)
  Ratings_Rks <- rbind(Ratings_Rks, FinalTable)
  write_csv(Ratings_Rks, paste(data_dir, "/TrackingChartCSVs", "/", season, week_text, "0_14Ratings_Rks.csv", sep = ""))
} else if (as.numeric(week) == 15) {
  Ratings_Rks <- read_csv(here("Data", paste0("VoA", season), "TrackingChartCSVs", paste(season, week_text, "0_14Ratings_Rks.csv", sep = ""))) |>
    select(team, week, VoA_Output, VoA_Ranking_Ovr, VoA_Rating_Ovr)
  Ratings_Rks <- rbind(Ratings_Rks, FinalTable)
  write_csv(Ratings_Rks, paste(data_dir, "/TrackingChartCSVs", "/", season, week_text, "0_15Ratings_Rks.csv", sep = ""))
} else if (as.numeric(week) == 16) {
  Ratings_Rks <- read_csv(here("Data", paste0("VoA", season), "TrackingChartCSVs", paste(season, week_text, "0_15Ratings_Rks.csv", sep = ""))) |>
    select(team, week, VoA_Output, VoA_Ranking_Ovr, VoA_Rating_Ovr)
  Ratings_Rks <- rbind(Ratings_Rks, FinalTable)
  ## no need to write out a new tracking csv since "week 16" is the postseason VoA
  ## write_csv(Ratings_Rks, paste(data_dir, "/TrackingChartCSVs", "/", season, week_text, "0_16Ratings_Rks.csv", sep = ""))
} else if (as.numeric(week) == 17){
  
} else if (as.numeric(week) == 18){
  
} else if (as.numeric(week) == 19){
  
} else if (as.numeric(week) == 20){
  
} else if (as.numeric(week) == 21){
  
} else if (as.numeric(week) == 22){
  
} else {
  print("No charts until Week 3! or maybe there's another week before the super bowl")
}

##### Creating Charts #####
### charting VoA_Rating and VoA_Ranking for each week from week 3 on
if (as.numeric(week) >= 3){
  ### creating rating chart
  VoA_Rating_Chart <- ggplot(Ratings_Rks, aes(x = week, y = VoA_Rating_Ovr, group = team)) +
    theme_bw() +
    geom_line(linewidth = 1.5) +
    geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA Overall Rating") +
    labs(caption = "chart by @gshelor, data from nflfastR") +
    ggtitle("Vortex of Accuracy Overall Ratings by Week") +
    expand_limits(y = c(floor(floor(min(VoA_Variables$VoA_Rating_Ovr)) / 10) * 10, ceiling((ceiling(max(VoA_Variables$VoA_Rating_Ovr)) / 10)) * 10)) +
    scale_y_continuous(breaks = seq((floor((floor(min(VoA_Variables$VoA_Rating_Ovr)) / 10)) * 10), (ceiling((ceiling(max(VoA_Variables$VoA_Rating_Ovr)) / 10)) * 10), by = 5)) +
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25)) +
    geom_nfl_logos(aes(team_abbr = team), width = 0.04) +
    # geom_cfb_logos(aes(team = team, width = 0.035)) +
    theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
  VoA_Rating_Chart
  ggsave(Output_filename, path = output_dir, width = 50, height = 40, units = 'cm')
  
  VoA_Ranking_Chart <- ggplot(Ratings_Rks, aes(x = week, y = VoA_Ranking_Ovr, group = team)) +
    theme_bw() +
    geom_line(linewidth = 1.5) +
    geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA Ranking") +
    labs(caption = "chart by @gshelor, data from nflfastR") +
    ggtitle("NFL Vortex of Accuracy Rankings by Week") +
    expand_limits(y = c(0,32)) +
    scale_y_continuous(breaks = c(0,4,8,12,16,20,24,28,32)) +
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25)) +
    geom_nfl_logos(aes(team_abbr = team), width = 0.04) +
    theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
  VoA_Ranking_Chart
  ggsave(Ranking_filename, path = output_dir, width = 50, height = 40, units = 'cm')
} else {
  print("no charts until week 3!")
}

##### creating histogram of VoA Ratings #####
Rating_histogram <- ggplot(VoA_Variables, aes(VoA_Rating_Ovr)) +
  geom_histogram(binwidth = 2,
                 col = "black",
                 fill = "orange") +
  scale_x_continuous(breaks = seq(-40,40,5)) +
  scale_y_continuous(breaks = seq(0,32,4)) +
  ggtitle(hist_title) +
  xlab("VoA Rating") +
  ylab("Frequency") +
  labs(caption = "chart by @gshelor, data from nflfastR") +
  theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
Rating_histogram
ggsave(hist_filename, path = output_dir, width = 50, height = 40, units = 'cm')


### Creating Scatterplot of VoA_Output vs VoA_Rating
VoA_Output_Rating_plot <- ggplot(VoA_Variables, aes(x = VoA_Output, y = VoA_Rating_Ovr)) +
  geom_point(size = 5) +
  geom_nfl_logos(aes(team_abbr = team), width = 0.05) +
  geom_smooth() +
  scale_x_continuous(breaks = seq(0,32,2)) +
  scale_y_continuous(breaks = seq(-40,40,5)) +
  ggtitle(Output_Rating_Plot_title) +
  xlab("VoA Output") +
  ylab("VoA Overall Rating") +
  labs(caption = "chart by @gshelor, data from nflfastR") +
  theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
VoA_Output_Rating_plot
ggsave(Output_Rating_Plot_filename, path = output_dir, width = 50, height = 40, units = 'cm')



EndTime <- Sys.time()
EndTime - StartTime
##### End of Script #####