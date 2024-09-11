##### NFL Vortex of Accuracy Version 0.0 #####

##### loading packages #####
library(pacman)
p_load(tidyverse, nflfastR, nflverse, here, gtExtras)

##### reading in data #####
PY_PBP <- nflfastR::load_pbp(2021:2023) |>
  filter(play_type_nfl != "GAME_START" & play_type_nfl != "TIMEOUT" & play_type_nfl != "END_QUARTER" & play_type_nfl != "END_GAME")
PBP_PY1 <- PY_PBP |>
  filter(season == 2023)
PBP_PY2 <- PY_PBP |>
  filter(season == 2022)
PBP_PY3 <- PY_PBP |>
  filter(season == 2021)


#####
unique(PY_PBP$home_team)
max(PBP_PY3$week)
unique(PY_PBP$play_type)
unique(PY_PBP$play_type_nfl)
unique(PY_PBP$posteam_type)
