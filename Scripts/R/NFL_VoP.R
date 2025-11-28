##### NFL Vortex of Projection Version 1.1 #####
start_time <- Sys.time()
##### loading packages #####
### loading packages
library(pacman)
p_load(tidyverse, gt, nflverse, here, gtExtras, RColorBrewer, webshot2, cmdstanr, betareg)
### Inputting season
season <- readline(prompt = "What season is it? ")
### Inputting upcoming week number
upcoming <- readline(prompt = "What week is upcoming? ")

### Text Strings for gt table of game projections at the end
nfl_text <- "NFL"
week_text <- "Week"
gameprojections_png <- "GameProjections.png"
gameprojections_filename <- paste(season, week_text, upcoming, gameprojections_png, sep = "")

### setting gt title based on whether it's a playoff week or not
if (as.numeric(upcoming) == 19){
  gt_title <- paste(season, nfl_text, "Vortex of Accuracy Wildcard Round Game Projections")
} else if (as.numeric(upcoming) == 20){
  gt_title <- paste(season, nfl_text, "Vortex of Accuracy Divisional Round Game Projections")
} else if (as.numeric(upcoming) == 21){
  gt_title <- paste(season, nfl_text, "Vortex of Accuracy Conference Championship Game Projections")
} else if (as.numeric(upcoming) == 22){
  gt_title <- paste(season, nfl_text, "Vortex of Accuracy Super Bowl Projection")
} else{
  gt_title <- paste(season, nfl_text, week_text, upcoming, "Vortex of Accuracy Game Projections")
}

##### reading in most recent VoA overall ratings #####
PrevWeek_VoA <- read_csv(here("Data", paste0("VoA", season), paste0(season, week_text, as.character(as.numeric(upcoming) - 1), "_VoA.csv"))) |>
  select(team, VoA_Rating_Ovr)


### reading in upcoming games to create df of games and VoA projected margins
if (as.numeric(upcoming) == 1){
  FullSeason_Games <- load_schedules(as.numeric(season)) |>
    select(game_id, season, game_type, week, gameday, weekday, gametime, away_team, away_score, home_team, home_score, location, result, total, overtime, spread_line, total_line, div_game, temp, wind, stadium) |>
    mutate(home_VoA_Rating = 0,
           away_VoA_Rating = 0)
} else {
  upcoming_games_df <- load_schedules(as.numeric(season)) |>
    select(game_id, season, game_type, week, gameday, weekday, gametime, away_team, away_score, home_team, home_score, location, result, total, overtime, spread_line, total_line, div_game, temp, wind, stadium) |>
    filter(week == as.numeric(upcoming)) |>
    mutate(home_VoA_Rating = 0,
           away_VoA_Rating = 0)
}

##### matching up VoA ratings with appropriate teams #####
if (as.numeric(upcoming) == 1){
  ### matching up VoA ratings with appropriate teams
  set.seed(802)
  for (game in 1:nrow(FullSeason_Games)){
    FullSeason_Games$home_VoA_Rating[game] = PrevWeek_VoA$VoA_Rating_Ovr[PrevWeek_VoA$team == FullSeason_Games$home_team[game]]
  }
  ### repeating to fill in away ratings
  for (game in 1:nrow(FullSeason_Games)){
    FullSeason_Games$away_VoA_Rating[game] = PrevWeek_VoA$VoA_Rating_Ovr[PrevWeek_VoA$team == FullSeason_Games$away_team[game]]
  }
} else {
  ### matching up VoA ratings with appropriate teams
  set.seed(802)
  for (game in 1:nrow(upcoming_games_df)){
    upcoming_games_df$home_VoA_Rating[game] = PrevWeek_VoA$VoA_Rating_Ovr[PrevWeek_VoA$team == upcoming_games_df$home_team[game]]
  }
  ### repeating to fill in away ratings
  for (game in 1:nrow(upcoming_games_df)){
    upcoming_games_df$away_VoA_Rating[game] = PrevWeek_VoA$VoA_Rating_Ovr[PrevWeek_VoA$team == upcoming_games_df$away_team[game]]
  }
}


##### Evaluating Projected Winner and Win Margins #####
if (as.numeric(upcoming) == 1){
  ### Projecting win 
  FullSeason_Games <- FullSeason_Games |>
    mutate(Proj_Margin = case_when(location == "Home" ~  (home_VoA_Rating + 2.5) - away_VoA_Rating,
                                 TRUE ~ home_VoA_Rating - away_VoA_Rating),
           proj_margin_abs = abs(Proj_Margin),
           proj_winner = case_when(Proj_Margin > 0 ~ home_team,
                                   Proj_Margin < 0 ~ away_team,
                                   TRUE ~ "TIE"))
} else{
  ### Creating Vortex of Accuracy projected win margin and winner column for upcoming week's games
  upcoming_games_df <- upcoming_games_df |>
    mutate(Proj_Margin = case_when(location == "Home" ~  (home_VoA_Rating + 2.5) - away_VoA_Rating,
                                   TRUE ~ home_VoA_Rating - away_VoA_Rating),
           proj_margin_abs = abs(Proj_Margin),
           proj_winner = case_when(Proj_Margin > 0 ~ home_team,
                                   Proj_Margin < 0 ~ away_team,
                                   TRUE ~ "TIE"))
}

### simple function to take VoA Ratings and field neutrality as inputs
margin_projection <- function(away, home, neutral) {
  margin_proj = PrevWeek_VoA$VoA_Rating_Ovr[PrevWeek_VoA$team == home] -  PrevWeek_VoA$VoA_Rating_Ovr[PrevWeek_VoA$team == away]
  if (neutral == FALSE) {
    margin_proj = margin_proj + 2.5
  }
  return(margin_proj)
}

##### Evaluating VoP's projected win probability #####
### coefficients for calculating win probability aren't just random long decimal numbers, I fit a model using lm() to Bill Connelly's projected win probs and just took them out and wrote them into this script instead of just fitting that model over and over every week
### it's a lazy way of "calculating" win prob but it works well enough for my purposes
### now I fit a beta regression model using betareg instead of that lm thing above
# SP_WPdata <- read_csv(here("Data", "SPPlusData", "All_SP.csv")) |>
#   separate(col = "Game", into = c("away_team", "home_team"), sep = " at ") |>
#   drop_na(away_team, home_team) |>
#   filter(home_team == Proj_winner | away_team == Proj_winner) |>
#   mutate(home_WP_pct = case_when(Proj_winner == home_team ~ WP_pct,
#                                  TRUE ~ 1 - WP_pct),
#          proj_margin = case_when(Proj_winner == home_team ~ Proj_margin,
#                                       TRUE ~ -1 * Proj_margin))


### I wanted to fit a stan model but that didn't work so I'm trying a beta regression model with betareg to do something different, see how it goes
# set.seed(802)
# options(mc.cores = parallel::detectCores() / 2)
# WP_betareg <- betareg(home_WP_pct ~ proj_margin, data = SP_WPdata)
### reading in saved model copied from CFB_VoA since it's the same data used to fit it
WP_betareg <- read_rds(here("Data", "SPPlusData", "WP_betareg.rds"))
summary(WP_betareg)

# SP_WPdata$predicted_home_winprob <- predict(WP_betareg)

# VoP_WP_datalist <- list(N = nrow(SP_WPdata), win_prob = SP_WPdata$home_WP_pct, win_margin = SP_WPdata$Proj_margin)

### fitting stan model
# init_func <- function(){
#   list(b0 = 50, b1 = 1, sigma = 5)
# }
# set.seed(802)
# options(mc.cores = parallel::detectCores())
# VoP_WP_fit <- stan(file=here("Scripts","Stan", "NFLVoP_WinProb.stan"), data = VoP_WP_datalist, chains = 3, iter = 15000, warmup = 5000, seed = 802)
# VoP_WP_fit

### Extracting Parameters
# VoP_WP_pars <- rstan::extract(VoP_WP_fit, c("b0", "b1", "sigma"))

### creating matrix to hold ratings
### adding in process uncertainty
# VoP_WinProbs <- matrix(NA, length(VoP_WP_pars$b0), nrow(FullSeason_Games))

### creating ratings
# set.seed(802)
# for (p in 1:length(VoP_WP_pars$b0)){
#   for(t in 1:nrow(FullSeason_Games)){
#     VoP_winprob <- rnorm(1, mean = VoP_WP_pars$b0[p] + VoP_WP_pars$b1[p] * abs(FullSeason_Games$spread_line[t]), sd = VoP_WP_pars$sigma[p])
#     VoP_WinProbs[p,t] <- VoP_winprob
#   }
# }


### generating median Win Probability
# MedianPred <- apply(VoP_WinProbs,2,median)

# FullSeason_Games$MedianWP <- MedianPred




if (as.numeric(upcoming) == 1){
  ### adding projected winner, projected win margin, and win probability
  ### home field advantage of 2 points when neutral_site == FALSE
  FullSeason_Games <- FullSeason_Games |>
    mutate(home_win_prob = predict(WP_betareg, newdata = FullSeason_Games),
           win_prob = case_when(proj_winner == home_team ~ home_win_prob,
                                TRUE ~ 1 - home_win_prob))
    # mutate(win_prob = 50.37036489 + (2.38892221 * Proj_Margin) + (-0.02809534 * (Proj_Margin^2)))
  ### making sure no game has win probability for projected winner lower than 50 or higher than 100
  # FullSeason_Games <- FullSeason_Games |>
  #   mutate(Win_Prob = case_when((Initial_Win_Prob < 50) ~ 50.01,
  #                               Initial_Win_Prob > 100 ~ 100,
  #                               Proj_Margin > 45 ~ 100,
  #                               TRUE ~ Initial_Win_Prob)) |>
  #   select(-one_of("Initial_Win_Prob"))
  upcoming_games_df <- FullSeason_Games |>
    filter(week == as.numeric(upcoming))
} else{
  ### making gt table of upcoming games df to display games with close spreads
  upcoming_games_df <- upcoming_games_df |>
    mutate(home_win_prob = predict(WP_betareg, newdata = upcoming_games_df),
           win_prob = case_when(proj_winner == home_team ~ home_win_prob,
                                TRUE ~ 1 - home_win_prob)) #|>
    # select(game_id, season, week, neutral_site, home_team, home_VoA_Rating, away_team, away_VoA_Rating, Proj_Winner, Proj_Margin, Initial_Win_Prob) ## |>
  # arrange(desc(Proj_Margin))
  ### making sure no game has win probability for projected winner lower than 50 or higher than 100
  # upcoming_games_df <- upcoming_games_df |>
  #   mutate(Win_Prob = case_when((Initial_Win_Prob < 50) ~ 50.01,
  #                               Initial_Win_Prob > 100 ~ 100,
  #                               Proj_Margin > 45 ~ 100,
  #                               TRUE ~ Initial_Win_Prob)) |>
  #   select(game_id, season, week, neutral_site, home_team, home_VoA_Rating, away_team, away_VoA_Rating, Proj_Winner, Proj_Margin, Win_Prob)
}


##### WEEK 0 (week 1 upcoming) ONLY Calculating projected number of wins #####
if (as.numeric(upcoming) == 1){
  ### adding column to store projected number of wins
  ## storing dummy value in it for now
  NFL_VoA <- PrevWeek_VoA |>
    mutate(proj_wins = -999)
  ### calculating median projected wins, storing it in NFL_VoA$Proj_Wins for appropriate teams
  for (nfl_team in 1:nrow(NFL_VoA)){
    temp_games_df <- FullSeason_Games |>
      filter(home_team == NFL_VoA$team[nfl_team] | away_team == NFL_VoA$team[nfl_team])
    temp_wins_df <- temp_games_df |>
      filter(proj_winner == NFL_VoA$team[nfl_team])
    temp_losses_df <- temp_games_df |>
      filter(proj_winner != NFL_VoA$team[nfl_team])
    temp_proj_wins <- sum(temp_wins_df$win_prob) + (nrow(temp_losses_df) - sum(temp_losses_df$win_prob))
    NFL_VoA$proj_wins[nfl_team] = temp_proj_wins
  }
  
  ### making tables with gt for each conference showing each team's projected wins
  ### each conference (including independents) gets separate tables
  NFL_ProjWins <- NFL_VoA |>
    arrange(desc(proj_wins))
  
  ### Creating gt table
  ## adding title and subtitle
  NFL_ProjWins_gt <- NFL_ProjWins |>
    gt() |> # use 'gt' to make an awesome table...
    gt_theme_538() |>
    tab_header(
      title = paste(season, "NFL Median Win Total Projections"), # ...with this title
      subtitle = "The Unquestionably Puzzling Yet Impeccibly Perceptive Vortex of Projection")  |>  # and this subtitle
    fmt_number( # Another numeric column
      columns = c(VoA_Rating_Ovr),
      decimals = 3
    ) |>
    fmt_number( # Another numeric column
      columns = c(proj_wins),
      decimals = 1
    ) |>  
    data_color( # Update cell colors, testing different color palettes
      columns = c(VoA_Rating_Ovr), # ...for dose column
      fn = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(11, "RdBu"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    data_color( # Update cell colors, testing different color palettes
      columns = c(proj_wins), # ...for dose column
      fn = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    gt_nfl_wordmarks(columns = "team") |>
    cols_label(team = "Team", VoA_Rating_Ovr = "VoA Overall Rating", proj_wins = "Median Projected Wins") |> # Update labels
    # cols_move_to_end(columns = "Win_Prob") |>
    # cols_hide(c(conference)) |>
    tab_footnote(
      footnote = "table by @gshelor, Data from nflfastR"
    ) #|>
    # tab_options(table.width = pct(60))
  NFL_ProjWins_gt
  NFL_ProjWins_gt |>
    gtsave(
      "NFLWinProjections.png", expand = 5,
      path = here("Outputs", "RVoA", paste0("VoA", season), "VoP")
    )
} else{
  print("Season ongoing")
}



##### making table of games and projected winners and margins #####
## Creating gt table
# adding title and subtitle
### sorting column order
upcoming_games_df <- upcoming_games_df |>
  select(game_id, season, game_type, week, gameday, weekday, gametime, away_team, away_VoA_Rating, home_team, home_VoA_Rating, location, result, total, overtime, spread_line, total_line, div_game, temp, wind, stadium, Proj_Margin, proj_margin_abs, proj_winner, home_win_prob, win_prob)

upcoming_games_gt <- upcoming_games_df |>
  gt() |> # use 'gt' to make an awesome table...
  gt_theme_538() |>
  tab_header(
    title = gt_title, # ...with this title
    subtitle = "The Unquestionably Puzzling Yet Impeccibly Perceptive Vortex of Projection")  |>  # and this subtitle
  fmt_number( # A column (numeric data)
    columns = c(proj_margin_abs), # What column variable? FinalVoATop25$VoA_Rating
    decimals = 3 # With 3 decimal places
  ) |>
  fmt_number( # Another column (also numeric data)
    columns = c(home_VoA_Rating), # What column variable? FinalVoATop25$VoA_Ranking
    decimals = 3 # I want this column to have 2 decimal places
  ) |>
  fmt_number( # Another numeric column
    columns = c(away_VoA_Rating),
    decimals = 3
  ) |>
  fmt_number( # Another numeric column
    columns = c(away_VoA_Rating),
    decimals = 3
  ) |>
  fmt_number( # Another numeric column
    columns = c(win_prob),
    decimals = 3
  ) |>
  data_color( # Update cell colors, testing different color palettes
    columns = c(proj_margin_abs), # ...for dose column
    fn = scales::col_numeric( # <- bc it's numeric
      palette = brewer.pal(11, "RdBu"), # A color scheme (gradient)
      domain = c(), # Column scale endpoints
      reverse = FALSE
    )
  ) |>
  data_color( # Update cell colors, testing different color palettes
    columns = c(win_prob), # ...for dose column
    fn = scales::col_numeric( # <- bc it's numeric
      palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
      domain = c(), # Column scale endpoints
      reverse = FALSE
    )
  ) |>
  nflplotR::gt_nfl_wordmarks(columns = c("away_team", "home_team", "proj_winner")) |>
  cols_label(home_team = "Home", away_team = "Away", home_VoA_Rating = "Home VoA Rating", away_VoA_Rating = "Away VoA Rating", proj_winner = "Projected Winner", proj_margin_abs = "Projected Margin", win_prob = "Win Probability") |> # Update labels
  cols_move_to_end(columns = "win_prob") |>
  cols_hide(c(game_id, season, week, game_type, gameday, weekday, gametime, location, result, total, overtime, spread_line, total_line, div_game, temp, stadium, wind, home_win_prob, Proj_Margin)) |>
  tab_footnote(
    footnote = "Table by @gshelor, Data from nflfastR"
  )
upcoming_games_gt
upcoming_games_gt |>
  gtsave(
    gameprojections_filename, expand = 5,
    path = here("Outputs", "RVoA", paste0("VoA", season), "VoP")
  )

##### making game projection table arranged by projected win margin #####
upcoming_games_projmargin <- upcoming_games_df |>
  arrange(proj_margin_abs)

upcoming_games_projmargin_gt <- upcoming_games_projmargin |>
  gt() |> # use 'gt' to make an awesome table...
  gt_theme_538() |>
  tab_header(
    title = gt_title, # ...with this title
    subtitle = "The Unquestionably Puzzling Yet Impeccibly Perceptive Vortex of Projection")  |>  # and this subtitle
  fmt_number( # A column (numeric data)
    columns = c(proj_margin_abs), # What column variable? FinalVoATop25$VoA_Rating
    decimals = 3 # With 3 decimal places
  ) |>
  fmt_number( # Another column (also numeric data)
    columns = c(home_VoA_Rating), # What column variable? FinalVoATop25$VoA_Ranking
    decimals = 3 # I want this column to have 2 decimal places
  ) |>
  fmt_number( # Another numeric column
    columns = c(away_VoA_Rating),
    decimals = 3
  ) |>
  fmt_number( # Another numeric column
    columns = c(away_VoA_Rating),
    decimals = 3
  ) |>
  fmt_number( # Another numeric column
    columns = c(win_prob),
    decimals = 3
  ) |>
  data_color( # Update cell colors, testing different color palettes
    columns = c(proj_margin_abs), # ...for dose column
    fn = scales::col_numeric( # <- bc it's numeric
      palette = brewer.pal(11, "RdBu"), # A color scheme (gradient)
      domain = c(), # Column scale endpoints
      reverse = FALSE
    )
  ) |>
  data_color( # Update cell colors, testing different color palettes
    columns = c(win_prob), # ...for dose column
    fn = scales::col_numeric( # <- bc it's numeric
      palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
      domain = c(), # Column scale endpoints
      reverse = FALSE
    )
  ) |>
  nflplotR::gt_nfl_wordmarks(columns = c("away_team", "home_team", "proj_winner")) |>
  cols_label(home_team = "Home", away_team = "Away", home_VoA_Rating = "Home VoA Rating", away_VoA_Rating = "Away VoA Rating", proj_winner = "Projected Winner", proj_margin_abs = "Projected Margin", win_prob = "Win Probability") |> # Update labels
  cols_move_to_end(columns = "win_prob") |>
  cols_hide(c(game_id, season, week, game_type, gameday, weekday, gametime, location, result, total, overtime, spread_line, total_line, div_game, temp, stadium, wind, home_win_prob, Proj_Margin)) |>
  tab_footnote(
    footnote = "Table by @gshelor, Data from nflfastR"
  )
upcoming_games_projmargin_gt

### writing csv of projections
write_csv(upcoming_games_df, here("Data", paste0("VoA", season), "VoP", paste0(season, nfl_text, week_text, upcoming, "VoP.csv")))

end_time <- Sys.time()
end_time - start_time
##### End of Script #####