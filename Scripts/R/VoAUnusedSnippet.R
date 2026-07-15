##### storing unused VoA code here in case I decide I want to use it again #####
### used to calculate error to try to better calibrate the VoA ratings
## but I don't think it really worked or even made much of an impact, so I'm just taking it out

##### Calculating Mean Error of Offensive and Defensive Ratings in Completed games based on previous week's VoA #####
if (as.numeric(nfl_week) == 0) {
  print("no error calculation yet")
} else if (as.numeric(nfl_week) <= 2) {
  ##### Week 1 - 2 Error Calculations #####
  VoAVariables <- VoAVariables |>
    mutate(off_error = -999, def_error = -999)

  PrevWeek_VoA <- read_csv(here(
    "Data",
    paste0("VoA", season),
    paste0(season, week_text, as.numeric(nfl_week) - 1, "_", VoAString)
  ))
  CompletedGames <- CompletedGames |>
    mutate(
      home_off_VoA_rating = -999,
      home_def_VoA_rating = -999,
      away_off_VoA_rating = -999,
      away_def_VoA_rating = -999
    )
  for (i in 1:nrow(CompletedGames)) {
    CompletedGames$home_off_VoA_rating[i] <- PrevWeek_VoA$OffVoA_MedRating[
      PrevWeek_VoA$team == CompletedGames$home_team[i]
    ]
    CompletedGames$home_def_VoA_rating[i] <- PrevWeek_VoA$DefVoA_MedRating[
      PrevWeek_VoA$team == CompletedGames$home_team[i]
    ]
    CompletedGames$away_off_VoA_rating[i] <- PrevWeek_VoA$OffVoA_MedRating[
      PrevWeek_VoA$team == CompletedGames$away_team[i]
    ]
    CompletedGames$away_def_VoA_rating[i] <- PrevWeek_VoA$DefVoA_MedRating[
      PrevWeek_VoA$team == CompletedGames$away_team[i]
    ]
  }

  ### Calculating error for offense and defense based on average performance during season
  for (i in 1:nrow(VoAVariables)) {
    temp_games <- CompletedGames |>
      filter(
        home_team == VoAVariables$team[i] | away_team == VoAVariables$team[i]
      ) |>
      mutate(
        team = VoAVariables$team[i],
        off_error = case_when(
          home_team == team ~ home_score -
            ((home_off_VoA_rating + away_def_VoA_rating) / 2),
          TRUE ~ away_score - ((away_off_VoA_rating + home_def_VoA_rating) / 2)
        ),
        def_error = case_when(
          home_team == team ~ away_score -
            ((home_def_VoA_rating + away_off_VoA_rating) / 2),
          TRUE ~ home_score - ((away_def_VoA_rating + home_off_VoA_rating) / 2)
        )
      )

    VoAVariables$off_error[i] <- mean(temp_games$off_error)
    VoAVariables$def_error[i] <- mean(temp_games$def_error)
  }

  ### adjusting adjusted off and def ppg to account for error
  set.seed(802)
  for (i in 1:nrow(VoAVariables)) {
    temp_off_ppg <- VoAVariables$weighted_off_ppg[i]
    VoAVariables$weighted_off_ppg[i] <- temp_off_ppg +
      rnorm(
        1,
        mean = VoAVariables$off_error[i] / 10,
        sd = sd(VoAVariables$off_error)
      )
    temp_def_ppg <- VoAVariables$weighted_def_ppg[i]
    VoAVariables$weighted_def_ppg[i] <- temp_def_ppg +
      rnorm(
        1,
        mean = VoAVariables$def_error[i] / 10,
        sd = sd(VoAVariables$def_error)
      )

    ### making sure all values are > 0
    if (VoAVariables$weighted_off_ppg[i] <= 0) {
      VoAVariables$weighted_off_ppg[i] <- abs(VoAVariables$weighted_off_ppg[
        i
      ]) +
        abs(rnorm(1, 5, 1))
    }
    if (VoAVariables$weighted_def_ppg[i] <= 0) {
      VoAVariables$weighted_def_ppg[i] <- abs(VoAVariables$weighted_def_ppg[
        i
      ]) +
        abs(rnorm(1, 5, 1))
    }
  }
} else if (as.numeric(nfl_week) <= 5) {
  ##### Week 3 - 5 Error Calculations #####
  VoAVariables <- VoAVariables |>
    mutate(off_error = -999, def_error = -999)

  PrevWeek_VoA <- read_csv(here(
    "Data",
    paste0("VoA", season),
    paste0(season, week_text, as.numeric(nfl_week) - 1, "_", VoAString)
  ))
  CompletedGames <- CompletedGames |>
    mutate(
      home_off_VoA_rating = -999,
      home_def_VoA_rating = -999,
      away_off_VoA_rating = -999,
      away_def_VoA_rating = -999
    )
  for (i in 1:nrow(CompletedGames)) {
    CompletedGames$home_off_VoA_rating[i] <- PrevWeek_VoA$OffVoA_MedRating[
      PrevWeek_VoA$team == CompletedGames$home_team[i]
    ]
    CompletedGames$home_def_VoA_rating[i] <- PrevWeek_VoA$DefVoA_MedRating[
      PrevWeek_VoA$team == CompletedGames$home_team[i]
    ]
    CompletedGames$away_off_VoA_rating[i] <- PrevWeek_VoA$OffVoA_MedRating[
      PrevWeek_VoA$team == CompletedGames$away_team[i]
    ]
    CompletedGames$away_def_VoA_rating[i] <- PrevWeek_VoA$DefVoA_MedRating[
      PrevWeek_VoA$team == CompletedGames$away_team[i]
    ]
  }

  for (i in 1:nrow(VoAVariables)) {
    temp_games <- CompletedGames |>
      filter(
        home_team == VoAVariables$team[i] | away_team == VoAVariables$team[i]
      ) |>
      mutate(
        team = VoAVariables$team[i],
        off_error = case_when(
          home_team == team ~ home_score -
            ((home_off_VoA_rating + away_def_VoA_rating) / 2),
          TRUE ~ away_score - ((away_off_VoA_rating + home_def_VoA_rating) / 2)
        ),
        def_error = case_when(
          home_team == team ~ away_score -
            ((home_def_VoA_rating + away_off_VoA_rating) / 2),
          TRUE ~ home_score - ((away_def_VoA_rating + home_off_VoA_rating) / 2)
        )
      )

    VoAVariables$off_error[i] <- mean(temp_games$off_error)
    VoAVariables$def_error[i] <- mean(temp_games$def_error)
  }

  ### adjusting adjusted off and def ppg to account for error
  set.seed(802)
  for (i in 1:nrow(VoAVariables)) {
    temp_off_ppg <- VoAVariables$weighted_off_ppg[i]
    VoAVariables$weighted_off_ppg[i] <- temp_off_ppg +
      rnorm(
        1,
        mean = VoAVariables$off_error[i] / 5,
        sd = sd(VoAVariables$off_error)
      )
    temp_def_ppg <- VoAVariables$weighted_def_ppg[i]
    VoAVariables$weighted_def_ppg[i] <- temp_def_ppg +
      rnorm(
        1,
        mean = VoAVariables$def_error[i] / 5,
        sd = sd(VoAVariables$def_error)
      )

    ### making sure all values are > 0
    set.seed(802)
    if (VoAVariables$weighted_off_ppg[i] <= 0) {
      VoAVariables$weighted_off_ppg[i] <- abs(VoAVariables$weighted_off_ppg[
        i
      ]) +
        abs(rnorm(1, 5, 1))
    }
    if (VoAVariables$weighted_def_ppg[i] <= 0) {
      VoAVariables$weighted_def_ppg[i] <- abs(VoAVariables$weighted_def_ppg[
        i
      ]) +
        abs(rnorm(1, 5, 1))
    }
  }
} else if (as.numeric(nfl_week) <= 10) {
  ##### Week 6 - 10 Error Calculations #####
  VoAVariables <- VoAVariables |>
    mutate(off_error = -999, def_error = -999)

  PrevWeek_VoA <- read_csv(here(
    "Data",
    paste0("VoA", season),
    paste0(season, week_text, as.numeric(nfl_week) - 1, "_", VoAString)
  ))
  CompletedGames <- CompletedGames |>
    mutate(
      home_off_VoA_rating = -999,
      home_def_VoA_rating = -999,
      away_off_VoA_rating = -999,
      away_def_VoA_rating = -999
    )
  for (i in 1:nrow(CompletedGames)) {
    CompletedGames$home_off_VoA_rating[i] <- PrevWeek_VoA$OffVoA_MedRating[
      PrevWeek_VoA$team == CompletedGames$home_team[i]
    ]
    CompletedGames$home_def_VoA_rating[i] <- PrevWeek_VoA$DefVoA_MedRating[
      PrevWeek_VoA$team == CompletedGames$home_team[i]
    ]
    CompletedGames$away_off_VoA_rating[i] <- PrevWeek_VoA$OffVoA_MedRating[
      PrevWeek_VoA$team == CompletedGames$away_team[i]
    ]
    CompletedGames$away_def_VoA_rating[i] <- PrevWeek_VoA$DefVoA_MedRating[
      PrevWeek_VoA$team == CompletedGames$away_team[i]
    ]
  }

  for (i in 1:nrow(VoAVariables)) {
    temp_games <- CompletedGames |>
      filter(
        home_team == VoAVariables$team[i] | away_team == VoAVariables$team[i]
      ) |>
      mutate(
        team = VoAVariables$team[i],
        off_error = case_when(
          home_team == team ~ home_score -
            ((home_off_VoA_rating + away_def_VoA_rating) / 2),
          TRUE ~ away_score - ((away_off_VoA_rating + home_def_VoA_rating) / 2)
        ),
        def_error = case_when(
          home_team == team ~ away_score -
            ((home_def_VoA_rating + away_off_VoA_rating) / 2),
          TRUE ~ home_score - ((away_def_VoA_rating + home_off_VoA_rating) / 2)
        )
      )

    VoAVariables$off_error[i] <- mean(temp_games$off_error)
    VoAVariables$def_error[i] <- mean(temp_games$def_error)
  }

  ### adjusting adjusted off and def ppg to account for error
  set.seed(802)
  for (i in 1:nrow(VoAVariables)) {
    temp_off_ppg <- VoAVariables$weighted_off_ppg[i]
    VoAVariables$weighted_off_ppg[i] <- temp_off_ppg +
      rnorm(
        1,
        mean = VoAVariables$off_error[i] / 2.5,
        sd = sd(VoAVariables$off_error)
      )
    temp_def_ppg <- VoAVariables$weighted_def_ppg[i]
    VoAVariables$weighted_def_ppg[i] <- temp_def_ppg +
      rnorm(
        1,
        mean = VoAVariables$def_error[i] / 2.5,
        sd = sd(VoAVariables$def_error)
      )

    ### making sure all values are > 0
    set.seed(802)
    if (VoAVariables$weighted_off_ppg[i] <= 0) {
      VoAVariables$weighted_off_ppg[i] <- abs(VoAVariables$weighted_off_ppg[
        i
      ]) +
        abs(rnorm(1, 5, 1))
    }
    if (VoAVariables$weighted_def_ppg[i] <= 0) {
      VoAVariables$weighted_def_ppg[i] <- abs(VoAVariables$weighted_def_ppg[
        i
      ]) +
        abs(rnorm(1, 5, 1))
    }
  }
} else {
  ##### Week 11 - End of Season Error Calculations #####
  VoAVariables <- VoAVariables |>
    mutate(off_error = -999, def_error = -999)

  PrevWeek_VoA <- read_csv(here(
    "Data",
    paste0("VoA", season),
    paste0(season, week_text, as.numeric(nfl_week) - 1, "_", VoAString)
  ))
  CompletedGames <- CompletedGames |>
    mutate(
      home_off_VoA_rating = -999,
      home_def_VoA_rating = -999,
      away_off_VoA_rating = -999,
      away_def_VoA_rating = -999
    )
  for (i in 1:nrow(CompletedGames)) {
    CompletedGames$home_off_VoA_rating[i] <- PrevWeek_VoA$OffVoA_MedRating[
      PrevWeek_VoA$team == CompletedGames$home_team[i]
    ]
    CompletedGames$home_def_VoA_rating[i] <- PrevWeek_VoA$DefVoA_MedRating[
      PrevWeek_VoA$team == CompletedGames$home_team[i]
    ]
    CompletedGames$away_off_VoA_rating[i] <- PrevWeek_VoA$OffVoA_MedRating[
      PrevWeek_VoA$team == CompletedGames$away_team[i]
    ]
    CompletedGames$away_def_VoA_rating[i] <- PrevWeek_VoA$DefVoA_MedRating[
      PrevWeek_VoA$team == CompletedGames$away_team[i]
    ]
  }

  for (i in 1:nrow(VoAVariables)) {
    temp_games <- CompletedGames |>
      filter(
        home_team == VoAVariables$team[i] | away_team == VoAVariables$team[i]
      ) |>
      mutate(
        team = VoAVariables$team[i],
        off_error = case_when(
          home_team == team ~ home_score -
            ((home_off_VoA_rating + away_def_VoA_rating) / 2),
          TRUE ~ away_score - ((away_off_VoA_rating + home_def_VoA_rating) / 2)
        ),
        def_error = case_when(
          home_team == team ~ away_score -
            ((home_def_VoA_rating + away_off_VoA_rating) / 2),
          TRUE ~ home_score - ((away_def_VoA_rating + home_off_VoA_rating) / 2)
        )
      )

    VoAVariables$off_error[i] <- mean(temp_games$off_error)
    VoAVariables$def_error[i] <- mean(temp_games$def_error)
  }

  ### adjusting adjusted off and def ppg to account for error
  for (i in 1:nrow(VoAVariables)) {
    set.seed(802)
    temp_off_ppg <- VoAVariables$adj_off_ppg[i]
    VoAVariables$adj_off_ppg[i] <- temp_off_ppg +
      rnorm(
        1,
        mean = VoAVariables$off_error[i],
        sd = sd(VoAVariables$off_error)
      )
    temp_def_ppg <- VoAVariables$adj_def_ppg[i]
    VoAVariables$adj_def_ppg[i] <- temp_def_ppg +
      rnorm(
        1,
        mean = VoAVariables$def_error[i],
        sd = sd(VoAVariables$def_error)
      )

    ### making sure all values are > 0
    set.seed(802)
    if (VoAVariables$adj_off_ppg[i] <= 0) {
      VoAVariables$adj_off_ppg[i] <- abs(VoAVariables$adj_off_ppg[i]) +
        abs(rnorm(1, 5, 1))
    }
    if (VoAVariables$adj_def_ppg[i] <= 0) {
      VoAVariables$adj_def_ppg[i] <- abs(VoAVariables$adj_def_ppg[i]) +
        abs(rnorm(1, 5, 1))
    }
  }
}
### might put this back in there above
# - (sd(VoAVariables$off_error) / 2))
# - (sd(VoAVariables$def_error) / 2))
