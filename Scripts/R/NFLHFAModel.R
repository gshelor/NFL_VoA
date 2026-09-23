##### NFL VoA Home Field Advantage Model #####
### Going to use lme4 and a mixed effects model to try and get a home-field advantage value to use in the VoP

##### loading packages #####
### loading packages
library(pacman)
# fmt: skip
p_load(tidyverse, gt, nflverse, here, gtExtras, RColorBrewer, webshot2, cmdstanr, arrow, tidybayes, posterior, lme4)
### Inputting season
season <- readline(prompt = "What season is it? ")
### Inputting upcoming week number
upcoming <- readline(prompt = "What week is upcoming? ")

games_PY1 <- nflfastR::load_schedules(2025) |>
  mutate(home_team2 = home_team, away_team2 = away_team) |>
  pivot_longer(
    cols = c("away_team", "home_team"),
    names_to = "home_away",
    values_to = "team"
  ) |>
  mutate(
    team = case_when(
      home_away == "home_team" ~ home_team2,
      TRUE ~ away_team2
    ),
    opp_team = case_when(
      home_away == "home_team" ~ away_team2,
      TRUE ~ home_team2
    ),
    hfa = as.factor(
      case_when(
        location == "Neutral" ~ 0,
        home_team2 == team ~ 1,
        TRUE ~ -1
      )
    )
  )

games <- nflfastR::load_schedules(2026) |>
  drop_na(result) |>
  mutate(home_team2 = home_team, away_team2 = away_team) |>
  pivot_longer(
    cols = c("away_team", "home_team"),
    names_to = "home_away",
    values_to = "team"
  ) |>
  mutate(
    team = case_when(
      home_away == "home_team" ~ home_team2,
      TRUE ~ away_team2
    ),
    opp_team = case_when(
      home_away == "home_team" ~ away_team2,
      TRUE ~ home_team2
    ),
    hfa = as.factor(
      case_when(
        location == "Neutral" ~ 0,
        home_team2 == team ~ 1,
        TRUE ~ -1
      )
    )
  )

full_games <- rbind(games, games_PY1)


set.seed(802)
hfa_model <- lmer(result ~ hfa + (1 | team) + (1 | opp_team), data = games_PY1)

hfa_coef <- data.frame(fixef(hfa_model))
