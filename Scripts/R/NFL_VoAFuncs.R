##### This script will store functions used in the VoA to help make the main VoA script shorter and easier to read #####

### function to create VoA Variables dataframe
create_voa_vars <- function(nfl_week) {
    if (nfl_week == 0) {
        ### creating dataframe to eventually store VoA Variables and ratings
        VoAVariables <- data.frame(
            season = rep(as.integer(season), 32),
            week = rep(as.integer(nfl_week), 32),
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
            net_st_ppg_PY3 = -999
        )
    } else {
        ### creating dataframe to eventually store VoA Variables and ratings
        VoAVariables <- data.frame(
            season = rep(as.integer(season), 32),
            week = rep(as.integer(nfl_week), 32),
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
            net_st_ppg = -999
        )
    }
    ### returning appropriate df
    return(VoAVariables)
}


### function which creates dataframe specifically for model training
### so, only to be used in week 0
create_voa_vars_train <- function(year) {
    ### creating dataframe to eventually store VoA Variables and ratings
    VoAVariables_train <- data.frame(
        season = rep(as.integer(year), 32),
        week = rep(as.integer(nfl_week), 32),
        team = unique(PY_PBP$home_team),
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
        net_st_ppg = -999
    )
    ### returning model training df
    return(VoAVariables_train)
}

### function for getting clean games
get_clean_games <- function() {
    CompletedGames_output <- load_schedules(as.numeric(season)) |>
        select(
            game_id,
            season,
            game_type,
            week,
            gameday,
            weekday,
            gametime,
            away_team,
            away_score,
            home_team,
            home_score,
            location,
            result,
            total,
            overtime,
            spread_line,
            total_line,
            div_game,
            temp,
            wind,
            stadium
        ) |>
        filter(week <= as.numeric(nfl_week))

    return(CompletedGames_output)
}

### function which calculates stats directly from PBP data
extract_pbp_stats <- function(
    VoA_df,
    rushpass_plays,
    success_plays,
    ThirdDowns,
    FourthDowns,
    passplays,
    rushplays,
    scoringopp_plays,
    turnovers,
    TDs,
    TwoPts,
    FGs,
    Punts,
    Kickoffs,
    XPts
) {
    for (x in 1:nrow(VoA_df)) {
        ### creating temp dfs
        temp_offplays <- rushpass_plays |>
            filter(posteam == VoA_df$team[x])
        temp_offsuccessplays <- success_plays |>
            filter(posteam == VoA_df$team[x])
        temp_offthirddowns <- ThirdDowns |>
            filter(posteam == VoA_df$team[x]) |>
            drop_na(third_down_converted)
        temp_conv_offthirddowns <- temp_offthirddowns |>
            filter(third_down_converted == 1)
        temp_off_fourthdowns <- FourthDowns |>
            filter(posteam == VoA_df$team[x]) |>
            drop_na(fourth_down_converted)
        temp_conv_offfourthdowns <- temp_off_fourthdowns |>
            filter(fourth_down_converted == 1)
        temp_off_passplays <- passplays |>
            filter(posteam == VoA_df$team[x])
        temp_off_comppass <- temp_off_passplays |>
            filter(complete_pass == 1)
        temp_off_rushplays <- rushplays |>
            filter(posteam == VoA_df$team[x])
        temp_off_scoringoppplays <- scoringopp_plays |>
            filter(posteam == VoA_df$team[x]) |>
            drop_na(drive)
        temp_off_scorringopp_TDs <- temp_off_scoringoppplays |>
            filter(touchdown == 1)
        temp_off_scorringopp_FGs <- temp_off_scoringoppplays |>
            filter(field_goal_result == "made")
        temp_off_turnovers <- turnovers |>
            filter(posteam == VoA_df$team[x])
        temp_off_TDs <- TDs |>
            filter(posteam == VoA_df$team[x])
        temp_off_2pts <- TwoPts |>
            filter(
                posteam == VoA_df$team[x] &
                    two_point_conv_result == "success"
            )
        ### PY1 def stats
        temp_defplays <- rushpass_plays |>
            filter(defteam == VoA_df$team[x])
        temp_defsuccessplays <- success_plays |>
            filter(defteam == VoA_df$team[x])
        temp_defthirddowns <- ThirdDowns |>
            filter(defteam == VoA_df$team[x]) |>
            drop_na(third_down_converted)
        temp_conv_defthirddowns <- temp_defthirddowns |>
            filter(third_down_converted == 1)
        temp_def_fourthdowns <- FourthDowns |>
            filter(defteam == VoA_df$team[x]) |>
            drop_na(fourth_down_converted)
        temp_conv_deffourthdowns <- temp_def_fourthdowns |>
            filter(fourth_down_converted == 1)
        temp_def_passplays <- passplays |>
            filter(defteam == VoA_df$team[x])
        temp_def_comppass <- temp_def_passplays |>
            filter(complete_pass == 1)
        temp_def_rushplays <- rushplays |>
            filter(defteam == VoA_df$team[x])
        temp_def_scoringoppplays <- scoringopp_plays |>
            filter(defteam == VoA_df$team[x]) |>
            drop_na(drive)
        temp_def_scorringopp_TDs <- temp_def_scoringoppplays |>
            filter(touchdown == 1)
        temp_def_scorringopp_FGs <- temp_def_scoringoppplays |>
            filter(field_goal_result == "made")
        temp_def_turnovers <- turnovers |>
            filter(defteam == VoA_df$team[x])
        temp_def_TDs <- TDs |>
            filter(defteam == VoA_df$team[x])
        temp_def_2pts <- TwoPts |>
            filter(
                defteam == VoA_df$team[x] &
                    two_point_conv_result == "success"
            )
        ### temp PY1 special teams dfs
        ## on kickoffs, defteam does kicking
        ## on punts, posteam does punting
        temp_off_FGs <- FGs |>
            filter(posteam == VoA_df$team[x])
        temp_off_goodFGs <- temp_off_FGs |>
            filter(field_goal_result == "made")
        temp_def_FGs <- FGs |>
            filter(
                defteam == VoA_df$team[x] & field_goal_result == "made"
            )
        temp_def_goodFGs <- temp_def_FGs |>
            filter(field_goal_result == "made")
        temp_returned_punts <- Punts |>
            filter(defteam == VoA_df$team[x])
        temp_returned_kicks <- Kickoffs |>
            filter(posteam == VoA_df$team[x])
        temp_returned_punt_TDs <- temp_returned_punts |>
            filter(return_touchdown == 1)
        temp_returned_kick_TDs <- temp_returned_kicks |>
            filter(return_touchdown == 1)
        temp_kicked_punts <- Punts |>
            filter(posteam == VoA_df$team[x])
        temp_kicked_kicks <- Kickoffs |>
            filter(defteam == VoA_df$team[x])
        temp_kicked_punt_TDs <- temp_kicked_punts |>
            filter(return_touchdown == 1)
        temp_kicked_kick_TDs <- temp_kicked_kicks |>
            filter(return_touchdown == 1)
        temp_off_xps <- XPts |>
            filter(posteam == VoA_df$team[x])
        temp_def_xps <- XPts |>
            filter(defteam == VoA_df$team[x])
        temp_off_good_xps <- temp_off_xps |>
            filter(extra_point_result == "good")
        temp_def_good_xps <- temp_def_xps |>
            filter(extra_point_result == "good")
        ### used to get net ST epa/play
        temp_off_st_plays <- rbind(
            temp_off_FGs,
            temp_off_xps,
            temp_returned_kicks,
            temp_returned_punts
        )
        temp_def_st_plays <- rbind(
            temp_def_FGs,
            temp_def_xps,
            temp_kicked_kicks,
            temp_kicked_punts
        )

        ### Evaluating Stats
        VoA_df$off_ypp[x] <- mean(temp_offplays$yards_gained)
        VoA_df$off_epa[x] <- mean(temp_offplays$epa)
        VoA_df$off_success_rt[x] <- nrow(temp_offsuccessplays) /
            nrow(temp_offplays)
        VoA_df$off_explosiveness[x] <- mean(temp_offsuccessplays$epa)
        VoA_df$off_third_conv_rate[x] <- nrow(temp_conv_offthirddowns) /
            nrow(temp_offthirddowns)
        VoA_df$off_fourth_conv_rate[x] <- nrow(temp_conv_offfourthdowns) /
            nrow(temp_off_fourthdowns)
        VoA_df$off_pass_ypa[x] <- mean(temp_off_passplays$yards_gained)
        VoA_df$off_pass_ypc[x] <- mean(temp_off_comppass$yards_gained)
        VoA_df$off_rush_ypa[x] <- mean(temp_off_rushplays$yards_gained)
        VoA_df$off_pts_per_opp[x] <- ((nrow(temp_off_scorringopp_TDs) *
            6) +
            (nrow(temp_off_scorringopp_FGs) * 3)) /
            length(unique(paste0(
                temp_off_scoringoppplays$game_id,
                temp_off_scoringoppplays$drive
            )))
        VoA_df$off_turnovers[x] <- nrow(temp_off_turnovers) /
            length(unique(temp_offplays$week))
        VoA_df$off_plays_pg[x] <- nrow(temp_offplays) /
            length(unique(temp_offplays$week))
        VoA_df$off_ppg[x] <- ((nrow(temp_off_TDs) * 6) +
            (nrow(temp_off_2pts) * 2)) /
            length(unique(temp_off_rushplays$week))
        ## PY1 defensive stats now
        VoA_df$def_ypp[x] <- mean(temp_defplays$yards_gained)
        VoA_df$def_epa[x] <- mean(temp_defplays$epa)
        VoA_df$def_success_rt[x] <- nrow(temp_defsuccessplays) /
            nrow(temp_defplays)
        VoA_df$def_explosiveness[x] <- mean(temp_defsuccessplays$epa)
        VoA_df$def_third_conv_rate[x] <- nrow(temp_conv_defthirddowns) /
            nrow(temp_defthirddowns)
        VoA_df$def_fourth_conv_rate[x] <- nrow(temp_conv_deffourthdowns) /
            nrow(temp_def_fourthdowns)
        VoA_df$def_pass_ypa[x] <- mean(temp_def_passplays$yards_gained)
        VoA_df$def_pass_ypc[x] <- mean(temp_def_comppass$yards_gained)
        VoA_df$def_rush_ypa[x] <- mean(temp_def_rushplays$yards_gained)
        VoA_df$def_pts_per_opp[x] <- ((nrow(temp_def_scorringopp_TDs) *
            6) +
            (nrow(temp_def_scorringopp_FGs) * 3)) /
            length(unique(paste0(
                temp_def_scoringoppplays$game_id,
                temp_def_scoringoppplays$drive
            )))
        VoA_df$def_turnovers[x] <- nrow(temp_def_turnovers) /
            length(unique(temp_defplays))
        VoA_df$def_plays_pg[x] <- nrow(temp_defplays) /
            length(unique(temp_defplays$week))
        VoA_df$def_ppg[x] <- ((nrow(temp_def_TDs) * 6) +
            (nrow(temp_def_2pts) * 2)) /
            length(unique(temp_def_rushplays$week))
        ## Current Special teams stats now
        VoA_df$st_net_epa[x] <- mean(temp_off_st_plays$epa) -
            mean(temp_def_st_plays$epa)
        VoA_df$st_punt_return_yds[x] <- mean(
            temp_returned_punts$return_yards
        )
        VoA_df$st_kick_return_yds[x] <- mean(
            temp_returned_kicks$return_yards
        )
        VoA_df$st_kick_return_TDs[x] <- nrow(temp_returned_kick_TDs) /
            length(unique(temp_offplays$week))
        VoA_df$st_punt_return_TDs[x] <- nrow(temp_returned_punt_TDs) /
            length(unique(temp_offplays$week))
        VoA_df$fg_rate[x] <- nrow(temp_off_goodFGs) / nrow(temp_off_FGs)
        VoA_df$fg_made_pg[x] <- nrow(temp_off_goodFGs) /
            length(unique(temp_offplays$week))
        VoA_df$xp_rate[x] <- nrow(temp_off_good_xps) / nrow(temp_off_xps)
        VoA_df$xp_made_pg[x] <- nrow(temp_off_good_xps) /
            length(unique(temp_offplays$week))
        VoA_df$st_punt_return_yds_allowed[x] <- mean(
            temp_kicked_punts$return_yards
        )
        VoA_df$st_kick_return_yds_allowed[x] <- mean(
            temp_kicked_kicks$return_yards
        )
        VoA_df$st_kick_return_TDs_allowed[x] <- nrow(
            temp_kicked_kick_TDs
        ) /
            length(unique(temp_offplays$week))
        VoA_df$st_punt_return_TDs_allowed[x] <- nrow(
            temp_kicked_punt_TDs
        ) /
            length(unique(temp_offplays$week))
        VoA_df$fg_rate_allowed[x] <- nrow(temp_def_goodFGs) /
            nrow(temp_def_FGs)
        VoA_df$fg_made_pg_allowed[x] <- nrow(temp_def_goodFGs) /
            length(unique(temp_offplays$week))
        VoA_df$xp_rate_allowed[x] <- nrow(temp_def_good_xps) /
            nrow(temp_def_xps)
        VoA_df$xp_made_pg_allowed[x] <- nrow(temp_def_good_xps) /
            length(unique(temp_offplays$week))
        VoA_df$net_st_ppg[x] <- (((nrow(temp_off_goodFGs) * 3) +
            (nrow(temp_returned_punt_TDs) * 6) +
            (nrow(temp_returned_kick_TDs) * 6) +
            nrow(temp_off_good_xps)) -
            ((nrow(temp_def_goodFGs) * 3) +
                (nrow(temp_kicked_punt_TDs) * 6) +
                (nrow(temp_kicked_kick_TDs) * 6) +
                nrow(temp_def_good_xps))) /
            length(unique(temp_offplays$week))
    }
    ### Adding columns of ppg above avg for both offense and defense and adjusting off_ppg and def_ppg
    VoA_df <- VoA_df |>
        mutate(
            net_punt_return_yds = st_punt_return_yds -
                st_punt_return_yds_allowed,
            net_kick_return_yds = st_kick_return_yds -
                st_kick_return_yds_allowed,
            net_punt_return_TDs = st_punt_return_TDs -
                st_punt_return_TDs_allowed,
            net_kick_return_TDs = st_kick_return_TDs -
                st_kick_return_TDs_allowed,
            net_fg_rate = fg_rate - fg_rate_allowed,
            net_fg_made_pg = fg_made_pg - fg_made_pg_allowed,
            net_xp_rate = xp_rate - xp_rate_allowed,
            net_xp_made_pg = xp_made_pg - xp_made_pg_allowed,
            off_ppg_aboveavg = off_ppg - mean(off_ppg),
            def_ppg_aboveavg = def_ppg - mean(def_ppg)
        )

    ### Creating opponent-adjusted stats
    ### EPA/play
    ### subsetting columns for epa/play adjustment
    PBP_EPAAdjustment <- rushpass_plays |>
        select(game_id, home_team, posteam, defteam, epa, location) |>
        mutate(
            hfa = as.factor(case_when(
                location == "Neutral" ~ 0,
                ### home team on offense
                posteam == home_team ~ 1,
                ### home team on defense
                TRUE ~ -1
            )),
            posteam = as.factor(posteam),
            defteam = as.factor(defteam)
        ) |>
        drop_na()

    ### fitting mixed effects model, treating posessing team and defensive team as random effects
    set.seed(802)
    epa_mixed_model <- lmer(
        epa ~ hfa + (1 | posteam) + (1 | defteam),
        data = PBP_EPAAdjustment
    )

    ### Extract random effects (team adjustments)
    team_effects <- ranef(epa_mixed_model)

    ### Extract offensive adjustments
    off_adj <- as.data.frame(team_effects$posteam) |>
        rename(adj_off_epa = `(Intercept)`) |>
        mutate(team = rownames(team_effects$posteam))

    ### extract defensive adjustment
    def_adj <- as.data.frame(team_effects$defteam) |>
        rename(adj_def_epa = `(Intercept)`) |>
        mutate(team = rownames(team_effects$defteam))

    ### average EPA (model intercept)
    avg_epa <- fixef(epa_mixed_model)["(Intercept)"]

    ### combine and join back to VoA_df
    VoA_df <- VoA_df |>
        left_join(off_adj, by = "team") |>
        left_join(def_adj, by = "team") |>
        mutate(
            adj_off_epa = adj_off_epa + avg_epa,
            adj_def_epa = adj_def_epa + avg_epa
        )

    ### opponent adjusted plays per game
    PlaysPG_Adjustment <- PBP_EPAAdjustment |>
        group_by(game_id) |>
        summarize(
            home_off_plays = sum(posteam == home_team),
            away_off_plays = sum(posteam == away_team),
            home_team = as.factor(unique(home_team)[1]),
            away_team = as.factor(unique(away_team)[1]),
            location = unique(location)[1]
        ) |>
        pivot_longer(
            cols = ends_with("_plays"),
            names_to = "home_away_col_names",
            values_to = "team_plays"
        ) |>
        mutate(
            team = case_when(
                home_away_col_names == "home_off_plays" ~ home_team,
                TRUE ~ away_team
            ),
            opp_team = case_when(
                home_away_col_names == "home_off_plays" ~ away_team,
                TRUE ~ home_team
            ),
            hfa = as.factor(case_when(
                location == "Neutral" ~ 0,
                home_team == team ~ 1,
                TRUE ~ -1
            ))
        )

    ### fitting mixed effects model, treating team and opposing team as random effects
    set.seed(802)
    plays_mixed_model <- lmer(
        team_plays ~ hfa + (1 | team) + (1 | opp_team),
        data = PlaysPG_Adjustment
    )

    ### Extract random effects (team adjustments)
    team_effects <- ranef(plays_mixed_model)

    ### Extract offensive adjustments
    off_adj <- as.data.frame(team_effects$team) |>
        rename(adj_off_plays_pg = `(Intercept)`) |>
        mutate(team = rownames(team_effects$team))

    ### extract defensive adjustment
    def_adj <- as.data.frame(team_effects$opp_team) |>
        rename(adj_def_plays_pg = `(Intercept)`) |>
        mutate(team = rownames(team_effects$opp_team))

    ### average plays per game (model intercept)
    avg_plays_pg <- fixef(plays_mixed_model)["(Intercept)"]

    ### combine and join back to VoA_df
    VoA_df <- VoA_df |>
        left_join(off_adj, by = "team") |>
        left_join(def_adj, by = "team") |>
        mutate(
            adj_off_plays_pg = adj_off_plays_pg + avg_plays_pg,
            adj_def_plays_pg = adj_def_plays_pg + avg_plays_pg
        )

    ### Explosiveness
    ### subsetting columns for epa/play (explosiveness, so only EPA/play on successful plays) adjustment
    PBP_ExpAdjustment <- success_plays |>
        select(game_id, home_team, posteam, defteam, epa, location) |>
        mutate(
            hfa = as.factor(case_when(
                location == "Neutral" ~ 0,
                ### home team on offense
                posteam == home_team ~ 1,
                ### home team on defense
                TRUE ~ -1
            )),
            posteam = as.factor(posteam),
            defteam = as.factor(defteam)
        ) |>
        drop_na()

    ### fitting mixed effects model, treating posessing team and defensive team as random effects
    set.seed(802)
    exp_mixed_model <- lmer(
        epa ~ hfa + (1 | posteam) + (1 | defteam),
        data = PBP_ExpAdjustment
    )

    ### Extract random effects (team adjustments)
    team_effects <- ranef(exp_mixed_model)

    ### Extract offensive adjustments
    off_adj <- as.data.frame(team_effects$posteam) |>
        rename(adj_off_explosiveness = `(Intercept)`) |>
        mutate(team = rownames(team_effects$posteam))

    ### extract defensive adjustment
    def_adj <- as.data.frame(team_effects$defteam) |>
        rename(adj_def_explosiveness = `(Intercept)`) |>
        mutate(team = rownames(team_effects$defteam))

    ### average EPA (model intercept)
    avg_explosiveness <- fixef(exp_mixed_model)["(Intercept)"]

    ### combine and join back to VoA_df
    VoA_df <- VoA_df |>
        left_join(off_adj, by = "team") |>
        left_join(def_adj, by = "team") |>
        mutate(
            adj_off_explosiveness = adj_off_explosiveness + avg_explosiveness,
            adj_def_explosiveness = adj_def_explosiveness + avg_explosivensss
        )

    ### ppg
    ## this will initially give me pts/play, then I will multiply it by off/def plays per game when binding to VoA_df
    ### subsetting columns for pts/play adjustment
    PBP_PPGAdjustment <- rushpass_plays |>
        select(
            game_id,
            home_team,
            posteam,
            defteam,
            two_point_conv_result,
            pass_touchdown,
            rush_touchdown,
            location
        ) |>
        mutate(
            hfa = as.factor(case_when(
                location == "Neutral" ~ 0,
                ### home team on offense
                posteam == home_team ~ 1,
                ### home team on defense
                TRUE ~ -1
            )),
            play_pts_scored = case_when(
                two_point_conv_result == "success" ~ 2,
                pass_touchdown == 1 ~ 6,
                rush_touchdown == 1 ~ 6,
                TRUE ~ 0
            ),
            posteam = as.factor(posteam),
            defteam = as.factor(defteam)
        ) |>
        drop_na(game_id, home_team, posteam, defteam, hfa, location)

    ### fitting mixed effects model, treating posessing team and defensive team as random effects
    set.seed(802)
    ppg_mixed_model <- lmer(
        play_pts_scored ~ hfa + (1 | posteam) + (1 | defteam),
        data = PBP_PPGAdjustment
    )

    ### Extract random effects (team adjustments)
    team_effects <- ranef(ppg_mixed_model)

    ### average EPA (model intercept)
    avg_ppp <- fixef(ppg_mixed_model)["(Intercept)"]

    ### Extract offensive adjustments
    off_adj <- as.data.frame(team_effects$posteam) |>
        rename(adj_off_pts_per_play = `(Intercept)`) |>
        mutate(team = rownames(team_effects$posteam)) |>
        mutate(adj_off_pts_per_play = adj_off_pts_per_play + avg_ppp)

    ### extract defensive adjustment
    def_adj <- as.data.frame(team_effects$defteam) |>
        rename(adj_def_pts_per_play = `(Intercept)`) |>
        mutate(team = rownames(team_effects$defteam)) |>
        mutate(adj_def_pts_per_play = adj_def_pts_per_play + avg_ppp)

    ### combine and join back to VoA_df
    VoA_df <- VoA_df |>
        left_join(off_adj, by = "team") |>
        left_join(def_adj, by = "team") |>
        mutate(
            adj_off_ppg = adj_off_pts_per_play * mean(adj_off_plays_pg) * 1.5,
            adj_def_ppg = adj_def_pts_per_play * mean(adj_def_plays_pg) * 1.5
        )

    ### yards/play opponent adjustment
    ### subsetting columns for adjustment
    PBP_YPPAdjustment <- rushpass_plays |>
        select(game_id, home_team, posteam, defteam, yards_gained, location) |>
        mutate(
            hfa = as.factor(case_when(
                location == "Neutral" ~ 0,
                ### home team on offense
                posteam == home_team ~ 1,
                ### home team on defense
                TRUE ~ -1
            )),
            posteam = as.factor(posteam),
            defteam = as.factor(defteam)
        ) |>
        drop_na()

    ### fitting mixed effects model, treating posessing team and defensive team as random effects
    set.seed(802)
    ypp_mixed_model <- lmer(
        yards_gained ~ hfa + (1 | posteam) + (1 | defteam),
        data = PBP_YPPAdjustment
    )

    ### Extract random effects (team adjustments)
    team_effects <- ranef(ypp_mixed_model)

    ### Extract offensive adjustments
    off_adj <- as.data.frame(team_effects$posteam) |>
        rename(adj_off_ypp = `(Intercept)`) |>
        mutate(team = rownames(team_effects$posteam))

    ### extract defensive adjustment
    def_adj <- as.data.frame(team_effects$defteam) |>
        rename(adj_def_ypp = `(Intercept)`) |>
        mutate(team = rownames(team_effects$defteam))

    ### average EPA (model intercept)
    avg_ypp <- fixef(ypp_mixed_model)["(Intercept)"]

    ### combine and join back to VoA_df
    VoA_df <- VoA_df |>
        left_join(off_adj, by = "team") |>
        left_join(def_adj, by = "team") |>
        mutate(
            adj_off_ypp = adj_off_ypp + avg_ypp,
            adj_def_ypp = adj_def_ypp + avg_ypp
        )

    ### return VoAVariables object
    return(VoA_df)
}

### function for calculating opponent-adjusted stats
## do not use in week 0 unless using on model training dfs
# calc_adj_stats <- function(
#     VoA_df,
#     rushpass_plays,
#     success_plays,
#     ThirdDowns,
#     FourthDowns,
#     passplays,
#     rushplays,
#     scoringopp_plays,
#     turnovers,
#     TDs,
#     TwoPts,
#     FGs,
#     Punts,
#     Kickoffs,
#     XPts
# ) {}

extract_VoAVars_pbp_stats <- function(
    VoA_df,
    PY1_rushpass_plays,
    PY1_success_plays,
    PY1_3rdDowns,
    PY1_4thDowns,
    PY1_passplays,
    PY1_rushplays,
    PY1_scoringopp_plays,
    PY1_turnovers,
    PY1_TDs,
    PY1_2pts,
    PY1_FGs,
    PY1_Punts,
    PY1_Kickoffs,
    PY1_XPts,
    ### PY2 PBP args
    PY2_rushpass_plays,
    PY2_success_plays,
    PY2_3rdDowns,
    PY2_4thDowns,
    PY2_passplays,
    PY2_rushplays,
    PY2_scoringopp_plays,
    PY2_turnovers,
    PY2_TDs,
    PY2_2pts,
    PY2_FGs,
    PY2_Punts,
    PY2_Kickoffs,
    PY2_XPts,
    ### PY3 PBP args
    PY3_rushpass_plays,
    PY3_success_plays,
    PY3_3rdDowns,
    PY3_4thDowns,
    PY3_passplays,
    PY3_rushplays,
    PY3_scoringopp_plays,
    PY3_turnovers,
    PY3_TDs,
    PY3_2pts,
    PY3_FGs,
    PY3_Punts,
    PY3_Kickoffs,
    PY3_XPts
) {
    for (x in 1:nrow(VoA_df)) {
        ### PY1 temp dfs
        ### temp PY1 offensive stat dfs
        temp_PY1_offplays <- PY1_rushpass_plays |>
            filter(posteam == VoA_df$team[x])
        temp_PY1_offsuccessplays <- PY1_success_plays |>
            filter(posteam == VoA_df$team[x])
        temp_PY1_offthirddowns <- PY1_3rdDowns |>
            filter(posteam == VoA_df$team[x]) |>
            drop_na(third_down_converted)
        temp_PY1_conv_offthirddowns <- temp_PY1_offthirddowns |>
            filter(third_down_converted == 1)
        temp_PY1_off_fourthdowns <- PY1_4thDowns |>
            filter(posteam == VoA_df$team[x]) |>
            drop_na(fourth_down_converted)
        temp_PY1_conv_offfourthdowns <- temp_PY1_off_fourthdowns |>
            filter(fourth_down_converted == 1)
        temp_PY1_off_passplays <- PY1_passplays |>
            filter(posteam == VoA_df$team[x])
        temp_PY1_off_comppass <- temp_PY1_off_passplays |>
            filter(complete_pass == 1)
        temp_PY1_off_rushplays <- PY1_rushplays |>
            filter(posteam == VoA_df$team[x])
        temp_PY1_off_scoringoppplays <- PY1_scoringopp_plays |>
            filter(posteam == VoA_df$team[x]) |>
            drop_na(drive)
        temp_PY1_off_scorringopp_TDs <- temp_PY1_off_scoringoppplays |>
            filter(touchdown == 1)
        temp_PY1_off_scorringopp_FGs <- temp_PY1_off_scoringoppplays |>
            filter(field_goal_result == "made")
        temp_PY1_off_turnovers <- PY1_Turnovers |>
            filter(posteam == VoA_df$team[x])
        temp_PY1_off_TDs <- PY1_TDs |>
            filter(posteam == VoA_df$team[x])
        temp_PY1_off_2pts <- PY1_2pts |>
            filter(
                posteam == VoA_df$team[x] &
                    two_point_conv_result == "success"
            )
        ### PY1 def stats
        temp_PY1_defplays <- PY1_rushpass_plays |>
            filter(defteam == VoA_df$team[x])
        temp_PY1_defsuccessplays <- PY1_success_plays |>
            filter(defteam == VoA_df$team[x])
        temp_PY1_defthirddowns <- PY1_3rdDowns |>
            filter(defteam == VoA_df$team[x]) |>
            drop_na(third_down_converted)
        temp_PY1_conv_defthirddowns <- temp_PY1_defthirddowns |>
            filter(third_down_converted == 1)
        temp_PY1_def_fourthdowns <- PY1_4thDowns |>
            filter(defteam == VoA_df$team[x]) |>
            drop_na(fourth_down_converted)
        temp_PY1_conv_deffourthdowns <- temp_PY1_def_fourthdowns |>
            filter(fourth_down_converted == 1)
        temp_PY1_def_passplays <- PY1_passplays |>
            filter(defteam == VoA_df$team[x])
        temp_PY1_def_comppass <- temp_PY1_def_passplays |>
            filter(complete_pass == 1)
        temp_PY1_def_rushplays <- PY1_rushplays |>
            filter(defteam == VoA_df$team[x])
        temp_PY1_def_scoringoppplays <- PY1_scoringopp_plays |>
            filter(defteam == VoA_df$team[x]) |>
            drop_na(drive)
        temp_PY1_def_scorringopp_TDs <- temp_PY1_def_scoringoppplays |>
            filter(touchdown == 1)
        temp_PY1_def_scorringopp_FGs <- temp_PY1_def_scoringoppplays |>
            filter(field_goal_result == "made")
        temp_PY1_def_turnovers <- PY1_Turnovers |>
            filter(defteam == VoA_df$team[x])
        temp_PY1_def_TDs <- PY1_TDs |>
            filter(defteam == VoA_df$team[x])
        temp_PY1_def_2pts <- PY1_2pts |>
            filter(
                defteam == VoA_df$team[x] &
                    two_point_conv_result == "success"
            )
        ### temp PY1 special teams dfs
        ## on kickoffs, defteam does kicking
        ## on punts, posteam does punting
        temp_PY1_off_FGs <- PY1_FGs |>
            filter(posteam == VoA_df$team[x])
        temp_PY1_off_goodFGs <- temp_PY1_off_FGs |>
            filter(field_goal_result == "made")
        temp_PY1_def_FGs <- PY1_FGs |>
            filter(
                defteam == VoA_df$team[x] & field_goal_result == "made"
            )
        temp_PY1_def_goodFGs <- temp_PY1_def_FGs |>
            filter(field_goal_result == "made")
        temp_PY1_returned_punts <- PY1_punts |>
            filter(defteam == VoA_df$team[x])
        temp_PY1_returned_kicks <- PY1_kickoffs |>
            filter(posteam == VoA_df$team[x])
        temp_PY1_returned_punt_TDs <- temp_PY1_returned_punts |>
            filter(return_touchdown == 1)
        temp_PY1_returned_kick_TDs <- temp_PY1_returned_kicks |>
            filter(return_touchdown == 1)
        temp_PY1_kicked_punts <- PY1_punts |>
            filter(posteam == VoA_df$team[x])
        temp_PY1_kicked_kicks <- PY1_kickoffs |>
            filter(defteam == VoA_df$team[x])
        temp_PY1_kicked_punt_TDs <- temp_PY1_kicked_punts |>
            filter(return_touchdown == 1)
        temp_PY1_kicked_kick_TDs <- temp_PY1_kicked_kicks |>
            filter(return_touchdown == 1)
        temp_PY1_off_xps <- PY1_XPts |>
            filter(posteam == VoA_df$team[x])
        temp_PY1_def_xps <- PY1_XPts |>
            filter(defteam == VoA_df$team[x])
        temp_PY1_off_good_xps <- temp_PY1_off_xps |>
            filter(extra_point_result == "good")
        temp_PY1_def_good_xps <- temp_PY1_def_xps |>
            filter(extra_point_result == "good")
        ### used to get net ST epa/play
        temp_PY1_off_st_plays <- rbind(
            temp_PY1_off_FGs,
            temp_PY1_off_xps,
            temp_PY1_returned_kicks,
            temp_PY1_returned_punts
        )
        temp_PY1_def_st_plays <- rbind(
            temp_PY1_def_FGs,
            temp_PY1_def_xps,
            temp_PY1_kicked_kicks,
            temp_PY1_kicked_punts
        )

        ### PY2 temp dfs
        ### temp PY2 offensive stat dfs
        temp_PY2_offplays <- PY2_rushpass_plays |>
            filter(posteam == VoA_df$team[x])
        temp_PY2_offsuccessplays <- PY2_success_plays |>
            filter(posteam == VoA_df$team[x])
        temp_PY2_offthirddowns <- PY2_3rdDowns |>
            filter(posteam == VoA_df$team[x]) |>
            drop_na(third_down_converted)
        temp_PY2_conv_offthirddowns <- temp_PY2_offthirddowns |>
            filter(third_down_converted == 1)
        temp_PY2_off_fourthdowns <- PY2_4thDowns |>
            filter(posteam == VoA_df$team[x]) |>
            drop_na(fourth_down_converted)
        temp_PY2_conv_offfourthdowns <- temp_PY2_off_fourthdowns |>
            filter(fourth_down_converted == 1)
        temp_PY2_off_passplays <- PY2_passplays |>
            filter(posteam == VoA_df$team[x])
        temp_PY2_off_comppass <- temp_PY2_off_passplays |>
            filter(complete_pass == 1)
        temp_PY2_off_rushplays <- PY2_rushplays |>
            filter(posteam == VoA_df$team[x])
        temp_PY2_off_scoringoppplays <- PY2_scoringopp_plays |>
            filter(posteam == VoA_df$team[x]) |>
            drop_na(drive)
        temp_PY2_off_scorringopp_TDs <- temp_PY2_off_scoringoppplays |>
            filter(touchdown == 1)
        temp_PY2_off_scorringopp_FGs <- temp_PY2_off_scoringoppplays |>
            filter(field_goal_result == "made")
        temp_PY2_off_turnovers <- PY2_Turnovers |>
            filter(posteam == VoA_df$team[x])
        temp_PY2_off_TDs <- PY2_TDs |>
            filter(posteam == VoA_df$team[x])
        temp_PY2_off_2pts <- PY2_2pts |>
            filter(
                posteam == VoA_df$team[x] &
                    two_point_conv_result == "success"
            )
        ### PY2 def stats
        temp_PY2_defplays <- PY2_rushpass_plays |>
            filter(defteam == VoA_df$team[x])
        temp_PY2_defsuccessplays <- PY2_success_plays |>
            filter(defteam == VoA_df$team[x])
        temp_PY2_defthirddowns <- PY2_3rdDowns |>
            filter(defteam == VoA_df$team[x]) |>
            drop_na(third_down_converted)
        temp_PY2_conv_defthirddowns <- temp_PY2_defthirddowns |>
            filter(third_down_converted == 1)
        temp_PY2_def_fourthdowns <- PY2_4thDowns |>
            filter(defteam == VoA_df$team[x]) |>
            drop_na(fourth_down_converted)
        temp_PY2_conv_deffourthdowns <- temp_PY2_def_fourthdowns |>
            filter(fourth_down_converted == 1)
        temp_PY2_def_passplays <- PY2_passplays |>
            filter(defteam == VoA_df$team[x])
        temp_PY2_def_comppass <- temp_PY2_def_passplays |>
            filter(complete_pass == 1)
        temp_PY2_def_rushplays <- PY2_rushplays |>
            filter(defteam == VoA_df$team[x])
        temp_PY2_def_scoringoppplays <- PY2_scoringopp_plays |>
            filter(defteam == VoA_df$team[x]) |>
            drop_na(drive)
        temp_PY2_def_scorringopp_TDs <- temp_PY2_def_scoringoppplays |>
            filter(touchdown == 1)
        temp_PY2_def_scorringopp_FGs <- temp_PY2_def_scoringoppplays |>
            filter(field_goal_result == "made")
        temp_PY2_def_turnovers <- PY2_Turnovers |>
            filter(defteam == VoA_df$team[x])
        temp_PY2_def_TDs <- PY2_TDs |>
            filter(defteam == VoA_df$team[x])
        temp_PY2_def_2pts <- PY2_2pts |>
            filter(
                defteam == VoA_df$team[x] &
                    two_point_conv_result == "success"
            )
        ### temp PY2 special teams dfs
        ## on kickoffs, defteam does kicking
        ## on punts, posteam does punting
        temp_PY2_off_FGs <- PY2_FGs |>
            filter(posteam == VoA_df$team[x])
        temp_PY2_off_goodFGs <- temp_PY2_off_FGs |>
            filter(field_goal_result == "made")
        temp_PY2_def_FGs <- PY2_FGs |>
            filter(
                defteam == VoA_df$team[x] & field_goal_result == "made"
            )
        temp_PY2_def_goodFGs <- temp_PY2_def_FGs |>
            filter(field_goal_result == "made")
        temp_PY2_returned_punts <- PY2_punts |>
            filter(defteam == VoA_df$team[x])
        temp_PY2_returned_kicks <- PY2_kickoffs |>
            filter(posteam == VoA_df$team[x])
        temp_PY2_returned_punt_TDs <- temp_PY2_returned_punts |>
            filter(return_touchdown == 1)
        temp_PY2_returned_kick_TDs <- temp_PY2_returned_kicks |>
            filter(return_touchdown == 1)
        temp_PY2_kicked_punts <- PY2_punts |>
            filter(posteam == VoA_df$team[x])
        temp_PY2_kicked_kicks <- PY2_kickoffs |>
            filter(defteam == VoA_df$team[x])
        temp_PY2_kicked_punt_TDs <- temp_PY2_kicked_punts |>
            filter(return_touchdown == 1)
        temp_PY2_kicked_kick_TDs <- temp_PY2_kicked_kicks |>
            filter(return_touchdown == 1)
        temp_PY2_off_xps <- PY2_XPts |>
            filter(posteam == VoA_df$team[x])
        temp_PY2_def_xps <- PY2_XPts |>
            filter(defteam == VoA_df$team[x])
        temp_PY2_off_good_xps <- temp_PY2_off_xps |>
            filter(extra_point_result == "good")
        temp_PY2_def_good_xps <- temp_PY2_def_xps |>
            filter(extra_point_result == "good")
        ### used to get net ST epa/play
        temp_PY2_off_st_plays <- rbind(
            temp_PY2_off_FGs,
            temp_PY2_off_xps,
            temp_PY2_returned_kicks,
            temp_PY2_returned_punts
        )
        temp_PY2_def_st_plays <- rbind(
            temp_PY2_def_FGs,
            temp_PY2_def_xps,
            temp_PY2_kicked_kicks,
            temp_PY2_kicked_punts
        )

        ### PY3 temp dfs
        ### temp PY3 offensive stat dfs
        temp_PY3_offplays <- PY3_rushpass_plays |>
            filter(posteam == VoA_df$team[x])
        temp_PY3_offsuccessplays <- PY3_success_plays |>
            filter(posteam == VoA_df$team[x])
        temp_PY3_offthirddowns <- PY3_3rdDowns |>
            filter(posteam == VoA_df$team[x]) |>
            drop_na(third_down_converted)
        temp_PY3_conv_offthirddowns <- temp_PY3_offthirddowns |>
            filter(third_down_converted == 1)
        temp_PY3_off_fourthdowns <- PY3_4thDowns |>
            filter(posteam == VoA_df$team[x]) |>
            drop_na(fourth_down_converted)
        temp_PY3_conv_offfourthdowns <- temp_PY3_off_fourthdowns |>
            filter(fourth_down_converted == 1)
        temp_PY3_off_passplays <- PY3_passplays |>
            filter(posteam == VoA_df$team[x])
        temp_PY3_off_comppass <- temp_PY3_off_passplays |>
            filter(complete_pass == 1)
        temp_PY3_off_rushplays <- PY3_rushplays |>
            filter(posteam == VoA_df$team[x])
        temp_PY3_off_scoringoppplays <- PY3_scoringopp_plays |>
            filter(posteam == VoA_df$team[x]) |>
            drop_na(drive)
        temp_PY3_off_scorringopp_TDs <- temp_PY3_off_scoringoppplays |>
            filter(touchdown == 1)
        temp_PY3_off_scorringopp_FGs <- temp_PY3_off_scoringoppplays |>
            filter(field_goal_result == "made")
        temp_PY3_off_turnovers <- PY3_Turnovers |>
            filter(posteam == VoA_df$team[x])
        temp_PY3_off_TDs <- PY3_TDs |>
            filter(posteam == VoA_df$team[x])
        temp_PY3_off_2pts <- PY3_2pts |>
            filter(
                posteam == VoA_df$team[x] &
                    two_point_conv_result == "success"
            )
        ### PY3 def stats
        temp_PY3_defplays <- PY3_rushpass_plays |>
            filter(defteam == VoA_df$team[x])
        temp_PY3_defsuccessplays <- PY3_success_plays |>
            filter(defteam == VoA_df$team[x])
        temp_PY3_defthirddowns <- PY3_3rdDowns |>
            filter(defteam == VoA_df$team[x]) |>
            drop_na(third_down_converted)
        temp_PY3_conv_defthirddowns <- temp_PY1_defthirddowns |>
            filter(third_down_converted == 1)
        temp_PY3_def_fourthdowns <- PY3_4thDowns |>
            filter(defteam == VoA_df$team[x]) |>
            drop_na(fourth_down_converted)
        temp_PY3_conv_deffourthdowns <- temp_PY3_off_fourthdowns |>
            filter(fourth_down_converted == 1)
        temp_PY3_def_passplays <- PY3_passplays |>
            filter(defteam == VoA_df$team[x])
        temp_PY3_def_comppass <- temp_PY3_def_passplays |>
            filter(complete_pass == 1)
        temp_PY3_def_rushplays <- PY3_rushplays |>
            filter(defteam == VoA_df$team[x])
        temp_PY3_def_scoringoppplays <- PY3_scoringopp_plays |>
            filter(defteam == VoA_df$team[x]) |>
            drop_na(drive)
        temp_PY3_def_scorringopp_TDs <- temp_PY3_def_scoringoppplays |>
            filter(touchdown == 1)
        temp_PY3_def_scorringopp_FGs <- temp_PY3_def_scoringoppplays |>
            filter(field_goal_result == "made")
        temp_PY3_def_turnovers <- PY3_Turnovers |>
            filter(defteam == VoA_df$team[x])
        temp_PY3_def_TDs <- PY3_TDs |>
            filter(defteam == VoA_df$team[x])
        temp_PY3_def_2pts <- PY3_2pts |>
            filter(
                defteam == VoA_df$team[x] &
                    two_point_conv_result == "success"
            )
        ### temp PY3 special teams dfs
        ## on kickoffs, defteam does kicking
        ## on punts, posteam does punting
        temp_PY3_off_FGs <- PY3_FGs |>
            filter(posteam == VoA_df$team[x])
        temp_PY3_off_goodFGs <- temp_PY3_off_FGs |>
            filter(field_goal_result == "made")
        temp_PY3_def_FGs <- PY3_FGs |>
            filter(
                defteam == VoA_df$team[x] & field_goal_result == "made"
            )
        temp_PY3_def_goodFGs <- temp_PY3_def_FGs |>
            filter(field_goal_result == "made")
        temp_PY3_returned_punts <- PY3_punts |>
            filter(defteam == VoA_df$team[x])
        temp_PY3_returned_kicks <- PY3_kickoffs |>
            filter(posteam == VoA_df$team[x])
        temp_PY3_returned_punt_TDs <- temp_PY3_returned_punts |>
            filter(return_touchdown == 1)
        temp_PY3_returned_kick_TDs <- temp_PY3_returned_kicks |>
            filter(return_touchdown == 1)
        temp_PY3_kicked_punts <- PY3_punts |>
            filter(posteam == VoA_df$team[x])
        temp_PY3_kicked_kicks <- PY3_kickoffs |>
            filter(defteam == VoA_df$team[x])
        temp_PY3_kicked_punt_TDs <- temp_PY3_kicked_punts |>
            filter(return_touchdown == 1)
        temp_PY3_kicked_kick_TDs <- temp_PY3_kicked_kicks |>
            filter(return_touchdown == 1)
        temp_PY3_off_xps <- PY3_XPts |>
            filter(posteam == VoA_df$team[x])
        temp_PY3_def_xps <- PY3_XPts |>
            filter(defteam == VoA_df$team[x])
        temp_PY3_off_good_xps <- temp_PY3_off_xps |>
            filter(extra_point_result == "good")
        temp_PY3_def_good_xps <- temp_PY3_def_xps |>
            filter(extra_point_result == "good")
        ### used to get net ST epa/play
        temp_PY3_off_st_plays <- rbind(
            temp_PY3_off_FGs,
            temp_PY3_off_xps,
            temp_PY3_returned_kicks,
            temp_PY3_returned_punts
        )
        temp_PY3_def_st_plays <- rbind(
            temp_PY3_def_FGs,
            temp_PY3_def_xps,
            temp_PY3_kicked_kicks,
            temp_PY3_kicked_punts
        )

        ### deriving stats from temp dfs
        ### PY1 stats
        VoA_df$off_ypp_PY1[x] <- mean(temp_PY1_offplays$yards_gained)
        VoA_df$off_epa_PY1[x] <- mean(temp_PY1_offplays$epa)
        VoA_df$off_success_rt_PY1[x] <- nrow(temp_PY1_offsuccessplays) /
            nrow(temp_PY1_offplays)
        VoA_df$off_explosiveness_PY1[x] <- mean(
            temp_PY1_offsuccessplays$epa
        )
        VoA_df$off_third_conv_rate_PY1[x] <- nrow(
            temp_PY1_conv_offthirddowns
        ) /
            nrow(temp_PY1_offthirddowns)
        VoA_df$off_fourth_conv_rate_PY1[x] <- nrow(
            temp_PY1_conv_offfourthdowns
        ) /
            nrow(temp_PY1_off_fourthdowns)
        VoA_df$off_pass_ypa_PY1[x] <- mean(
            temp_PY1_off_passplays$yards_gained
        )
        VoA_df$off_pass_ypc_PY1[x] <- mean(
            temp_PY1_off_comppass$yards_gained
        )
        VoA_df$off_rush_ypa_PY1[x] <- mean(
            temp_PY1_off_rushplays$yards_gained
        )
        VoA_df$off_pts_per_opp_PY1[x] <- ((nrow(
            temp_PY1_off_scorringopp_TDs
        ) *
            6) +
            (nrow(temp_PY1_off_scorringopp_FGs) * 3)) /
            length(unique(paste0(
                temp_PY1_off_scoringoppplays$game_id,
                temp_PY1_off_scoringoppplays$drive
            )))
        VoA_df$off_turnovers_PY1[x] <- nrow(temp_PY1_off_turnovers) /
            length(unique(temp_PY1_offplays$week))
        VoA_df$off_plays_pg_PY1[x] <- nrow(temp_PY1_offplays) /
            length(unique(temp_PY1_offplays$week))
        VoA_df$off_ppg_PY1[x] <- ((nrow(temp_PY1_off_TDs) * 6) +
            (nrow(temp_PY1_off_2pts) * 2)) /
            length(unique(temp_PY1_off_rushplays$week))
        ## PY1 defensive stats now
        VoA_df$def_ypp_PY1[x] <- mean(temp_PY1_defplays$yards_gained)
        VoA_df$def_epa_PY1[x] <- mean(temp_PY1_defplays$epa)
        VoA_df$def_success_rt_PY1[x] <- nrow(temp_PY1_defsuccessplays) /
            nrow(temp_PY1_defplays)
        VoA_df$def_explosiveness_PY1[x] <- mean(
            temp_PY1_defsuccessplays$epa
        )
        VoA_df$def_third_conv_rate_PY1[x] <- nrow(
            temp_PY1_conv_defthirddowns
        ) /
            nrow(temp_PY1_defthirddowns)
        VoA_df$def_fourth_conv_rate_PY1[x] <- nrow(
            temp_PY1_conv_deffourthdowns
        ) /
            nrow(temp_PY1_def_fourthdowns)
        VoA_df$def_pass_ypa_PY1[x] <- mean(
            temp_PY1_def_passplays$yards_gained
        )
        VoA_df$def_pass_ypc_PY1[x] <- mean(
            temp_PY1_def_comppass$yards_gained
        )
        VoA_df$def_rush_ypa_PY1[x] <- mean(
            temp_PY1_def_rushplays$yards_gained
        )
        VoA_df$def_pts_per_opp_PY1[x] <- ((nrow(
            temp_PY1_def_scorringopp_TDs
        ) *
            6) +
            (nrow(temp_PY1_def_scorringopp_FGs) * 3)) /
            length(unique(paste0(
                temp_PY1_def_scoringoppplays$game_id,
                temp_PY1_def_scoringoppplays$drive
            )))
        VoA_df$def_turnovers_PY1[x] <- nrow(temp_PY1_def_turnovers) /
            length(unique(temp_PY1_defplays$week))
        VoA_df$def_plays_pg_PY1[x] <- nrow(temp_PY1_defplays) /
            length(unique(temp_PY1_defplays$week))
        VoA_df$def_ppg_PY1[x] <- ((nrow(temp_PY1_def_TDs) * 6) +
            (nrow(temp_PY1_def_2pts) * 2)) /
            length(unique(temp_PY1_def_rushplays$week))
        ## PY1 Special teams stats now
        VoA_df$st_net_epa_PY1[x] <- mean(temp_PY1_off_st_plays$epa) -
            mean(temp_PY1_def_st_plays$epa)
        VoA_df$st_punt_return_yds_PY1[x] <- mean(
            temp_PY1_returned_punts$return_yards
        )
        VoA_df$st_kick_return_yds_PY1[x] <- mean(
            temp_PY1_returned_kicks$return_yards
        )
        VoA_df$st_kick_return_TDs_PY1[x] <- nrow(
            temp_PY1_returned_kick_TDs
        ) /
            length(unique(temp_PY1_offplays$week))
        VoA_df$st_punt_return_TDs_PY1[x] <- nrow(
            temp_PY1_returned_punt_TDs
        ) /
            length(unique(temp_PY1_offplays$week))
        VoA_df$fg_rate_PY1[x] <- nrow(temp_PY1_off_goodFGs) /
            nrow(temp_PY1_off_FGs)
        VoA_df$fg_made_pg_PY1[x] <- nrow(temp_PY1_off_goodFGs) /
            length(unique(temp_PY1_offplays$week))
        VoA_df$xp_rate_PY1[x] <- nrow(temp_PY1_off_good_xps) /
            nrow(temp_PY1_off_xps)
        VoA_df$xp_made_pg_PY1[x] <- nrow(temp_PY1_off_good_xps) /
            length(unique(temp_PY1_offplays$week))
        VoA_df$st_punt_return_yds_allowed_PY1[x] <- mean(
            temp_PY1_kicked_punts$return_yards
        )
        VoA_df$st_kick_return_yds_allowed_PY1[x] <- mean(
            temp_PY1_kicked_kicks$return_yards
        )
        VoA_df$st_kick_return_TDs_allowed_PY1[x] <- nrow(
            temp_PY1_kicked_kick_TDs
        ) /
            length(unique(temp_PY1_offplays$week))
        VoA_df$st_punt_return_TDs_allowed_PY1[x] <- nrow(
            temp_PY1_kicked_punt_TDs
        ) /
            length(unique(temp_PY1_offplays$week))
        VoA_df$fg_rate_allowed_PY1[x] <- nrow(temp_PY1_def_goodFGs) /
            nrow(temp_PY1_def_FGs)
        VoA_df$fg_made_pg_allowed_PY1[x] <- nrow(temp_PY1_def_goodFGs) /
            length(unique(temp_PY1_offplays$week))
        VoA_df$xp_rate_allowed_PY1[x] <- nrow(temp_PY1_def_good_xps) /
            nrow(temp_PY1_def_xps)
        VoA_df$xp_made_pg_allowed_PY1[x] <- nrow(temp_PY1_def_good_xps) /
            length(unique(temp_PY1_offplays$week))
        VoA_df$net_st_ppg_PY1[x] <- (((nrow(temp_PY1_off_goodFGs) * 3) +
            (nrow(temp_PY1_returned_punt_TDs) * 6) +
            (nrow(temp_PY1_returned_kick_TDs) * 6) +
            nrow(temp_PY1_off_good_xps)) -
            ((nrow(temp_PY1_def_goodFGs) * 3) +
                (nrow(temp_PY1_kicked_punt_TDs) * 6) +
                (nrow(temp_PY1_kicked_kick_TDs) * 6) +
                nrow(temp_PY1_def_good_xps))) /
            length(unique(temp_PY1_offplays$week))

        ### evaluating PY2 variables
        VoA_df$off_ypp_PY2[x] <- mean(temp_PY2_offplays$yards_gained)
        VoA_df$off_epa_PY2[x] <- mean(temp_PY2_offplays$epa)
        VoA_df$off_success_rt_PY2[x] <- nrow(temp_PY2_offsuccessplays) /
            nrow(temp_PY2_offplays)
        VoA_df$off_explosiveness_PY2[x] <- mean(
            temp_PY2_offsuccessplays$epa
        )
        VoA_df$off_third_conv_rate_PY2[x] <- nrow(
            temp_PY2_conv_offthirddowns
        ) /
            nrow(temp_PY2_offthirddowns)
        VoA_df$off_fourth_conv_rate_PY2[x] <- nrow(
            temp_PY2_conv_offfourthdowns
        ) /
            nrow(temp_PY2_off_fourthdowns)
        VoA_df$off_pass_ypa_PY2[x] <- mean(
            temp_PY2_off_passplays$yards_gained
        )
        VoA_df$off_pass_ypc_PY2[x] <- mean(
            temp_PY2_off_comppass$yards_gained
        )
        VoA_df$off_rush_ypa_PY2[x] <- mean(
            temp_PY2_off_rushplays$yards_gained
        )
        VoA_df$off_pts_per_opp_PY2[x] <- ((nrow(
            temp_PY2_off_scorringopp_TDs
        ) *
            6) +
            (nrow(temp_PY2_off_scorringopp_FGs) * 3)) /
            length(unique(paste0(
                temp_PY2_off_scoringoppplays$game_id,
                temp_PY2_off_scoringoppplays$drive
            )))
        VoA_df$off_turnovers_PY2[x] <- nrow(temp_PY2_off_turnovers) /
            length(unique(temp_PY2_offplays$week))
        VoA_df$off_plays_pg_PY2[x] <- nrow(temp_PY2_offplays) /
            length(unique(temp_PY2_offplays$week))
        VoA_df$off_ppg_PY2[x] <- ((nrow(temp_PY2_off_TDs) * 6) +
            (nrow(temp_PY2_off_2pts) * 2)) /
            length(unique(temp_PY2_off_rushplays$week))
        ## PY2 defensive stats now
        VoA_df$def_ypp_PY2[x] <- mean(temp_PY2_defplays$yards_gained)
        VoA_df$def_epa_PY2[x] <- mean(temp_PY2_defplays$epa)
        VoA_df$def_success_rt_PY2[x] <- nrow(temp_PY2_defsuccessplays) /
            nrow(temp_PY2_defplays)
        VoA_df$def_explosiveness_PY2[x] <- mean(
            temp_PY2_defsuccessplays$epa
        )
        VoA_df$def_third_conv_rate_PY2[x] <- nrow(
            temp_PY2_conv_defthirddowns
        ) /
            nrow(temp_PY2_defthirddowns)
        VoA_df$def_fourth_conv_rate_PY2[x] <- nrow(
            temp_PY2_conv_deffourthdowns
        ) /
            nrow(temp_PY2_def_fourthdowns)
        VoA_df$def_pass_ypa_PY2[x] <- mean(
            temp_PY2_def_passplays$yards_gained
        )
        VoA_df$def_pass_ypc_PY2[x] <- mean(
            temp_PY2_def_comppass$yards_gained
        )
        VoA_df$def_rush_ypa_PY2[x] <- mean(
            temp_PY2_def_rushplays$yards_gained
        )
        VoA_df$def_pts_per_opp_PY2[x] <- ((nrow(
            temp_PY2_def_scorringopp_TDs
        ) *
            6) +
            (nrow(temp_PY2_def_scorringopp_FGs) * 3)) /
            length(unique(paste0(
                temp_PY2_def_scoringoppplays$game_id,
                temp_PY2_def_scoringoppplays$drive
            )))
        VoA_df$def_turnovers_PY2[x] <- nrow(temp_PY2_def_turnovers) /
            length(unique(temp_PY2_defplays$week))
        VoA_df$def_plays_pg_PY2[x] <- nrow(temp_PY2_defplays) /
            length(unique(temp_PY2_defplays$week))
        VoA_df$def_ppg_PY2[x] <- ((nrow(temp_PY2_def_TDs) * 6) +
            (nrow(temp_PY2_def_2pts) * 2)) /
            length(unique(temp_PY2_def_rushplays$week))
        ## PY2 Special teams stats now
        VoA_df$st_net_epa_PY2[x] <- mean(temp_PY2_off_st_plays$epa) -
            mean(temp_PY2_def_st_plays$epa)
        VoA_df$st_punt_return_yds_PY2[x] <- mean(
            temp_PY2_returned_punts$return_yards
        )
        VoA_df$st_kick_return_yds_PY2[x] <- mean(
            temp_PY2_returned_kicks$return_yards
        )
        VoA_df$st_kick_return_TDs_PY2[x] <- nrow(
            temp_PY2_returned_kick_TDs
        ) /
            length(unique(temp_PY2_offplays$week))
        VoA_df$st_punt_return_TDs_PY2[x] <- nrow(
            temp_PY2_returned_punt_TDs
        ) /
            length(unique(temp_PY2_offplays$week))
        VoA_df$fg_rate_PY2[x] <- nrow(temp_PY2_off_goodFGs) /
            nrow(temp_PY2_off_FGs)
        VoA_df$fg_made_pg_PY2[x] <- nrow(temp_PY2_off_goodFGs) /
            length(unique(temp_PY2_offplays$week))
        VoA_df$xp_rate_PY2[x] <- nrow(temp_PY2_off_good_xps) /
            nrow(temp_PY2_off_xps)
        VoA_df$xp_made_pg_PY2[x] <- nrow(temp_PY2_off_good_xps) /
            length(unique(temp_PY2_offplays$week))
        VoA_df$st_punt_return_yds_allowed_PY2[x] <- mean(
            temp_PY2_kicked_punts$return_yards
        )
        VoA_df$st_kick_return_yds_allowed_PY2[x] <- mean(
            temp_PY2_kicked_kicks$return_yards
        )
        VoA_df$st_kick_return_TDs_allowed_PY2[x] <- nrow(
            temp_PY2_kicked_kick_TDs
        ) /
            length(unique(temp_PY2_offplays$week))
        VoA_df$st_punt_return_TDs_allowed_PY2[x] <- nrow(
            temp_PY2_kicked_punt_TDs
        ) /
            length(unique(temp_PY2_offplays$week))
        VoA_df$fg_rate_allowed_PY2[x] <- nrow(temp_PY2_def_goodFGs) /
            nrow(temp_PY2_def_FGs)
        VoA_df$fg_made_pg_allowed_PY2[x] <- nrow(temp_PY2_def_goodFGs) /
            length(unique(temp_PY2_offplays$week))
        VoA_df$xp_rate_allowed_PY2[x] <- nrow(temp_PY2_def_good_xps) /
            nrow(temp_PY2_def_xps)
        VoA_df$xp_made_pg_allowed_PY2[x] <- nrow(temp_PY2_def_good_xps) /
            length(unique(temp_PY2_offplays$week))
        VoA_df$net_st_ppg_PY2[x] <- (((nrow(temp_PY2_off_goodFGs) * 3) +
            (nrow(temp_PY2_returned_punt_TDs) * 6) +
            (nrow(temp_PY2_returned_kick_TDs) * 6) +
            nrow(temp_PY2_off_good_xps)) -
            ((nrow(temp_PY2_def_goodFGs) * 3) +
                (nrow(temp_PY2_kicked_punt_TDs) * 6) +
                (nrow(temp_PY2_kicked_kick_TDs) * 6) +
                nrow(temp_PY2_def_good_xps))) /
            length(unique(temp_PY2_offplays$week))

        ### evaluating PY3 variables
        VoA_df$off_ypp_PY3[x] <- mean(temp_PY3_offplays$yards_gained)
        VoA_df$off_epa_PY3[x] <- mean(temp_PY3_offplays$epa)
        VoA_df$off_success_rt_PY3[x] <- nrow(temp_PY3_offsuccessplays) /
            nrow(temp_PY3_offplays)
        VoA_df$off_explosiveness_PY3[x] <- mean(
            temp_PY3_offsuccessplays$epa
        )
        VoA_df$off_third_conv_rate_PY3[x] <- nrow(
            temp_PY3_conv_offthirddowns
        ) /
            nrow(temp_PY3_offthirddowns)
        VoA_df$off_fourth_conv_rate_PY3[x] <- nrow(
            temp_PY3_conv_offfourthdowns
        ) /
            nrow(temp_PY3_off_fourthdowns)
        VoA_df$off_pass_ypa_PY3[x] <- mean(
            temp_PY3_off_passplays$yards_gained
        )
        VoA_df$off_pass_ypc_PY3[x] <- mean(
            temp_PY3_off_comppass$yards_gained
        )
        VoA_df$off_rush_ypa_PY3[x] <- mean(
            temp_PY3_off_rushplays$yards_gained
        )
        VoA_df$off_pts_per_opp_PY3[x] <- ((nrow(
            temp_PY3_off_scorringopp_TDs
        ) *
            6) +
            (nrow(temp_PY3_off_scorringopp_FGs) * 3)) /
            length(unique(paste0(
                temp_PY3_off_scoringoppplays$game_id,
                temp_PY3_off_scoringoppplays$drive
            )))
        VoA_df$off_turnovers_PY3[x] <- nrow(temp_PY3_off_turnovers) /
            length(unique(temp_PY3_offplays$week))
        VoA_df$off_plays_pg_PY3[x] <- nrow(temp_PY3_offplays) /
            length(unique(temp_PY3_offplays$week))
        VoA_df$off_ppg_PY3[x] <- ((nrow(temp_PY3_off_TDs) * 6) +
            (nrow(temp_PY3_off_2pts) * 2)) /
            length(unique(temp_PY3_off_rushplays$week))
        ## PY3 defensive stats now
        VoA_df$def_ypp_PY3[x] <- mean(temp_PY3_defplays$yards_gained)
        VoA_df$def_epa_PY3[x] <- mean(temp_PY3_defplays$epa)
        VoA_df$def_success_rt_PY3[x] <- nrow(temp_PY3_defsuccessplays) /
            nrow(temp_PY3_defplays)
        VoA_df$def_explosiveness_PY3[x] <- mean(
            temp_PY3_defsuccessplays$epa
        )
        VoA_df$def_third_conv_rate_PY3[x] <- nrow(
            temp_PY3_conv_defthirddowns
        ) /
            nrow(temp_PY3_defthirddowns)
        VoA_df$def_fourth_conv_rate_PY3[x] <- nrow(
            temp_PY3_conv_deffourthdowns
        ) /
            nrow(temp_PY3_def_fourthdowns)
        VoA_df$def_pass_ypa_PY3[x] <- mean(
            temp_PY3_def_passplays$yards_gained
        )
        VoA_df$def_pass_ypc_PY3[x] <- mean(
            temp_PY3_def_comppass$yards_gained
        )
        VoA_df$def_rush_ypa_PY3[x] <- mean(
            temp_PY3_def_rushplays$yards_gained
        )
        VoA_df$def_pts_per_opp_PY3[x] <- ((nrow(
            temp_PY3_def_scorringopp_TDs
        ) *
            6) +
            (nrow(temp_PY3_def_scorringopp_FGs) * 3)) /
            length(unique(paste0(
                temp_PY3_def_scoringoppplays$game_id,
                temp_PY3_def_scoringoppplays$drive
            )))
        VoA_df$def_turnovers_PY3[x] <- nrow(temp_PY3_def_turnovers) /
            length(unique(temp_PY3_defplays$week))
        VoA_df$def_plays_pg_PY3[x] <- nrow(temp_PY3_defplays) /
            length(unique(temp_PY3_defplays$week))
        VoA_df$def_ppg_PY3[x] <- ((nrow(temp_PY3_def_TDs) * 6) +
            (nrow(temp_PY3_def_2pts) * 2)) /
            length(unique(temp_PY3_def_rushplays$week))
        ## PY3 Special teams stats now
        VoA_df$st_net_epa_PY3[x] <- mean(temp_PY3_off_st_plays$epa) -
            mean(temp_PY3_def_st_plays$epa)
        VoA_df$st_punt_return_yds_PY3[x] <- mean(
            temp_PY3_returned_punts$return_yards
        )
        VoA_df$st_kick_return_yds_PY3[x] <- mean(
            temp_PY3_returned_kicks$return_yards
        )
        VoA_df$st_kick_return_TDs_PY3[x] <- nrow(
            temp_PY3_returned_kick_TDs
        ) /
            length(unique(temp_PY3_offplays$week))
        VoA_df$st_punt_return_TDs_PY3[x] <- nrow(
            temp_PY3_returned_punt_TDs
        ) /
            length(unique(temp_PY3_offplays$week))
        VoA_df$fg_rate_PY3[x] <- nrow(temp_PY3_off_goodFGs) /
            nrow(temp_PY3_off_FGs)
        VoA_df$fg_made_pg_PY3[x] <- nrow(temp_PY3_off_goodFGs) /
            length(unique(temp_PY3_offplays$week))
        VoA_df$xp_rate_PY3[x] <- nrow(temp_PY3_off_good_xps) /
            nrow(temp_PY3_off_xps)
        VoA_df$xp_made_pg_PY3[x] <- nrow(temp_PY3_off_good_xps) /
            length(unique(temp_PY3_offplays$week))
        VoA_df$st_punt_return_yds_allowed_PY3[x] <- mean(
            temp_PY3_kicked_punts$return_yards
        )
        VoA_df$st_kick_return_yds_allowed_PY3[x] <- mean(
            temp_PY3_kicked_kicks$return_yards
        )
        VoA_df$st_kick_return_TDs_allowed_PY3[x] <- nrow(
            temp_PY3_kicked_kick_TDs
        ) /
            length(unique(temp_PY3_offplays$week))
        VoA_df$st_punt_return_TDs_allowed_PY3[x] <- nrow(
            temp_PY3_kicked_punt_TDs
        ) /
            length(unique(temp_PY3_offplays$week))
        VoA_df$fg_rate_allowed_PY3[x] <- nrow(temp_PY3_def_goodFGs) /
            nrow(temp_PY3_def_FGs)
        VoA_df$fg_made_pg_allowed_PY3[x] <- nrow(temp_PY3_def_goodFGs) /
            length(unique(temp_PY3_offplays$week))
        VoA_df$xp_rate_allowed_PY3[x] <- nrow(temp_PY3_def_good_xps) /
            nrow(temp_PY3_def_xps)
        VoA_df$xp_made_pg_allowed_PY3[x] <- nrow(temp_PY3_def_good_xps) /
            length(unique(temp_PY3_offplays$week))
        VoA_df$net_st_ppg_PY3[x] <- (((nrow(temp_PY3_off_goodFGs) * 3) +
            (nrow(temp_PY3_returned_punt_TDs) * 6) +
            (nrow(temp_PY3_returned_kick_TDs) * 6) +
            nrow(temp_PY3_off_good_xps)) -
            ((nrow(temp_PY3_def_goodFGs) * 3) +
                (nrow(temp_PY3_kicked_punt_TDs) * 6) +
                (nrow(temp_PY3_kicked_kick_TDs) * 6) +
                nrow(temp_PY3_def_good_xps))) /
            length(unique(temp_PY3_offplays$week))
    }

    ### PY1 Adjusted Stats
    ### Creating opponent-adjusted stats
    ### EPA/play
    ### subsetting columns for epa/play adjustment
    PBP_EPAAdjustment_PY1 <- PY1_rushpass_plays |>
        select(game_id, home_team, posteam, defteam, epa, location) |>
        mutate(
            hfa = as.factor(case_when(
                location == "Neutral" ~ 0,
                ### home team on offense
                posteam == home_team ~ 1,
                ### home team on defense
                TRUE ~ -1
            )),
            posteam = as.factor(posteam),
            defteam = as.factor(defteam)
        ) |>
        drop_na()

    ### fitting mixed effects model, treating posessing team and defensive team as random effects
    set.seed(802)
    epa_mixed_model <- lmer(
        epa ~ hfa + (1 | posteam) + (1 | defteam),
        data = PBP_EPAAdjustment_PY1
    )

    ### Extract random effects (team adjustments)
    team_effects <- ranef(epa_mixed_model)

    ### Extract offensive adjustments
    off_adj <- as.data.frame(team_effects$posteam) |>
        rename(adj_off_epa_PY1 = `(Intercept)`) |>
        mutate(team = rownames(team_effects$posteam))

    ### extract defensive adjustment
    def_adj <- as.data.frame(team_effects$defteam) |>
        rename(adj_def_epa_PY1 = `(Intercept)`) |>
        mutate(team = rownames(team_effects$defteam))

    ### average EPA (model intercept)
    avg_epa <- fixef(epa_mixed_model)["(Intercept)"]

    ### combine and join back to VoA_df
    VoA_df <- VoA_df |>
        left_join(off_adj, by = "team") |>
        left_join(def_adj, by = "team") |>
        mutate(
            adj_off_epa_PY1 = adj_off_epa_PY1 + avg_epa,
            adj_def_epa_PY1 = adj_def_epa_PY1 + avg_epa
        )

    ### opponent adjusted plays per game
    ### uses the same pbp dataset as the epa adjustment above
    PlaysPG_Adjustment_PY1 <- PBP_EPAAdjustment_PY1 |>
        group_by(game_id) |>
        summarize(
            home_off_plays = sum(posteam == home_team),
            away_off_plays = sum(posteam == away_team),
            home_team = as.factor(unique(home_team)[1]),
            away_team = as.factor(unique(away_team)[1]),
            location = unique(location)[1]
        ) |>
        pivot_longer(
            cols = ends_with("_plays"),
            names_to = "home_away_col_names",
            values_to = "team_plays"
        ) |>
        mutate(
            team = case_when(
                home_away_col_names == "home_off_plays" ~ home_team,
                TRUE ~ away_team
            ),
            opp_team = case_when(
                home_away_col_names == "home_off_plays" ~ away_team,
                TRUE ~ home_team
            ),
            hfa = as.factor(case_when(
                location == "Neutral" ~ 0,
                home_team == team ~ 1,
                TRUE ~ -1
            ))
        )

    ### fitting mixed effects model, treating team and opposing team as random effects
    set.seed(802)
    plays_mixed_model <- lmer(
        team_plays ~ hfa + (1 | team) + (1 | opp_team),
        data = PlaysPG_Adjustment_PY1
    )

    ### Extract random effects (team adjustments)
    team_effects <- ranef(plays_mixed_model)

    ### Extract offensive adjustments
    off_adj <- as.data.frame(team_effects$team) |>
        rename(adj_off_plays_pg_PY1 = `(Intercept)`) |>
        mutate(team = rownames(team_effects$team))

    ### extract defensive adjustment
    def_adj <- as.data.frame(team_effects$opp_team) |>
        rename(adj_def_plays_pg_PY1 = `(Intercept)`) |>
        mutate(team = rownames(team_effects$opp_team))

    ### average plays per game (model intercept)
    avg_plays_pg <- fixef(plays_mixed_model)["(Intercept)"]

    ### combine and join back to VoA_df
    VoA_df <- VoA_df |>
        left_join(off_adj, by = "team") |>
        left_join(def_adj, by = "team") |>
        mutate(
            adj_off_plays_pg_PY1 = adj_off_plays_pg_PY1 + avg_plays_pg,
            adj_def_plays_pg_PY1 = adj_def_plays_pg_PY1 + avg_plays_pg
        )

    ### Explosiveness
    ### subsetting columns for epa/play (explosiveness, so only EPA/play on successful plays) adjustment
    PBP_ExpAdjustment_PY1 <- PY1_success_plays |>
        select(game_id, home_team, posteam, defteam, epa, location) |>
        mutate(
            hfa = as.factor(case_when(
                location == "Neutral" ~ 0,
                ### home team on offense
                posteam == home_team ~ 1,
                ### home team on defense
                TRUE ~ -1
            )),
            posteam = as.factor(posteam),
            defteam = as.factor(defteam)
        ) |>
        drop_na()

    ### fitting mixed effects model, treating posessing team and defensive team as random effects
    set.seed(802)
    exp_mixed_model <- lmer(
        epa ~ hfa + (1 | posteam) + (1 | defteam),
        data = PBP_ExpAdjustment_PY1
    )

    ### Extract random effects (team adjustments)
    team_effects <- ranef(exp_mixed_model)

    ### Extract offensive adjustments
    off_adj <- as.data.frame(team_effects$posteam) |>
        rename(adj_off_explosiveness_PY1 = `(Intercept)`) |>
        mutate(team = rownames(team_effects$posteam))

    ### extract defensive adjustment
    def_adj <- as.data.frame(team_effects$defteam) |>
        rename(adj_def_explosiveness_PY1 = `(Intercept)`) |>
        mutate(team = rownames(team_effects$defteam))

    ### average EPA (model intercept)
    avg_explosiveness <- fixef(exp_mixed_model)["(Intercept)"]

    ### combine and join back to VoA_df
    VoA_df <- VoA_df |>
        left_join(off_adj, by = "team") |>
        left_join(def_adj, by = "team") |>
        mutate(
            adj_off_explosiveness_PY1 = adj_off_explosiveness_PY1 +
                avg_explosiveness,
            adj_def_explosiveness_PY1 = adj_def_explosiveness_PY1 +
                avg_explosivensss
        )

    ### ppg
    ## this will initially give me pts/play, then I will multiply it by off/def plays per game when binding to VoA_df
    ### subsetting columns for pts/play adjustment
    PBP_PPGAdjustment_PY1 <- PY1_rushpass_plays |>
        select(
            game_id,
            home_team,
            posteam,
            defteam,
            two_point_conv_result,
            pass_touchdown,
            rush_touchdown,
            location
        ) |>
        mutate(
            hfa = as.factor(case_when(
                location == "Neutral" ~ 0,
                ### home team on offense
                posteam == home_team ~ 1,
                ### home team on defense
                TRUE ~ -1
            )),
            play_pts_scored = case_when(
                two_point_conv_result == "success" ~ 2,
                pass_touchdown == 1 ~ 6,
                rush_touchdown == 1 ~ 6,
                TRUE ~ 0
            ),
            posteam = as.factor(posteam),
            defteam = as.factor(defteam)
        ) |>
        drop_na(game_id, home_team, posteam, defteam, hfa, location)

    ### fitting mixed effects model, treating posessing team and defensive team as random effects
    set.seed(802)
    ppg_mixed_model <- lmer(
        play_pts_scored ~ hfa + (1 | posteam) + (1 | defteam),
        data = PBP_PPGAdjustment_PY1
    )

    ### Extract random effects (team adjustments)
    team_effects <- ranef(ppg_mixed_model)

    ### average EPA (model intercept)
    avg_ppp <- fixef(ppg_mixed_model)["(Intercept)"]

    ### Extract offensive adjustments
    off_adj <- as.data.frame(team_effects$posteam) |>
        rename(adj_off_pts_per_play_PY1 = `(Intercept)`) |>
        mutate(team = rownames(team_effects$posteam)) |>
        mutate(adj_off_pts_per_play_PY1 = adj_off_pts_per_play_PY1 + avg_ppp)

    ### extract defensive adjustment
    def_adj <- as.data.frame(team_effects$defteam) |>
        rename(adj_def_pts_per_play_PY1 = `(Intercept)`) |>
        mutate(team = rownames(team_effects$defteam)) |>
        mutate(adj_def_pts_per_play_PY1 = adj_def_pts_per_play_PY1 + avg_ppp)

    ### combine and join back to VoA_df
    VoA_df <- VoA_df |>
        left_join(off_adj, by = "team") |>
        left_join(def_adj, by = "team") |>
        mutate(
            adj_off_ppg_PY1 = adj_off_pts_per_play_PY1 *
                mean(adj_off_plays_pg) *
                1.5,
            adj_def_pp_PY1g = adj_def_pts_per_play_PY1 *
                mean(adj_def_plays_pg) *
                1.5
        )

    ### yards/play opponent adjustment
    ### subsetting columns for adjustment
    PBP_YPPAdjustment_PY1 <- PY1_rushpass_plays |>
        select(game_id, home_team, posteam, defteam, yards_gained, location) |>
        mutate(
            hfa = as.factor(case_when(
                location == "Neutral" ~ 0,
                ### home team on offense
                posteam == home_team ~ 1,
                ### home team on defense
                TRUE ~ -1
            )),
            posteam = as.factor(posteam),
            defteam = as.factor(defteam)
        ) |>
        drop_na()

    ### fitting mixed effects model, treating posessing team and defensive team as random effects
    set.seed(802)
    ypp_mixed_model <- lmer(
        yards_gained ~ hfa + (1 | posteam) + (1 | defteam),
        data = PBP_YPPAdjustment_PY1
    )

    ### Extract random effects (team adjustments)
    team_effects <- ranef(ypp_mixed_model)

    ### Extract offensive adjustments
    off_adj <- as.data.frame(team_effects$posteam) |>
        rename(adj_off_ypp_PY1 = `(Intercept)`) |>
        mutate(team = rownames(team_effects$posteam))

    ### extract defensive adjustment
    def_adj <- as.data.frame(team_effects$defteam) |>
        rename(adj_def_ypp_PY1 = `(Intercept)`) |>
        mutate(team = rownames(team_effects$defteam))

    ### average EPA (model intercept)
    avg_ypp <- fixef(ypp_mixed_model)["(Intercept)"]

    ### combine and join back to VoA_df
    VoA_df <- VoA_df |>
        left_join(off_adj, by = "team") |>
        left_join(def_adj, by = "team") |>
        mutate(
            adj_off_ypp_PY1 = adj_off_ypp_PY1 + avg_ypp,
            adj_def_ypp_PY1 = adj_def_ypp_PY1 + avg_ypp
        )

    ### PY2 Adjusted Stats
    ### Creating opponent-adjusted stats
    ### EPA/play
    ### subsetting columns for epa/play adjustment
    PBP_EPAAdjustment_PY2 <- PY2_rushpass_plays |>
        select(game_id, home_team, posteam, defteam, epa, location) |>
        mutate(
            hfa = as.factor(case_when(
                location == "Neutral" ~ 0,
                ### home team on offense
                posteam == home_team ~ 1,
                ### home team on defense
                TRUE ~ -1
            )),
            posteam = as.factor(posteam),
            defteam = as.factor(defteam)
        ) |>
        drop_na()

    ### fitting mixed effects model, treating posessing team and defensive team as random effects
    set.seed(802)
    epa_mixed_model <- lmer(
        epa ~ hfa + (1 | posteam) + (1 | defteam),
        data = PBP_EPAAdjustment_PY2
    )

    ### Extract random effects (team adjustments)
    team_effects <- ranef(epa_mixed_model)

    ### Extract offensive adjustments
    off_adj <- as.data.frame(team_effects$posteam) |>
        rename(adj_off_epa_PY2 = `(Intercept)`) |>
        mutate(team = rownames(team_effects$posteam))

    ### extract defensive adjustment
    def_adj <- as.data.frame(team_effects$defteam) |>
        rename(adj_def_epa_PY2 = `(Intercept)`) |>
        mutate(team = rownames(team_effects$defteam))

    ### average EPA (model intercept)
    avg_epa <- fixef(epa_mixed_model)["(Intercept)"]

    ### combine and join back to VoA_df
    VoA_df <- VoA_df |>
        left_join(off_adj, by = "team") |>
        left_join(def_adj, by = "team") |>
        mutate(
            adj_off_epa_PY2 = adj_off_epa_PY2 + avg_epa,
            adj_def_epa_PY2 = adj_def_epa_PY2 + avg_epa
        )

    ### opponent adjusted plays per game
    ### uses the same pbp dataset as the epa adjustment above
    PlaysPG_Adjustment_PY2 <- PBP_EPAAdjustment_PY2 |>
        group_by(game_id) |>
        summarize(
            home_off_plays = sum(posteam == home_team),
            away_off_plays = sum(posteam == away_team),
            home_team = as.factor(unique(home_team)[1]),
            away_team = as.factor(unique(away_team)[1]),
            location = unique(location)[1]
        ) |>
        pivot_longer(
            cols = ends_with("_plays"),
            names_to = "home_away_col_names",
            values_to = "team_plays"
        ) |>
        mutate(
            team = case_when(
                home_away_col_names == "home_off_plays" ~ home_team,
                TRUE ~ away_team
            ),
            opp_team = case_when(
                home_away_col_names == "home_off_plays" ~ away_team,
                TRUE ~ home_team
            ),
            hfa = as.factor(case_when(
                location == "Neutral" ~ 0,
                home_team == team ~ 1,
                TRUE ~ -1
            ))
        )

    ### fitting mixed effects model, treating team and opposing team as random effects
    set.seed(802)
    plays_mixed_model <- lmer(
        team_plays ~ hfa + (1 | team) + (1 | opp_team),
        data = PlaysPG_Adjustment_PY2
    )

    ### Extract random effects (team adjustments)
    team_effects <- ranef(plays_mixed_model)

    ### Extract offensive adjustments
    off_adj <- as.data.frame(team_effects$team) |>
        rename(adj_off_plays_pg_PY2 = `(Intercept)`) |>
        mutate(team = rownames(team_effects$team))

    ### extract defensive adjustment
    def_adj <- as.data.frame(team_effects$opp_team) |>
        rename(adj_def_plays_pg_PY2 = `(Intercept)`) |>
        mutate(team = rownames(team_effects$opp_team))

    ### average plays per game (model intercept)
    avg_plays_pg <- fixef(plays_mixed_model)["(Intercept)"]

    ### combine and join back to VoA_df
    VoA_df <- VoA_df |>
        left_join(off_adj, by = "team") |>
        left_join(def_adj, by = "team") |>
        mutate(
            adj_off_plays_pg_PY2 = adj_off_plays_pg_PY2 + avg_plays_pg,
            adj_def_plays_pg_PY2 = adj_def_plays_pg_PY2 + avg_plays_pg
        )

    ### Explosiveness
    ### subsetting columns for epa/play (explosiveness, so only EPA/play on successful plays) adjustment
    PBP_ExpAdjustment_PY2 <- PY2_success_plays |>
        select(game_id, home_team, posteam, defteam, epa, location) |>
        mutate(
            hfa = as.factor(case_when(
                location == "Neutral" ~ 0,
                ### home team on offense
                posteam == home_team ~ 1,
                ### home team on defense
                TRUE ~ -1
            )),
            posteam = as.factor(posteam),
            defteam = as.factor(defteam)
        ) |>
        drop_na()

    ### fitting mixed effects model, treating posessing team and defensive team as random effects
    set.seed(802)
    exp_mixed_model <- lmer(
        epa ~ hfa + (1 | posteam) + (1 | defteam),
        data = PBP_ExpAdjustment_PY2
    )

    ### Extract random effects (team adjustments)
    team_effects <- ranef(exp_mixed_model)

    ### Extract offensive adjustments
    off_adj <- as.data.frame(team_effects$posteam) |>
        rename(adj_off_explosiveness_PY2 = `(Intercept)`) |>
        mutate(team = rownames(team_effects$posteam))

    ### extract defensive adjustment
    def_adj <- as.data.frame(team_effects$defteam) |>
        rename(adj_def_explosiveness_PY2 = `(Intercept)`) |>
        mutate(team = rownames(team_effects$defteam))

    ### average EPA (model intercept)
    avg_explosiveness <- fixef(exp_mixed_model)["(Intercept)"]

    ### combine and join back to VoA_df
    VoA_df <- VoA_df |>
        left_join(off_adj, by = "team") |>
        left_join(def_adj, by = "team") |>
        mutate(
            adj_off_explosiveness_PY2 = adj_off_explosiveness_PY2 +
                avg_explosiveness,
            adj_def_explosiveness_PY2 = adj_def_explosiveness_PY2 +
                avg_explosivensss
        )

    ### ppg
    ## this will initially give me pts/play, then I will multiply it by off/def plays per game when binding to VoA_df
    ### subsetting columns for pts/play adjustment
    PBP_PPGAdjustment_PY2 <- PY2_rushpass_plays |>
        select(
            game_id,
            home_team,
            posteam,
            defteam,
            two_point_conv_result,
            pass_touchdown,
            rush_touchdown,
            location
        ) |>
        mutate(
            hfa = as.factor(case_when(
                location == "Neutral" ~ 0,
                ### home team on offense
                posteam == home_team ~ 1,
                ### home team on defense
                TRUE ~ -1
            )),
            play_pts_scored = case_when(
                two_point_conv_result == "success" ~ 2,
                pass_touchdown == 1 ~ 6,
                rush_touchdown == 1 ~ 6,
                TRUE ~ 0
            ),
            posteam = as.factor(posteam),
            defteam = as.factor(defteam)
        ) |>
        drop_na(game_id, home_team, posteam, defteam, hfa, location)

    ### fitting mixed effects model, treating posessing team and defensive team as random effects
    set.seed(802)
    ppg_mixed_model <- lmer(
        play_pts_scored ~ hfa + (1 | posteam) + (1 | defteam),
        data = PBP_PPGAdjustment_PY2
    )

    ### Extract random effects (team adjustments)
    team_effects <- ranef(ppg_mixed_model)

    ### average EPA (model intercept)
    avg_ppp <- fixef(ppg_mixed_model)["(Intercept)"]

    ### Extract offensive adjustments
    off_adj <- as.data.frame(team_effects$posteam) |>
        rename(adj_off_pts_per_play_PY2 = `(Intercept)`) |>
        mutate(team = rownames(team_effects$posteam)) |>
        mutate(adj_off_pts_per_play_PY2 = adj_off_pts_per_play_PY2 + avg_ppp)

    ### extract defensive adjustment
    def_adj <- as.data.frame(team_effects$defteam) |>
        rename(adj_def_pts_per_play_PY2 = `(Intercept)`) |>
        mutate(team = rownames(team_effects$defteam)) |>
        mutate(adj_def_pts_per_play_PY2 = adj_def_pts_per_play_PY2 + avg_ppp)

    ### combine and join back to VoA_df
    VoA_df <- VoA_df |>
        left_join(off_adj, by = "team") |>
        left_join(def_adj, by = "team") |>
        mutate(
            adj_off_ppg_PY2 = adj_off_pts_per_play_PY2 *
                mean(adj_off_plays_pg) *
                1.5,
            adj_def_pp_PY2g = adj_def_pts_per_play_PY2 *
                mean(adj_def_plays_pg) *
                1.5
        )

    ### yards/play opponent adjustment
    ### subsetting columns for adjustment
    PBP_YPPAdjustment_PY2 <- PY2_rushpass_plays |>
        select(game_id, home_team, posteam, defteam, yards_gained, location) |>
        mutate(
            hfa = as.factor(case_when(
                location == "Neutral" ~ 0,
                ### home team on offense
                posteam == home_team ~ 1,
                ### home team on defense
                TRUE ~ -1
            )),
            posteam = as.factor(posteam),
            defteam = as.factor(defteam)
        ) |>
        drop_na()

    ### fitting mixed effects model, treating posessing team and defensive team as random effects
    set.seed(802)
    ypp_mixed_model <- lmer(
        yards_gained ~ hfa + (1 | posteam) + (1 | defteam),
        data = PBP_YPPAdjustment_PY2
    )

    ### Extract random effects (team adjustments)
    team_effects <- ranef(ypp_mixed_model)

    ### Extract offensive adjustments
    off_adj <- as.data.frame(team_effects$posteam) |>
        rename(adj_off_ypp_PY2 = `(Intercept)`) |>
        mutate(team = rownames(team_effects$posteam))

    ### extract defensive adjustment
    def_adj <- as.data.frame(team_effects$defteam) |>
        rename(adj_def_ypp_PY2 = `(Intercept)`) |>
        mutate(team = rownames(team_effects$defteam))

    ### average EPA (model intercept)
    avg_ypp <- fixef(ypp_mixed_model)["(Intercept)"]

    ### combine and join back to VoA_df
    VoA_df <- VoA_df |>
        left_join(off_adj, by = "team") |>
        left_join(def_adj, by = "team") |>
        mutate(
            adj_off_ypp_PY2 = adj_off_ypp_PY2 + avg_ypp,
            adj_def_ypp_PY2 = adj_def_ypp_PY2 + avg_ypp
        )

    ### PY3 Adjusted Stats
    ### Creating opponent-adjusted stats
    ### EPA/play
    ### subsetting columns for epa/play adjustment
    PBP_EPAAdjustment_PY3 <- PY3_rushpass_plays |>
        select(game_id, home_team, posteam, defteam, epa, location) |>
        mutate(
            hfa = as.factor(case_when(
                location == "Neutral" ~ 0,
                ### home team on offense
                posteam == home_team ~ 1,
                ### home team on defense
                TRUE ~ -1
            )),
            posteam = as.factor(posteam),
            defteam = as.factor(defteam)
        ) |>
        drop_na()

    ### fitting mixed effects model, treating posessing team and defensive team as random effects
    set.seed(802)
    epa_mixed_model <- lmer(
        epa ~ hfa + (1 | posteam) + (1 | defteam),
        data = PBP_EPAAdjustment_PY3
    )

    ### Extract random effects (team adjustments)
    team_effects <- ranef(epa_mixed_model)

    ### Extract offensive adjustments
    off_adj <- as.data.frame(team_effects$posteam) |>
        rename(adj_off_epa_PY3 = `(Intercept)`) |>
        mutate(team = rownames(team_effects$posteam))

    ### extract defensive adjustment
    def_adj <- as.data.frame(team_effects$defteam) |>
        rename(adj_def_epa_PY3 = `(Intercept)`) |>
        mutate(team = rownames(team_effects$defteam))

    ### average EPA (model intercept)
    avg_epa <- fixef(epa_mixed_model)["(Intercept)"]

    ### combine and join back to VoA_df
    VoA_df <- VoA_df |>
        left_join(off_adj, by = "team") |>
        left_join(def_adj, by = "team") |>
        mutate(
            adj_off_epa_PY3 = adj_off_epa_PY3 + avg_epa,
            adj_def_epa_PY3 = adj_def_epa_PY3 + avg_epa
        )

    ### opponent adjusted plays per game
    ### uses the same pbp dataset as the epa adjustment above
    PlaysPG_Adjustment_PY3 <- PBP_EPAAdjustment_PY3 |>
        group_by(game_id) |>
        summarize(
            home_off_plays = sum(posteam == home_team),
            away_off_plays = sum(posteam == away_team),
            home_team = as.factor(unique(home_team)[1]),
            away_team = as.factor(unique(away_team)[1]),
            location = unique(location)[1]
        ) |>
        pivot_longer(
            cols = ends_with("_plays"),
            names_to = "home_away_col_names",
            values_to = "team_plays"
        ) |>
        mutate(
            team = case_when(
                home_away_col_names == "home_off_plays" ~ home_team,
                TRUE ~ away_team
            ),
            opp_team = case_when(
                home_away_col_names == "home_off_plays" ~ away_team,
                TRUE ~ home_team
            ),
            hfa = as.factor(case_when(
                location == "Neutral" ~ 0,
                home_team == team ~ 1,
                TRUE ~ -1
            ))
        )

    ### fitting mixed effects model, treating team and opposing team as random effects
    set.seed(802)
    plays_mixed_model <- lmer(
        team_plays ~ hfa + (1 | team) + (1 | opp_team),
        data = PlaysPG_Adjustment_PY3
    )

    ### Extract random effects (team adjustments)
    team_effects <- ranef(plays_mixed_model)

    ### Extract offensive adjustments
    off_adj <- as.data.frame(team_effects$team) |>
        rename(adj_off_plays_pg_PY3 = `(Intercept)`) |>
        mutate(team = rownames(team_effects$team))

    ### extract defensive adjustment
    def_adj <- as.data.frame(team_effects$opp_team) |>
        rename(adj_def_plays_pg_PY3 = `(Intercept)`) |>
        mutate(team = rownames(team_effects$opp_team))

    ### average plays per game (model intercept)
    avg_plays_pg <- fixef(plays_mixed_model)["(Intercept)"]

    ### combine and join back to VoA_df
    VoA_df <- VoA_df |>
        left_join(off_adj, by = "team") |>
        left_join(def_adj, by = "team") |>
        mutate(
            adj_off_plays_pg_PY3 = adj_off_plays_pg_PY3 + avg_plays_pg,
            adj_def_plays_pg_PY3 = adj_def_plays_pg_PY3 + avg_plays_pg
        )

    ### Explosiveness
    ### subsetting columns for epa/play (explosiveness, so only EPA/play on successful plays) adjustment
    PBP_ExpAdjustment_PY3 <- PY3_success_plays |>
        select(game_id, home_team, posteam, defteam, epa, location) |>
        mutate(
            hfa = as.factor(case_when(
                location == "Neutral" ~ 0,
                ### home team on offense
                posteam == home_team ~ 1,
                ### home team on defense
                TRUE ~ -1
            )),
            posteam = as.factor(posteam),
            defteam = as.factor(defteam)
        ) |>
        drop_na()

    ### fitting mixed effects model, treating posessing team and defensive team as random effects
    set.seed(802)
    exp_mixed_model <- lmer(
        epa ~ hfa + (1 | posteam) + (1 | defteam),
        data = PBP_ExpAdjustment_PY3
    )

    ### Extract random effects (team adjustments)
    team_effects <- ranef(exp_mixed_model)

    ### Extract offensive adjustments
    off_adj <- as.data.frame(team_effects$posteam) |>
        rename(adj_off_explosiveness_PY3 = `(Intercept)`) |>
        mutate(team = rownames(team_effects$posteam))

    ### extract defensive adjustment
    def_adj <- as.data.frame(team_effects$defteam) |>
        rename(adj_def_explosiveness_PY3 = `(Intercept)`) |>
        mutate(team = rownames(team_effects$defteam))

    ### average EPA (model intercept)
    avg_explosiveness <- fixef(exp_mixed_model)["(Intercept)"]

    ### combine and join back to VoA_df
    VoA_df <- VoA_df |>
        left_join(off_adj, by = "team") |>
        left_join(def_adj, by = "team") |>
        mutate(
            adj_off_explosiveness_PY3 = adj_off_explosiveness_PY3 +
                avg_explosiveness,
            adj_def_explosiveness_PY3 = adj_def_explosiveness_PY3 +
                avg_explosivensss
        )

    ### ppg
    ## this will initially give me pts/play, then I will multiply it by off/def plays per game when binding to VoA_df
    ### subsetting columns for pts/play adjustment
    PBP_PPGAdjustment_PY3 <- PY3_rushpass_plays |>
        select(
            game_id,
            home_team,
            posteam,
            defteam,
            two_point_conv_result,
            pass_touchdown,
            rush_touchdown,
            location
        ) |>
        mutate(
            hfa = as.factor(case_when(
                location == "Neutral" ~ 0,
                ### home team on offense
                posteam == home_team ~ 1,
                ### home team on defense
                TRUE ~ -1
            )),
            play_pts_scored = case_when(
                two_point_conv_result == "success" ~ 2,
                pass_touchdown == 1 ~ 6,
                rush_touchdown == 1 ~ 6,
                TRUE ~ 0
            ),
            posteam = as.factor(posteam),
            defteam = as.factor(defteam)
        ) |>
        drop_na(game_id, home_team, posteam, defteam, hfa, location)

    ### fitting mixed effects model, treating posessing team and defensive team as random effects
    set.seed(802)
    ppg_mixed_model <- lmer(
        play_pts_scored ~ hfa + (1 | posteam) + (1 | defteam),
        data = PBP_PPGAdjustment_PY3
    )

    ### Extract random effects (team adjustments)
    team_effects <- ranef(ppg_mixed_model)

    ### average EPA (model intercept)
    avg_ppp <- fixef(ppg_mixed_model)["(Intercept)"]

    ### Extract offensive adjustments
    off_adj <- as.data.frame(team_effects$posteam) |>
        rename(adj_off_pts_per_play_PY3 = `(Intercept)`) |>
        mutate(team = rownames(team_effects$posteam)) |>
        mutate(adj_off_pts_per_play_PY3 = adj_off_pts_per_play_PY3 + avg_ppp)

    ### extract defensive adjustment
    def_adj <- as.data.frame(team_effects$defteam) |>
        rename(adj_def_pts_per_play_PY3 = `(Intercept)`) |>
        mutate(team = rownames(team_effects$defteam)) |>
        mutate(adj_def_pts_per_play_PY3 = adj_def_pts_per_play_PY3 + avg_ppp)

    ### combine and join back to VoA_df
    VoA_df <- VoA_df |>
        left_join(off_adj, by = "team") |>
        left_join(def_adj, by = "team") |>
        mutate(
            adj_off_ppg_PY3 = adj_off_pts_per_play_PY3 *
                mean(adj_off_plays_pg) *
                1.5,
            adj_def_pp_PY3g = adj_def_pts_per_play_PY3 *
                mean(adj_def_plays_pg) *
                1.5
        )

    ### yards/play opponent adjustment
    ### subsetting columns for adjustment
    PBP_YPPAdjustment_PY3 <- PY3_rushpass_plays |>
        select(game_id, home_team, posteam, defteam, yards_gained, location) |>
        mutate(
            hfa = as.factor(case_when(
                location == "Neutral" ~ 0,
                ### home team on offense
                posteam == home_team ~ 1,
                ### home team on defense
                TRUE ~ -1
            )),
            posteam = as.factor(posteam),
            defteam = as.factor(defteam)
        ) |>
        drop_na()

    ### fitting mixed effects model, treating posessing team and defensive team as random effects
    set.seed(802)
    ypp_mixed_model <- lmer(
        yards_gained ~ hfa + (1 | posteam) + (1 | defteam),
        data = PBP_YPPAdjustment_PY3
    )

    ### Extract random effects (team adjustments)
    team_effects <- ranef(ypp_mixed_model)

    ### Extract offensive adjustments
    off_adj <- as.data.frame(team_effects$posteam) |>
        rename(adj_off_ypp_PY3 = `(Intercept)`) |>
        mutate(team = rownames(team_effects$posteam))

    ### extract defensive adjustment
    def_adj <- as.data.frame(team_effects$defteam) |>
        rename(adj_def_ypp_PY3 = `(Intercept)`) |>
        mutate(team = rownames(team_effects$defteam))

    ### average EPA (model intercept)
    avg_ypp <- fixef(ypp_mixed_model)["(Intercept)"]

    ### combine and join back to VoA_df
    VoA_df <- VoA_df |>
        left_join(off_adj, by = "team") |>
        left_join(def_adj, by = "team") |>
        mutate(
            adj_off_ypp_PY3 = adj_off_ypp_PY3 + avg_ypp,
            adj_def_ypp_PY3 = adj_def_ypp_PY3 + avg_ypp
        )

    ### returning VoAVariables
    return(VoA_df)
}


### function for ranking select columns in VoAVariables (and in the train model)
## for right now, only getting called after Week 10 anf for the train model

rank_voa_cols <- function(VoA_df) {
    VoA_df <- VoA_df |>
        mutate(
            Rank_off_ypp = dense_rank(desc(off_ypp)),
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
            Rank_adj_off_epa = dense_rank(desc(adj_off_epa)),
            Rank_adj_off_explosiveness = dense_rank(desc(
                adj_off_explosiveness
            )),
            Rank_adj_off_ypp = dense_rank(desc(adj_off_ypp)),
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
            Rank_adj_def_epa = dense_rank(adj_def_epa),
            Rank_adj_def_explosiveness = dense_rank(adj_def_explosiveness),
            Rank_adj_def_ypp = dense_rank(adj_def_ypp),
            ### ranking ST variables now
            Rank_net_st_epa = dense_rank(desc(st_net_epa)),
            Rank_net_punt_return_yds = dense_rank(desc(net_punt_return_yds)),
            Rank_net_punt_return_TDs = dense_rank(desc(net_punt_return_TDs)),
            Rank_net_kick_return_yds = dense_rank(desc(net_kick_return_yds)),
            Rank_net_kick_return_TDs = dense_rank(desc(net_kick_return_TDs)),
            Rank_net_xp_rate = dense_rank(desc(net_xp_rate)),
            Rank_net_xp_made_pg = dense_rank(desc(net_xp_made_pg)),
            Rank_net_xp_rate = dense_rank(desc(net_xp_rate)),
            Rank_net_xp_made_pg = dense_rank(desc(net_xp_made_pg)),
            Rank_net_st_ppg = dense_rank(desc(net_st_ppg)),
        )

    ### returning VoAVariables
    return(VoA_df)
}
