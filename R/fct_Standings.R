#' helpers
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
## Create Function
calculateStandings <- function(season = 2024){
  if(!IsWhole(season)){
    stop("Please enter integer value between 2003 and 2024")
    #return(NULL)
  }

  if(season < 2003 | season > get_current_season()){
    stop("Please enter integer value between 2003 and 2024")
  }

  gameDataCurrent <- load_schedules(seasons = season) |>
    filter(complete.cases(result)) |>
    filter(game_type == "REG") |>
    mutate(
      home_team = clean_team_abbrs(home_team),
      away_team = clean_team_abbrs(away_team)
    )

  gameDataLongCurrent <- gameDataCurrent |>
    clean_homeaway(invert = c("result", "spread_line"))

  standingCurrent <- gameDataLongCurrent |>
    filter(!is.na(result)) |>
    select(
      week,
      team,
      team_score,
      opponent,
      opponent_score,
      location,
      result
    ) |>
    mutate(
      win = ifelse(result > 0, 1, 0),
      loss = ifelse(result < 0, 1, 0),
      tie = ifelse(result == 0, 1, 0)
    ) |>
    group_by(team) |>
    summarise(
      games_played = n(),
      across(c(win,
               loss,
               tie,
               team_score,
               opponent_score,
               result),
             ~sum(.x)),
    ) |>
    mutate(
      win_loss_percent = (win + tie/2)/(win + loss + tie/2),
      MOV = result/games_played
    ) |>
    mutate(team_PPG = team_score/games_played, .after = team_score) |>
    mutate(opp_PPG = opponent_score/games_played, .after = opponent_score) |>
    select(
      "team",
      "games_played",
      "win",
      "loss",
      "tie",
      "win_loss_percent",
      everything()
    )

  leaguePPGCurrent <- sum(standingCurrent$team_score)/sum(standingCurrent$games_played)
  standingCurrent <- standingCurrent |>
    mutate(SOS = 0, .after = MOV) |>
    mutate(SRS = MOV, .after = SOS) |>
    mutate(OSRS = team_PPG - leaguePPGCurrent) |>
    mutate(DSRS = SRS - OSRS)

  max_iterations <- 100
  tolerance <- 0.001
  for (i in 1:max_iterations) {
    previous_SRS <- standingCurrent$SRS
    previous_OSRS <- standingCurrent$OSRS
    previous_DSRS <- standingCurrent$DSRS

    standingCurrent <- standingCurrent |>
      left_join(
        gameDataLongCurrent |>
          select(team, opponent, result, team_score) |>
          left_join(standingCurrent |> select(team, SRS, DSRS), by = c("opponent" = "team")) |>
          mutate(
            SOS = SRS,
            SRS = result + SOS,
            OSOS = DSRS,
            OSRS = team_score + OSOS - mean(team_score)
          ) |>
          group_by(team) |>
          summarise(
            newSOS = mean(SOS, na.rm = TRUE),
            newSRS = mean(SRS, na.rm = TRUE),
            newOSRS = mean(OSRS)
          ),
        by = join_by(team)
      ) |>
      mutate(
        SOS = ifelse(is.na(newSOS), 0, newSOS),
        SRS = ifelse(is.na(newSRS), 0, newSRS),
        OSRS = ifelse(is.na(newOSRS), 0, newOSRS),
        DSRS = SRS - OSRS
      ) |>
      select(-newSOS, -newSRS, -newOSRS)

    if(max(abs(standingCurrent$SRS - previous_SRS),
           abs(standingCurrent$OSRS - previous_OSRS),
           abs(standingCurrent$DSRS - previous_DSRS)) < tolerance){
      cat("Converged after", i, "iterations.\n")
      break
    }

    # If last iteration and not converged
    if (i == max_iterations) {
      cat("Reached maximum iterations = ",i, "without full convergence.\n")
    }
  }

  standingCurrent <- standingCurrent |> arrange(desc(SRS)) |>
    left_join(teamsData |> select(team_abbr, team_name, team_conf, team_division),
              by = c("team" = "team_abbr")) |>
    select(
      team,
      team_name,
      team_conf,
      team_division,
      everything()
    )

  return(standingCurrent)
}
