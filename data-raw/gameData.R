## Create Game Data for App
library(nflverse)
library(tidyverse)

gameData <- load_schedules(seasons = 2002:get_current_season()) |>
  mutate(
    home_team = clean_team_abbrs(home_team),
    away_team = clean_team_abbrs(away_team)
  )

usethis::use_data(gameData, overwrite = TRUE)
