#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' Load Libraries
#' @import shiny
#' @noRd
library(shiny)
library(shinydashboard)
library(bs4Dash)
library(shinyWidgets)
library(shinycssloaders)
library(shinyjs)
library(waiter)
library(RColorBrewer)
library(fresh)
library(markdown)
library(stringr)
library(rvest)
library(htmltools)
library(gt)
library(gtsummary)
library(gtExtras)
library(reactable)
library(reactablefmtr)
library(smplot2)
library(patchwork)
library(pracma)
library(forecast)
library(elo)
library(MASS)
library(bestNormalize)
library(tictoc)
library(caret)
library(splines)
library(mgcv)
library(DescTools)
library(car)
library(bayesplot)
library(BayesFactor)
library(rstanarm)
library(tidybayes)
library(loo)
library(brms)
library(performance)
library(nflverse)
library(tidyverse)

# Input Filters ----
teamsDataInput <- load_teams(current = TRUE) |>
  select(team_abbr, team_name, team_conf, team_division) |>
  arrange(team_division, team_name) |>
  as.data.frame()


# Set theme ====
my_theme <- create_theme(
  theme = "paper",
  bs4dash_sidebar_dark(
    bg = "#2d3b4d"
  ),
  bs4dash_status(
    primary = "purple", info = "#eec900"
  )
)
tags$style(".buttoncolor.bttn-primary{background-color: #6399b8")

app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    dashboardPage(dark = NULL,
                  footer = dashboardFooter(left = br()),
                  freshTheme = my_theme,
                  #preloader = list(html = tagList(spin_1(), "Loading ..."), color = "#3c8dbc"),

                  # Dahsboard Header ===============
                  header = dashboardHeader(
                    title = dashboardBrand(
                      title = div(style = "font-size:14pt",
                                  align = "center",
                                  "NFL Analysis App",
                                  dashboardBadge(
                                    color = "warning",
                                    rounded = TRUE,
                                    position = "right",
                                    "v1.0")
                      ),
                      color = "primary"
                    ),
                    compact = FALSE,
                    rightUi = tags$li(
                      class = "dropdown",
                      dropdownMenu(
                        badgeStatus = NULL,
                        type = "notifications",
                        headerText = "NFL Analysis",
                        icon = icon("info-circle"),
                        notificationItem(
                          inputId = "info1",
                          text = "Developer: Tyler Pollard",
                          icon = icon("users-cog"),
                          status = "info"
                        ),
                        notificationItem(
                          inputId = "info2",
                          text = "Release Date: 20 Oct 2024",
                          icon = icon("calendar"),
                          status = "info"
                        ),
                        notificationItem(
                          inputId = "info3",
                          text = "Version: 1.0",
                          icon = icon("code"),
                          status = "info"
                        )
                      )
                    ),
                    ## Navbar Menu ------------------
                    navbarMenu(
                      id = "navMenu",
                      ### Home Tab ----
                      navbarTab(tabName = "homeTab", text = "Home")
                    ) # end navbarMenu
                  ), # close header
                  scrollToTop = TRUE,
                  # Dashboard Sidebar =============
                  sidebar = dashboardSidebar(
                    id = "sidebar",
                    skin = "dark",
                    elevation = 5,
                    fixed = FALSE,
                    minified = FALSE,
                    status = "primary",
                    compact = TRUE,
                    collapsed = TRUE,
                    width = "150px",
                    ## Sidebar Menu ---------------
                    sidebarMenu(
                      id = "menu_items",
                      ### Data Tab ----
                      h4("Data", style = "color: white"),
                      menuItem(text = "Standings", tabName = "standingsTab", icon = icon("table")),
                      menuItem(text = "Scores", tabName = "scoresTab", icon = icon("table")),
                      menuItem(text = "Team Statistics", icon = icon("users"),
                               menuSubItem(text = "Offense", tabName = "teamOffenseTab"),
                               menuSubItem(text = "Defense", tabName = "teamDefenseTab"),
                               menuSubItem(text = "Special Teams", tabName = "teamSpecialTeamsTab"),
                               menuSubItem(text = "Scoring", tabName = "teamScoringTab")
                      ),
                      menuItem(text = "Player Statistics", icon = icon("user"),
                               menuSubItem(text = "Offense", tabName = "playerOffenseTab"),
                               menuSubItem(text = "Defense", tabName = "playerDefenseTab"),
                               menuSubItem(text = "Special Teams", tabName = "playerSpecialTeamsTab"),
                               menuSubItem(text = "Scoring", tabName = "playerScoringTab"),
                               menuSubItem(text = "Fantasy", tabName = "playerFantasyTab")
                      ),
                      h4("Betting", style = "color: white"),
                      menuItem(text = "Games", tabName = "bettingGamesTab", icon = icon("user")),
                      menuItem(text = "Player Props", tabName = "bettingPlayerPropsTab", icon = icon("user"))
                    ) # close sidebar menu
                  ), # close dashboard sidebar
                  # Dashboard Controlbar ==================
                  controlbar = dashboardControlbar(),
                  # Dashboard Body ================
                  body = dashboardBody(
                    useShinyjs()
                  )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(ext = 'png'),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "nflAnalysis"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
