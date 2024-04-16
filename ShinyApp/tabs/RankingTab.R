# Ranking
## Current Season
current_season_subtab <- 
  tabPanel(
    "Current Season",
    sidebarLayout(
      sidebarPanel(
        pickerInput(
          inputId = "vis_season",
          label = "Season",
          choices = 1:10,
          multiple = F,
          selected = 1
        ),
        pickerInput(
          inputId = "vis_variable",
          label = "Stat",
          choices = c("Goals & Distance",
                      "Assisting",
                      "Shooting",
                      "Passing",
                      "Dribbling",
                      "Tackling",
                      "Possession",
                      "Rating & Appearance"
          ),
          multiple = F,
          selected = "Goals & Distance"
        ),
        plotlyOutput("vis_radar"),
      ),
      mainPanel(
        column(6,
               plotlyOutput("vis_1")),
        column(6,
               plotlyOutput("vis_2"))
      )
    )
  )

## Overall
overall_subtab <- 
  tabPanel(
    "Overall",
    sidebarLayout(
      sidebarPanel(
        pickerInput(
          inputId = "vis_variable_overall",
          label = "Stat",
          choices = c("Goals & Distance",
                      "Assisting",
                      "Shooting",
                      "Passing",
                      "Dribbling",
                      "Tackling",
                      "Possession",
                      "Rating & Appearance"
          ),
          multiple = F,
          selected = "Goals & Distance"
        ),
      ),
      mainPanel(
        # column(6,
        #        plotlyOutput("vis_overall_1")),
        # column(6,
        #        plotlyOutput("vis_overall_2")),
        column(4,
               h3("Goals"),
               DTOutput("vis_overall_dt_goals")),
        column(4,
               h3("Assists"),
               DTOutput("vis_overall_dt_assists")),
        column(4,
               h3("Games Played"),
               DTOutput("vis_overall_dt_games")),
        column(4,
               h3("Rating"),
               DTOutput("vis_overall_dt_rating")),
        column(4,
               h3("Tackles"),
               DTOutput("vis_overall_dt_tackles")),
        column(4,
               h3("Distance"),
               DTOutput("vis_overall_dt_distance"))
      )
    )
  )

## Combination
ranking_tab <- 
  tabPanel("Ranking", 
           tabsetPanel(
             current_season_subtab,
             overall_subtab
           )
  )

