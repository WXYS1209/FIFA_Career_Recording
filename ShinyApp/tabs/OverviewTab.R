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
        pickerInput(
          "vis_comp",
          "Competition",
          choices = c("PL", "EC", "CC", "FA", "Other"),
          multiple = T,
          selected = "PL",
          options = pickerOptions(actionsBox = T)
        ),
        pickerInput(
          "vis_goal_type",
          "Goal Type",
          choices = c("GF", "GA", "diff"),
          multiple = F
        ),
        pickerInput(
          "vis_where",
          "H / A",
          choices = c("Home", "Away"),
          multiple = T,
          selected = c("Home", "Away")
        ),
        
        plotlyOutput("vis_radar"),
      ),
      mainPanel(
        column(6,
               plotlyOutput("vis_1")),
        column(6,
               plotlyOutput("vis_2")),
        plotOutput("vis_match")
      )
    )
  )

## Overall
overall_subtab <- 
  tabPanel(
    "Overall",
    
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

## Combination
overview_tab <- 
  tabPanel("Overview", 
           tabsetPanel(
             current_season_subtab,
             overall_subtab
           )
  )

