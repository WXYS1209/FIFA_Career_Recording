# Player

player_tab <- 
  tabPanel(
    "Player",
    sidebarLayout(
      sidebarPanel(
        fluidRow(
          column(6,
                 pickerInput(
                   "player_season",
                   "Season",
                   choices = 1:10,
                   selected = 1,
                   multiple = T,
                   options = pickerOptions(actionsBox = T)
                 )),
          column(6,
                 pickerInput(
                   "player_comp",
                   "Competition",
                   choices = c("PL", "EC", "CC", "FA", "Other"),
                   multiple = T,
                   selected = "PL",
                   options = pickerOptions(actionsBox = T)
                 ))
        ),
        
        fluidRow(
          column(6,
                 pickerInput(
                   inputId = "player_name_1",
                   label = "Player 1",
                   choices = c("J. Félix", 
                               "M. Mount",
                               "K. Havertz"),
                   multiple = F,
                   selected = "J. Félix",
                   options = pickerOptions(liveSearch = T)
                 )),
          column(6,
                 pickerInput(
                   inputId = "player_name_2",
                   label = "Player 2",
                   choices = c("J. Félix", 
                               "M. Mount",
                               "K. Havertz"),
                   multiple = F,
                   selected = "J. Félix",
                   options = pickerOptions(liveSearch = T)
                 ))
        ),

        pickerInput(
          inputId = "player_variable",
          label = "Stat",
          choices = c("Played", "Started", 
                      "MOTM", "Goals", 
                      "Assists", "Shots", 
                      "Shot_Comp", "Shot_Acc",
                      "Pass", "Pass_Comp", "Pass_Acc",
                      "Key_Pass",
                      "Dribble", "Dribble_Comp", "Dribble_Acc",
                      "Tackle", "Tackle_Comp", "Tackle_Acc",
                      "Possession_Won", "Possession_Lost",
                      "Distance", "Rating"
          ),
          multiple = F,
          selected = "Goals",
          options = pickerOptions(liveSearch = T)
        ),
        pickerInput(
          inputId = "player_variable_radar",
          label = "Stat for Radar",
          choices = c("Played", "Started", 
                      "MOTM", "Goals", 
                      "Assists", "Shots", 
                      "Shot_Acc", "Pass", 
                      "Pass_Acc", "Key_Pass",
                      "Dribble", "Dribble_Acc", 
                      "Tackle", "Tackle_Acc", 
                      "Possession_Won", "Possession_Lost",
                      "Distance", "Rating"
          ),
          multiple = T,
          selected = "Goals",
          options = pickerOptions(actionsBox = T,
                                  liveSearch = T)
        ),
        plotlyOutput("player_radar")
      ),
      mainPanel(
        fluidRow(
          column(3,
                 uiOutput("player_1_image")),
          column(9,
                 plotlyOutput("player_1"))
        ),
        fluidRow(
          column(3,
                 uiOutput("player_2_image")),
          column(9,
                 plotlyOutput("player_2"))
        ),
        
        
        # plotlyOutput("player_2")
        # imageOutput("player_1_image")
        # plotlyOutput("vis_match")
      )
    )
  )

