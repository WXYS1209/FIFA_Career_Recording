# Player

player_tab <- 
  tabPanel(
    "Player",
    sidebarLayout(
      sidebarPanel(
        pickerInput(
          inputId = "player_name_1",
          label = "Player 1",
          choices = c("J. Félix", 
                      "M. Mount",
                      "K. Havertz"),
          multiple = F,
          selected = "J. Félix",
          options = pickerOptions(liveSearch = T)
        ),
        pickerInput(
          inputId = "player_name_2",
          label = "Player 2",
          choices = c("J. Félix", 
                      "M. Mount",
                      "K. Havertz"),
          multiple = F,
          selected = "J. Félix",
          options = pickerOptions(liveSearch = T)
        ),
        pickerInput(
          "player_season",
          "Season",
          choices = 1:10,
          selected = 1,
          multiple = T,
          options = pickerOptions(actionsBox = T)
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
          selected = "Goals"
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
        pickerInput(
          "player_comp",
          "Competition",
          choices = c("PL", "EC", "CC", "FA", "Other"),
          multiple = T,
          selected = "PL",
          options = pickerOptions(actionsBox = T)
        ),
        plotlyOutput("player_radar")
      ),
      mainPanel(
        plotlyOutput("player_1"),
        plotlyOutput("player_2")
        
        # plotlyOutput("vis_match")
      )
    )
  )

