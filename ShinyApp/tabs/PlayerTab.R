# Player

player_tab <- 
  tabPanel(
    "Player",
    sidebarLayout(
      sidebarPanel(
        pickerInput(
          inputId = "player_name",
          label = "Name",
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
          "player_comp",
          "Competition",
          choices = c("PL", "EC", "CC", "FA", "Other"),
          multiple = T,
          selected = "PL",
          options = pickerOptions(actionsBox = T)
        )
      ),
      mainPanel(
        plotlyOutput("player_1"),
        plotlyOutput("player_2")
        
        # plotlyOutput("vis_match")
      )
    )
  )

