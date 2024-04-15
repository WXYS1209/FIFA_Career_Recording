# Visualization
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

overall_subtab <- 
  tabPanel(
    "Overall",
    sidebarLayout(
      sidebarPanel(
      ),
      mainPanel(
        plotlyOutput("aa", height = "700px")
      )
    )
  )

## Combination
visualization_tab <- 
  tabPanel("Visualization", 
    tabsetPanel(
      current_season_subtab,
      overall_subtab
    )
  )

