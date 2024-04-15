# Data Tab
#### Data Table #### 
record_match_subtab <-
  tabPanel(
    "Record Match",
    sidebarLayout(
      sidebarPanel(
        fileInput("fileUpload", "Upload Match Record File", accept = c(".csv")),
        pickerInput("player", "Select Player", 
                    choices = NULL,
                    multiple = F,
                    options = list(`live-search` = T)),
        h4("G&A"),
        column(6,
               numericInput("goals", "Goals", min = 0, value = 0)),
        column(6,
               numericInput("assists", "Assists", min = 0, value = 0)),
        h4("Shooting"),
        column(6,
               numericInput("shot", "Shots", min = 0, value = 0)),
        column(6,
               numericInput("shot_comp", "Shot On Target", min = 0, value = 0)),
        h4("Passing"),
        column(6,
               numericInput("pass", "Passes Attempted", min = 0, value = 0)),
        column(6,
               numericInput("pass_comp", "Passes Completed", min = 0, value = 0)),
        h4("Dribbling"),
        column(6,
               numericInput("dribble", "Dribbles Attempted", min = 0, value = 0)),
        column(6,
               numericInput("dribble_comp", "Dribbles Completed",
                            min = 0, value = 0)),
        h4("Tackling"),
        column(6,
               numericInput("tackle", "Tackles Attempted", min = 0, value = 0)),
        column(6,
               numericInput("tackle_comp", "Tackles Completed",
                            min = 0, value = 0)),
        h4("Possession"),
        column(6,
               numericInput("poss_won", "Possession Won", min = 0, value = 0)),
        column(6,
               numericInput("poss_lost", "Possession Lost", min = 0, value = 0)),
        h4("Other"),
        column(6,
               numericInput("dist", "Distance Covered", min = 0, step = 0.1,
                            value = 0)),
        column(6,
               numericInput("rating", "Rating", min = 0, step = 0.1,
                            value = 0)),
        column(6,
               checkboxInput("motm", "Check if MOTM", FALSE)),
        column(6,
               checkboxInput("starter", "Check if starter", FALSE)),
        actionButton("add", "Add Match Data"),
        downloadButton("downloadData", "Download Match Data")
      ),
      mainPanel(
        DTOutput("updatedData")
      )
    )
  )

merge_subtab <-
  tabPanel(
    "Merge Match",
    sidebarLayout(
      sidebarPanel(
        fileInput("fileUpload_merge", "Upload Match Record File", accept = c(".csv")),
        actionButton("merge", "Merge Match Data"),
        downloadButton("downloadData_merge", "Download Match Data")
      ),
      mainPanel(
        DTOutput("updatedData_merge")
      )
    )
  )


updata_data_tab <- 
  tabPanel("Update Data",
    tabsetPanel(
      record_match_subtab,
      merge_subtab
    )
  )


