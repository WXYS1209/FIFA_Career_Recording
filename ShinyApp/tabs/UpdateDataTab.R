# Data Tab
#### Data Table #### 
record_match_subtab <-
  tabPanel(
    "Record Data",
    sidebarLayout(
      sidebarPanel(
        # fileInput("fileUpload", "Upload Match Record File", accept = c(".csv")),
        pickerInput("season", "Season", 
                    choices = 1:10,
                    selected = 1,
                    multiple = F),
        column(6,
               numericInput("match", "Match", min = 1, step = 1, value = 1)),
        column(6,
               pickerInput("competition", "Competition",
                           choices = c("PL", "EC",
                                       "CC", "FA",
                                       "Other"))),
        pickerInput("player", "Select Player", 
                    choices = NULL,
                    multiple = F,
                    options = list(`live-search` = T)),
        h4("G&A"),
        fluidRow(
          column(4,
                 numericInput("goals", "Goals", min = 0, value = 0)),
          column(4,
                 numericInput("assists", "Assists", min = 0, value = 0)),
          column(4,
                 numericInput("key_pass", "Key Passes", min = 0, value = 0))),
        h4("Shooting"),
        fluidRow(
          column(6,
                 numericInput("shot", "Shots", min = 0, value = 0)),
          column(6,
                 numericInput("shot_comp", "Shot On Target", min = 0, value = 0))),
        h4("Passing"),
        fluidRow(
          column(6,
                 numericInput("pass", "Passes Attempted", min = 0, value = 0)),
          column(6,
                 numericInput("pass_comp", "Passes Completed", min = 0, value = 0))),
        h4("Dribbling"),
        fluidRow(
          column(6,
                 numericInput("dribble", "Dribbles Attempted", min = 0, value = 0)),
          column(6,
                 numericInput("dribble_comp", "Dribbles Completed",
                              min = 0, value = 0))),
        h4("Tackling"),
        fluidRow(
          column(6,
                 numericInput("tackle", "Tackles Attempted", min = 0, value = 0)),
          column(6,
                 numericInput("tackle_comp", "Tackles Completed",
                              min = 0, value = 0))),
        h4("Possession"),
        fluidRow(
          column(6,
                 numericInput("poss_won", "Possession Won", min = 0, value = 0)),
          column(6,
                 numericInput("poss_lost", "Possession Lost", min = 0, value = 0))),
        h4("Other"),
        fluidRow(
          column(6,
                 numericInput("dist", "Distance Covered", min = 0, step = 0.1,
                              value = 0)),
          column(6,
                 numericInput("rating", "Rating", min = 0, step = 0.1,
                              value = 0))),
        fluidRow(
          column(6,
                 checkboxInput("motm", "Check if MOTM", FALSE)),
          column(6,
                 checkboxInput("starter", "Check if starter", FALSE))),
        fluidRow(
          column(4,
                 actionButton("add", "Add Match Data")),
          column(4,
                 downloadButton("downloadData", "Download Data")),
          column(4,
                 actionButton("record_undo", "Undo"))
        )
      ),
      mainPanel(
        DTOutput("updatedData_dt")
      )
    )
  )

merge_subtab <-
  tabPanel(
    "Merge Data",
    sidebarLayout(
      sidebarPanel(
        fluidRow(
          column(6,
                 pickerInput("season_merge", "Select Season",
                             choices = c(1:10, "All"),
                             selected = 1)),
          column(6,
                 pickerInput("competition_merge", "Select Competition",
                             choices = c("PL", "EC", "CC", 
                                         "FA", "Other", "All")))
        ),
        # fileInput("fileUpload_main", "Upload Aggregated Record File",
        #           accept = c(".csv")),
        fileInput("fileUpload_merge", "Upload Match Record File",
                  accept = c(".csv"),
                  multiple = T),
        actionButton("merge", "Merge Data"),
        actionButton("updateData_merge", 
                     "Update Merged Aggregated Record")
      ),
      mainPanel(
        DTOutput("updatedData_merge_dt")
      )
    )
  )

match_stat_subtab <- 
  tabPanel(
    "Match Stat",
    sidebarLayout(
      sidebarPanel(
        pickerInput("match_stat_season", "Season",
                    choices = 1,
                    selected = 1),
        fluidRow(
          column(6,
                 pickerInput("match_stat_comp", "Competition",
                             choices = c("PL", "EC",
                                         "CC", "FA",
                                         "Other"))),
          column(6,
                 textInput("opponent", "Against"))
        ),
        numericInput("gf", "Goals For", value = 0, min = 0),
        numericInput("ga", "Goals Against", min = 0, value = 0),
        numericInput("possession", "Possession (%)", min = 0, value = 0),
        radioButtons("where", "Home or Away", choices = c("Home", "Away")),
        fluidRow(
          column(4,
                 actionButton("add_match_stat", "Add Match Stat")),
          column(4,
                 actionButton("updateMatchStat", "Update Match Stat")),
          column(4,
                 actionButton("match_stat_undo", "Undo"))
        )
      ),
      mainPanel(
        DTOutput("updatedMatchStat_dt")
      )
    )
  )


updata_data_tab <- 
  tabPanel(
    "Update Data",
    tabsetPanel(
      record_match_subtab,
      merge_subtab,
      match_stat_subtab
    )
  )


