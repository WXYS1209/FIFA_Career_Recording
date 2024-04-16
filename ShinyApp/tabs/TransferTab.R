# Transfer Tab
add_transfer_subtab <- 
  tabPanel(
    "Add Transfer",
    sidebarLayout(
      sidebarPanel(
        pickerInput(
          inputId = "transfer_season",
          label = "Season",
          choices = 1:10,
          multiple = F
        ),
        radioButtons(
          inputId = "transfer_window",
          label = "Window",
          choices = c("Summer", "Winter"),
          selected = "Summer"
        ),
        radioButtons(
          "transfer_type",
          "Type",
          choices = c("In", "Out"),
          selected = NULL
        ),
        textInput("transfer_name", "Player"),
        numericInput("transfer_fee", "Fee (M)", value = 0, min = 0),
        actionButton("transfer_add", "Add Transfer"),
        downloadButton("downloadFile_transfer", "Download Transfer Record")
      ),
      mainPanel(
        column(6,
               h3("In"),
               DTOutput("transfer_in_dt")),
        column(6,
               h3("Out"),
               DTOutput("transfer_out_dt"))
      )
    )
  )


vis_transfer_subtab <- 
  tabPanel(
    "Ranking Transfer",
    sidebarLayout(
      sidebarPanel(
        pickerInput(
          inputId = "vis_transfer_position",
          label = "Position",
          choices = c("ST", "CF", "LW", "RW", "CAM",
                      "LM", "CM", "CDM", "RM",
                      "LWB", "LB", "CB", "RB", "RWB",
                      "GK"),
          multiple = T,
          options = list(`action-button` = T)
        ),
        radioButtons(
          "vis_transfer_type",
          "Type",
          choices = c("In", "Out"),
          selected = NULL
        )
      ),
      mainPanel(
        h3("Plot"),
        plotlyOutput("vis_transfer")
      )
    )
  )

transfer_tab <- 
  tabPanel(
    "Transfer",
    tabsetPanel(
      add_transfer_subtab,
      vis_transfer_subtab
    )
  )