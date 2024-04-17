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
        pickerInput(
          inputId = "transfer_window",
          label = "Window",
          choices = c("Summer", "Winter"),
          selected = "Summer",
          multiple = F
        ),
        radioButtons("transfer_type", "Type", 
                     choices = c("In", "Out"),
                     selected = "In"),
        textInput("transfer_name", "Player"),
        pickerInput(
          inputId = "transfer_position",
          label = "Position",
          choices = c("ST", "CF", "LW", "RW", "CAM",
                      "LM", "CM", "CDM", "RM",
                      "LWB", "LB", "CB", "RB", "RWB",
                      "GK"),
          multiple = F
        ),
        numericInput("transfer_fee", "Fee (M)", value = 0, min = 0),
        actionButton("transfer_add", "Add Transfer"),
        actionButton("updateFile_transfer", 
                     "Update Transfer Record"),
        actionButton("transfer_undo", "Undo")
      ),
      mainPanel(
        h3("New Transfers"),
        DTOutput("transfer_dt"),
        h3("All Transfers"),
        DTOutput("transfer_overview_dt")
      )
    )
  )


vis_transfer_subtab <- 
  tabPanel(
    "Overview Transfer",
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
          selected = "ST",
          options = pickerOptions(actionsBox = T)
        ),
        pickerInput(
          "vis_transfer_type",
          "Type",
          choices = c("In", "Out"),
          selected = "In",
          multiple = T
        )
      ),
      mainPanel(
        h3("Transfer Overview"),
        dataTableOutput("vis_transfer_dt")
      )
    ),
    fluidRow(
      column(6, plotlyOutput("vis_transfer_fee")),
      column(6, plotlyOutput("vis_transfer_rank"))
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