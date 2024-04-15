source("./tabs/UpdateDataTab.R")
source("./tabs/visualizationTab.R")

ui <- fluidPage(
  theme = shinytheme("cerulean"),
  # useShinyjs(),
  # custom_styles,
  # custom_styles_code,
  navbarPage(
    "FIFA Match Data Tracker",
    tabsetPanel(id = "mainTab",
                updata_data_tab,
                visualization_tab
                
    )
  )
)
