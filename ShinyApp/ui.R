source("./tabs/UpdateDataTab.R")
source("./tabs/OverviewTab.R")
source("./tabs/TransferTab.R")
source("./tabs/PlayerTab.R")

ui <- fluidPage(
  theme = shinytheme("cerulean"),
  # useShinyjs(),
  # custom_styles,
  # custom_styles_code,
  navbarPage(
    "FIFA Match Data Tracker",
    updata_data_tab,
    overview_tab,
    transfer_tab,
    player_tab
  )
)
