source("./tabs/UpdateDataTab.R")
source("./tabs/RankingTab.R")
source("./tabs/TransferTab.R")

ui <- fluidPage(
  theme = shinytheme("cerulean"),
  # useShinyjs(),
  # custom_styles,
  # custom_styles_code,
  navbarPage(
    "FIFA Match Data Tracker",
    updata_data_tab,
    ranking_tab,
    transfer_tab
  )
)
