library(shiny)
library(readxl)
library(DT)
library(shinyWidgets)
library(shinythemes)
source("./tabs/UpdateDataTab.R")

ui <- fluidPage(
  theme = shinytheme("cerulean"),
  # useShinyjs(),
  # custom_styles,
  # custom_styles_code,
  navbarPage(
    "FIFA Match Data Tracker",
    tabsetPanel(id = "mainTab",
                updata_data_tab
                
    )
  )
)
