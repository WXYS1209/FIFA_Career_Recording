library(shiny)
library(readxl)
library(DT)
library(shinyWidgets)
library(shinythemes)
library(plotly)
library(tidyverse)
source("./functions/overview.R")
source("./functions/player.R")
source("./functions/update_data.R")

seasons_global = list.files("./data/")[grepl("^Season", 
                                      list.files("./data/"))]
seasons_global = gsub("Season", "", seasons_global)