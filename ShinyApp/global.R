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

metadata = read.csv("./data/player-data-full.csv")
metadata = metadata %>% select(name, image)

col_bars = c("#51b48c", "#cf3d6e", "#7ab743",
             "#7b62cb", "#c49644", "#c364b9", 
             "#6a803a", "#688dcd", "#c95a38",
             "#c26b7e")

competition_colors = c(
  "PL" = "#38003d", "EC" = "royalblue", "CC" = "#008f5e",
  "FA" = "#d71921", "Other" = "#ead945")

positions_colors = c(
  "CF" = "#d80a0e",
  "CAM" = "#f45c5c",
  "ST" = "#d80a0e",
  "LW" = "#fb658b",
  "RW" = "#fb658b",
  "CM" = "#73cce4",
  "CDM" = "#116e8d",
  "LM" = "#73cce4",
  "RM" = "#73cce4",
  "CB" = "black",
  "LB" = "#2f3642",
  "RB" = "#2f3642",
  "LWB" = "#4d547a",
  "RWB" = "#4d547a",
  "GK" = "#9dce74"
)