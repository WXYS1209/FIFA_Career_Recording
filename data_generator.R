if (!dir.exists("./data")) {
  dir.create("./data")
}

setwd("./data")

df_match <- data.frame(
  Name = character(),
  Pos = character(),
  Played = integer(),
  Started = integer(),
  MOTM = integer(),
  Goals = integer(),
  Assists = integer(),
  Shots = integer(),
  Shot_Comp = integer(),
  Pass = integer(),
  Pass_Comp = integer(),
  Key_Pass = integer(),
  Dribble = integer(),
  Dribble_Comp = integer(),
  Tackle = integer(),
  Tackle_Comp = integer(),
  Possession_Won = integer(),
  Possession_Lost = integer(),
  Distance = numeric(),  # Assuming 'Distance' might be a decimal value
  Rating = numeric(),  # Assuming 'Rating' might be a decimal value
  stringsAsFactors = FALSE # Ensure character data does not convert to factors
)

df_match_stat = data.frame(
  Against = character(),
  HA = character(),
  GF = integer(),
  GA = integer(),
  Competition = character(),
  WLD = character(),
  Possession = numeric()
)

df_transfer = data.frame(
  Name = character(),
  Pos	= character(),
  Season = integer(),
  Window = character(),
  Type = character(),
  Fee = numeric()
)

write.csv(df_transfer,
          "Transfer_Info.csv",
          row.names = F)

write.csv(df_match,
          "OverAllSeasons.csv",
          row.names = F)

for (i in 1:10) {
  dir.create(paste0("Season", i))
  write.csv(df_match_stat,
            paste0("./Season", i,
                   "/Match_Stat.csv"),
            row.names = F)
  
  write.csv(df_match,
            paste0("./Season", i,
                   "/OverAll.csv"),
            row.names = F)
  
  for (comp in c("PL", "EC", "CC", "FA", "Other")) {
    dir.create(paste0("Season", i, "/", comp))
    write.csv(data_df, 
              paste0("Season", i, "/", comp, "/",
                     "merged_data_", comp, ".csv"),
              row.names = F)
  }
}

