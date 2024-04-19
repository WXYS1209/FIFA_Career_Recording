clean_merged_data = function(df) {
  df = df %>% 
    group_by(Name) %>% 
    reframe(
      Pos = Pos[!is.na(Pos)],
      across(c(Played, Started, MOTM, 
               Goals, Assists, Shots, Shot_Comp, 
               Pass, Pass_Comp, Key_Pass, 
               Dribble, Dribble_Comp, 
               Tackle, Tackle_Comp, 
               Possession_Won, Possession_Lost, 
               Distance, Rating),
             ~ sum(.x, na.rm = TRUE))
    )
  df = df[!duplicated(df),]
  return(df)
}

update_current_season = function(season){
  df_overall = data.frame()
  for (comp in c("PL", "EC", "CC", "FA", "Other")){
    temp = read.csv(paste0("./data/Season",
                           season,
                           "/", comp,
                           "/merged_data_", comp,
                           ".csv"))
    df_overall = rbind(df_overall, temp)
  }
  
  df_overall = clean_merged_data(df_overall)
  
  write.csv(df_overall,
            paste0("./data/Season",
                   season,
                   "/OverAll.csv"),
            row.names = F)
}

update_all_seasons = function(){
  df_allseasons = data.frame()
  for (i in 1:seasons_global){
    temp = read.csv(paste0("./data/Season",
                           i,
                           "/OverAll.csv"))
    df_allseasons = rbind(df_allseasons, temp)
  }
  
  df_allseasons = clean_merged_data(df_allseasons)
  
  write.csv(df_allseasons,
            paste0("./data/OverAllSeasons.csv"),
            row.names = F)
  
}