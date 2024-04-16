plot_stat = function(df, variable){
  variable = rlang::sym(variable)
  p = df %>% 
    arrange(-!!variable) %>% 
    filter(!!variable > 0) %>% 
    head(5) %>% 
    ggplot(aes(x = !!variable, y = reorder(Name, !!variable))) + 
    geom_col(fill = "royalblue", width = 0.5) + 
    labs(y = "",
         title = paste0("Ranking of ", variable)) +
    theme_minimal()
  return(ggplotly(p))
}

get_variable = function(name){
  if (name ==  "Goals & Distance"){
    return(c("Goals", "Distance"))
  }
  if (name == "Assisting") {
    return (c("Assists", "Key_Pass"))
  }
  if (name == "Shooting") {
    return (c("Shots", "Shot_Acc"))
  }
  if (name == "Passing") {
    return (c("Pass", "Pass_Acc"))
  }
  if (name == "Dribbling") {
    return (c("Dribble", "Dribble_Acc"))
  }
  if (name == "Tackling") {
    return (c("Tackle", "Tackle_Acc"))
  }
  if (name == "Possession") {
    return (c("Possession_Won", "Possession_Lost"))
  }
  if (name == "Rating & Appearance") {
    return (c("Rating", "Played"))
  }
}
