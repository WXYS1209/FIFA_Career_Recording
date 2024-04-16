plot_stat = function(df, variable){
  if (dim(df)[1] == 0) {
    p_sc = ggplot(df, aes(x = c(0,1), y = c(0,1))) + 
      annotate("text",x=0.5,y=0.5,
               label="Please select another competition.") + 
      labs(x = "", y = "") + 
      theme(plot.caption = element_text(size = 10))
    ggplotly(p_sc)
  } else{
    variable = rlang::sym(variable)
    p = df %>% 
      arrange(-!!variable) %>% 
      filter(!!variable > 0) %>% 
      head(5) %>% 
      ggplot(aes(x = !!variable, y = reorder(Name, !!variable))) + 
      geom_col(fill = "royalblue", width = 0.5) + 
      labs(y = "",
           title = paste0(variable)) +
      theme_minimal()
    ggplotly(p)
  }

}

plot_match_stat = function(df, competition, where, type) {
  df = df %>% 
    filter(Competition %in% competition,
           HA %in% where)
  if (dim(df)[1] == 0) {
    p_sc = ggplot(df, aes(x = c(0,1), y = c(0,1))) + 
      annotate("text",x=0.5,y=0.5,
               label="Please select another competition.") + 
      labs(x = "", y = "") + 
      theme(plot.caption = element_text(size = 10))
    p_sc
  } else{
    p1 = df %>%
      mutate(game = 1:dim(df)[1],
             diff = GF - GA) %>% 
      pivot_longer(c(GF, GA, diff), names_to = "goal_type") %>% 
      filter(goal_type %in% type) %>% 
      ggplot(aes(x = game, y = value,
                 fill = WLD)) + 
      geom_col() +
      labs(x = "Game Number",
           y = "Goals",
           title = paste0(type, " for ", competition, " in ",
                          where, " game")) +
      theme_minimal()
    # ggplotly(p1)
    p1
  }
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
