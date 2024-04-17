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

plot_match_stat = function(df, competition, where) {
  df = df %>% 
    filter(Competition %in% competition,
           HA %in% where) %>% 
    as.data.frame()
  if (dim(df)[1] == 0) {
    p_sc = ggplot(df, aes(x = c(0,1), y = c(0,1))) + 
      annotate("text",x=0.5,y=0.5,
               label="Please select another competition.") + 
      labs(x = "", y = "") + 
      theme(plot.caption = element_text(size = 10))
    p_sc
  } else{
    df = df %>%
      mutate(game = 1:dim(df)[1],
             diff = abs(GF - GA))
    dff = df %>% 
      pivot_longer(c(GF, GA, diff), names_to = "goal_type")
    # filter(goal_type %in% type) %>% 
    p1 = ggplot() + 
      geom_col(data = dff,
               aes(x = game, y = value, alpha = WLD, fill = goal_type),
               position = "dodge") +
      geom_line(data = df,
                aes(x = game, y = Possession / 10),
                color = "royalblue") +
      ylim(0, 10) +
      labs(x = "Game Number",
           y = "Goals") +
      scale_alpha_manual(values = c("W" = 1, "L" = 0.5, "D" = 0.75)) + 
      theme_minimal()
    ggplotly(p1)
    # p1
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
