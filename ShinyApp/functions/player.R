plot_player_stat = function(df, variable, player){
  if (dim(df)[1] == 0) {
    p_sc = ggplot(df, aes(x = c(0,1), y = c(0,1))) + 
      annotate("text",x=0.5,y=0.5,
               label="Please select another competition.") + 
      labs(x = "", y = "") + 
      theme(plot.caption = element_text(size = 10))
    ggplotly(p_sc)
  } else{
    variable = rlang::sym(variable)
    df = df %>% 
      filter(Name == player) 
    p = df %>% 
      ggplot(aes(x = 1:dim(df)[1], y = !!variable)) + 
      geom_line(aes(color = Pos)) +
      geom_col(aes(fill = Competition), alpha = 0.5) +
      scale_fill_manual(values = competition_colors) + 
      scale_color_manual(values = positions_colors) + 
      ylim(0, NA) + 
      labs(y = "",
           x = "Games Played",
           title = paste0(variable, " for ", player)) +
      theme_minimal()
    ggplotly(p)
  }
  
}


plot_radar_player = function(df, variables) {
  data_radar = clean_merged_data(df) %>% 
    mutate(
      Shot_Acc = Shot_Comp / Shots,
      Pass_Acc = Pass_Comp / Pass,
      Dribble_Acc = Dribble_Comp / Dribble,
      Tackle_Acc = Tackle_Comp / Tackle,
      Shots = Shots / Played,
      Pass = Pass / Played,
      Key_Pass = Key_Pass / Played,
      Dribble = Dribble / Played,
      Tackle = Tackle / Played,
      Distance = Distance / Played,
      Rating = Rating / Played,
      Possession_Won = Possession_Won / Played,
      Possession_Lost = Possession_Lost / Played) %>% 
    pivot_longer(!Name & !Pos & !Shot_Comp & !Pass_Comp &
                   !Dribble_Comp & !Tackle_Comp, 
                 names_to = "Var", values_to = "value") %>% 
    filter(Var %in% variables)
  
  figr = plot_ly(data_radar, 
                 type = "scatterpolar",
                 mode = "lines+markers+fill",
                 fill = "toself",
                 # fillcolor = ~I(sapply(colors, function(x) paste0(x, "40"))),
                 theta = ~Var,
                 r = ~value,
                 line = list(shape = "spline"),
                 marker = list(symbol = "circle"),
                 hoverinfo = 'text',
                 text = ~paste('</br> Player: ', Name,
                               '</br> Position: ', Pos),
                 color=~factor(Name),
                 colors = c("royalblue", "tomato")) %>%
    layout(
      polar = list(
        radialaxis = list(
          visible = TRUE
        )
      ),
      showlegend = T
    )
  
  figr
}