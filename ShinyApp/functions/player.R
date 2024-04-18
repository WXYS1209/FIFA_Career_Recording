plot_player_stat = function(df, variable){
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
      ggplot(aes(x = 1:dim(df)[1], y = !!variable)) + 
      geom_line(col = "royalblue") +
      geom_col(aes(fill = Competition)) +
      ylim(0, NA) + 
      labs(y = "",
           title = paste0(variable)) +
      theme_minimal()
    ggplotly(p)
  }
  
}