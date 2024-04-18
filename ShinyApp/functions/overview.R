plot_radar = function(df) {
  data_radar = df %>% 
    mutate(Shots = Shots / Played,
           Pass = Pass / Played,
           Key_Pass = Key_Pass / Played,
           Dribble = Dribble / Played,
           Tackle = Tackle / Played,
           Possession_Won = Possession_Won / Played,
           Possession_Lost = Possession_Lost / Played) %>% 
    pivot_longer(!Name & !Pos & !Shot_Comp & !Pass_Comp &
                   !Dribble_Comp & !Tackle_Comp, 
                 names_to = "Var", values_to = "value")
  
  figr = plot_ly(data_radar, 
                 type = "scatterpolar",
                 mode = "lines+markers",
                 theta = ~Var,
                 r = ~value,
                 line = list(shape = "spline"),
                 marker = list(symbol = "circle"),
                 hoverinfo = 'text',
                 text = ~paste('</br> Player: ', Name,
                               '</br> Position: ', Pos),
                 color=~factor(Pos)) %>%
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


plot_classification = function(df, position){
  
}



# 
# aa = read.csv("./data/Season1/OverAll.csv")
# aa = aa %>% 
#   filter(# Pos %in% c("ST", "CF", "CAM", "LW", "RW"),
#          Played > 0) %>% 
#   mutate(Shot_Acc = Shot_Comp / Shots,
#          Pass_Acc = Pass_Comp / Pass,
#          Dribble_Acc = Dribble_Comp / Dribble,
#          Tackle_Acc = Tackle_Comp / Tackle,
#          Rating = Rating / Played,
#          Distance = Distance / Played)
# aa[is.na(aa)] = 0
# plot(aa$Goals, aa$Assists)

# 
# library(tidymodels)
# library(tidytext)
# 
# get_pca_rec = function(DF){
#   PCA = recipe(~., data = DF) %>%
#     update_role(Name, Pos, new_role = "id") %>%
#     step_normalize(all_predictors()) %>%
#     step_pca(all_predictors())
# }
# pca_rec = get_pca_rec(aa[,-c(9,11,14,16)])
# pca_prep = prep(pca_rec)
# components = tidy(pca_prep, 2)
# # scores = juice(pca_prep)
# variances = tidy(pca_prep, 2, type = "variance")
# 
# X_prcomp = prcomp(aa[,-c(1:2, 9,11,14,16)], center = TRUE,
#                   scale. = TRUE,
#                   retx = TRUE)
# scores_pca = data.frame(Name = aa$Name, 
#                         Pos = aa$Pos, 
#                         X_prcomp$x)
# 
# variances %>% filter(terms == "cumulative percent variance") %>% 
#   head(50) %>%
#   ggplot() +
#   geom_col(aes(component, value), fill = "orange2")
# 
# components_ = components %>%
#   filter(component %in% str_c("PC", 1:3)) %>%
#   mutate(terms = reorder_within(terms, abs(value), component))
# 
# ggplot(components_, aes(value, terms)) +
#   geom_col(show.legend = FALSE, fill = "royalblue") +
#   facet_wrap(~ component, scales = "free", ncol = 3) +
#   scale_y_reordered() +
#   labs(y = NULL, x=NULL) +
#   theme(axis.text = element_text(size = 5)) +
#   ggtitle("Non-Goalkeeper")
# 
#  
# p = ggplot(scores_pca) +
#   geom_point(aes(x=PC1, y = PC2, col = Pos)) +
#   theme(legend.text=element_text(size=6))
# 
# ggplotly(p)
