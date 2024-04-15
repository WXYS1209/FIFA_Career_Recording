server <- function(input, output, session) {

  #### Update Data ####
  matchData <- reactiveVal(data.frame()) # 存储每场比赛数据
  wholeData <- reactiveVal(data.frame()) # 存储上传的累计数据
  
  observeEvent(input$fileUpload, {
    req(input$fileUpload)
    df <- read.csv(input$fileUpload$datapath)
    updatePickerInput(session, "player", choices = df$Name)
    wholeData(df)
  })
  
  # 添加比赛数据
  observeEvent(input$add, {
    req(matchData())  # 确保数据已加载
    motm_val <- if (input$motm) 1 else 0
    started_val <- if (input$starter) 1 else 0
    
    new_data <- data.frame(
      Name = input$player,
      Pos = NA,
      Played = 1,
      Started = started_val,
      MOTM = motm_val,
      Goals = as.integer(input$goals),
      Assists = as.integer(input$assists),
      Shots = as.integer(input$shot),
      Shot_Comp = as.integer(input$shot_comp),
      Pass = as.integer(input$pass),
      Pass_Comp = as.integer(input$pass_comp),
      Key_Passes = as.integer(input$key_pass),
      Dribble = as.integer(input$dribble),
      Dribble_Comp = as.integer(input$dribble_comp),
      Tackle = as.integer(input$tackle),
      Tackle_Comp = as.integer(input$tackle_comp),
      Possession_Won = as.integer(input$poss_won),
      Possession_Lost = as.integer(input$poss_lost),
      Distance = as.double(input$dist),
      Rating = as.double(input$rating)
    )

    current_data <- matchData()
    updated_data <- rbind(current_data, new_data)
    matchData(updated_data)
  })
  
  output$updatedData <- renderDT({
    datatable(matchData(),
              options = list(scrollY = 500,
                             scrollX = 500,
                             deferRender = TRUE,
                             pageLength = 20,
                             autoWidth = T)
              )
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("match_data_", input$competition, "_", 
            input$match, ".csv", sep="")
    },
    content = function(file) {
      write.csv(matchData(), file, row.names = FALSE)
    }
  )
  
  #### Merge Data ####
  newmatchData <- reactiveVal(data.frame()) # 存储上传的比赛数据
  
  observeEvent(input$fileUpload_merge, {
    req(input$fileUpload_merge)
    df <- read.csv(input$fileUpload_merge$datapath)
    newmatchData(df)
  })
  
  observeEvent(input$merge, {
    req(newmatchData())  # 确保数据已加载
    req(wholeData())
    temp_whole = wholeData()
    temp_match = newmatchData()
    
    df <- temp_whole %>% 
      rbind(temp_match) %>% 
      group_by(Name) %>% 
      summarise(
        Pos = Pos[!is.na(Pos)],
        across(c(Played, Started, MOTM, Goals, Assists, Shots, Shot_Comp, 
                 Pass, Pass_Comp, Key_Pass, Dribble, Dribble_Comp, Tackle, Tackle_Comp, 
                 Possession_Won, Possession_Lost, Distance, Rating),
               ~ sum(.x, na.rm = TRUE))
      )
    wholeData(df[!duplicated(df),])
  })
  
  output$updatedData_merge <- renderDT({
    datatable(wholeData(), 
              options = list(scrollY = 500,
                             scrollX = 500,
                             deferRender = TRUE,
                             pageLength = 20,
                             autoWidth = T
              ))
  })
  
  output$downloadData_merge <- downloadHandler(
    filename = function() {
      paste("merged_data_", input$competition, "_to_", 
            input$match, ".csv", sep="")
    },
    content = function(file) {
      write.csv(wholeData(), file, row.names = FALSE)
    }
  )
  
  #### Current Season ####
  season_df = reactiveVal(data.frame())
  observeEvent(input$vis_season, {
    file_name = paste0("../data/Season",
                       input$vis_season,
                       "/OverAll.csv")
    if (file.exists(file_name)) {
      df = read.csv(file_name) %>% 
        mutate(Shot_Acc = Shot_Comp / Shots,
               Pass_Acc = Pass_Comp / Pass,
               Dribble_Acc = Dribble_Comp / Dribble,
               Tackle_Acc = Tackle_Comp / Tackle)
    }
    else{
      df = NULL
    }
    season_df(df)
  })
  
  output$vis_radar = renderPlotly({
    data_radar = season_df() %>% 
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
    
  })
  
  output$vis_1 = renderPlotly({
    plot_stat(season_df(), get_variable(input$vis_variable)[1])
  })
  
  output$vis_2 = renderPlotly({
    plot_stat(season_df(), get_variable(input$vis_variable)[2])
  })
}
