server <- function(input, output, session) {

  # Update Data
  matchData <- reactiveVal(data.frame())
  wholeData <- reactiveVal(data.frame())
  
  observeEvent(input$fileUpload, {
    req(input$fileUpload)
    df <- read_csv(input$fileUpload$datapath)
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
      Dribble = as.integer(input$dribble),
      Dribble_Comp = as.integer(input$dribble_comp),
      Tackle = as.integer(input$tackle),
      Tackle_Comp = as.integer(input$tackle_comp),
      Possession_Won = as.integer(input$poss_won),
      Possession_Lost = as.integer(input$poss_lost),
      Distance = as.double(input$dist),
      Rating = as.double(input$rating)
    )
    # 合并新数据
    current_data <- matchData()
    updated_data <- rbind(current_data, new_data)
    matchData(updated_data)  # 更新数据
  })
  
  # 渲染更新后的数据表
  output$updatedData <- renderDT({
    datatable(matchData(), options = list(pageLength = 20))
  })
  
  # 提供下载更新后的数据文件
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("match_data_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(matchData(), file, row.names = FALSE)
    }
  )
  
  # Merge Data
  newmatchData <- reactiveVal(data.frame())
  
  observeEvent(input$fileUpload_merge, {
    req(input$fileUpload_merge)
    df <- read_csv(input$fileUpload_merge$datapath)
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
                 Pass, Pass_Comp, Dribble, Dribble_Comp, Tackle, Tackle_Comp, 
                 Possession_Won, Possession_Lost, Distance, Rating),
               ~ sum(.x, na.rm = TRUE))
      )
    wholeData(df)
  })
  
  output$updatedData_merge <- renderDT({
    datatable(wholeData(), options = list(pageLength = 20))
  })
}
