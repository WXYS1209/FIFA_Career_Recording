server = function(input, output, session) {
  # Update Seasons
  observe({
    updatePickerInput(
      session,
      "season",
      choices = seasons_global,
      selected = "1"
    )
    
    updatePickerInput(
      session,
      "season_merge",
      choices = seasons_global,
      selected = "1"
    )
    
    updatePickerInput(
      session,
      "match_stat_season",
      choices = seasons_global,
      selected = "1"
    )
    
    updatePickerInput(
      session,
      "vis_season",
      choices = seasons_global,
      selected = "1"
    )
    
    updatePickerInput(
      session,
      "transfer_season",
      choices = seasons_global,
      selected = "1"
    )
    
    updatePickerInput(
      session,
      "player_season",
      choices = seasons_global,
      selected = "1"
    )
  })
  #### Record Data ####
  matchData = reactiveValues(dt = data.frame(), history = list(NULL)) # Store match data
  startData = reactiveVal(data.frame()) # Store Season_Start
  
  # Read Season_Start.csv for selected season
  observeEvent(input$season, {
    file_name = paste0("./data/Season",
                       input$season,
                       "/Season_Start.csv")
    if (file.exists(file_name)) {
      df = read.csv(file_name)
      updatePickerInput(session, "player", choices = df$Name)
    }
    else{
      df = NULL
      updatePickerInput(session, "player", 
                        choices = "Season not started")
    }
    
    startData(df)
  })
  
  # Add match data
  observeEvent(input$add, {
    motm_val = if (input$motm) 1 else 0
    started_val = if (input$starter) 1 else 0
    
    new_data = data.frame(
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
      Key_Pass = as.integer(input$key_pass),
      Dribble = as.integer(input$dribble),
      Dribble_Comp = as.integer(input$dribble_comp),
      Tackle = as.integer(input$tackle),
      Tackle_Comp = as.integer(input$tackle_comp),
      Possession_Won = as.integer(input$poss_won),
      Possession_Lost = as.integer(input$poss_lost),
      Distance = as.double(input$dist),
      Rating = as.double(input$rating)
    )
    
    current_data = matchData$dt
    updated_data = rbind(current_data, new_data)
    matchData$history[[length(matchData$history)+1]] = current_data
    matchData$dt = updated_data
  })
  
  # Display data added
  output$updatedData_dt = renderDT({
    datatable(matchData$dt,
              editable = T,
              options = list(scrollY = 500,
                             scrollX = 500,
                             deferRender = TRUE,
                             pageLength = 20,
                             autoWidth = T)
    )
  })
  # Fix
  observeEvent(input$updatedData_dt_cell_edit, {
    info = input$updatedData_dt_cell_edit
    temp = matchData$dt
    matchData$history[[length(matchData$history)+1]] = temp
    
    temp[info$row, info$col] = info$value
    matchData$dt = temp
  })
  # Download
  output$downloadData = downloadHandler(
    filename = function() {
      paste("match_data_", input$competition, "_", 
            input$match, ".csv", sep="")
    },
    content = function(file) {
      write.csv(matchData$dt, file, row.names = FALSE)
    }
  )
  # Undo
  observeEvent(input$record_undo, {
    if(length(matchData$history) > 1) {
      matchData$dt = tail(matchData$history, 1)[[1]]
      matchData$history = matchData$history[-length(matchData$history)]
    }
  })
  
  
  #### Merge Data ####
  newmatchData = reactiveVal(data.frame()) # Store uploaded match records
  aggregatedData = reactiveVal(data.frame()) # Store aggregated record
  
  # Read Match Records
  observeEvent(input$fileUpload_merge, {
    inFile = input$fileUpload_merge
    if (is.null(inFile)) {
      return(NULL)
    }
    
    # Read all files
    data_list = lapply(inFile$datapath, read.csv)
    
    # Merge all files
    if (length(data_list) > 1) {
      df = do.call(rbind, data_list)
    } else {
      df = data_list[[1]]
    }
    newmatchData(df)
  })
  
  # Read Aggregated Record
  observe({
    # if (input$competition_merge != "All" & input$season_merge != "All"){
    file_name = paste0("./data/Season", input$season_merge,
                       "/", input$competition_merge, "/merged_data_",
                       input$competition_merge,
                       ".csv")
    df = read.csv(file_name)
    aggregatedData(df)
    # }
    # else {
    #   aggregatedData(data.frame())
    # }
    
  })
  
  # Merge and update aggregated record
  observeEvent(input$merge, {
    temp_aggr = aggregatedData()
    temp_match = newmatchData()
    
    df = clean_merged_data(rbind(temp_aggr, temp_match))
    aggregatedData(df)
  })
  
  # Display merged aggregated record
  output$updatedData_merge_dt = renderDT({
    datatable(aggregatedData(), 
              options = list(scrollY = 500,
                             scrollX = 500,
                             deferRender = TRUE,
                             pageLength = 20,
                             autoWidth = T
              ))
  })
  
  # Rewrite
  observeEvent(input$updateData_merge, {
    # if (input$competition_merge != "All" &
    #     input$season_merge != "All"){
    write.csv(aggregatedData(), 
              paste0("./data/Season", input$season_merge,
                     "/", input$competition_merge, "/merged_data_",
                     input$competition_merge,
                     ".csv"),
              row.names = F
    )
    update_current_season(input$season_merge)
    update_all_seasons()
    # }
    # if (input$competition_merge == "All" & input$season_merge != "All") {
    # write.csv(aggregatedData(), 
    #           paste0("./data/Season", input$season_merge,
    #                  "/OverAll.csv"),
    #           row.names = F
    # )
    # }
    # if (input$season_merge == "All") {
    # write.csv(aggregatedData(), 
    #           paste0("./data/OverAllSeasons.csv"),
    #           row.names = F
    # )
    # }
  })
  # output$updateData_merge = downloadHandler(
  #   filename = function() {
  #     paste0("merged_data_", input$competition_merge, ".csv")
  #   },
  #   content = function(file) {
  #     write.csv(aggregatedData(), file, row.names = FALSE)
  #   }
  # )
  
  #### Match Stat ####
  matchstatData = reactiveValues(dt = data.frame(), history = list(NULL))
  
  observeEvent(input$match_stat_season, {
    file_name = paste0("./data/Season",
                       input$match_stat_season,
                       "/Match_Stat.csv")
    df = read.csv(file_name)
    matchstatData$dt = df
    # matchstatData$history[[length(matchstatData$history)+1]] = matchstatData$dt
  })
  
  observeEvent(input$add_match_stat, {
    new_data = data.frame(
      Against = input$opponent,
      HA = input$where,
      GF = input$gf,
      GA = input$ga,
      Possession = as.numeric(input$possession),
      Competition = input$match_stat_comp
    ) %>% 
      mutate(WLD = if_else(GF > GA, "W", if_else(GF < GA, "L", "D")))
    current_data = matchstatData$dt
    matchstatData$history[[length(matchstatData$history)+1]] = matchstatData$dt
    updated_data = rbind(current_data, new_data)
    matchstatData$dt = updated_data
  })
  
  # Display data added
  output$updatedMatchStat_dt = renderDT({
    datatable(matchstatData$dt,
              rownames = T,
              editable = T,
              options = list(scrollY = 500,
                             scrollX = 500,
                             deferRender = TRUE,
                             pageLength = 20,
                             autoWidth = F)
    )
  })
  
  # Fix
  observeEvent(input$updatedMatchStat_dt_cell_edit, {
    info = input$updatedMatchStat_dt_cell_edit
    temp = matchstatData$dt
    matchstatData$history[[length(matchstatData$history)+1]] = temp
    
    temp[info$row, info$col] = info$value
    matchstatData$dt = temp
  })
  
  # Undo
  observeEvent(input$match_stat_undo, {
    if(length(matchstatData$history) > 1) {
      matchstatData$dt = tail(matchstatData$history, 1)[[1]]
      matchstatData$history = matchstatData$history[-length(matchstatData$history)]
    }
  })
  
  # Rewrite
  observeEvent(input$updateMatchStat, {
    write.csv(matchstatData$dt, 
              paste0("./data/Season",
                     input$match_stat_season,
                     "/Match_Stat.csv"),
              row.names = F)
  })
  
  
  #### Current Season ####
  season_df = reactiveVal(data.frame())
  season_match_df = reactiveVal(data.frame())
  
  observe({
    if (length(input$vis_comp) == 0){
      df = data.frame()
    } else{
      file_names = c()
      for (i in 1:length(input$vis_comp)){
        file_name = paste0("./data/Season",
                           input$vis_season,
                           "/", input$vis_comp[i],
                           "/merged_data_", input$vis_comp[i],
                           ".csv")
        file_names = c(file_names, file_name)
      }
      # print(file_names)
      
      # Read all files
      data_list = lapply(file_names, read.csv)
      
      # Merge all files
      if (length(data_list) > 1) {
        df = do.call(rbind, data_list)
      } else {
        df = data_list[[1]]
      }
      
      df = clean_merged_data(df)
      
      df = df %>% 
        mutate(Shot_Acc = Shot_Comp / Shots,
               Pass_Acc = Pass_Comp / Pass,
               Dribble_Acc = Dribble_Comp / Dribble,
               Tackle_Acc = Tackle_Comp / Tackle,
               Rating = Rating / Played)
      
    }
    
    file_name_match = paste0("./data/Season",
                             input$vis_season,
                             "/Match_Stat.csv")
    
    
    df_match = read.csv(file_name_match)
    
    season_df(df)
    season_match_df(df_match)
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
  
  output$vis_match = renderPlotly({
    plot_match_stat(season_match_df(), 
                    input$vis_comp, 
                    input$vis_where)
  })
  
  #### Overall ####
  # overall_df = reactiveVal(data.frame())
  
  file_name_all = "./data/OverAllSeasons.csv"
  if (file.exists(file_name_all)) {
    df_all_seasons = read.csv(file_name_all) %>% 
      mutate(Shot_Acc = Shot_Comp / Shots,
             Pass_Acc = Pass_Comp / Pass,
             Dribble_Acc = Dribble_Comp / Dribble,
             Tackle_Acc = Tackle_Comp / Tackle,
             Rating = Rating / Played,
             Distance = Distance / Played)
  }
  else{
    df_all_seasons = NULL
  }
  
  # output$vis_overall_1 = renderPlotly({
  #   plot_stat(df_all_seasons, get_variable(input$vis_variable_overall)[1])
  # })
  # output$vis_overall_2 = renderPlotly({
  #   plot_stat(df_all_seasons, get_variable(input$vis_variable_overall)[2])
  # })
  output$vis_overall_dt_goals = renderDT({
    datatable(
      df_all_seasons %>% 
        arrange(-Goals) %>% 
        filter(Goals > 0) %>% 
        select(Name, Pos, Goals),
      rownames = F,
      options = list(
        scrollY = 300,
        scrollX = 300,
        deferRender = TRUE,
        pageLength = 20,
        dom = "tip",
        autoWidth = F
      )
    )
  })
  output$vis_overall_dt_assists = renderDT({
    datatable(
      df_all_seasons %>% 
        arrange(-Assists) %>% 
        filter(Assists > 0) %>% 
        select(Name, Pos, Assists),
      rownames = F,
      options = list(
        scrollY = 300,
        scrollX = 300,
        deferRender = TRUE,
        pageLength = 20,
        dom = "tip",
        autoWidth = F
      )
    )
  })
  output$vis_overall_dt_games = renderDT({
    datatable(
      df_all_seasons %>% 
        arrange(-Played) %>% 
        filter(Played > 0) %>% 
        select(Name, Pos, Played),
      rownames = F,
      options = list(
        scrollY = 300,
        scrollX = 300,
        deferRender = TRUE,
        pageLength = 20,
        dom = "tip",
        autoWidth = F
      )
    )
  })
  output$vis_overall_dt_rating = renderDT({
    datatable(
      df_all_seasons %>% 
        arrange(-Rating) %>%
        filter(Rating > 0) %>% 
        select(Name, Pos, Rating),
      rownames = F,
      options = list(
        scrollY = 300,
        scrollX = 300,
        deferRender = TRUE,
        pageLength = 20,
        dom = "tip",
        autoWidth = F
      )
    )
  })
  output$vis_overall_dt_tackles = renderDT({
    datatable(
      df_all_seasons %>% 
        arrange(-Tackle_Comp) %>% 
        filter(Tackle_Comp > 0) %>% 
        select(Name, Pos, Tackle_Comp),
      rownames = F,
      options = list(
        scrollY = 300,
        scrollX = 300,
        deferRender = TRUE,
        pageLength = 20,
        dom = "tip",
        autoWidth = F
      )
    )
  })
  output$vis_overall_dt_distance = renderDT({
    datatable(
      df_all_seasons %>% 
        arrange(-Distance) %>% 
        select(Name, Pos, Distance),
      rownames = F,
      options = list(
        scrollY = 300,
        scrollX = 300,
        deferRender = TRUE,
        pageLength = 20,
        dom = "tip",
        autoWidth = F
      )
    )
  })
  
  #### Transfer #### 
  transferOverview = reactiveVal(data.frame())
  file_name_trans = "./data/Transfer_Info.csv"
  if (file.exists(file_name_trans)) {
    df_transfer = read.csv(file_name_trans)
  }
  else{
    df_transfer = NULL
  }
  transferOverview(df_transfer)
  
  transferData = reactiveValues(dt = data.frame(), 
                                history = list(NULL)) # Store transfer info
  
  observeEvent(input$transfer_add, {
    new_data = data.frame(
      Name = input$transfer_name,
      Pos = input$transfer_position,
      Season = as.integer(input$transfer_season),
      Window = input$transfer_window,
      Type = input$transfer_type,
      Fee = as.numeric(input$transfer_fee)
    )
    current_data = transferData$dt
    updated_data = rbind(current_data, new_data)
    transferData$history[[length(transferData$history) + 1]] = current_data
    transferData$dt = updated_data
  })
  
  output$transfer_dt = renderDT({
    datatable(
      transferData$dt,
      editable = T,
      rownames = T,
      options = list(
        scrollY = 300,
        scrollX = 300,
        deferRender = TRUE,
        pageLength = 10,
        dom = "tip",
        autoWidth = F
      )
    )
  })
  
  # Fix
  observeEvent(input$transfer_dt_cell_edit, {
    info = input$transfer_dt_cell_edit
    temp = transferData$dt
    transferData$history[[length(transferData$history)+1]] = temp
    
    temp[info$row, info$col] = info$value
    transferData$dt = temp
  })
  
  # Undo
  observeEvent(input$transfer_undo, {
    if(length(transferData$history) > 1) {
      transferData$dt = tail(transferData$history, 1)[[1]]
      transferData$history = transferData$history[-length(transferData$history)]
    }
  })
  
  # Rewrite
  observeEvent(input$updateFile_transfer, {
    df_transfer = rbind(transferOverview(), transferData$dt)
    write.csv(df_transfer, 
              paste0("./data/Transfer_Info.csv"),
              row.names = F)
    transferOverview(df_transfer)
  })
  
  output$transfer_overview_dt = renderDT({
    datatable(
      transferOverview() %>% arrange(-Season),
      rownames = F,
      options = list(
        scrollY = 300,
        scrollX = 300,
        deferRender = TRUE,
        pageLength = 20,
        dom = "tip",
        autoWidth = F
      )
    )
  })
  #### Transfer Overview ####
  output$vis_transfer_fee = renderPlotly({
    df = transferOverview() %>% 
      filter(Pos %in% input$vis_transfer_position) %>% 
      group_by(Season, Type) %>% 
      summarise(Fee_Total = sum(Fee))
    
    p = ggplot(df, aes(x = Season, y = Fee_Total)) + 
      geom_col(aes(fill = Type), position = "dodge") + 
      theme_minimal() + 
      labs(y = "Transfer Fee (M)",
           title = "Transfer Fee in Seasons")
    
    ggplotly(p)
  })
  
  output$vis_transfer_dt = renderDT({
    datatable(
      transferOverview() %>% 
        filter(Pos %in% input$vis_transfer_position,
               Type %in% input$vis_transfer_type) %>% 
        arrange(-Fee),
      rownames = F,
      options = list(
        scrollY = 200,
        scrollX = 300,
        deferRender = TRUE,
        pageLength = 10,
        dom = "tip",
        autoWidth = F
      )
    )
  })
  
  output$vis_transfer_rank = renderPlotly({
    dff = transferOverview() 
    dff$Fee[which(dff$Type == "Out")] = -dff$Fee[which(dff$Type == "Out")] 
    dff = dff %>% 
      filter(Pos %in% input$vis_transfer_position,
             Type %in% input$vis_transfer_type) %>% 
      arrange(-abs(Fee)) %>% 
      head(10)
    p = ggplot(dff, aes(y = reorder(Name, abs(Fee)), x = Fee)) + 
      geom_col(aes(fill = Type)) + 
      theme_minimal() + 
      labs(y = "Transfer Fee (M)",
           title = "Transfer Fee in Players")
    
    ggplotly(p)
  })
  
  #### Player #### 
  playerData = reactiveVal(data.frame())
  observeEvent(input$player_season, {
    Players = c()
    for (season in input$player_season) {
      files = paste0("./data/Season",
                     season,
                     "/Season_Start.csv")
      temp = read.csv(files)
      Players = c(Players, temp$Name)
    }
    
    updatePickerInput(session, "player_name",
                      choices = unique(Players),
                      options = pickerOptions(liveSearch = T))
  })
  
  observe({
    file_names = c()
    comps = c()
    for (season in input$player_season) {
      for (comp in input$player_comp) {
        files = list.files(paste0("./data/Season",
                                  season,
                                  "/",
                                  comp,
                                  "/"),
                           full.names = T)
        file_names = c(file_names, files)
        if (length(files) > 1){
          comps = c(comps, rep(comp, length(files) - 1))
        }
        
      }
    }
    
    
    df = data.frame()
    for (file_name in file_names){
      if (!str_detect(file_name, "merge")) {
        temp = read.csv(file_name) %>% 
          mutate(
            Competition = 
              sub(".*match_data_(.*?)_.*", "\\1", file_name))
        df = rbind(df, temp)
      }
    }
    
    df = df %>% 
      filter(Name == input$player_name) %>% 
      mutate(
        Shot_Acc = Shot_Comp / Shots * 100,
        Pass_Acc = Pass_Comp / Pass * 100,
        Dribble_Acc = Dribble_Comp / Dribble * 100,
        Tackle_Acc = Tackle_Comp / Tackle * 100)
    
    playerData(df)
    
  })
  
  output$player_1 = renderPlotly({
    plot_player_stat(playerData(), 
                     get_variable(input$player_variable)[1])
  })
  output$player_2 = renderPlotly({
    plot_player_stat(playerData(), 
                     get_variable(input$player_variable)[2])
  })
}








