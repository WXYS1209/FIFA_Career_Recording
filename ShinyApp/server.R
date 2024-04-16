server <- function(input, output, session) {
  
  #### Record Data ####
  matchData <- reactiveVal(data.frame()) # Store match data
  startData <- reactiveVal(data.frame()) # Store Season_Start
  
  # Read Season_Start.csv for selected season
  observeEvent(input$season, {
    file_name = paste0("../data/Season",
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
    
    current_data <- matchData()
    updated_data <- rbind(current_data, new_data)
    matchData(updated_data)
  })
  
  # Display data added
  output$updatedData <- renderDT({
    datatable(matchData(),
              options = list(scrollY = 500,
                             scrollX = 500,
                             deferRender = TRUE,
                             pageLength = 20,
                             autoWidth = T)
    )
  })
  
  # Download
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
  newmatchData <- reactiveVal(data.frame()) # Store uploaded match records
  aggregatedData <- reactiveVal(data.frame()) # Store aggregated record
  
  # Read Match Records
  observeEvent(input$fileUpload_merge, {
    inFile <- input$fileUpload_merge
    if (is.null(inFile)) {
      return(NULL)
    }
    
    # Read all files
    data_list <- lapply(inFile$datapath, read.csv)
    
    # Merge all files
    if (length(data_list) > 1) {
      df <- do.call(rbind, data_list)
    } else {
      df <- data_list[[1]]
    }
    newmatchData(df)
  })
  
  # Read Aggregated Record
  observeEvent(input$competition_merge, {
    if (input$competition_merge != "All" &
        input$season_merge != "All"){
      file_name = paste0("../data/Season", input$season_merge,
                         "/", input$competition_merge, "/merged_data_",
                         input$competition_merge,
                         ".csv")
    }
    if (input$competition_merge == "All") {
      file_name = paste0("../data/Season", input$season_merge,
                         "/OverAll.csv")
    }
    
    if (input$season_merge == "All") {
      file_name = paste0("../data/OverAllSeasons.csv")
    }
    df = read.csv(file_name)
    aggregatedData(df)
  })
  
  # Merge and update aggregated record
  observeEvent(input$merge, {
    temp_aggr = aggregatedData()
    temp_match = newmatchData()
    
    df <- temp_aggr %>% 
      rbind(temp_match) %>% 
      group_by(Name) %>% 
      summarise(
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
    aggregatedData(df[!duplicated(df),])
  })
  
  # Display merged aggregated record
  output$updatedData_merge_dt <- renderDT({
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
    if (input$competition_merge != "All" &
        input$season_merge != "All"){
      write.csv(aggregatedData(), 
                paste0("../data/Season", input$season_merge,
                       "/", input$competition_merge, "/merged_data_",
                       input$competition_merge,
                       ".csv"),
                row.names = F
      )
    }
    if (input$competition_merge == "All") {
      write.csv(aggregatedData(), 
                paste0("../data/Season", input$season_merge,
                       "/OverAll.csv"),
                row.names = F
      )
    }
    if (input$season_merge == "All") {
      write.csv(aggregatedData(), 
                paste0("../data/OverAllSeasons.csv"),
                row.names = F
      )
    }
  })
  # output$updateData_merge <- downloadHandler(
  #   filename = function() {
  #     paste0("merged_data_", input$competition_merge, ".csv")
  #   },
  #   content = function(file) {
  #     write.csv(aggregatedData(), file, row.names = FALSE)
  #   }
  # )
  
  #### Match Stat ####
  matchstatData = reactiveVal(data.frame())
  
  observeEvent(input$match_stat_fileUpload, {
    df = read.csv(input$match_stat_fileUpload$datapath)
    matchstatData(df)
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
    current_data = matchstatData()
    updated_data = rbind(current_data, new_data)
    matchstatData(updated_data)
  })
  
  # Display data added
  output$updatedMatchStat <- renderDT({
    datatable(matchstatData(),
              rownames = F,
              options = list(scrollY = 500,
                             scrollX = 500,
                             deferRender = TRUE,
                             pageLength = 20,
                             autoWidth = F)
    )
  })
  
  # Download
  output$downloadMatchStat <- downloadHandler(
    filename = "Match_Stat.csv",
    content = function(file) {
      write.csv(matchstatData(), file, row.names = FALSE)
    }
  )
  
  #### Ranking ####
  ##### Current Season #####
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
               Tackle_Acc = Tackle_Comp / Tackle,
               Rating = Rating / Played)
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
  
  ##### Overall #####
  # overall_df = reactiveVal(data.frame())
  
  file_name_all = "../data/OverAllSeasons.csv"
  if (file.exists(file_name_all)) {
    df_all_seasons = read.csv(file_name_all) %>% 
      mutate(Shot_Acc = Shot_Comp / Shots,
             Pass_Acc = Pass_Comp / Pass,
             Dribble_Acc = Dribble_Comp / Dribble,
             Tackle_Acc = Tackle_Comp / Tackle,
             Rating = Rating / Played)
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
}
