
server <- function(input, output) {
  
  kdf <- reactive({
    if(input$timelineType == 'day')
      mdf %>% filter(date >= input$chooseDate[1] & date <= input$chooseDate[2])
    else if(input$timelineType == 'month')
      mdf_month %>% filter(date >= format(input$chooseDate[1], "%Y-%m") & date <= format(input$chooseDate[2], "%Y-%m"))
    else
      mdf_year %>% filter(date >= format(input$chooseDate[1], "%Y") & date <= format(input$chooseDate[2], "%Y"))
  })
  
  
  output$casesPerText <- renderText({
    paste('Number of cases by', input$timelineType, sep = ' ')
  })
  
  
  output$casesCountPlot <- renderPlotly(
    bar_counts_plot <- plot_ly(
      data = kdf(),
      x = ~date,
      y = ~cases,
      type = 'bar')
    
  )
  
  output$casesCumulativePlot <- renderPlotly(
    cumulative_counts_plot <- plot_ly(
      data = kdf(),
      x = ~date,
      y = ~cumulative_cases,
      type = 'scatter',
      mode = 'lines+markers')
    
  )
  
  total_cases_world <- reactive({
    df_world_date <-  df %>% filter(Release_Date >= input$chooseDate[1] & Release_Date <= input$chooseDate[2])
    total_cases_world <- nrow(df_world_date)
  })
  
  output$worldMapTitle <- renderText(
    paste('World Map with total cases for the selected period are ', total_cases_world())
  )
  
  
  mapData <- reactive({
    country_df_date <-  df %>% filter(Release_Date >= input$chooseDate[1] & Release_Date <= input$chooseDate[2])
    country_temp <- as.data.frame(table(country_df_date$Country))
    colnames(country_temp) <- c('country', 'cases')
    country_temp$country <- as.character(country_temp$country)
    country_temp$country <- replace(country_temp$country, country_temp$country == 'Viet Nam', 'Vietnam')
    
    country_merge <- merge(country_temp, countries_all, by = 'country')  
  })
  
  
  output$mapPlot <- renderPlotly(
    myPlot <- ggplotly(ggplot() +
      geom_sf(data = countries_all, fill = '#84ad8f') +
      # add size and fill after adding numerical data
      geom_sf(data = mapData(), pch = 21, aes(geometry = position, size = cases, fill = cases), color = 'grey')+
      scale_fill_gradientn(colours = hcl.colors(5, "RdBu",
                                                rev = TRUE,
                                                alpha = 0.9))+
      guides(fill = guide_legend(title = ""))+
      labs(size = "")+
      theme_void()+
      theme(axis.line=element_blank())
    )
  )
  
  
  germany_bar_data <- reactive({
    if(input$timelineType == 'day')
      germany_df %>% filter(date >= input$chooseDate[1] & date <= input$chooseDate[2])
    else if(input$timelineType == 'month')
      germany_df_month %>% filter(date >= format(input$chooseDate[1], "%Y-%m") & date <= format(input$chooseDate[2], "%Y-%m"))
    else
      germany_df_year %>% filter(date >= format(input$chooseDate[1], "%Y") & date <= format(input$chooseDate[2], "%Y"))
  })
  
  
  output$germanyCasesCountPlot <- renderPlotly(
    bar_counts_plot <- plot_ly(
      data = germany_bar_data(),
      x = ~date,
      y = ~cases,
      type = 'bar')
    
  )
  
  output$germanyCasesCumulativePlot <- renderPlotly(
    cumulative_counts_plot <- plot_ly(
      data = germany_bar_data(),
      x = ~date,
      y = ~cumulative_cases,
      type = 'scatter',
      mode = 'lines+markers')
  )
    
    
  
  total_cases_germany <- reactive({
    df_germany_date <-  df_germany %>% filter(Release_Date >= input$chooseDate[1] & Release_Date <= input$chooseDate[2])
    total_cases_germany <- nrow(df_germany_date)
  })
  
  output$germanyMapHeading <- renderText(
    paste('Germany Map with total cases for the selected period are ', total_cases_germany())
  )
  
  
  germanyData <- reactive({
    df_germany_date <-  df_germany %>% filter(Release_Date >= input$chooseDate[1] & Release_Date <= input$chooseDate[2])
    df_germany_count <- as.data.frame(table(df_germany_date$Geo_Location))
    colnames(df_germany_count) <- c('state', 'cases')
    df_germany_count$state <- as.character(df_germany_count$state)
    
    ok <- sapply(df_germany_count$state, function(x) grepl('Germany:Europe/Germany/', x))
    df_germany_count_state <- df_germany_count[ok,]
    df_germany_count_state$state <- gsub('Germany:Europe/Germany/', '', df_germany_count_state$state)
    
    germany_merge <- merge(df_germany_count_state, germany_all, by = 'state')
  })
  
  
  output$germanyPlot <- renderPlotly(
    myGermanyPlot <- ggplot() +
      geom_sf(data = germany_all, fill = "#84a1ad") +
      # add size and fill after adding numerical data
      geom_sf(data = germanyData(), pch = 21, aes(geometry = position, size = cases, fill = cases), color = 'grey')+
      scale_fill_gradientn(colours = hcl.colors(5, "RdBu",
                                                rev = TRUE,
                                                alpha = 0.9))+
      guides(fill = guide_legend(title = ""))+
      labs(size = "")+
      theme_void()+
      theme(axis.line=element_blank())
  )
  
  ## Lineages Page ###############################################################
  
  lineages_df_count_filtered <- reactive({
    lineages_df_count <- lineages_df_count %>% filter(Release_Date >= input$chooseDateLineages[1] & Release_Date <= input$chooseDateLineages[2])
  })
  
  selectedLineagesList <- reactive({
    checked <- input$lineagesChoose
  })
  
  my_p <- reactive({
    p <- plot_ly(lineages_df_count_filtered(), type = 'scatter', mode = 'lines')
    my_voc_list <- selectedLineagesList()
    if(length(my_voc_list) == 0){
      my_voc_list <- voc_list
    }
    for(trace in my_voc_list){
      p <- p %>% plotly::add_trace(x = ~Release_Date,
                                   y = as.formula(paste0("~`", trace, "`")), name = trace)}
    p
  })
  
  
  output$lineagesPlot <- renderPlotly(
    myLineagesPlot <- my_p() %>%
      layout(showlegend = T, plot_bgcolor='#e5ecf6',
             xaxis = list(zerolinecolor = '#ffff',
                          zerolinewidth = 2,
                          gridcolor = 'ffff'),
             yaxis = list(zerolinecolor = '#ffff',
                          zerolinewidth = 2,
                          gridcolor = 'ffff',title = 'Counts'))
  )
  
  host_df_table <- reactive({
    host_df <- host_df %>% filter(Release_Date >= input$chooseDateLineages[1] & Release_Date <= input$chooseDateLineages[2])
    host_df <- host_df %>% filter(Length >= input$sequenceLengthInput[1] & Length <= input$sequenceLengthInput[2])
    host_df_table <- as.data.frame(table(host_df$Host))
    colnames(host_df_table) <- c('host', 'counts')
    host_df_table$host <- as.character(host_df_table$host)
    host_df_table$host <- sapply(host_df_table$host, unknown_host)
    host_df_table
  })
  
  output$hostTable <- DT::renderDT(
    host_df_table()
  )
  
  df_partial_func <- reactive({
    df_partial <- df %>% filter(Release_Date >= input$chooseDateLineages[1] & Release_Date <= input$chooseDateLineages[2])
    df_partial <- df_partial %>% filter(Length >= input$sequenceLengthInput[1] & Length <= input$sequenceLengthInput[2])
    df_partial <- as.data.frame(table(df_partial$Nuc_Completeness))
  })
  
  output$partialPiePlot <- renderPlotly(
    fig <- plot_ly(df_partial_func(), labels = ~Var1, values = ~Freq , type = 'pie')
  )
  
  
}