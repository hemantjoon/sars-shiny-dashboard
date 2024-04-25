library(shiny)

##Loading Libraries
library(markdown)
library(bslib)
library(bsicons)
library(htmlwidgets)

library(plotly)
library(dplyr)
library(lubridate)

## for map
library(giscoR)
library(sf)

# for str_to_title
library(stringr) 

#options(shiny.host = '0.0.0.0')
#options(shiny.port = 7777)

## for data table
library(DT)

## function
format_bignum = function(n){
  case_when(
    n >= 1e12 ~ paste(round(n/1e12, digits = 2), 'Tn'),
    n >= 1e9 ~ paste(round(n/1e9, digits = 2), 'Bn'),
    n >= 1e6 ~ paste(round(n/1e6, digits = 2), 'M'),
    n >= 1e3 ~ paste(round(n/1e3, digits = 2), 'K'),
    TRUE ~ as.character(n))
}

## attaching database
df <- read.csv("random.csv")
df <- df %>% 
  mutate(Release_Date= as.Date(Release_Date, format= "%Y-%m-%d"))
totalCases <- nrow(df)

mdf <- as.data.frame(table(df$Release_Date))
colnames(mdf) <- c('date', 'cases')
mdf <- mdf %>% 
  mutate(date = as.Date(date, format= "%Y-%m-%d")) %>%
  mutate(cumulative_cases = cumsum(cases))


mdf_month <- mdf
mdf_month$date <- format(as.POSIXlt(mdf$date, format="%Y-%m-%d"), "%Y-%m")
mdf_month <- mdf_month %>% group_by(date) %>%
  summarise(cases = sum(cases))
mdf_month <- mdf_month %>% 
  mutate(cumulative_cases = cumsum(cases))


mdf_year <- mdf
mdf_year$date <- format(as.POSIXlt(mdf$date, format="%Y-%m-%d"), "%Y")
mdf_year <- mdf_year %>% group_by(date) %>%
  summarise(cases = sum(cases))
mdf_year <- mdf_year %>% 
  mutate(cumulative_cases = cumsum(cases))
  
  
## data germany

germany_df <- subset(df, df$Country == 'Germany')
germany_df <- as.data.frame(table(germany_df$Release_Date))
colnames(germany_df) <- c('date', 'cases')
germany_df <- germany_df %>% 
  mutate(date = as.Date(date, format= "%Y-%m-%d")) %>%
  mutate(cumulative_cases = cumsum(cases))


germany_df_month <- germany_df
germany_df_month$date <- format(as.POSIXlt(germany_df$date, format="%Y-%m-%d"), "%Y-%m")
germany_df_month <- germany_df_month %>% group_by(date) %>%
  summarise(cases = sum(cases))
germany_df_month <- germany_df_month %>% 
  mutate(cumulative_cases = cumsum(cases))


germany_df_year <- germany_df
germany_df_year$date <- format(as.POSIXlt(germany_df$date, format="%Y-%m-%d"), "%Y")
germany_df_year <- germany_df_year %>% group_by(date) %>%
  summarise(cases = sum(cases))
germany_df_year <- germany_df_year %>% 
  mutate(cumulative_cases = cumsum(cases))




countries_all <- gisco_get_countries()
colnames(countries_all)[4] <- 'country'
countries_all$country <- replace(countries_all$country, countries_all$country == 'Russian Federation', 'Russia')
countries_all$country <- replace(countries_all$country, countries_all$country == 'United States', 'USA')
countries_all$country <- replace(countries_all$country, countries_all$country == 'Myanmar/Burma', 'Myanmar')
countries_all_centre <- suppressWarnings({st_centroid(countries_all, of_largest_polygon = TRUE)})
countries_all$position <- countries_all_centre$geometry 


country_temp <- as.data.frame(table(df$Country))
colnames(country_temp) <- c('country', 'cases')
country_temp$country <- as.character(country_temp$country)
country_temp$country <- replace(country_temp$country, country_temp$country == 'Viet Nam', 'Vietnam')

country_merge <- merge(country_temp, countries_all, by = 'country')  


## Germany Map

df_germany <- subset(df, df$Country == 'Germany')
df_germany_count <- as.data.frame(table(df_germany$Geo_Location))

colnames(df_germany_count) <- c('state', 'cases')
df_germany_count$state <- as.character(df_germany_count$state)

total_cases_germany <- nrow(df_germany)
ok <- sapply(df_germany_count$state, function(x) grepl('Germany:Europe/Germany/', x))
df_germany_count_state <- df_germany_count[ok,]
df_germany_count_state$state <- gsub('Germany:Europe/Germany/', '', df_germany_count_state$state)

germany_all <-  gisco_get_nuts(nuts_level = 1, country = "Germany")

colnames(germany_all)[6] <- 'state'
germany_all$state <- sapply(germany_all$state, function(x) str_to_title(x))

germany_all$state <- replace(germany_all$state, germany_all$state == 'Baden-Württemberg', 'Baden-Wurttemberg')
germany_all$state <- replace(germany_all$state, germany_all$state == 'Bayern', 'Bavaria')
germany_all$state <- replace(germany_all$state, germany_all$state == 'Hessen', 'Hesse')
germany_all$state <- replace(germany_all$state, germany_all$state == 'Mecklenburg-Vorpommern', 'Mecklenburg-Western Pomerania')
germany_all$state <- replace(germany_all$state, germany_all$state == 'Niedersachsen', 'Lower Saxony')
germany_all$state <- replace(germany_all$state, germany_all$state == 'Nordrhein-Westfalen', 'North Rhine-Westphalia')
germany_all$state <- replace(germany_all$state, germany_all$state == 'Rheinland-Pfalz', 'Rhineland-Palatinate')
germany_all$state <- replace(germany_all$state, germany_all$state == 'Sachsen', 'Saxony')
germany_all$state <- replace(germany_all$state, germany_all$state == 'Sachsen-Anhalt', 'Saxony-Anhalt')
germany_all$state <- replace(germany_all$state, germany_all$state == 'Thüringen', 'Thuringia')

germany_all_centre <- suppressWarnings({st_centroid(germany_all, of_largest_polygon = TRUE)})
germany_all$position <- germany_all_centre$geometry 

germany_merge <- merge(df_germany_count_state, germany_all, by = 'state')




#####################  Lineages ##########################

host_df <-df[,c('Release_Date', 'Host', 'Length')]
host_df_table <- as.data.frame(table(host_df$Host))
colnames(host_df_table) <- c('host', 'counts')
host_df_table$host <- as.character(host_df_table$host)

unknown_host <- function(x){
  if(x=="")
    return('Unknown')
  else 
    return(x)
}

host_df_table$host <- sapply(host_df_table$host, unknown_host)


## Pie Chart NucleotideCompletness

df_partial <- as.data.frame(table(df$Nuc_Completeness))


## Line Chart
voc_list <- c('BA.2', 'BA.4', 'BA.5', 'XBB.1.5', 'BA.2.86', 'BA.2.87.1', 'XAK', 'B.1.617.2', 'P.1', 'B.1.1.7')

lineages_df_count <- read.csv('lineages_df_count.csv')
lineages_df_count$Release_Date <- as.Date(lineages_df_count$Release_Date, format= "%Y-%m-%d")


sparkline <- plot_ly(mdf) %>%
  add_lines(
    x = ~date, y = ~cases,
    color = I("white"), span = I(1),
    fill = 'tozeroy', alpha = 0.2
  ) %>%
  layout(
    xaxis = list(visible = F, showgrid = F, title = ""),
    yaxis = list(visible = F, showgrid = F, title = ""),
    hovermode = "x",
    margin = list(t = 0, r = 0, l = 0, b = 0),
    font = list(color = "white"),
    paper_bgcolor = "transparent",
    plot_bgcolor = "transparent"
  ) %>%
  config(displayModeBar = F) %>%
  htmlwidgets::onRender(
    "function(el) {
      var ro = new ResizeObserver(function() {
         var visible = el.offsetHeight > 200;
         Plotly.relayout(el, {'xaxis.visible': visible});
      });
      ro.observe(el);
    }"
  )

bar_counts <- plot_ly(
  data = mdf,
  x = ~date,
  y = ~cases,
  type = 'bar'
)

vbs <- list(
  value_box(
    title = 'Total Cases',
    value = format_bignum(totalCases),
    theme = 'success'
  ),
  value_box(
    title = 'Absolute Count',
    value = totalCases,
    theme = 'blue',
  )
)

ui <- navbarPage(title = 'SARS-CoV-2', collapsible = TRUE, fluid = TRUE,
                 theme = bs_theme(
                   version = 5,
                 ),
                 header = 'The data used does not represent the whole data but a subset 
                 of randomly selected 100k samples',
                 tabPanel(title = 'Dashboard',
                          sidebarLayout(
                            sidebarPanel(
                              
                              layout_column_wrap(
                                vbs[[1]], 
                                vbs[[2]],
                              ),
                                
                              
                              card(
                                card_header('Choose the desired option'),
                                radioButtons('timelineType', label = 'Cases per', choices = c('Day' = 'day', 'Month' = 'month', 'Year' = 'year'),
                                             selected = 'day'),
                                dateRangeInput('chooseDate', label = 'Select Timeperiod',
                                               min = min(df$Release_Date), max = max(df$Release_Date),
                                               start = min(df$Release_Date), end = max(df$Release_Date), 
                                               format = 'dd-mm-yyyy', startview = 'year')
                                ),
                              
                              card(
                                height = '250px',
                                card_header(textOutput('casesPerTextGermany')),
                                plotlyOutput('germanyCasesCountPlot')
                              ),
                              
                              card(
                                height = '250px',
                                card_header('Cumulative cases Germany specific'),
                                plotlyOutput('germanyCasesCumulativePlot')
                              ),
                              
                              
                              card(
                                card_header(textOutput('germanyMapHeading')),
                                full_screen = TRUE,
                                plotlyOutput('germanyPlot')
                              )
                              
                            ),
                            
                            mainPanel(
                              
                              card(
                                card_header(textOutput('casesPerText')),
                                full_screen = TRUE,
                                plotlyOutput('casesCountPlot')
                              ),
                              
                              card(
                                card_header('Cumulative cases'),
                                full_screen = TRUE,
                                height = '300px',
                                plotlyOutput('casesCumulativePlot')
                              ),
                              
                              card(
                                card_header(textOutput('worldMapTitle')),
                                full_screen = TRUE,
                                plotlyOutput('mapPlot')
                              )
                              
                              
                            )
                          )
                 ),
                 
                 tabPanel(title = 'Lineages',
                          sidebarLayout(
                            sidebarPanel(
                              checkboxGroupInput("lineagesChoose", "SARS-CoV-2 variants to be plotted:",
                                            c('BA.2', 'BA.4', 'BA.5', 'XBB.1.5', 'BA.2.86', 'BA.2.87.1', 
                                              'XAK', 'B.1.617.2', 'P.1', 'B.1.1.7')),
                              tags$br(),
                              dateRangeInput('chooseDateLineages', label = 'Select Timeperiod',
                                             min = min(df$Release_Date), max = max(df$Release_Date),
                                             start = min(df$Release_Date), end = max(df$Release_Date), 
                                             format = 'dd-mm-yyyy', startview = 'year'),
                              tags$br(),
                              sliderInput('sequenceLengthInput', label = 'Customize the min and max sequence length',
                                          min = min(df$Length), max(df$Length), value = c(min(df$Length), max(df$Length)), step = 1)
                            ),
                            
                            mainPanel(
                              card(
                                card_header('Lineages count over time interval'),
                                full_screen = TRUE,
                                plotlyOutput('lineagesPlot')
                                ),
                              layout_column_wrap(
                                card(
                                  card_header('Host counts table'),
                                  DT::DTOutput('hostTable')
                                  ),
                                card(
                                  card_header('Pie Chart for partial and complete sequences'),
                                  full_screen = TRUE,
                                  plotlyOutput('partialPiePlot')
                                  )
                              )
                            )
                          )
                 ),
                 
                 tabPanel(title = 'About',
                          tags$style(".infoCards {display: flex;} 
                                     .infoCards a {margin-right: 20px}
                                     .infoCards p {margin-right: 10px"),
                          card(
                            card_header('About me'),
                            tags$div(class = 'infoCards', strong('Name'), p(': Hemant Kumar Joon')),
                            tags$div(class = 'infoCards', 
                                     a('Linkedln', href='https://www.linkedin.com/in/hemantjoon/', target='_blank'),
                                     a('Github', href='https://github.com/hemantjoon', target='_blank'),
                                     a('Research Gate', href='https://www.researchgate.net/profile/Hemant-Joon', target='_blank'),
                                     a('Portfolio', href='https://hemantjoon.github.io/', target='_blank'))
                          ),
                          card(
                            card_header('Data & codebase'),
                            tags$div(class = 'infoCards', 
                                     p('Original and Updated data was retrieved from '), 
                                     a('NCBI SARS-CoV-2 Data Hub', target='_blank', href='https://www.ncbi.nlm.nih.gov/labs/virus/vssi/#/virus?SeqType_s=Nucleotide&VirusLineage_ss=taxid:2697049')),
                            tags$div(class = 'infoCards', 
                                     p('Due to the limitation of resources the randomly generated data of 100K samples is also available at Github repository'), 
                                     a('sars-shiny-dashboard/random.csv', target='_blank', href='https://github.com/hemantjoon/sars-shiny-dashboard')),
                            tags$div(class = 'infoCards', 
                                     p('The entire codebase for the development of Shiny Dashboard is available at Github repository'), 
                                     a('sars-shiny-dashboard', target='_blank', href='https://github.com/hemantjoon/sars-shiny-dashboard'))
                          ),
                          card(
                            card_header('Contact'),
                            tags$div(class = 'infoCards', strong('Email'), p(':'),
                                     a('joonhemant99@gmail.com', href='mailto:joonhemant99@gmail.com', target='_blank', rel='noreferrer')),
                          )
                          
                 )
                 
)

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
  
  output$casesPerTextGermany <- renderText({
    paste('Number of cases by', input$timelineType, ' Germany specific', sep = ' ')
  })
  
  
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

shinyApp(ui, server)