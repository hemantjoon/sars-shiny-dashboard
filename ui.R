source('data.R', local = TRUE)
source('elements.R', local = TRUE)

ui <- navbarPage(title = 'SARS-CoV-2', collapsible = TRUE, fluid = TRUE,
                 theme = bs_theme(
                   version = 5,
                   # bootswatch = "darkly",
                   #navbar_bg = "#25443B"
                 ),
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
                              
                              card(height = '250px',
                                plotlyOutput('germanyCasesCountPlot')
                              ),
                              
                              card(height = '250px',
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
                         #c('BA.2', 'BA.4', 'BA.5', 'XBB.1.5', 'BA.2.86', 'BA.2.87.1', 'XAK', 'B.1.617.2', 'P.1', 'B.1.1.7')
                                            c('BA.2', 'BA.4', 'BA.5', 'XBB.1.5', 'BA.2.86', 'BA.2.87.1', 
                                              'XAK', 'B.1.617.2', 'P.1', 'B.1.1.7')),
                              dateRangeInput('chooseDateLineages', label = 'Select Timeperiod',
                                             min = min(df$Release_Date), max = max(df$Release_Date),
                                             start = min(df$Release_Date), end = max(df$Release_Date), 
                                             format = 'dd-mm-yyyy', startview = 'year'),
                              sliderInput('sequenceLengthInput', label = 'Customize the min and max sequence length',
                                          min = min(df$Length), max(df$Length), value = c(min(df$Length), max(df$Length)), step = 1)
                            ),
                            
                            mainPanel(
                              card(plotlyOutput('lineagesPlot')),
                              layout_column_wrap(
                                card(DT::DTOutput('hostTable')),
                                card(plotlyOutput('partialPiePlot'))
                              )
                            )
                          )
                 ),
                 
                 tabPanel(title = 'About',
                          
                 ),
                 
                 tabPanel(title = 'Contact',
                          
                 ),
                 
                 tabPanel(title = 'Data and Privacy',
                          
                 )
)