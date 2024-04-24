## To be Removed
setwd("~/ICGEB/Shiny/Dashboard_Covid/")

library(shiny)

source('libraries.R')
source('functions.R')

#source('data.R', local = TRUE)
#source('elements.R', local = TRUE)

source('ui.R', local = TRUE)
source('server.R', local = TRUE)

shinyApp(ui, server)