library(shiny)
library(shinyjs)
library(dbplyr)
source('R/ui.R')
source('R/server.R')
source('R/fonction.R')


demineur = shinyApp(ui = ui, server = server)
demineur
