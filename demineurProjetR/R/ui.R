source('R/fonction.R')
library(shiny)
library(shinyjs)
library(dbplyr)

ui <- fluidPage(
  tags$style(HTML("
      body {
        background-color: #ADD8E6;
      }
  ")),
  shinyjs::useShinyjs(),
  titlePanel("Démineur"),
  uiOutput("error_message"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("difficulty", "Niveau de difficulté:",
                   choices = c("Personnalisé", "Facile", "Moyen", "Difficile"),
                   selected = "Personnalisé",
                   inline = TRUE),
      conditionalPanel(condition = "input.difficulty === 'Personnalisé'",
                       sliderInput("rows", "Nombre de lignes:", min = 5, max = 20, value = 20),
                       sliderInput("cols", "Nombre de colonnes:", min = 5, max = 20, value = 20),
                       sliderInput("mines", "Nombre de mines:", min = 1, max = 100, value = 25)),
      radioButtons("action", "Sélectionner l'action:", choices = c("Révéler", "Drapeau"), selected = "Révéler"),
      actionButton("reset", "Réinitialiser le jeu"),
      wellPanel(
        style = "padding: 10px; background-color: #F0F0F0; margin-top: 20px;",
        h4("Règles du jeu"),
        tags$ul(
          tags$li("Le but du jeu est de révéler toutes les cases sans mines."),
          tags$li("Cliquez sur une case pour la révéler. Si la case contient une mine, vous perdez."),
          tags$li("Si une case révélée ne contient pas de mine, un chiffre s'affiche pour indiquer le nombre de mines adjacentes."),
          tags$li("Utilisez les chiffres pour déduire l'emplacement des mines."),
          tags$li("Placez un drapeau sur une case que vous pensez contenir une mine. Vous pouvez changer l'action en sélectionnant 'Drapeau' dans les options."),
          tags$li("Vous gagnez lorsque toutes les cases sans mines sont révélées.")
        )
      )
    ),
    mainPanel(
      div(style = "text-align: center; font-size: 24px; font-weight: bold; margin-bottom: 10px;",
          textOutput("timer")),
      fluidRow(
        column(width = 12,
               tags$style(type = "text/css", "#board td { border: 1px solid black; width: 30px; height: 30px; text-align: center; }"),
               div(style = "display: flex; justify-content: center;", uiOutput("board")))
      ),
      div(style = "text-align: center; font-size: 24px; font-weight: bold; margin-top: 10px;",
          textOutput("flags_count"))
    )
  )
)
