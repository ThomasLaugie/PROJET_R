library(shiny)

nrows = 10
ncols = 10
density = 0.5

generate_board <- function(nrows, ncols) {
  board <- matrix(0, nrow = nrows, ncol = ncols)
  return(board)
}

place_mines <- function(board, density) {
  ncells <- length(board)
  nmines <- round(density * ncells)
  indices <- sample(ncells, nmines)
  board[indices] <- -1
  return(board)
}

display_board <- function(board) {
  nrows <- nrow(board)
  ncols <- ncol(board)
  display <- matrix("", nrow = nrows, ncol = ncols)
  for (i in 1:nrows) {
    for (j in 1:ncols) {
      if (board[i, j] == -1) {
        display[i, j] <- "*"
      } else {
        n_adjacent <- sum(board[max(1, i-1):min(nrows, i+1), max(1, j-1):min(ncols, j+1)] == -1)
        display[i, j] <- ifelse(n_adjacent == 0, ".", as.character(n_adjacent))
      }
    }
  }
  print(display)
  return(display)
}

ui <- fluidPage(
  titlePanel("Tableau Rouge et Noir"),
  sidebarLayout(
    sidebarPanel(("OPTION"),
                 numericInput("n_col","Entrer le nombre de colonne",value = 5),
                 numericInput("n_ligne","Entrer le nombre de ligne",value = 5),),
    mainPanel(("Main"),
              textOutput("cell_click"),
              uiOutput("tableau")
    )
  )
)

server <- function(input, output) {

  matrice_test <- reactive({
    board = generate_board(input$n_ligne,input$n_col)
    board = place_mines(board,density)
    display_board(board)
  })

  color_cell <- function(x) {
    if (x == "*") {
      tagList(
        tags$td(style = "background-color: red", x)
      )
    } else {
      tagList(
        tags$td(style = "background-color: black", x)
      )
    }
  }

  create_table <- function(matrice) {
    rows <- apply(matrice, 1, function(row) {
      tags$tr(lapply(row, color_cell))
    })
    tags$table(id="board_table", rows)
  }

  output$tableau <- renderUI({
    create_table(matrice_test())
  })

  observeEvent(input$board_table_cell_clicked,{
    output$cell_click <- renderText(paste("Row: ", input$board_table_cell_clicked$row, ", Column: ", input$board_table_cell_clicked$col))
  })
}

shinyApp(ui = ui, server = server)
