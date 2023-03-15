library(shiny)

generate_board <- function(rows, cols, mines) {
  board <- matrix(0, nrow = rows, ncol = cols)
  mine_positions <- sample(1:(rows * cols), size = mines)
  board[mine_positions] <- 1
  return(board)
}
count_adjacent_mines <- function(board, row, col) {
  rows <- nrow(board)
  cols <- ncol(board)
  
  adjacent_cells <- expand.grid(row = (row - 1):(row + 1), col = (col - 1):(col + 1))
  adjacent_cells <- adjacent_cells[adjacent_cells$row >= 1 & adjacent_cells$row <= rows & adjacent_cells$col >= 1 & adjacent_cells$col <= cols, ]
  adjacent_cells <- adjacent_cells[!(adjacent_cells$row == row & adjacent_cells$col == col), ]
  
  mine_count <- 0
  for (i in 1:nrow(adjacent_cells)) {
    mine_count <- mine_count + board[adjacent_cells[i, "row"], adjacent_cells[i, "col"]]
  }
  
  return(mine_count)
}

reveal_adjacent_cells <- function(game_state, input_rows, input_cols, row, col) {
  adjacent_cells <- expand.grid(row = (row - 1):(row + 1), col = (col - 1):(col + 1))
  adjacent_cells <- adjacent_cells[adjacent_cells$row >= 1 & adjacent_cells$row <= input_rows & adjacent_cells$col >= 1 & adjacent_cells$col <= input_cols, ]
  adjacent_cells <- adjacent_cells[!(adjacent_cells$row == row & adjacent_cells$col == col), ]
  
  for (i in 1:nrow(adjacent_cells)) {
    adj_row <- adjacent_cells[i, "row"]
    adj_col <- adjacent_cells[i, "col"]
    
    if (!game_state$revealed[adj_row, adj_col] && !game_state$flagged[adj_row, adj_col]) {
      game_state$revealed[adj_row, adj_col] <- TRUE
      if (count_adjacent_mines(game_state$board, adj_row, adj_col) == 0) {
        reveal_adjacent_cells(game_state, input_rows, input_cols, adj_row, adj_col)
      }
    }
  }
}




ui <- fluidPage(
  titlePanel("Démineur"),
  sidebarLayout(
    sidebarPanel(
      numericInput("rows", "Nombre de lignes:", min = 5, max = 30, value = 9),
      numericInput("cols", "Nombre de colonnes:", min = 5, max = 30, value = 9),
      numericInput("mines", "Nombre de mines:", min = 1, max = 100, value = 10),
      actionButton("reset", "Réinitialiser le jeu")
    ),
    mainPanel(uiOutput("board"))
  )
)

server <- function(input, output, session) {
  game_state <- reactiveValues(board = NULL, revealed = NULL, flagged = NULL)
  
  observeEvent(input$reset, {
    game_state$board <- generate_board(input$rows, input$cols, input$mines)
    game_state$revealed <- matrix(FALSE, nrow = input$rows, ncol = input$cols)
    game_state$flagged <- matrix(FALSE, nrow = input$rows, ncol = input$cols)
    print(game_state$board)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  output$board <- renderUI({
    req(game_state$board)
    
    board_buttons <- lapply(1:(input$rows * input$cols), function(i) {
      row <- (i - 1) %/% input$cols + 1
      col <- (i - 1) %% input$cols + 1
      
      if (game_state$revealed[row, col]) {
        if (game_state$board[row, col] == 1) {
          label <- "M"
        } else {
          label <- count_adjacent_mines(game_state$board, row, col)
        }
      } else if (game_state$flagged[row, col]) {
        label <- "F"
      } else {
        label <- ""
      }
      
      actionButton(paste0("cell_", row, "_", col), label, width = "30px", height = "30px")
    })
    
    do.call(tagList, board_buttons)
  })
  
  cell_ids <- reactive(sprintf("cell_%d_%d", rep(1:input$rows, each = input$cols), rep(1:input$cols, times = input$rows)))
  
  observeEvent(lapply(cell_ids(), function(cell_id) input[[cell_id]]), {
    for (cell_id in cell_ids()) {
      row <- as.integer(gsub("cell_(\\d+)_(\\d+)", "\\1", cell_id))
      col <- as.integer(gsub("cell_(\\d+)_(\\d+)", "\\2", cell_id))
      
      if (!is.na(input[[cell_id]]) && input[[cell_id]] %% 2 == 1) {
        if (game_state$revealed[row, col]) {
          game_state$flagged[row, col] <- !game_state$flagged[row, col]
        } else {
          if (game_state$board[row, col] == 1) {
            showModal(modalDialog(
              title = "Perdu !",
              "Vous avez touché une mine. Réessayez en réinitialisant le jeu.",
              easyClose = TRUE
            ))
          } else {
            game_state$revealed[row, col] <- TRUE
            if (count_adjacent_mines(game_state$board, row, col) == 0) {
              reveal_adjacent_cells(game_state, input$rows, input$cols, row, col)
            }
          }
        }
      }
    }
    
    if (sum(game_state$revealed & !game_state$board) == (input$rows * input$cols - input$mines)) {
      showModal(modalDialog(
        title = "Félicitations !",
        "Vous avez gagné la partie. Réinitialisez le jeu pour jouer à nouveau.",
        easyClose = TRUE
      ))
    }
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
}


shinyApp(ui = ui, server = server)

                      
