library(shiny)
library(shinyjs)
library(dbplyr)
source('R/ui.R')
source('R/fonction.R')

server <- function(input, output, session) {
  game_state <- reactiveValues(board = NULL, revealed = NULL, flagged = NULL, game_lost = FALSE, start_time = NULL)
  
  input_rows <- reactive(input$rows)
  input_cols <- reactive(input$cols)
  input_mines <- reactive(input$mines)
  
  observeEvent(input$reset, {
    game_state$board <- generate_board(input_rows(), input_cols(), input_mines())
    game_state$revealed <- matrix(FALSE, nrow = input_rows(), ncol = input_cols())
    game_state$flagged <- matrix(FALSE, nrow = input_rows(), ncol = input_cols())
    game_state$game_lost <- FALSE
    game_state$start_time <- Sys.time()
    print(game_state$board)
    
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  observeEvent(c(input$rows, input$cols, input$mines), {
    if (input$mines > input$rows * input$cols) {shinyjs::toggleState("reset", FALSE)}
    else{
      shinyjs::toggleState("reset", TRUE)
      game_state$board <- generate_board(input_rows(), input_cols(), input_mines())
      game_state$revealed <- matrix(FALSE, nrow = input_rows(), ncol = input_cols())
      game_state$flagged <- matrix(FALSE, nrow = input_rows(), ncol = input_cols())
      game_state$game_lost <- FALSE
      game_state$start_time <- Sys.time()
    }})
  
  observeEvent(input$difficulty, {
    if (input$difficulty == "Facile") {
      updateSliderInput(session, "rows", value = 10)
      updateSliderInput(session, "cols", value = 10)
      updateSliderInput(session, "mines", value = 10)
    } else if (input$difficulty == "Moyen") {
      updateSliderInput(session, "rows", value = 20)
      updateSliderInput(session, "cols", value = 20)
      updateSliderInput(session, "mines", value = 40)
    } else if (input$difficulty == "Difficile") {
      updateSliderInput(session, "rows", value = 20)
      updateSliderInput(session, "cols", value = 20)
      updateSliderInput(session, "mines", value = 100)
    }
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  
  
  output$error_message <- renderUI({
    if (input$mines > input$rows * input$cols) {
      tags$div(style = "color: red;font-size: 24px;", "Attention ! Trop de bombes.")
    } else {
      NULL
    }
  })
  
  output$board <- renderUI({
    if (is.null(input_rows()) || is.null(input_cols())) return(NULL)
    req(game_state$board)
    
    board_size <- isolate({
      list(rows = input_rows(), cols = input_cols())
    })
    
    board_buttons <- lapply(1:(board_size$rows * board_size$cols), function(i) {
      row <- (i - 1) %/% board_size$cols + 1
      col <- (i - 1) %% board_size$cols + 1
      
      if (game_state$revealed[row, col]) {
        if (game_state$board[row, col] == 1) {
          label <- "💣"
          button_style <- "background-color: red;"
        } else {
          label <- count_adjacent_mines(game_state$board, row, col)
          button_style <- "background-color: green;"
        }
      } else if (game_state$flagged[row, col]) {
        label <- "\U1F6A9"
        button_style <- "background-color: yellow;"
      } else {
        label <- "?"
        button_style <- "background-color: gray;"
      }
      
      actionButton(paste0("cell_", row, "_", col), label, width = "50px", height = "50px", style = button_style)
    })
    
    
    board_matrix <- matrix(board_buttons, nrow = input_rows(), ncol = input_cols(), byrow = TRUE)
    
    apply(board_matrix, 1, function(row) {
      do.call(tags$div, lapply(row, function(button) {
        div(style = "display: inline-block; margin: 0px;", button)
      }))
    }) %>%
      do.call(tags$div, .)
  })
  
  
  cell_ids <- reactive({
    req(input_rows(), input_cols())
    sprintf("cell_%d_%d", rep(1:input_rows(), each = input_cols()), rep(1:input_cols(), times = input_rows()))
  })
  
  
  observeEvent(lapply(cell_ids(), function(cell_id) {
    if (is.null(input[[cell_id]]) || is.null(input_rows()) || is.null(input_cols())) return(NULL)
    input[[cell_id]]
  }), {
    
    req(input_rows(), input_cols())
    
    if (is.null(input_rows()) || is.null(input_cols())) return()
    
    valid_cell_ids <- sprintf("cell_%d_%d", rep(1:input_rows(), each = input_cols()), rep(1:input_cols(), times = input_rows()))
    for (cell_id in cell_ids()) {
      row <- as.integer(gsub("cell_(\\d+)_(\\d+)", "\\1", cell_id))
      col <- as.integer(gsub("cell_(\\d+)_(\\d+)", "\\2", cell_id))
      
      if (!is.na(input[[cell_id]]) && !is.na(input[[cell_id]]) && input[[cell_id]] %% 2 == 1) {
        if (input$action == "Drapeau") {
          game_state$flagged[row, col] <- !game_state$flagged[row, col]
        } else {
          if (!game_state$flagged[row, col]) {
            if (game_state$board[row, col] == 1) {
              game_state$game_lost <- TRUE
              game_state$revealed[] <- TRUE
              showModal(modalDialog(
                title = "Perdu !",
                "Vous avez touché une mine. Réessayez en réinitialisant le jeu.",
                easyClose = TRUE
              ))
              break
            } else {
              game_state$revealed[row, col] <- TRUE
              if (count_adjacent_mines(game_state$board, row, col) == 0) {
                reveal_adjacent_cells(game_state, input_rows(), input_cols(), row, col)
              }
            }
          }
        }
      }
    }
    if (!game_state$game_lost && sum(game_state$revealed & !game_state$board) == (input_rows() * input_cols() - input_mines())) {
      showModal(modalDialog(
        title = "Félicitations !",
        "Vous avez gagné la partie. Réinitialisez le jeu pour jouer à nouveau.",
        easyClose = TRUE
      ))
    }
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  
  observe({
    if (!is.null(game_state$start_time) && !game_state$game_lost && !is.null(game_state$board)) {
      elapsed_time <- as.numeric(difftime(Sys.time(), game_state$start_time, units = "secs"))
      if (elapsed_time > 120) {
        game_state$game_lost <- TRUE
        game_state$revealed[] <- TRUE
        showModal(modalDialog(
          title = "Perdu !",
          "Le temps est écoulé. Réessayez en réinitialisant le jeu.",
          easyClose = TRUE
        ))
      } else {
        invalidateLater(1000)  # Check again in 1 second.
      }
    }
  })
  
  output$timer <- renderText({
    if (!is.null(game_state$start_time)) {
      elapsed_time <- as.numeric(difftime(Sys.time(), game_state$start_time, units = "secs"))
      remaining_time <- 120 - elapsed_time
      if (remaining_time > 0) {
        invalidateLater(1000)
        remaining_time_int <- as.integer(remaining_time)
        return(sprintf("Temps restant : %02d:%02d", remaining_time_int %/% 60, remaining_time_int %% 60))
      } else {
        return("Temps écoulé")
      }
    } else {
      return("Cliquez sur 'Réinitialiser le jeu' pour commencer")
    }
  })
  
  output$flags_count <- renderText({
    flags_placed <- sum(game_state$flagged)
    return(paste("Drapeaux placés :", flags_placed))
  })
  
}
