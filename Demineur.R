library(shiny)

ui <- fluidPage(
  # Create a 8x8 grid of buttons for the game board
  fluidRow(
    column(width = 10, offset = 1,
           fluidRow(
             column(width = 1),
             column(width = 1, actionButton("11", "")),
             column(width = 1, actionButton("12", "")),
             column(width = 1, actionButton("13", "")),
             column(width = 1, actionButton("14", "")),
             column(width = 1, actionButton("15", "")),
             column(width = 1, actionButton("16", "")),
             column(width = 1, actionButton("17", "")),
             column(width = 1, actionButton("18", ""))
           ),
           fluidRow(
             column(width = 1, actionButton("21", "")),
             column(width = 1, actionButton("22", "")),
             column(width = 1, actionButton("23", "")),
             column(width = 1, actionButton("24", "")),
             column(width = 1, actionButton("25", "")),
             column(width = 1, actionButton("26", "")),
             column(width = 1, actionButton("27", "")),
             column(width = 1, actionButton("28", ""))
           ),
           fluidRow(
             column(width = 1, actionButton("31", "")),
             column(width = 1, actionButton("32", "")),
             column(width = 1, actionButton("33", "")),
             column(width = 1, actionButton("34", "")),
             column(width = 1, actionButton("35", "")),
             column(width = 1, actionButton("36", "")),
             column(width = 1, actionButton("37", "")),
             column(width = 1, actionButton("38", ""))
           ),
           fluidRow(
             column(width = 1, actionButton("41", "")),
             column(width = 1, actionButton("42", "")),
             column(width = 1, actionButton("43", "")),
             column(width = 1, actionButton("44", "")),
             column(width = 1, actionButton("45", "")),
             column(width = 1, actionButton("46", "")),
             column(width = 1, actionButton("47", "")),
             column(width = 1, actionButton("48", ""))
           ),
           fluidRow(
             column(width = 1, actionButton("51", "")),
             column(width = 1, actionButton("52", "")),
             column(width = 1, actionButton("53", "")),
             column(width = 1, actionButton("54", "")),
             column(width = 1, actionButton("55", "")),
             column(width = 1, actionButton("56", "")),
             column(width = 1, actionButton("57", "")),
             column(width = 1, actionButton("58", ""))
           ),
           fluidRow(
             column(width = 1, actionButton("61", "")),
             column(width = 1, actionButton("62", "")),
             column(width = 1, actionButton("63", "")),
             column(width = 1, actionButton("64", "")),
             column(width = 1, actionButton("65", "")),
             column(width = 1, actionButton("66", "")),
             column(width = 1, actionButton("67", "")),
             column(width = 1, actionButton("68", ""))
           ),
           fluidRow(
             column(width = 1, actionButton("71", "")),
             column(width = 1, actionButton("72", "")),
             column(width = 1, actionButton("73", "")),
             column(width = 1, actionButton("74", "")),
             column(width = 1, actionButton("75", "")),
             column(width = 1, actionButton("76", "")),
             column(width = 1, actionButton("77", "")),
             column(width = 1, actionButton("78", ""))
           ),
           fluidRow(
             column(width = 1, actionButton("81", "")),
             column(width = 1, actionButton("82", "")),
             column(width = 1, actionButton("83", "")),
             column(width = 1, actionButton("84", "")),
             column(width = 1, actionButton("85", "")),
             column(width = 1, actionButton("86", "")),
             column(width = 1, actionButton("87", "")),
             column(width = 1, actionButton("88", ""))
           )
    )
  )
)

initialize_board <- function() {
  # Create a blank 8x8 game board
  board <- matrix(0, nrow = 8, ncol = 8)
  
  # Randomly place 10 mines on the game board
  mine_indices <- sample(1:64, 10, replace = FALSE)
  board[mine_indices] <- -1
  
  # Assign the appropriate number of mines to each button based on its adjacent buttons
  for (i in 1:8) {
    for (j in 1:8) {
      if (board[i, j] != -1) {
        num_adjacent_mines <- sum(board[max(i-1,1):min(i+1,8), max(j-1,1):min(j+1,8)] == -1)
        board[i, j] <- num_adjacent_mines
      }
    }
  }
  
  return(board)
}

server <- function(input, output) {
  # Initialize the game board
  board <- initialize_board()
  
  # Define the event handlers for button clicks
  for (i in 1:8) {
    for (j in 1:8) {
      button_id <- paste0(i, j)
      observeEvent(input[[button_id]], {
        if (board[i, j] == -1) {
          # If the button is a mine, reveal all mines and end the game
          for (x in 1:8) {
            for (y in 1:8) {
              if (board[x, y] == -1) {
                button_id <- paste0(x, y)
                updateActionButton(session, button_id, label = "a")
              }
            }
          }
          showModal(modalDialog("Game over! You hit a mine.", easyClose = TRUE))
        } else {
          # If the button is not a mine, reveal the button and any adjacent buttons with 0 mines
          updateActionButton(session, button_id, label = board[i, j])
          if (board[i, j] == 0) {
            for (x in max(i-1,1):min(i+1,8)) {
              for (y in max(j-1,1):min(j+1,8)) {
                if (board[x, y] != -1) {
                  adjacent_button_id <- paste0(x, y)
                  if (!input[[adjacent_button_id]]) {
                    # Only reveal adjacent buttons if they have not been clicked yet
                    updateActionButton(session, adjacent_button_id, label = board[x, y])
                  }
                }
              }
            }
          }
        }
      })
    }
  }
}


shinyApp(ui, server)
    
          
                                            
