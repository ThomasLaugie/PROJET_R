
library(shiny)
defaultColor <- "padding:20px; font-size:120%;
         color: white; background-color: Grey;
         border-color: #2C3E50"
ui <- fluidPage(
  # Create a 8x8 grid of buttons for the game board
  numericInput("i", "i", 3),
  numericInput("j", "j", 3),
  fluidRow(
    column(width = 10, offset = 1,
           fluidRow(
             
             column(width = 1, actionButton("11", "",style=defaultColor)),
             column(width = 1, actionButton("12", "",style=defaultColor)),
             column(width = 1, actionButton("13", "",style=defaultColor)),
             column(width = 1, actionButton("14", "",style=defaultColor)),
             column(width = 1, actionButton("15", "",style=defaultColor)),
             column(width = 1, actionButton("16", "",style=defaultColor)),
             column(width = 1, actionButton("17", "",style=defaultColor)),
             column(width = 1, actionButton("18", "",style=defaultColor))
           ),
           fluidRow(
             column(width = 1, actionButton("21", "",style=defaultColor)),
             column(width = 1, actionButton("22", "",style=defaultColor)),
             column(width = 1, actionButton("23", "",style=defaultColor)),
             column(width = 1, actionButton("24", "",style=defaultColor)),
             column(width = 1, actionButton("25", "",style=defaultColor)),
             column(width = 1, actionButton("26", "",style=defaultColor)),
             column(width = 1, actionButton("27", "",style=defaultColor)),
             column(width = 1, actionButton("28", "",style=defaultColor))
           ),
           fluidRow(
             column(width = 1, actionButton("31", "",style=defaultColor)),
             column(width = 1, actionButton("32", "",style=defaultColor)),
             column(width = 1, actionButton("33", "",style=defaultColor)),
             column(width = 1, actionButton("34", "",style=defaultColor)),
             column(width = 1, actionButton("35", "",style=defaultColor)),
             column(width = 1, actionButton("36", "",style=defaultColor)),
             column(width = 1, actionButton("37", "",style=defaultColor)),
             column(width = 1, actionButton("38", "",style=defaultColor))
           ),
           fluidRow(
             column(width = 1, actionButton("41", "",style=defaultColor)),
             column(width = 1, actionButton("42", "",style=defaultColor)),
             column(width = 1, actionButton("43", "",style=defaultColor)),
             column(width = 1, actionButton("44", "",style=defaultColor)),
             column(width = 1, actionButton("45", "",style=defaultColor)),
             column(width = 1, actionButton("46", "",style=defaultColor)),
             column(width = 1, actionButton("47", "",style=defaultColor)),
             column(width = 1, actionButton("48", "",style=defaultColor))
           ),
           fluidRow(
             column(width = 1, actionButton("51", "",style=defaultColor)),
             column(width = 1, actionButton("52", "",style=defaultColor)),
             column(width = 1, actionButton("53", "",style=defaultColor)),
             column(width = 1, actionButton("54", "",style=defaultColor)),
             column(width = 1, actionButton("55", "",style=defaultColor)),
             column(width = 1, actionButton("56", "",style=defaultColor)),
             column(width = 1, actionButton("57", "",style=defaultColor)),
             column(width = 1, actionButton("58", "",style=defaultColor))
           ),
           fluidRow(
             column(width = 1, actionButton("61", "",style=defaultColor)),
             column(width = 1, actionButton("62", "",style=defaultColor)),
             column(width = 1, actionButton("63", "",style=defaultColor)),
             column(width = 1, actionButton("64", "",style=defaultColor)),
             column(width = 1, actionButton("65", "",style=defaultColor)),
             column(width = 1, actionButton("66", "",style=defaultColor)),
             column(width = 1, actionButton("67", "",style=defaultColor)),
             column(width = 1, actionButton("68", "",style=defaultColor))
           ),
           fluidRow(
             column(width = 1, actionButton("71", "",style=defaultColor)),
             column(width = 1, actionButton("72", "",style=defaultColor)),
             column(width = 1, actionButton("73", "",style=defaultColor)),
             column(width = 1, actionButton("74", "",style=defaultColor)),
             column(width = 1, actionButton("75", "",style=defaultColor)),
             column(width = 1, actionButton("76", "",style=defaultColor)),
             column(width = 1, actionButton("77", "",style=defaultColor)),
             column(width = 1, actionButton("78", "",style=defaultColor))
           ),
           fluidRow(
             column(width = 1, actionButton("81", "",style=defaultColor)),
             column(width = 1, actionButton("82", "",style=defaultColor)),
             column(width = 1, actionButton("83", "",style=defaultColor)),
             column(width = 1, actionButton("84", "",style=defaultColor)),
             column(width = 1, actionButton("85", "",style=defaultColor)),
             column(width = 1, actionButton("86", "",style=defaultColor)),
             column(width = 1, actionButton("87", "",style=defaultColor)),
             column(width = 1, actionButton("88", "",style=defaultColor))
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


server <- function(input, output, session) {
  
  # Initialize the game board
  
  # Store the board state in a reactiveValues object
  rv <- reactiveValues(board = initialize_board)
  rv$board <- initialize_board()
  
  # Define the event handlers for button clicks
  observe({
    # Reset the boar
    lapply(1:8, function(i) {
      lapply(1:8, function(j) {
        button_id <- paste0(i, j)
        observeEvent(input[[button_id]], {
          if (rv$board[i, j] == -1) {
            # If the button is a mine, reveal all mines and end the game
            for (x in 1:8) {
              for (y in 1:8) {
                button_id <- paste0(x, y)
                updateActionButton(session, button_id, label = rv$board[x, y])
              }
            }
            showModal(modalDialog("Game over! You hit a mine.", easyClose = TRUE))
          }else {
            # If the button is not a mine, reveal the button and any adjacent buttons with 0 mines
            updateActionButton(session, button_id, label = rv$board[i, j])
            if (rv$board[i, j] == 0) {
              for (x in max(i-1,1):min(i+1,8)) {
                for (y in max(j-1,1):min(j+1,8)) {
                  if (rv$board[x, y] != -1) {
                    adjacent_button_id <- paste0(x, y)
                    if (!input[[adjacent_button_id]]) {
                      # Only reveal adjacent buttons if they have not been clicked yet
                      updateActionButton(session, adjacent_button_id, label = rv$board[x, y])
                    }
                  }
                }
              }
            }
          }})
      })
    })
  })
}
shinyApp(ui, server)


