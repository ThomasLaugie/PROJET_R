library(shiny)
library(shinyjs)


ui <- fluidPage(
  # Create a 8x8 grid of buttons for the game board
  tags$style(HTML("body {background-color: green;}
                  footer{background-color: green}
                  #modal1 .modal-body {padding: 10px}
                  #modal1 .modal-content  {-webkit-border-radius: 6px !important;-moz-border-radius: 6px !important;border-radius: 6px !important;}
                  #modal1 .modal-dialog { width: 240px; display: inline-block; text-align: left; vertical-align: top;}
                  #modal1 .modal-header {background-color: #339FFF; border-top-left-radius: 6px; border-top-right-radius: 6px}
                  #modal1 .modal { text-align: right; padding-right:10px; padding-top: 24px;}
                  #modal1 .close { font-size: 16px}
                  .btn { width : 220%; height: 50px;background-color: orange}")),
  fluidRow(
    column(width = 7, offset = 4,
           fluidRow(
             
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
  ),
  mainPanel(
    align = "center" )
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
  board <- ifelse(board < 0, "💣" , board)
  return(board)
}
liste = c()
convex <- function(i,j) {
  for (x in max(i-1,1):min(i+1,8)) {
    for (y in max(j-1,1):min(j+1,8)) {
      liste <- paste0(x,y)}}
  return(liste)
  
}



server <- function(input, output, session) {
  
  # Initialize the game board
  
  
  # Store the board state in a reactiveValues object
  rv <- reactiveValues( board = initialize_board)
  rv$board <- initialize_board()
  
  # Define the event handlers for button clicks
  observe({
    # Reset the boar
    lapply(1:8, function(i) {
      lapply(1:8, function(j) {
        button_id <- paste0(i, j)
        observeEvent(input[[button_id]], {
          if (rv$board[i, j] == "💣") {
            # If the button is a mine, reveal all mines and end the game
            for (x in 1:8) {
              for (y in 1:8) {
                button_id <- paste0(x, y)
                
                updateActionButton(session, button_id, label = rv$board[x, y])
              }
            }
            # showModal( modalDialog(
            #   title = "Game over 😵! ","You hit a mine.",
            #   size = c("m"),
            #   easyClose = TRUE,
            #   #modalButton("Dismiss",tag$style(".btn { width : 100%; height: 50px;background-color: green}"))
            # ))
              
              showModal(tags$div(id="modal1", modalDialog(
                inputId = 'Dialog1', 
                title = HTML('<span style="color:white; font-size: 20px; font-weight:bold; font-family:sans-serif ">Game over 😵! <span>
               <button type = "button" class="close" data-dismiss="modal" ">
               <span style="color:white; ">X<span>
               </button> '),
                "You hit a mine.",
                easyClose = TRUE,
                footer = NULL )))
            
           
          }else {
            # If the button is not a mine, reveal the button and any adjacent buttons with 0 mines
            updateActionButton(session, button_id, label = rv$board[i, j])
            
            if (rv$board[i, j] == 0) {
              
              for (x in max(i-1,1):min(i+1,8)) {
                for (y in max(j-1,1):min(j+1,8)) {
                  if (rv$board[x, y] != "💣" ) {
                    adjacent_button_id <- paste0(x, y)
                    
                    if (!input[[adjacent_button_id]]) {
                      # Only reveal adjacent buttons if they have not been clicked yet
                      updateActionButton(session, adjacent_button_id, label = rv$board[x, y])
                    }
                    
                  }
                  for (x in max(i-1,1):min(i+1,8) ){
                    for(y in max(j-1,1):min(j+1,8) ){
                      if (rv$board[x, y] == 0){
                        for (t in max(x-1,1):min(x+1,8)) {
                          for (s in max(y-1,1):min(y+1,8)) {
                            adjacent_button_id <- paste0(t, s)
                            if (rv$board[t, s] != "💣" ) {
                              if (!input[[adjacent_button_id]]) {
                                # Only reveal adjacent buttons if they have not been clicked yet
                                updateActionButton(session, adjacent_button_id, label = rv$board[t, s])
                              }}
                          }}}}}
                  for (t in max(x-1,1):min(x+1,8)) {
                    for (s in max(y-1,1):min(y+1,8)) {
                      if (rv$board[t, s] == 0){
                        
                        for (a in max(t-1,1):min(t+1,8)) {
                          for (b in max(s-1,1):min(s+1,8)) {
                            adjacent_button_id <- paste0(a,b)
                            if (rv$board[a, b] != "💣" ) {
                              if (!input[[adjacent_button_id]]) {
                                # Only reveal adjacent buttons if they have not been clicked yet
                                updateActionButton(session, adjacent_button_id, label = rv$board[a, b])
                              }}
                          }}}}}
                  
                  
                  
                  
                }
              }
            }
          }})
      })
    })
  })
}
shinyApp(ui, server)