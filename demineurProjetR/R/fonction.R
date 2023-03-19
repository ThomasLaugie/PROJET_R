library(shiny)
library(shinyjs)
library(dbplyr)

generate_board <- function(rows, cols, mines) {
  board <- matrix(0, nrow = rows, ncol = cols)
  mine_positions <- sample(1:(rows * cols), size = mines)
  board[mine_positions] <- 1
  return(board)
}
count_adjacent_mines <- function(board, row, col) {
  rows <- nrow(board)
  cols <- ncol(board)
  
  if (row < 1 || row > rows || col < 1 || col > cols) {
    return(0)
  }
  
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