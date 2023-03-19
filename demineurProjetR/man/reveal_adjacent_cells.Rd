\name{reveal_adjacent_cells}
\alias{reveal_adjacent_cells}
\title{Révéler les cases adjacentes}
\description{
  Révèle récursivement les cases adjacentes à une case spécifique de la grille.
}
\usage{
reveal_adjacent_cells(game_state, input_rows, input_cols, row, col)
}
\arguments{
  \item{game_state}{Une liste représentant l'état du jeu, contenant la grille de démineur et les matrices de cases révélées et marquées.}
  \item{input_rows}{Nombre de lignes de la grille.}
  \item{input_cols}{Nombre de colonnes de la grille.}
  \item{row}{L'index de la ligne de la case à partir de laquelle révéler les cases adjacentes.}
  \item{col}{L'index de la colonne de la case à partir de laquelle révéler les cases adjacentes.}
}
\value{
  Aucune valeur de retour. La fonction modifie directement l'état du jeu en révélant les cases adjacentes.
}
\examples{
board <- generate_board(10, 10, 20)
game_state <- list(board = board, revealed = matrix(FALSE, 10, 10), flagged = matrix(FALSE, 10, 10))
reveal_adjacent_cells(game_state, 10, 10, 5, 5)
}
