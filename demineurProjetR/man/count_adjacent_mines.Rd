\name{count_adjacent_mines}
\alias{count_adjacent_mines}
\title{Compter les mines adjacentes}
\description{
  Compte le nombre de mines adjacentes à une case spécifique de la grille.
}
\usage{
count_adjacent_mines(board, row, col)
}
\arguments{
  \item{board}{Une matrice représentant la grille de démineur.}
  \item{row}{L'index de la ligne de la case à vérifier.}
  \item{col}{L'index de la colonne de la case à vérifier.}
}
\value{
  Le nombre de mines adjacentes à la case spécifiée (entre 0 et 8).
}
\examples{
board <- generate_board(10, 10, 20)
mine_count <- count_adjacent_mines(board, 5, 5)
}
