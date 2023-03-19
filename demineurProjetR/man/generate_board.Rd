\name{generate_board}
\alias{generate_board}
\title{Générer une grille de démineur}
\description{
  Génère une grille de démineur avec un nombre donné de mines.
}
\usage{
generate_board(rows, cols, mines)
}
\arguments{
  \item{rows}{Nombre de lignes de la grille.}
  \item{cols}{Nombre de colonnes de la grille.}
  \item{mines}{Nombre de mines à placer sur la grille.}
}
\value{
  Une matrice représentant la grille de démineur, avec 1 pour les mines et 0 pour les cases vides.
}
\examples{
board <- generate_board(10, 10, 20)
}
