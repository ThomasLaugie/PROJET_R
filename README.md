# DemineurProjetR

DemineurProjetR est une implémentation du célèbre jeu de démineur en R. Le jeu consiste à découvrir un champ de mines sans déclencher aucune mine. Les joueurs doivent révéler les cases du champ de mines tout en évitant les cases contenant des mines. Chaque case révélée affiche soit un chiffre indiquant le nombre de mines adjacentes, soit une mine (entraînant la fin de la partie), soit une case vide. En se basant sur ces informations, les joueurs doivent déduire l'emplacement des mines et marquer les cases suspectes avec des drapeaux. Le but du jeu est de découvrir toutes les cases sans mines et de marquer correctement toutes les cases contenant des mines.

## Installation

Vous pouvez installer DemineurProjetR à partir de GitHub en utilisant la commande suivante :

```R
# Installer le package devtools si vous ne l'avez pas déjà
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

# Installer DemineurProjetR à partir de GitHub
devtools::install_github("votre_nom_utilisateur/demineurProjetR")
```
N'oubliez pas de remplacer "votre_nom_utilisateur" par votre nom d'utilisateur GitHub.

## Utilisation
Après avoir installé le package, vous pouvez charger DemineurProjetR et lancer le jeu en utilisant les commandes suivantes :

```R
Copy code
# Charger le package
library(demineurProjetR)

# Lancer le jeu
demineur
```
