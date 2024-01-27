library(dplyr)
library(lubridate)

################################################################################
## DATA LOADER #################################################################
################################################################################
# Chemin vers le dossier contenant les fichiers
chemin_dossier <- "/Users/noedebrois/Desktop/STAT_PROJET/NonParametricStatsProject/BDD_MONTHLY_WORLD/"
# Obtenir la liste des fichiers dans le dossier
fichiers <- list.files(path = chemin_dossier, pattern = "\\.csv", full.names = TRUE)
# Initialiser une liste pour stocker les données
liste_donnees <- list()
# Boucle pour lire chaque fichier et stocker les données dans la liste
for (fichier in fichiers) {
  # Lire le fichier et stocker les données dans la liste
  donnees <- read.table(fichier, header = TRUE, sep = ",")
  liste_donnees[[basename(fichier)]] <- donnees
}

# Calculer la longueur de la liste_donnees
longueur_liste <- length(liste_donnees)

# Créer un tableau vide avec le nombre de lignes égal à la longueur de liste_donnees
RP <- array(NA, dim = c(longueur_liste, 12, 3))

# Convertir le tableau en data frame avec des noms de lignes et de colonnes
# RP <- as.data.frame(RP, row.names = NULL)

# Ajouter des noms de lignes basés sur les noms de fichiers et les mois
# for (i in 1:longueur_liste) {
#   nom_fichier <- basename(fichiers[i])
#   rownames(RP)[i] <- nom_fichier
# }
# colnames(RP) <- month.name

# Définir le nombre total d'itérations (ajustez selon votre besoin)
nombre_iterations <- length(liste_donnees)
# Créer une barre de progression texte
barre_progression <- txtProgressBar(min = 0, max = nombre_iterations, style = 3)
# Boucle avec la barre de progression
for (i in 1:length(liste_donnees)){
# Importer les données depuis un fichier
data <- liste_donnees[[i]] # accéder à la i-ème table
if (nrow(data) == 0){
  next
}
data <- na.omit(data)
data$time <- as.Date(data$time)

for (mois in 1:12){
data_annee_en_cours = data %>% filter(month(time) == mois)
# data_annee_en_cours = na.omit(data_annee_en_cours)
if (nrow(data_annee_en_cours) == 0){
  next
}
annees <- unique(year(data_annee_en_cours$time)) # seq(1978, 2022, by = 1)

# Variables de réponse et de prédicteur
response  <- data_annee_en_cours$tavg
regressor <- annees

# Tracer les données
# plot(regressor, response, asp=1)
# grid()

# Ajuster un modèle linéaire
fm <- lm(response ~ regressor)

# Tracer la droite de régression et les valeurs ajustées
# abline(coefficients(fm), col='red')
# points(regressor, fitted(fm), col='red', pch=16)

################################################################################
### Bootstrap Inference ########################################################
################################################################################
# Calculer les résidus et les valeurs ajustées
fitted.obs <- fitted(fm)
res.obs    <- residuals(fm)

# Coefficients estimés du modèle original
b0.obs <- coefficients(fm)[1] # ordonnée à l'origine de la droite de régression
b1.obs <- coefficients(fm)[2] # coefficient directeur de la droite de régression

# Fixer la graine aléatoire pour la reproductibilité
set.seed(24021979)
# Nombre d'itérations Bootstrap
B <- 1000
# Initialiser des vecteurs pour stocker les estimations Bootstrap des coefficients
T.boot.b0 <- numeric(B)
T.boot.b1 <- numeric(B)

# Boucle sur les itérations Bootstrap
for(b in 1:B)
{
  # Ré-échantillonner les résidus avec remplacement
  response.b <- fitted.obs + sample(res.obs, replace = TRUE)
  # Ajuster un modèle linéaire sur les données Bootstrap
  fm.b <- lm(response.b ~ regressor)
  # Stocker les coefficients estimés Bootstrap
  T.boot.b0[b] <- coefficients(fm.b)[1]
  T.boot.b1[b] <- coefficients(fm.b)[2]
}

# Diviser la fenêtre graphique en 2 lignes et 1 colonne
# par(mfrow=c(2,1), mar = c(4, 4, 2, 1))
# Tracer les fonctions de répartition empiriques des coefficients Bootstrap
# plot(ecdf(T.boot.b0), main='Intercept')
# abline(v=b0.obs, lty=2)
# plot(ecdf(T.boot.b1), main='Slope', col='red')
# abline(v=b1.obs, lty=2, col='red')

################################################################################
### Intervalle de confiance Reverse Percentile pour l'ordonnée à l'origine #####
################################################################################
# Diviser la fenêtre graphique en 2 lignes et 1 colonne
# par(mfrow=c(2,1))

# Niveau de confiance
alpha <- 0.05

# Calculer les quantiles pour l'intercept
right.quantile.b0 <- quantile(T.boot.b0, 1 - alpha/2, na.rm = TRUE)
left.quantile.b0  <- quantile(T.boot.b0, alpha/2, na.rm = TRUE)

# Valeurs observées
b0_pointwise <- b0.obs
b0_right <- right.quantile.b0 - b0.obs
b0_left <- left.quantile.b0  - b0.obs

# Calculer l'intervalle de confiance Reverse Percentile
CI.RP.b0 <- c(b0.obs - (right.quantile.b0 - b0.obs), b0.obs - (left.quantile.b0 - b0.obs))

# Tracer la fonction de répartition empirique de l'intercept avec l'intervalle de confiance
# plot(ecdf(T.boot.b0), main='Intercept')
# abline(v = b0.obs, lty=2)
# abline(v = CI.RP.b0)

################################################################################
### Intervalle de confiance Reverse Percentile pour le coefficient directeur ###
################################################################################
alpha <- 0.05

# Calculer les quantiles pour la pente
right.quantile.b1 <- quantile(T.boot.b1, 1 - alpha/2, na.rm = TRUE)
left.quantile.b1  <- quantile(T.boot.b1, alpha/2, na.rm = TRUE)

# Valeurs observées
b1_pointwise <- b1.obs
b1_right <- right.quantile.b1 - b1.obs
b1_left <- left.quantile.b1  - b1.obs

# Calculer l'intervalle de confiance Reverse Percentile
CI.RP.b1 <- c(b1.obs - (right.quantile.b1 - b1.obs), b1.obs - (left.quantile.b1 - b1.obs))
CI.RP.b1

# Tracer la fonction de répartition empirique de la pente avec l'intervalle de confiance
# plot(ecdf(T.boot.b1), main='Slope', col='red')
# abline(v = b1.obs, lty=2, col='red')
# abline(v = CI.RP.b1, col='red')

RP[i, mois, 1] <- unname(CI.RP.b1)[1]
RP[i, mois, 2] <- unname(b1.obs)
RP[i, mois, 3] <- unname(CI.RP.b1)[2]
}
# Mettre à jour la barre de progression
setTxtProgressBar(barre_progression, i)
# Forcer l'affichage immédiat de la barre de progression dans la console
flush.console()
}
# Imprimer une nouvelle ligne pour éviter d'écraser la barre de progression
cat("\n")
# Fermer la barre de progression à la fin
close(barre_progression)

## COMMENTAIRE :
# Lorsqu'un intervalle de confiance ne contient pas zéro pour un coefficient 
# estimé, cela indique que l'on peut rejeter l'hypothèse nulle selon laquelle 
# ce coefficient est égal à zéro. 
# Dans le cadre de ce test bootstrap sur les résidus, nous avons effectué une
# procédure de bootstrap pour obtenir des intervalles de confiance autour des 
# coefficients de régression. L'intervalle de confiance ne contient pas zéro,
# cela suggère que le coefficient en question (la pente) est probablement 
# différent de zéro. En conclusion, il y a un shift en température pour
# Barcelone, entre 1978 et 2022. 

# Renommer les lignes et les colonnes
noms_lignes <- basename(fichiers)
dimnames(RP)[[1]] <- noms_lignes 
noms_colonnes <- c("RP Int January", "RP Int February", "RP Int March", "RP Int April", "RP Int May", "RP Int June", "RP Int July", "RP Int August", "RP Int September", "RP Int October", "RP Int November", "RP Int December")
dimnames(RP)[[2]] <- noms_colonnes
noms_matrices <- c("M1", "M2", "M3")
dimnames(RP)[[3]] <- noms_matrices

# Spécifier le chemin complet vers le fichier de sortie
chemin_fichier <- "/Users/noedebrois/Desktop/STAT_PROJET/NonParametricStatsProject/EXPORT.csv"
# Exporter le dataframe en format CSV
write.csv(RP, file = chemin_fichier, row.names = TRUE)

