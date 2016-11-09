# Initialisation du répertoire courant et importation des données
chemin <- getwd()
setwd(chemin)
bdd_complet <- read.csv2(file="geoducation-data2_cecile.csv", sep=";", header=TRUE, na.strings = "")
#names(bdd_complet)

#affichage des données
#summary(bdd_complet)
#bdd_complet

#####################
# Régression simple #
#####################

# [1] Extraction des champs qui nous interessent
chu_reg_simple_extrac_data = bdd_complet[, c('Etablissement','Code.Etablissement','Effectif.Présents.Total.séries','Taux.Brut.de.réussite.Total.séries')]

# [2] Renommage des colonnes
names(chu_reg_simple_extrac_data)[names(chu_reg_simple_extrac_data)=="Etablissement"] <- "Nom_lycee"
names(chu_reg_simple_extrac_data)[names(chu_reg_simple_extrac_data)=="Code.Etablissement"] <- "Code_lycee"
names(chu_reg_simple_extrac_data)[names(chu_reg_simple_extrac_data)=="Effectif.Présents.Total.séries"] <- "Effectif_lycee"
names(chu_reg_simple_extrac_data)[names(chu_reg_simple_extrac_data)=="Taux.Brut.de.réussite.Total.séries"] <- "Taux_reussite"

# [3] Copie de l'extraction pour travailler dessus
chu_reg_simple_bdd = chu_reg_simple_extrac_data 

# [4] Nettoyage des données
# suppression des lignes contenant des NA
chu_reg_simple_bdd <- na.omit(chu_reg_simple_bdd)
# conversion pour la reconnaissance des variables QUANTI
for (i in 3:ncol(chu_reg_simple_bdd)) {
  chu_reg_simple_bdd[, i] <- as.numeric(as.character(chu_reg_simple_bdd[, i]))
}
# visualisation générale des données
str(chu_reg_simple_bdd)
summary(chu_reg_simple_bdd)
plot(Taux_reussite ~ Effectif_lycee, data=chu_reg_simple_bdd)
# Chaque point représente pour un lycée donné, l'effectif de l'établissement et son taux de réussite au baccalauréat
# On remarque que la liaison entre les effectifs et le taux de réussite ne semble pas être linéaire

# [5] Estimation du paramètre Effectif_lycee
# regression linéaire : nuage de points + droite de regression
chu_reg_simple <- lm(Taux_reussite ~ Effectif_lycee, data=chu_reg_simple_bdd)
summary(chu_reg_simple)
chu_reg_simple
abline(chu_reg_simple, col = 'red')
# affichage de l'équation de la droite de régression
title("Nuage de points & droite de regression")
text(400, 50, as.expression(substitute(y==b+a*x, list(
  a=round(chu_reg_simple$coefficients[2],3),
  b=round(chu_reg_simple$coefficients[1],3)
))), col = 'red')

# [6] Conclusion

# Rédaction à revoir

# Std.Error = 0.251276 : Ecart-type associé à l'estimation des effectifs est petite, ce qui signifie un gage de stabilité du modèle et donc du pouvoir prédictif (valeur de b stable)
# Residual standard error = 6.122 : Estimateur de l'écart-type résiduel est faible donc bon pouvoir prédictif mais DDL à 2286
# Multiple R-squared = 0.007084 : coeff de corrélation (% de variations expliquées par le modèle), R² doit être proche de 1 pour bon pouvoir explicatif

# le modèle n'a pas un bon pouvoir explicatif sur les données : R²=0.007084
# le pouvoir prédictif risque d'être entaché par l'instabilité du coefficient b et une variance résiduelle importante

#############################################################################


#######################
# Régression multiple #
#######################

# [1] Extraction des champs qui nous interessent
chu_reg_multi_extrac_data = bdd_complet[, c(
  'Secteur.Public.PU.Privé.PR',
  'Sructure.p.dagogique.en.7.groupes',
  'Effectif.Présents.série.L',
  'Effectif.Présents.série.ES', 
  'Effectif.Présents.série.S', 
  'Taux.Brut.de.réussite.série.L', 
  'Taux.Brut.de.réussite.série.ES',
  'Taux.Brut.de.réussite.série.S',
  'Taux.Brut.de.réussite.Total.séries',
  'Taux.accés.Brut.premi.re.BAC',
  'Taux.accés.Brut.terminale.BAC'
)]

# [2] Renommage des colonnes
names(chu_reg_multi_extrac_data) <- c(
  'Secteur_lycee',
  'Sructure_lycee',
  'Effectif_L',
  'Effectif_ES', 
  'Effectif_S', 
  'Reussite_L', 
  'Reussite_ES',
  'Reussite_S',
  'Reussite_Total',
  'Acces_prem_BAC',
  'Acces_term_BAC'
)

# [3] Copie de l'extraction pour travailler dessus
chu_reg_multi_bdd = chu_reg_multi_extrac_data
str(chu_reg_multi_bdd)

# [4] Nettoyage des données
# on ne s'interesse que aux données des établissements avec uniquement des filières généraux
chu_reg_multi_bdd <- subset(chu_reg_multi_bdd, Sructure_lycee == "A")
# suppression des lignes contenant des NA
chu_reg_multi_bdd <- na.omit(chu_reg_multi_bdd)
# suppression des colonnes non utilisées pour la regression
chu_reg_multi_bdd <- chu_reg_multi_bdd[,-2]
# conversion pour la reconnaissance des variables QUANTI
for (i in 2:ncol(chu_reg_multi_bdd)) {
  chu_reg_multi_bdd[, i] <- as.numeric(as.character(chu_reg_multi_bdd[, i]))
}
# remplacement de la variable quali en quanti
chu_reg_multi_bdd[, 1] <- as.character(chu_reg_multi_bdd[, 1])
chu_reg_multi_bdd$Secteur_lycee[chu_reg_multi_bdd$Secteur_lycee=="PR"]<-"1"
chu_reg_multi_bdd$Secteur_lycee[chu_reg_multi_bdd$Secteur_lycee=="PU"]<-"0"
chu_reg_multi_bdd[, 1] <- as.numeric(chu_reg_multi_bdd[, 1])

# [5] Estimation des paramètres explicatives
chu_reg_multi <- lm(Reussite_Total ~ ., data=chu_reg_multi_bdd)
summary(chu_reg_multi)
chu_reg_multi
hist(resid(chu_reg_multi), col="grey", main="")

# exclure les paramètres 

# [6] Conclusion

# R² = 97% donc le modèle est précis
# Nous avons 97% de la variance du taux de réussite qui peut être expliquée par les variations de ... 

#############################################################################



