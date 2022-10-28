## Variogramme ominidirectionnel - 
## Ici, le variogramme est calculé à partir des valeurs de l'épaisseur avec le fichier BDdepot2 (valeur mesurées) mais le krigeage se fait avec le fichier BDdepot1 (correspond à BDdepot2 avec ajout de données hypothétiques, non mesurées)
## Script adapté à partir des aides sur les library gstat, geoR et sp
## Script écrit en mars 2017 - Amélie Laurent-Dehecq

#1 - Ouvrir les données (A faire automatiquement depuis FILE/Import Dataset ou taper ci-dessous) + ATTENtion NE PAS UTILISER Excel mais export csv avec délimiteurs ";" ou semicolon et les décimales avec des points
library(readr)
BDdepot2 <- read_delim("BDdepot2.csv", ";", escape_double = FALSE, trim_ws = TRUE)
View(BDdepot2)

#2 - Vérification des données (nb de données, nb colonnes / noms des champs / résumé pour chaques variables)

library(gstat)
dim(BDdepot2)
names(BDdepot2)
summary(BDdepot2)

library(geoR)
geoBDdepot2 <- as.geodata(BDdepot2,coords.col=2:3,data.col=6)
plot.geodata(geoBDdepot2)

#4 - Isotropie ou anisotropie ? = spatialisation de la variance. Il faut générer coordinates sur le DATA.frame sinon ne peut pas marcher

##4-1 - Préparation des données
library(sp)
head(BDdepot2)    #créer un identifiant unique devant tableau de données
class(BDdepot2)   #> il faut obtenir :
                  #[1] "data.frame"
coordinates(BDdepot2) <- ~X+Y #Définir les coordonnées x et y en utlisant les nom des champs
class(BDdepot2)   #> il faut obtenir :
                      #[1] "SpatialPointsDataFrame"
                      #attr(,"package")
                      #[1] "sp"
summary(BDdepot2)
plot(BDdepot2)    #> cartographie des points (permet de vérfier la carte)


##4-2a - Cartographie de la semi-variance de "Epaisseur" (cutoff = longueur max / witdh = taille de la cellule ou distance de voisinage/ thresold = seuil (nb paires de pointé) -> il faut tenter plusieurs mesures pour visulaiser les variations)
## "~1" veut dire qu'il n'y a une constance dans l'espace, on pourrait remplacer par un autre paramètre tel que le relief, voir l'aide sur ces fonctions
vario.map.Epaisseur <- gstat::variogram(Epaisseur~1, BDdepot2, cutoff = 1200, width = 100, map = TRUE)
plot(vario.map.Epaisseur , threshold = 5)
      ## Cela montre dépendance directionnelle = anisotropie = en théorie, faire variogramme directionnel par la suite (cf autre script)


# 5 - Tracer les variogrammes omnidirectionnel

## 5a-1 - Variogramme ominidirectionnel avec une tendance constante par défaut, longueur de la diagonal divisé par 3 (fonction variogram)
vario.Epaisseur <- gstat::variogram(Epaisseur~1,BDdepot2)
vario.Epaisseur #déclarer la variable = créer un type "gstat.variogram"
plot(vario.Epaisseur, type ="o", main = "Epaisseur - Variogamme expérimental par défaut", plot.numbers = TRUE)
View(vario.Epaisseur)
#### il faudrait étiqueter le nombres de paires de points (np) visibles dans le tableau "vario.Epaisseur"
#### dist = distance moyenne pour les paires de points pris en compte dans l'estimation
#### gamma = semi-variance estimée

## 5a-2 - Variogramme ominidirectionnel expérimental avec une tendance constante en imposant distance limite (cutoff) et le pas (width)

vario.Epaisseur <- gstat::variogram(Epaisseur~1, BDdepot2, cutoff = 1200, width = 100, cloud = FALSE)
vario.Epaisseur #déclarer la variable = créer un type "gstat.variogram"
plot(vario.Epaisseur, type ="o", main = "Epaisseur - Variogamme expérimental", plot.numbers = TRUE) #type "o" = points calculés sont tracés et reliés
View(vario.Epaisseur)

##exporter les valeurs du variogramme de Epaisseur
write.table(vario.Epaisseur, file = "outputs/vario_Epaisseur.txt",sep=";")

# 5b - Ajustement d'un modéle (fit) de variogramme (fonction vgm) / Ici, psill et range sont donnés à tritre indicatif 
# si valeur de nugget non-mentionnée, l'ajustement ne propose par d'effets de pépite
# si fit.sills = TRUE => nugget et psill sont ajustés automatiquement / si fit.sills = FALSE => nugget et psill sont ajustés selon mon modèle
# si fit.range = TRUE => range est ajusté automatiquement / # si fit.range = FALSE => range est ajusté manuellement

vario.Epaisseur.fit <- fit.variogram(vario.Epaisseur , vgm(psill = 3, model = "Exp", range = 200, nugget = 0.5), fit.sills = TRUE, fit.range = TRUE)
vario.Epaisseur.fit
plot(vario.Epaisseur,vario.Epaisseur.fit, type ="o", main = "Epaisseur - Variogamme ajusté")


# 6 - Krigeage
## Préparation des données de BDdepot1
###  Ouvrir les données de BDdepot1 (A faire automatiquement depuis FILE/Import Dataset ou taper ci-dessous) + ATTENtion NE PAS UTILISER Excel mais export csv avec délimiteurs ";" ou semicolon et les décimales avec des points
BDdepot1 <- read_delim("BDdepot1.csv", ";", escape_double = FALSE, trim_ws = TRUE)
View(BDdepot1)

### Vérification des données de BDdepot1 (nb de données, nb colonnes / noms des champs / résumé pour chaques variables)

dim(BDdepot1)
names(BDdepot1)
summary(BDdepot1)


geoBDdepot1 <- as.geodata(BDdepot1,coords.col=2:3,data.col=6)
plot.geodata(geoBDdepot1)

### préparation carto de BDdepot1
head(BDdepot1)    #créer un identifiant unique devant tableau de données
class(BDdepot1)   #> il faut obtenir :
#[1] "data.frame"
coordinates(BDdepot1) <- ~X+Y #Définir les coordonnées x et y en utlisant les nom des champs
class(BDdepot1)   #> il faut obtenir :
#[1] "SpatialPointsDataFrame"
#attr(,"package")
#[1] "sp"
summary(BDdepot1)
plot(BDdepot1)    #> cartographie des points (permet de vérfier la carte)

## Créer un grid vide (limite de la fenêtre) = attention à la taille de la cellule en sortie, demande qq minutes pour kriger!
ouest<-474242.855888
est<-476935.196567
nord<-267752.513007
sud<-266551.203890
grx<- seq(ouest,est,by=1)
gry<- seq(sud,nord,by=1)
x<-rep(grx, length(gry))
y<-rep(gry, length(grx))
y<-sort(y, decreasing=F)
Grid<-data.frame(x=x, y=y)

coordinates(Grid) <- ~x+y
gridded(Grid)<-TRUE

class(Grid)

##Faire le krigeage de BDdepot1 à l'aide du modèle défini dans BDdepot2 dans le grid

Epaisseur.kriged <- krige(Epaisseur~1, BDdepot1, Grid, model = vario.Epaisseur.fit)

###Affiche les valeurs estimées

spplot(Epaisseur.kriged ["var1.pred"], main = "Estimation Epaisseur - krigeage ordinaire")

###Affiche la variance calculée

spplot(Epaisseur.kriged ["var1.var"], main = "ordinary kriging variance")

### exporter les résultats du krigeage de Epaisseur

library(rgdal) 
writeGDAL(Epaisseur.kriged, "outputs/predict_Epaisseur.tiff", drivername="GTiff")

# 7 - validation croisée = pour valider le modèle

Epaisseur.cv = krige.cv(Epaisseur~1, BDdepot1, model = vario.Epaisseur.fit)


### définition des titres d'axes
Epaisseur_mesure <- Epaisseur.cv$observed
Epaisseur_estime <- Epaisseur.cv$var1.pred

### tracer le graphique
plot(Epaisseur_mesure, Epaisseur_estime, main = "validation crois?e" )

###exporter les valeurs de la validation croisée de Epaisseur

write.table (Epaisseur.cv, file = "outputs/validation_croisee_Epaisseur.txt",sep=";") 

### calcul coefficient de corr?lation entre 2 variables
## on teste H0 = 0 vaec un alpha de 0.05. Si p-value < 0.05 alors Ho rejet?e donc corefficient de corr?lation repr?sentatif
cor(Epaisseur_mesure, Epaisseur_estime, method = c("pearson"))
cor.test(Epaisseur_mesure, Epaisseur_estime, method=c("pearson"))

# 8 - Si besoin de tester la m?thode d'interpolation inverse ? la distance : IDW
Epaisseur.idw <- idw(Epaisseur~1, BDdepot2, Grid)
class(Epaisseur.idw)
spplot(Epaisseur.idw ["var1.pred"], main = "Estimation - IDW")



