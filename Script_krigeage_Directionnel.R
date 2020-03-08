## Variogramme directionnel 
## Ici, le variogramme est calculé à partir des valeurs de l'épaisseur avec le fichier BDdepot2 (valeur mesurées) mais le krigeage se fait avec le fichier BDdepot1 (correspond à BDdepot2 avec ajout de données hypothétiques, non mesurées)
## Script adapté à partir des aides sur les library gstat, geoR et sp
## Script écrit en mars 2017 - Amélie Laurent-Dehecq

#1 - Ouvrir les données (A faire automatiquement depuis FILE/Import Dataset ou taper ci-dessous) + ATTENtion NE PAS UTILISER Excel mais export csv avec délimiteurs ";" ou semicolon et les décimales avec des points
library(readr)
BDdepot2 <- read_delim("C:/Users/Amélie/Desktop/Formation_ISA/R/BDdepot2.csv", ";", escape_double = FALSE, trim_ws = TRUE)
View(BDdepot2)

#2 - Vérification des données (nb de données, nb colonnes / noms des champs / résumé pour chaques variables)

library(gstat)
dim(BDdepot2)
names(BDdepot2)
summary(BDdepot2)

library(geoR)
geoBDdepot2<-as.geodata(BDdepot2,coords.col=2:3,data.col=6)
plot.geodata(geoBDdepot2)

#4 - Isotropie ou anisotropie = spatialisation de la variance. Il faut générer coordinates sur le DATA.frame sinon ne peut pas marcher

##4-1 - Préparation des données
library(gstat)
library(sp)
head(BDdepot2)    #créer un identifiant unique devant tableau de données
class(BDdepot2)   #> il faut obtenir :
                  #[1] "data.frame"
coordinates(BDdepot2)=~X+Y #Définir les coordonnées x et y en utlisant les nom des champs
class(BDdepot2)   #> il faut obtenir :
                      #[1] "SpatialPointsDataFrame"
                      #attr(,"package")
                      #[1] "sp"
summary(BDdepot2)
plot(BDdepot2)    #> cartographie des points (permet de vérfier la carte)


##4-2a - Cartographie de la semi-variance de "Epaisseur" (cutoff = longueur max / witdh = taille de la cellule ou distance de voisinage/ thresold = seuil (nb paires de pointé) -> il faut tenter plusieurs mesures pour visualiser les variations)
## "~1" veut dire qu'il n'y a une constance dans l'espace, on pourrait remplacer par un autre paramètre tel que le relief, voir l'aide sur ces fonctions
vario.map.Epaisseur = variogram((Epaisseur)~1, BDdepot2, cutoff = 1200, width = 100, map = TRUE)
plot(vario.map.Epaisseur , threshold = 10)
            ## Cela montre dépendance directionnelle = anisotropie = faire variogramme directionnel par la suite


# 5 - Tracer les variogrammes directionnel

## 5-1a - Variogramme directionnel avec log(épaisseur) une tendance constante 
## avec angle (70° et 160°) et angle tolérance 20°(pour avoir assez de paires de points)
##en imposant distance limite (cutoff) et le pas (width)

vario.Epaisseur = variogram((Epaisseur)~1, BDdepot2, cutoff = 1200, width = 100, alpha = c(70, 160), tol.hor = 20, cloud = FALSE)
vario.Epaisseur #déclarer la variable = créer un type "gstat.variogram"
plot (vario.Epaisseur, type ="o", main = "Epaisseur - Variogamme expérimental 70 ° et 160°") #type "o" = points calculés sont tracés et reliés
View (vario.Epaisseur)

## 5-1b exporter les valeurs du variogramme de Epaisseur
write.table(vario.Epaisseur, file = "C:/Users/Amélie/Desktop/Formation_ISA/export/vario_Epaisseur_70_160.txt",sep=";")

## 5- 2 - Ajustement d'un modéle (fit) de variogramme (fonction vgm) / Ici, psill et range sont donnés à titre indicatif 
# si valeur de nugget non-mentionnée, l'ajustement ne propose par d'effets de pépite
# si fit.sills = TRUE => nugget et psill sont ajustés automatiquement / si fit.sills = FALSE => nugget et psill sont ajustés selon mon modèle
# si fit.range = TRUE => range est ajusté automatiquement / # si fit.range = FALSE => range est ajusté manuellement
vario.Epaisseur.fit = fit.variogram (vario.Epaisseur , vgm(psill = 1.9, model = "Exp", range = 325, nugget = 1.1, anis = c(70, 0.66)), fit.sills = FALSE , fit.range = FALSE)
vario.Epaisseur.fit
plot(vario.Epaisseur,vario.Epaisseur.fit, type ="o", main = "Epaisseur_70_160 - Variogamme ajusté")

# 6 - Krigeage
## Préparation des données de BDdepot1
###  Ouvrir les données de BDdepot1 (A faire automatiquement depuis FILE/Import Dataset ou taper ci-dessous) + ATTENtion NE PAS UTILISER Excel mais export csv avec délimiteurs ";" ou semicolon et les décimales avec des points
library(readr)
BDdepot1 <- read_delim("C:/Users/Amélie/Desktop/Formation_ISA/R/BDdepot1.csv", ";", escape_double = FALSE, trim_ws = TRUE)
View(BDdepot1)

### préparation carto de BDdepot1
library(gstat)
library(sp)
head(BDdepot1)    #créer un identifiant unique devant tableau de données
class(BDdepot1)   #> il faut obtenir :
#[1] "data.frame"
coordinates(BDdepot1)=~X+Y #Définir les coordonnées x et y en utlisant les nom des champs
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

coordinates(Grid)=~x+y
gridded(Grid)<-TRUE

class(Grid)

##Faire le krigeage de BDdepot1 à l'aide du modèle défini dans BDdepot2 dans le grid

Epaisseur.kriged = krige((Epaisseur)~1, BDdepot1, Grid, model = vario.Epaisseur.fit)

###Affiche les valeurs estimées

spplot(Epaisseur.kriged ["var1.pred"], main = "Estimation Epaisseur - krigeage ordinaire")

###Affiche la variance calculée

spplot(Epaisseur.kriged ["var1.var"], main = "ordinary kriging variance")

### exporter les résultats du krigeage de Epaisseur

library(rgdal) 
writeGDAL(Epaisseur.kriged, "C:/Users/Amélie/Desktop/Formation_ISA/export/predict_Epaisseur_direct_choisi.tiff", drivername="GTiff")

# 7 - validation croisée = pour valider le modèle

Epaisseur.cv = krige.cv(Epaisseur~1, BDdepot1, model = vario.Epaisseur.fit)


### définition des titres d'axes
Epaisseur_mesuré = Epaisseur.cv$observed
Epaisseur_estimé = Epaisseur.cv$var1.pred

### tracer le graphique
plot(Epaisseur_mesuré, Epaisseur_estimé, main = "validation croisée" )

###exporter les valeurs de la validation croisée de Epaisseur

write.table (Epaisseur.cv, file = "C:/Users/Amélie/Desktop/Formation_ISA/export/validation_croisee_Epaisseur.txt",sep=";") 

### calcul coefficient de corrélation entre 2 variables
## on teste H0 = 0 vaec un alpha de 0.05. Si p-value < 0.05 alors Ho rejetée donc corefficient de corrélation représentatif
cor(Epaisseur_mesuré, Epaisseur_estimé, method = c("pearson"))
cor.test(Epaisseur_mesuré, Epaisseur_estimé, method=c("pearson"))

# 8 - Si besoin de tester la méthode d'interpolation inverse à la distance : IDW
Epaisseur.idw = idw(Epaisseur~1, BDdepot2, Grid)
class(Epaisseur.idw)
spplot(Epaisseur.idw ["var1.pred"], main = "Estimation - IDW")



