## Variogramme ominidirectionnel - 
## Ici, le variogramme est calcul? ? partir des valeurs de l'?paisseur avec le fichier BDdepot2 (valeur mesur?es) mais le krigeage se fait avec le fichier BDdepot1 (correspond ? BDdepot2 avec ajout de donn?es hypoth?tiques, non mesur?es)
## Script adapt? ? partir des aides sur les library gstat, geoR et sp
## Script ?crit en mars 2017 - Am?lie Laurent-Dehecq

#1 - Ouvrir les donn?es (A faire automatiquement depuis FILE/Import Dataset ou taper ci-dessous) + ATTENtion NE PAS UTILISER Excel mais export csv avec d?limiteurs ";" ou semicolon et les d?cimales avec des points
library(readr)
BDdepot2 <- read_delim("BDdepot2.csv", ";", escape_double = FALSE, trim_ws = TRUE)
View(BDdepot2)

#2 - V?rification des donn?es (nb de donn?es, nb colonnes / noms des champs / r?sum? pour chaques variables)

library(gstat)
dim(BDdepot2)
names(BDdepot2)
summary(BDdepot2)

library(geoR)
geoBDdepot2 <- as.geodata(BDdepot2,coords.col=2:3,data.col=6)
plot.geodata(geoBDdepot2)

#4 - Isotropie ou anisotropie ? = spatialisation de la variance. Il faut g?n?rer coordinates sur le DATA.frame sinon ne peut pas marcher

##4-1 - Pr?paration des donn?es
library(sp)
head(BDdepot2)    #cr?er un identifiant unique devant tableau de donn?es
class(BDdepot2)   #> il faut obtenir :
                  #[1] "data.frame"
coordinates(BDdepot2) <- ~X+Y #D?finir les coordonn?es x et y en utlisant les nom des champs
class(BDdepot2)   #> il faut obtenir :
                      #[1] "SpatialPointsDataFrame"
                      #attr(,"package")
                      #[1] "sp"
summary(BDdepot2)
plot(BDdepot2)    #> cartographie des points (permet de v?rfier la carte)


##4-2a - Cartographie de la semi-variance de "Epaisseur" (cutoff = longueur max / witdh = taille de la cellule ou distance de voisinage/ thresold = seuil (nb paires de point?) -> il faut tenter plusieurs mesures pour visulaiser les variations)
## "~1" veut dire qu'il n'y a une constance dans l'espace, on pourrait remplacer par un autre param?tre tel que le relief, voir l'aide sur ces fonctions
vario.map.Epaisseur <- gstat::variogram(Epaisseur~1, BDdepot2, cutoff = 1200, width = 100, map = TRUE)
plot(vario.map.Epaisseur , threshold = 5)
            ## Cela montre d?pendance directionnelle = anisotropie = en th?orie, faire variogramme directionnel par la suite (cf autre script)


# 5 - Tracer les variogrammes omnidirectionnel

## 5a-1 - Variogramme ominidirectionnel avec une tendance constante par d?faut, longueur de la diagonal divis? par 3 (fonction variogram)
vario.Epaisseur <- gstat::variogram(Epaisseur~1,BDdepot2)
vario.Epaisseur #d?clarer la variable = cr?er un type "gstat.variogram"
plot(vario.Epaisseur, type ="o", main = "Epaisseur - Variogamme exp?rimental par d?faut", plot.numbers = TRUE)
View(vario.Epaisseur)
#### il faudrait ?tiqueter le nombres de paires de points (np) visibles dans le tableau "vario.Epaisseur"
#### dist = distance moyenne pour les paires de points pris en compte dans l'estimation
#### gamma = semi-variance estim?e

## 5a-2 - Variogramme ominidirectionnel exp?rimental avec une tendance constante en imposant distance limite (cutoff) et le pas (width)

vario.Epaisseur <- gstat::variogram(Epaisseur~1, BDdepot2, cutoff = 1200, width = 100, cloud = FALSE)
vario.Epaisseur #d?clarer la variable = cr?er un type "gstat.variogram"
plot(vario.Epaisseur, type ="o", main = "Epaisseur - Variogamme exp?rimental", plot.numbers = TRUE) #type "o" = points calcul?s sont trac?s et reli?s
View(vario.Epaisseur)

##exporter les valeurs du variogramme de Epaisseur
write.table(vario.Epaisseur, file = "outputs/vario_Epaisseur.txt",sep=";")

# 5b - Ajustement d'un mod?le (fit) de variogramme (fonction vgm) / Ici, psill et range sont donn?s ? tritre indicatif 
# si valeur de nugget non-mentionn?e, l'ajustement ne propose par d'effets de p?pite
# si fit.sills = TRUE => nugget et psill sont ajust?s automatiquement / si fit.sills = FALSE => nugget et psill sont ajust?s selon mon mod?le
# si fit.range = TRUE => range est ajust? automatiquement / # si fit.range = FALSE => range est ajust? manuellement

vario.Epaisseur.fit <- fit.variogram (vario.Epaisseur , vgm(psill = 3, model = "Exp", range = 200, nugget = 0.5), fit.sills = TRUE, fit.range = TRUE)
vario.Epaisseur.fit
plot(vario.Epaisseur,vario.Epaisseur.fit, type ="o", main = "Epaisseur - Variogamme ajust?")


# 6 - Krigeage
## Pr?paration des donn?es de BDdepot1
###  Ouvrir les donn?es de BDdepot1 (A faire automatiquement depuis FILE/Import Dataset ou taper ci-dessous) + ATTENtion NE PAS UTILISER Excel mais export csv avec d?limiteurs ";" ou semicolon et les d?cimales avec des points
BDdepot1 <- read_delim("BDdepot1.csv", ";", escape_double = FALSE, trim_ws = TRUE)
View(BDdepot1)

### V?rification des donn?es de BDdepot1 (nb de donn?es, nb colonnes / noms des champs / r?sum? pour chaques variables)

dim(BDdepot1)
names(BDdepot1)
summary(BDdepot1)


geoBDdepot1 <- as.geodata(BDdepot1,coords.col=2:3,data.col=6)
plot.geodata(geoBDdepot1)

### pr?paration carto de BDdepot1
head(BDdepot1)    #cr?er un identifiant unique devant tableau de donn?es
class(BDdepot1)   #> il faut obtenir :
#[1] "data.frame"
coordinates(BDdepot1) <- ~X+Y #D?finir les coordonn?es x et y en utlisant les nom des champs
class(BDdepot1)   #> il faut obtenir :
#[1] "SpatialPointsDataFrame"
#attr(,"package")
#[1] "sp"
summary(BDdepot1)
plot(BDdepot1)    #> cartographie des points (permet de v?rfier la carte)

## Cr?er un grid vide (limite de la fen?tre) = attention ? la taille de la cellule en sortie, demande qq minutes pour kriger!
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

##Faire le krigeage de BDdepot1 ? l'aide du mod?le d?fini dans BDdepot2 dans le grid

Epaisseur.kriged <- krige(Epaisseur~1, BDdepot1, Grid, model = vario.Epaisseur.fit)

###Affiche les valeurs estim?es

spplot(Epaisseur.kriged ["var1.pred"], main = "Estimation Epaisseur - krigeage ordinaire")

###Affiche la variance calcul?e

spplot(Epaisseur.kriged ["var1.var"], main = "ordinary kriging variance")

### exporter les r?sultats du krigeage de Epaisseur

library(rgdal) 
writeGDAL(Epaisseur.kriged, "outputs/predict_Epaisseur.tiff", drivername="GTiff")

# 7 - validation crois?e = pour valider le mod?le

Epaisseur.cv = krige.cv(Epaisseur~1, BDdepot1, model = vario.Epaisseur.fit)


### d?finition des titres d'axes
Epaisseur_mesure <- Epaisseur.cv$observed
Epaisseur_estime <- Epaisseur.cv$var1.pred

### tracer le graphique
plot(Epaisseur_mesure, Epaisseur_estime, main = "validation crois?e" )

###exporter les valeurs de la validation crois?e de Epaisseur

write.table (Epaisseur.cv, file = "outputs/validation_croisee_Epaisseur.txt",sep=";") 

### calcul coefficient de corr?lation entre 2 variables
## on teste H0 = 0 vaec un alpha de 0.05. Si p-value < 0.05 alors Ho rejet?e donc corefficient de corr?lation repr?sentatif
cor(Epaisseur_mesure, Epaisseur_estime, method = c("pearson"))
cor.test(Epaisseur_mesure, Epaisseur_estime, method=c("pearson"))

# 8 - Si besoin de tester la m?thode d'interpolation inverse ? la distance : IDW
Epaisseur.idw <- idw(Epaisseur~1, BDdepot2, Grid)
class(Epaisseur.idw)
spplot(Epaisseur.idw ["var1.pred"], main = "Estimation - IDW")



