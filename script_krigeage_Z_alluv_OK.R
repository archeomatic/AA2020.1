## Variogramme Z_alluv

#1 - Ouvrir les donn�es (A faire automatiquement depuis FILE/Import Dataset ou taper ci-dessous) + ATTENtion NE PAS UTILISER Excel mais export csv avec d�limiteurs ";" ou semicolon et les d�cimales avec des points
library(readr)
BDdepot2 <- read_delim("C:/Users/Am�lie/Desktop/Formation_ISA/R/BDdepot2.csv", ";", escape_double = FALSE, trim_ws = TRUE)
View(BDdepot2)

#2 - V�rification des donn�es (nb de donn�es, nb colonnes / noms des champs / r�sum� pour chaques variables)

library(gstat)
dim(BDdepot2)
names(BDdepot2)
summary(BDdepot2)

library(geoR)
geoBDdepot2<-as.geodata(BDdepot2,coords.col=2:3,data.col=6)
plot.geodata(geoBDdepot2)

#4 - Isotropie ou anisotropie = spatialisation de la variance. Il faut g�n�rer coordinates sur le DATA.frame sinon ne peut pas marcher

##4-1 - Pr�paration des donn�es
library(gstat)
library(sp)
head(BDdepot2)    #cr�er un identifiant unique devant tableau de donn�es
class(BDdepot2)   #> il faut obtenir :
                  #[1] "data.frame"
coordinates(BDdepot2)=~X+Y #D�finir les coordonn�es x et y en utlisant les nom des champs
class(BDdepot2)   #> il faut obtenir :
                      #[1] "SpatialPointsDataFrame"
                      #attr(,"package")
                      #[1] "sp"
summary(BDdepot2)
plot(BDdepot2)    #> cartographie des points (permet de v�rfier la carte)


##4-2a - Cartographie de la semi-variance du Z_alluv (cutoff = longueur max / witdh = taille de la cellule ou distance de voisinage/ thresold = seuil (nb paires de point�) -> il faut tenter plusieurs mesures pour visulaiser les variations)
vario.map.alluv = variogram(Z_alluv~1, BDdepot2, cutoff = 1200, width = 100, map = TRUE)
plot(vario.map.alluv , threshold = 5)
            ## Cela ne montre pas de d�pendance directionnelle = anisotropie = faire variogramme omnidirectionnel


# 5 - Tracer les variogrammes

## 5a-1 - Variogramme ominidirectionnel avec une tendance constante par d�faut longueur de la diagonal divis� par 3 (fonction variogram)
vario.Z_alluv = variogram(Z_alluv~1,BDdepot2)
plot (vario.Z_alluv)
View (vario.Z_alluv)
#### il faudrait �tiqueter le nombres de paires de points (np) visibles dans le tableau "vario.Z_alluv"
#### dist = distance moyenne pour les paires de points pris en compte dans l'estimation
#### gamma = semi-variance estim�e

## 5a-2 - Variogramme ominidirectionnel exp�rimental avec une tendance constante en imposant distance limite (cutoff) et le pas (width)

vario.Z_alluv = variogram(Z_alluv~1, BDdepot2, cutoff = 1200, width = 100, cloud = FALSE)
vario.Z_alluv #dd�clarer la variable = cr�er un type "gstat.variogram"
plot (vario.Z_alluv, type ="o", main = "Variogamme exp�rimental") #type "o" = points calcul�s sont trac�s et reli�s
View (vario.Z_alluv)

##exporter les valeurs du variogramme de Z_alluv
write.table(vario.Z_alluv, file = "C:/Users/Am�lie/Desktop/Formation_ISA/export/vario_Z_alluv.txt",sep=";")

# 5b - Ajustement d'un mod�le (fit) de variogramme (fonction vgm) / Ici, psill et range sont donn�s à tritre indicatif

vario.Z_alluv.fit = fit.variogram (vario.Z_alluv , vgm(psill = 1.3, model = "Sph", range = 175, nugget = 0.6))
vario.Z_alluv.fit
plot(vario.Z_alluv,vario.Z_alluv.fit)

# 6 - Krigeage

## Cr�er un grid vide (limite de la fenêtre) = attention à la taille de la cellule en sortie, demande qq minutes pour kriger!
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

##Faire le krigeage dans le grid

Z_alluv.kriged = krige(Z_alluv~1, BDdepot2, Grid, model = vario.Z_alluv.fit)

###Affiche les valeurs estim�es

spplot(Z_alluv.kriged ["var1.pred"], main = "Estimation - krigeage ordinaire")

###Affiche la variance calcul�e

spplot(Z_alluv.kriged ["var1.var"], main = "ordinary kriging variance")

### exporter les r�sultats du krigeage de Z_alluv

library(rgdal) 
writeGDAL(Z_alluv.kriged, "C:/Users/Am�lie/Desktop/Formation_ISA/export/predict_Z_alluv.tiff", drivername="GTiff")

# 7 - validation crois�e = pour valider le modèle

Z.alluv.cv = krige.cv(Z_alluv~1, BDdepot2, model = vario.Z_alluv.fit)


### d�finition des titres d'axes
Z_alluv_mesur� = Z.alluv.cv$observed
Z_alluv_estim� = Z.alluv.cv$var1.pred

### tracer le graphique
plot(Z.alluv_mesur�, Z_alluv_estim�, main = "validation crois�e" )

###exporter les valeurs de la validation crois�e de Z_alluv

write.table (Z.alluv.cv, file = "C:/Users/Am�lie/Desktop/Formation_ISA/export/validation_croisee_Z_alluv.txt",sep=";") 

### calcul corefficient de corr�lation entre 2 variables
cor(Z.alluv_mesur�, Z_alluv_estim�, method = c("pearson"))
cor.test(Z.alluv_mesur�, Z_alluv_estim�, method=c("pearson"))

# 8 - IDW
Z_alluv.idw = idw(Z_alluv~1, BDdepot2, Grid)
class(Z_alluv.idw)
spplot(Z_alluv.idw ["var1.pred"], main = "Estimation - IDW")



