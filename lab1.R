# LABORATORIO 1 DE DATA SCIENCE 
# PABLO VIANA 16091
# SERGIO MARCHENA 16387

# Paquetes
install.packages("rela")
install.packages("psych")
install.packages("FactoMineR")
install.packages("ggfortify")
install.packages("corrplot")
install.packages("arules")
install.packages("Hmisc")

# Librerias
library(corrplot)
library(ggfortify)
library(rela)
library(psych)
library(FactoMineR)
library(fpc)
library(arules)
library(ggplot2)
library(caret)
library("ggpubr")
library(cluster)
library(dplyr)
library(plyr)
library(Hmisc)
library(cluster) #Para calcular la silueta
library(e1071)#para cmeans
library(mclust) #mixtures of gaussians
library(fpc) #para hacer el plotcluster
library(NbClust) #Para determinar el n?mero de clusters ?ptimo
library(factoextra) #Para hacer gr?ficos bonitos de clustering

# Datos
datos <- read.csv("train.csv")
datos <- as.data.frame(datos)
# 1. Haga una exploración rápida de sus datos para eso haga un resumen de su dataset
View(datos)
summary(datos)
str(datos)

# 2. Diga el tipo de cada una de las variables del dataset (cualitativa o categórica, cuantitativa continua, cuantitativa discreta)
# VER PDF
for(i in datos){
  str(i)
  summary(i)
}

# 3. Aísle las variables numéricas de las categóricas, haga un análisis de correlación entre las mismas.
nums<-dplyr::select_if(datos, is.numeric)   #NUMERICAS
cats<-dplyr::select_if(datos,is.factor)     #CATEGORICAS
View(nums)
nums.cor <- cor(nums)
corrplot(nums.cor)

# 4. Utilice las variables categóricas, haga tablas de frecuencia, proporción, gráficas de barras o cualquier otra técnica que le permita explorar los datos.

#Todas las tablas de frecuencia de las variables categoricas
nombres <- colnames(cats)
cont <- 1
for(i in cats){
  print(nombres[cont])
  print(table(i))
  cont <- cont + 1
}

barplot(table(cats$MSZoning), main="Clasificación de la zona general", beside=TRUE, col="blue", border = TRUE)
barplot(table(cats$Street), main="Tipo de calle de acceso", beside=TRUE, col="blue", border = TRUE)
barplot(table(cats$Alley), main="Tipo de callejón para accesar la propiedad",beside=TRUE, col="yellow", border = TRUE)
barplot(table(cats$LotShape), main="Forma general de la propiedad",beside=TRUE, col="yellow", border = TRUE)
barplot(table(cats$Utilities), main="Utilidades de la casa", beside=TRUE, col="green", border = TRUE)
barplot(table(cats$LotConfig), main="Configuración del lote", beside=TRUE, col="green", border = TRUE)

# 5. Haga un análisis de componentes principales, interprete los componentes
nums[is.na(nums)] <- 0
compPrinc
summary(compPrinc)
autoplot(compPrinc)
compPrincPCA<-PCA(nums,ncp=ncol(nums), scale.unit = T)
summary(compPrincPCA)


# 6. Haga un análisis de clustering, describa los grupos.

#Utilizamos la función complete.cases ya que existen demasiados valores NA en nums
nums_completo <- nums[complete.cases(nums),]

#Utilización de nbClust para encontrar número óptimo de clusters
nb <- NbClust(nums_completo, distance = "euclidean", min.nc = 2,
              max.nc = 10, method = "complete", index ="all")

#K-medias
km<-kmeans(nums_completo,10)
nums_completo$grupo<-km$cluster
plotcluster(nums_completo,km$cluster) #grafica la ubicación de los clusters
fviz_cluster(km, data = nums_completo,geom = "point", ellipse.type = "norm")

#Fuzzy C-Means
fcm<-cmeans(nums_completo,10)
nums_completo$FCGrupos<-fcm$cluster
nums_completo<-cbind(nums_completo,fcm$membership)

#Metodos de la silueta
#Método de la silueta para las k-medias
silkm<-silhouette(km$cluster,dist(nums_completo))
mean(silkm[,3]) #0.48 

#Método de la silueta para fuzzy cmeans
silfcm<-silhouette(fcm$cluster,dist(nums_completo))
mean(silfcm[,3]) #0.477


# 7. Obtenga reglas de asociación más interesantes del dataset. Discuta sobre el nivel de confianza y soporte.


# 8. Haga un resumen de los hallazgos más importantes encontrados al explorar los datos y llegue a conclusiones sobre las posibles líneas de investigación.
