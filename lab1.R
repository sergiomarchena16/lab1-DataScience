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
install.packages("factoextra")


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
library(NbClust) #Para determinar el n?mero de clusters optimo
library(factoextra) #Para hacer gr?ficos bonitos de clustering
library(arules)

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
#Utilizamos la función complete.cases ya que existen demasiados valores NA en nums
nums_completo <- nums[complete.cases(nums),]

#TEST ESFERICIDAD DE BARLETT
nums.NoNa <- na.omit(nums) 
cortest.bartlett(nums.NoNa)
#$chisq
#[1] 98752

#$p.value
#[1] 0

#$df
#[1] 703

KMO(nums.NoNa)
bartlett.test(nums.NoNa)
#Bartlett's K-squared = 531000, df = 37, p-value <2e-16
#PCA
values<- nums.NoNa[,c(7,8,13,14,17,27)]
View(values)
#values2<- datos[,c(5,7,8,13,14,17,20,24,27,28)]
#values2<-datos %>% select(19,20,21,39,44, 47, 50, 55, 62,63)
#View(values2)
compPrin <- prcomp(values, scale=T)
compPrin
autoplot(compPrin)
autoplot(compPrin, data = nums_completo,loadings = TRUE, loadings.colour = 'green',loadings.label = TRUE, loadings.label.size = 3)

#ESTAS COMPONENTES EXPLICAN APROXIMADAMENTE EL 73% DE LA VARIABILIDAD DE LOS DATOS

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
reglas<-apriori(cats, parameter = list(support = 0.90,
                                        confidence = 0.90,
                                        maxlen = 5,
                                        maxtime = 3,
                                        minlen = 2,
                                        target = "rules"))

# 7. Obtenga reglas de asociación más interesantes del dataset. Discuta sobre el nivel de confianza y soporte


#--- codigo extraído de documentación ?is.redundant
## for better comparison we sort the rules by confidence and add Bayado's improvement
reglas <- sort(reglas, by = "confidence")
quality(reglas)$improvement <- interestMeasure(reglas, measure = "improvement")
inspect(reglas)

## non-redundant rules
inspect(reglas[!is.redundant(reglas)])

# 8. Haga un resumen de los hallazgos más importantes encontrados al explorar los datos y llegue a conclusiones sobre las posibles líneas de investigación.

cor(nums_completo, nums_completo$SalePrice)

# Ver PDF