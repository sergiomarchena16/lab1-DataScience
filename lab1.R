# LABORATORIO 1 DE DATA SCIENCE 
# PABLO VIANA 16091
# SERGIO MARCHENA 16387

# Paquetes
install.packages("rela")
install.packages("psych")
install.packages("FactoMineR")
install.packages("ggfortify")
install.packages("corrplot")
# Librerias
library(corrplot)
library(ggfortify)
library(rela)
library(psych)
library(FactoMineR)
# Datos
datos <- read.csv("train.csv")
View(datos)
# 1. Haga una exploración rápida de sus datos para eso haga un resumen de su dataset
View(datos)
summary(datos)
str(datos)

# 2. Diga el tipo de cada una de las variables del dataset (cualitativa o categórica, cuantitativa continua, cuantitativa discreta)
# VER PDF
str(datos$Id)
str(datos$MSSubClass)
str(datos$MSZoning)
str(datos$LotFrontage)
str(datos$LotArea)
str(datos$Street) 
summary(datos$Street) 
str(datos$Alley)
summary(datos$Alley)
str(datos$LotShape)
summary(datos$LotShape)
str(datos$LandContour)
summary(datos$LandContour)
str(datos$Utilities)
summary(datos$Utilities)
str(datos$LotConfig)
summary(datos$LotConfig)
str(datos$LandSlope)
summary(datos$LandSlope)
str(datos$Neighborhood)
summary(datos$Neighborhood)
str(datos$Condition1)
summary(datos$Condition1)
str(datos$Condition2)
summary(datos$Condition2)
str(datos$BldgType)
summary(datos$BldgType)
str(datos$HouseStyle)
summary(datos$HouseStyle)
str(datos$OverallQual)
str(datos$OverallCond)
str(datos$YearBuilt)
str(datos$YearRemodAdd)
str(datos$RoofStyle)
summary(datos$RoofStyle)
str(datos$RoofMatl)
summary(datos$RoofMatl)
str(datos$Exterior1st)
summary(datos$Exterior1st)
str(datos$Exterior2nd)
summary(datos$Exterior2nd)
str(datos$MasVnrType)
summary(datos$MasVnrType)
str(datos$MasVnrArea)
str(datos$ExterQual)
summary(datos$ExterQual)



# 3. Aísle las variables numéricas de las categóricas, haga un análisis de correlación entre las mismas.

nums<-dplyr::select_if(datos, is.numeric)   #NUMERICAS
cats<-dplyr::select_if(datos,is.factor)     #CATEGORICAS
View(nums)
nums.cor <- cor(nums)
corrplot(nums.cor)

# 4. Utilice las variables categóricas, haga tablas de frecuencia, proporción, gráficas de barras o cualquier otra técnica que le permita explorar los datos.
# 5. Haga un análisis de componentes principales, interprete los componentes

nums[is.na(nums)] <- 0
compPrinc<-prcomp(nums, scale = T)
compPrinc
summary(compPrinc)
autoplot(compPrinc)
compPrincPCA<-PCA(nums,ncp=ncol(nums), scale.unit = T)

# 6. Haga un análisis de clustering, describa los grupos.


# 7. Obtenga reglas de asociación más interesantes del dataset. Discuta sobre el nivel de confianza y soporte.
# 8. Haga un resumen de los hallazgos más importantes encontrados al explorar los datos y llegue a conclusiones sobre las posibles líneas de investigación.
