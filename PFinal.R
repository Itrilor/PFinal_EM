install.packages("psych")
install.packages("polycor")
install.packages("ggcorrplot")
install.packages("corrr")
install.packages("moments") #skewness

library(psych)
library(polycor)
library(ggcorrplot)
library(corrplot)
library(corrr)
library(moments)

# -------------------------------------- #
# PASO 0: carga de unos datos de interés #
# -------------------------------------- #
getwd()

library(foreign)
datos_original<-read.spss("DB_3.sav", to.data.frame = TRUE)
#Quitamos la columna de las 
datos<-datos_original[,-1]


# -------------------------------------- #
# PASO 1: carga de unos datos de interés #
# -------------------------------------- #
not_available<-function(data,na.rm=F){
  data[is.na(data)]<-mean(data,na.rm=T)
  data
}
per_not_available<-function(data, na.rm=F){
  colMeans(is.na(data))*100
}
per_not_available(datos)
datos$ZTLIBROP<-not_available(datos$ZTLIBROP)

cor(datos)
corrplot(cor(datos), order = "hclust", tl.col='black', tl.cex=1)

summary(datos)
skewness(datos) #https://www.programmingr.com/statistics/skewness/ 
hist(x = datos$ZPOBDENS) #Si media a la derecha de mediana -> skewness positivo
kurtosis(x = datos)

boxplot(datos,main="Análisis exploratorio de datos",
        xlab="Variables sociodemográficas",
        ylab="z-values",
        col=c(1:11))
# ZPOBDENS, ZTENERGI, ZTEJERCI

outlier<-function(data,na.rm=T){
  H<-1.5*IQR(data)
  data[data<quantile(data,0.25,na.rm = T)-H]<-NA
  data[data>quantile(data,0.75, na.rm = T)+H]<-NA
  data[is.na(data)]<-mean(data, na.rm = T)
  H<-1.5*IQR(data)
  if (TRUE %in% (data<quantile(data,0.25,na.rm = T)-H) | TRUE %in% (data>quantile(data,0.75,na.rm = T)+H))
    outlier(data)
  else
    return(data)
}
datos$ZPOBDENS <- outlier(datos$ZPOBDENS)
datos$ZTENERGI <- outlier(datos$ZTENERGI)
datos$ZTEJERCI <- outlier(datos$ZTEJERCI)
boxplot(datos,main="Análisis exploratorio de datos",
        xlab="Variables sociodemográficas",
        ylab="z-values",
        col=c(1:11))


#Normalidad con gráficas qqplot
qqnorm(datos$ZPOBDENS, main="ZPOBDENS", pch=1, frame = FALSE)
qqline(datos$ZPOBDENS, col = 'blue', lwd=2)


#Test de Saphiro-Wilk
install.packages("dplyr")
library(reshape2)
library(knitr)
library(dplyr)

datos_tidy <- melt(datos, value.name = "valor")
aggregate(valor ~ variable, data = datos_tidy,
          FUN = function(x){shapiro.test(x)$p.value})

#x>0.05 -> ZPOBURB, ZPSERVI, ZTEJERCI, ZTPOBACT 
qqnorm(datos$ZPOBURB, pch=1, frame = FALSE)
qqline(datos$ZPOBURB, col = 'blue', lwd=2)
