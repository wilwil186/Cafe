library(ggplot2)
# Leer los datos
### Datos ###
setwd("~/Documentos/Cafe") # Establecer ruta de trabajo
datosHist <- read.csv('datosHist.csv')
datosHist$Fecha <- as.Date(datosHist$Fecha)
### fin Datos ###

### Serie de timepo ###
KCN4 <- ts(datosHist$precio_promedio,freq=12)
plot(datosHist$Fecha, KCN4, type = "l", xlab = "Fecha", 
     ylab = "Precio Promedio", main = "Precios del Café")

datosHist_2000 <- subset(datosHist, Fecha >= as.Date("2000-01-01"))
KCN4_2000 <- ts(datosHist_2000$precio_promedio,freq=12)
plot(datosHist_2000$Fecha, KCN4_2000, type = "l", xlab = "Fecha", 
     ylab = "Precio Promedio", main = "Precios del Café")
### fin Series de tiempo ####

### Cálculo de la variación de las series de tiempo ###

par(mfrow = c(2, 1))
#plot(diff(KCN4)) # calculo de la variación
#plot(log(KCN4)) #  útil para estabilizar la varianza y transformar los datos en una escala logarítmica.
plot(diff(log(KCN4))) 
abline(h=0)

#plot(diff(KCN4_2000)) # calculo de la variación
#plot(log(KCN4_2000)) #  útil para estabilizar la varianza y transformar los datos en una escala logarítmica.
plot(diff(log(KCN4_2000))) 
abline(h=0)
### fin Cálculo de la variación de las series de tiempo ###

## Descomposición de la serie de tiempo ###
decomposition <- decompose(KCN4, type = "additive")
par(mfrow = c(4, 1))

plot(decomposition$x, ylab = "Serie de Tiempo Original", xlab = "")
plot(decomposition$trend, ylab = "Tendencia", xlab = "")
plot(decomposition$seasonal, ylab = "Estacionalidad", xlab = "")
plot(decomposition$random, ylab = "Residuales", xlab = "")

decomposition_2000 <- decompose(KCN4_2000, type = "additive")
par(mfrow = c(4, 1))

plot(decomposition_2000$x, ylab = "Serie de Tiempo Original", xlab = "")
plot(decomposition_2000$trend, ylab = "Tendencia", xlab = "")
plot(decomposition_2000$seasonal, ylab = "Estacionalidad", xlab = "")
plot(decomposition_2000$random, ylab = "Residuales", xlab = "")

par(mfrow = c(1, 1)) # normal
plot(decomposition_2000$x, ylab = "Serie de Tiempo Original", xlab = "")
abline(lm(decomposition_2000$x ~ time(decomposition_2000$x)), col = "red")

## fin Descomposición de la serie de tiempo ###

KCN4_2000 <- log(KCN4_2000) # Estabilización de la varianza
#PASA TODO A ESCALA LOGARITMICA
plot(datosHist_2000$Fecha, KCN4_2000, type = "l", xlab = "Fecha", 
     ylab = "Precio Promedio", main = "Precios del Café")

### Estacionariedad ### 

install.packages("urca")
library(urca)

adf_test <- ur.df(KCN4_2000, type = "trend", lags = 1)
summary(adf_test)

### AUTOCORRELACIÓN ### 

autocorrelacion<-acf(KCN4_2000, type ="correlation", plot = FALSE)
autocorrelacion
plot(autocorrelacion)









