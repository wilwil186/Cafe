library(ggplot2)
# Leer los datos
setwd("~/Documentos/Cafe") # Establecer ruta de trabajo
datosHist <- read.csv('datosHist.csv')
datosHist$Fecha <- as.Date(datosHist$Fecha)

KCN4 <- ts(datosHist$precio_promedio,freq=12)
plot(datosHist$Fecha, KCN4, type = "l", xlab = "Fecha", 
     ylab = "Precio Promedio", main = "Precios del Café")

plot(diff(KCN4)) # calculo de la variación

plot(log(KCN4)) #  útil para estabilizar la varianza y transformar los datos en una escala logarítmica.

plot(diff(log(KCN4))) 
abline(h=0)


descomposicion <- decompose(KCN4, type = "additive")

plot(descomposicion)

