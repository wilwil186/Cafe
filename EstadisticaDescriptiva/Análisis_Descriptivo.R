library(ggplot2)

### Datos ###
setwd("~/Documentos/Cafe") # Establecer ruta de trabajo
datosHist <- read.csv('datosHist.csv')
datosHist$Fecha <- as.Date(datosHist$Fecha)
### fin Datos ###

### Series de timepo ###
## Serie de tiempo de todos los datos ##
KCN4 <- ts(datosHist$precio_promedio,freq=12)
plot(datosHist$Fecha, KCN4, type = "l", xlab = "Fecha", 
     ylab = "Precio Promedio", main = "Precios del Café")
## Serie de tiempo de todos los datos ##

# Crear un bucle for para particionar los datos por año
for (i in 1980:2023) {
  start_date <- as.Date(paste(i, "-01-01", sep = ""))
  end_date <- as.Date(paste(i + 1, "-01-01", sep = ""))
  
  datosporAño <- subset(datosHist, Fecha >= start_date & Fecha < end_date)
  KCN4porAño <- ts(datosporAño$precio_promedio, frequency = 12)
  assign(paste0("Datos_Hist_", i), datosporAño)
  assign(paste0("KCN4_", i), KCN4porAño)
}

plot(Datos_Hist_2000$Fecha, KCN4_2000, type = "l", xlab = "Fecha", 
     ylab = "Precio Promedio", main = "Precios del Café")

### fin Series de tiempo ####


### Cálculo de la variación de las series de tiempo ###

#plot(diff(KCN4)) # calculo de la variación
#plot(log(KCN4)) #  útil para estabilizar la varianza y transformar los datos en una escala logarítmica.
datos_diff<- tail(datosHist, -1)
plot(datos_diff$Fecha,diff(log(KCN4)),type="l",
     main = "Variación en cada año", 
     xlab = "Fecha", 
     ylab = "Variación logarítmica") 
abline(h=0)


#plot(diff(KCN4_2000)) # calculo de la variación
#plot(log(KCN4_2000)) #  útil para estabilizar la varianza y transformar los datos en una escala logarítmica.
datos_diff_2000 <- tail(Datos_Hist_2000, -1)
plot(datos_diff_2000$Fecha,diff(log(KCN4_2000)),type="l",
     main = "Variación en cada año", 
     xlab = "Fecha", 
     ylab = "Variación logarítmica") 
abline(h=0)
### fin Cálculo de la variación de las series de tiempo ###

## Descomposición de la serie de tiempo ###
decomposition <- decompose(KCN4, type = "additive")
par(mfrow = c(4, 1))
par(mar = c(3, 3, 1, 1))

plot(decomposition$x, ylab = "Serie de Tiempo Original", xlab = "")
plot(decomposition$trend, ylab = "Tendencia", xlab = "")
plot(decomposition$seasonal, ylab = "Estacionalidad", xlab = "")
plot(decomposition$random, ylab = "Residuales", xlab = "")

decomposition_2000 <- decompose(KCN4_2000, type = "additive")
par(mfrow = c(4, 1))
par(mar = c(3, 3, 1, 1))  
# Crear las gráficas
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
plot(Datos_Hist_2000$Fecha, KCN4_2000, type = "l", xlab = "Fecha", 
     ylab = "Precio Promedio", main = "Precios del Café")

### Estacionariedad ### 


#install.packages("urca")
library(urca)

adf_test <- ur.df(KCN4_2000, type = "trend", lags = 1)

summary_text <- capture.output(summary(adf_test))
tail_summary <- tail(summary_text, n = 11)
tail_summary_df <- data.frame(tail_summary)
colnames(tail_summary_df) <- c("Resumen")
tail_summary_df


library(knitr)

kable(tail[1], format = "html")
### AUTOCORRELACIÓN por año #