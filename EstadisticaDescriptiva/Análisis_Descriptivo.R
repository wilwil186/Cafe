install.packages('tseries') # instalar la libreria

library(tseries)

# Leer los datos
datosHist <- read.csv('/home/wilson/Documentos/cafe/datosHist.csv')

# Verificar estacionariedad
adf.test(datosHist)

# Descomponer la serie temporal
decompose(datosHist)

# Analizar tendencias
plot.ts(datosHist)
lines(fitted(datosHist), col = 'red')

# Modelos de pronóstico
# ARIMA
fit_arima <- arima(datosHist, order = c(1,1,1))
forecast_arima <- forecast(fit_arima, h = 30)

# ETS
fit_ets <- ets(datosHist)
forecast_ets <- forecast(fit_ets, h = 30)