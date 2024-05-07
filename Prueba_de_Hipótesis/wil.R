setwd("~/Documentos/cafe") # ruta de trabajo
# Cargar datos y verlos
datosHist <- read.csv('datosHist.csv')
View(datosHist)

DestinoDeEsportación <- read.csv('DestinoDeEsportación.csv')
View(DestinoDeEsportación)

# Filtrar los datos para incluir solo los registros del año 2023 y 2024
DestinoDeEsportación23_24 <- subset(DestinoDeEsportación, Año >= 2023 & Año <= 2024)
View(DestinoDeEsportación23_24)

# Calcular el promedio de la columna Valor_factura_USD para los años 2023 y 2024 (hipótesis nula)
promedio_valor <- mean(DestinoDeEsportación23_24$Valor_factura_USD)
desviacion_estandar <- sd(DestinoDeEsportación23_24$Valor_factura_USD)
print(promedio_valor)  # Imprimir el resultado

# Definir los valores de las facturas (USD)
# Dominio 
x <- seq(min(DestinoDeEsportación23_24$Valor_factura_USD),
         max(DestinoDeEsportación23_24$Valor_factura_USD),
         length.out = 100)

# Calcular los límites para la curva normal
lim_inf <- max(0, promedio_valor - 3 * desviacion_estandar)
lim_sup <- promedio_valor + 3 * desviacion_estandar

# Graficar la curva normal (DestinoDeEsportación23_24) en rojo
curve(dnorm(x, mean = promedio_valor, sd = desviacion_estandar), 
      from = lim_inf, 
      to = lim_sup, 
      xlab = "Valor de la factura (USD)", 
      ylab = "Densidad",
      main = "Distribución Normal del Valor de la Factura",
      col = "red")


# Agregar línea punteada en el promedio
abline(v = promedio_valor, col = "darkred", lty = 2)


# Filtrar y seleccionar aleatoriamente el 10% de los datos del año 2024 (hipótesis alternativa)
DestinoDeEsportación24 <- subset(DestinoDeEsportación, Año == 2024)
tamano_muestra <- round(0.1 * nrow(DestinoDeEsportación24))
set.seed(42)
DesExRandom24 <- DestinoDeEsportación24[sample(nrow(DestinoDeEsportación24), size = tamano_muestra, replace = FALSE), ]
View(DesExRandom24)

# Calcular el promedio de la columna Valor_factura_USD para los datos aleatorios del año 2024 (hipótesis alternativa)
promedioExrandom24 <- mean(DesExRandom24$Valor_factura_USD)
DesviaciónExRandom24 <- sd(DesExRandom24$Valor_factura_USD)
print(promedioExrandom24)  # Imprimir el resultado
x <- DesExRandom24$Valor_factura_USD
# Superponer la segunda curva normal (DesExRandom24) en azul
curve(dnorm(x, mean = promedioExrandom24, sd =desviacion_estandar),
      add = TRUE, 
      col = "blue")

# Agregar línea punteada en el promedio de DesExRandom24
abline(v = promedioExrandom24, col = "darkblue", lty = 2)

# Calcular el p-valor
p_valor <- 2 * pnorm(abs((promedio_valor - promedioExrandom24) / desviacion_estandar))

print(p_valor)  # Imprimir el resultado

