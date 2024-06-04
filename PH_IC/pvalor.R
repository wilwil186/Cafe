# Definir la hipótesis nula
H0 <- 521257.7

# Filtrar y seleccionar aleatoriamente el 10% de los datos del año 2024
DestinoDeEsportación24 <- subset(DestinoDeEsportación, Año == 2024)
tamano_muestra <- round(0.1 * nrow(DestinoDeEsportación24))
set.seed(42)
DesExRandom24 <- DestinoDeEsportación24[sample(nrow(DestinoDeEsportación24), size = tamano_muestra, replace = FALSE), ]

# Utilizar DesExRandom24 como las facturas
facturas <- DesExRandom24$Valor_factura_USD

# Calcular la media muestral
media_muestral <- mean(facturas)

# Calcular el estadístico de contraste
Z <- (media_muestral - H0) / sd(facturas) / sqrt(length(facturas))

# Calcular el p-valor
p_valor <- pt(-abs(Z), df = length(facturas) - 1)

# Comparar el p-valor con el nivel de significación
alpha <- 0.05
if (p_valor < alpha) {
  print("Rechazamos la hipótesis nula. Hay evidencia estadística significativa para concluir que el valor promedio de las facturas es diferente del valor establecido en la hipótesis nula.")
} else {
  print("No rechazamos la hipótesis nula. No hay evidencia estadística significativa para concluir que el valor promedio de las facturas es diferente del valor establecido en la hipótesis nula.")
}
