##################
# Estudio del Café
##################
library(readxl)
library(ggplot2)
library(dplyr)


# Objetivos 1. CREAR UNA NUEVA BASE DE DATOS: (LISTA)

#############################################################
#############################################################
# Situar en la carpeta de trabajo

ruta <-"/home/angel/Descargas/universidad/estadistica/cafe/"
#############################################################
#############################################################

setwd(ruta)
getwd()

# Leer el archivo y separar por sus pestañas 

#########################
#Tabla Precio_Producción
#########################

fichero_precio_produccion <- "Precios-area-y-produccion-de-cafe2.xlsx"
precios_produccion <- paste0(ruta, fichero_precio_produccion)
datos_1_PID <- read_xlsx(precios_produccion, sheet = "1. Precio Interno Diario ")
datos_2_PIM <- read_xlsx(precios_produccion, sheet = "2. Precio Interno Mensual")
datos_3_PEDM <- read_xlsx(precios_produccion, sheet = "3. Precio Ex_Dock Mensual")
datos_4_PEDAC <- read_xlsx(precios_produccion, sheet = "4. Precio Ex_Dock Anual Civil")
datos_5_PEDACF <- read_xlsx(precios_produccion, sheet = "5.Precio Ex_Dock Anual Cafetero")
datos_6_POM <- read_xlsx(precios_produccion, sheet = "6. Precio OIC Mensual")
datos_7_ACDP <- read_xlsx(precios_produccion, sheet = "7. Área cult. dep. producto")
datos_8_ACST <- read_xlsx(precios_produccion, sheet = "8. Área cult. según tecnifi")
datos_9_PROME <- read_xlsx(precios_produccion, sheet = "9. Producción mensual")
datos_10_VACO <- read_xlsx(precios_produccion, sheet = "10. Valor cosecha")


###################
#Tabla Exportación
###################

fichero_exportacion <- "Exportaciones.xlsx"
exportacion <- paste0(ruta, fichero_exportacion)
datosexp_1_VT <- read_xlsx(exportacion, sheet = "1. Total_Volumen", col_names = TRUE, col_types = "text")
datosexp_2_VaT <- read_xlsx(exportacion, sheet = "2. Total_Valor")
datosexp_3_TiVo <- read_xlsx(exportacion, sheet = "3. Tipo_Volumen")
datosexp_4_PV <- read_xlsx(exportacion, sheet = "4. Puerto_Volumen")
datosexp_5_PTVV <- read_xlsx(exportacion, sheet = "5. Puerto_Tipo_Vol_Val")
datosexp_6_DV <- read_xlsx(exportacion, sheet = "6. Destino_Vol")
datosexp_7_DTVV <- read_xlsx(exportacion, sheet = "7. Destino_Tipo_Vol_Val")
datosexp_8_EV <- read_xlsx(exportacion, sheet = "8. Exportador_Vol")
datosexp_9_EVa <- read_xlsx(exportacion, sheet = "9. Exportador_Val")
datosexp_10_ED <- read_xlsx(exportacion, sheet = "10. Exportador_Destino")

#######################
#Tabla Datos Investing
#######################

fichero_DHFc_EEUU <-c("DHFc_EE.UU.(1).csv", "DHFc_EE.UU.(2).csv", "DHFc_EE.UU.(3).csv")
datos_historicos_1 <- paste0(ruta, fichero_DHFc_EEUU[1])
datos_historicos_2 <- paste0(ruta, fichero_DHFc_EEUU[2])
datos_historicos_3 <- paste0(ruta, fichero_DHFc_EEUU[3])

datoshist_1 <- read.csv(datos_historicos_1)
datoshist_2 <- read.csv(datos_historicos_2)
datoshist_3 <- read.csv(datos_historicos_3)

########################
# Nueva Base de DATOS
########################



###############
###############
### Exportacion
###############
###############

#1. Total_Volumen
datosexp_1_VT <- read_xlsx(exportacion, sheet = "1. Total_Volumen", skip = 4, col_names = c("", "", "MES", "Total_Exportaciones"))
Total_Volumen <- datosexp_1_VT %>%
  filter(MES != "MES" & !is.na(as.numeric(Total_Exportaciones))) %>%
  mutate(MES = as.Date(as.numeric(MES), origin = "1899-12-30")) %>%
  select(MES, Total_Exportaciones)
head(Total_Volumen)

#2. Total_Valor
datosexp_2_VaT <- read_xlsx(exportacion, sheet = "2. Total_Valor", skip = 4, col_names = c("", "MES", "Valor_Nominal", ""))
Total_Valor <- datosexp_2_VaT %>%
  filter(MES != "MES" & !is.na(as.numeric(Valor_Nominal))) %>%
  mutate(MES = as.Date(as.numeric(MES), origin = "1899-12-30")) %>%
  select(MES, Valor_Nominal)
head(Total_Valor)


#3. Tipo_Volumen
datosexp_3_TiVo <- read_xlsx(exportacion, sheet = "3. Tipo_Volumen", skip = 8, col_names = c("AÑO", "CAFE_VERDE", "CAFE_VERDE_DESCAFEINADO", "TOSTADO_EN_GRANO", "TOSTADO_Y_MOLIDO", "EXTRACTO_Y_SOLUBLE", "PRODUCTO_DE_COLOMBIA_VERDE_MAS_INDUSTRIALIZADO", "TOTAL"))
Tipo_Volumen <- datosexp_3_TiVo %>%
  select(AÑO, CAFE_VERDE, CAFE_VERDE_DESCAFEINADO, TOSTADO_EN_GRANO, TOSTADO_Y_MOLIDO, EXTRACTO_Y_SOLUBLE, PRODUCTO_DE_COLOMBIA_VERDE_MAS_INDUSTRIALIZADO, TOTAL)
head(Tipo_Volumen)

#4. Puerto_Volumen
datosexp_4_PV <- read_xlsx(exportacion, sheet = "4. Puerto_Volumen", skip = 25, col_names = c("Puerto_de_Embarque", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023" ))
Puerto_Volumen <- datosexp_4_PV
head(Puerto_Volumen)
#OBS: No muestra todas las filas.

#5. Puerto_Tipo_Vol_Val
datosexp_5_PTVV <- read_xlsx(exportacion, sheet = "5. Puerto_Tipo_Vol_Val", skip = 6, col_names = c("Año", "Mes", "Puerto_de_Embarque", "Tipo_de_Cafe", "Sacos_de_70kg_equivalente_real_exportado", "Sacos_de_60Kg_Exportados", "Valor_provisional_de_la_Exportación_USD"))
Puerto_Tipo_Vol_Val <- datosexp_5_PTVV %>%
  slice(-1)
head(Puerto_Tipo_Vol_Val)
#OBS: Se puede obtener las filas faltantes en esta base de datos.

#6. Destino_Vol
datosexp_6_DV <- read_xlsx(exportacion, sheet = "6. Destino_Vol", skip = 25, col_names = c("Paises", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023" ), na = "", n_max = Inf )
Destino_Vol <- datosexp_6_DV
head(Destino_Vol)
#OBS: Faltan Paises. Que raro.

#7. Destino_Tipo_Vol_Val
datosexp_7_DTVV <- read_xlsx(exportacion, sheet = "7. Destino_Tipo_Vol_Val", skip = 7, col_names = c("Año", "Mes", "Pais_de_Destino", "Tipo_de_Cafe", "Sacos_de_70kg_equivalente_real_exportado", "Sacos_de_60Kg_Exportados", "Valor_provisional_de_la_Exportación_USD"), na = "", n_max = Inf )
Destino_Tipo_Vol_Val <- datosexp_7_DTVV %>%
  slice(-1)
head(Destino_Tipo_Vol_Val)

#8. Exportador_Vol
datosexp_8_EV <- read_xlsx(exportacion, sheet = "8. Exportador_Vol", skip = 4, col_names = c("", "", "Año", "Federación_Nacional_de_Cafeteros", "Particulares", "Total"), na = "", n_max = Inf )
Exportador_Vol <- datosexp_8_EV %>%
  slice(5:n()) %>%
  select(-c(...1, ...2))
head(Exportador_Vol)

#9. Exportador_Val
datosexp_9_EVa <- read_xlsx(exportacion, sheet = "9. Exportador_Val", skip = 7, col_names = c( "Año", "Mes", "Federación_Nacional_de_Cafeteros", "Particulares", "Total",""), na = "", n_max = Inf )
Exportador_Val <- datosexp_9_EVa  %>%
  slice(-1) %>%
  select(-c(...6))
head(Exportador_Val)

#10. Exportador_Destino
datosexp_10_ED <- read_xlsx(exportacion, sheet = "10. Exportador_Destino", skip = 8, col_names = c( "Año", "Mes", "Pais_Destino", "Nombre_Exportador", "Sacos_de_70kg_equivalente_real_exportado", "Sacos_de_60Kg_Exportados", "Valor_factura_USD"), na = "", n_max = Inf )
#warning(datosexp_10_ED)
Exportador_Destino <- datosexp_10_ED
head(Exportador_Destino)

#######################
#######################
### FIN <- Exportacion
#######################
#######################


################################
################################
### Precios-Area-Produccion-CAFE
################################
################################


#1. Precio_Interno_Diario
datos_1_PID <- read_xlsx(precios_produccion, sheet = "1. Precio Interno Diario ", skip = 15, col_names = c( "Fecha", "Precio_Interno_125Kg", "Precio_Almendra_Sana", "Incentivo_Calidad", "", "", "", "", "", "", "", "", "", "", ""), na = "", n_max = Inf )
Precio_Interno_Diario <- datos_1_PID %>%
  select(Fecha, Precio_Interno_125Kg, Precio_Almendra_Sana, Incentivo_Calidad)
head(Precio_Interno_Diario)
# Aparce a partir del 11 de enero del 2003 (los datos anteriores del 02 al 10 no aparecen)

#2. Precio_Interno_Mensual
datos_2_PIM <- read_xlsx(precios_produccion, sheet = "2. Precio Interno Mensual", skip =3 , col_names = c( "", "MES", "Precio_Interno"), na = "", n_max = Inf )
Precio_Interno_Mensual <- datos_2_PIM %>%
  mutate(MES = as.Date(as.numeric(MES), origin = "1899-12-30")) %>%
  slice(4:n()) %>%
  select(MES, Precio_Interno)
head(Precio_Interno_Mensual)

#3. Precio_Ex_Dock_Mensual
datos_3_PEDM <- read_xlsx(precios_produccion, sheet = "3. Precio Ex_Dock Mensual", skip =3 , col_names = c( "", "MES", "Precio_Externo"), na = "", n_max = Inf )
Precio_Ex_Dock_Mensual <-datos_3_PEDM %>%
  mutate(MES = as.Date(as.numeric(MES), origin = "1899-12-30")) %>%
  slice(4:n()) %>%
  select(MES, Precio_Externo)  
head(Precio_Ex_Dock_Mensual)


#4. Precio_Ex_Dock_Anual_Civil
datos_4_PEDAC <- read_xlsx(precios_produccion, sheet = "4. Precio Ex_Dock Anual Civil", skip =3 , col_names = c( "", "Año", "Precio_Externo"), na = "", n_max = Inf )
Precio_Ex_Dock_Anual_Civil <-datos_4_PEDAC %>%
  slice(4:n()) %>%
  select(Año, Precio_Externo)  
head(Precio_Ex_Dock_Anual_Civil)


#5. Precio_Ex_Dock_Anual_Cafetero
datos_5_PEDACF <- read_xlsx(precios_produccion, sheet = "5.Precio Ex_Dock Anual Cafetero", skip =3 , col_names = c( "", "Año", "Precio_Externo"), na = "", n_max = Inf )
Precio_Ex_Dock_Anual_Cafetero <-datos_5_PEDACF %>%
  slice(4:n()) %>%
  select(Año, Precio_Externo)  
head(Precio_Ex_Dock_Anual_Cafetero)


#6. Precio_OIC_Mensual
datos_6_POM <- read_xlsx(precios_produccion, sheet = "6. Precio OIC Mensual", skip =15 , col_names = c( "Fecha", "Precio_Indicador_Compuesto_OIC", "Nueva_York_Suaves_Colombianos", "Europa_Suaves_Colombianos", "Promedio_Ponderado_Suaves_Colombianos", "Nueva_York_Otros_Suaves", "Europa_Otros_Suaves", "Promedio_Ponderado_Otros_Suaves", "Nueva_York_Naturales_Brasil", "Europa_Naturales_Brasil", "Promedio_Ponderado_Naturales_Brasil", "Nueva_York_Robustas", "Europa_Robustas", "Promedio_Ponderado_Robustas", ""), na = "", n_max = Inf )
Precio_OIC_Mensual <-datos_6_POM %>%
  select(Fecha, Precio_Indicador_Compuesto_OIC, Nueva_York_Suaves_Colombianos, Europa_Suaves_Colombianos, Promedio_Ponderado_Suaves_Colombianos, Nueva_York_Otros_Suaves, Europa_Otros_Suaves, Promedio_Ponderado_Otros_Suaves, Nueva_York_Naturales_Brasil, Europa_Naturales_Brasil, Promedio_Ponderado_Naturales_Brasil, Nueva_York_Robustas, Europa_Robustas, Promedio_Ponderado_Robustas)  
head(Precio_OIC_Mensual)
# No coloca desde el 01/01/2000 hasta 01/08/2000


#7. Area_cult_dep_producto
datos_7_ACDP <- read_xlsx(precios_produccion, sheet = "7. Área cult. dep. producto", skip =22 , col_names = c("Departamento",  "2002", "2003",  "2004", "2005",  "2006", "2007",  "2008", "2009",  "2010", "2011",  "2012", "2013",  "2014", "2015",  "2016", "2017",  "2018", "2019",  "2020", "2021",  "2022"), na = "", n_max = Inf )
Area_cult_dep_producto <-datos_7_ACDP %>%
  select(Departamento, '2002', '2003',  '2004', '2005',  '2006', '2007',  '2008', '2009',  '2010', '2011',  '2012', '2013',  '2014', '2015',  '2016', '2017', '2018', '2019',  '2020', '2021',  '2022')  
head(Area_cult_dep_producto)
# No toma en cuenta todos los departamentos.


#8. Area_cult_segun_tecnifi
nombres_columnas_8 <- c("","Departamento", "2007_tradicional_1", "2008_tradicional_1", "2009_tradicional_1", "2010_tradicional_1", "2011_tradicional_1", "2012_tradicional_1", "2013_tradicional_1", "2014_tradicional_1", "2015_tradicional_1", "2016_tradicional_1", "2017_tradicional_1", "2018_tradicional_1", "2019_tradicional_1", "2020_tradicional_1", "2021_tradicional_1", "2022_tradicional_1", "2007_tecnificado_envejecido", "2008_tecnificado_envejecido", "2009_tecnificado_envejecido", "2010_tecnificado_envejecido", "2011_tecnificado_envejecido", "2012_tecnificado_envejecido", "2013_tecnificado_envejecido", "2014_tecnificado_envejecido", "2015_tecnificado_envejecido", "2016_tecnificado_envejecido", "2017_tecnificado_envejecido", "2018_tecnificado_envejecido", "2019_tecnificado_envejecido", "2020_tecnificado_envejecido", "2021_tecnificado_envejecido", "2022_tecnificado_envejecido", "2007_tecnificado_joven", "2008_tecnificado_joven", "2009_tecnificado_joven", "2010_tecnificado_joven", "2011_tecnificado_joven", "2012_tecnificado_joven", "2013_tecnificado_joven", "2014_tecnificado_joven", "2015_tecnificado_joven", "2016_tecnificado_joven", "2017_tecnificado_joven", "2018_tecnificado_joven", "2019_tecnificado_joven", "2020_tecnificado_joven", "2021_tecnificado_joven", "2022_tecnificado_joven", "2007_total", "2008_total", "2009_total", "2010_total", "2011_total", "2012_total", "2013_total", "2014_total", "2015_total", "2016_total", "2017_total", "2018_total", "2019_total", "2020_total", "2021_total", "2022_total")
datos_8_ACST <- read_xlsx(precios_produccion, sheet = "8. Área cult. según tecnifi", range = "A6:BN100", col_names = nombres_columnas_8)
Area_cult_segun_tecnifi <-datos_8_ACST %>%
  slice(4:n()) %>%
  select(Departamento, "2007_tradicional_1", "2008_tradicional_1", "2009_tradicional_1", "2010_tradicional_1", "2011_tradicional_1", "2012_tradicional_1", "2013_tradicional_1", "2014_tradicional_1", "2015_tradicional_1", "2016_tradicional_1", "2017_tradicional_1", "2018_tradicional_1", "2019_tradicional_1", "2020_tradicional_1", "2021_tradicional_1", "2022_tradicional_1", "2007_tecnificado_envejecido", "2008_tecnificado_envejecido", "2009_tecnificado_envejecido", "2010_tecnificado_envejecido", "2011_tecnificado_envejecido", "2012_tecnificado_envejecido", "2013_tecnificado_envejecido", "2014_tecnificado_envejecido", "2015_tecnificado_envejecido", "2016_tecnificado_envejecido", "2017_tecnificado_envejecido", "2018_tecnificado_envejecido", "2019_tecnificado_envejecido", "2020_tecnificado_envejecido", "2021_tecnificado_envejecido", "2022_tecnificado_envejecido", "2007_tecnificado_joven", "2008_tecnificado_joven", "2009_tecnificado_joven", "2010_tecnificado_joven", "2011_tecnificado_joven", "2012_tecnificado_joven", "2013_tecnificado_joven", "2014_tecnificado_joven", "2015_tecnificado_joven", "2016_tecnificado_joven", "2017_tecnificado_joven", "2018_tecnificado_joven", "2019_tecnificado_joven", "2020_tecnificado_joven", "2021_tecnificado_joven", "2022_tecnificado_joven", "2007_total", "2008_total", "2009_total", "2010_total", "2011_total", "2012_total", "2013_total", "2014_total", "2015_total", "2016_total", "2017_total", "2018_total", "2019_total", "2020_total", "2021_total", "2022_total")
head(Area_cult_segun_tecnifi)

#9. Produccion_mensual
datos_9_PROME <- read_xlsx(precios_produccion, sheet = "9. Producción mensual", col_names = c( "", "MES", "Produccion"), na = "", n_max = Inf )
Produccion_mensual <-datos_9_PROME %>%
  mutate(MES = as.Date(as.numeric(MES), origin = "1899-12-30")) %>%
  slice(6:n()) %>%
  select(MES, Produccion)
head(Produccion_mensual)

#10. Valor_cosecha
datos_10_VACO <- read_xlsx(precios_produccion, sheet = "10. Valor cosecha", col_names = c( "Año_Calendario", "Valor_Cosecha_Calendario", "", "Año_Cafetero", "Valor_Cosecha_Cafetero"), na = "", n_max = Inf )
Valor_cosecha <-datos_10_VACO %>%
  slice(6:n()) %>%
  select(Año_Calendario, Valor_Cosecha_Calendario, Año_Cafetero, Valor_Cosecha_Cafetero)
head(Valor_cosecha)


#######################################
#######################################
### FIN <- Precios-Area-Produccion-CAFE
#######################################
#######################################

# Objetivo 1 FIN. CREAR UNA NUEVA BASE DE DATOS: (LISTA)

# Objetivo 2 Comparar los precios y los volumenes de exportaciones y crear una columna de Precio x Volumen de exportaciones.
# Objetivo 3 Graficar las Cifras Mensuales (Esta ambigua esta pregunta)
# Objetivo 4 Graficar las Cifras Mensuales de Exportaciones 
# Objetivo 5 Graficar los paises a los que se exporta el cafe colombiano con el volumen de exportacion
# Objetivo 6 Graficar los paises a los que se exporta el cafe colombiano con el precio de exportacion
# Objetivo 7 (Opcional) Cuantas hectareas (o cuales fincas) son las que producen mas cafe y menos cafe, Cuanto producen en terminos de volumen, Que tipo de cafe y que tipo de geografia produce cada cafe.
