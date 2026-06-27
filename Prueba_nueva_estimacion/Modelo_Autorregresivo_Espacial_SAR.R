# ==============================================================================
# FASE 1: MODELACIÓN ESTRUCTURAL ESPACIAL MUNICIPAL (ANCLA 2018)
# ==============================================================================

install.packages("spatialreg", "spdep")

# 1. Cargar librerías necesarias
library(sf)        # Para el manejo de datos geométricos (.shp)
library(spatialreg)# Para los modelos de regresión espacial (SAR/SEM)
library(spdep)     # Para la creación de matrices de vecindad y pesos
library(tidyverse) # Para manipulación de datos (dplyr, ggplot2)

# 2. Cargar el Shapefile municipal de Colombia
# Reemplaza la ruta por la ubicación real de tu archivo
mpios_shp <- st_read("Ruta/A/Tu/Archivo/mpios_colombia.shp")

# Asegurar que el código DANE municipal sea numérico para el cruce
# Nota: Ajusta "MPIO_CCDGO" si tu shapefile tiene otro nombre de columna
mpios_shp$cod_mpio <- as.numeric(mpios_shp$MPIO_CCDGO)

# 3. Cargar tus datos consolidados del 2018 (IPM + Covariables + ICA)
datos_2018 <- read.csv("Ruta/A/Tu/Archivo/datos_municipales_2018.csv")
datos_2018$cod_mpio <- as.numeric(datos_2018$cod_mpio)

# 4. Unir el Shapefile con los datos alfanuméricos
# Usamos un inner_join para quedarnos solo con los municipios que tengan datos y geometría
base_espacial_2018 <- mpios_shp %>% 
  inner_join(datos_2018, by = "cod_mpio")

# 5. Construcción de la estructura espacial (Matriz de pesos W)
# Creación de la lista de vecinos basándonos en contigüidad criterio Reina (Queen)
vecinos_mpio <- poly2nb(base_espacial_2018, queen = TRUE)

# NOTA MAESTRÍA: San Andrés y Providencia al ser islas no tendrán vecinos y generarán un error.
# Con 'zero.policy = TRUE' le permitimos al modelo manejar municipios aislados.
W_municipal <- nb2listw(vecinos_mpio, style = "W", zero.policy = TRUE)

# ==============================================================================
# 6. ESPECIFICACIÓN Y AJUSTE DEL MODELO SPATIAL LAG (SAR)
# ==============================================================================
# Formulamos la ecuación. Recuerda NO incluir coberturas individuales e índice compuesto juntos.
# Incluimos el ICA_2018 (idealmente transformado en logaritmo si está muy sesgado)

formula_2018 <- ipm_2018 ~ repitencia + tasa_matriculacion_5_16 + 
  log(ica_2018 + 1) + infraestructura_basica + 
  mortalidad_infantil

# Ajuste del modelo mediante Máxima Verosimilitud (Spatial Lag Model)
modelo_sar_2018 <- lagsarlm(
  formula = formula_2018,
  data = base_espacial_2018,
  listw = W_municipal,
  zero.policy = TRUE
)

# 7. Diagnóstico del Modelo
cat("--- Resumen del Modelo Estructural Espacial 2018 ---\n")
summary(modelo_sar_2018)

# Guardar los coeficientes y parámetros para la Fase 2
coeficientes_2018 <- coef(modelo_sar_2018)
rho_2018          <- modelo_sar_2018$rho

cat("\nParámetro de dependencia espacial (Rho):", rho_2018, "\n")