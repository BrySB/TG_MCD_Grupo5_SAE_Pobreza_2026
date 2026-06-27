# ==============================================================================
# FASE 1: MODELACIÓN ESTRUCTURAL ESPACIAL MUNICIPAL (ANCLA 2018)
# ==============================================================================

library(sf)
library(spatialreg)
library(spdep)
library(tidyverse)

# 1. Cargar la base de datos limpia que acabas de generar
base_cov <- read.csv("Covariables_Municipales_18_24_Limpia.csv")

# Filtrar únicamente el año del Censo (2018) para calibrar la estructura
base_2018 <- base_cov %>% filter(periodo == 2018)

# 2. Cargar tu Shapefile municipal de Colombia
ruta_mapa <- "mpios_shp/Municipios.shp"
mpios_shp <- st_read(ruta_mapa)
mpios_shp$cod_mpio <- as.numeric(paste(mpios_shp$DPTO_CCDGO,mpios_shp$MPIO_CCDGO, sep ="")) # Ajusta segun el SHP

mpios_shp <- st_make_valid(mpios_shp)

# 3. Unir la geometría con los datos del 2018
library(readxl)
base_analitica_2018 <- read_excel("BD_IPM_Municipal.xlsx")
base_analitica_2018 <- base_analitica_2018 %>% rename(cod_mpio = ID)
base_analitica_2018 <- base_analitica_2018 %>% rename(ipm_2018 = `IPM Municipal`)

base_analitica_2018 <- base_analitica_2018 %>%
  inner_join(mpios_shp, by = "cod_mpio") %>% 
  inner_join(base_2018, by = "cod_mpio")

# ==============================================================================
# 4. TRANSFORMACIÓN LOGIT DEL IPM (Rigor Metodológico para Proporciones)
# ==============================================================================
# Nota: Si el IPM está en escala de 0 a 100, divídelo por 100 primero.

base_analitica_2018 <- base_analitica_2018 %>%
  mutate(
    ipm_0_1 = ifelse(ipm_2018 > 1, ipm_2018 / 100, ipm_2018),
    ipm_0_1 = pmax(0.001, pmin(0.999, ipm_0_1)), # Ajuste de seguridad
    ipm_logit = log(ipm_0_1 / (1 - ipm_0_1))     # Transformación Logit
  )

base_analitica_2018 <- base_analitica_2018 %>%
  st_as_sf() %>%                       # RE-FUERZA el formato de objeto espacial (SF)
  filter(!st_is_empty(.))             # Elimina registros con geometrías vacías si existen

# 5. MATRIZ DE VECINDAD Y PESOS ESPACIALES W (CON ENFOQUE SEGURO)
# Extraemos explícitamente la geometría limpia para que poly2nb no se confunda
geometria_limpia <- st_geometry(base_analitica_2018)

cat("Calculando la lista de vecinos sobre la geometría validada...\n")
vecinos <- poly2nb(geometria_limpia, queen = TRUE, snap = 0.001)

# zero.policy = TRUE permite que municipios aislados no rompan el modelo
W_municipal <- nb2listw(vecinos, style = "W", zero.policy = TRUE)
cat("¡Matriz W construida con éxito!\n")

# ==============================================================================
# 6. AJUSTE DEL MODELO DE REZAGO ESPACIAL (SPATIAL LAG)
# ==============================================================================
# Seleccionamos variables estratégicas evitando multicolinealidad perfecta.
# Transformamos el Recaudo de ICA per cápita en logaritmo debido a su alta asimetría.

formula_estructural <- ipm_logit ~ tasa_matriculacion_5_16 + desercion +
  infraestructura_basica + mortalidad_infantil_1 +
  log(Rec_ICA_PC + 1) + pct_subsidiado

cat("Ajustando el Modelo de Rezago Espacial para 2018...\n")
modelo_sar_2018 <- lagsarlm(
  formula = formula_estructural,
  data = base_analitica_2018,
  listw = W_municipal,
  zero.policy = TRUE
)

# 7. Desplegar resultados para interpretación de la tesis
print(summary(modelo_sar_2018))

# ==============================================================================
# 8. EXTRACCIÓN DE PARÁMETROS CLAVE PARA LA FASE 2
# ==============================================================================
# Guardamos los coeficientes Betas y el parámetro Rho (dependencia espacial)
coeficientes_beta <- coef(modelo_sar_2018)
rho_estimado <- modelo_sar_2018$rho

cat("\n--- PARÁMETROS ESTRUCTURALES GUARDADOS ---\n")
cat("Rho (Autocorrelación Espacial):", rho_estimado, "\n")