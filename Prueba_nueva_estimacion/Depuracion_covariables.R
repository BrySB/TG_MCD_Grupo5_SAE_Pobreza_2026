# ==============================================================================
# SCRIPT DE DEPURACIÓN E IMPUTACIÓN ESPACIO-TEMPORAL DE COVARIABLES
# ==============================================================================

library(tidyverse)
library(sf)
library(spdep)
library(readr)
library(csv2)

# 1. Cargar tu archivo CSV (el que acabas de subir)
# Reemplaza por tu ruta local si es necesario
df_cov <- read.csv2("Covariables_18&24.csv", stringsAsFactors = FALSE)

# 2. Cargar el Shapefile municipal de Colombia para extraer la vecindad geográfica
# (Asegúrate de que contenga los 1.122 municipios o la gran mayoría)

mpios_shp <- st_read("mpios_shp/Municipios.shp")
mpios_shp$cod_mpio <- as.numeric(paste(mpios_shp$DPTO_CCDGO, mpios_shp$MPIO_CCDGO, sep =)) # Ajusta segun el SHP

# 3. Extraer la lista de vecinos geográficos de cada municipio a partir del SHP
# Corregimos los errores de bordes y auto-intersecciones del mapa del DANE
cat("Corrigiendo inconsistencias topológicas del shapefile...\n")
mpios_shp <- st_make_valid(mpios_shp)

# Añadimos snap = 0.001 (un pequeño margen de tolerancia en metros por si los límites no se tocan perfectamente)
vecinos_lista <- poly2nb(mpios_shp, queen = TRUE, snap = 0.001)

# Crear un data.frame con las relaciones de vecindad para poder usarlas en dplyr
df_vecinos <- data.frame(
  cod_mpio = mpios_shp$cod_mpio,
  vecinos = I(lapply(vecinos_lista, function(x) mpios_shp$cod_mpio[x]))
)

# 4. FUNCIÓN DE IMPUTACIÓN MIXTA (Espacial + Departamental)


imputar_periodo_seguro <- function(df_año, df_vecindad) {
  
  # Unir la estructura de vecinos por el código explícito (NUNCA por posición de fila)
  df_trabajo <- df_año %>% left_join(df_vecindad, by = "cod_mpio")
  
  # Identificar variables numéricas a procesar
  vars_numericas <- names(df_trabajo)[sapply(df_trabajo, is.numeric)]
  vars_a_imputar <- setdiff(vars_numericas, c("cod_depto", "cod_mpio", "periodo", "poblacion"))
  
  # Iterar sobre cada variable de forma segura
  for (var in vars_a_imputar) {
    
    # Identificar qué filas tienen NA en esta variable específica
    filas_con_na <- which(is.na(df_trabajo[[var]]))
    
    if (length(filas_con_na) == 0) next
    
    for (idx in filas_con_na) {
      depto_actual <- df_trabajo$cod_depto[idx]
      vecinos_actuales <- df_trabajo$vecinos[[idx]]
      
      # --- PASO 1: Buscar en vecinos espaciales reales ---
      valores_vecinos <- df_trabajo %>% 
        filter(cod_mpio %in% vecinos_actuales) %>% 
        pull(!!sym(var))
      
      val_imputado <- mean(valores_vecinos, na.rm = TRUE)
      
      # --- PASO 2: Si falla la vecindad, usar la Mediana Departamental ---
      if (is.na(val_imputado) || is.nan(val_imputado)) {
        val_imputado <- df_trabajo %>% 
          filter(cod_depto == depto_actual) %>% 
          pull(!!sym(var)) %>% 
          median(na.rm = TRUE)
      }
      
      # Asignar el valor recuperado directamente a la celda
      df_trabajo[idx, var] <- val_imputado
    }
  }
  
  return(df_trabajo %>% select(-vecinos))
}

# 5. EJECUTAR LA IMPUTACIÓN SEPARANDO POR PERIODO (2018 y 2024)
df_2018_limpio <- df_cov %>% filter(periodo == 2018) %>% imputar_periodo(df_vecinos)
df_2024_limpio <- df_cov %>% filter(periodo == 2024) %>% imputar_periodo(df_vecinos)

# 6. UNIFICAR LA BASE FINAL TOTALMENTE DEPURADA
base_covariables_limpia <- bind_rows(df_2018_limpio, df_2024_limpio)

# 7. Prueba de que los NA funcionan

nas_iniciales <- colSums(is.na(df_cov))
nas_intermedios <- colSums(is.na(base_covariables_limpia))

tabla_comparativa <- data.frame(
  Variable = names(nas_iniciales),
  NAs_Originales = nas_iniciales,
  NAs_Post_Espacial = nas_intermedios
) %>% filter(NAs_Originales > 0)

cat("--- NUEVA COMPARATIVA DE ENTRADA (Mapeo Seguro) ---\n")
print(tabla_comparativa)

# Guardar tu nueva base analítica limpia
write.csv(base_covariables_limpia, "Covariables_Municipales_18_24_Limpia.csv", row.names = FALSE)

