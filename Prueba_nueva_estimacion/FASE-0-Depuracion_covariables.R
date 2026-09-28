# ==============================================================================
# SCRIPT DE DEPURACIÓN E IMPUTACIÓN ESPACIO-TEMPORAL DE COVARIABLES (CORREGIDO)
# ==============================================================================

library(tidyverse)
library(sf)
library(spdep)
library(readr)
library(stringr)

# 1. Cargar el archivo CSV
setwd("C:/Users/basbo/PUJ Cali/Erick Caicedo Ruiz - TDG Ciencia de Datos/Prueba_nueva_estimacion")

df_cov <- read.csv2("Covariables_18&24.csv", stringsAsFactors = FALSE)

# ------------------------------------------------------------------------------
# NUEVO PASO CRÍTICO: Limpiar comas y convertir covariables a NUMÉRICA (dbl)
# ------------------------------------------------------------------------------
limpiar_num <- function(x) {
  x_clean <- str_replace_all(as.character(x), ",", ".")
  x_clean <- ifelse(x_clean %in% c("#N/D", "NA", "N/A", "", "NaN"), NA, x_clean)
  return(as.numeric(x_clean))
}

# Aplicar la limpieza numérica a las columnas de covariables
df_cov <- df_cov %>%
  mutate(across(
    .cols = -c(cod_depto, depto, cod_mpio, mpio, periodo), 
    .fns = limpiar_num
  ))


###############################################################################################
library(tidyverse)
library(readr)

# 1. Cargar el nuevo CSV de Earth Engine con departamento
viirs_raw <- read_csv("VIIRS_Municipios_Colombia_18_24_v2.csv")

# 2. Limpieza y homologación de Departamento y Municipio
library(tidyverse)

# Preparar la base satelital limpiando strings de Departamento y Municipio
viirs_limpio <- viirs_raw %>%
  rename(
    depto_satelital = ADM1_NAME,
    mpio_satelital  = ADM2_NAME,
    viirs_rad       = mean
  ) %>%
  mutate(
    # Quitar tildes y mayúsculas
    depto_clean = toupper(iconv(depto_satelital, to = "ASCII//TRANSLIT")),
    mpio_clean  = toupper(iconv(mpio_satelital, to = "ASCII//TRANSLIT")),
    
    # Estandarizar Departamentos (Mapeo a la convención DANE de tu base)
    depto_clean = case_when(
      grepl("VALLE", depto_clean) ~ "VALLE",
      grepl("GUAJIRA", depto_clean) ~ "LA GUAJIRA",
      grepl("SAN ANDRES|ARCHIPIELAGO", depto_clean) ~ "SAN ANDRES",
      grepl("CUNDINAMARCA", depto_clean) & grepl("BOGOTA", mpio_clean) ~ "BOGOTA D.C.",
      grepl("BOGOTA", depto_clean) ~ "BOGOTA D.C.",
      TRUE ~ depto_clean
    ),
    
    # Homologar los Municipios problemáticos
    mpio_clean = case_when(
      grepl("BOGOTA|SANTAFE", mpio_clean) ~ "BOGOTA D.C.",
      grepl("PROVIDENCIA", mpio_clean) ~ "PROVIDENCIA",
      grepl("SAN ANDRES", mpio_clean) ~ "SAN ANDRES",
      TRUE ~ mpio_clean
    ),
    
    log_viirs = log(pmax(0, viirs_rad) + 1)
  ) %>%
  group_by(depto_clean, mpio_clean, periodo) %>%
  summarise(
    viirs_rad = mean(viirs_rad, na.rm = TRUE),
    log_viirs = mean(log_viirs, na.rm = TRUE),
    .groups = "drop"
  )

# 2. Estandarizar la base de covariables
df_cov <- df_cov %>%
  mutate(
    depto_clean = toupper(iconv(depto, to = "ASCII//TRANSLIT")),
    mpio_clean  = toupper(iconv(mpio, to = "ASCII//TRANSLIT")),
    
    # Ajustar para alinearse a la regla
    depto_clean = case_when(
      grepl("VALLE", depto_clean) ~ "VALLE",
      grepl("GUAJIRA", depto_clean) ~ "LA GUAJIRA",
      grepl("SAN ANDRES", depto_clean) ~ "SAN ANDRES",
      grepl("BOGOTA", depto_clean) ~ "BOGOTA D.C.",
      TRUE ~ depto_clean
    ),
    mpio_clean = case_when(
      grepl("BOGOTA", mpio_clean) ~ "BOGOTA D.C.",
      grepl("PROVIDENCIA", mpio_clean) ~ "PROVIDENCIA",
      grepl("SAN ANDRES", mpio_clean) ~ "SAN ANDRES",
      TRUE ~ mpio_clean
    )
  ) %>%
  left_join(viirs_limpio, by = c("depto_clean", "mpio_clean", "periodo")) %>%
  select(-depto_clean, -mpio_clean)

# 3. Verificación de NAs
cat("NAs restantes en log_viirs:", sum(is.na(df_cov$log_viirs)), "\n")
######################################################################################


# 2. Cargar el Shapefile municipal de Colombia para extraer la vecindad geográfica
# (1.122 municipios o la gran mayoría)

mpios_shp <- st_read("mpios_shp/Div_Pol.shp")
mpios_shp$cod_mpio <- as.numeric(mpios_shp$MpCodigo) # Ajusta segun el SHP

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


imputar_periodo <- function(df_año, df_vecindad) {
  
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


# Ver exactamente cuáles municipios no cruzaron
as.data.frame(base_covariables_limpia %>% 
  filter(is.na(log_viirs)) %>% 
  select(cod_mpio, depto, mpio, periodo))
