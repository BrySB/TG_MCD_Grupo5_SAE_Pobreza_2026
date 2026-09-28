# ==============================================================================
# FASE 2: PROYECCIÓN SINTÉTICA ESPACIAL MUNICIPAL (PERIODO 2024)
# ==============================================================================

library(sf)
library(spdep)
library(tidyverse)

# 1. Filtrar los datos limpios para el año 2024
base_2024 <- base_cov %>% filter(periodo == 2024)

# 2. Asegurar que el orden de los municipios en la base 2024 coincida 
# exactamente con el de la matriz espacial W construida en la Fase 1
base_analitica_2024 <- data.frame(cod_mpio = base_analitica_2018$cod_mpio) %>%
  left_join(base_2024, by = "cod_mpio")

# ==============================================================================
# FASE 3. CONSTRUCCIÓN DE LA MATRIZ INVERSA DE LEONTIEF (I - Rho * W)
# ==============================================================================
cat("Generando la estructura matricial inversa de Leontief para 2024...\n")

# Convertimos la lista de pesos W en una matriz densa tradicional
W_matriz <- listw2mat(W_municipal)
I_matriz <- diag(nrow(W_matriz)) # Matriz Identidad

# Calculamos (I - Rho * W)
matriz_espacial_inversa <- solve(I_matriz - rho_estimado * W_matriz)

# ==============================================================================
# FASE 4: CÁLCULO DEL COMPONENTE DE REGRESIÓN (X * Beta) - MODELO 3
# ==============================================================================

# 1. Verificar y tratar valores NA en las covariables de 2024 (Relleno de seguridad con la media)
vars_modelo3 <- c("tasa_matriculacion_5_16", "infraestructura_basica", 
                  "mortalidad_infantil_1", "log_viirs", "pct_subsidiado", 
                  "ICEE_rural", "rmm42")

base_analitica_2024 <- base_analitica_2024 %>%
  mutate(across(all_of(vars_modelo3), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# 2. Construcción de la Matriz X 2024 (na.action = na.pass evita que elimine filas)
X_2024 <- model.matrix(~ 1 + tasa_matriculacion_5_16 + infraestructura_basica + 
                         mortalidad_infantil_1 + log_viirs + pct_subsidiado + 
                         ICEE_rural + rmm42, 
                       data = base_analitica_2024,
                       na.action = na.pass)

# 3. Extraer y alinear Betas del Modelo 3 con la Matriz X
betas_mod3 <- coef(mod_sar_3)[colnames(X_2024)]

# Verificación en consola
cat("Dimensiones Matriz X 2024:", dim(X_2024), "\n")
cat("Cantidad de Betas alineados:", length(betas_mod3), "\n")

# 4. Predicción lineal estructural (X * Beta)
X_beta_2024 <- X_2024 %*% betas_mod3

# ==============================================================================
# FASE 5: PROYECCIÓN ESPACIAL SIMULTÁNEA Y TRANSFORMACIÓN INVERSA LOGIT
# =============================================================================

# 1. Matriz de pesos W en formato denso e Identidad
W_matriz <- listw2mat(W_municipal)
I_matriz <- diag(nrow(W_matriz))

# 2. Inversa de Leontief usando el Rho del Modelo 3 (rho = 0.6181)
rho_mod3 <- mod_sar_3$rho
matriz_espacial_inversa <- solve(I_matriz - rho_mod3 * W_matriz)

# 3. Propagación del efecto espacial sobre X_beta
ipm_logit_pred_2024 <- matriz_espacial_inversa %*% X_beta_2024

# 4. Asignar vector a la base y aplicar la transformación Logit Inversa (Escala 0-100%)
base_analitica_2024 <- base_analitica_2024 %>%
  mutate(
    ipm_logit_pred = as.vector(ipm_logit_pred_2024),
    # Transformación logística inversa
    ipm_pred_0_1 = exp(ipm_logit_pred) / (1 + exp(ipm_logit_pred)),
    ipm_municipal_sintetico_2024 = ipm_pred_0_1 * 100
  )

# 5. Comprobación del resultado
cat("\n--- RESUMEN DEL IPM SINTÉTICO MUNICIPAL PROYECTADO 2024 ---\n")
print(summary(base_analitica_2024$ipm_municipal_sintetico_2024))