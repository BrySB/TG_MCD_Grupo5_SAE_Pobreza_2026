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
# 3. CONSTRUCCIÓN DE LA MATRIZ INVERSA DE LEONTIEF (I - Rho * W)
# ==============================================================================
cat("Generando la estructura matricial inversa de Leontief para 2024...\n")

# Convertimos la lista de pesos W en una matriz densa tradicional
W_matriz <- listw2mat(W_municipal)
I_matriz <- diag(nrow(W_matriz)) # Matriz Identidad

# Calculamos (I - Rho * W)
matriz_espacial_inversa <- solve(I_matriz - rho_estimado * W_matriz)

# ==============================================================================
# 4. CALCULAR EL COMPONENTE DE REGRESIÓN (X * Beta) CORREGIDO
# ==============================================================================
# Construimos la matriz X para 2024 respetando la fórmula exacta de la Fase 1
X_2024 <- model.matrix(~ tasa_matriculacion_5_16 + desercion +
                         infraestructura_basica + mortalidad_infantil_1 +
                         log(Rec_ICA_PC + 1) + pct_subsidiado, 
                       data = base_analitica_2024)

# Extraemos los nombres de las columnas que realmente generó la matriz X
nombres_variables <- colnames(X_2024)

# Filtramos el vector de coeficientes para quedarnos SOLO con los que coinciden con X
# Esto elimina de forma segura a Rho o Sigma si estaban metidos en 'coeficientes_beta'
betas_limpios <- coeficientes_beta[nombres_variables]

cat("Dimensiones de verificación:\n")
cat("Columnas de la matriz X 2024:", ncol(X_2024), "\n")
cat("Elementos del vector Beta limpio:", length(betas_limpios), "\n")

# Predicción lineal pura (Ya no dará error porque las dimensiones coinciden)
X_beta_2024 <- X_2024 %*% betas_limpios

# ==============================================================================
# 5. PROYECCIÓN ESPACIAL SIMULTÁNEA (Multiplicador Inverso)
# ==============================================================================
# Multiplicamos la matriz inversa por el vector X_beta para propagar el efecto vecino
ipm_logit_pred_2024 <- matriz_espacial_inversa %*% X_beta_2024

# Guardamos el vector en nuestra base analítica
base_analitica_2024$ipm_logit_pred <- as.vector(ipm_logit_pred_2024)

# ==============================================================================
# 6. TRANSFORMACIÓN INVERSA LOGIT (Retorno a Porcentaje 0% - 100%)
# ==============================================================================
base_analitica_2024 <- base_analitica_2024 %>%
  mutate(
    ipm_pred_0_1 = exp(ipm_logit_pred) / (1 + exp(ipm_logit_pred)),
    ipm_municipal_sintetico_2024 = ipm_pred_0_1 * 100
  )

# ==============================================================================
# 7. INSPECCIÓN VISUAL DESCRIPTIVA PRELIMINAR
# ==============================================================================
cat("\n--- RESUMEN DESCRIPTIVO DEL IPM MUNICIPAL ESTIMADO 2024 ---\n")
print(summary(base_analitica_2024$ipm_municipal_sintetico_2024))