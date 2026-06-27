# ==============================================================================
# FASE 2: PROYECCIÓN SINTÉTICA ESPACIAL MUNICIPAL (PERIODO 2024)
# ==============================================================================

library(sf)
library(spdep)
library(tidyverse)

# 1. Filtrar los datos limpios para el año 2024
base_2024 <- base_final_perfecta %>% filter(periodo == 2024)

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
# 4. CALCULAR EL COMPONENTE DE REGRESIÓN (X * Beta) PARA 2024
# ==============================================================================
# Extraemos los coeficientes del modelo calibrado en la Fase 1
betas <- coeficientes_beta

# Construimos la matriz X para 2024 respetando exactamente la misma fórmula
X_2024 <- model.matrix(~ tasa_matriculacion_5_16 + desercion +
                         infraestructura_basica + mortalidad_infantil_1 +
                         log(Rec_ICA_PC + 1) + pct_subsidiado, 
                       data = base_analitica_2024)

# Predicción lineal pura (Sintética sin espacio)
X_beta_2024 <- X_2024 %*% betas

# ==============================================================================
# 5. PROYECCIÓN ESPACIAL SIMULTÁNEA (Multiplicador Espacial)
# ==============================================================================
# Multiplicamos la matriz inversa por el vector X_beta para incorporar el rezago de los vecinos
ipm_logit_pred_2024 <- matriz_espacial_inversa %*% X_beta_2024

# Enlazar la predicción en escala logit a nuestra base
base_analitica_2024$ipm_logit_pred <- as.vector(ipm_logit_pred_2024)

# ==============================================================================
# 6. TRANSFORMACIÓN INVERSA LOGIT (Regresar a la escala de porcentaje de IPM)
# ==============================================================================
# Pasamos de la escala (-Inf, +Inf) a la escala original (0% a 100%)
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

# Verificación de que no existan valores extraños
errores_rango <- base_analitica_2024 %>% 
  filter(ipm_municipal_sintetico_2024 < 0 | ipm_municipal_sintetico_2024 > 100) %>% 
  nrow()

cat("Municipios con estimaciones fuera de rango lógico (0-100%):", errores_rango, "\n")

# Guardar base intermedia para la Fase 3
write.csv(base_analitica_2024 %>% select(cod_mpio, mpio, cod_depto, depto, poblacion, ipm_municipal_sintetico_2024), 
          "IPM_Municipal_Sintetico_Espacial_2024.csv", row.names = FALSE)