# ==============================================================================
# FASE 3: CALIBRACIÓN POR BENCHMARKING DEPARTAMENTAL OFICIAL (2024)
# ==============================================================================

library(openxlsx)
library(tidyverse)

# 1. Cargar el archivo de IPM Departamental Oficial 2024 del DANE
# (Ajusta la ruta y el nombre del archivo según corresponda en tu equipo)
ruta_depto_oficial <- "Ruta/A/Tu/Archivo/IPM_Depto_2024.xlsx"
df_depto_oficial <- read.xlsx(ruta_depto_oficial)

# Asegurar nombres estándar y escala 0-100 para el IPM oficial
# (Cambia 'ipm_directo' y 'cod_depto' si en tu Excel se llaman distinto)
df_depto_oficial <- df_depto_oficial %>%
  mutate(
    cod_depto = as.numeric(cod_depto),
    ipm_oficial_depto = ifelse(ipm_directo <= 1, ipm_directo * 100, ipm_directo)
  ) %>%
  select(cod_depto, ipm_oficial_depto)

# ==============================================================================
# 2. CALCULAR LA POBLACIÓN POBRE TEÓRICA ANTES DEL AJUSTE
# ==============================================================================
# Unimos nuestras predicciones sintéticas de la Fase 2 con los marcos oficiales

base_pre_benchmarking <- base_analitica_2024 %>%
  select(cod_mpio, mpio, cod_depto, depto, poblacion, ipm_municipal_sintetico_2024) %>%
  left_join(df_depto_oficial, by = "cod_depto")

# 3. CÁLCULO DEL FACTOR DE AJUSTE (PHI) POR DEPARTAMENTO
# Buscamos la relación entre lo que dice el DANE y lo que suma nuestro modelo
factores_ajuste_depto <- base_pre_benchmarking %>%
  group_by(cod_depto) %>%
  summarise(
    poblacion_total_depto = sum(poblacion, na.rm = TRUE),
    # Total de personas pobres en el depto según el DANE (Oficial)
    pob_pobre_oficial = unique(ipm_oficial_depto) * poblacion_total_depto / 100,
    # Total de personas pobres en el depto según nuestro modelo sintético
    pob_pobre_sintetica = sum(poblacion * ipm_municipal_sintetico_2024 / 100, na.rm = TRUE),
    # Factor multiplicativo de calibración
    phi_depto = pob_pobre_oficial / pob_pobre_sintetica
  ) %>%
  select(cod_depto, phi_depto)

# ==============================================================================
# 4. APLICACIÓN DEL BENCHMARKING Y ACOTAMIENTO DE SEGURIDAD
# ==============================================================================
cat("Aplicando factores de calibración departamental...\n")

base_ipm_final_2024 <- base_pre_benchmarking %>%
  left_join(factores_ajuste_depto, by = "cod_depto") %>%
  mutate(
    # Calibración: Multiplicamos el valor sintético por el factor de su departamento
    ipm_municipal_calibrado_2024 = ipm_municipal_sintetico_2024 * phi_depto,
    # Control de seguridad analítica por si la calibración desborda el 100%
    ipm_municipal_calibrado_2024 = pmax(0, pmin(100, ipm_municipal_calibrado_2024))
  )

# ==============================================================================
# 5. VERIFICACIÓN DE COHERENCIA INSTITUCIONAL (EL REPORTE DE CALIBRACIÓN)
# ==============================================================================
cat("\n--- REVISIÓN DE CONSISTENCIA FINAL (EJEMPLO POR DEPARTAMENTO) ---\n")

verificacion <- base_ipm_final_2024 %>%
  group_by(cod_depto, depto) %>%
  summarise(
    IPM_Oficial_DANE = unique(ipm_oficial_depto),
    IPM_Agregado_Modelo = sum(poblacion * ipm_municipal_calibrado_2024) / sum(poblacion),
    Diferencia = IPM_Oficial_DANE - IPM_Agregado_Modelo,
    .groups = "drop"
  )

print(head(verificacion, 10))

# ==============================================================================
# 6. EXPORTACIÓN DEL ENTREGABLE FINAL DE LA TESIS
# ==============================================================================
entregable_tesis <- base_ipm_final_2024 %>%
  select(cod_mpio, mpio, cod_depto, depto, poblacion, 
         ipm_sintetico_espacial = ipm_municipal_sintetico_2024, 
         ipm_final_calibrado_2024 = ipm_municipal_calibrado_2024)

write.csv(entregable_tesis, "Resultados_IPM_Municipal_Final_2024.csv", row.names = FALSE)
cat("\n¡Fase 3 completada! Archivo 'Resultados_IPM_Municipal_Final_2024.csv' exportado.\n")