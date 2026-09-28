# ==============================================================================
# FASE 1: MODELACIÓN ESTRUCTURAL ESPACIAL MUNICIPAL (ANCLA 2018)
# ==============================================================================

library(sf)
library(spatialreg)
library(spdep)
library(tidyverse)
library(readxl)
library(stringr)

# 1. Cargar la base de datos de covariables
base_cov <- read.csv("Covariables_Municipales_18_24_Limpia.csv", stringsAsFactors = FALSE)

# Filtrar únicamente el año 2018
base_2018 <- base_cov %>% filter(periodo == 2018)

# 2. Cargar Shapefile municipal de Colombia
ruta_mapa <- "mpios_shp/Municipios.shp"
mpios_shp <- st_read(ruta_mapa)
mpios_shp$cod_mpio <- as.numeric(paste(mpios_shp$DPTO_CCDGO, mpios_shp$MPIO_CCDGO, sep = ""))
mpios_shp <- st_make_valid(mpios_shp)

# 3. Unir la geometría con el IPM y las covariables
base_analitica_2018 <- read_excel("BD_IPM_Municipal.xlsx") %>% 
  rename(cod_mpio = ID, ipm_2018 = `IPM Municipal`) %>% 
  select(cod_mpio, ipm_2018)

base_analitica_2018 <- base_analitica_2018 %>%
  inner_join(mpios_shp, by = "cod_mpio") %>% 
  inner_join(base_2018, by = "cod_mpio")

# ==============================================================================
# 4. LIMPIEZA MASIVA DE TIPO DE DATOS Y TRANSFORMACIÓN LOGIT
# ==============================================================================

# Función auxiliar para convertir texto con comas / "#N/D" a número
limpiar_num <- function(x) {
  x_clean <- str_replace_all(as.character(x), ",", ".")
  x_clean <- ifelse(x_clean %in% c("#N/D", "NA", "N/A", ""), NA, x_clean)
  return(as.numeric(x_clean))
}

base_analitica_2018 <- base_analitica_2018 %>%
  mutate(
    # A. Transformación Logit del IPM
    ipm_0_1 = ifelse(ipm_2018 > 1, ipm_2018 / 100, ipm_2018),
    ipm_0_1 = pmax(0.001, pmin(0.999, ipm_0_1)),
    ipm_logit = log(ipm_0_1 / (1 - ipm_0_1)),
    
    # B. Limpieza masiva de covariables a numéricas
    aseguramiento_capado    = limpiar_num(aseguramiento_capado),
    pct_subsidiado          = limpiar_num(pct_subsidiado),
    tasa_matriculacion_5_16 = limpiar_num(tasa_matriculacion_5_16),
    desercion               = limpiar_num(desercion),
    infraestructura_basica  = limpiar_num(infraestructura_basica),
    ICEE_rural              = limpiar_num(ICEE_rural),
    Rec_ICA_PC              = limpiar_num(Rec_ICA_PC)
  )

# Imputación rápida de seguridad si existen NA en infraestructura o ICA
base_analitica_2018 <- base_analitica_2018 %>%
  mutate(
    infraestructura_basica = ifelse(is.na(infraestructura_basica), median(infraestructura_basica, na.rm = TRUE), infraestructura_basica),
    Rec_ICA_PC             = ifelse(is.na(Rec_ICA_PC), median(Rec_ICA_PC, na.rm = TRUE), Rec_ICA_PC)
  )

# Convertir a SF
base_analitica_2018 <- base_analitica_2018 %>%
  st_as_sf() %>% 
  filter(!st_is_empty(.))

# ==============================================================================
# 5. MATRIZ DE VECINDAD Y PESOS ESPACIALES W
# ==============================================================================

geometria_limpia <- st_geometry(base_analitica_2018)

cat("Calculando lista de vecinos...\n")
vecinos <- poly2nb(geometria_limpia, queen = TRUE, snap = 0.001)
W_municipal <- nb2listw(vecinos, style = "W", zero.policy = TRUE)
cat("¡Matriz W construida con éxito!\n")

# ==============================================================================
# 6. ESTIMACIÓN DEL MODELO SAR (MODELO 1)
# ==============================================================================

# 6.1 Modelo 1: Modelo con todas las variables (Robusto)
f_mod1 <- ipm_logit ~ tasa_matriculacion_5_16 + desercion + infraestructura_basica + 
  mortalidad_infantil_1 + log_viirs + aseguramiento_capado + 
  Rec_ICA_PC + ICEE_rural + rmm42

mod_sar_1 <- lagsarlm(formula = f_mod1, data = base_analitica_2018, listw = W_municipal, zero.policy = TRUE)

summary(mod_sar_1)

# 6.2 Modelo 2: Depuración de no significativas (p > 0.05).
#     Se mantiene aseguramiento para revisar si sigue siendo no significativa
f_mod2 <- ipm_logit ~ tasa_matriculacion_5_16 + infraestructura_basica + 
  mortalidad_infantil_1 + log_viirs + aseguramiento_capado + 
  ICEE_rural + rmm42

mod_sar_2 <- lagsarlm(formula = f_mod2, data = base_analitica_2018, listw = W_municipal, zero.policy = TRUE)

summary(mod_sar_2)


# 6.3 Modelo 3: Como variable de aseguramiento sigue sin ser significativa (p > 0.05), se 
#     prueba con la covariable de % poblacion subsidiada que sí es significativa
f_mod3 <- ipm_logit ~ tasa_matriculacion_5_16 + infraestructura_basica + 
  mortalidad_infantil_1 + log_viirs + pct_subsidiado + 
  ICEE_rural + rmm42

mod_sar_3 <- lagsarlm(formula = f_mod3, data = base_analitica_2018, listw = W_municipal, zero.policy = TRUE)

summary(mod_sar_3)

# ==============================================================================
# MODELO OPTIMO: VALIDACION DE SUPUESTOS PARA UN MODELO SAR
# ==============================================================================
library(spdep)
library(lmtest)
library(car)
library(spatialreg)

# 1. Extracción de residuos estandarizados
residuos_sar3 <- residuals(mod_sar_3)

# ----------------------------------------------------------------------
# SUPUESTO 1: Ausencia de Autocorrelación Espacial Residual (I de Moran)
# ----------------------------------------------------------------------
moran_residuos <- moran.mc(residuos_sar3, listw = W_municipal, nsim = 999, zero.policy = TRUE)
print(moran_residuos)

# ----------------------------------------------------------------------
# SUPUESTO 2: Ausencia de Multicolinealidad (VIF)
# (Evaluado sobre la estructura lineal base)
# ----------------------------------------------------------------------
mod_mco_base <- lm(f_mod3, data = base_analitica_2018)
vif_valores <- vif(mod_mco_base)
print(vif_valores)

# ----------------------------------------------------------------------
# SUPUESTO 3: Normalidad de los Residuales
# ----------------------------------------------------------------------
# Grafico Q-Q de diagnostico visual
qqnorm(residuos_sar3, main = "Q-Q Plot Residuales Modelo SAR 3", col = "navy")
qqline(residuos_sar3, col = "red", lwd = 2)

# Prueba de Shapiro-Wilk / Jarque-Bera
shapiro.test(residuos_sar3)

# ----------------------------------------------------------------------
# SUPUESTO 4: Homocedasticidad Residual
# ----------------------------------------------------------------------
# Grafico de Residuales vs Ajustados
valores_ajustados <- fitted(mod_sar_3)

plot(valores_ajustados, residuos_sar3, 
     main = "Residuales vs Valores Ajustados",
     xlab = "Valores Ajustados (ipm_logit)", 
     ylab = "Residuales",
     col = alpha("black", 0.3), pch = 20)
abline(h = 0, col = "red", lty = 2)
# ==============================================================================
# SE USA POR TANTO EL MODELO 3 CON ERRORES ROBUSTOS POR HETEROCEDASTICIDAD
# ==============================================================================
library(spatialreg)
library(lmtest)
library(sandwich)
library(stargazer)

# 1. Extracción de Errores Estándar Robustos
# Se extrae la diagonal de la matriz de varianza-covarianza con rescale = TRUE
vcov_robust_mod3 <- vcov(mod_sar_3, rescale = TRUE)
se_robustos_mod3 <- sqrt(diag(vcov_robust_mod3))

# Eliminar el error estándar correspondiente a Rho para dejar solo los de las covariables + intercepto
se_covar_robust <- se_robustos_mod3[names(coef(mod_sar_3))]

# 2. Test de Coeficientes con Errores Robustos
res_robusto_mod3 <- coeftest(mod_sar_3, vcov. = vcov_robust_mod3)
print("--- MODELO 3 CON ERRORES ESTÁNDAR ROBUSTOS ---")
print(res_robusto_mod3)

# ==============================================================================
# 7. Desplegar resultados para interpretación de la tesis
#===============================================================================
library(stargazer)

# Comparación rápida de AIC y Log-Likelihood
AIC(mod_sar_1, mod_sar_2, mod_sar_3)
BIC(mod_sar_1, mod_sar_2, mod_sar_3)

# Comparar con RMSE (menor es mejor)
rmse <- function(modelo) {
  sqrt(mean(residuals(modelo)^2))
}
rmse(mod_sar_1)
rmse(mod_sar_2)
rmse(mod_sar_3)

# Extraer los rhos de los modelos
rho_1 <- mod_sar_1$rho
rho_2 <- mod_sar_2$rho
rho_3 <- mod_sar_3$rho

# Calcular el BIC de cada modelo
bic_m1 <- round(BIC(mod_sar_1), 2)
bic_m2 <- round(BIC(mod_sar_2), 2)
bic_m3 <- round(BIC(mod_sar_3), 2)


# 4. Tabla comparativa de Modelos
stargazer(
  mod_sar_1, mod_sar_2, mod_sar_3,
  type = "text",
  title = "Comparación de Estimaciones: Errores Convencionales vs. Robustos",
  column.labels = c("Modelo 1 (Robusto)", "Modelo 2 (Clásico)", "Modelo 3 (Robusto)"),
  show.aic = TRUE, # Muestra el AIC predeterminado
  add.lines = list(
    c("Parámetro Espacial (rho)", round(mod_sar_1$rho, 4), round(mod_sar_2$rho, 4), round(mod_sar_3$rho, 4)),
    c("BIC", bic_m1, bic_m2, bic_m3), # <--- Fila agregada para el BIC
    c("Corrección Heterocedasticidad", "No evaluada", "No", "Sí (Robust SE)")
  )
)

# ==============================================================================
# CÁLCULO DE IMPACTOS DIRECTOS, INDIRECTOS Y TOTALES (MODELO 3)
# ==============================================================================
cat("\n--- CÁLCULO DE EFECTOS MARGINALES (IMPACTS) ---\n")
impactos_sar3 <- impacts(mod_sar_3, listw = W_municipal, R = 500)

# Resumen con pruebas de significancia (z-stats)
summary(impactos_sar3, zstats = TRUE, short = TRUE)

# ==============================================================================
# 8. EXTRACCIÓN DE PARÁMETROS CLAVE PARA LA FASE 2
# ==============================================================================
# Guardamos los coeficientes Betas y el parámetro Rho (dependencia espacial)
coeficientes_beta <- coef(mod_sar_3)
rho_estimado <- mod_sar_3$rho

cat("\n--- PARÁMETROS ESTRUCTURALES GUARDADOS ---\n")
cat("Rho (Autocorrelación Espacial):", rho_estimado, "\n")  ## Valor considerablemente alto 
                                                            ## que indica correlacion espacial
