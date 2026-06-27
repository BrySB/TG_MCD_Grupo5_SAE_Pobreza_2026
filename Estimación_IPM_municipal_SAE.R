# =========================================================
# TESIS DE MAESTRÍA
# Estimación del IPM municipal mediante SAE
# Script 1: Preparación de datos y calibración departamental
# Autor: Erick Caicedo Ruiz
# =========================================================

rm(list = ls())

options(stringsAsFactors = FALSE, scipen = 999)

# =========================================================
# 0. PAQUETES
# =========================================================
# Instalar solo una vez si hace falta:
# install.packages("openxlsx")
# install.packages("sae")

library(openxlsx)
library(sae)

# =========================================================
# 1. RUTAS DE ENTRADA Y SALIDA
# =========================================================

ruta_ipm <- "C:/Users/Erick Caicedo Ruiz/Desktop/Maestría Ciencia de Datos/Trabajo de Grado/IPM_dptos/IPM_dptos/IPM_Depto_2024.xlsx"

ruta_cov <- "C:/Users/Erick Caicedo Ruiz/Desktop/Maestría Ciencia de Datos/Trabajo de Grado/BD Auxiliares/X_salud_edu_vivienda2024.xlsx"

carpeta_salida <- "C:/Users/Erick Caicedo Ruiz/Desktop/Maestría Ciencia de Datos/Trabajo de Grado/BD Auxiliares"

# Validación de existencia de archivos
if (!file.exists(ruta_ipm)) {
  stop("No se encontró el archivo del IPM en la ruta especificada.")
}

if (!file.exists(ruta_cov)) {
  stop("No se encontró el archivo de covariables en la ruta especificada.")
}

if (!dir.exists(carpeta_salida)) {
  stop("No existe la carpeta de salida especificada.")
}

# =========================================================
# 2. LECTURA DE BASES DE DATOS
# =========================================================

ipm <- openxlsx::read.xlsx(ruta_ipm)
x_mun <- openxlsx::read.xlsx(ruta_cov, sheet = "BD")

cat("Base IPM leída correctamente.\n")
cat("Filas IPM:", nrow(ipm), "\n")
cat("Columnas IPM:", ncol(ipm), "\n\n")

cat("Base de covariables leída correctamente.\n")
cat("Filas covariables:", nrow(x_mun), "\n")
cat("Columnas covariables:", ncol(x_mun), "\n\n")

# =========================================================
# 3. PREPARACIÓN DE LA BASE IPM
# =========================================================
# Esta base contiene el IPM directo 2024 a nivel departamental,
# junto con la varianza del estimador directo (vardir).

ipm_2024 <- ipm

# Renombrar variable del nombre del departamento
names(ipm_2024)[names(ipm_2024) == "Departamento"] <- "departamento"

# Asegurar tipo numérico para el código DANE departamental
ipm_2024$cod_depto <- as.numeric(ipm_2024$cod_depto)

# Revisión básica
cat("Resumen base IPM 2024:\n")
print(str(ipm_2024))
cat("\n")

# =========================================================
# 4. PREPARACIÓN DE LA BASE MUNICIPAL DE COVARIABLES
# =========================================================
# Se estandarizan nombres de variables para mantener consistencia.

names(x_mun)[names(x_mun) == "Cobertura_acueducto"] <- "cobertura_acueducto"
names(x_mun)[names(x_mun) == "Cobertura_alcantarillado"] <- "cobertura_alcantarillado"
names(x_mun)[names(x_mun) == "Cobertura_aseo"] <- "cobertura_aseo"
names(x_mun)[names(x_mun) == "Cobertura_energia_rural"] <- "cobertura_energia_rural"

# Asegurar tipos numéricos de las llaves territoriales
x_mun$cod_depto <- as.numeric(x_mun$cod_depto)
x_mun$cod_mpio  <- as.numeric(x_mun$cod_mpio)

cat("Resumen base municipal de covariables:\n")
print(str(x_mun))
cat("\n")

# =========================================================
# 5. DEFINICIÓN DE COVARIABLES AUXILIARES
# =========================================================
# Se incluyen variables de salud, educación e infraestructura.

vars_x <- c(
  "pct_subsidiado_2024",
  "pct_cont_2024",
  "aseguramiento_capado_2024",
  "ratio_subs_cont_2024",
  "log_total_afiliados_2024",
  "tasa_matriculacion_5_16",
  "cobertura_neta",
  "cobertura_neta_media",
  "desercion",
  "repitencia",
  "reprobacion",
  "cobertura_acueducto",
  "cobertura_alcantarillado",
  "cobertura_aseo",
  "infraestructura_basica",
  "cobertura_energia_rural"
)

faltan_vars <- vars_x[!vars_x %in% names(x_mun)]

if (length(faltan_vars) > 0) {
  stop(paste("Faltan las siguientes variables en x_mun:", paste(faltan_vars, collapse = ", ")))
}

# =========================================================
# 6. AGREGACIÓN DE COVARIABLES MUNICIPALES A NIVEL DEPARTAMENTAL
# =========================================================
# Se emplea el promedio simple por departamento para construir
# la matriz auxiliar en la escala de calibración del modelo SAE.

x_depto <- aggregate(
  x = x_mun[, vars_x],
  by = list(cod_depto = x_mun$cod_depto),
  FUN = function(z) {
    if (all(is.na(z))) {
      return(NA)
    } else {
      return(mean(z, na.rm = TRUE))
    }
  }
)

# Agregar nombre del departamento
depto_nombre <- unique(x_mun[, c("cod_depto", "depto")])
depto_nombre <- depto_nombre[!duplicated(depto_nombre$cod_depto), ]

x_depto <- merge(
  x_depto,
  depto_nombre,
  by = "cod_depto",
  all.x = TRUE
)

names(x_depto)[names(x_depto) == "depto"] <- "departamento"

# Número de municipios por departamento
n_mpios <- aggregate(
  x = x_mun$cod_mpio,
  by = list(cod_depto = x_mun$cod_depto),
  FUN = length
)

names(n_mpios)[2] <- "n_municipios"

x_depto <- merge(
  x_depto,
  n_mpios,
  by = "cod_depto",
  all.x = TRUE
)

# Reordenar columnas
x_depto <- x_depto[, c("cod_depto", "departamento", vars_x, "n_municipios")]

cat("Base agregada a nivel departamental construida.\n")
cat("Filas x_depto:", nrow(x_depto), "\n")
cat("Columnas x_depto:", ncol(x_depto), "\n\n")

# =========================================================
# 7. CONSTRUCCIÓN DE LA BASE ANALÍTICA SAE
# =========================================================
# La unión se realiza por cod_depto para evitar errores de nomenclatura.

base_sae <- merge(
  ipm_2024,
  x_depto,
  by = "cod_depto",
  all.x = TRUE
)

# Resolver posibles duplicaciones del nombre del departamento
if ("departamento.x" %in% names(base_sae)) {
  names(base_sae)[names(base_sae) == "departamento.x"] <- "departamento"
}
if ("departamento.y" %in% names(base_sae)) {
  base_sae$departamento.y <- NULL
}

# Ordenar columnas
base_sae <- base_sae[, c(
  "cod_depto",
  "departamento",
  "Año",
  "Area",
  "Variable",
  "ipm_directo",
  "vardir",
  "LATITUD",
  "LONGITUD",
  "pct_subsidiado_2024",
  "pct_cont_2024",
  "aseguramiento_capado_2024",
  "ratio_subs_cont_2024",
  "log_total_afiliados_2024",
  "tasa_matriculacion_5_16",
  "cobertura_neta",
  "cobertura_neta_media",
  "desercion",
  "repitencia",
  "reprobacion",
  "cobertura_acueducto",
  "cobertura_alcantarillado",
  "cobertura_aseo",
  "infraestructura_basica",
  "cobertura_energia_rural",
  "n_municipios"
)]

cat("Base analítica final construida: base_sae\n")
cat("Filas base_sae:", nrow(base_sae), "\n")
cat("Columnas base_sae:", ncol(base_sae), "\n\n")

cat("Valores faltantes por variable:\n")
print(colSums(is.na(base_sae)))
cat("\n")

# Guardar base analítica
write.csv(
  base_sae,
  file.path(carpeta_salida, "base_sae_2024.csv"),
  row.names = FALSE
)

# =========================================================
# 8. ANÁLISIS EXPLORATORIO: CORRELACIONES
# =========================================================

vars_numericas <- c(
  "ipm_directo",
  "vardir",
  "pct_subsidiado_2024",
  "pct_cont_2024",
  "aseguramiento_capado_2024",
  "ratio_subs_cont_2024",
  "log_total_afiliados_2024",
  "tasa_matriculacion_5_16",
  "cobertura_neta",
  "cobertura_neta_media",
  "desercion",
  "repitencia",
  "reprobacion",
  "cobertura_acueducto",
  "cobertura_alcantarillado",
  "cobertura_aseo",
  "infraestructura_basica",
  "cobertura_energia_rural",
  "n_municipios"
)

mat_cor <- cor(base_sae[, vars_numericas], use = "pairwise.complete.obs")

cor_ipm <- data.frame(
  variable = rownames(mat_cor),
  correlacion_con_ipm = mat_cor[, "ipm_directo"]
)

cor_ipm <- cor_ipm[cor_ipm$variable != "ipm_directo", ]
cor_ipm <- cor_ipm[order(-abs(cor_ipm$correlacion_con_ipm)), ]
rownames(cor_ipm) <- NULL

cat("Correlaciones con el IPM directo:\n")
print(cor_ipm)
cat("\n")

write.csv(
  cor_ipm,
  file.path(carpeta_salida, "correlaciones_ipm_2024.csv"),
  row.names = FALSE
)

# =========================================================
# 9. MODELOS OLS EXPLORATORIOS
# =========================================================
# Estos modelos no son el estimador final, sino una etapa de apoyo
# para revisar signos, estabilidad y pertinencia de covariables.

ajustar_modelo <- function(vars, nombre_modelo, base = base_sae) {
  
  datos <- na.omit(base[, c("ipm_directo", "vardir", vars)])
  formula_modelo <- as.formula(
    paste("ipm_directo ~", paste(vars, collapse = " + "))
  )
  
  modelo <- lm(formula_modelo, data = datos)
  
  resultado <- list(
    nombre = nombre_modelo,
    vars = vars,
    base = datos,
    formula = formula_modelo,
    modelo = modelo,
    resumen = summary(modelo)
  )
  
  return(resultado)
}

mod_1 <- ajustar_modelo(
  vars = c("repitencia", "cobertura_neta_media", "ratio_subs_cont_2024", "cobertura_energia_rural"),
  nombre_modelo = "modelo_1"
)

mod_2 <- ajustar_modelo(
  vars = c("repitencia", "cobertura_neta_media", "ratio_subs_cont_2024"),
  nombre_modelo = "modelo_2"
)

mod_3 <- ajustar_modelo(
  vars = c("repitencia", "aseguramiento_capado_2024", "infraestructura_basica"),
  nombre_modelo = "modelo_3"
)

mod_4 <- ajustar_modelo(
  vars = c("repitencia", "infraestructura_basica"),
  nombre_modelo = "modelo_4"
)

mod_A <- ajustar_modelo(
  vars = c("repitencia", "ratio_subs_cont_2024", "infraestructura_basica", "cobertura_energia_rural"),
  nombre_modelo = "modelo_A"
)

mod_B <- ajustar_modelo(
  vars = c("repitencia", "cobertura_neta_media", "infraestructura_basica", "aseguramiento_capado_2024"),
  nombre_modelo = "modelo_B"
)

mod_salud <- ajustar_modelo(
  vars = c("repitencia", "infraestructura_basica", "pct_cont_2024"),
  nombre_modelo = "modelo_salud"
)

# Mostrar resúmenes
print(mod_1$resumen)
print(mod_2$resumen)
print(mod_3$resumen)
print(mod_4$resumen)
print(mod_A$resumen)
print(mod_B$resumen)
print(mod_salud$resumen)

# =========================================================
# 10. COMPARACIÓN DE MODELOS OLS
# =========================================================

comparacion_modelos <- data.frame(
  modelo = c("modelo_1", "modelo_2", "modelo_3", "modelo_4", "modelo_A", "modelo_B", "modelo_salud"),
  n = c(
    nrow(mod_1$base),
    nrow(mod_2$base),
    nrow(mod_3$base),
    nrow(mod_4$base),
    nrow(mod_A$base),
    nrow(mod_B$base),
    nrow(mod_salud$base)
  ),
  r2 = c(
    summary(mod_1$modelo)$r.squared,
    summary(mod_2$modelo)$r.squared,
    summary(mod_3$modelo)$r.squared,
    summary(mod_4$modelo)$r.squared,
    summary(mod_A$modelo)$r.squared,
    summary(mod_B$modelo)$r.squared,
    summary(mod_salud$modelo)$r.squared
  ),
  r2_ajustado = c(
    summary(mod_1$modelo)$adj.r.squared,
    summary(mod_2$modelo)$adj.r.squared,
    summary(mod_3$modelo)$adj.r.squared,
    summary(mod_4$modelo)$adj.r.squared,
    summary(mod_A$modelo)$adj.r.squared,
    summary(mod_B$modelo)$adj.r.squared,
    summary(mod_salud$modelo)$adj.r.squared
  ),
  AIC = c(
    AIC(mod_1$modelo),
    AIC(mod_2$modelo),
    AIC(mod_3$modelo),
    AIC(mod_4$modelo),
    AIC(mod_A$modelo),
    AIC(mod_B$modelo),
    AIC(mod_salud$modelo)
  ),
  BIC = c(
    BIC(mod_1$modelo),
    BIC(mod_2$modelo),
    BIC(mod_3$modelo),
    BIC(mod_4$modelo),
    BIC(mod_A$modelo),
    BIC(mod_B$modelo),
    BIC(mod_salud$modelo)
  )
)

comparacion_modelos <- comparacion_modelos[order(-comparacion_modelos$r2_ajustado), ]
rownames(comparacion_modelos) <- NULL

cat("Comparación de modelos OLS exploratorios:\n")
print(comparacion_modelos)
cat("\n")

write.csv(
  comparacion_modelos,
  file.path(carpeta_salida, "comparacion_modelos_ols.csv"),
  row.names = FALSE
)

# =========================================================
# 11. DEFINICIÓN DE ESPECIFICACIONES CANDIDATAS PARA SAE
# =========================================================
# Modelo principal: 3 covariables (educación, infraestructura, salud)
# Modelo sensibilidad: 2 covariables (educación, infraestructura)

base_fh_principal <- na.omit(base_sae[, c(
  "cod_depto",
  "departamento",
  "ipm_directo",
  "vardir",
  "repitencia",
  "infraestructura_basica",
  "pct_cont_2024"
)])

base_fh_sens <- na.omit(base_sae[, c(
  "cod_depto",
  "departamento",
  "ipm_directo",
  "vardir",
  "repitencia",
  "infraestructura_basica"
)])

cat("Filas base_fh_principal:", nrow(base_fh_principal), "\n")
cat("Filas base_fh_sens:", nrow(base_fh_sens), "\n\n")

write.csv(
  base_fh_principal,
  file.path(carpeta_salida, "base_fh_principal.csv"),
  row.names = FALSE
)

write.csv(
  base_fh_sens,
  file.path(carpeta_salida, "base_fh_sens.csv"),
  row.names = FALSE
)

# =========================================================
# 12. AJUSTE DEL MODELO FAY-HERRIOT
# =========================================================

modelo_fh_principal <- eblupFH(
  formula = ipm_directo ~ repitencia + infraestructura_basica + pct_cont_2024,
  vardir = vardir,
  method = "REML",
  data = base_fh_principal
)

modelo_fh_sens <- eblupFH(
  formula = ipm_directo ~ repitencia + infraestructura_basica,
  vardir = vardir,
  method = "REML",
  data = base_fh_sens
)

cat("Resumen modelo Fay-Herriot principal:\n")
print(modelo_fh_principal)
cat("\n")

cat("Resumen modelo Fay-Herriot sensibilidad:\n")
print(modelo_fh_sens)
cat("\n")

# =========================================================
# 13. COMPARACIÓN DE ESTIMACIONES FH VS DIRECTAS
# =========================================================

resultados_fh <- data.frame(
  cod_depto = base_fh_principal$cod_depto,
  departamento = base_fh_principal$departamento,
  ipm_directo = base_fh_principal$ipm_directo,
  vardir = base_fh_principal$vardir,
  fh_principal = as.numeric(modelo_fh_principal$eblup),
  fh_sens = as.numeric(modelo_fh_sens$eblup)
)

resultados_fh$dif_principal_directo <- resultados_fh$fh_principal - resultados_fh$ipm_directo
resultados_fh$dif_sens_directo <- resultados_fh$fh_sens - resultados_fh$ipm_directo
resultados_fh$dif_entre_modelos <- resultados_fh$fh_principal - resultados_fh$fh_sens

cat("Resumen diferencias FH vs directas:\n")
print(summary(resultados_fh$dif_principal_directo))
print(summary(resultados_fh$dif_sens_directo))
print(summary(resultados_fh$dif_entre_modelos))
cat("\n")

write.csv(
  resultados_fh,
  file.path(carpeta_salida, "resultados_fh_comparados.csv"),
  row.names = FALSE
)


cat("Proceso completado correctamente.\n")