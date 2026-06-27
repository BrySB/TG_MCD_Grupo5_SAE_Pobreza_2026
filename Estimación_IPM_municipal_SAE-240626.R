# =========================================================
# TESIS DE MAESTRÍA
# Estimación del IPM municipal mediante SAE
# Actividad 3: Modelamiento, comparación y resultados
# Autor: Erick Caicedo Ruiz
# =========================================================

rm(list = ls())
options(stringsAsFactors = FALSE, scipen = 999)

# =========================================================
# 0. PAQUETES
# =========================================================
# Esta versión evita el error: objeto 'pkg' no encontrado.
# Solo crea un vector de paquetes, instala los faltantes y luego los carga.

paquetes <- c(
  "openxlsx",
  "sae",
  "dplyr",
  "ggplot2",
  "car",
  "lmtest",
  "sf",
  "spdep",
  "spatialreg"
)

paquetes_faltantes <- paquetes[!(paquetes %in% rownames(installed.packages()))]

if (length(paquetes_faltantes) > 0) {
  install.packages(paquetes_faltantes, dependencies = TRUE)
}

library(openxlsx)
library(sae)
library(dplyr)
library(ggplot2)
library(car)
library(lmtest)
library(sf)
library(spdep)
library(spatialreg)

# =========================================================
# 1. RUTAS DE ENTRADA Y SALIDA
# =========================================================

ruta_ipm <- "C:/Users/Erick Caicedo Ruiz/Desktop/Maestría Ciencia de Datos/Trabajo de Grado/IPM_dptos/IPM_dptos/IPM_Depto_2024.xlsx"

ruta_cov <- "C:/Users/Erick Caicedo Ruiz/Desktop/Maestría Ciencia de Datos/Trabajo de Grado/BD Auxiliares/X_salud_edu_vivienda2024.xlsx"

carpeta_salida <- "C:/Users/Erick Caicedo Ruiz/Desktop/Maestría Ciencia de Datos/Trabajo de Grado/BD Auxiliares"

ruta_shp_dptos <- "C:/Users/Erick Caicedo Ruiz/Desktop/Maestría Ciencia de Datos/Trabajo de Grado/SHP_MGN2018_INTGRD_DEPTO/MGN_ANM_DPTOS.shp"

carpeta_graficos <- file.path(carpeta_salida, "graficos_resultados")

if (!dir.exists(carpeta_graficos)) {
  dir.create(carpeta_graficos, recursive = TRUE)
}

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
# 2. FUNCIONES AUXILIARES
# =========================================================

limpiar_nombres <- function(nombres) {
  nombres <- iconv(nombres, from = "", to = "ASCII//TRANSLIT")
  nombres <- tolower(nombres)
  nombres <- gsub("%", "pct", nombres)
  nombres <- gsub("[^a-z0-9]+", "_", nombres)
  nombres <- gsub("^_|_$", "", nombres)
  nombres <- make.unique(nombres, sep = "_")
  return(nombres)
}

asegurar_numerico <- function(x) {
  if (is.numeric(x)) return(x)
  x <- as.character(x)
  x <- trimws(x)
  x <- gsub("%", "", x)
  x <- gsub(" ", "", x)

  # Si tiene punto y coma, se asume formato latino: 1.234,56
  tiene_punto <- grepl("\\.", x)
  tiene_coma <- grepl(",", x)

  x[tiene_punto & tiene_coma] <- gsub("\\.", "", x[tiene_punto & tiene_coma])
  x[tiene_punto & tiene_coma] <- gsub(",", ".", x[tiene_punto & tiene_coma])

  # Si solo tiene coma, se asume coma decimal: 12,5
  x[!tiene_punto & tiene_coma] <- gsub(",", ".", x[!tiene_punto & tiene_coma])

  x[x %in% c("", "NA", "NaN", "NULL", "null", "-")] <- NA
  return(as.numeric(x))
}

rmse <- function(obs, pred) {
  sqrt(mean((obs - pred)^2, na.rm = TRUE))
}

mae <- function(obs, pred) {
  mean(abs(obs - pred), na.rm = TRUE)
}

calcular_vif_max <- function(modelo) {
  salida <- tryCatch({
    v <- car::vif(modelo)
    max(as.numeric(v), na.rm = TRUE)
  }, error = function(e) NA_real_)
  return(salida)
}

calcular_bp_p <- function(modelo) {
  salida <- tryCatch({
    lmtest::bptest(modelo)$p.value
  }, error = function(e) NA_real_)
  return(salida)
}

calcular_shapiro_p <- function(modelo) {
  salida <- tryCatch({
    res <- residuals(modelo)
    if (length(res) >= 3 && length(res) <= 5000) {
      shapiro.test(res)$p.value
    } else {
      NA_real_
    }
  }, error = function(e) NA_real_)
  return(salida)
}

extraer_coef_fh <- function(modelo_fh) {
  estcoef <- modelo_fh$fit$estcoef

  if ("beta" %in% colnames(estcoef)) {
    beta <- estcoef[, "beta"]
  } else {
    beta <- estcoef[, 1]
  }

  beta <- setNames(as.numeric(beta), rownames(estcoef))
  return(beta)
}

extraer_mse_fh <- function(obj_mse) {
  if (!is.null(obj_mse$mse)) {
    return(as.numeric(obj_mse$mse))
  }

  if (!is.null(obj_mse$est$mse)) {
    return(as.numeric(obj_mse$est$mse))
  }

  if (!is.null(obj_mse$MSE)) {
    return(as.numeric(obj_mse$MSE))
  }

  stop("No se encontró el vector MSE dentro del objeto mseFH.")
}

predecir_fh_municipal <- function(modelo_fh, base_fh, x_mun, vars_modelo, nombre_pred) {

  formula_fija <- as.formula(
    paste("~", paste(vars_modelo, collapse = " + "))
  )

  beta <- extraer_coef_fh(modelo_fh)

  base_mun <- x_mun[, c("cod_depto", "depto", "cod_mpio", "mpio", vars_modelo)]
  base_mun <- na.omit(base_mun)

  X_mun <- model.matrix(formula_fija, data = base_mun)
  beta_ordenado <- beta[colnames(X_mun)]

  base_mun[[paste0(nombre_pred, "_fijo")]] <- as.numeric(X_mun %*% beta_ordenado)

  X_depto <- model.matrix(formula_fija, data = base_fh)
  beta_depto <- beta[colnames(X_depto)]

  parte_fija_depto <- as.numeric(X_depto %*% beta_depto)
  u_hat_depto <- as.numeric(modelo_fh$eblup) - parte_fija_depto

  efectos_depto <- data.frame(
    cod_depto = base_fh$cod_depto,
    u_hat_depto = u_hat_depto
  )

  base_mun <- merge(
    base_mun,
    efectos_depto,
    by = "cod_depto",
    all.x = TRUE
  )

  base_mun[[paste0(nombre_pred, "_calibrado")]] <-
    base_mun[[paste0(nombre_pred, "_fijo")]] + base_mun$u_hat_depto

  base_mun[[paste0(nombre_pred, "_calibrado_acotado")]] <- pmax(
    0,
    pmin(100, base_mun[[paste0(nombre_pred, "_calibrado")]])
  )

  base_mun[[paste0(nombre_pred, "_fijo_acotado")]] <- pmax(
    0,
    pmin(100, base_mun[[paste0(nombre_pred, "_fijo")]])
  )

  return(base_mun)
}

# =========================================================
# 3. LECTURA DE BASES
# =========================================================

ipm <- openxlsx::read.xlsx(ruta_ipm)
x_mun <- openxlsx::read.xlsx(ruta_cov, sheet = "BD")

names(ipm) <- limpiar_nombres(names(ipm))
names(x_mun) <- limpiar_nombres(names(x_mun))

cat("Base IPM leída correctamente.\n")
cat("Filas IPM:", nrow(ipm), "\n")
cat("Columnas IPM:", ncol(ipm), "\n\n")

cat("Base de covariables leída correctamente.\n")
cat("Filas covariables:", nrow(x_mun), "\n")
cat("Columnas covariables:", ncol(x_mun), "\n\n")

# =========================================================
# 4. PREPARACIÓN DE LA BASE IPM
# =========================================================

if ("codigo" %in% names(ipm) && !("cod_depto" %in% names(ipm))) {
  names(ipm)[names(ipm) == "codigo"] <- "cod_depto"
}

if ("estimacion" %in% names(ipm) && !("ipm_directo" %in% names(ipm))) {
  names(ipm)[names(ipm) == "estimacion"] <- "ipm_directo"
}

if ("error_estandar" %in% names(ipm) && !("vardir" %in% names(ipm))) {
  ipm$vardir <- asegurar_numerico(ipm$error_estandar)^2
}

if (!("cod_depto" %in% names(ipm))) {
  stop("La base IPM debe tener una columna cod_depto.")
}

if (!("ipm_directo" %in% names(ipm))) {
  stop("La base IPM debe tener una columna ipm_directo o estimacion.")
}

if (!("vardir" %in% names(ipm))) {
  stop("La base IPM debe tener vardir o error_estandar para calcular vardir.")
}

if (!("departamento" %in% names(ipm))) {
  stop("La base IPM debe tener una columna departamento.")
}

ipm$cod_depto <- as.numeric(ipm$cod_depto)
ipm$ipm_directo <- asegurar_numerico(ipm$ipm_directo)
ipm$vardir <- asegurar_numerico(ipm$vardir)

ipm_2024 <- ipm

cat("Resumen base IPM 2024:\n")
print(str(ipm_2024))
cat("\n")

# =========================================================
# 5. PREPARACIÓN DE LA BASE MUNICIPAL DE COVARIABLES
# =========================================================

x_mun$cod_depto <- as.numeric(x_mun$cod_depto)
x_mun$cod_mpio  <- as.numeric(x_mun$cod_mpio)

vars_posibles_numericas <- c(
  "pct_subsidiado_2024",
  "pct_cont_2024",
  "aseguramiento_capado_2024",
  "ratio_subs_cont_2024",
  "log_total_afiliados_2024",
  "mortalidad_infantil",
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

vars_posibles_numericas <- vars_posibles_numericas[
  vars_posibles_numericas %in% names(x_mun)
]

for (v in vars_posibles_numericas) {
  x_mun[[v]] <- asegurar_numerico(x_mun[[v]])
}

if ("mortalidad_infantil" %in% names(x_mun)) {
  x_mun$log_mortalidad_infantil <- log1p(x_mun$mortalidad_infantil)
}

cat("Resumen base municipal de covariables:\n")
print(str(x_mun))
cat("\n")

# =========================================================
# 6. DEFINICIÓN DE COVARIABLES AUXILIARES
# =========================================================

vars_x_candidatas <- c(
  "pct_subsidiado_2024",
  "pct_cont_2024",
  "aseguramiento_capado_2024",
  "ratio_subs_cont_2024",
  "log_total_afiliados_2024",
  "mortalidad_infantil",
  "log_mortalidad_infantil",
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

vars_x <- vars_x_candidatas[vars_x_candidatas %in% names(x_mun)]

faltan_vars <- vars_x_candidatas[!(vars_x_candidatas %in% names(x_mun))]

if (length(faltan_vars) > 0) {
  cat("Advertencia: estas variables no están en la base municipal y no serán usadas:\n")
  print(faltan_vars)
  cat("\n")
}

vars_obligatorias_fh <- c(
  "repitencia",
  "infraestructura_basica",
  "mortalidad_infantil",
  "log_mortalidad_infantil"
)

faltan_obligatorias <- vars_obligatorias_fh[
  !(vars_obligatorias_fh %in% names(x_mun))
]

if (length(faltan_obligatorias) > 0) {
  stop(paste(
    "Faltan variables necesarias para los modelos FH:",
    paste(faltan_obligatorias, collapse = ", ")
  ))
}

# =========================================================
# 7. AGREGACIÓN MUNICIPAL A NIVEL DEPARTAMENTAL
# =========================================================

x_depto <- aggregate(
  x = x_mun[, vars_x],
  by = list(cod_depto = x_mun$cod_depto),
  FUN = function(z) {
    if (all(is.na(z))) {
      return(NA_real_)
    } else {
      return(mean(z, na.rm = TRUE))
    }
  }
)

depto_nombre <- unique(x_mun[, c("cod_depto", "depto")])
depto_nombre <- depto_nombre[!duplicated(depto_nombre$cod_depto), ]

x_depto <- merge(
  x_depto,
  depto_nombre,
  by = "cod_depto",
  all.x = TRUE
)

names(x_depto)[names(x_depto) == "depto"] <- "departamento"

n_mpios <- aggregate(
  x = x_mun$cod_mpio,
  by = list(cod_depto = x_mun$cod_depto),
  FUN = function(z) length(unique(z))
)

names(n_mpios)[2] <- "n_municipios"

x_depto <- merge(
  x_depto,
  n_mpios,
  by = "cod_depto",
  all.x = TRUE
)

x_depto <- x_depto[, c("cod_depto", "departamento", vars_x, "n_municipios")]

cat("Base agregada a nivel departamental construida.\n")
cat("Filas x_depto:", nrow(x_depto), "\n")
cat("Columnas x_depto:", ncol(x_depto), "\n\n")

write.csv(
  x_depto,
  file.path(carpeta_salida, "x_depto_covariables_agregadas.csv"),
  row.names = FALSE
)

# =========================================================
# 8. CONSTRUCCIÓN DE LA BASE ANALÍTICA SAE
# =========================================================

base_sae <- merge(
  ipm_2024,
  x_depto,
  by = "cod_depto",
  all.x = TRUE
)

if ("departamento.x" %in% names(base_sae)) {
  names(base_sae)[names(base_sae) == "departamento.x"] <- "departamento"
}

if ("departamento.y" %in% names(base_sae)) {
  base_sae$departamento.y <- NULL
}

cols_base <- c(
  "cod_depto",
  "departamento",
  intersect(c("ano", "anio"), names(base_sae)),
  intersect(c("area"), names(base_sae)),
  intersect(c("variable"), names(base_sae)),
  "ipm_directo",
  "vardir",
  intersect(c("latitud", "longitud"), names(base_sae)),
  vars_x,
  "n_municipios"
)

base_sae <- base_sae[, unique(cols_base)]

cat("Base analítica final construida: base_sae\n")
cat("Filas base_sae:", nrow(base_sae), "\n")
cat("Columnas base_sae:", ncol(base_sae), "\n\n")

cat("Valores faltantes por variable:\n")
print(colSums(is.na(base_sae)))
cat("\n")

write.csv(
  base_sae,
  file.path(carpeta_salida, "base_sae_2024.csv"),
  row.names = FALSE
)

# =========================================================
# 9. ANÁLISIS EXPLORATORIO: CORRELACIONES
# =========================================================

vars_numericas <- c("ipm_directo", "vardir", vars_x, "n_municipios")
vars_numericas <- vars_numericas[vars_numericas %in% names(base_sae)]

correlaciones <- cor(
  base_sae[, vars_numericas],
  use = "pairwise.complete.obs"
)

write.csv(
  correlaciones,
  file.path(carpeta_salida, "matriz_correlaciones_base_sae.csv"),
  row.names = TRUE
)

cor_ipm <- data.frame(
  variable = rownames(correlaciones),
  correlacion_ipm = correlaciones[, "ipm_directo"]
)

cor_ipm <- cor_ipm[
  order(abs(cor_ipm$correlacion_ipm), decreasing = TRUE),
]

write.csv(
  cor_ipm,
  file.path(carpeta_salida, "correlaciones_con_ipm.csv"),
  row.names = FALSE
)

g_cor <- cor_ipm %>%
  filter(variable != "ipm_directo") %>%
  head(15) %>%
  ggplot(aes(x = reorder(variable, correlacion_ipm), y = correlacion_ipm)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Principales correlaciones con el IPM directo 2024",
    x = "Variable auxiliar",
    y = "Correlación con IPM"
  ) +
  theme_minimal()

ggsave(
  filename = file.path(carpeta_graficos, "01_correlaciones_ipm.png"),
  plot = g_cor,
  width = 10,
  height = 7
)

# =========================================================
# 10. MODELOS OLS EXPLORATORIOS
# =========================================================

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

especificaciones <- list(
  modelo_1 = c("repitencia", "cobertura_neta_media", "ratio_subs_cont_2024", "cobertura_energia_rural"),
  modelo_2 = c("repitencia", "cobertura_neta_media", "ratio_subs_cont_2024"),
  modelo_3 = c("repitencia", "aseguramiento_capado_2024", "infraestructura_basica"),
  modelo_4 = c("repitencia", "infraestructura_basica"),
  modelo_A = c("repitencia", "ratio_subs_cont_2024", "infraestructura_basica", "cobertura_energia_rural"),
  modelo_B = c("repitencia", "cobertura_neta_media", "infraestructura_basica", "aseguramiento_capado_2024"),
  modelo_salud = c("repitencia", "infraestructura_basica", "pct_cont_2024"),
  modelo_mortalidad = c("repitencia", "infraestructura_basica", "mortalidad_infantil"),
  modelo_log_mortalidad = c("repitencia", "infraestructura_basica", "log_mortalidad_infantil"),
  modelo_salud_mortalidad = c("repitencia", "infraestructura_basica", "pct_cont_2024", "log_mortalidad_infantil"),
  modelo_principal_ampliado = c("repitencia", "infraestructura_basica", "mortalidad_infantil", "pct_cont_2024"),
  modelo_sens_ampliado = c("repitencia", "infraestructura_basica", "log_mortalidad_infantil", "pct_cont_2024")
)

especificaciones_validas <- list()

for (nm in names(especificaciones)) {
  vars <- especificaciones[[nm]]
  if (all(vars %in% names(base_sae))) {
    especificaciones_validas[[nm]] <- vars
  } else {
    cat("Se omite", nm, "porque faltan variables:\n")
    print(vars[!(vars %in% names(base_sae))])
    cat("\n")
  }
}

modelos_ols <- list()

for (nm in names(especificaciones_validas)) {
  modelos_ols[[nm]] <- ajustar_modelo(
    vars = especificaciones_validas[[nm]],
    nombre_modelo = nm,
    base = base_sae
  )
}

for (nm in names(modelos_ols)) {
  cat("=========================================\n")
  cat("Resumen:", nm, "\n")
  print(modelos_ols[[nm]]$resumen)
  cat("\n")
}

# =========================================================
# 11. COMPARACIÓN DE MODELOS OLS
# =========================================================

comparacion_modelos <- do.call(
  rbind,
  lapply(names(modelos_ols), function(nm) {

    mod <- modelos_ols[[nm]]$modelo
    obs <- modelos_ols[[nm]]$base$ipm_directo
    pred <- fitted(mod)

    data.frame(
      modelo = nm,
      n = nrow(modelos_ols[[nm]]$base),
      variables = paste(modelos_ols[[nm]]$vars, collapse = " + "),
      r2 = summary(mod)$r.squared,
      r2_ajustado = summary(mod)$adj.r.squared,
      RMSE = rmse(obs, pred),
      MAE = mae(obs, pred),
      AIC = AIC(mod),
      BIC = BIC(mod),
      VIF_max = calcular_vif_max(mod),
      BP_p_value = calcular_bp_p(mod),
      Shapiro_p_value = calcular_shapiro_p(mod)
    )
  })
)

comparacion_modelos <- comparacion_modelos[
  order(-comparacion_modelos$r2_ajustado),
]

rownames(comparacion_modelos) <- NULL

cat("Comparación de modelos OLS exploratorios:\n")
print(comparacion_modelos)
cat("\n")

write.csv(
  comparacion_modelos,
  file.path(carpeta_salida, "comparacion_modelos_ols.csv"),
  row.names = FALSE
)

g_r2 <- comparacion_modelos %>%
  ggplot(aes(x = reorder(modelo, r2_ajustado), y = r2_ajustado)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Comparación de modelos OLS exploratorios",
    subtitle = "Ordenados por R² ajustado",
    x = "Modelo",
    y = "R² ajustado"
  ) +
  theme_minimal()

ggsave(
  filename = file.path(carpeta_graficos, "02_comparacion_modelos_r2_ajustado.png"),
  plot = g_r2,
  width = 10,
  height = 7
)

g_aic <- comparacion_modelos %>%
  ggplot(aes(x = reorder(modelo, AIC), y = AIC)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Comparación de modelos OLS exploratorios",
    subtitle = "Menor AIC indica mejor ajuste penalizado",
    x = "Modelo",
    y = "AIC"
  ) +
  theme_minimal()

ggsave(
  filename = file.path(carpeta_graficos, "03_comparacion_modelos_aic.png"),
  plot = g_aic,
  width = 10,
  height = 7
)

# =========================================================
# 12. ESPECIFICACIONES CANDIDATAS PARA FAY-HERRIOT
# =========================================================

base_fh_principal <- na.omit(base_sae[, c(
  "cod_depto",
  "departamento",
  "ipm_directo",
  "vardir",
  "repitencia",
  "infraestructura_basica",
  "mortalidad_infantil"
)])

base_fh_sens <- na.omit(base_sae[, c(
  "cod_depto",
  "departamento",
  "ipm_directo",
  "vardir",
  "repitencia",
  "infraestructura_basica",
  "log_mortalidad_infantil"
)])

base_fh_principal <- base_fh_principal[base_fh_principal$vardir > 0, ]
base_fh_sens <- base_fh_sens[base_fh_sens$vardir > 0, ]

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
# 13. AJUSTE DEL MODELO FAY-HERRIOT
# =========================================================

modelo_fh_principal <- eblupFH(
  formula = ipm_directo ~ repitencia + infraestructura_basica + mortalidad_infantil,
  vardir = vardir,
  method = "REML",
  data = base_fh_principal
)

modelo_fh_sens <- eblupFH(
  formula = ipm_directo ~ repitencia + infraestructura_basica + log_mortalidad_infantil,
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

mse_fh_principal <- mseFH(
  formula = ipm_directo ~ repitencia + infraestructura_basica + mortalidad_infantil,
  vardir = vardir,
  method = "REML",
  data = base_fh_principal
)

mse_fh_sens <- mseFH(
  formula = ipm_directo ~ repitencia + infraestructura_basica + log_mortalidad_infantil,
  vardir = vardir,
  method = "REML",
  data = base_fh_sens
)

mse_vec_principal <- extraer_mse_fh(mse_fh_principal)
mse_vec_sens <- extraer_mse_fh(mse_fh_sens)

cat("MSE modelo Fay-Herriot principal:\n")
print(mse_fh_principal)
cat("\n")

cat("MSE modelo Fay-Herriot sensibilidad:\n")
print(mse_fh_sens)
cat("\n")

# =========================================================
# 14. RESULTADOS FH: DIRECTO VS EBLUP
# =========================================================

res_fh_principal <- data.frame(
  cod_depto = base_fh_principal$cod_depto,
  departamento = base_fh_principal$departamento,
  ipm_directo = base_fh_principal$ipm_directo,
  vardir = base_fh_principal$vardir,
  fh_principal = as.numeric(modelo_fh_principal$eblup),
  mse_principal = mse_vec_principal
)

res_fh_principal$se_fh_principal <- sqrt(res_fh_principal$mse_principal)
res_fh_principal$dif_principal_directo <- res_fh_principal$fh_principal - res_fh_principal$ipm_directo

res_fh_sens <- data.frame(
  cod_depto = base_fh_sens$cod_depto,
  departamento = base_fh_sens$departamento,
  ipm_directo_sens = base_fh_sens$ipm_directo,
  fh_sens = as.numeric(modelo_fh_sens$eblup),
  mse_sens = mse_vec_sens
)

res_fh_sens$se_fh_sens <- sqrt(res_fh_sens$mse_sens)
res_fh_sens$dif_sens_directo <- res_fh_sens$fh_sens - res_fh_sens$ipm_directo_sens

resultados_fh <- merge(
  res_fh_principal,
  res_fh_sens[, c("cod_depto", "fh_sens", "mse_sens", "se_fh_sens", "dif_sens_directo")],
  by = "cod_depto",
  all = TRUE
)

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

g_directo_fh <- resultados_fh %>%
  ggplot(aes(x = ipm_directo, y = fh_principal)) +
  geom_point(size = 2.5, alpha = 0.8) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(
    title = "IPM directo vs estimación Fay-Herriot",
    subtitle = "Modelo principal",
    x = "IPM directo ECV 2024",
    y = "IPM Fay-Herriot"
  ) +
  theme_minimal()

ggsave(
  filename = file.path(carpeta_graficos, "04_directo_vs_fh_principal.png"),
  plot = g_directo_fh,
  width = 8,
  height = 7
)

g_dif <- resultados_fh %>%
  ggplot(aes(x = reorder(departamento, dif_principal_directo), y = dif_principal_directo)) +
  geom_col() +
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Diferencia entre Fay-Herriot y estimación directa",
    subtitle = "Valores positivos indican estimación FH mayor que la directa",
    x = "Departamento",
    y = "FH principal - IPM directo"
  ) +
  theme_minimal()

ggsave(
  filename = file.path(carpeta_graficos, "05_diferencias_fh_directo.png"),
  plot = g_dif,
  width = 10,
  height = 8
)

g_se <- resultados_fh %>%
  ggplot(aes(x = reorder(departamento, se_fh_principal), y = se_fh_principal)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Error estándar del estimador Fay-Herriot",
    subtitle = "Modelo principal",
    x = "Departamento",
    y = "SE Fay-Herriot"
  ) +
  theme_minimal()

ggsave(
  filename = file.path(carpeta_graficos, "06_se_fh_principal.png"),
  plot = g_se,
  width = 10,
  height = 8
)

# =========================================================
# 15. PREDICCIÓN MUNICIPAL DEL IPM 2024
# =========================================================

pred_mun_principal <- predecir_fh_municipal(
  modelo_fh = modelo_fh_principal,
  base_fh = base_fh_principal,
  x_mun = x_mun,
  vars_modelo = c("repitencia", "infraestructura_basica", "mortalidad_infantil"),
  nombre_pred = "ipm_pred_principal"
)

pred_mun_sens <- predecir_fh_municipal(
  modelo_fh = modelo_fh_sens,
  base_fh = base_fh_sens,
  x_mun = x_mun,
  vars_modelo = c("repitencia", "infraestructura_basica", "log_mortalidad_infantil"),
  nombre_pred = "ipm_pred_sens"
)

cat("Resumen predicción municipal calibrada - modelo principal:\n")
print(summary(pred_mun_principal$ipm_pred_principal_calibrado_acotado))
cat("\n")

cat("Resumen predicción municipal calibrada - modelo sensibilidad:\n")
print(summary(pred_mun_sens$ipm_pred_sens_calibrado_acotado))
cat("\n")

pred_municipal_comparada <- merge(
  pred_mun_principal[, c(
    "cod_depto",
    "depto",
    "cod_mpio",
    "mpio",
    "ipm_pred_principal_fijo",
    "ipm_pred_principal_fijo_acotado",
    "ipm_pred_principal_calibrado",
    "ipm_pred_principal_calibrado_acotado"
  )],
  pred_mun_sens[, c(
    "cod_mpio",
    "ipm_pred_sens_fijo",
    "ipm_pred_sens_fijo_acotado",
    "ipm_pred_sens_calibrado",
    "ipm_pred_sens_calibrado_acotado"
  )],
  by = "cod_mpio",
  all = TRUE
)

pred_municipal_comparada$dif_modelos_calibrado <-
  pred_municipal_comparada$ipm_pred_principal_calibrado -
  pred_municipal_comparada$ipm_pred_sens_calibrado

pred_municipal_comparada$dif_modelos_calibrado_acotado <-
  pred_municipal_comparada$ipm_pred_principal_calibrado_acotado -
  pred_municipal_comparada$ipm_pred_sens_calibrado_acotado

cat("Resumen diferencia municipal entre modelos calibrados:\n")
print(summary(pred_municipal_comparada$dif_modelos_calibrado))
cat("\n")

write.csv(
  pred_mun_principal,
  file.path(carpeta_salida, "prediccion_municipal_ipm_2024_principal.csv"),
  row.names = FALSE
)

write.csv(
  pred_mun_sens,
  file.path(carpeta_salida, "prediccion_municipal_ipm_2024_sensibilidad.csv"),
  row.names = FALSE
)

write.csv(
  pred_municipal_comparada,
  file.path(carpeta_salida, "prediccion_municipal_ipm_2024_comparada.csv"),
  row.names = FALSE
)

ranking_ipm_municipal <- pred_municipal_comparada[
  order(-pred_municipal_comparada$ipm_pred_principal_calibrado_acotado),
]

rownames(ranking_ipm_municipal) <- NULL

write.csv(
  ranking_ipm_municipal,
  file.path(carpeta_salida, "ranking_ipm_municipal_2024.csv"),
  row.names = FALSE
)

cat("Top 20 municipios con mayor IPM predicho:\n")
print(head(ranking_ipm_municipal, 20))
cat("\n")

cat("Top 20 municipios con menor IPM predicho:\n")
print(head(
  ranking_ipm_municipal[order(ranking_ipm_municipal$ipm_pred_principal_calibrado_acotado), ],
  20
))
cat("\n")

g_hist_mun <- pred_municipal_comparada %>%
  ggplot(aes(x = ipm_pred_principal_calibrado_acotado)) +
  geom_histogram(bins = 35) +
  labs(
    title = "Distribución del IPM municipal predicho 2024",
    subtitle = "Modelo Fay-Herriot principal calibrado",
    x = "IPM municipal predicho",
    y = "Número de municipios"
  ) +
  theme_minimal()

ggsave(
  filename = file.path(carpeta_graficos, "07_distribucion_ipm_municipal_predicho.png"),
  plot = g_hist_mun,
  width = 9,
  height = 6
)

g_dif_mun <- pred_municipal_comparada %>%
  ggplot(aes(x = dif_modelos_calibrado_acotado)) +
  geom_histogram(bins = 35) +
  labs(
    title = "Diferencia entre predicciones municipales",
    subtitle = "Modelo principal vs modelo de sensibilidad",
    x = "Diferencia en IPM predicho",
    y = "Número de municipios"
  ) +
  theme_minimal()

ggsave(
  filename = file.path(carpeta_graficos, "08_diferencias_modelos_municipales.png"),
  plot = g_dif_mun,
  width = 9,
  height = 6
)

# =========================================================
# 16. VALIDACIÓN AGREGADA POR DEPARTAMENTO
# =========================================================

validacion_depto <- pred_municipal_comparada %>%
  group_by(cod_depto, depto) %>%
  summarise(
    promedio_municipal_predicho = mean(ipm_pred_principal_calibrado_acotado, na.rm = TRUE),
    n_municipios_predichos = n(),
    .groups = "drop"
  )

validacion_depto <- merge(
  validacion_depto,
  ipm_2024[, c("cod_depto", "departamento", "ipm_directo")],
  by = "cod_depto",
  all.x = TRUE
)

validacion_depto$dif_promedio_directo <-
  validacion_depto$promedio_municipal_predicho -
  validacion_depto$ipm_directo

metricas_validacion_depto <- data.frame(
  RMSE = rmse(validacion_depto$ipm_directo, validacion_depto$promedio_municipal_predicho),
  MAE = mae(validacion_depto$ipm_directo, validacion_depto$promedio_municipal_predicho),
  correlacion = cor(
    validacion_depto$ipm_directo,
    validacion_depto$promedio_municipal_predicho,
    use = "complete.obs"
  )
)

cat("Validación agregada departamental:\n")
print(validacion_depto)
cat("\n")

cat("Métricas validación departamental:\n")
print(metricas_validacion_depto)
cat("\n")

write.csv(
  validacion_depto,
  file.path(carpeta_salida, "validacion_promedio_departamental_ipm_municipal.csv"),
  row.names = FALSE
)

write.csv(
  metricas_validacion_depto,
  file.path(carpeta_salida, "metricas_validacion_departamental.csv"),
  row.names = FALSE
)

g_val_depto <- validacion_depto %>%
  ggplot(aes(x = ipm_directo, y = promedio_municipal_predicho)) +
  geom_point(size = 2.5, alpha = 0.8) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(
    title = "Validación agregada departamental",
    subtitle = "Promedio municipal predicho vs IPM directo departamental",
    x = "IPM directo departamental",
    y = "Promedio municipal predicho"
  ) +
  theme_minimal()

ggsave(
  filename = file.path(carpeta_graficos, "09_validacion_departamental.png"),
  plot = g_val_depto,
  width = 8,
  height = 7
)

# =========================================================
# 17. DIAGNÓSTICO ESPACIAL DE RESIDUALES
# =========================================================

if (file.exists(ruta_shp_dptos)) {

  dptos <- st_read(ruta_shp_dptos, quiet = TRUE)

  if ("DPTO_CCDGO" %in% names(dptos)) {
    dptos$cod_depto <- as.numeric(dptos$DPTO_CCDGO)
  } else {
    stop("No se encontró la columna DPTO_CCDGO en el shapefile.")
  }

  base_espacial <- merge(
    dptos,
    base_fh_principal,
    by = "cod_depto",
    all.x = FALSE,
    all.y = TRUE
  )

  vecinos <- poly2nb(base_espacial, queen = TRUE)
  W <- nb2listw(vecinos, style = "W", zero.policy = TRUE)

  modelo_base_esp <- lm(
    ipm_directo ~ repitencia + infraestructura_basica + mortalidad_infantil,
    data = base_espacial
  )

  residuos_base <- residuals(modelo_base_esp)

  moran_res <- moran.test(residuos_base, W, zero.policy = TRUE)

  cat("Prueba de Moran sobre residuos del modelo base:\n")
  print(moran_res)
  cat("\n")

  capture.output(
    moran_res,
    file = file.path(carpeta_salida, "moran_residuos_modelo_base.txt")
  )

  modelo_sem <- tryCatch({
    spatialreg::errorsarlm(
      ipm_directo ~ repitencia + infraestructura_basica + mortalidad_infantil,
      data = base_espacial,
      listw = W,
      zero.policy = TRUE
    )
  }, error = function(e) NULL)

  modelo_sar <- tryCatch({
    spatialreg::lagsarlm(
      ipm_directo ~ repitencia + infraestructura_basica + mortalidad_infantil,
      data = base_espacial,
      listw = W,
      zero.policy = TRUE
    )
  }, error = function(e) NULL)

  comparacion_espacial <- data.frame(
    modelo = c("OLS_base", "SEM_error_espacial", "SAR_lag_espacial"),
    AIC = c(
      AIC(modelo_base_esp),
      if (!is.null(modelo_sem)) AIC(modelo_sem) else NA_real_,
      if (!is.null(modelo_sar)) AIC(modelo_sar) else NA_real_
    )
  )

  write.csv(
    comparacion_espacial,
    file.path(carpeta_salida, "comparacion_modelos_espaciales_exploratorios.csv"),
    row.names = FALSE
  )

  cat("Comparación exploratoria modelos espaciales:\n")
  print(comparacion_espacial)
  cat("\n")

  mapa_ipm_directo <- ggplot(base_espacial) +
    geom_sf(aes(fill = ipm_directo), color = "white", linewidth = 0.2) +
    labs(
      title = "IPM directo departamental 2024",
      fill = "IPM"
    ) +
    theme_minimal()

  ggsave(
    filename = file.path(carpeta_graficos, "10_mapa_ipm_directo_departamental.png"),
    plot = mapa_ipm_directo,
    width = 8,
    height = 9
  )

  base_espacial_fh <- merge(
    base_espacial,
    resultados_fh[, c("cod_depto", "fh_principal", "dif_principal_directo")],
    by = "cod_depto",
    all.x = TRUE
  )

  mapa_fh <- ggplot(base_espacial_fh) +
    geom_sf(aes(fill = fh_principal), color = "white", linewidth = 0.2) +
    labs(
      title = "Estimación Fay-Herriot departamental 2024",
      fill = "FH"
    ) +
    theme_minimal()

  ggsave(
    filename = file.path(carpeta_graficos, "11_mapa_fh_departamental.png"),
    plot = mapa_fh,
    width = 8,
    height = 9
  )

  mapa_dif <- ggplot(base_espacial_fh) +
    geom_sf(aes(fill = dif_principal_directo), color = "white", linewidth = 0.2) +
    labs(
      title = "Diferencia FH - IPM directo departamental",
      fill = "Diferencia"
    ) +
    theme_minimal()

  ggsave(
    filename = file.path(carpeta_graficos, "12_mapa_diferencia_fh_directo.png"),
    plot = mapa_dif,
    width = 8,
    height = 9
  )

} else {
  cat("No se encontró shapefile departamental. Se omite diagnóstico espacial.\n")
}

# =========================================================
# 18. EXPORTAR TODO EN EXCEL
# =========================================================

wb <- createWorkbook()

addWorksheet(wb, "base_sae")
writeData(wb, "base_sae", base_sae)

addWorksheet(wb, "comparacion_ols")
writeData(wb, "comparacion_ols", comparacion_modelos)

addWorksheet(wb, "resultados_fh")
writeData(wb, "resultados_fh", resultados_fh)

addWorksheet(wb, "pred_municipal")
writeData(wb, "pred_municipal", pred_municipal_comparada)

addWorksheet(wb, "validacion_depto")
writeData(wb, "validacion_depto", validacion_depto)

addWorksheet(wb, "metricas_validacion")
writeData(wb, "metricas_validacion", metricas_validacion_depto)

if (exists("comparacion_espacial")) {
  addWorksheet(wb, "comparacion_espacial")
  writeData(wb, "comparacion_espacial", comparacion_espacial)
}

saveWorkbook(
  wb,
  file.path(carpeta_salida, "resultados_actividad_3_modelamiento.xlsx"),
  overwrite = TRUE
)

cat("=========================================================\n")
cat("Proceso completado correctamente.\n")
cat("Archivos exportados en:\n")
cat(carpeta_salida, "\n")
cat("Gráficos exportados en:\n")
cat(carpeta_graficos, "\n")
cat("=========================================================\n")
