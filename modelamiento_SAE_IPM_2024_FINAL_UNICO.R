# ============================================================================
# TRABAJO DE GRADO - SAE / IPM MUNICIPAL 2024
# PROGRAMA UNICO DE EJECUCION
# Incluye M1, M2, M3, validacion LODO, sensibilidad a rho
# y robustez frente a departamentos influyentes en E2.
#
# INSTRUCCIONES:
# 1. Ajustar RUTA_BASE en el bloque inicial de configuracion.
# 2. Ejecutar todo este archivo en una SESION LIMPIA de R.
# 3. El archivo ejecuta los tres bloques en orden y reutiliza sus objetos.
# 4. Las rutas relativas de los CSV se crean bajo getwd().
# 5. No se requiere ejecutar ni cargar otros scripts .R.
#
# Esta integracion preserva las operaciones de los tres programas
# previamente separados. Revisar las metricas contra las salidas validadas.
# ============================================================================


# ============================================================================
# BLOQUE I: MODELAMIENTO PRINCIPAL M1-M2-M3
# ============================================================================

# =========================================================
# ESTIMACIÓN MUNICIPAL DEL IPM MEDIANTE MODELOS SAE
#
# Modelo 1: Fay-Herriot de área
# Modelo 2: desagregación municipal no lineal acotada
#           con benchmarking departamental
# Modelo 3: desagregación espacial municipal con efecto
#           CAR reducido (2024) y benchmarking departamental
#
# Secuencia metodológica:
# 1) ajuste de cada modelo;
# 2) validación interna / condiciones de validez;
# 3) validación predictiva;
# 4) habilitación de escenarios aptos;
# 5) comparación entre modelos.
#
# Dos niveles de comparación:
# A) comparación controlada M1-M2-M3 con escenarios comunes
#    que hayan superado la validación mínima;
# B) comparación de formulaciones técnicamente admisibles.
#
# FUENTE ÚNICA DE COVARIABLES
# - Covariables_18&24.csv es la matriz auxiliar maestra.
# - M1, M2 y M3 utilizan exclusivamente periodo == 2024
#   para la comparación principal.
# - M3 incorpora un efecto espacial municipal CAR reducido,
#   estimado conjuntamente con las covariables de 2024.
# =========================================================
#
# CORRECCIONES INCORPORADAS RESPECTO A LA VERSIÓN ANTERIOR
#
# 1. Verificación de alineación de columnas por llave.
#    Se detectó que algunas columnas incorporadas desde
#    fuentes externas quedaron yuxtapuestas por posición de
#    fila en lugar de cruzadas por código municipal. Este
#    error no genera fallo de ejecución y compromete el
#    sistema de ponderación.
#
# 2. Ponderadores de agregación conceptualmente pertinentes.
#    Los indicadores educativos se agregan por población de
#    5 a 16 años y la mortalidad infantil por nacidos vivos.
#
# 3. Validación LODO del Modelo 2. Se corrigen tres aspectos:
#    a) el dominio excluido NO recibe benchmarking, dado que
#       ese procedimiento usa la estimación directa que se
#       busca predecir;
#    b) la estandarización se recalcula en cada iteración
#       con municipios de entrenamiento únicamente;
#    c) se emplean múltiples arranques del optimizador para
#       evitar óptimos locales.
#
# 4. Normalizacion por diccionario de unidades y control de escalas 2024.
# =========================================================


# =========================================================
# 0. PAQUETES Y CONFIGURACIÓN
# =========================================================

paquetes <- c("dplyr", "tidyr", "readr", "readxl", "openxlsx", "sae",
              "spdep", "spatialreg", "sf", "ggplot2", "car",
              "lmtest", "sandwich", "Matrix", "RSpectra")

for (p in paquetes) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p, dependencies = TRUE)
  }
  library(p, character.only = TRUE)
}

options(stringsAsFactors = FALSE)
set.seed(2026)

# =========================================================
# RUTAS LOCALES DEL PROYECTO
# =========================================================

RUTA_BASE <- "C:/Users/Erick Caicedo Ruiz/Desktop/Maestría Ciencia de Datos/Trabajo de Grado"
RUTA_AUX  <- file.path(RUTA_BASE, "BD Auxiliares")

# Fuente única de covariables para M1, M2 y M3
RUTA_COV <- file.path(RUTA_AUX, "Covariables_18&24.csv")

# Fuentes de respuesta y soporte espacial
RUTA_IPM <- file.path(RUTA_BASE, "IPM_dptos", "IPM_dptos", "IPM_Depto_2024.xlsx")
RUTA_SHP <- "C:/Users/Erick Caicedo Ruiz/Desktop/Maestría Ciencia de Datos/Trabajo de Grado/ADMINISTRATIVO/MGN_ADM_MPIO_GRAFICO.shp"
CARPETA_SALIDA <- file.path(RUTA_AUX, "salidas_modelos_1_2_3")
if (!dir.exists(CARPETA_SALIDA)) {
  dir.create(CARPETA_SALIDA, recursive = TRUE)
}

cat("\nVERIFICACION DE ARCHIVOS PRINCIPALES\n")
archivos_verificar <- c(
  RUTA_COV, RUTA_IPM, RUTA_SHP
)
print(data.frame(archivo = archivos_verificar, existe = file.exists(archivos_verificar)))
if (any(!file.exists(archivos_verificar))) {
  stop("Hay archivos de entrada que no existen. Revise las rutas antes de continuar.")
}


# =========================================================
# 1. FUNCIONES AUXILIARES
# =========================================================

asegurar_numerico <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))

  z <- trimws(as.character(x))
  z[z %in% c("", "NA", "NaN", "NULL")] <- NA_character_
  tiene_coma <- grepl(",", z, fixed = TRUE)

  out <- suppressWarnings(as.numeric(z))

  if (any(tiene_coma, na.rm = TRUE)) {
    z2 <- z[tiene_coma]
    z2 <- gsub(".", "", z2, fixed = TRUE)
    z2 <- gsub(",", ".", z2, fixed = TRUE)
    out[tiene_coma] <- suppressWarnings(as.numeric(z2))
  }

  out
}

rmse <- function(obs, pred) {
  ok <- is.finite(obs) & is.finite(pred)
  sqrt(mean((obs[ok] - pred[ok])^2))
}

mae <- function(obs, pred) {
  ok <- is.finite(obs) & is.finite(pred)
  mean(abs(obs[ok] - pred[ok]))
}

logit <- function(p) log(p / (1 - p))
inv_logit <- function(x) 1 / (1 + exp(-x))


# -------------------------------------------------------------------
# Código municipal desde shapefile
# -------------------------------------------------------------------
# La capa principal utilizada es MGN_ADM_MPIO_GRAFICO.shp, donde
# mpio_cdpmp contiene el código DIVIPOLA municipal completo.
# Se dejan respaldos para otras nomenclaturas posibles.
obtener_codigo_municipal <- function(mapa) {

  if ("mpio_cdpmp" %in% names(mapa)) {
    return(
      suppressWarnings(
        as.integer(as.character(mapa$mpio_cdpmp))
      )
    )
  }

  if ("MPIO_CDPMP" %in% names(mapa)) {
    return(
      suppressWarnings(
        as.integer(as.character(mapa$MPIO_CDPMP))
      )
    )
  }

  if ("MpCodigo" %in% names(mapa)) {
    return(
      suppressWarnings(
        as.integer(as.character(mapa$MpCodigo))
      )
    )
  }

  if (all(c("dpto_ccdgo", "mpio_ccdgo") %in% names(mapa))) {
    return(
      suppressWarnings(
        as.integer(
          paste0(
            sprintf("%02d", as.integer(as.character(mapa$dpto_ccdgo))),
            sprintf("%03d", as.integer(as.character(mapa$mpio_ccdgo)))
          )
        )
      )
    )
  }

  if (all(c("DPTO_CCDGO", "MPIO_CCDGO") %in% names(mapa))) {
    return(
      suppressWarnings(
        as.integer(
          paste0(
            sprintf("%02d", as.integer(as.character(mapa$DPTO_CCDGO))),
            sprintf("%03d", as.integer(as.character(mapa$MPIO_CCDGO)))
          )
        )
      )
    )
  }

  cat("\nColumnas disponibles en el shapefile:\n")
  print(names(mapa))

  stop("No se pudo construir cod_mpio desde el shapefile.")
}


# =========================================================
# 2. CARGA Y VERIFICACIÓN DE INTEGRIDAD
# =========================================================

cat("=========================================\n")
cat("CARGA DE DATOS - FUENTE ÚNICA DE COVARIABLES\n")
cat("=========================================\n")

covariables <- read.csv2(
  RUTA_COV,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

vars_requeridas_cov <- c(
  "cod_depto", "depto", "cod_mpio", "mpio", "periodo",
  "poblacion",
  "pct_subsidiado", "pct_cont", "pob_asegurada",
  "aseguramiento_capado",
  "tasa_matriculacion_5_16",
  "cobertura_neta", "cobertura_neta_media",
  "desercion", "repitencia", "reprobacion",
  "Cobertura_acueducto", "Cobertura_alcantarillado", "Cobertura_aseo",
  "infraestructura_basica", "ICEE_rural",
  "rmm42", "mortalidad_infantil",
  "Recaudo_ICA", "Rec_ICA_PC",
  "viirs_rad", "log_viirs",
  "Nacidos_vivos", "Poblacion_5_a_16"
)

faltan_cov <- setdiff(vars_requeridas_cov, names(covariables))

if (length(faltan_cov) > 0) {
  stop(
    paste(
      "Covariables_18&24.csv no contiene las variables requeridas:",
      paste(faltan_cov, collapse = ", ")
    )
  )
}

covariables$cod_mpio  <- suppressWarnings(as.integer(covariables$cod_mpio))
covariables$cod_depto <- suppressWarnings(as.integer(covariables$cod_depto))
covariables$periodo   <- suppressWarnings(as.integer(covariables$periodo))

# -------------------------------------------------------------------
# Alias canónicos
# -------------------------------------------------------------------
# Se conserva ICEE_rural para la formulación propia de M3 y se crea
# cobertura_energia_rural para que E1-E3 usen una nomenclatura común.
# Nacidos_vivos y Poblacion_5_a_16 se normalizan a minúscula para
# utilizarlos como ponderadores en M1-M2.

covariables$nacidos_vivos <-
  asegurar_numerico(covariables$Nacidos_vivos)

covariables$poblacion_5_a_16 <-
  asegurar_numerico(covariables$Poblacion_5_a_16)

# -------------------------------------------------------------------
# Conversión numérica
# -------------------------------------------------------------------

vars_numericas_base <- intersect(
  c(
    "poblacion", "poblacion_2024",
    "pct_subsidiado", "pct_cont", "pob_asegurada",
    "aseguramiento_capado",
    "tasa_matriculacion_5_16",
    "cobertura_neta", "cobertura_neta_media",
    "desercion", "repitencia", "reprobacion",
    "Cobertura_acueducto", "Cobertura_alcantarillado", "Cobertura_aseo",
    "infraestructura_basica",
    "ICEE_rural", "cobertura_energia_rural",
    "rmm42", "mortalidad_infantil",
    "Recaudo_ICA", "Rec_ICA_PC",
    "luces_viirs_2024", "log_viirs_2024",
    "viirs_rad", "log_viirs",
    "Nacidos_vivos", "Poblacion_5_a_16",
    "nacidos_vivos", "poblacion_5_a_16"
  ),
  names(covariables)
)

for (v in vars_numericas_base) {
  covariables[[v]] <- asegurar_numerico(covariables[[v]])
}

# -------------------------------------------------------------------
# NORMALIZACION DE UNIDADES: DICCIONARIO EXPLICITO Y AUDITORIA
# -------------------------------------------------------------------
# CORRECCION METODOLOGICA:
# NO se decide la conversion usando el maximo observado.
# Cada columna tiene una unidad de origen declarada de forma explicita.
# Los rangos solo se usan para DETECTAR errores, nunca para elegir unidad.
#
# IMPORTANTE: verificar esta configuracion frente al diccionario de fuentes
# de Covariables_18&24.csv antes de interpretar resultados definitivos.
# Las columnas declaradas como porcentaje se esperan en 0--100 en el CSV;
# las declaradas como proporcion se esperan ya como cociente (1 = 100%).
# Si la auditoria encuentra una discrepancia, se detiene ANTES de modelar.
#
# La matriculacion es una tasa bruta ya expresada como proporcion y puede
# ser mayor que 1. El aseguramiento_capado debe estar ya en proporcion.
# No se tocan los conteos, mortalidad, recaudo ni VIIRS.
# -------------------------------------------------------------------

UNIDADES_ORIGEN <- c(
  pct_subsidiado          = "proporcion",
  pct_cont               = "proporcion",
  aseguramiento_capado   = "proporcion",
  tasa_matriculacion_5_16 = "proporcion",
  cobertura_neta         = "porcentaje",
  cobertura_neta_media   = "porcentaje",
  desercion              = "porcentaje",
  repitencia             = "porcentaje",
  reprobacion            = "porcentaje",
  Cobertura_acueducto    = "porcentaje",
  Cobertura_alcantarillado = "porcentaje",
  Cobertura_aseo         = "porcentaje",
  infraestructura_basica = "porcentaje",
  ICEE_rural             = "porcentaje"
)

# Si una variable de la fuente YA viene en escala de proporcion,
# cambiar explicitamente su unidad en UNIDADES_ORIGEN; NO dividir por 100.
# Se genera una auditoria con minimos, maximos y controles previos/posteriores.
# ADVERTENCIA: las unidades de columnas no documentadas no pueden inferirse
# solo a partir de sus nombres; contrastar las 14 asignaciones con la fuente.

vars_normalizar_01 <- names(UNIDADES_ORIGEN)
faltan_normalizacion <- setdiff(vars_normalizar_01, names(covariables))
if (length(faltan_normalizacion) > 0) {
  stop(paste("Faltan columnas para normalizacion:",
             paste(faltan_normalizacion, collapse = ", ")))
}

normalizar_proporcion_01 <- function(x, nombre_variable = "") {
  x <- asegurar_numerico(x)
  if (!nombre_variable %in% names(UNIDADES_ORIGEN)) {
    stop(paste("Unidad de origen no definida:", nombre_variable))
  }
  unidad <- unname(UNIDADES_ORIGEN[[nombre_variable]])
  if (!unidad %in% c("porcentaje", "proporcion")) {
    stop(paste("Unidad no valida para", nombre_variable, ":", unidad))
  }
  if (unidad == "porcentaje") return(x / 100)
  x
}

cat("\n=========================================\n")
cat("NORMALIZACION POR UNIDAD DECLARADA\n")
cat("=========================================\n")

# Se audita el periodo 2024 de forma separada; las mismas reglas se aplican
# a toda la matriz auxiliar para mantener consistencia intertemporal.
indices_2024 <- !is.na(covariables$periodo) & covariables$periodo == 2024
resumen_escala <- function(z) {
  z <- z[is.finite(z)]
  c(minimo = if (length(z)) min(z) else NA_real_,
    maximo = if (length(z)) max(z) else NA_real_)
}

tabla_auditoria_escalas <- do.call(rbind, lapply(vars_normalizar_01, function(v) {
  original <- asegurar_numerico(covariables[[v]])
  z2024 <- original[indices_2024]
  rango <- resumen_escala(z2024)
  unidad <- unname(UNIDADES_ORIGEN[[v]])
  transformado <- normalizar_proporcion_01(original, v)
  rango_post <- resumen_escala(transformado[indices_2024])
  n_validos <- sum(is.finite(z2024))
  # Una cobertura expresada en porcentaje con maximo <= 1 podria ser una
  # columna ya transformada. Se bloquea para evitar segunda division.
  alerta <- if (n_validos == 0) {
    "SIN_DATOS_2024"
  } else if (unidad == "porcentaje" && rango["maximo"] <= 1) {
    "POSIBLE_PROPORCION_YA_CONVERTIDA"
  } else if (unidad == "porcentaje" && rango["maximo"] > 100) {
    "PORCENTAJE_SUPERIOR_100_REVISAR_DEFINICION"
  } else if (unidad == "proporcion" && v %in% c("pct_subsidiado", "pct_cont") &&
             any(is.finite(z2024) & (z2024 < 0 | z2024 > 1 + 1e-8))) {
    "PROPORCION_SALUD_FUERA_0_1"
  } else if (unidad == "proporcion" && v == "aseguramiento_capado" &&
             any(is.finite(z2024) & (z2024 < 0 | z2024 > 1 + 1e-8))) {
    "ASEGURAMIENTO_CAPADO_FUERA_0_1"
  } else if (unidad == "proporcion" && v == "tasa_matriculacion_5_16" &&
             (rango["minimo"] < 0 || rango["maximo"] > 10)) {
    "MATRICULACION_EXTREMA_VERIFICAR_UNIDAD"
  } else {
    "OK"
  }
  data.frame(variable = v, unidad_origen = unidad,
             n_2024_validos = n_validos,
             minimo_2024_original = unname(rango["minimo"]),
             maximo_2024_original = unname(rango["maximo"]),
             minimo_2024_transformado = unname(rango_post["minimo"]),
             maximo_2024_transformado = unname(rango_post["maximo"]),
             accion = if (unidad == "porcentaje") "dividir_entre_100" else "conservar",
             alerta = alerta, row.names = NULL)
}))

print(tabla_auditoria_escalas, row.names = FALSE)

# Archivo de auditoria guardado aun si el chequeo detecta problemas.
write.csv2(tabla_auditoria_escalas,
           file.path(CARPETA_SALIDA, "auditoria_escalas_2024.csv"),
           row.names = FALSE, na = "")

# IMPORTANTE: en lugar de convertir una unidad posiblemente erronea,
# detener el proceso para revisar el diccionario de fuentes.
alertas_bloqueantes <- c("SIN_DATOS_2024", "POSIBLE_PROPORCION_YA_CONVERTIDA",
                        "ASEGURAMIENTO_CAPADO_FUERA_0_1", "PROPORCION_SALUD_FUERA_0_1",
                        "MATRICULACION_EXTREMA_VERIFICAR_UNIDAD")
if (any(tabla_auditoria_escalas$alerta %in% alertas_bloqueantes)) {
  stop(paste0(
    "Auditoria de escalas: se detectaron unidades por verificar. ",
    "Abra auditoria_escalas_2024.csv, contraste con las fuentes y ",
    "ajuste UNIDADES_ORIGEN. No se ejecuto la modelacion."
  ))
}
if (any(tabla_auditoria_escalas$alerta != "OK")) {
  warning("Hay alertas de escala no bloqueantes; consulte auditoria_escalas_2024.csv")
}

for (v in vars_normalizar_01) {
  covariables[[v]] <- normalizar_proporcion_01(covariables[[v]], v)
  cat("Unidad aplicada:", v, "->", unname(UNIDADES_ORIGEN[[v]]), "\n")
}

# Alias construido DESPUES de normalizar ICEE_rural.
covariables$cobertura_energia_rural <- covariables$ICEE_rural

# Tabla que ya usan las exportaciones originales.
tabla_control_escalas <- data.frame(
  variable = vars_normalizar_01,
  minimo = sapply(vars_normalizar_01, function(v)
    if (any(is.finite(covariables[[v]]))) min(covariables[[v]], na.rm = TRUE) else NA_real_),
  maximo = sapply(vars_normalizar_01, function(v)
    if (any(is.finite(covariables[[v]]))) max(covariables[[v]], na.rm = TRUE) else NA_real_),
  row.names = NULL
)
print(tabla_control_escalas)
# Las tasas brutas pueden superar 1: NO se truncan por esta rutina.

cat("\\nPeriodos disponibles en Covariables_18&24.csv:\\n")
print(sort(unique(covariables$periodo)))

if (!2024 %in% unique(covariables$periodo)) {
  stop("Covariables_18&24.csv debe contener el periodo 2024.")
}

# M1 y M2: exclusivamente periodo 2024.
x_mun <- covariables %>%
  dplyr::filter(periodo == 2024) %>%
  dplyr::filter(!is.na(cod_mpio)) %>%
  dplyr::arrange(cod_depto, cod_mpio) %>%
  as.data.frame()

if (anyDuplicated(x_mun$cod_mpio) > 0) {
  stop("Existen códigos municipales duplicados en periodo 2024.")
}

# Los ponderadores específicos se requieren únicamente para M1-M2 (2024).
ponderadores_2024 <- c(
  "poblacion",
  "nacidos_vivos",
  "poblacion_5_a_16"
)

for (v in ponderadores_2024) {
  if (!v %in% names(x_mun)) {
    stop(paste("Falta el ponderador 2024:", v))
  }
}

cat("NAs en ponderadores 2024:\n")
print(
  sapply(
    x_mun[, ponderadores_2024, drop = FALSE],
    function(z) sum(!is.finite(z))
  )
)


cat("Municipios en M1-M2 (periodo 2024):", nrow(x_mun), "\n")
cat("Municipios disponibles para M3 espacial 2024:",
    nrow(x_mun), "\n\n")

# ---------------------------------------------------------
# 2.1. Verificación de alineación por llave
# ---------------------------------------------------------
#
# Una columna yuxtapuesta por posición no produce error de
# ejecución. La verificación contrasta la coherencia interna
# entre variables relacionadas y revisa que los valores
# extremos correspondan a unidades plausibles.
# ---------------------------------------------------------

verificar_alineacion <- function(datos) {

  cat("-----------------------------------------\n")
  cat("VERIFICACIÓN DE ALINEACIÓN\n")
  cat("-----------------------------------------\n")

  problemas <- character(0)

  # -------------------------------------------------------
  # 1. Coherencia entre recaudo total, población y per cápita
  # -------------------------------------------------------
  if (all(c("Recaudo_ICA", "Rec_ICA_PC", "poblacion")
          %in% names(datos))) {

    calc <- datos$Recaudo_ICA / datos$poblacion
    ok <- is.finite(calc) & is.finite(datos$Rec_ICA_PC)
    n_validos <- sum(ok)

    cat("Pares válidos para verificación de recaudo:",
        n_validos, "\n")

    if (n_validos >= 3 &&
        sd(calc[ok], na.rm = TRUE) > 0 &&
        sd(datos$Rec_ICA_PC[ok], na.rm = TRUE) > 0) {

      coher <- cor(calc[ok], datos$Rec_ICA_PC[ok],
                   use = "complete.obs")

      cat("Coherencia recaudo per cápita:",
          round(coher, 4), "\n")

      if (is.finite(coher) && coher < 0.95) {
        problemas <- c(problemas,
                       "Recaudo_ICA / Rec_ICA_PC desalineados")
        cat("  ADVERTENCIA: las columnas de recaudo no son\n",
            "  internamente coherentes. Verifique que el cruce\n",
            "  se haya realizado por cod_mpio.\n")
      }

    } else {
      cat("Coherencia recaudo per cápita: no evaluable\n")
      cat("  ADVERTENCIA: no hay suficientes pares válidos\n",
          "  o alguna de las variables no presenta variación.\n")
    }
  }

  # -------------------------------------------------------
  # 2. Verificación de municipios más poblados
  # -------------------------------------------------------
  if (all(c("poblacion", "mpio") %in% names(datos))) {

    datos_pop <- datos[is.finite(datos$poblacion), ]

    if (nrow(datos_pop) >= 3) {
      top <- datos_pop[order(-datos_pop$poblacion), ][1:3, ]
      cat("Municipios más poblados:",
          paste(top$mpio, collapse = ", "), "\n")

      esperados <- c("BOGOTA", "MEDELLIN", "CALI", "BARRANQUILLA")
      hallados <- sum(sapply(esperados, function(e) {
        any(grepl(e, toupper(top$mpio)))
      }))

      if (hallados < 2) {
        problemas <- c(problemas, "poblacion desalineada")
        cat("  ADVERTENCIA: la columna de población no\n",
            "  corresponde a los municipios esperados.\n")
      }
    } else {
      cat("No hay suficientes valores válidos de población\n",
          "para verificar los municipios más poblados.\n")
    }
  }

  # -------------------------------------------------------
  # 3. Resultado final
  # -------------------------------------------------------
  if (length(problemas) == 0) {
    cat("Sin problemas críticos detectados.\n")
  } else {
    cat("Problemas detectados:\n")
    for (p in problemas) cat(" -", p, "\n")
  }

  cat("\n")
  return(problemas)
}

problemas <- verificar_alineacion(x_mun)


# ---------------------------------------------------------
# 2.2. Base departamental
# ---------------------------------------------------------

ipm <- read_excel(RUTA_IPM, sheet = "Sheet1")
ipm <- ipm[, c("cod_depto", "Departamento",
               "ipm_directo", "vardir")]
names(ipm) <- c("cod_depto", "departamento",
                "ipm_directo", "vardir")
ipm$cod_depto <- as.integer(ipm$cod_depto)

cat("Dominios departamentales:", nrow(ipm), "\n\n")


# =========================================================
# 3. ESCENARIOS DE COVARIABLES
# =========================================================

E1 <- c("repitencia", "tasa_matriculacion_5_16",
        "infraestructura_basica", "cobertura_energia_rural",
        "mortalidad_infantil", "aseguramiento_capado",
        "log_viirs")

E2 <- c("repitencia", "infraestructura_basica",
        "aseguramiento_capado", "log_viirs")

E3 <- c("repitencia", "cobertura_energia_rural",
        "aseguramiento_capado", "log_viirs")

ESCENARIOS <- list(
  E1_Completa        = E1,
  E2_Parsimoniosa    = E2,
  E3_Alt_vivienda    = E3
)

# Ponderador de agregación pertinente para cada covariable.
#
# Educación:
# - repitencia y tasa de matriculación se ponderan por población de 5 a 16 años.
#
# Salud:
# - mortalidad infantil se pondera por nacidos vivos.
#
# Resto de covariables:
# - se ponderan por población municipal del periodo 2024.
#
# Todas las variables porcentuales/coberturas utilizadas en los escenarios
# ya fueron normalizadas previamente a escala 0-1.

PONDERADOR <- c(
  repitencia                = "poblacion_5_a_16",
  tasa_matriculacion_5_16   = "poblacion_5_a_16",
  mortalidad_infantil       = "nacidos_vivos",
  infraestructura_basica    = "poblacion",
  cobertura_energia_rural   = "poblacion",
  aseguramiento_capado      = "poblacion",
  log_viirs                 = "poblacion"
)

DENOMINADORES <- unique(PONDERADOR)


# =========================================================
# 4. BASE ANALÍTICA
# =========================================================
#
# Se aplica un criterio único de completitud sobre todas las
# covariables del escenario más amplio, de modo que las
# diferencias entre escenarios sean atribuibles a la
# especificación y no a variaciones en la muestra.
# =========================================================

for (v in c(E1, DENOMINADORES)) {
  x_mun[[v]] <- asegurar_numerico(x_mun[[v]])
}

completos <- complete.cases(x_mun[, c(E1, DENOMINADORES), drop = FALSE]) &
  is.finite(x_mun$poblacion) &
  x_mun$poblacion > 0

x_ok <- x_mun[completos, , drop = FALSE]
rownames(x_ok) <- NULL


# =========================================================
# IDENTIFICAR MUNICIPIOS EXCLUIDOS
# =========================================================

municipios_excluidos <- x_mun[!completos, , drop = FALSE]

variables_revision <- unique(c(E1, DENOMINADORES))

if (nrow(municipios_excluidos) > 0) {
  
  motivos <- apply(
    municipios_excluidos[, variables_revision, drop = FALSE],
    1,
    function(fila) {
      faltantes <- names(fila)[!is.finite(as.numeric(fila))]
      if (length(faltantes) == 0) {
        return("Sin faltantes en covariables")
      }
      paste(faltantes, collapse = ", ")
    }
  )
  
  tabla_excluidos <- data.frame(
    cod_depto = municipios_excluidos$cod_depto,
    departamento = municipios_excluidos$depto,
    cod_mpio = municipios_excluidos$cod_mpio,
    municipio = municipios_excluidos$mpio,
    poblacion = municipios_excluidos$poblacion,
    variables_faltantes = motivos
  )
  
  print(tabla_excluidos)
  
} else {
  cat("No se excluyeron municipios.\n")
}

cat("Municipios originales:", nrow(x_mun), "\n")
cat("Municipios excluidos:", nrow(municipios_excluidos), "\n")
cat("Municipios finales:", nrow(x_ok), "\n")


if (nrow(x_ok) == 0) {
  diagnostico_completitud <- data.frame(
    variable = unique(c(E1, DENOMINADORES)),
    n_validos = sapply(
      unique(c(E1, DENOMINADORES)),
      function(v) sum(is.finite(x_mun[[v]]))
    ),
    n_no_validos = sapply(
      unique(c(E1, DENOMINADORES)),
      function(v) sum(!is.finite(x_mun[[v]]))
    )
  )
  print(diagnostico_completitud)
  stop("La base analítica quedó con 0 municipios. Revise las variables reportadas.")
}

cat("=========================================\n")
cat("BASE ANALÍTICA\n")
cat("=========================================\n")
cat("Municipios completos:", nrow(x_ok),
    "de", nrow(x_mun), "\n")
cat("Excluidos:", nrow(x_mun) - nrow(x_ok), "\n\n")


# ---------------------------------------------------------
# 4.1. Agregación departamental ponderada
# ---------------------------------------------------------

agregar_departamental <- function(datos, variables, ponderador) {
  
  # -------------------------------------------------------
  # Controles previos
  # -------------------------------------------------------
  
  if (!"cod_depto" %in% names(datos)) {
    stop("La base no contiene la variable cod_depto.")
  }
  
  faltan_variables <- setdiff(
    variables,
    names(datos)
  )
  
  if (length(faltan_variables) > 0) {
    stop(
      paste(
        "Faltan covariables para la agregación departamental:",
        paste(faltan_variables, collapse = ", ")
      )
    )
  }
  
  ponderadores_necesarios <- unique(
    unname(
      ponderador[
        variables
      ]
    )
  )
  
  faltan_ponderadores <- setdiff(
    ponderadores_necesarios,
    names(datos)
  )
  
  if (length(faltan_ponderadores) > 0) {
    stop(
      paste(
        "Faltan ponderadores para la agregación departamental:",
        paste(faltan_ponderadores, collapse = ", ")
      )
    )
  }
  
  # Garantizar código numérico simple
  datos$cod_depto <- as.integer(
    datos$cod_depto
  )
  
  codigos <- sort(
    unique(
      datos$cod_depto[
        !is.na(datos$cod_depto)
      ]
    )
  )
  
  salida <- data.frame(
    cod_depto = codigos
  )
  
  # -------------------------------------------------------
  # Agregación ponderada de cada covariable
  # -------------------------------------------------------
  
  for (v in variables) {
    
    w_col <- ponderador[[v]]
    
    if (
      is.null(w_col) ||
      is.na(w_col) ||
      !nzchar(w_col)
    ) {
      stop(
        paste(
          "No se definió ponderador para la variable:",
          v
        )
      )
    }
    
    valores <- rep(
      NA_real_,
      length(codigos)
    )
    
    for (k in seq_along(codigos)) {
      
      g <- datos[
        datos$cod_depto == codigos[k],
        ,
        drop = FALSE
      ]
      
      w <- asegurar_numerico(
        g[[w_col]]
      )
      
      y <- asegurar_numerico(
        g[[v]]
      )
      
      ok <- is.finite(w) &
        is.finite(y) &
        w > 0
      
      if (sum(ok) > 0) {
        
        valores[k] <- sum(
          y[ok] * w[ok]
        ) / sum(
          w[ok]
        )
        
      } else {
        
        valores[k] <- NA_real_
      }
    }
    
    salida[[v]] <- valores
  }
  
  # -------------------------------------------------------
  # Número de municipios por departamento
  #
  # Se usa dplyr::count() en lugar de table() para evitar
  # problemas de estructura/clase del objeto.
  # -------------------------------------------------------
  
  n_mun <- datos %>%
    dplyr::filter(
      !is.na(cod_depto)
    ) %>%
    dplyr::count(
      cod_depto,
      name = "n_municipios"
    ) %>%
    dplyr::mutate(
      cod_depto = as.integer(cod_depto)
    ) %>%
    as.data.frame()
  
  # -------------------------------------------------------
  # Unión final
  # -------------------------------------------------------
  
  salida <- dplyr::left_join(
    salida,
    n_mun,
    by = "cod_depto"
  )
  
  salida <- salida[
    order(salida$cod_depto),
    ,
    drop = FALSE
  ]
  
  rownames(salida) <- NULL
  
  return(salida)
}


# Aplicar agregación
dep <- agregar_departamental(
  datos = x_ok,
  variables = E1,
  ponderador = PONDERADOR
)

cat("\nAgregación departamental completada.\n")
cat("Departamentos generados:", nrow(dep), "\n")
print(dep)

# ---------------------------------------------------------
# 4.1.1. Construcción de la base departamental de modelamiento
# ---------------------------------------------------------

dep$cod_depto <- suppressWarnings(as.integer(dep$cod_depto))
ipm$cod_depto <- suppressWarnings(as.integer(ipm$cod_depto))

base <- dplyr::inner_join(
  ipm,
  dep,
  by = "cod_depto"
) %>%
  dplyr::filter(
    is.finite(vardir),
    vardir > 0
  ) %>%
  dplyr::filter(
    complete.cases(dplyr::across(dplyr::all_of(E1)))
  ) %>%
  dplyr::arrange(cod_depto) %>%
  as.data.frame()

rownames(base) <- NULL

cat("\n=========================================\n")
cat("BASE DEPARTAMENTAL FINAL\n")
cat("=========================================\n")
cat("Departamentos en el análisis:", nrow(base), "\n")

if (nrow(base) == 0) {
  stop("La base departamental quedó vacía. Revise covariables, códigos o vardir.")
}

cat(
  "IPM directo: mínimo", round(min(base$ipm_directo), 1),
  "máximo", round(max(base$ipm_directo), 1),
  "media", round(mean(base$ipm_directo), 2), "\n\n"
)

# ---------------------------------------------------------
# 4.2. Base municipal para el Modelo 2
# ---------------------------------------------------------

x_m2 <- x_ok[x_ok$cod_depto %in% base$cod_depto, ]

# Pesos poblacionales dentro de cada departamento.
x_m2 <- x_m2 %>%
  group_by(cod_depto) %>%
  mutate(peso = poblacion / sum(poblacion, na.rm = TRUE)) %>%
  ungroup() %>%
  as.data.frame()

x_m2 <- x_m2[order(x_m2$cod_depto, x_m2$cod_mpio), ]

# Índice del departamento al que pertenece cada municipio.
idx_dep <- match(x_m2$cod_depto, base$cod_depto)

cat("Municipios en el Modelo 2:", nrow(x_m2), "\n\n")


# =========================================================
# 5. DIAGNÓSTICO POR MÍNIMOS CUADRADOS ORDINARIOS
# =========================================================

cat("=========================================\n")
cat("DIAGNÓSTICO OLS POR ESCENARIO\n")
cat("=========================================\n")

diagnostico <- list()

for (nom in names(ESCENARIOS)) {

  covs <- ESCENARIOS[[nom]]
  f <- as.formula(paste("ipm_directo ~",
                        paste(covs, collapse = " + ")))
  m <- lm(f, data = base)

  cf <- summary(m)$coefficients
  cf <- cf[rownames(cf) != "(Intercept)", , drop = FALSE]

  vifs <- if (length(covs) > 1) car::vif(m) else NA

  d <- data.frame(
    escenario   = nom,
    variable    = rownames(cf),
    coeficiente = round(cf[, 1], 4),
    error_est   = round(cf[, 2], 4),
    valor_p     = round(cf[, 4], 4),
    VIF         = round(as.numeric(vifs[rownames(cf)]), 3),
    row.names   = NULL
  )

  diagnostico[[nom]] <- d

  cat("\n---", nom, "---\n")
  cat("R2 ajustado:", round(summary(m)$adj.r.squared, 4),
      " Observaciones por parámetro:",
      round(nrow(base) / (length(covs) + 1), 2), "\n")
  print(d[order(d$valor_p), ])
}

tabla_diagnostico <- do.call(rbind, diagnostico)
rownames(tabla_diagnostico) <- NULL
cat("\n")


# =========================================================
# 6. MODELO 1: FAY-HERRIOT
# =========================================================

cat("=========================================\n")
cat("MODELO 1: AJUSTE\n")
cat("=========================================\n")

ajuste_m1 <- list()
filas_m1  <- list()

for (nom in names(ESCENARIOS)) {

  covs <- ESCENARIOS[[nom]]
  f <- as.formula(paste("ipm_directo ~",
                        paste(covs, collapse = " + ")))

  # IMPORTANTE: eblupFH espera el nombre de la columna de varianza
  # dentro de data; no debe pasarse como base$vardir.
  fit <- sae::eblupFH(formula = f, vardir = vardir,
                       method = "REML", data = base)

  # Predicción municipal: relación fija más efecto aleatorio
  # del departamento correspondiente.
  X_dep <- model.matrix(f, data = base)
  beta  <- as.numeric(fit$fit$estcoef$beta)
  u_dep <- as.numeric(fit$eblup) - as.numeric(X_dep %*% beta)

  f_x <- as.formula(paste("~", paste(covs, collapse = " + ")))
  X_mun <- model.matrix(f_x, data = x_m2)
  pred_mun <- as.numeric(X_mun %*% beta) + u_dep[idx_dep]

  # Factor de contracción.
  sigma2_u <- fit$fit$refvar
  gamma <- sigma2_u / (sigma2_u + base$vardir)

  ajuste_m1[[nom]] <- list(
    fit = fit, beta = beta, u_dep = u_dep,
    pred_mun = pred_mun, gamma = gamma, formula = f
  )

  filas_m1[[nom]] <- data.frame(
    escenario     = nom,
    n_cov         = length(covs),
    sigma2_u      = round(sigma2_u, 4),
    gamma_medio   = round(mean(gamma), 4),
    loglike       = round(fit$fit$goodness["loglike"], 3),
    AIC           = round(fit$fit$goodness["AIC"], 2),
    BIC           = round(fit$fit$goodness["BIC"], 2),
    n_negativos   = sum(pred_mun < 0),
    n_sup_100     = sum(pred_mun > 100),
    row.names     = NULL
  )
}

tabla_m1 <- do.call(rbind, filas_m1)
print(tabla_m1)
cat("\n")

cat("NOTA: un factor de contracción cercano a uno indica que\n")
cat("el predictor se apoya principalmente en la estimación\n")
cat("directa, de modo que la corrección del modelo en el\n")
cat("nivel departamental es marginal.\n\n")



# =========================================================
# 6.A. VALIDACIÓN INTERNA Y CONDICIONES DE VALIDEZ - M1
# =========================================================
#
# La comparación entre modelos se realiza únicamente después
# de caracterizar las condiciones de validez del Fay-Herriot.
#
# CONDICIONES EVALUADAS:
# 1. Varianzas directas D_d finitas y estrictamente positivas.
# 2. Varianza del efecto de área sigma_u^2 finita y no negativa.
# 3. Coeficientes estimados finitos.
# 4. Ausencia de colinealidad severa (VIF).
# 5. Adecuación aproximada de la forma lineal del componente fijo.
# 6. Comportamiento de residuos estandarizados respecto a N(0,1).
# 7. Identificación de dominios potencialmente influyentes.
# 8. Soporte de la proyección municipal: se documentan valores
#    fuera de [0,100], aunque esta es una limitación estructural
#    del FH y no invalida por sí sola el ajuste departamental.
#
# IMPORTANTE:
# Shapiro-Wilk, RESET, Breusch-Pagan e influencia se interpretan
# como DIAGNÓSTICOS y no como reglas mecánicas de descarte.
# Los criterios "NO_APTO" se reservan para fallas estructurales
# o numéricas que impidan utilizar el modelo.
# =========================================================

cat("\n=========================================\n")
cat("VALIDACIÓN INTERNA MODELO 1 - FAY-HERRIOT\n")
cat("=========================================\n")

validacion_m1 <- list()

for (nom in names(ESCENARIOS)) {

  covs <- ESCENARIOS[[nom]]
  obj <- ajuste_m1[[nom]]
  fit <- obj$fit
  f <- obj$formula

  X_dep <- model.matrix(f, data = base)
  beta <- obj$beta
  sigma2_u <- as.numeric(fit$fit$refvar)

  # Residuo respecto al componente fijo. Bajo FH:
  # y_d - x_d'beta = u_d + e_d, con Var = sigma_u^2 + D_d.
  resid_fijo <- base$ipm_directo - as.numeric(X_dep %*% beta)
  sd_total <- sqrt(pmax(sigma2_u + base$vardir, .Machine$double.eps))
  resid_std <- resid_fijo / sd_total

  # Modelo lineal de referencia para diagnosticos funcionales.
  lm_ref <- lm(f, data = base)

  vif_vals <- tryCatch(
    if (length(covs) > 1) car::vif(lm_ref) else NA_real_,
    error = function(e) NA_real_
  )
  max_vif <- if (all(is.na(vif_vals))) NA_real_ else max(vif_vals, na.rm = TRUE)

  p_reset <- tryCatch(
    lmtest::resettest(lm_ref, power = 2:3, type = "fitted")$p.value,
    error = function(e) NA_real_
  )

  p_bp <- tryCatch(
    lmtest::bptest(lm_ref)$p.value,
    error = function(e) NA_real_
  )

  p_shapiro <- if (length(resid_std) >= 3 && length(resid_std) <= 5000) {
    tryCatch(shapiro.test(resid_std)$p.value, error = function(e) NA_real_)
  } else {
    NA_real_
  }

  cooks <- tryCatch(cooks.distance(lm_ref), error = function(e) rep(NA_real_, nrow(base)))
  n_influyentes <- sum(cooks > (4 / nrow(base)), na.rm = TRUE)
  n_resid_std_gt3 <- sum(abs(resid_std) > 3, na.rm = TRUE)

  vardir_valida <- all(is.finite(base$vardir) & base$vardir > 0)
  sigma2_valida <- is.finite(sigma2_u) && sigma2_u >= 0
  beta_validos <- all(is.finite(beta))
  ajuste_finito <- all(is.finite(as.numeric(fit$eblup)))

  # Criterios duros: si alguno falla, el modelo no es apto.
  apto_estructural <- all(
    vardir_valida,
    sigma2_valida,
    beta_validos,
    ajuste_finito
  )

  advertencias <- character(0)

  if (is.finite(max_vif) && max_vif >= 5) {
    advertencias <- c(advertencias, "VIF>=5")
  }
  if (is.finite(p_reset) && p_reset < 0.05) {
    advertencias <- c(advertencias, "RESET significativo")
  }
  if (is.finite(p_shapiro) && p_shapiro < 0.05) {
    advertencias <- c(advertencias, "Normalidad aproximada cuestionada")
  }
  if (n_resid_std_gt3 > 0) {
    advertencias <- c(advertencias, "Residuos estandarizados >3")
  }
  if (n_influyentes > 0) {
    advertencias <- c(advertencias, "Dominios influyentes")
  }
  if (sum(obj$pred_mun < 0 | obj$pred_mun > 100, na.rm = TRUE) > 0) {
    advertencias <- c(advertencias, "Predicciones municipales fuera de rango")
  }

  estado <- if (!apto_estructural) {
    "NO_APTO"
  } else if (length(advertencias) > 0) {
    "APTO_CON_ADVERTENCIAS"
  } else {
    "APTO"
  }

  validacion_m1[[nom]] <- data.frame(
    escenario = nom,
    modelo = "M1_FayHerriot",
    vardir_valida = vardir_valida,
    sigma2_u = sigma2_u,
    sigma2_u_valida = sigma2_valida,
    beta_finitos = beta_validos,
    eblup_finitos = ajuste_finito,
    max_VIF = max_vif,
    RESET_p = p_reset,
    Breusch_Pagan_p = p_bp,
    Shapiro_resid_std_p = p_shapiro,
    n_resid_std_abs_gt3 = n_resid_std_gt3,
    n_dominios_influyentes = n_influyentes,
    n_pred_mun_fuera_0_100 = sum(obj$pred_mun < 0 | obj$pred_mun > 100, na.rm = TRUE),
    apto_pre_lodo = apto_estructural,
    estado_pre_lodo = estado,
    advertencias = if (length(advertencias) == 0) "Ninguna" else paste(advertencias, collapse = "; "),
    row.names = NULL
  )
}

tabla_validacion_m1 <- dplyr::bind_rows(validacion_m1)
print(tabla_validacion_m1)

cat("\nInterpretación M1:\n")
cat("- NO_APTO = falla estructural o numérica.\n")
cat("- APTO_CON_ADVERTENCIAS = puede evaluarse predictivamente, pero las\n")
cat("  desviaciones de supuestos deben documentarse y discutirse.\n")
cat("- APTO = no se detectaron alertas relevantes en los diagnósticos aplicados.\n\n")

# =========================================================
# 7. MODELO 2: DESAGREGACIÓN NO LINEAL ACOTADA
# =========================================================

# ---------------------------------------------------------
# 7.1. Función de ajuste
# ---------------------------------------------------------
#
# Se emplean múltiples arranques porque la verosimilitud no
# es lineal y un único punto inicial puede converger a un
# óptimo local.
# ---------------------------------------------------------

ajustar_m2 <- function(Z, idx, pesos, y_dep, vardir,
                       n_dep, n_starts = 12, semilla = 2026) {

  set.seed(semilla)
  p <- ncol(Z)

  agregar <- function(beta) {
    pm <- inv_logit(as.numeric(Z %*% beta))
    sapply(seq_len(n_dep), function(d) {
      k <- idx == d
      100 * sum(pesos[k] * pm[k])
    })
  }

  nll <- function(par) {
    beta <- par[1:p]
    tau2 <- exp(par[p + 1])^2
    mu <- agregar(beta)
    V <- vardir + tau2

    if (any(!is.finite(mu)) || any(V <= 0)) return(1e10)

    0.5 * sum(log(2 * pi * V) + (y_dep - mu)^2 / V)
  }

  mejor <- NULL
  historial_nll <- rep(NA_real_, n_starts)
  historial_conv <- rep(NA_integer_, n_starts)

  for (s in seq_len(n_starts)) {

    par0 <- rep(0, p + 1)
    par0[1] <- logit(0.12)
    par0[p + 1] <- 0

    if (s > 1) {
      par0[1:p] <- par0[1:p] + rnorm(p, 0, 0.4)
      par0[p + 1] <- log(runif(1, 0.3, 3))
    }

    r <- tryCatch({
      r1 <- optim(
        par0, nll, method = "Nelder-Mead",
        control = list(maxit = 15000, reltol = 1e-10)
      )
      optim(
        r1$par, nll, method = "BFGS",
        control = list(maxit = 4000)
      )
    }, error = function(e) NULL)

    if (!is.null(r) && is.finite(r$value)) {
      historial_nll[s] <- r$value
      historial_conv[s] <- r$convergence

      if (is.null(mejor) || r$value < mejor$value) {
        mejor <- r
      }
    }
  }

  if (is.null(mejor)) return(NULL)

  beta <- mejor$par[1:p]
  validos <- is.finite(historial_nll)
  nll_validos <- historial_nll[validos]

  # Numero de arranques que llegan esencialmente al mismo minimo.
  tol_optimo <- 1e-4
  n_cercanos_optimo <- if (length(nll_validos) > 0) {
    sum(abs(nll_validos - min(nll_validos)) <= tol_optimo)
  } else {
    0L
  }

  list(
    beta = beta,
    tau  = exp(mejor$par[p + 1]),
    nll  = mejor$value,
    eta  = as.numeric(Z %*% beta),
    convergencia = mejor$convergence,
    par_completo = mejor$par,
    historial_nll = historial_nll,
    historial_convergencia = historial_conv,
    n_starts = n_starts,
    n_starts_validos = sum(validos),
    n_cercanos_optimo = n_cercanos_optimo,
    prop_cercanos_optimo = if (sum(validos) > 0) {
      n_cercanos_optimo / sum(validos)
    } else {
      NA_real_
    }
  )
}


# ---------------------------------------------------------
# 7.2. Benchmarking
# ---------------------------------------------------------
#
# Para cada departamento se determina un desplazamiento en
# la escala del predictor tal que el promedio ponderado
# municipal reproduzca la estimación directa. La función es
# continua y estrictamente creciente, de modo que la
# solución existe y es única.
# ---------------------------------------------------------

benchmark_logit <- function(eta, idx, pesos, y_dep, n_dep) {

  delta <- numeric(n_dep)

  for (d in seq_len(n_dep)) {
    k <- idx == d
    e <- eta[k]
    w <- pesos[k]
    objetivo <- y_dep[d] / 100

    f <- function(s) sum(w * inv_logit(e + s)) - objetivo

    delta[d] <- tryCatch(
      uniroot(f, interval = c(-25, 25), tol = 1e-12)$root,
      error = function(err) 0
    )
  }

  delta
}


# ---------------------------------------------------------
# 7.3. Ajuste por escenario
# ---------------------------------------------------------

cat("=========================================\n")
cat("MODELO 2: AJUSTE\n")
cat("=========================================\n")

n_dep <- nrow(base)
ajuste_m2 <- list()
filas_m2  <- list()

for (nom in names(ESCENARIOS)) {

  covs <- ESCENARIOS[[nom]]

  # Estandarización con parámetros globales.
  Z <- cbind(1, sapply(covs, function(v) {
    (x_m2[[v]] - mean(x_m2[[v]])) / sd(x_m2[[v]])
  }))
  colnames(Z) <- c("(Intercept)", covs)

  r <- ajustar_m2(Z, idx_dep, x_m2$peso,
                  base$ipm_directo, base$vardir, n_dep)

  delta <- benchmark_logit(r$eta, idx_dep, x_m2$peso,
                           base$ipm_directo, n_dep)

  pred_mun <- 100 * inv_logit(r$eta + delta[idx_dep])

  # Verificación de coherencia del benchmarking.
  agregado <- sapply(seq_len(n_dep), function(d) {
    k <- idx_dep == d
    sum(x_m2$peso[k] * pred_mun[k])
  })
  error_bench <- max(abs(agregado - base$ipm_directo))

  ajuste_m2[[nom]] <- list(
    beta = r$beta,
    tau = r$tau,
    eta = r$eta,
    delta = delta,
    pred_mun = pred_mun,
    Z = Z,
    nll = r$nll,
    convergencia = r$convergencia,
    par_completo = r$par_completo,
    historial_nll = r$historial_nll,
    historial_convergencia = r$historial_convergencia,
    n_starts = r$n_starts,
    n_starts_validos = r$n_starts_validos,
    n_cercanos_optimo = r$n_cercanos_optimo,
    prop_cercanos_optimo = r$prop_cercanos_optimo,
    error_benchmark = error_bench
  )

  filas_m2[[nom]] <- data.frame(
    escenario        = nom,
    n_cov            = length(covs),
    NLL              = round(r$nll, 3),
    tau              = round(r$tau, 4),
    min_pred         = round(min(pred_mun), 3),
    max_pred         = round(max(pred_mun), 3),
    fuera_rango      = sum(pred_mun < 0 | pred_mun > 100),
    error_benchmark  = signif(error_bench, 3),
    row.names        = NULL
  )
}

tabla_m2 <- do.call(rbind, filas_m2)
print(tabla_m2)
cat("\n")

cat("NOTA: el error de benchmarking debe ser numéricamente\n")
cat("nulo. Un valor apreciable indica que la raíz no se\n")
cat("encontró en algún dominio.\n\n")



# =========================================================
# 7.A. VALIDACIÓN INTERNA Y CONDICIONES DE VALIDEZ - M2
# =========================================================
#
# CONDICIONES EVALUADAS:
# 1. Convergencia del optimizador.
# 2. Parámetros finitos y tau no negativo.
# 3. Estabilidad frente a múltiples puntos iniciales.
# 4. Identificación local mediante Hessiano numérico.
# 5. Suficiente variación de las covariables.
# 6. Predicciones estrictamente acotadas en [0,100].
# 7. Benchmarking numéricamente exacto.
# 8. Diagnóstico de residuos departamentales PRE-benchmark.
# 9. Relación número de dominios / parámetros.
#
# El benchmarking NO se utiliza para demostrar capacidad
# predictiva; por eso los residuos aquí examinados incluyen
# también la versión PRE-benchmark y LODO se ejecuta después.
# =========================================================

cat("\n=========================================\n")
cat("VALIDACIÓN INTERNA MODELO 2 - NO LINEAL\n")
cat("=========================================\n")

validacion_m2 <- list()

for (nom in names(ESCENARIOS)) {

  covs <- ESCENARIOS[[nom]]
  obj <- ajuste_m2[[nom]]

  Z <- obj$Z
  p <- ncol(Z)

  # Variacion de covariables en la base municipal.
  sd_covs <- sapply(covs, function(v) sd(x_m2[[v]], na.rm = TRUE))
  covariables_con_variacion <- all(is.finite(sd_covs) & sd_covs > 0)

  # Prediccion departamental antes del benchmarking.
  pm_pre <- inv_logit(obj$eta)
  agregado_pre <- sapply(seq_len(n_dep), function(d) {
    k <- idx_dep == d
    100 * sum(x_m2$peso[k] * pm_pre[k])
  })

  resid_pre <- base$ipm_directo - agregado_pre
  V_pre <- base$vardir + obj$tau^2
  resid_pre_std <- resid_pre / sqrt(pmax(V_pre, .Machine$double.eps))

  p_shapiro <- if (length(resid_pre_std) >= 3 && length(resid_pre_std) <= 5000) {
    tryCatch(shapiro.test(resid_pre_std)$p.value, error = function(e) NA_real_)
  } else {
    NA_real_
  }

  n_resid_std_gt3 <- sum(abs(resid_pre_std) > 3, na.rm = TRUE)

  # Reconstrucción de la función objetivo para evaluar Hessiano.
  agregar_local <- function(beta) {
    pm <- inv_logit(as.numeric(Z %*% beta))
    sapply(seq_len(n_dep), function(d) {
      k <- idx_dep == d
      100 * sum(x_m2$peso[k] * pm[k])
    })
  }

  nll_local <- function(par) {
    beta <- par[1:p]
    tau2 <- exp(par[p + 1])^2
    mu <- agregar_local(beta)
    V <- base$vardir + tau2

    if (any(!is.finite(mu)) || any(!is.finite(V)) || any(V <= 0)) {
      return(1e10)
    }

    0.5 * sum(log(2 * pi * V) + (base$ipm_directo - mu)^2 / V)
  }

  par_hat <- c(obj$beta, log(pmax(obj$tau, .Machine$double.eps)))

  H <- tryCatch(
    optimHess(par_hat, nll_local),
    error = function(e) NULL
  )

  if (!is.null(H) && all(is.finite(H))) {
    eig_H <- tryCatch(
      eigen((H + t(H)) / 2, symmetric = TRUE, only.values = TRUE)$values,
      error = function(e) rep(NA_real_, length(par_hat))
    )
  } else {
    eig_H <- rep(NA_real_, length(par_hat))
  }

  hessiano_pd <- all(is.finite(eig_H)) && min(eig_H) > 1e-8
  min_eigen_H <- if (all(is.finite(eig_H))) min(eig_H) else NA_real_
  condicion_H <- if (hessiano_pd) max(eig_H) / min(eig_H) else NA_real_

  convergencia_ok <- isTRUE(obj$convergencia == 0)
  parametros_finitos <- all(is.finite(obj$beta)) && is.finite(obj$tau) && obj$tau >= 0
  rango_ok <- all(is.finite(obj$pred_mun)) &&
    all(obj$pred_mun >= 0 & obj$pred_mun <= 100)
  benchmark_ok <- is.finite(obj$error_benchmark) && obj$error_benchmark < 1e-6

  n_starts_validos <- obj$n_starts_validos
  prop_cercanos <- obj$prop_cercanos_optimo
  multistart_ok <- is.finite(n_starts_validos) &&
    n_starts_validos >= 2 &&
    is.finite(prop_cercanos) &&
    prop_cercanos >= 0.25

  obs_por_param <- n_dep / (length(covs) + 2)

  # Criterios duros del M2.
  apto_estructural <- all(
    convergencia_ok,
    parametros_finitos,
    covariables_con_variacion,
    rango_ok,
    benchmark_ok
  )

  advertencias <- character(0)

  if (!multistart_ok) {
    advertencias <- c(advertencias, "Estabilidad multistart limitada")
  }
  if (!hessiano_pd) {
    advertencias <- c(advertencias, "Hessiano no definido positivo")
  } else if (is.finite(condicion_H) && condicion_H > 1e8) {
    advertencias <- c(advertencias, "Hessiano mal condicionado")
  }
  if (obs_por_param < 5) {
    advertencias <- c(advertencias, "Menos de 5 dominios por parámetro")
  }
  if (is.finite(p_shapiro) && p_shapiro < 0.05) {
    advertencias <- c(advertencias, "Normalidad aproximada residual cuestionada")
  }
  if (n_resid_std_gt3 > 0) {
    advertencias <- c(advertencias, "Residuos pre-benchmark estandarizados >3")
  }

  estado <- if (!apto_estructural) {
    "NO_APTO"
  } else if (length(advertencias) > 0) {
    "APTO_CON_ADVERTENCIAS"
  } else {
    "APTO"
  }

  validacion_m2[[nom]] <- data.frame(
    escenario = nom,
    modelo = "M2_NoLineal",
    convergencia = obj$convergencia,
    convergencia_ok = convergencia_ok,
    tau = obj$tau,
    parametros_finitos = parametros_finitos,
    covariables_con_variacion = covariables_con_variacion,
    n_starts = obj$n_starts,
    n_starts_validos = n_starts_validos,
    n_cercanos_optimo = obj$n_cercanos_optimo,
    prop_cercanos_optimo = prop_cercanos,
    estabilidad_multistart_ok = multistart_ok,
    hessiano_PD = hessiano_pd,
    min_eigen_H = min_eigen_H,
    condicion_H = condicion_H,
    dominios_por_parametro = obs_por_param,
    Shapiro_resid_pre_std_p = p_shapiro,
    n_resid_pre_std_abs_gt3 = n_resid_std_gt3,
    rango_0_100_ok = rango_ok,
    error_benchmark = obj$error_benchmark,
    benchmark_ok = benchmark_ok,
    apto_pre_lodo = apto_estructural,
    estado_pre_lodo = estado,
    advertencias = if (length(advertencias) == 0) "Ninguna" else paste(advertencias, collapse = "; "),
    row.names = NULL
  )
}

tabla_validacion_m2 <- dplyr::bind_rows(validacion_m2)
print(tabla_validacion_m2)

cat("\nInterpretación M2:\n")
cat("- Convergencia, parámetros finitos, variación de X, rango y benchmarking\n")
cat("  son condiciones estructurales para continuar.\n")
cat("- Multistart, Hessiano, residuos y dominios por parámetro son diagnósticos\n")
cat("  de estabilidad/identificación que deben quedar documentados.\n\n")

# =========================================================
# 8. VALIDACIÓN POR EXCLUSIÓN DE DOMINIO
# =========================================================
#
# ADVERTENCIA METODOLÓGICA
#
# El dominio excluido NO recibe benchmarking. Ese
# procedimiento emplea la estimación directa del dominio,
# que es precisamente la cantidad que se busca predecir.
# Aplicarlo contaminaría la métrica y haría que los
# resultados del Modelo 2 no fueran comparables con los del
# Modelo 1.
#
# Del mismo modo, la estandarización se recalcula en cada
# iteración con municipios de entrenamiento únicamente.
#
# CORRECCIÓN TÉCNICA
#
# Las matrices Z de entrenamiento y prueba se construyen
# explícitamente como matrices para evitar que sapply()
# simplifique el resultado cuando el departamento excluido
# contiene un solo municipio (por ejemplo Bogotá D.C.).
# =========================================================

cat("=========================================\n")
cat("VALIDACIÓN LODO\n")
cat("=========================================\n")

resultados_lodo <- list()
detalle_lodo    <- list()


# ---------------------------------------------------------
# 8.1. Función robusta para construir matrices Z
# ---------------------------------------------------------

construir_Z <- function(datos, covs, medias, desvs) {

  X <- as.matrix(
    datos[, covs, drop = FALSE]
  )

  storage.mode(X) <- "double"

  if (length(medias) != length(covs) ||
      length(desvs) != length(covs)) {
    stop("Longitud incompatible entre covariables, medias y desviaciones.")
  }

  if (any(!is.finite(medias))) {
    stop("Se encontraron medias no finitas durante la estandarización LODO.")
  }

  if (any(!is.finite(desvs)) || any(desvs <= 0)) {
    stop("Se encontraron desviaciones estándar no válidas durante la estandarización LODO.")
  }

  X_std <- sweep(
    X,
    MARGIN = 2,
    STATS = medias,
    FUN = "-"
  )

  X_std <- sweep(
    X_std,
    MARGIN = 2,
    STATS = desvs,
    FUN = "/"
  )

  Z <- cbind(
    "(Intercept)" = 1,
    X_std
  )

  colnames(Z) <- c(
    "(Intercept)",
    covs
  )

  return(Z)
}


# ---------------------------------------------------------
# 8.2. LODO por escenario
# ---------------------------------------------------------

for (nom in names(ESCENARIOS)) {

  cat("\n-----------------------------------------\n")
  cat("Escenario:", nom, "\n")
  cat("-----------------------------------------\n")

  covs <- ESCENARIOS[[nom]]

  f <- reformulate(
    termlabels = covs,
    response = "ipm_directo"
  )

  f_x <- reformulate(
    termlabels = covs
  )

  pred_m1 <- rep(NA_real_, n_dep)
  pred_m2 <- rep(NA_real_, n_dep)


  # -------------------------------------------------------
  # Excluir un departamento en cada iteración
  # -------------------------------------------------------

  for (i in seq_len(n_dep)) {

    cat(
      "  Excluyendo:",
      base$departamento[i],
      "\n"
    )

    entrena_dep <- setdiff(
      seq_len(n_dep),
      i
    )

    base_tr <- base[
      entrena_dep,
      ,
      drop = FALSE
    ]


    # =====================================================
    # MODELO 1
    # =====================================================

    fit_i <- tryCatch(

      sae::eblupFH(
        formula = f,
        vardir = vardir,
        method = "REML",
        data = base_tr
      ),

      error = function(e) {

        cat(
          "    M1 no convergió:",
          conditionMessage(e),
          "\n"
        )

        NULL
      }
    )

    if (!is.null(fit_i)) {

      beta_i <- as.numeric(
        fit_i$fit$estcoef$beta
      )

      X_i <- model.matrix(
        f_x,
        data = base[
          i,
          ,
          drop = FALSE
        ]
      )

      if (ncol(X_i) == length(beta_i)) {
        pred_m1[i] <- as.numeric(
          X_i %*% beta_i
        )
      } else {
        cat(
          "    M1 omitido por incompatibilidad de dimensiones:",
          ncol(X_i), "columnas X frente a",
          length(beta_i), "betas\n"
        )
      }
    }


    # =====================================================
    # MODELO 2
    # =====================================================

    mask_tr <- idx_dep != i

    mun_tr <- x_m2[
      mask_tr,
      ,
      drop = FALSE
    ]

    mun_te <- x_m2[
      !mask_tr,
      ,
      drop = FALSE
    ]


    if (nrow(mun_te) == 0) {

      cat(
        "    ADVERTENCIA: no hay municipios de prueba para",
        base$departamento[i],
        "\n"
      )

      next
    }


    # -----------------------------------------------------
    # Medias y desviaciones SOLO con entrenamiento
    # -----------------------------------------------------

    medias <- sapply(
      covs,
      function(v) {
        mean(
          mun_tr[[v]],
          na.rm = TRUE
        )
      }
    )

    desvs <- sapply(
      covs,
      function(v) {
        sd(
          mun_tr[[v]],
          na.rm = TRUE
        )
      }
    )

    medias <- as.numeric(medias)
    desvs  <- as.numeric(desvs)

    names(medias) <- covs
    names(desvs)  <- covs


    # -----------------------------------------------------
    # Matrices robustas de entrenamiento y prueba
    # -----------------------------------------------------

    Z_tr <- construir_Z(
      datos = mun_tr,
      covs = covs,
      medias = medias,
      desvs = desvs
    )

    Z_te <- construir_Z(
      datos = mun_te,
      covs = covs,
      medias = medias,
      desvs = desvs
    )


    if (ncol(Z_tr) != length(covs) + 1) {
      stop(
        paste(
          "Dimensión incorrecta de Z_tr en",
          nom,
          "para",
          base$departamento[i]
        )
      )
    }

    if (ncol(Z_te) != length(covs) + 1) {
      stop(
        paste(
          "Dimensión incorrecta de Z_te en",
          nom,
          "para",
          base$departamento[i]
        )
      )
    }


    # -----------------------------------------------------
    # Índices departamentales de entrenamiento
    # -----------------------------------------------------

    idx_tr <- match(
      mun_tr$cod_depto,
      base_tr$cod_depto
    )

    if (any(is.na(idx_tr))) {
      stop(
        paste(
          "Se encontraron municipios de entrenamiento sin",
          "correspondencia departamental en",
          nom,
          "al excluir",
          base$departamento[i]
        )
      )
    }


    # -----------------------------------------------------
    # Ajuste M2 sin el dominio excluido
    # -----------------------------------------------------

    r_i <- tryCatch(

      ajustar_m2(
        Z_tr,
        idx_tr,
        mun_tr$peso,
        base_tr$ipm_directo,
        base_tr$vardir,
        nrow(base_tr),
        n_starts = 5,
        semilla = 5000 + i
      ),

      error = function(e) {

        cat(
          "    M2 no convergió:",
          conditionMessage(e),
          "\n"
        )

        NULL
      }
    )


    # -----------------------------------------------------
    # Predicción del dominio excluido SIN benchmarking
    # -----------------------------------------------------

    if (!is.null(r_i)) {

      if (ncol(Z_te) != length(r_i$beta)) {

        stop(
          paste0(
            "\nIncompatibilidad de dimensiones en M2 LODO.\n",
            "Escenario: ", nom, "\n",
            "Departamento: ", base$departamento[i], "\n",
            "Municipios de prueba: ", nrow(mun_te), "\n",
            "Columnas Z_te: ", ncol(Z_te), "\n",
            "Betas: ", length(r_i$beta), "\n"
          )
        )
      }

      eta_te <- as.numeric(
        Z_te %*% r_i$beta
      )

      pm <- inv_logit(
        eta_te
      )

      # x_m2 conserva pesos normalizados dentro de cada
      # departamento. Aun así se normalizan nuevamente para
      # hacer la operación explícita y robusta.
      pesos_te <- mun_te$peso

      if (!all(is.finite(pesos_te)) ||
          sum(pesos_te, na.rm = TRUE) <= 0) {

        cat(
          "    M2 omitido: pesos no válidos en",
          base$departamento[i],
          "\n"
        )

      } else {

        pesos_te <- pesos_te / sum(
          pesos_te,
          na.rm = TRUE
        )

        pred_m2[i] <- 100 * sum(
          pesos_te * pm,
          na.rm = TRUE
        )
      }
    }
  }


  # -------------------------------------------------------
  # 8.3. Detalle departamental
  # -------------------------------------------------------

  detalle_lodo[[nom]] <- data.frame(
    cod_depto = base$cod_depto,

    departamento =
      base$departamento,

    ipm_directo =
      base$ipm_directo,

    M1 = round(
      pred_m1,
      4
    ),

    M2 = round(
      pred_m2,
      4
    ),

    error_M1 = round(
      pred_m1 - base$ipm_directo,
      4
    ),

    error_M2 = round(
      pred_m2 - base$ipm_directo,
      4
    )
  )


  # -------------------------------------------------------
  # 8.4. Métricas
  # -------------------------------------------------------

  for (mod in c("M1", "M2")) {

    pr <- if (mod == "M1") {
      pred_m1
    } else {
      pred_m2
    }

    ok <- is.finite(pr) &
      is.finite(base$ipm_directo)

    if (sum(ok) < 3) {

      warning(
        paste(
          "Muy pocas predicciones válidas para",
          mod,
          "en",
          nom
        )
      )

      next
    }

    nombre_resultado <- paste(
      nom,
      mod
    )

    resultados_lodo[[nombre_resultado]] <- data.frame(

      escenario = nom,

      modelo = ifelse(
        mod == "M1",
        "M1_FayHerriot",
        "M2_NoLineal"
      ),

      RMSE = round(
        rmse(
          base$ipm_directo[ok],
          pr[ok]
        ),
        4
      ),

      MAE = round(
        mae(
          base$ipm_directo[ok],
          pr[ok]
        ),
        4
      ),

      correlacion = round(
        cor(
          base$ipm_directo[ok],
          pr[ok]
        ),
        4
      ),

      fuera_rango = sum(
        pr[ok] < 0 |
          pr[ok] > 100
      ),

      n_validos = sum(ok),

      row.names = NULL
    )
  }

  cat(
    "\n",
    nom,
    "completado.\n"
  )
}


# =========================================================
# 8.5. CONSOLIDAR RESULTADOS LODO
# =========================================================

if (length(resultados_lodo) == 0) {
  stop("No se generaron resultados válidos en la validación LODO.")
}

tabla_lodo <- do.call(
  rbind,
  resultados_lodo
)

rownames(
  tabla_lodo
) <- NULL

cat("\n=========================================\n")
cat("RESULTADOS VALIDACIÓN LODO\n")
cat("=========================================\n")

print(
  tabla_lodo[
    order(
      tabla_lodo$RMSE
    ),
    ,
    drop = FALSE
  ]
)

cat("\n")


# =========================================================
# 8.6. ESTABILIDAD ENTRE ESCENARIOS
# =========================================================

for (
  mod in unique(
    tabla_lodo$modelo
  )
) {

  s <- tabla_lodo$RMSE[
    tabla_lodo$modelo == mod
  ]

  s <- s[
    is.finite(s)
  ]

  if (length(s) > 0) {

    cat(
      mod,
      ": RMSE entre",
      round(
        min(s),
        2
      ),
      "y",
      round(
        max(s),
        2
      ),
      " amplitud",
      round(
        diff(
          range(s)
        ),
        2
      ),
      "\n"
    )
  }
}

cat("\n")



# =========================================================
# 8.7. VALIDACIÓN FINAL M1-M2 Y PUERTA DE COMPARACIÓN
# =========================================================
#
# La validación final integra:
# - condiciones internas / numéricas;
# - disponibilidad de predicciones LODO para los 33 dominios;
# - métricas predictivas finitas.
#
# Solo los escenarios en los que M1 y M2 son APTOS para
# comparación pasan a las secciones comparativas posteriores.
# Las advertencias no eliminan automáticamente un escenario,
# pero quedan registradas para la discusión metodológica.
# =========================================================

cat("\n=========================================\n")
cat("VALIDACIÓN FINAL PREVIA A LA COMPARACIÓN\n")
cat("=========================================\n")

tabla_validez_final_m12 <- dplyr::bind_rows(
  tabla_validacion_m1 %>%
    dplyr::select(
      escenario, modelo, apto_pre_lodo, estado_pre_lodo, advertencias
    ),
  tabla_validacion_m2 %>%
    dplyr::select(
      escenario, modelo, apto_pre_lodo, estado_pre_lodo, advertencias
    )
) %>%
  dplyr::left_join(
    tabla_lodo %>%
      dplyr::select(
        escenario, modelo, RMSE, MAE, correlacion, n_validos
      ),
    by = c("escenario", "modelo")
  ) %>%
  dplyr::mutate(
    lodo_completo = is.finite(n_validos) & n_validos == n_dep,
    metricas_finitas = is.finite(RMSE) & is.finite(MAE) & is.finite(correlacion),
    apto_comparacion = apto_pre_lodo & lodo_completo & metricas_finitas,
    estado_final = dplyr::case_when(
      !apto_pre_lodo ~ "NO_APTO",
      !lodo_completo ~ "NO_APTO_LODO_INCOMPLETO",
      !metricas_finitas ~ "NO_APTO_METRICAS",
      estado_pre_lodo == "APTO_CON_ADVERTENCIAS" ~ "APTO_CON_ADVERTENCIAS",
      TRUE ~ "APTO"
    )
  )

print(tabla_validez_final_m12)

# Escenario válido únicamente si ambos modelos pasan la puerta.
escenarios_validos_df <- tabla_validez_final_m12 %>%
  dplyr::group_by(escenario) %>%
  dplyr::summarise(
    n_modelos = dplyr::n_distinct(modelo),
    todos_aptos = all(apto_comparacion),
    .groups = "drop"
  ) %>%
  dplyr::filter(n_modelos == 2, todos_aptos)

ESCENARIOS_VALIDOS_M12 <- escenarios_validos_df$escenario

cat("\nEscenarios M1-M2 habilitados para comparación:\n")
print(ESCENARIOS_VALIDOS_M12)

if (length(ESCENARIOS_VALIDOS_M12) == 0) {
  stop(
    paste(
      "Ningún escenario superó la validación mínima simultánea de M1 y M2.",
      "Revise tabla_validez_final_m12 antes de comparar modelos."
    )
  )
}

tabla_lodo_validada <- tabla_lodo %>%
  dplyr::filter(escenario %in% ESCENARIOS_VALIDOS_M12)

cat("\nLa comparación posterior utilizará únicamente escenarios validados.\n\n")

# =========================================================
# 9. COMPARACIÓN MUNICIPAL ENTRE MODELOS
# =========================================================

cat("=========================================\n")
cat("COMPARACIÓN MUNICIPAL\n")
cat("=========================================\n")

filas_comp <- list()

for (nom in ESCENARIOS_VALIDOS_M12) {

  m1 <- ajuste_m1[[nom]]$pred_mun
  m1_acotado <- pmin(pmax(m1, 0), 100)
  m2 <- ajuste_m2[[nom]]$pred_mun

  filas_comp[[nom]] <- data.frame(
    escenario       = nom,
    correlacion     = round(cor(m1_acotado, m2), 4),
    dif_media_abs   = round(mean(abs(m1_acotado - m2)), 3),
    dif_maxima      = round(max(abs(m1_acotado - m2)), 3),
    M1_negativos    = sum(m1 < 0),
    row.names       = NULL
  )
}

tabla_comparacion <- do.call(rbind, filas_comp)
print(tabla_comparacion)
cat("\n")

# Estimaciones municipales del escenario de referencia.
ESC_REF <- if ("E2_Parsimoniosa" %in% ESCENARIOS_VALIDOS_M12) {
  "E2_Parsimoniosa"
} else {
  ESCENARIOS_VALIDOS_M12[1]
}

estimaciones <- data.frame(
  cod_depto = x_m2$cod_depto,
  depto     = x_m2$depto,
  cod_mpio  = x_m2$cod_mpio,
  mpio      = x_m2$mpio,
  poblacion = x_m2$poblacion,
  IPM_M1    = round(pmin(pmax(
    ajuste_m1[[ESC_REF]]$pred_mun, 0), 100), 3),
  IPM_M2    = round(ajuste_m2[[ESC_REF]]$pred_mun, 3)
)
estimaciones$diferencia <- round(
  estimaciones$IPM_M2 - estimaciones$IPM_M1, 3)


# =========================================================
# 10. DIAGNÓSTICO ESPACIAL
# =========================================================

cat("=========================================\n")
cat("DIAGNÓSTICO ESPACIAL\n")
cat("=========================================\n")

if (all(c("LATITUD", "LONGITUD") %in% names(
  read_excel(RUTA_IPM, sheet = "Sheet1")))) {

  coords_raw <- read_excel(RUTA_IPM, sheet = "Sheet1")
  coords_raw <- coords_raw[, c("cod_depto", "LATITUD", "LONGITUD")]
  coords_raw$cod_depto <- as.integer(coords_raw$cod_depto)

  base_esp <- merge(base, coords_raw, by = "cod_depto")
  base_esp <- base_esp[order(base_esp$cod_depto), ]
  coords <- as.matrix(base_esp[, c("LONGITUD", "LATITUD")])

  filas_esp <- list()

  for (k in c(3, 4, 5)) {

    vecinos <- knn2nb(knearneigh(coords, k = k))
    vecinos <- make.sym.nb(vecinos)
    W <- nb2listw(vecinos, style = "W")

    for (nom in names(ESCENARIOS)) {

      covs <- ESCENARIOS[[nom]]
      f <- as.formula(paste("ipm_directo ~",
                            paste(covs, collapse = " + ")))

      m_ols <- lm(f, data = base_esp)

      mi <- moran.mc(residuals(m_ols), W, nsim = 9999)

      m_sar <- lagsarlm(f, data = base_esp, listw = W)
      m_sem <- errorsarlm(f, data = base_esp, listw = W)

      filas_esp[[paste(k, nom)]] <- data.frame(
        vecinos     = k,
        escenario   = nom,
        Moran_I     = round(mi$statistic, 4),
        valor_p     = round(mi$p.value, 4),
        AIC_OLS     = round(AIC(m_ols), 2),
        AIC_SAR     = round(AIC(m_sar), 2),
        rho_SAR     = round(m_sar$rho, 3),
        AIC_SEM     = round(AIC(m_sem), 2),
        lambda_SEM  = round(m_sem$lambda, 3),
        row.names   = NULL
      )
    }
  }

  tabla_espacial <- do.call(rbind, filas_esp)
  rownames(tabla_espacial) <- NULL
  print(tabla_espacial)
  cat("\n")

  cat("NOTA: la significancia de la autocorrelación residual\n")
  cat("puede variar entre escenarios. Un escenario sin\n")
  cat("autocorrelación significativa indica que sus\n")
  cat("covariables ya capturan la estructura territorial.\n\n")

} else {
  tabla_espacial <- NULL
  cat("Coordenadas no disponibles. Sección omitida.\n\n")
}


# =========================================================
# 11. CARTOGRAFÍA M1-M2
# =========================================================
#
# Se utiliza la capa municipal MGN_ADM_MPIO_GRAFICO.shp.
# Como control de consistencia, se conservan únicamente los códigos
# presentes en la base municipal analítica.
# =========================================================

if (file.exists(RUTA_SHP)) {

  cat("=========================================\n")
  cat("CARTOGRAFÍA M1-M2\n")
  cat("=========================================\n")

  mapa <- sf::st_read(RUTA_SHP, quiet = TRUE)
  mapa$cod_mpio <- obtener_codigo_municipal(mapa)

  mapa <- mapa %>%
    dplyr::filter(cod_mpio %in% estimaciones$cod_mpio) %>%
    dplyr::left_join(
      estimaciones[, c("cod_mpio", "IPM_M1", "IPM_M2", "diferencia")],
      by = "cod_mpio"
    )

  # Cortes comunes a los tres modelos para comparación visual válida.
  CORTES <- c(0, 10, 20, 30, 40, 50, Inf)
  ETIQ <- c("0 a 10", "10 a 20", "20 a 30",
            "30 a 40", "40 a 50", "50 o más")

  PALETA <- c("#FDE725", "#7AD151", "#22A884",
              "#2A788E", "#414487", "#440154")

  for (col in c("IPM_M1", "IPM_M2")) {

    mapa$clase <- cut(
      mapa[[col]],
      breaks = CORTES,
      labels = ETIQ,
      include.lowest = TRUE
    )

    g <- ggplot(mapa) +
      geom_sf(aes(fill = clase), color = "white", linewidth = 0.05) +
      scale_fill_manual(
        values = PALETA,
        na.value = "#D9D9D9",
        name = "IPM estimado (%)",
        na.translate = TRUE,
        drop = FALSE
      ) +
      theme_void(base_size = 13) +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 12)
      )

    ggsave(
      file.path(CARPETA_SALIDA, paste0("mapa_", col, ".png")),
      g, width = 7, height = 8.5, dpi = 300, bg = "white"
    )

    cat("  mapa_", col, ".png generado\n", sep = "")
  }
  cat("\n")
}


# =========================================================
# 12. EXPORTACIÓN
# =========================================================

wb <- createWorkbook()

hojas <- list(
  Control_escalas       = tabla_control_escalas,
  Auditoria_escalas_2024 = tabla_auditoria_escalas,
  Diagnostico_OLS       = tabla_diagnostico,
  Ajuste_Modelo1        = tabla_m1,
  Validez_interna_M1    = tabla_validacion_m1,
  Ajuste_Modelo2        = tabla_m2,
  Validez_interna_M2    = tabla_validacion_m2,
  LODO                  = tabla_lodo,
  Validez_final_M1_M2   = tabla_validez_final_m12,
  LODO_validada         = tabla_lodo_validada,
  Comparacion_municipal = tabla_comparacion,
  Estimaciones          = estimaciones
)

if (!is.null(tabla_espacial)) {
  hojas$Diagnostico_espacial <- tabla_espacial
}

for (k in names(detalle_lodo)) {
  hojas[[substr(paste0("LODO_", k), 1, 31)]] <- detalle_lodo[[k]]
}

for (nom in names(hojas)) {
  addWorksheet(wb, nom)
  writeData(wb, nom, hojas[[nom]])
  freezePane(wb, nom, firstRow = TRUE)
  setColWidths(wb, nom, cols = seq_len(ncol(hojas[[nom]])),
               widths = "auto")
}

archivo <- file.path(CARPETA_SALIDA, "resultados_SAE_IPM_intermedio_M1_M2.xlsx")
saveWorkbook(wb, archivo, overwrite = TRUE)

cat("=========================================\n")
cat("Resultados exportados en:\n")
cat(archivo, "\n")
cat("=========================================\n")


# =========================================================
# 13. RESUMEN DE ADVERTENCIAS
# =========================================================

if (length(problemas) > 0) {
  cat("\nADVERTENCIAS DE INTEGRIDAD DE DATOS:\n")
  for (p in problemas) cat("  -", p, "\n")
  cat("\nRevise el cruce de las columnas señaladas antes de\n")
  cat("emplear los resultados.\n")
}



# ==============================================================================
# 14. MODELO 3: DESAGREGACION ESPACIAL MUNICIPAL CON EFECTO CAR REDUCIDO
# ==============================================================================
#
# Reformulación del tercer modelo:
# - utiliza exclusivamente información 2024, al igual que M1 y M2;
# - conserva la respuesta directa departamental y su varianza de muestreo;
# - modela el IPM municipal mediante una función logística acotada;
# - incorpora un efecto espacial municipal con estructura CAR en una
#   representación de rango reducido para hacer viable la estimación con
#   33 dominios departamentales;
# - aplica benchmarking únicamente a la estimación municipal final;
# - la validación LODO del dominio excluido se realiza SIN benchmarking.
#
# Esto permite que M1, M2 y M3 sean evaluados bajo el mismo esquema LODO 2024.
# ==============================================================================

cat("\n=========================================================\n")
cat("MODELO 3 - CAR MUNICIPAL REDUCIDO 2024\n")
cat("=========================================================\n")

# ------------------------------------------------------------------------------
# 14.1. Preparación espacial común
# ------------------------------------------------------------------------------


m3_shp <- sf::st_read(RUTA_SHP, quiet = TRUE)
m3_shp <- sf::st_make_valid(m3_shp)
m3_shp$cod_mpio <- obtener_codigo_municipal(m3_shp)

# IMPORTANTE:
# La capa MGN_ADM_MPIO_GRAFICO.shp corresponde al nivel municipal.
# Se conservan exclusivamente los códigos presentes en x_m2 para asegurar
# correspondencia exacta entre la base analítica y la red espacial.
m3_shp <- m3_shp %>%
  dplyr::filter(cod_mpio %in% x_m2$cod_mpio) %>%
  dplyr::distinct(cod_mpio, .keep_all = TRUE)

# Control de cobertura espacial.
# La capa MGN_ADM_MPIO_GRAFICO.shp contiene 1121 geometrías municipales.
# Para mantener la comparación estrictamente común entre M1, M2 y M3, todos
# los códigos de x_m2 deben contar con geometría.
faltan_geo_m3 <- sort(setdiff(x_m2$cod_mpio, m3_shp$cod_mpio))
extras_geo_m3 <- sort(setdiff(m3_shp$cod_mpio, x_m2$cod_mpio))

cat("Municipios/unidades en base M3:", nrow(x_m2), "\n")
cat("Geometrías municipales disponibles:", nrow(m3_shp), "\n")
cat("Municipios de x_m2 sin geometría:", length(faltan_geo_m3), "\n")
cat("Geometrías sin correspondencia en x_m2:", length(extras_geo_m3), "\n")

if (length(faltan_geo_m3) > 0) {
  stop(
    paste(
      "La capa municipal no cubre todos los códigos de x_m2. Faltan:",
      paste(faltan_geo_m3, collapse = ", ")
    )
  )
}

# Índices de correspondencia entre la capa municipal y x_m2.
# Con la capa verificada se espera correspondencia completa de las 1121 unidades.
idx_geo_x_m3 <- match(m3_shp$cod_mpio, x_m2$cod_mpio)

if (any(is.na(idx_geo_x_m3))) {
  stop("Se encontraron geometrías sin correspondencia en x_m2.")
}

# Ordenar la geometría según el orden de x_m2 para que la expansión posterior
# del efecto espacial sea determinista.
ord_geo_m3 <- order(idx_geo_x_m3)
m3_shp <- m3_shp[ord_geo_m3, ]
idx_geo_x_m3 <- idx_geo_x_m3[ord_geo_m3]

# Vecindad Queen únicamente entre unidades con geometría disponible.
vecinos_m3 <- spdep::poly2nb(
  sf::st_geometry(m3_shp),
  queen = TRUE,
  snap = 0.001
)

n_islas_red_m3 <- sum(spdep::card(vecinos_m3) == 0)
n_sin_geometria_m3 <- length(faltan_geo_m3)
n_islas_m3 <- n_islas_red_m3
n_componentes_m3 <- spdep::n.comp.nb(vecinos_m3)$nc

cat("Unidades sin vecinos dentro de la red espacial:", n_islas_red_m3, "\n")
cat("Unidades sin geometría:", n_sin_geometria_m3, "\n")
cat("Componentes conexos de la red con geometría:", n_componentes_m3, "\n")

# Matriz binaria de adyacencia para el subconjunto con geometría.
A_m3 <- spdep::nb2mat(
  vecinos_m3,
  style = "B",
  zero.policy = TRUE
)

A_m3 <- Matrix::Matrix(
  A_m3,
  sparse = TRUE
)

grados_m3 <- as.numeric(Matrix::rowSums(A_m3))
activos_geo_m3 <- which(grados_m3 > 0)

if (length(activos_geo_m3) < 10) {
  stop("La red municipal contiene muy pocos municipios conectados.")
}

# Posiciones correspondientes en la base municipal completa x_m2.
activos_x_m3 <- idx_geo_x_m3[activos_geo_m3]

# ------------------------------------------------------------------------------
# 14.2. Base espacial reducida tipo CAR
# ------------------------------------------------------------------------------
#
# Para una estructura CAR propia:
#     Q(rho) = D - rho A
#
# usando S = D^{-1/2} A D^{-1/2}, se tiene:
#     Q(rho) = D^{1/2}(I - rho S)D^{1/2}.
#
# La representación reducida conserva los K patrones espaciales asociados
# a los mayores autovalores de S, es decir, los patrones de autocorrelación
# positiva de escala más amplia.
#
# Esta reducción evita estimar un efecto aleatorio independiente para más
# de mil municipios a partir de únicamente 33 observaciones departamentales.
# ------------------------------------------------------------------------------

K_CAR <- 6L

Dinv_sqrt_act <- Matrix::Diagonal(
  x = 1 / sqrt(grados_m3[activos_geo_m3])
)

S_act <- Dinv_sqrt_act %*%
  A_m3[activos_geo_m3, activos_geo_m3, drop = FALSE] %*%
  Dinv_sqrt_act

K_CAR <- min(
  K_CAR,
  nrow(S_act) - 2L
)

eig_car <- RSpectra::eigs_sym(
  S_act,
  k = K_CAR,
  which = "LA"
)

lambda_car <- as.numeric(eig_car$values)
U_car <- as.matrix(eig_car$vectors)

# Escala de la base para una magnitud numéricamente estable.
B_car <- matrix(
  0,
  nrow = nrow(x_m2),
  ncol = K_CAR
)

B_car[activos_x_m3, ] <- sweep(
  U_car,
  1,
  sqrt(grados_m3[activos_geo_m3]),
  FUN = "/"
) * sqrt(length(activos_geo_m3))

colnames(B_car) <- paste0("CAR_", seq_len(K_CAR))

cat("Rango de autovalores CAR usados:",
    round(min(lambda_car), 4), "a",
    round(max(lambda_car), 4), "\n")
cat("Rango reducido K =", K_CAR, "\n\n")


# ------------------------------------------------------------------------------
# 14.3. Matriz de agregación municipal -> departamental
# ------------------------------------------------------------------------------

H_m3 <- matrix(
  0,
  nrow = n_dep,
  ncol = nrow(x_m2)
)

for (d in seq_len(n_dep)) {
  k <- idx_dep == d
  H_m3[d, k] <- x_m2$peso[k] / sum(x_m2$peso[k], na.rm = TRUE)
}

if (max(abs(rowSums(H_m3) - 1)) > 1e-10) {
  stop("Los pesos departamentales de M3 no suman uno.")
}


# ------------------------------------------------------------------------------
# 14.4. Función de ajuste M3 CAR reducido
# ------------------------------------------------------------------------------
#
# Predictor municipal:
#   eta_i = Z_i beta + B_i a
#   p_i   = logit^{-1}(eta_i)
#   theta_i = 100 p_i
#
# Observación departamental:
#   y_d ~ N(H_d theta, D_d + tau^2)
#
# Penalización CAR reducida:
#   a_k ~ N(0, 1 / q_k)
#   q_k = 1 - rho * lambda_k
#
# Se fija la escala base de la penalización CAR en 1 para evitar una
# varianza espacial adicional débilmente identificada con solo 33 dominios.
# La magnitud espacial efectiva queda determinada por los coeficientes a_k.
# ------------------------------------------------------------------------------

ajustar_m3_car <- function(
  Z,
  B,
  lambda,
  H,
  y_dep,
  vardir,
  rho_fijo,
  n_starts = 8,
  semilla = 2026
) {

  set.seed(semilla)

  p <- ncol(Z)
  K <- ncol(B)

  # -------------------------------------------------------
  # Control del valor de rho
  # -------------------------------------------------------

  if (
    !is.finite(rho_fijo) ||
    rho_fijo <= -0.95 ||
    rho_fijo >= 0.95
  ) {
    stop("rho_fijo debe estar estrictamente entre -0.95 y 0.95.")
  }

  q_fijo <- 1 - rho_fijo * lambda

  if (
    any(!is.finite(q_fijo)) ||
    any(q_fijo <= 1e-6)
  ) {
    stop(
      paste(
        "El valor rho =",
        rho_fijo,
        "no produce una precisión CAR válida."
      )
    )
  }

  # -------------------------------------------------------
  # Función objetivo
  # -------------------------------------------------------

  nll <- function(par) {

    beta <- par[seq_len(p)]

    # Parametrización no centrada:
    # z ~ N(0,I), a(rho) = z/sqrt(q).
    z <- par[p + seq_len(K)]

    log_tau <- par[p + K + 1]
    tau <- exp(log_tau)

    a <- z / sqrt(q_fijo)

    efecto_espacial <- as.numeric(
      B %*% a
    )

    eta <- as.numeric(
      Z %*% beta +
        efecto_espacial
    )

    pm <- inv_logit(eta)

    mu <- as.numeric(
      100 * H %*% pm
    )

    V <- vardir + tau^2

    if (
      any(!is.finite(mu)) ||
      any(!is.finite(V)) ||
      any(V <= 0)
    ) {
      return(1e12)
    }

    nll_datos <- 0.5 * sum(
      log(2 * pi * V) +
        (y_dep - mu)^2 / V
    )

    nll_car <- 0.5 * sum(
      z^2
    )

    nll_datos + nll_car
  }

  mejor <- NULL
  historial_nll <- rep(NA_real_, n_starts)
  historial_conv <- rep(NA_integer_, n_starts)

  beta0 <- rep(0, p)
  beta0[1] <- logit(0.15)

  for (s in seq_len(n_starts)) {

    z0 <- rep(0, K)
    tau0 <- 3

    if (s > 1) {
      beta0_s <- beta0 + rnorm(p, 0, 0.20)
      z0 <- rnorm(K, 0, 0.15)
      tau0 <- runif(1, 1, 6)
    } else {
      beta0_s <- beta0
    }

    par0 <- c(
      beta0_s,
      z0,
      log(tau0)
    )

    r <- tryCatch(
      {
        r1 <- optim(
          par0,
          nll,
          method = "Nelder-Mead",
          control = list(
            maxit = 12000,
            reltol = 1e-9
          )
        )

        optim(
          r1$par,
          nll,
          method = "BFGS",
          control = list(
            maxit = 5000,
            reltol = 1e-9
          )
        )
      },
      error = function(e) NULL
    )

    if (!is.null(r) && is.finite(r$value)) {

      historial_nll[s] <- r$value
      historial_conv[s] <- r$convergence

      if (is.null(mejor) || r$value < mejor$value) {
        mejor <- r
      }
    }
  }

  if (is.null(mejor)) {
    return(NULL)
  }

  beta <- mejor$par[seq_len(p)]
  z <- mejor$par[p + seq_len(K)]
  tau <- exp(mejor$par[p + K + 1])

  q <- q_fijo
  a <- z / sqrt(q)

  efecto_espacial <- as.numeric(
    B %*% a
  )

  eta <- as.numeric(
    Z %*% beta +
      efecto_espacial
  )

  validos <- is.finite(historial_nll)
  nll_validos <- historial_nll[validos]

  tol_optimo <- 1e-4

  n_cercanos_optimo <- if (length(nll_validos) > 0) {
    sum(
      abs(nll_validos - min(nll_validos)) <= tol_optimo
    )
  } else {
    0L
  }

  list(
    beta = beta,
    z = z,
    a = a,
    tau = tau,
    rho = rho_fijo,
    q = q,
    eta = eta,
    efecto_espacial = efecto_espacial,
    nll = mejor$value,
    convergencia = mejor$convergence,
    par_completo = mejor$par,
    historial_nll = historial_nll,
    historial_convergencia = historial_conv,
    n_starts = n_starts,
    n_starts_validos = sum(validos),
    n_cercanos_optimo = n_cercanos_optimo,
    prop_cercanos_optimo = if (sum(validos) > 0) {
      n_cercanos_optimo / sum(validos)
    } else {
      NA_real_
    },
    nll_fun = nll
  )
}


# ==============================================================================
# PARÁMETROS DE SENSIBILIDAD ESPACIAL M3
# ==============================================================================

RHO_GRID <- c(
  0.25,
  0.50,
  0.75,
  0.90
)

# Especificación de referencia definida a priori.
# Los demás valores se interpretan como análisis de sensibilidad.
RHO_REF <- 0.50

cat("\nValores de rho evaluados en M3:\n")
print(RHO_GRID)

cat(
  "rho de referencia para comparación principal:",
  RHO_REF,
  "\n\n"
)


# ==============================================================================
# 15. AJUSTE M3 CAR POR ESCENARIO Y RHO
# ==============================================================================

cat("\n=========================================================\n")
cat("MODELO 3: AJUSTE CAR 2024 - SENSIBILIDAD RHO\n")
cat("=========================================================\n")

ajuste_m3_sensibilidad <- list()
filas_m3_sensibilidad <- list()

for (nom in names(ESCENARIOS)) {

  covs <- ESCENARIOS[[nom]]

  medias_m3 <- sapply(
    covs,
    function(v) mean(x_m2[[v]], na.rm = TRUE)
  )

  desvs_m3 <- sapply(
    covs,
    function(v) sd(x_m2[[v]], na.rm = TRUE)
  )

  Z_m3 <- construir_Z(
    datos = x_m2,
    covs = covs,
    medias = medias_m3,
    desvs = desvs_m3
  )

  for (rho_actual in RHO_GRID) {

    cat(
      "\nEscenario:",
      nom,
      "- rho:",
      rho_actual,
      "\n"
    )

    r3 <- ajustar_m3_car(
      Z = Z_m3,
      B = B_car,
      lambda = lambda_car,
      H = H_m3,
      y_dep = base$ipm_directo,
      vardir = base$vardir,
      rho_fijo = rho_actual,
      n_starts = 8,
      semilla =
        8000 +
        100 * match(nom, names(ESCENARIOS)) +
        round(rho_actual * 100)
    )

    if (is.null(r3)) {

      warning(
        paste(
          "M3 CAR no logró ajustarse en",
          nom,
          "rho =",
          rho_actual
        )
      )

      next
    }

    pred_pre <- 100 * inv_logit(r3$eta)

    agregado_pre <- as.numeric(
      H_m3 %*% pred_pre
    )

    delta_m3 <- benchmark_logit(
      eta = r3$eta,
      idx = idx_dep,
      pesos = x_m2$peso,
      y_dep = base$ipm_directo,
      n_dep = n_dep
    )

    pred_final <- 100 * inv_logit(
      r3$eta +
        delta_m3[idx_dep]
    )

    agregado_post <- as.numeric(
      H_m3 %*% pred_final
    )

    error_bench <- max(
      abs(
        agregado_post -
          base$ipm_directo
      )
    )

    clave <- paste0(
      nom,
      "_rho_",
      format(
        rho_actual,
        nsmall = 2
      )
    )

    ajuste_m3_sensibilidad[[clave]] <- list(
      escenario = nom,
      rho = rho_actual,
      fit = r3,
      Z = Z_m3,
      medias = medias_m3,
      desvs = desvs_m3,
      delta = delta_m3,
      pred_pre = pred_pre,
      pred_mun = pred_final,
      agregado_pre = agregado_pre,
      agregado_post = agregado_post,
      error_benchmark = error_bench
    )

    filas_m3_sensibilidad[[clave]] <- data.frame(
      escenario = nom,
      modelo = "M3_CAR_Reducido",
      rho_CAR = rho_actual,
      n_cov = length(covs),
      K_CAR = K_CAR,
      NLL_penalizada = round(r3$nll, 3),
      tau = round(r3$tau, 4),
      sd_efecto_espacial = round(sd(r3$efecto_espacial), 4),
      min_pred = round(min(pred_final), 3),
      max_pred = round(max(pred_final), 3),
      fuera_rango = sum(pred_final < 0 | pred_final > 100),
      error_benchmark = signif(error_bench, 3),
      row.names = NULL
    )
  }
}

tabla_m3_sensibilidad <- dplyr::bind_rows(
  filas_m3_sensibilidad
)

cat("\n=========================================\n")
cat("RESULTADOS SENSIBILIDAD RHO - M3\n")
cat("=========================================\n")
print(tabla_m3_sensibilidad)


# ==============================================================================
# 15.1. AJUSTES M3 DE REFERENCIA
# ==============================================================================

ajuste_m3 <- list()

for (nom in names(ESCENARIOS)) {

  clave_ref <- paste0(
    nom,
    "_rho_",
    format(
      RHO_REF,
      nsmall = 2
    )
  )

  if (
    !clave_ref %in%
      names(ajuste_m3_sensibilidad)
  ) {

    stop(
      paste(
        "No existe ajuste M3 para",
        nom,
        "con rho de referencia =",
        RHO_REF
      )
    )
  }

  ajuste_m3[[nom]] <-
    ajuste_m3_sensibilidad[[clave_ref]]
}

tabla_m3 <- tabla_m3_sensibilidad %>%
  dplyr::filter(
    abs(
      rho_CAR -
        RHO_REF
    ) < 1e-10
  )

cat("\n=========================================\n")
cat("M3 DE REFERENCIA PARA COMPARACIÓN M1-M2-M3\n")
cat("rho =", RHO_REF, "\n")
cat("=========================================\n")
print(tabla_m3)


# ==============================================================================
# 16. VALIDACION INTERNA Y CONDICIONES DE VALIDEZ - M3
# ==============================================================================

cat("\n=========================================================\n")
cat("VALIDACION INTERNA MODELO 3 - CAR REDUCIDO\n")
cat("=========================================================\n")

validacion_m3 <- list()

for (nom in names(ESCENARIOS)) {

  covs <- ESCENARIOS[[nom]]
  obj <- ajuste_m3[[nom]]
  r3 <- obj$fit

  # Residuos departamentales PRE-benchmark.
  resid_pre <- base$ipm_directo -
    obj$agregado_pre

  V_pre <- base$vardir +
    r3$tau^2

  resid_pre_std <- resid_pre /
    sqrt(
      pmax(
        V_pre,
        .Machine$double.eps
      )
    )

  p_shapiro <- if (
    length(resid_pre_std) >= 3 &&
      length(resid_pre_std) <= 5000
  ) {
    tryCatch(
      shapiro.test(resid_pre_std)$p.value,
      error = function(e) NA_real_
    )
  } else {
    NA_real_
  }

  n_resid_std_gt3 <- sum(
    abs(resid_pre_std) > 3,
    na.rm = TRUE
  )

  # Hessiano numérico.
  H3 <- tryCatch(
    optimHess(
      r3$par_completo,
      r3$nll_fun
    ),
    error = function(e) NULL
  )

  if (
    !is.null(H3) &&
      all(is.finite(H3))
  ) {

    eig_H3 <- tryCatch(
      eigen(
        (H3 + t(H3)) / 2,
        symmetric = TRUE,
        only.values = TRUE
      )$values,
      error = function(e) rep(
        NA_real_,
        length(r3$par_completo)
      )
    )

  } else {

    eig_H3 <- rep(
      NA_real_,
      length(r3$par_completo)
    )
  }

  hessiano_pd <- all(is.finite(eig_H3)) &&
    min(eig_H3) > 1e-8

  min_eigen_H <- if (
    all(is.finite(eig_H3))
  ) {
    min(eig_H3)
  } else {
    NA_real_
  }

  condicion_H <- if (hessiano_pd) {
    max(eig_H3) / min(eig_H3)
  } else {
    NA_real_
  }

  # Moran del efecto espacial estimado.
  # Moran se calcula únicamente sobre las unidades que tienen geometría,
  # porque las unidades sin shapefile no forman parte de la matriz de vecindad.
  efecto_geo_m3 <- r3$efecto_espacial[idx_geo_x_m3]

  moran_efecto <- tryCatch(
    spdep::moran.mc(
      efecto_geo_m3,
      spdep::nb2listw(
        vecinos_m3,
        style = "W",
        zero.policy = TRUE
      ),
      nsim = 999,
      zero.policy = TRUE
    ),
    error = function(e) NULL
  )

  moran_I_efecto <- if (!is.null(moran_efecto)) {
    as.numeric(moran_efecto$statistic)
  } else {
    NA_real_
  }

  moran_p_efecto <- if (!is.null(moran_efecto)) {
    moran_efecto$p.value
  } else {
    NA_real_
  }

  convergencia_ok <- isTRUE(
    r3$convergencia == 0
  )

  parametros_finitos <- all(
    is.finite(
      c(
        r3$beta,
        r3$a,
        r3$tau,
        r3$rho
      )
    )
  ) &&
    r3$tau >= 0

  car_definida_positiva <- all(
    is.finite(r3$q) &
      r3$q > 0
  )

  rho_valido <- is.finite(r3$rho) &&
    r3$rho > -0.95 &&
    r3$rho < 0.95

  rango_ok <- all(
    is.finite(obj$pred_mun)
  ) &&
    all(
      obj$pred_mun >= 0 &
        obj$pred_mun <= 100
    )

  benchmark_ok <- is.finite(
    obj$error_benchmark
  ) &&
    obj$error_benchmark < 1e-6

  n_starts_validos <- r3$n_starts_validos
  prop_cercanos <- r3$prop_cercanos_optimo

  multistart_ok <- is.finite(
    n_starts_validos
  ) &&
    n_starts_validos >= 2 &&
    is.finite(prop_cercanos) &&
    prop_cercanos >= 0.25

  efecto_espacial_no_degenerado <- is.finite(
    sd(r3$efecto_espacial)
  ) &&
    sd(r3$efecto_espacial) > 1e-6

  apto_estructural <- all(
    convergencia_ok,
    parametros_finitos,
    car_definida_positiva,
    rho_valido,
    rango_ok,
    benchmark_ok
  )

  advertencias <- character(0)

  if (!multistart_ok) {
    advertencias <- c(
      advertencias,
      "Estabilidad multistart limitada"
    )
  }

  if (!hessiano_pd) {
    advertencias <- c(
      advertencias,
      "Hessiano no definido positivo"
    )
  } else if (
    is.finite(condicion_H) &&
      condicion_H > 1e8
  ) {
    advertencias <- c(
      advertencias,
      "Hessiano mal condicionado"
    )
  }


  if (!efecto_espacial_no_degenerado) {
    advertencias <- c(
      advertencias,
      "Efecto espacial prácticamente nulo"
    )
  }

  if (
    is.finite(p_shapiro) &&
      p_shapiro < 0.05
  ) {
    advertencias <- c(
      advertencias,
      "Normalidad aproximada residual cuestionada"
    )
  }

  if (n_resid_std_gt3 > 0) {
    advertencias <- c(
      advertencias,
      "Residuos pre-benchmark estandarizados >3"
    )
  }

  if (n_islas_red_m3 > 0) {
    advertencias <- c(
      advertencias,
      paste0(
        n_islas_red_m3,
        " unidades con geometría sin vecinos"
      )
    )
  }

  if (n_sin_geometria_m3 > 0) {
    advertencias <- c(
      advertencias,
      paste0(
        n_sin_geometria_m3,
        " unidades sin geometría; efecto CAR fijado en 0"
      )
    )
  }

  if (n_componentes_m3 > 1) {
    advertencias <- c(
      advertencias,
      paste0(
        n_componentes_m3,
        " componentes conexos"
      )
    )
  }

  estado <- if (!apto_estructural) {
    "NO_APTO"
  } else if (length(advertencias) > 0) {
    "APTO_CON_ADVERTENCIAS"
  } else {
    "APTO"
  }

  validacion_m3[[nom]] <- data.frame(
    escenario = nom,
    modelo = "M3_CAR_Reducido",
    convergencia = r3$convergencia,
    convergencia_ok = convergencia_ok,
    tau = r3$tau,
    rho_CAR = r3$rho,
    rho_fijado = TRUE,
    rho_valido = rho_valido,
    min_q_CAR = min(r3$q),
    CAR_definida_positiva = car_definida_positiva,
    parametros_finitos = parametros_finitos,
    n_starts = r3$n_starts,
    n_starts_validos = n_starts_validos,
    n_cercanos_optimo = r3$n_cercanos_optimo,
    prop_cercanos_optimo = prop_cercanos,
    estabilidad_multistart_ok = multistart_ok,
    hessiano_PD = hessiano_pd,
    min_eigen_H = min_eigen_H,
    condicion_H = condicion_H,
    sd_efecto_espacial = sd(r3$efecto_espacial),
    Moran_I_efecto = moran_I_efecto,
    Moran_p_efecto = moran_p_efecto,
    Shapiro_resid_pre_std_p = p_shapiro,
    n_resid_pre_std_abs_gt3 = n_resid_std_gt3,
    n_islas = n_islas_m3,
    n_sin_geometria = n_sin_geometria_m3,
    n_islas_red = n_islas_red_m3,
    n_componentes = n_componentes_m3,
    rango_0_100_ok = rango_ok,
    error_benchmark = obj$error_benchmark,
    benchmark_ok = benchmark_ok,
    apto_pre_lodo = apto_estructural,
    estado_pre_lodo = estado,
    advertencias = if (
      length(advertencias) == 0
    ) {
      "Ninguna"
    } else {
      paste(
        advertencias,
        collapse = "; "
      )
    },
    row.names = NULL
  )
}

tabla_validacion_m3 <- dplyr::bind_rows(
  validacion_m3
)

print(tabla_validacion_m3)

cat("\nInterpretación M3:\n")
cat("- La validez estructural exige convergencia, parámetros finitos,\n")
cat("  precisión CAR positiva, predicciones acotadas y benchmarking exacto.\n")
cat("- Multistart, Hessiano, residuos, islas y conectividad son\n")
cat("  diagnósticos que se documentan sin convertirlos automáticamente\n")
cat("  en reglas de descarte.\n")
cat("- rho se fija a priori en", RHO_REF, "para la comparación principal;\n")
cat("  su sensibilidad se evalúa separadamente con RHO_GRID.\n\n")


# ==============================================================================
# 17. VALIDACION LODO DEL MODELO 3
# ==============================================================================
#
# El departamento excluido no participa en la verosimilitud del ajuste
# y NO recibe benchmarking. La geometría, la matriz de vecindad y las
# covariables del dominio excluido sí pueden emplearse porque son
# información auxiliar disponible, no la respuesta directa excluida.
#
# La estandarización de covariables se calcula exclusivamente con los
# municipios pertenecientes a departamentos de entrenamiento.
# La comparación principal utiliza rho = RHO_REF, definido a priori.
# ==============================================================================

cat("\n=========================================================\n")
cat("VALIDACION LODO - MODELO 3 CAR\n")
cat("=========================================================\n")

resultados_lodo_m3 <- list()
detalle_lodo_m3 <- list()

for (nom in names(ESCENARIOS)) {

  cat("\n-----------------------------------------\n")
  cat("M3 escenario:", nom, "\n")
  cat("-----------------------------------------\n")

  covs <- ESCENARIOS[[nom]]

  pred_m3_lodo <- rep(
    NA_real_,
    n_dep
  )

  for (i in seq_len(n_dep)) {

    cat(
      "  Excluyendo:",
      base$departamento[i],
      "\n"
    )

    entrena_dep <- setdiff(
      seq_len(n_dep),
      i
    )

    mask_tr_mun <- idx_dep != i

    mun_tr <- x_m2[
      mask_tr_mun,
      ,
      drop = FALSE
    ]

    medias_i <- sapply(
      covs,
      function(v) {
        mean(
          mun_tr[[v]],
          na.rm = TRUE
        )
      }
    )

    desvs_i <- sapply(
      covs,
      function(v) {
        sd(
          mun_tr[[v]],
          na.rm = TRUE
        )
      }
    )

    Z_i <- construir_Z(
      datos = x_m2,
      covs = covs,
      medias = medias_i,
      desvs = desvs_i
    )

    H_i <- H_m3[
      entrena_dep,
      ,
      drop = FALSE
    ]

    fit_i <- tryCatch(
      ajustar_m3_car(
        Z = Z_i,
        B = B_car,
        lambda = lambda_car,
        H = H_i,
        y_dep = base$ipm_directo[entrena_dep],
        vardir = base$vardir[entrena_dep],
        rho_fijo = RHO_REF,
        n_starts = 3,
        semilla = 12000 +
          100 * match(nom, names(ESCENARIOS)) +
          i
      ),
      error = function(e) {
        cat(
          "    M3 no convergió:",
          conditionMessage(e),
          "\n"
        )
        NULL
      }
    )

    if (!is.null(fit_i)) {

      pred_mun_i <- 100 *
        inv_logit(
          fit_i$eta
        )

      pred_m3_lodo[i] <- as.numeric(
        H_m3[
          i,
          ,
          drop = FALSE
        ] %*% pred_mun_i
      )
    }
  }

  detalle_lodo_m3[[nom]] <- data.frame(
    cod_depto = base$cod_depto,
    departamento = base$departamento,
    ipm_directo = base$ipm_directo,
    M3 = round(pred_m3_lodo, 4),
    error_M3 = round(
      pred_m3_lodo -
        base$ipm_directo,
      4
    )
  )

  ok <- is.finite(
    pred_m3_lodo
  ) &
    is.finite(
      base$ipm_directo
    )

  resultados_lodo_m3[[nom]] <- data.frame(
    escenario = nom,
    modelo = "M3_CAR_Reducido",
    RMSE = round(
      rmse(
        base$ipm_directo[ok],
        pred_m3_lodo[ok]
      ),
      4
    ),
    MAE = round(
      mae(
        base$ipm_directo[ok],
        pred_m3_lodo[ok]
      ),
      4
    ),
    correlacion = round(
      cor(
        base$ipm_directo[ok],
        pred_m3_lodo[ok]
      ),
      4
    ),
    fuera_rango = sum(
      pred_m3_lodo[ok] < 0 |
        pred_m3_lodo[ok] > 100
    ),
    n_validos = sum(ok),
    row.names = NULL
  )
}

tabla_lodo_m3 <- dplyr::bind_rows(
  resultados_lodo_m3
)

cat("\nRESULTADOS LODO M3:\n")
print(
  tabla_lodo_m3[
    order(tabla_lodo_m3$RMSE),
    ,
    drop = FALSE
  ]
)


# ==============================================================================
# 18. VALIDACION FINAL Y COMPARACION CONTROLADA M1-M2-M3
# ==============================================================================

cat("\n=========================================================\n")
cat("VALIDACION FINAL M1-M2-M3\n")
cat("=========================================================\n")

tabla_lodo_3modelos <- dplyr::bind_rows(
  tabla_lodo,
  tabla_lodo_m3
)

tabla_validez_final_m123 <- dplyr::bind_rows(
  tabla_validacion_m1 %>%
    dplyr::select(
      escenario,
      modelo,
      apto_pre_lodo,
      estado_pre_lodo,
      advertencias
    ),
  tabla_validacion_m2 %>%
    dplyr::select(
      escenario,
      modelo,
      apto_pre_lodo,
      estado_pre_lodo,
      advertencias
    ),
  tabla_validacion_m3 %>%
    dplyr::select(
      escenario,
      modelo,
      apto_pre_lodo,
      estado_pre_lodo,
      advertencias
    )
) %>%
  dplyr::left_join(
    tabla_lodo_3modelos %>%
      dplyr::select(
        escenario,
        modelo,
        RMSE,
        MAE,
        correlacion,
        n_validos
      ),
    by = c(
      "escenario",
      "modelo"
    )
  ) %>%
  dplyr::mutate(
    lodo_completo = is.finite(n_validos) &
      n_validos == n_dep,
    metricas_finitas = is.finite(RMSE) &
      is.finite(MAE) &
      is.finite(correlacion),
    apto_comparacion = apto_pre_lodo &
      lodo_completo &
      metricas_finitas,
    estado_final = dplyr::case_when(
      !apto_pre_lodo ~ "NO_APTO",
      !lodo_completo ~ "NO_APTO_LODO_INCOMPLETO",
      !metricas_finitas ~ "NO_APTO_METRICAS",
      estado_pre_lodo == "APTO_CON_ADVERTENCIAS" ~
        "APTO_CON_ADVERTENCIAS",
      TRUE ~ "APTO"
    )
  )

print(tabla_validez_final_m123)

escenarios_validos_3 <- tabla_validez_final_m123 %>%
  dplyr::group_by(escenario) %>%
  dplyr::summarise(
    n_modelos = dplyr::n_distinct(modelo),
    todos_aptos = all(apto_comparacion),
    .groups = "drop"
  ) %>%
  dplyr::filter(
    n_modelos == 3,
    todos_aptos
  )

ESCENARIOS_VALIDOS_M123 <- escenarios_validos_3$escenario

cat("\nEscenarios habilitados para comparación de los tres modelos:\n")
print(ESCENARIOS_VALIDOS_M123)

if (length(ESCENARIOS_VALIDOS_M123) == 0) {
  warning(
    paste(
      "Ningún escenario superó simultáneamente la puerta de comparación",
      "de M1, M2 y M3. Revise tabla_validez_final_m123."
    )
  )
}

comparacion_controlada <- tabla_lodo_3modelos %>%
  dplyr::filter(
    escenario %in% ESCENARIOS_VALIDOS_M123
  ) %>%
  dplyr::mutate(
    evaluacion = "LODO_2024"
  ) %>%
  dplyr::arrange(
    escenario,
    RMSE
  )

cat("\n=========================================================\n")
cat("COMPARACION CONTROLADA M1-M2-M3 - MISMO LODO 2024\n")
cat("=========================================================\n")
print(comparacion_controlada)

cat("\nNOTA METODOLOGICA:\n")
cat("Las métricas RMSE, MAE y correlación de M1, M2 y M3 se obtienen ahora\n")
cat("bajo el mismo esquema LODO 2024. AIC y BIC no se emplean para ordenar\n")
cat("familias de modelos distintas; se reservan para comparación interna\n")
cat("cuando la estructura probabilística lo permite.\n")



# 18.A. Diagnostico de contribucion departamental al error LODO (sin reestimacion)
# ============================================================
# DIAGNOSTICO DE INFLUENCIA EN METRICAS LODO - MODELO 3
# E1, E2, E3 - 33 departamentos
# ============================================================

stopifnot(
  exists("detalle_lodo_m3"),
  is.list(detalle_lodo_m3)
)

tabla_influencia_lodo <- list()
resumen_influencia <- list()

for (esc in names(detalle_lodo_m3)) {
  
  datos <- detalle_lodo_m3[[esc]]
  
  stopifnot(all(c(
    "cod_depto", "departamento",
    "ipm_directo", "M3"
  ) %in% names(datos)))
  
  datos <- as.data.frame(datos)
  
  y <- as.numeric(datos$ipm_directo)
  pred <- as.numeric(datos$M3)
  
  if (
    length(y) != 33 ||
    any(!is.finite(y)) ||
    any(!is.finite(pred))
  ) {
    stop("Datos LODO incompletos: ", esc)
  }
  
  error <- pred - y
  rmse_total <- sqrt(mean(error^2))
  mae_total <- mean(abs(error))
  
  # Aporte individual al error cuadratico total
  contrib_rmse <- 100 * error^2 / sum(error^2)
  
  # Metricas recalculadas sin cada departamento
  rmse_sin <- sapply(seq_along(y), function(i) {
    sqrt(mean(error[-i]^2))
  })
  
  mae_sin <- sapply(seq_along(y), function(i) {
    mean(abs(error[-i]))
  })
  
  tabla <- data.frame(
    escenario = esc,
    cod_depto = datos$cod_depto,
    departamento = datos$departamento,
    ipm_directo = y,
    prediccion_lodo = pred,
    residuo_lodo = error,
    error_absoluto = abs(error),
    contribucion_SSE_pct = contrib_rmse,
    RMSE_original = rmse_total,
    RMSE_sin_depto = rmse_sin,
    cambio_RMSE = rmse_sin - rmse_total,
    MAE_original = mae_total,
    MAE_sin_depto = mae_sin,
    cambio_MAE = mae_sin - mae_total
  )
  
  tabla <- tabla[
    order(-tabla$contribucion_SSE_pct),
  ]
  
  tabla_influencia_lodo[[esc]] <- tabla
  
  resumen_influencia[[esc]] <- head(
    tabla[, c(
      "escenario",
      "departamento",
      "residuo_lodo",
      "contribucion_SSE_pct",
      "cambio_RMSE"
    )],
    7
  )
}

resultado_influencia_lodo <- do.call(
  rbind, tabla_influencia_lodo
)

ranking_influencia_lodo <- do.call(
  rbind, resumen_influencia
)

dir.create(
  "salidas_departamentos_influyentes",
  showWarnings = FALSE
)

write.csv(
  resultado_influencia_lodo,
  paste0(
    "salidas_departamentos_influyentes/",
    "diagnostico_lodo_por_departamento.csv"
  ),
  row.names = FALSE
)

write.csv(
  ranking_influencia_lodo,
  paste0(
    "salidas_departamentos_influyentes/",
    "ranking_departamentos_lodo.csv"
  ),
  row.names = FALSE
)

cat("\n===== DEPARTAMENTOS CON MAYOR APORTE AL ERROR =====\n")

print(
  ranking_influencia_lodo,
  row.names = FALSE,
  digits = 4
)

cat("\n===== ARCHIVOS GENERADOS =====\n")
print(
  normalizePath("salidas_departamentos_influyentes")
)



# ==============================================================================
# 19. ESTABILIDAD ENTRE ESCENARIOS Y FORMULACIONES DE REFERENCIA
# ==============================================================================

cat("\n=========================================================\n")
cat("ESTABILIDAD ENTRE ESCENARIOS - TRES MODELOS\n")
cat("=========================================================\n")

tabla_estabilidad_3 <- tabla_lodo_3modelos %>%
  dplyr::group_by(modelo) %>%
  dplyr::summarise(
    RMSE_min = min(RMSE, na.rm = TRUE),
    RMSE_max = max(RMSE, na.rm = TRUE),
    amplitud_RMSE = RMSE_max - RMSE_min,
    .groups = "drop"
  )

print(tabla_estabilidad_3)

formulaciones_referencia <- tabla_lodo_3modelos %>%
  dplyr::group_by(modelo) %>%
  dplyr::arrange(
    RMSE,
    MAE,
    dplyr::desc(correlacion),
    .by_group = TRUE
  ) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    criterio = "Menor RMSE LODO dentro de la familia"
  )

cat("\nFORMULACIONES DE REFERENCIA POR MODELO:\n")
print(formulaciones_referencia)

cat("\nEstas formulaciones son un resumen operacional dentro de cada familia;\n")
cat("no se interpretan como una selección automática de un modelo ganador.\n")


# ==============================================================================
# 20. COMPARACION MUNICIPAL DE LOS TRES MODELOS
# ==============================================================================

ESC_REF_3 <- if (
  "E2_Parsimoniosa" %in% names(ajuste_m3)
) {
  "E2_Parsimoniosa"
} else {
  names(ajuste_m3)[1]
}

comparacion_municipal_3 <- data.frame(
  cod_depto = x_m2$cod_depto,
  depto = x_m2$depto,
  cod_mpio = x_m2$cod_mpio,
  mpio = x_m2$mpio,
  poblacion = x_m2$poblacion,
  IPM_M1 = pmin(
    pmax(
      ajuste_m1[[ESC_REF_3]]$pred_mun,
      0
    ),
    100
  ),
  IPM_M2 = ajuste_m2[[ESC_REF_3]]$pred_mun,
  IPM_M3 = ajuste_m3[[ESC_REF_3]]$pred_mun
)

tabla_cor_municipal_3 <- cor(
  comparacion_municipal_3[
    ,
    c(
      "IPM_M1",
      "IPM_M2",
      "IPM_M3"
    )
  ],
  use = "complete.obs"
)

cat("\nCorrelaciones municipales M1-M2-M3 -",
    ESC_REF_3, ":\n")
print(tabla_cor_municipal_3)

tabla_dif_municipal_3 <- data.frame(
  escenario = ESC_REF_3,
  cor_M1_M2 = cor(
    comparacion_municipal_3$IPM_M1,
    comparacion_municipal_3$IPM_M2
  ),
  cor_M1_M3 = cor(
    comparacion_municipal_3$IPM_M1,
    comparacion_municipal_3$IPM_M3
  ),
  cor_M2_M3 = cor(
    comparacion_municipal_3$IPM_M2,
    comparacion_municipal_3$IPM_M3
  ),
  DAM_M1_M2 = mean(
    abs(
      comparacion_municipal_3$IPM_M1 -
        comparacion_municipal_3$IPM_M2
    )
  ),
  DAM_M1_M3 = mean(
    abs(
      comparacion_municipal_3$IPM_M1 -
        comparacion_municipal_3$IPM_M3
    )
  ),
  DAM_M2_M3 = mean(
    abs(
      comparacion_municipal_3$IPM_M2 -
        comparacion_municipal_3$IPM_M3
    )
  )
)

print(tabla_dif_municipal_3)


# ==============================================================================
# 21. GRAFICOS COMPARATIVOS
# ==============================================================================

if (nrow(comparacion_controlada) > 0) {

  base_g <- comparacion_controlada %>%
    dplyr::mutate(
      modelo = dplyr::recode(
        modelo,
        "M1_FayHerriot" = "M1 Fay-Herriot",
        "M2_NoLineal" = "M2 no lineal",
        "M3_CAR_Reducido" = "M3 CAR espacial"
      )
    )

  g_rmse_3 <- ggplot(
    base_g,
    aes(
      x = escenario,
      y = RMSE,
      fill = modelo
    )
  ) +
    geom_col(
      position = "dodge"
    ) +
    labs(
      title = "Comparación controlada de RMSE",
      subtitle = "M1, M2 y M3 evaluados mediante LODO 2024",
      x = "Escenario",
      y = "RMSE",
      fill = "Modelo"
    ) +
    theme_minimal()

  g_mae_3 <- ggplot(
    base_g,
    aes(
      x = escenario,
      y = MAE,
      fill = modelo
    )
  ) +
    geom_col(
      position = "dodge"
    ) +
    labs(
      title = "Comparación controlada de MAE",
      subtitle = "M1, M2 y M3 evaluados mediante LODO 2024",
      x = "Escenario",
      y = "MAE",
      fill = "Modelo"
    ) +
    theme_minimal()

  g_cor_3 <- ggplot(
    base_g,
    aes(
      x = escenario,
      y = correlacion,
      fill = modelo
    )
  ) +
    geom_col(
      position = "dodge"
    ) +
    labs(
      title = "Comparación controlada de correlación",
      subtitle = "M1, M2 y M3 evaluados mediante LODO 2024",
      x = "Escenario",
      y = "Correlación",
      fill = "Modelo"
    ) +
    theme_minimal()

  ggsave(
    file.path(
      CARPETA_SALIDA,
      "comparacion_3_modelos_RMSE.png"
    ),
    g_rmse_3,
    width = 10,
    height = 7,
    dpi = 300,
    bg = "white"
  )

  ggsave(
    file.path(
      CARPETA_SALIDA,
      "comparacion_3_modelos_MAE.png"
    ),
    g_mae_3,
    width = 10,
    height = 7,
    dpi = 300,
    bg = "white"
  )

  ggsave(
    file.path(
      CARPETA_SALIDA,
      "comparacion_3_modelos_correlacion.png"
    ),
    g_cor_3,
    width = 10,
    height = 7,
    dpi = 300,
    bg = "white"
  )
}


# ==============================================================================
# 22. CARTOGRAFIA FINAL M1-M2-M3
# ==============================================================================

if (file.exists(RUTA_SHP)) {

  mapa_final <- sf::st_read(
    RUTA_SHP,
    quiet = TRUE
  )

  mapa_final$cod_mpio <- obtener_codigo_municipal(
    mapa_final
  )

  # Solo polígonos municipales presentes en la muestra común y con geometría.
  # Las unidades sin geometría participan en las métricas y agregación, pero no
  # pueden representarse cartográficamente con este shapefile.
  mapa_final <- mapa_final %>%
    dplyr::filter(
      cod_mpio %in%
        comparacion_municipal_3$cod_mpio
    ) %>%
    dplyr::distinct(
      cod_mpio,
      .keep_all = TRUE
    ) %>%
    dplyr::left_join(
      comparacion_municipal_3 %>%
        dplyr::select(
          cod_mpio,
          IPM_M1,
          IPM_M2,
          IPM_M3
        ),
      by = "cod_mpio"
    )

  for (
    col in c(
      "IPM_M1",
      "IPM_M2",
      "IPM_M3"
    )
  ) {

    mapa_final$clase <- cut(
      mapa_final[[col]],
      breaks = CORTES,
      labels = ETIQ,
      include.lowest = TRUE
    )

    titulo_modelo <- dplyr::case_when(
      col == "IPM_M1" ~ "Modelo 1 Fay-Herriot",
      col == "IPM_M2" ~ "Modelo 2 no lineal",
      TRUE ~ "Modelo 3 CAR espacial"
    )

    g <- ggplot(
      mapa_final
    ) +
      geom_sf(
        aes(
          fill = clase
        ),
        color = "white",
        linewidth = 0.05
      ) +
      scale_fill_manual(
        values = PALETA,
        na.value = "#D9D9D9",
        name = "IPM estimado (%)",
        drop = FALSE,
        na.translate = TRUE
      ) +
      labs(
        title = paste(
          "IPM municipal estimado -",
          titulo_modelo
        ),
        subtitle = paste(
          "Escenario de referencia:",
          ESC_REF_3
        )
      ) +
      theme_void(
        base_size = 13
      ) +
      theme(
        legend.position = "bottom"
      )

    ggsave(
      file.path(
        CARPETA_SALIDA,
        paste0(
          "mapa_final_",
          col,
          ".png"
        )
      ),
      g,
      width = 7,
      height = 8.5,
      dpi = 300,
      bg = "white"
    )
  }
}


# ==============================================================================
# 23. EXPORTACION CONSOLIDADA M1-M2-M3
# ==============================================================================

tabla_parametros_m3 <- data.frame(
  parametro = c(
    "RHO_REF",
    "RHO_GRID",
    "K_CAR"
  ),
  valor = c(
    as.character(RHO_REF),
    paste(RHO_GRID, collapse = ", "),
    as.character(K_CAR)
  ),
  stringsAsFactors = FALSE
)

tabla_fuente_covariables <- data.frame(
  fuente = "Covariables_18&24.csv",
  modelo = c(
    "M1_FayHerriot",
    "M2_NoLineal",
    "M3_CAR_Reducido"
  ),
  periodo_utilizado = c(
    "2024",
    "2024",
    "2024"
  ),
  n_municipios = c(
    nrow(x_m2),
    nrow(x_m2),
    nrow(x_m2)
  ),
  stringsAsFactors = FALSE
)

wb3 <- openxlsx::createWorkbook()

hojas3 <- list(
  Control_escalas = tabla_control_escalas,
  Auditoria_escalas_2024 = tabla_auditoria_escalas,
  Diagnostico_OLS = tabla_diagnostico,
  Ajuste_M1 = tabla_m1,
  Validez_interna_M1 = tabla_validacion_m1,
  Ajuste_M2 = tabla_m2,
  Validez_interna_M2 = tabla_validacion_m2,
  Ajuste_M3_CAR = tabla_m3,
  Sensibilidad_rho_M3 = tabla_m3_sensibilidad,
  Validez_interna_M3 = tabla_validacion_m3,
  LODO_M1_M2 = tabla_lodo,
  LODO_M3 = tabla_lodo_m3,
  LODO_3_modelos = tabla_lodo_3modelos,
  Validez_final_M123 = tabla_validez_final_m123,
  Fuente_covariables = tabla_fuente_covariables,
  Parametros_M3 = tabla_parametros_m3,
  Comparacion_controlada = comparacion_controlada,
  Estabilidad_escenarios = tabla_estabilidad_3,
  Formulaciones_referencia = formulaciones_referencia,
  Comparacion_municipal_3 = comparacion_municipal_3,
  Cor_municipal_3 = as.data.frame(tabla_cor_municipal_3),
  Diferencias_municipales = tabla_dif_municipal_3
)

if (!is.null(tabla_espacial)) {
  hojas3$Diagnostico_espacial_M12 <- tabla_espacial
}

for (nom in names(hojas3)) {

  hoja <- substr(
    nom,
    1,
    31
  )

  openxlsx::addWorksheet(
    wb3,
    hoja
  )

  openxlsx::writeData(
    wb3,
    hoja,
    hojas3[[nom]]
  )

  openxlsx::freezePane(
    wb3,
    hoja,
    firstRow = TRUE
  )

  openxlsx::setColWidths(
    wb3,
    hoja,
    cols = seq_len(
      ncol(hojas3[[nom]])
    ),
    widths = "auto"
  )
}

# Detalle LODO M1-M2.
for (k in names(detalle_lodo)) {

  hoja <- substr(
    paste0(
      "LODO12_",
      k
    ),
    1,
    31
  )

  openxlsx::addWorksheet(
    wb3,
    hoja
  )

  openxlsx::writeData(
    wb3,
    hoja,
    detalle_lodo[[k]]
  )
}

# Detalle LODO M3.
for (k in names(detalle_lodo_m3)) {

  hoja <- substr(
    paste0(
      "LODO3_",
      k
    ),
    1,
    31
  )

  openxlsx::addWorksheet(
    wb3,
    hoja
  )

  openxlsx::writeData(
    wb3,
    hoja,
    detalle_lodo_m3[[k]]
  )
}

# Predicciones municipales M3 por escenario.
for (nom in names(ajuste_m3)) {

  pred_export <- data.frame(
    cod_depto = x_m2$cod_depto,
    depto = x_m2$depto,
    cod_mpio = x_m2$cod_mpio,
    mpio = x_m2$mpio,
    poblacion = x_m2$poblacion,
    pred_M3_prebenchmark = ajuste_m3[[nom]]$pred_pre,
    pred_M3_final = ajuste_m3[[nom]]$pred_mun,
    efecto_espacial = ajuste_m3[[nom]]$fit$efecto_espacial,
    delta_benchmark = ajuste_m3[[nom]]$delta[idx_dep]
  )

  hoja <- substr(
    paste0(
      "M3pred_",
      nom
    ),
    1,
    31
  )

  openxlsx::addWorksheet(
    wb3,
    hoja
  )

  openxlsx::writeData(
    wb3,
    hoja,
    pred_export
  )
}

archivo_3_modelos <- file.path(
  CARPETA_SALIDA,
  "resultados_SAE_IPM_modelos_1_2_3_CAR.xlsx"
)

openxlsx::saveWorkbook(
  wb3,
  archivo_3_modelos,
  overwrite = TRUE
)

write.csv(
  comparacion_controlada,
  file.path(
    CARPETA_SALIDA,
    "comparacion_controlada_M1_M2_M3_CAR.csv"
  ),
  row.names = FALSE
)

write.csv(
  formulaciones_referencia,
  file.path(
    CARPETA_SALIDA,
    "formulaciones_referencia_M1_M2_M3_CAR.csv"
  ),
  row.names = FALSE
)

cat("\n=========================================================\n")
cat("MODELACION M1-M2-M3 COMPLETADA\n")
cat("=========================================================\n")
cat("M1: Fay-Herriot 2024.\n")
cat("M2: desagregación no lineal acotada 2024.\n")
cat("M3: desagregación espacial CAR reducida 2024 con rho de referencia fijado.\n")
cat("rho de referencia M3:", RHO_REF, "\n")
cat("Sensibilidad rho evaluada en:", paste(RHO_GRID, collapse = ", "), "\n")
cat("Los tres modelos se evalúan mediante LODO 2024.\n")
cat("El benchmarking se aplica únicamente a las estimaciones finales;\n")
cat("nunca al dominio excluido durante la validación LODO.\n")
cat("Resultados consolidados en:\n")
cat(archivo_3_modelos, "\n")
cat("=========================================================\n")

# ============================================================================
# BLOQUE II: SENSIBILIDAD LODO DE M3 A RHO
# ============================================================================

# Requiere ejecutar 01_modelamiento_principal.R en la misma sesion.
# Mantiene modelos, formulas y semillas originales.

# ============================================================
# M3 - SENSIBILIDAD PREDICTIVA A RHO CON LODO
# K = 6 | E1, E2, E3 | 33 departamentos
# ============================================================

stopifnot(
  exists("ajustar_m3_car"),
  exists("ESCENARIOS"),
  exists("B_car"),
  exists("lambda_car"),
  exists("H_m3"),
  exists("base"),
  exists("idx_dep"),
  exists("x_m2")
)

# 1. DATOS DEPARTAMENTALES
y <- as.numeric(base$ipm_directo)
v <- as.numeric(base$vardir)
dep <- as.character(base$cod_depto)

B <- as.matrix(B_car)
lambda <- as.numeric(lambda_car)
H <- H_m3

stopifnot(
  length(y) == 33,
  length(v) == 33,
  nrow(B) == 1121,
  ncol(B) == 6,
  length(lambda) == 6,
  all(dim(H) == c(33, 1121)),
  length(idx_dep) == 1121,
  !anyNA(y),
  all(is.finite(y)),
  all(is.finite(v) & v > 0)
)

# 2. RECUPERAR MATRIZ MUNICIPAL ANALITICA
# Se prioriza x_ok si contiene los 1121 municipios.

datos_mun <- x_m2

if (nrow(datos_mun) != 1121) {
  stop(
    "No fue posible recuperar los 1121 municipios ",
    "en el orden analitico."
  )
}

# 3. COMPROBAR ORDEN DEPARTAMENTAL
idx <- as.integer(idx_dep)

if (
  anyNA(idx) ||
  !setequal(unique(idx), seq_len(33))
) {
  stop("idx_dep no contiene indices departamentales 1:33.")
}

# H debe agregar cada departamento a partir de
# los municipios indicados por idx_dep.
for (d in seq_len(33)) {
  
  masa_propia <- sum(H[d, idx == d, drop = FALSE])
  masa_ajena <- sum(H[d, idx != d, drop = FALSE])
  
  if (
    !is.finite(masa_propia) ||
    abs(masa_propia - 1) > 1e-6 ||
    abs(masa_ajena) > 1e-6
  ) {
    stop(
      "Inconsistencia entre H_m3 e idx_dep ",
      "en el departamento ", d
    )
  }
}

# Comprobar que las filas municipales correspondan
# a los mismos departamentos que idx_dep.
codigo_mun <- as.character(datos_mun$cod_depto)

for (d in seq_len(33)) {
  if (!all(codigo_mun[idx == d] == dep[d])) {
    stop(
      "El orden municipal no coincide con H_m3 ",
      "e ipm. No se ejecuta LODO para evitar ",
      "resultados incorrectos."
    )
  }
}

# 4. CONSTRUIR MATRICES DE LOS TRES ESCENARIOS

nombres_esc <- c(
  "E1_Completa",
  "E2_Parsimoniosa",
  "E3_Alt_vivienda"
)

matrices <- lapply(nombres_esc, function(esc) {
  
  vars <- ESCENARIOS[[esc]]
  
  if (!is.character(vars) || !length(vars)) {
    stop(
      "ESCENARIOS[['", esc,
      "']] debe contener nombres de covariables."
    )
  }
  
  faltan <- setdiff(vars, names(datos_mun))
  
  if (length(faltan)) {
    stop(
      "Variables no encontradas en ", esc, ": ",
      paste(faltan, collapse = ", ")
    )
  }
  
  X <- as.matrix(datos_mun[, vars, drop = FALSE])
  storage.mode(X) <- "double"
  
  if (any(!is.finite(X))) {
    stop("Covariables no finitas en ", esc)
  }
  
  colnames(X) <- vars
  
  X
})

names(matrices) <- nombres_esc

# 5. EJECUCION LODO PARA CADA ESCENARIO Y RHO

rho_grid <- c(0.25, 0.50, 0.75, 0.90)

metricas <- list()
detalle <- list()
contador <- 0L

for (e in seq_along(nombres_esc)) {
  
  esc <- nombres_esc[e]
  X <- matrices[[esc]]
  
  for (rho in rho_grid) {
    
    cat(
      "\nEJECUTANDO:", esc,
      "| rho =", rho,
      "| K = 6\n"
    )
    
    pred <- rep(NA_real_, 33)
    conv <- rep(NA_integer_, 33)
    
    for (d in seq_len(33)) {
      
      train_dep <- seq_len(33) != d
      train_mun <- idx != d
      
      # Estandarizacion solo con municipios
      # pertenecientes al entrenamiento.
      Z <- matrix(
        1,
        nrow = 1121,
        ncol = ncol(X) + 1
      )
      
      colnames(Z) <- c("(Intercept)", colnames(X))
      
      for (j in seq_len(ncol(X))) {
        
        media <- mean(X[train_mun, j])
        desv <- sd(X[train_mun, j])
        
        if (!is.finite(desv) || desv <= 0) {
          stop("Desviacion invalida: ", esc)
        }
        
        Z[, j + 1] <- (X[, j] - media) / desv
      }
      
      fit <- ajustar_m3_car(
        Z = Z,
        B = B,
        lambda = lambda,
        H = H[train_dep, , drop = FALSE],
        y_dep = y[train_dep],
        vardir = v[train_dep],
        rho_fijo = rho,
        n_starts = 3,
        semilla = 12000 + 100 * e + d
      )
      
      if (is.null(fit)) {
        stop(
          "Fallo de ajuste en ", esc,
          ", rho=", rho, ", depto=", dep[d]
        )
      }
      
      conv[d] <- fit$convergencia
      
      # Prediccion sintetica SIN benchmarking
      pm <- 100 * plogis(fit$eta)
      
      pred[d] <- as.numeric(
        H[d, , drop = FALSE] %*% pm
      )
      
      if (!is.finite(pred[d])) {
        stop("Prediccion no finita en depto ", dep[d])
      }
      
      if (d %% 11 == 0) {
        cat("  Departamentos evaluados:", d, "/33\n")
      }
    }
    
    err <- pred - y
    contador <- contador + 1L
    
    metricas[[contador]] <- data.frame(
      escenario = esc,
      K = 6,
      rho = rho,
      RMSE = sqrt(mean(err^2)),
      MAE = mean(abs(err)),
      Pearson = cor(y, pred),
      Spearman = cor(y, pred, method = "spearman"),
      Kendall = cor(y, pred, method = "kendall"),
      n_LODO = 33,
      convergencias_ok = sum(conv == 0, na.rm = TRUE)
    )
    
    detalle[[contador]] <- data.frame(
      escenario = esc,
      K = 6,
      rho = rho,
      cod_depto = dep,
      IPM_directo = y,
      IPM_LODO = pred,
      error = err,
      error_abs = abs(err),
      convergencia = conv
    )
    
    print(metricas[[contador]])
  }
}

# 6. CONSOLIDAR Y EXPORTAR

tabla_rho_lodo <- do.call(rbind, metricas)
detalle_rho_lodo <- do.call(rbind, detalle)

dir.create(
  "salidas_sensibilidad_rho",
  showWarnings = FALSE
)

write.csv(
  tabla_rho_lodo,
  "salidas_sensibilidad_rho/metricas_lodo_rho.csv",
  row.names = FALSE
)

write.csv(
  detalle_rho_lodo,
  "salidas_sensibilidad_rho/detalle_lodo_rho.csv",
  row.names = FALSE
)

# 7. COMPARAR RHO = 0.50 CON RESULTADOS PUBLICADOS

referencia <- data.frame(
  escenario = nombres_esc,
  RMSE_referencia = c(6.0406, 4.7197, 10.4274),
  MAE_referencia = c(4.4623, 3.6731, 6.5350),
  Pearson_referencia = c(0.9001, 0.9410, 0.7063)
)

comparacion <- merge(
  subset(tabla_rho_lodo, rho == 0.50),
  referencia,
  by = "escenario"
)

comparacion$dif_RMSE <- (
  comparacion$RMSE -
    comparacion$RMSE_referencia
)

comparacion$dif_MAE <- (
  comparacion$MAE -
    comparacion$MAE_referencia
)

comparacion$dif_Pearson <- (
  comparacion$Pearson -
    comparacion$Pearson_referencia
)

cat("\n====================================\n")
cat("RESULTADOS FINALES DE SENSIBILIDAD\n")
cat("====================================\n")

print(tabla_rho_lodo, row.names = FALSE)

cat("\nCOMPARACION CON RHO = 0.50 ORIGINAL:\n")
print(comparacion, row.names = FALSE)

write.csv(
  comparacion,
  "salidas_sensibilidad_rho/comparacion_referencia.csv",
  row.names = FALSE
)

cat("\nArchivos guardados en:\n")
cat(normalizePath("salidas_sensibilidad_rho"), "\n")


# ============================================================
# CIERRE DE SENSIBILIDAD M3 A RHO
# Validación y exportación de tablas para la tesis
# ============================================================

ruta <- "salidas_sensibilidad_rho"

resultados <- read.csv(
  file.path(ruta, "metricas_lodo_rho.csv"),
  stringsAsFactors = FALSE
)

comparacion <- read.csv(
  file.path(ruta, "comparacion_referencia.csv"),
  stringsAsFactors = FALSE
)

cat("\n===== TABLA DE SENSIBILIDAD =====\n")
print(resultados, row.names = FALSE)

cat("\n===== COMPARACION CON MODELO ORIGINAL =====\n")
print(
  comparacion[, c(
    "escenario", "rho",
    "dif_RMSE", "dif_MAE", "dif_Pearson"
  )],
  row.names = FALSE
)

# Validar 12 configuraciones, 33 pliegues y convergencia
stopifnot(
  nrow(resultados) == 12,
  all(resultados$n_LODO == 33),
  all(is.finite(resultados$RMSE)),
  all(is.finite(resultados$MAE)),
  all(is.finite(resultados$Pearson))
)

# Tolerancia para comparar contra resultados
# originales redondeados a cuatro decimales
tol <- 0.01

coincide <- (
  abs(comparacion$dif_RMSE) < tol &
    abs(comparacion$dif_MAE) < tol &
    abs(comparacion$dif_Pearson) < tol
)

cat("\n===== VERIFICACION =====\n")
print(
  data.frame(
    escenario = comparacion$escenario,
    coincide_referencia = coincide
  ),
  row.names = FALSE
)

if (!all(coincide)) {
  warning(
    "La nueva ejecución NO reproduce el modelo ",
    "original con rho=0.50. Revisar antes de ",
    "incorporar los resultados a la tesis."
  )
}

if (any(resultados$convergencias_ok < 33)) {
  warning(
    "Existen configuraciones con pliegues ",
    "sin convergencia confirmada."
  )
}

# Tabla redondeada para el documento
tabla_documento <- resultados[, c(
  "escenario", "rho", "K",
  "RMSE", "MAE", "Pearson",
  "Spearman", "Kendall",
  "convergencias_ok"
)]

tabla_documento[, c(
  "RMSE", "MAE", "Pearson",
  "Spearman", "Kendall"
)] <- round(
  tabla_documento[, c(
    "RMSE", "MAE", "Pearson",
    "Spearman", "Kendall"
  )],
  4
)

write.csv(
  tabla_documento,
  file.path(ruta, "tabla_para_tesis.csv"),
  row.names = FALSE
)

cat("\n===== TABLA PARA LA TESIS =====\n")
print(tabla_documento, row.names = FALSE)

# ============================================================================
# BLOQUE III: ROBUSTEZ DEPARTAMENTAL E2
# ============================================================================

# ROBUSTEZ A DEPARTAMENTOS INFLUYENTES - ESCENARIO E2
# Ejecutar DESPUES del script principal, en la misma sesion de R.
# No sobrescribe ajuste_m1, ajuste_m2, ajuste_m3 ni resultados originales.
# Autor: análisis complementario de sensibilidad LODO.

requeridos <- c('base','x_m2','idx_dep','H_m3','B_car','lambda_car',
                'ESCENARIOS','ajustar_m2','ajustar_m3_car','ajuste_m1',
                'ajuste_m2','ajuste_m3','detalle_lodo','detalle_lodo_m3',
                'construir_Z','inv_logit')
falta <- requeridos[!vapply(requeridos, exists, logical(1), envir=.GlobalEnv, inherits=FALSE)]
if(length(falta)) stop('Faltan objetos del script principal: ',paste(falta,collapse=', '))

esc <- 'E2_Parsimoniosa'
covs <- ESCENARIOS[[esc]]
n <- nrow(base)
stopifnot(n == 33, nrow(x_m2) == nrow(B_car), length(idx_dep)==nrow(x_m2),
          nrow(H_m3)==n, ncol(H_m3)==nrow(x_m2), length(covs)==4)

ruta <- 'salidas_departamentos_influyentes/robustez_E2'
dir.create(ruta, recursive=TRUE, showWarnings=FALSE)

# 1. Diagnosticos formales, OLS DE REFERENCIA (NO son Cook de SAE)
f_ols <- reformulate(covs, response='ipm_directo')
ols <- lm(f_ols, data=base)
cooks <- as.numeric(cooks.distance(ols))
lev <- as.numeric(hatvalues(ols))
rstd <- as.numeric(rstandard(ols))
dfb <- as.matrix(dfbetas(ols))
max_dfb <- apply(abs(dfb), 1, max, na.rm=TRUE)
p <- length(coef(ols))
umbral_cook <- 4/n
umbral_lev <- 2*p/n
umbral_dfb <- 2/sqrt(n)
formal <- data.frame(cod_depto=base$cod_depto, departamento=base$departamento,
  cook=cooks, leverage=lev, residuo_estandarizado=rstd,
  max_abs_DFBETAS=max_dfb, senal_cook=cooks>umbral_cook,
  senal_leverage=lev>umbral_lev, senal_DFBETAS=max_dfb>umbral_dfb)
formal <- formal[order(-formal$cook),]
write.csv(formal,file.path(ruta,'01_diagnosticos_formales_OLS_E2.csv'),row.names=FALSE)

# Tres mayores Cook ENTRE LOS QUE SUPEREN 4/n. Criterio prespecificado.
# Modificar n_sensibles si desea analizar mas; 3 limita costo computacional.
n_sensibles <- 3L
candidatos <- which(cooks>umbral_cook)
if (!length(candidatos)) stop('Ningun departamento supera Cook > 4/33; no procede este protocolo tal como esta definido.')
seleccion <- head(candidatos[order(cooks[candidatos],decreasing=TRUE)], n_sensibles)
cat('\nDepartamentos seleccionados por distancia de Cook E2:\n')
print(formal[match(base$cod_depto[seleccion],formal$cod_depto),],row.names=FALSE)

# 2. Funciones auxiliares: MISMO preprocesamiento del script principal.
Z_con_entrenamiento <- function(dep_entrena) {
  mask <- idx_dep %in% dep_entrena
  mun <- x_m2[mask,,drop=FALSE]
  medias <- vapply(covs,function(v) mean(mun[[v]],na.rm=TRUE),numeric(1))
  desvs <- vapply(covs,function(v) sd(mun[[v]],na.rm=TRUE),numeric(1))
  if (any(!is.finite(desvs)|desvs<=0)) stop('Desviaciones invalidas')
  Z <- construir_Z(x_m2,covs,medias,desvs)
  list(Z=Z,medias=medias,desvs=desvs)
}
coef_escala_original <- function(beta,medias,desvs) {
  setNames(c(beta[1]-sum(beta[-1]*medias/desvs),beta[-1]/desvs),
           c('(Intercept)',covs))
}
M2_ajustar_entrena <- function(dep_entrena,semilla,n_starts=5L) {
  escz <- Z_con_entrenamiento(dep_entrena)
  mask <- idx_dep %in% dep_entrena
  idx_local <- match(idx_dep[mask],dep_entrena)
  fit <- ajustar_m2(escz$Z[mask,,drop=FALSE],idx_local,
       x_m2$peso[mask],base$ipm_directo[dep_entrena],
       base$vardir[dep_entrena],length(dep_entrena),
       n_starts=n_starts,semilla=semilla)
  if(is.null(fit) || fit$convergencia!=0) stop('M2 no converge')
  list(fit=fit, Z=escz$Z,medias=escz$medias,desvs=escz$desvs)
}
M3_ajustar_entrena <- function(dep_entrena,semilla,n_starts=3L) {
  escz <- Z_con_entrenamiento(dep_entrena)
  fit <- ajustar_m3_car(Z=escz$Z,B=B_car,lambda=lambda_car,
      H=H_m3[dep_entrena,,drop=FALSE],y_dep=base$ipm_directo[dep_entrena],
      vardir=base$vardir[dep_entrena],rho_fijo=0.50,
      n_starts=n_starts,semilla=semilla)
  if(is.null(fit) || fit$convergencia!=0) stop('M3 no converge')
  list(fit=fit,Z=escz$Z,medias=escz$medias,desvs=escz$desvs)
}
M1_ajustar_entrena <- function(dep_entrena) {
  fit <- sae::eblupFH(formula=f_ols,vardir=vardir,method='REML',
       data=base[dep_entrena,,drop=FALSE])
  b <- as.numeric(fit$fit$estcoef$beta)
  names(b) <- names(coef(ols))
  b
}
pred_M2 <- function(fit, Z, d) {
  k <- idx_dep==d
  w <- x_m2$peso[k]
  100*sum((w/sum(w))*inv_logit(as.numeric(Z[k,,drop=FALSE]%*%fit$beta)))
}
pred_M3 <- function(fit,d) {
  100*as.numeric(H_m3[d,,drop=FALSE] %*% inv_logit(fit$eta))
}
pred_M1 <- function(beta,d) {
  as.numeric(model.matrix(delete.response(terms(f_ols)),data=base[d,,drop=FALSE]) %*% beta)
}
calcular_metricas <- function(y,pr) c(RMSE=sqrt(mean((pr-y)^2)),
    MAE=mean(abs(pr-y)), Pearson=cor(y,pr))

# 3. Baselines comparables: quitar de la EVALUACION el mismo departamento
# a partir de LODO original (33 departamentos), sin recalibrar.
base_pred <- data.frame(M1=detalle_lodo[[esc]]$M1,
                        M2=detalle_lodo[[esc]]$M2,
                        M3=detalle_lodo_m3[[esc]]$M3)
if(any(!is.finite(as.matrix(base_pred)))) stop('LODO original incompleto')
if(any(as.character(detalle_lodo[[esc]]$cod_depto)!=as.character(base$cod_depto)))
   stop('Orden base y detalle_lodo distinto')

# 4. Coeficientes de ajuste global original vs refit excluyendo Cook.
# Los coeficientes de M2 y M3 se comparan en ESCALA CRUDA.
med_global <- vapply(covs,function(v) mean(x_m2[[v]]),numeric(1))
sd_global <- vapply(covs,function(v) sd(x_m2[[v]]),numeric(1))
original <- list(
 M1=as.numeric(ajuste_m1[[esc]]$beta),
 M2=coef_escala_original(ajuste_m2[[esc]]$beta,med_global,sd_global),
 M3=c(coef_escala_original(ajuste_m3[[esc]]$fit$beta,med_global,sd_global),
      setNames(as.numeric(ajuste_m3[[esc]]$fit$a),paste0('espacial_a',seq_len(6)))))
names(original$M1)<-names(coef(ols))

metricas_robustez <- list()
cambios_coef <- list()
predicciones_robustez <- list()

for (q in seq_along(seleccion)) {
  infl <- seleccion[q]
  resto <- setdiff(seq_len(n),infl)
  etiqueta <- as.character(base$departamento[infl])
  cat('\n===== EXCLUYENDO DE LA CALIBRACION:',etiqueta,'(',q,'/',length(seleccion),') =====\n')

  # Reajuste completo sobre 32 dominios para medir cambio de parametros.
  z32 <- Z_con_entrenamiento(resto)
  b1 <- M1_ajustar_entrena(resto)
  m2 <- M2_ajustar_entrena(resto,semilla=25000+infl,n_starts=12)
  m3 <- M3_ajustar_entrena(resto,semilla=35000+infl,n_starts=8)
  b2 <- coef_escala_original(m2$fit$beta,m2$medias,m2$desvs)
  b3 <- c(coef_escala_original(m3$fit$beta,m3$medias,m3$desvs),
          setNames(as.numeric(m3$fit$a),paste0('espacial_a',seq_len(6))))
  para <- list(M1=b1,M2=b2,M3=b3)

  for (mod in names(para)) {
    ant <- original[[mod]]; nuevo <- para[[mod]]
    stopifnot(identical(names(ant),names(nuevo)))
    cambios_coef[[length(cambios_coef)+1L]] <- data.frame(
      depto_excluido=etiqueta, modelo=mod, coeficiente=names(ant),
      beta_original=as.numeric(ant),beta_sin_depto=as.numeric(nuevo),
      diferencia=as.numeric(nuevo-ant),
      cambio_relativo_pct=ifelse(abs(ant)>1e-8,100*(nuevo-ant)/abs(ant),NA_real_))
  }
  cambios_coef[[length(cambios_coef)+1L]] <- data.frame(
      depto_excluido=etiqueta, modelo=c('M2','M3'),coeficiente='tau',
      beta_original=c(ajuste_m2[[esc]]$tau,ajuste_m3[[esc]]$fit$tau),
      beta_sin_depto=c(m2$fit$tau,m3$fit$tau),
      diferencia=c(m2$fit$tau-ajuste_m2[[esc]]$tau,m3$fit$tau-ajuste_m3[[esc]]$fit$tau),
      cambio_relativo_pct=100*c(m2$fit$tau/ajuste_m2[[esc]]$tau-1,
                               m3$fit$tau/ajuste_m3[[esc]]$fit$tau-1))

  # VALIDACION LODO ANIDADA EN LOS 32 DOMINIOS RESTANTES:
  # cada prueba de esos 32 se ajusta con solo 31 dominios.
  p1<-p2<-p3<-rep(NA_real_,n)
  for (j in seq_along(resto)) {
    d <- resto[j]
    tr <- setdiff(resto,d)
    # Se conservan en el predictor las covariables y geografia del dominio excluido.
    bp1 <- M1_ajustar_entrena(tr)
    fp2 <- M2_ajustar_entrena(tr,semilla=5000+d,n_starts=5)
    fp3 <- M3_ajustar_entrena(tr,semilla=12000+200+d,n_starts=3)
    p1[d] <- pred_M1(bp1,d)
    p2[d] <- pred_M2(fp2$fit,fp2$Z,d)
    p3[d] <- pred_M3(fp3$fit,d)
    if(j%%8==0) cat('  LODO robustez:',j,'/',length(resto),'\n')
  }
  preds <- list(M1=p1,M2=p2,M3=p3)
  for(mod in names(preds)) {
    basal <- calcular_metricas(base$ipm_directo[resto],base_pred[[mod]][resto])
    nuevo <- calcular_metricas(base$ipm_directo[resto],preds[[mod]][resto])
    metricas_robustez[[length(metricas_robustez)+1L]] <- data.frame(
      depto_excluido=etiqueta, modelo=mod, n_evaluacion=length(resto),
      RMSE_basal_32=basal['RMSE'],RMSE_reajuste_32=nuevo['RMSE'],
      delta_RMSE=nuevo['RMSE']-basal['RMSE'],
      MAE_basal_32=basal['MAE'],MAE_reajuste_32=nuevo['MAE'],
      delta_MAE=nuevo['MAE']-basal['MAE'],
      Pearson_basal_32=basal['Pearson'],Pearson_reajuste_32=nuevo['Pearson'])
    predicciones_robustez[[length(predicciones_robustez)+1L]] <- data.frame(
      depto_excluido=etiqueta,modelo=mod,
      cod_depto=base$cod_depto[resto],departamento=base$departamento[resto],
      IPM_directo=base$ipm_directo[resto],
      pred_LODO_original=base_pred[[mod]][resto],
      pred_LODO_reajuste=preds[[mod]][resto])
  }
  # Checkpoint tras cada dominio: se conservan resultados aunque se interrumpa.
  write.csv(do.call(rbind,metricas_robustez),file.path(ruta,'02_robustez_metricas_LODO_E2.csv'),row.names=FALSE)
  write.csv(do.call(rbind,cambios_coef),file.path(ruta,'03_robustez_coeficientes_E2.csv'),row.names=FALSE)
  write.csv(do.call(rbind,predicciones_robustez),file.path(ruta,'04_robustez_predicciones_E2.csv'),row.names=FALSE)
  cat('  Resultados guardados para',etiqueta,'\n')
}

cat('\n===== DIAGNOSTICOS FORMALES OLS =====\n')
print(formal[formal$senal_cook | formal$senal_leverage | formal$senal_DFBETAS,],row.names=FALSE)
cat('\n===== ROBUSTEZ PREDICTIVA (LODO 32 DOMINIOS) =====\n')
print(do.call(rbind,metricas_robustez),row.names=FALSE)
cat('\nArchivos finales en: ',normalizePath(ruta),'\n')

# ============================================================================
# FIN DEL PROGRAMA UNICO
# ============================================================================
cat("\nPROGRAMA UNICO COMPLETADO. Compruebe avisos y archivos de salida.\n")
