install.packages("readxl")
install.packages("dplyr")
install.packages("tidyr")
install.packages("glimpse")
install.packages("Writexl")


library(readxl)
library(dplyr)
library(tidyr)
library(glimpse)
library(dplyr)
library(stringr)
library(writexl)

# Comenzamos leyendo el archivo y las hojas requeridas ####

ruta <- "anex-PMultidimensional-Departamental-2024.xlsx"
ruta2 <- "anex-PMultidimensional-Departamental-2023.xlsx"

ipm_dep <- read_excel(ruta, sheet = "IPM_Departamentos", skip = 11)
ipm_var <- read_excel(ruta, sheet = "IPM_Variables_Departamento ", skip = 11)

# Revisar estructura clave

glimpse(ipm_dep)
glimpse(ipm_var)

# IPM Departamental: total ####

# Guardar la primera fila (areas)
areas <- ipm_dep[1, ]

# Quitar esa fila del dataset
ipm_dep <- ipm_dep[-1, ]

# Extraer nombres actuales
nombres <- colnames(ipm_dep)

# Quitar "Departamento"
nombres_sin_dep <- nombres[-1]

# Crear vector de años repitiendo patrón (ajusta según tu base)
años <- rep(c("2018", "2019", "2020", "2021", "2022", "2023", "2024"), each = 3)

# Extraer áreas desde la fila que guardaste
areas_vec <- unlist(areas)[-1]

# Crear nombres nuevos
nuevos_nombres <- c(
  "Departamento",
  paste0(años, "_", areas_vec)
)

colnames(ipm_dep) <- nuevos_nombres

# Transformar hoja a formato largo

ipm_dep_long <- ipm_dep %>%
  pivot_longer(
    cols = -Departamento,
    names_to = c("Año", "Area"),
    names_sep = "_",
    values_to = "Dato"
  ) %>%
  mutate(
    Año = as.numeric(Año),
    Variable = "IPM",
    Dato = as.numeric(Dato)
  )

ipm_dep_long <- ipm_dep_long %>%
filter(
  !is.na(Dato)
  )
  

# IPM Departamental: variables ####

# columnas base
colnames(ipm_var)[1:2] <- c("Departamento", "Variable")

# Rellenar departamentos
ipm_var <- ipm_var %>%
  fill(Departamento)

# Eliminar filas que no son necesarias
ipm_var <- ipm_var %>%
  filter(!is.na(Variable))

# Separar filas de area
areas <- ipm_var[1, ]
ipm_var <- ipm_var[-1, ]

# Reconstruir nombres
nombres <- colnames(ipm_var)

base_cols <- c("Departamento", "Variable")

nombres_datos <- nombres[!nombres %in% base_cols]

n_anios <- length(nombres_datos) / 3

anios <- rep(2018:(2018 + n_anios - 1), each = 3)

areas_vec <- unlist(areas)[!names(areas) %in% base_cols]

nuevos_nombres <- c(
  base_cols,
  paste0(anios, "_", areas_vec)
)

colnames(ipm_var) <- nuevos_nombres

# Eliminar demás filas no son necesarias
ipm_var <- ipm_var %>%
  filter(Departamento!="Departamento")

# Pivot o formato largo
ipm_var_long <- ipm_var %>%
  pivot_longer(
    cols = -c(Departamento, Variable),
    names_to = c("Año", "Area"),
    names_sep = "_",
    values_to = "Dato"
  ) %>%
  mutate(
    Año = as.numeric(Año),
    Dato = as.numeric(Dato)
  )

# IPM Departamental: error estandar 2023-2024 ####

# Cargue de base de datos
ic_ipm <- read_excel(ruta, sheet = "IC_IPM", skip = 28)

# columnas base
colnames(ic_ipm)[1] <- c("Departamento")

# Rellenar departamentos
ic_ipm <- ic_ipm %>%
  fill(Departamento)

# Separar filas de area
areas <- ic_ipm[1, ]
ic_ipm <- ic_ipm[-1, ]

# Eliminar demás filas que no son necesarias
ic_ipm <- ic_ipm[1:33,]

# Reconstruir nombres
areas <- rep(c("Total", "Cabeceras", "Centros poblados  y rural disperso"), each = 5)
metricas <- rep(c("Estimacion", "Error", "LI", "LS", "CV"), times = 3 * n_anios)
anios <- rep(rep(2023:(2023 + n_anios - 1), each = 3), each = 5)

nuevos_nombres <- c(
  "Departamento",
  paste(anios, areas, metricas, sep = "_")
)

colnames(ic_ipm) <- nuevos_nombres

colnames(ic_ipm)

# Pivot o formato largo
ipm_dep_err_long1 <- ic_ipm %>%
  pivot_longer(
    cols = -Departamento,
    names_to = c("Año", "Area", "Variable"),
    names_sep = "_",
    values_to = "Dato"
  ) %>%
  mutate(
    Año = as.numeric(Año),
    Dato = as.numeric(Dato)
  )

# Ajustar formato y nombres para union
ipm_dep_err_long1 <- ipm_dep_err_long1 %>%
  filter(Variable == "Error")

ipm_dep_err_long1 <- ipm_dep_err_long1 %>%
  rename(Error = Dato)

ipm_dep_err_long1 <- ipm_dep_err_long1 %>%
  mutate(Variable = "IPM")

ipm_dep_err_long1 <- ipm_dep_err_long1 %>%
  mutate(Varianza = Error^2) %>%
  select(-Error)

# IPM Departamental: error estandar 2018-2023 ####

# Cargue de base de datos
ic_ipm <- read_excel(ruta2, sheet = "IC_IPM", skip = 30)

# columnas base
colnames(ic_ipm)[1] <- c("Departamento")

# Rellenar departamentos
ic_ipm <- ic_ipm %>%
  fill(Departamento)

# Separar filas de area
areas <- ic_ipm[1, ]
ic_ipm <- ic_ipm[-1, ]

# Eliminar demás filas que no son necesarias
ic_ipm <- ic_ipm[1:33,]

# Reconstruir nombres
areas <- rep(c("Total", "Cabeceras", "Centros poblados  y rural disperso"), each = 5)
metricas <- rep(c("Estimacion", "Error", "LI", "LS", "CV"), times = 3 * n_anios)
anios <- rep(rep(2018:(2018 + n_anios - 1), each = 3), each = 5)

nuevos_nombres <- c(
  "Departamento",
  paste(anios, areas, metricas, sep = "_")
)

colnames(ic_ipm) <- nuevos_nombres

colnames(ic_ipm)

# Pivot o formato largo
ipm_dep_err_long2 <- ic_ipm %>%
  pivot_longer(
    cols = -Departamento,
    names_to = c("Año", "Area", "Variable"),
    names_sep = "_",
    values_to = "Dato"
  ) %>%
  mutate(
    Año = as.numeric(Año),
    Dato = as.numeric(Dato)
  )

# Ajustar formato y nombres para union
ipm_dep_err_long2 <- ipm_dep_err_long2 %>%
  filter(Variable == "Error")

ipm_dep_err_long2 <- ipm_dep_err_long2 %>%
  rename(Error = Dato)

ipm_dep_err_long2 <- ipm_dep_err_long2 %>%
  mutate(Variable = "IPM")

ipm_dep_err_long2 <- ipm_dep_err_long2 %>%
  mutate(Varianza = Error^2) %>%
  select(-Error)

ipm_dep_err_long2 <- ipm_dep_err_long2 %>%
  filter(Año!=2023)

# IPM Departamental Variables: error estandar 2023-2024 ####

# Cargue de base de datos
ic_ipm <- read_excel(ruta, sheet = "IC_IPM", skip = 140)

# Crear columna auxiliar
ic_ipm <- ic_ipm %>%
  mutate(
    raw_text = str_squish(as.character(.[[1]]))
  )

# Creacion  variable departamento
ic_ipm <- ic_ipm %>%
  mutate(
    Departamento = ifelse(
      str_detect(raw_text, "Privaciones"),
      raw_text %>%
        str_remove("^IC\\. Privaciones por hogar según variable\\s+") %>%
        str_remove("\\s+[0-9]{4}-[0-9]{4}$"),
      NA
    )
  )

# Rellenar
ic_ipm <- ic_ipm %>%
  fill(Departamento)

# Limpiar filas
ic_ipm <- ic_ipm %>%
  filter(!is.na(raw_text))

# Ajustar nombre variables

ic_ipm <- ic_ipm %>%
  filter(
    !is.na(raw_text),
    !raw_text %in% c("Variable", "Cifras en porcentaje"),
    !str_detect(raw_text, "Privaciones"),
    !str_detect(raw_text, "2018-"),
    raw_text != ""
  )

ic_ipm <- ic_ipm %>%
  mutate(Variable = str_squish(raw_text))

# Seleccionar valor de error estandar
cols_datos <- ic_ipm %>%
  select(c(2:31)) %>%
  colnames()

cols_error <- cols_datos[seq(2, length(cols_datos), by = 5)]

# Ajustar nombre variables
nombres_error <- c(
  "Error_Total_2023",
  "Error_Cabeceras_2023",
  "Error_Centros poblados  y rural disperso_2023",
  "Error_Total_2024",
  "Error_Cabeceras_2024",
  "Error_Centros poblados  y rural disperso_2024"
)

length(cols_error) == length(nombres_error)

ic_ipm <- ic_ipm %>%
  rename_with(
    ~ nombres_error,
    all_of(cols_error)
  )

# Formato largo
ic_error_long1 <- ic_ipm %>%
  select(Departamento, Variable, starts_with("Error")) %>%
  pivot_longer(
    cols = starts_with("Error"),
    names_to = c("Tipo", "Area", "Año"),
    names_sep = "_",
    values_to = "Error"
  ) %>%
  select(-Tipo) %>%
  mutate(
    Año = as.numeric(Año),
    Error = as.numeric(Error),
    Varianza = Error^2
  )

# IPM Departamental Variables: error estandar 2018-2023 ####

# Cargue de base de datos
ic_ipm <- read_excel(ruta2, sheet = "IC_IPM", skip = 142)

# Crear columna auxiliar
ic_ipm <- ic_ipm %>%
  mutate(
    raw_text = str_squish(as.character(.[[1]]))
  )

# Creacion  variable departamento
ic_ipm <- ic_ipm %>%
  mutate(
    Departamento = ifelse(
      str_detect(raw_text, "Privaciones"),
      raw_text %>%
        str_remove("^IC\\. Privaciones por hogar según variable\\s+") %>%
        str_remove("\\s+[0-9]{4}-[0-9]{4}$"),
      NA
    )
  )

# Rellenar
ic_ipm <- ic_ipm %>%
  fill(Departamento)

# Limpiar filas
ic_ipm <- ic_ipm %>%
  filter(!is.na(raw_text))

# Ajustar nombre variables
ic_ipm <- ic_ipm %>%
  filter(
    !is.na(raw_text),
    !raw_text %in% c("Variable", "Cifras en porcentaje"),
    !str_detect(raw_text, "Privaciones"),
    !str_detect(raw_text, "2018-"),
    raw_text != ""
  )

ic_ipm <- ic_ipm %>%
  mutate(Variable = str_squish(raw_text))

# Seleccionar valor de error estandar
cols_datos <- ic_ipm %>%
  select(c(2:91)) %>%
  colnames()

cols_error <- cols_datos[seq(2, length(cols_datos), by = 5)]

# Ajustar nombre variables
nombres_error <- c(
  "Error_Total_2018",
  "Error_Cabeceras_2018",
  "Error_Centros poblados  y rural disperso_2018",
  "Error_Total_2019",
  "Error_Cabeceras_2019",
  "Error_Centros poblados  y rural disperso_2019",
  "Error_Total_2020",
  "Error_Cabeceras_2020",
  "Error_Centros poblados  y rural disperso_2020",
  "Error_Total_2021",
  "Error_Cabeceras_2021",
  "Error_Centros poblados  y rural disperso_2021",
  "Error_Total_2022",
  "Error_Cabeceras_2022",
  "Error_Centros poblados  y rural disperso_2022",
  "Error_Total_2023",
  "Error_Cabeceras_2023",
  "Error_Centros poblados  y rural disperso_2023"
)

# Comparar si coinciden nombre corregidos con los errores
length(cols_error) == length(nombres_error) # Debe decir True


ic_ipm <- ic_ipm %>%
  rename_with(
    ~ nombres_error,
    all_of(cols_error)
  )

# Formato largo
ic_error_long2 <- ic_ipm %>%
  select(Departamento, Variable, starts_with("Error")) %>%
  pivot_longer(
    cols = starts_with("Error"),
    names_to = c("Tipo", "Area", "Año"),
    names_sep = "_",
    values_to = "Error"
  ) %>%
  select(-Tipo) %>%
  mutate(
    Año = as.numeric(Año),
    Error = as.numeric(Error),
    Varianza = Error^2
  )

ic_error_long2 <- ic_error_long2 %>%
  filter(
    Año!=2023
  )

# IPM Departamental: total variables ####

unique(ipm_var_long$Departamento)
unique(ipm_dep_long$Departamento)

ipm_var_long <- ipm_var_long %>%
  mutate(
    Departamento = if_else(Departamento == "Bogotá", "Bogotá D.C.", Departamento)
  )

ipm_dep_completo <- bind_rows(ipm_dep_long,ipm_var_long)

# IPM Departamental: error estandar ####

ipm_dep_err_long <- bind_rows(ipm_dep_err_long1,ipm_dep_err_long2)

ic_error_long <- bind_rows(ic_error_long1,ic_error_long2) 
ic_error_long <- ic_error_long %>%
  filter(!is.na(Error)) %>%
  select(-Error)

ipm_dep_err <- bind_rows(ipm_dep_err_long,ic_error_long)


# Unir bases de datos ####

unique(ipm_dep_completo$Area)
unique(ipm_dep_err$Area)


# Unificar nombres de areas para union
ipm_dep_completo <- ipm_dep_completo %>%
  mutate(
    Area = Area %>%
      str_replace_all("\\r\\n", " ") %>%  # quitar saltos de línea
      str_replace_all("\\s+", " ") %>%    # múltiples espacios → uno
      str_trim()
  )

ipm_dep_err <- ipm_dep_err %>%
  mutate(
    Area = Area %>%
      str_replace_all("\\r\\n", " ") %>%
      str_replace_all("\\s+", " ") %>%
      str_trim()
  )

base_final <- ipm_dep_completo %>%
  left_join(
    ipm_dep_err,
    by = c("Departamento", "Area", "Variable", "Año")
  )

# Ajuste de base y añadimos codigo DANE

base_final <- base_final %>%
  arrange(Departamento, Variable, Area, Año, Varianza)
base_final$Departamento <- toupper(base_final$Departamento) # Mayúsculas
base_final$Departamento <- str_replace_all(
  as.character(base_final$Departamento),
  ",",
  ""
) # Quitar comas
base_final$Departamento <- iconv(base_final$Departamento,
                                 from = "UTF-8", to = "ASCII//TRANSLIT") # Quitar tildes
base_final$Departamento[base_final$Departamento == "SAN ANDRES"] <- 
  "ARCHIPIELAGO DE SAN ANDRES PROVIDENCIA Y SANTA CATALINA"

# Asignar códigos de departamentos
cod_dep <- read_excel("DIVIPOLA_Departamentos.xlsx", skip = 9)
cod_dep <- cod_dep %>%
  filter(!is.na(`Nombre`))
cod_dep$Departamento <- str_replace_all(
  as.character(cod_dep$Nombre),
  ",",
  ""
)
cod_dep$Departamento <- iconv(cod_dep$Departamento,
                                 from = "UTF-8", to = "ASCII//TRANSLIT") # Quitar tildes


# Cruzar datos con codigo DANE ####
IPM_final <- merge(cod_dep, base_final, by = "Departamento") %>%
  arrange(`Código`, Departamento, Variable, Area, Año, Dato, Varianza, LATITUD, LONGITUD)

IPM_final <- IPM_final %>% select(-Nombre)
IPM_final <- IPM_final %>% 
  select(`Código`, Departamento, Año, Area, Variable, Dato, Varianza, LATITUD, LONGITUD)

# Exportar resultados a Excel

write_xlsx(IPM_final, "IPM_Grupo-9.xlsx")