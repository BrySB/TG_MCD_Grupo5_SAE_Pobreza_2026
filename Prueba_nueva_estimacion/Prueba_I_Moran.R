library(sf)
library(spdep)
library(spatialreg)
library(tidyverse)

# 1. Limpieza de códigos DANE
mpios_shp_clean <- mpios_shp %>%
  mutate(cod_mpio = str_pad(trimws(as.character(cod_mpio)), width = 5, side = "left", pad = "0"))

base_2018_clean <- base_analitica_2018 %>%
  # Remueve cualquier atributo sf en caso de que lo tenga y la vuelve data.frame plano
  st_drop_geometry() %>% 
  mutate(cod_mpio = str_pad(trimws(as.character(cod_mpio)), width = 5, side = "left", pad = "0"))

# 2. Unión tabular limpia y eliminación de NAs
data_espacial_completa <- mpios_shp_clean %>%
  inner_join(base_2018_clean, by = "cod_mpio") %>%
  drop_na(ipm_logit, tasa_matriculacion_5_16, desercion, 
          infraestructura_basica, mortalidad_infantil_1, 
          Rec_ICA_PC, log_viirs, pct_subsidiado)

# 3. Matriz de Pesos Espaciales
nb_mpios <- poly2nb(data_espacial_completa)
w_list <- nb2listw(nb_mpios, style = "W", zero.policy = TRUE)

# 4. Modelo OLS
mod_ols <- lm(ipm_logit ~ tasa_matriculacion_5_16 + desercion +
                infraestructura_basica + mortalidad_infantil_1 +
                log(Rec_ICA_PC + 1) + log_viirs + pct_subsidiado, 
              data = data_espacial_completa)

# 5. Prueba de Moran sobre los residuos
moran_test_result <- lm.morantest(mod_ols, w_list, zero.policy = TRUE)

# Mostrar resultado en consola