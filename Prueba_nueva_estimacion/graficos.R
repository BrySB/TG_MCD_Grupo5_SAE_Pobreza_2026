########################################### GRAFICO 1 ################################################
library(ggplot2)
library(dplyr)

# --- 1. DATAFRAME DE IMPORTANCIA DE VARIABLES (Modelo 3 - Errores Robustos) ---
df_importancia <- data.frame(
  Variable = c(
    "% Régimen Subsidiado", 
    "Tasa Matriculación (5-16)", 
    "Luces Nocturnas log(VIIRS)", 
    "ICEE Rural", 
    "Razón Mortalidad Materna (rmm42)", 
    "Mortalidad Infantil (<1 año)", 
    "Infraestructura Básica"
  ),
  Z_Value = c(9.80, 7.81, 6.58, 5.00, 3.33, 3.00, 2.00),
  Tipo = c(
    "Riesgo / Vulnerabilidad", 
    "Protector", 
    "Protector", 
    "Protector", 
    "Riesgo / Vulnerabilidad", 
    "Riesgo / Vulnerabilidad", 
    "Protector"
  )
)

# Reordenar el factor Variable para que se grafique en orden de importancia
df_importancia <- df_importancia %>%
  mutate(Variable = reorder(Variable, Z_Value))

# --- 2. GENERACIÓN DEL GRÁFICO ---
p_importancia <- ggplot(df_importancia, aes(x = Z_Value, y = Variable, fill = Tipo)) +
  geom_col(width = 0.7, color = "black", linewidth = 0.2) +
  geom_text(aes(label = sprintf("%.2f", Z_Value)), 
            hjust = -0.2, size = 3.8, fontface = "bold", color = "black") +
  scale_fill_manual(values = c("Protector" = "#2b5c8f", "Riesgo / Vulnerabilidad" = "#d95f02")) +
  scale_x_continuous(limits = c(0, 11), breaks = seq(0, 10, by = 2)) +
  labs(
    title = "Importancia Relativa de Covariables en el Modelo SAR (Modelo 3)",
    subtitle = "Magnitud del estadístico |z| con errores estándar robustos ante heterocedasticidad",
    x = "Estadístico |z| (Absoluto)",
    y = NULL,
    fill = "Efecto sobre el IPM:"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0),
    plot.subtitle = element_text(size = 10, color = "gray30", margin = margin(b = 10)),
    axis.text.y = element_text(face = "bold", size = 10, color = "black"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 10),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

# Desplegar gráfico
print(p_importancia)

######################################################################################################

#######################################GRAFICO 2#####################################################
library(sf)
library(tidyverse)
library(viridis)

# 1. Carga y preparación del Shapefile Municipal de Colombia
# Reemplaza 'municipios_colombia.geojson' o '.shp' por la ruta de tu mapa base
# Nota: Debe contener la columna del código DANE del municipio (ej. 'cod_mpio' o 'DPTO_CCDGO')
mapa_mpios <- mpios_shp %>%
  mutate(
    # Garantizar que el código municipal sea character de 5 dígitos (ej. "05001")
    cod_mpio = str_pad(as.character(cod_mpio), width = 5, side = "left", pad = "0")
  )

# 2. Preparar base analítica 2024 con formato de código alineado
base_mapa_2024 <- base_analitica_2024 %>%
  mutate(
    cod_mpio = str_pad(as.character(cod_mpio), width = 5, side = "left", pad = "0")
  )

# 3. Unir la geometría espacial con la estimación sintética del IPM 2024
mapa_ipm_2024 <- mapa_mpios %>%
  left_join(base_mapa_2024, by = "cod_mpio")

# 4. Generación del Mapa Coroplético de Alta Calidad
g_mapa_ipm <- ggplot(data = mapa_ipm_2024) +
  geom_sf(aes(fill = ipm_municipal_sintetico_2024), color = NA) +
  scale_fill_distiller(
    palette = "RdYlGn",
    direction = -1,  # Invierte para que Verde sea bajo y Rojo alto
    name = "IPM Sintético (%)",
    breaks = seq(0, 100, by = 20),
    labels = paste0(seq(0, 100, by = 20), "%"),
    na.value = "grey80"
  ) +
  labs(
    title = "Distribución Espacial del IPM Municipal Proyectado (2024)",
    subtitle = "Estimación en Pequeñas Áreas mediante Modelo Autorregresivo Espacial (SAR)",
    caption = "Fuente: Estimación propia basada en DANE y variables de entorno satelital (VIIRS)"
  ) +
  theme_void(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5, margin = margin(b = 4)),
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "grey30", margin = margin(b = 10)),
    plot.caption = element_text(size = 8, color = "grey40", hjust = 0.95, margin = margin(t = 10)),
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 9),
    legend.key.height = unit(1.2, "cm"),
    legend.key.width = unit(0.4, "cm"),
    plot.background = element_rect(fill = "white", color = NA)
  )

print(g_mapa_ipm)

# 5. Opcional: Guardar en alta resolución (PNG/PDF)
ggsave("mapa_ipm_sintetico_2024.png", g_mapa_ipm, width = 8, height = 10, dpi = 300)



########################################################################################################
################################### GRÁFICO 3 ##########################################################

library(sf)
library(tidyverse)
library(patchwork)

# 1. Preparar la geometría limpia (Asegurar cod_mpio a 5 caracteres)
mapa_mpios_limpio <- mpios_shp %>%
  mutate(
    cod_mpio = str_pad(trimws(as.character(cod_mpio)), width = 5, side = "left", pad = "0")
  )

# 2. Homologar cod_mpio en las bases analíticas y eliminar geometría secundaria
base_2018_clean <- base_analitica_2018 %>%
  st_drop_geometry() %>%
  mutate(
    cod_mpio = str_pad(trimws(as.character(cod_mpio)), width = 5, side = "left", pad = "0")
  )

base_2024_clean <- base_analitica_2024 %>%
  st_drop_geometry() %>%
  mutate(
    cod_mpio = str_pad(trimws(as.character(cod_mpio)), width = 5, side = "left", pad = "0")
  )

# 3. Unir datos a la cartografía
mapa_2018 <- mapa_mpios_limpio %>%
  left_join(base_2018_clean, by = "cod_mpio")

mapa_2024 <- mapa_mpios_limpio %>%
  left_join(base_2024_clean, by = "cod_mpio")

# --- MAPA 2018 ---
g_2018 <- ggplot(data = mapa_2018) +
  geom_sf(aes(fill = ipm_2018), color = "grey30", linewidth = 0.01) +
  scale_fill_distiller(
    palette = "RdYlGn",
    direction = -1,
    limits = c(0, 100),
    name = "IPM (%)"
  ) +
  labs(title = "A. IPM Observado (Censo 2018)") +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
    legend.position = "none"
  )

# --- MAPA 2024 (BENCHMARKING) ---
g_2024 <- ggplot(data = mapa_2024) +
  geom_sf(aes(fill = ipm_municipal_sintetico_2024), color = "grey30", linewidth = 0.01) +
  scale_fill_distiller(
    palette = "RdYlGn",
    direction = -1,
    limits = c(0, 100),
    name = "IPM (%)",
    breaks = seq(0, 100, by = 20),
    labels = paste0(seq(0, 100, by = 20), "%")
  ) +
  labs(title = "B. IPM Proyectado y Calibrado 2024") +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 9),
    legend.key.height = unit(1.2, "cm")
  )

# --- COMPOSICIÓN COMPARATIVA ---
mapa_comparativo <- (g_2018 + g_2024) +
  plot_layout(guides = "collect") &
  plot_annotation(
    title = "Evolución de la Distribución Espacial del IPM Municipal (2018 vs. 2024)",
    subtitle = "Comparación entre el Censo 2018 y la Estimación SAE-SAR Calibrada (Benchmarking DANE)",
    caption = "Fuente: Estimación propia basada en DANE, VIIRS y Modelo SAR (Modelo 3)",
    theme = theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      plot.subtitle = element_text(size = 10, hjust = 0.5, color = "grey30")
    )
  )

# Mostrar mapa
print(mapa_comparativo)

# Guardar en alta calidad
ggsave("mapa_comparativo_IPM_2018_2024.png", mapa_comparativo, width = 12, height = 8, dpi = 300)


######################################################################################################
########################################### GRÁFICO 4 ################################################

library(tidyverse)
library(viridis)
library(ggpubr) # Opcional, para añadir ecuaciones/métricas

# 1. Ajuste de la gráfica de dispersión y curva de tendencia
g_viirs_ipm <- ggplot(base_analitica_2024, aes(x = log_viirs, y = ipm_municipal_sintetico_2024)) +
  # Puntos con transparencia para evitar sobreploteo
  geom_point(aes(color = ipm_municipal_sintetico_2024), alpha = 0.5, size = 1.8) +
  
  # Línea de tendencia lineal (OLS simple)
  geom_smooth(method = "lm", color = "#d95f02", linewidth = 0.8, linetype = "dashed", se = FALSE) +
  
  # Tendencia no lineal smoothed (LOESS) para captar la curvatura del efecto
  geom_smooth(method = "loess", color = "#2b5c8f", linewidth = 1.2, se = TRUE, fill = "#2b5c8f", alpha = 0.15) +
  
  # Escala de color acorde a la paleta del mapa (Verde/Amarillo/Rojo)
  scale_color_distiller(
    palette = "RdYlGn",
    direction = -1,
    guide = "none" # Ocultar leyenda para concentrar foco en los ejes
  ) +
  
  # Personalización de ejes y etiquetas
  scale_x_continuous(breaks = seq(0, max(base_analitica_2024$log_viirs, na.rm = TRUE), by = 1)) +
  scale_y_continuous(breaks = seq(0, 100, by = 20), labels = paste0(seq(0, 100, by = 20), "%")) +
  
  labs(
    title = "Efecto de la Intensidad Lumínica Nocturna (VIIRS) sobre el IPM Proyectado (2024)",
    subtitle = expression(paste("Relación inversa estimada por el Modelo Espacial (", hat(beta)[log(VIIRS)], " = -0.2494, ", p, " < 0.001)")),
    x = expression(paste("Densidad Lumínica Nocturna en Escala Logarítmica: ", log(VIIRS + 1))),
    y = "Índice de Pobreza Multidimensional Sintético (%)",
    caption = "Fuente: Estimación propia basada en VIIRS-NPP Nighttime Lights y DANE"
  ) +
  
  # Estilo visual limpio
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13, color = "grey10"),
    plot.subtitle = element_text(size = 10, color = "grey30", margin = margin(b = 10)),
    axis.title = element_text(face = "bold", size = 10),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA)
  )

# Visualizar gráfico
print(g_viirs_ipm)

# 2. Guardar en alta calidad para el documento final
ggsave("efecto_viirs_vs_ipm_2024.png", g_viirs_ipm, width = 8, height = 6, dpi = 300)