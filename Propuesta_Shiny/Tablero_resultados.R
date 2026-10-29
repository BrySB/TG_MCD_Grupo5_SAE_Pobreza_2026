# =================================================
# MODIFICAR SEGÚN LA EL COMPU DESDE QUE SE EJECUTE
# ==============================================================================
RUTA_EXCEL <- "C:/Users/basbo/PUJ Cali/Erick Caicedo Ruiz - TDG Ciencia de Datos/salidas_modelos_1_2_3/resultados_SAE_IPM_modelos_1_2_3_CAR.xlsx"
RUTA_SHP   <- "C:/Users/basbo/PUJ Cali/Erick Caicedo Ruiz - TDG Ciencia de Datos/ADMINISTRATIVO/MGN_ADM_MPIO_GRAFICO.shp"

# ==============================================================================
# TABLERO INTERACTIVO SHINY: ESTIMACION EN AREAS PEQUEÑAS (SAE) - IPM 2024
# ==============================================================================

library(shiny)
library(shinydashboard)
library(readxl)
library(dplyr)
library(DT)
library(leaflet)
library(plotly)
library(sf)
library(ggplot2)
library(viridis)

# ==============================================================================
# 1. RUTA DE ARCHIVOS Y CARGA DE DATOS
# ==============================================================================
if (!file.exists(RUTA_EXCEL)) {
  stop(paste("No se encuentra el archivo de Excel en la ruta:", RUTA_EXCEL))
}

df_comparacion <- read_excel(RUTA_EXCEL, sheet = "Comparacion_municipal_3")
df_lodo        <- read_excel(RUTA_EXCEL, sheet = "LODO_3_modelos")

df_comparacion$cod_mpio <- as.integer(df_comparacion$cod_mpio)

mapa_municipal <- NULL
if (file.exists(RUTA_SHP)) {
  mapa_municipal <- sf::st_read(RUTA_SHP, quiet = TRUE)
  if ("mpio_cdpmp" %in% names(mapa_municipal)) {
    mapa_municipal$cod_mpio <- as.integer(as.character(mapa_municipal$mpio_cdpmp))
  } else if ("MPIO_CDPMP" %in% names(mapa_municipal)) {
    mapa_municipal$cod_mpio <- as.integer(as.character(mapa_municipal$MPIO_CDPMP))
  } else if ("MPIO_CCNCT" %in% names(mapa_municipal)) {
    mapa_municipal$cod_mpio <- as.integer(as.character(mapa_municipal$MPIO_CCNCT))
  }
}

deptos_disponibles <- sort(unique(df_comparacion$depto))

# ==============================================================================
# 2. INTERFAZ DE USUARIO (UI)
# ==============================================================================
ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(title = "SAE IPM Colombia 2024"),
  
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Evaluacion y Metricas", tabName = "evaluacion", icon = icon("chart-line")),
      menuItem("Mapas Municipales", tabName = "mapas", icon = icon("map-marked-alt")),
      menuItem("Tabla Detallada", tabName = "tabla", icon = icon("table"))
    ),
    hr(),
    conditionalPanel(
      condition = "input.tabs == 'mapas' || input.tabs == 'tabla'",
      div(style = "padding: 10px;",
          selectInput("filtro_depto", "Filtrar Departamento:",
                      choices = c("Todos los Departamentos" = "TODOS", deptos_disponibles),
                      selected = "TODOS"),
          hr(style = "border-color: #4f5962; margin-top: 5px; margin-bottom: 5px;"),
          selectInput("filtro_rango", "Filtrar por Rango de IPM:",
                      choices = c("Todos los rangos" = "TODOS",
                                  "0% - 10% (Baja pobreza)" = "R1",
                                  "10% - 25% (Moderada)" = "R2",
                                  "25% - 50% (Alta)" = "R3",
                                  "Mas de 50% (Muy alta)" = "R4"),
                      selected = "TODOS")
      )
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side { background-color: #ecf0f5; }
        .box.box-solid.box-primary>.box-header { background: #3c8dbc; background-color: #3c8dbc; }
      "))
    ),
    
    tabItems(
      # PESTANA 1: EVALUACION Y METRICAS DE MODELOS
      tabItem(tabName = "evaluacion",
              fluidRow(
                valueBoxOutput("vb_municipios", width = 4),
                valueBoxOutput("vb_deptos", width = 4),
                box(
                  title = NULL, status = "primary", solidHeader = FALSE, width = 4, background = "light-blue",
                  div(style = "padding: 0px;",
                      p(style = "margin-bottom: 2px; font-weight: bold;", "IPM Promedio Nacional (%)"),
                      div(style = "display: flex; align-items: center; justify-content: space-between;",
                          textOutput("val_prom_ipm"),
                          selectInput("modelo_promedio", NULL,
                                      choices = c("Modelo 1 (M1)" = "IPM_M1",
                                                  "Modelo 2 (M2)" = "IPM_M2",
                                                  "Modelo 3 (CAR)" = "IPM_M3"),
                                      selected = "IPM_M3", width = "130px")
                      )
                  )
                )
              ),
              fluidRow(
                box(
                  title = "Distribucion Comparativa de Estimaciones (M1 vs M2 vs M3)", status = "primary", solidHeader = TRUE, width = 6,
                  plotlyOutput("plot_densidad", height = 350)
                ),
                box(
                  title = "Desempeno Predictivo: RMSE por Escenarios (Validacion LODO)", status = "primary", solidHeader = TRUE, width = 6,
                  plotlyOutput("plot_lodo_escenarios", height = 350)
                )
              ),
              fluidRow(
                box(
                  title = "Tabla Resumen LODO (Sin Paginacion)", status = "info", solidHeader = TRUE, width = 12,
                  DTOutput("tabla_lodo_resumen", height = "auto")
                )
              ),
              fluidRow(
                box(
                  title = "Notas Metodologicas del Proyecto", status = "primary", solidHeader = TRUE, width = 12,
                  p("Este tablero presenta los resultados del modelado en Areas Pequeñas (SAE) para la Pobreza Multidimensional (IPM 2024) en Colombia como 
                    parte del trabajo de maestria para optar por el titulo en Ciencia de Datos. Sus códigos y fuentes se encuentran completos en el repositorio oficial:"),
                  tags$a(href = "https://github.com/BrySB/TG_MCD_Grupo5_SAE_Pobreza_2026", target = "_blank", "https://github.com/BrySB/TG_MCD_Grupo5_SAE_Pobreza_2026"),
                  tags$ul(
                    tags$li(strong("Modelo 1 (M1):"), " Fay-Herriot sintetico departamental."),
                    tags$li(strong("Modelo 2 (M2):"), " Desagregacion municipal no lineal acotada (logit) con benchmarking."),
                    tags$li(strong("Modelo 3 (M3):"), " Modelo no lineal con componente espacial CAR de rango reducido y benchmarking.")
                  )
                )
              )
      ),
      
      # PESTANA 2: MAPAS MUNICIPALES Y DISPERSION
      tabItem(tabName = "mapas",
              fluidRow(
                box(
                  title = "Control Exclusivo del Mapa", status = "primary", solidHeader = TRUE, width = 6,
                  selectInput("modelo_mapa", "Seleccionar Modelo SAE para el Mapa:",
                              choices = c("Modelo 1 (Fay-Herriot)" = "IPM_M1",
                                          "Modelo 2 (No Lineal)" = "IPM_M2",
                                          "Modelo 3 (Espacial CAR)" = "IPM_M3"),
                              selected = "IPM_M3")
                ),
                box(
                  title = "Controles Exclusivos del Grafico de Dispersion", status = "success", solidHeader = TRUE, width = 6,
                  fluidRow(
                    column(6, selectInput("eje_x", "Eje X (Modelo):",
                                          choices = c("Modelo 1" = "IPM_M1", "Modelo 2" = "IPM_M2", "Modelo 3" = "IPM_M3"),
                                          selected = "IPM_M1")),
                    column(6, selectInput("eje_y", "Eje Y (Modelo):",
                                          choices = c("Modelo 1" = "IPM_M1", "Modelo 2" = "IPM_M2", "Modelo 3" = "IPM_M3"),
                                          selected = "IPM_M3"))
                  )
                )
              ),
              fluidRow(
                box(
                  title = "Mapa Interactivo Municipal (Paleta Tradicional)", status = "primary", solidHeader = TRUE, width = 6,
                  leafletOutput("mapa_leaflet", height = 680)
                ),
                box(
                  title = "Correlacion Cruzada (Paleta Plasma, Tamaño = Poblacion)", status = "success", solidHeader = TRUE, width = 6,
                  plotlyOutput("plot_dispersion", height = 680)
                )
              )
      ),
      
      # PESTANA 3: TABLA DE ESTIMACIONES DETALLADAS
      tabItem(tabName = "tabla",
              fluidRow(
                box(
                  title = "Matriz Consolidada de Estimaciones Municipales (1.121 Municipios)", status = "primary", solidHeader = TRUE, width = 12,
                  DTOutput("tabla_detallada")
                )
              )
      )
    )
  )
)

# ==============================================================================
# 3. LOGICA DEL SERVIDOR (SERVER)
# ==============================================================================
server <- function(input, output, session) {
  
  datos_filtrados <- reactive({
    df <- df_comparacion
    
    if (input$filtro_depto != "TODOS") {
      df <- df %>% filter(depto == input$filtro_depto)
    }
    
    rango_sel <- if (!is.null(input$filtro_rango)) input$filtro_rango else "TODOS"
    val_col <- input$modelo_mapa
    
    if (rango_sel == "R1") {
      df <- df %>% filter(!!sym(val_col) >= 0 & !!sym(val_col) < 10)
    } else if (rango_sel == "R2") {
      df <- df %>% filter(!!sym(val_col) >= 10 & !!sym(val_col) < 25)
    } else if (rango_sel == "R3") {
      df <- df %>% filter(!!sym(val_col) >= 25 & !!sym(val_col) < 50)
    } else if (rango_sel == "R4") {
      df <- df %>% filter(!!sym(val_col) >= 50)
    }
    
    df
  })
  
  output$vb_municipios <- renderValueBox({
    valueBox(nrow(df_comparacion), "Municipios Analizados", icon = icon("city"), color = "navy")
  })
  
  output$vb_deptos <- renderValueBox({
    valueBox(length(unique(df_comparacion$depto)), "Dominios Departamentales", icon = icon("map"), color = "teal")
  })
  
  output$val_prom_ipm <- renderText({
    mod_sel <- input$modelo_promedio
    prom <- mean(df_comparacion[[mod_sel]], na.rm = TRUE)
    paste0(round(prom, 2), "%")
  })
  
  output$plot_densidad <- renderPlotly({
    p <- ggplot(df_comparacion) +
      geom_density(aes(x = IPM_M1, fill = "Modelo 1"), alpha = 0.4) +
      geom_density(aes(x = IPM_M2, fill = "Modelo 2"), alpha = 0.4) +
      geom_density(aes(x = IPM_M3, fill = "Modelo 3 (CAR)"), alpha = 0.4) +
      scale_fill_manual(name = "Modelos SAE", values = c("Modelo 1" = "#31688e", "Modelo 2" = "#35b779", "Modelo 3 (CAR)" = "#fde725")) +
      labs(x = "IPM Estimado (%)", y = "Densidad") +
      theme_minimal()
    
    ggplotly(p) %>% layout(legend = list(orientation = "h", x = 0.2, y = -0.2))
  })
  
  output$plot_lodo_escenarios <- renderPlotly({
    p <- ggplot(df_lodo, aes(x = escenario, y = RMSE, fill = modelo)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_viridis_d(name = "Modelo") +
      labs(x = "Escenario Metodologico", y = "RMSE (Validacion LODO)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 0, hjust = 0.5))
    
    ggplotly(p) %>% layout(legend = list(orientation = "h", x = 0.1, y = -0.3))
  })
  
  output$tabla_lodo_resumen <- renderDT({
    datatable(df_lodo, options = list(pageLength = -1, paging = FALSE, dom = 't'), rownames = FALSE)
  })
  
  # Mapa Leaflet con su paleta tradicional predeterminada "YlOrRd"
  output$mapa_leaflet <- renderLeaflet({
    df <- datos_filtrados()
    
    if (!is.null(mapa_municipal)) {
      map_data <- mapa_municipal %>% inner_join(df, by = "cod_mpio")
    } else {
      map_data <- NULL
    }
    
    val_col <- input$modelo_mapa
    pal <- colorNumeric(palette = "YlOrRd", domain = df_comparacion[[val_col]], na.color = "#808080")
    
    m <- leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -74.2973, lat = 4.5709, zoom = 6)
    
    if (!is.null(map_data) && nrow(map_data) > 0) {
      m <- m %>%
        addPolygons(
          data = map_data,
          fillColor = ~pal(map_data[[val_col]]),
          weight = 0.5,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.85,
          popup = ~paste0(
            "<strong>Municipio: </strong>", mpio, "<br>",
            "<strong>Departamento: </strong>", depto, "<br>",
            "<strong>Codigo DANE: </strong>", cod_mpio, "<br>",
            "<strong>Poblacion: </strong>", format(poblacion, big.mark = ","), "<br>",
            "<strong>Estimacion: </strong>", round(map_data[[val_col]], 2), "%"
          ),
          highlightOptions = highlightOptions(
            weight = 2, color = "#222", dashArray = "", fillOpacity = 0.95, bringToFront = TRUE
          )
        ) %>%
        addLegend(pal = pal, values = df_comparacion[[val_col]], opacity = 0.8, title = "IPM (%)", position = "bottomright")
    }
    m
  })
  
  output$plot_dispersion <- renderPlotly({
    df <- datos_filtrados()
    
    x_var <- input$eje_x
    y_var <- input$eje_y
    
    r_val <- round(cor(df[[x_var]], df[[y_var]], use = "complete.obs"), 4)
    
    df$tooltip_text <- paste0(
      "<b>Municipio:</b> ", df$mpio, " (", df$depto, ")<br>",
      "<b>Poblacion:</b> ", format(df$poblacion, big.mark = ","), "<br>",
      "<b>", x_var, ":</b> ", round(df[[x_var]], 2), "%<br>",
      "<b>", y_var, ":</b> ", round(df[[y_var]], 2), "%"
    )
    
    p <- ggplot(df, aes_string(x = x_var, y = y_var, size = "poblacion", color = x_var, text = "tooltip_text")) +
      geom_point(alpha = 0.85) +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray40") +
      scale_color_viridis_c(option = "plasma", name = "Intensidad") +
      scale_size_continuous(range = c(3, 16), guide = "none") +
      labs(x = paste("Estimacion", x_var), y = paste("Estimacion", y_var)) +
      theme_minimal()
    
    g <- ggplotly(p, tooltip = "text")
    
    g <- g %>% layout(
      annotations = list(
        list(
          x = 0.05, y = 0.95,
          xref = "paper", yref = "paper",
          text = paste0("<b>Correlacion (r): </b>", r_val),
          showarrow = FALSE,
          font = list(size = 13, color = "#2c3e50"),
          bgcolor = "rgba(255, 255, 255, 0.85)",
          bordercolor = "#3c8dbc",
          borderwidth = 1.5,
          borderpad = 6
        )
      )
    )
    
    g
  })
  
  output$tabla_detallada <- renderDT({
    df <- datos_filtrados()
    datatable(
      df,
      extensions = 'Buttons',
      options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel'),
        pageLength = 15,
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
      ),
      rownames = FALSE
    )
  })
}

shinyApp(ui, server)