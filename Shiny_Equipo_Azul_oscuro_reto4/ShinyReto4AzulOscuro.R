# LIBRERÍAS
library(shiny)
library(dplyr)
library(ggplot2)
library(lubridate)
library(DT)
library(plotly)
if (!require(RColorBrewer)) install.packages("RColorBrewer")
library(RColorBrewer)

# PALETAS DE COLORES
eroski_colores <- c("#E6001F", "#CC001C", "#FF3347", "#B30019", "#990015", "#005FAA", "#004C88")
paleta3 <- c("#FF3347", "#990015", "#005FAA")

# DATOS
df_ticket <- readRDS("Datos/Transformados/tickets_enc_Bien.rds")
df_maestro <- read.csv("Datos/Transformados/maestroestr.csv")
df <- merge(df_ticket, df_maestro, by = "cod_est")
df$dia <- as.Date(df$dia)

df_cluster <- read.csv("Resultados/clientes_con_cluster.csv")
df_cluster$Cluster <- as.factor(df_cluster$Cluster)
df_cluster <- df_cluster[, -1]

# VENTAS POR MES Y DÍA
ventas_por_mes <- df %>%
  mutate(Mes = month(dia, label = TRUE, abbr = FALSE)) %>%
  group_by(Mes) %>%
  summarise(Total = n())

df$dia_semana <- wday(df$dia, label = TRUE, abbr = FALSE, week_start = 1)
df$dia_semana <- factor(df$dia_semana,
                        levels = c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo"))
ventas_por_dia <- df %>%
  group_by(dia_semana) %>%
  summarise(cantidad = n())

# UI
ui <- navbarPage(
  "EROSKI",
  tabPanel("Inicio",
           fluidPage(
             titlePanel("🛒 Bienvenido al Panel de Análisis de Clientes - EROSKI"),
             
             fluidRow(
               column(12,
                      tags$div(style = "background-color:#F7F7F7; padding: 25px; border-radius: 10px;",
                               HTML("
                               <h4 style='color:#005FAA;'>📊 ¿Qué puedes hacer con esta aplicación?</h4>
                               <ul style='font-size:16px;'>
                                 <li><b>Análisis exploratorio:</b> Consulta productos más vendidos, hábitos por día de la semana, y detalle por cliente.</li>
                                 <li><b>Visualización de clusters:</b> Descubre perfiles de consumidores según su comportamiento.</li>
                                 <li><b>Modelización:</b> (Próximamente) Predicción y segmentación avanzada.</li>
                               </ul>
                               <p style='font-size:16px;'>Utiliza el menú superior para navegar entre secciones y descubrir información útil sobre los patrones de consumo de los clientes.</p>
                               <hr>
                               <p><i>Última actualización: mayo 2025</i></p>
                             ")
                      )
               )
             ),
             br(),
             fluidRow(
               column(6,
                      tags$p("Desarrollado por el equipo Azul Oscuro", 
                             style = "text-align:right; color:#666; font-style:italic; padding-top:30px;")
               )
             )
           )),
  
  tabPanel("Análisis exploratorio",
           sidebarLayout(
             sidebarPanel(
               h4("Resumen de datos"),
               verbatimTextOutput("info_general"),
               hr(),
               h4("Selecciona cliente:"),
               selectizeInput("cliente_select", label = NULL, 
                              choices = NULL, options = list(placeholder = 'Escribe para buscar...')),
               uiOutput("cliente_info_ui"),
               hr(),
               h4("NULL", style = "color: #666; font-style: italic;"),
               conditionalPanel("input.subtab == 'productos'",
                                sliderInput("n_productos", "Número de productos a mostrar:", 5, 30, 10)),
               conditionalPanel("input.subtab == 'producto_mes'",
                                selectInput("producto_seleccionado", "Selecciona un producto:", choices = NULL))
             ),
             mainPanel(
               tabsetPanel(id = "subtab",
                           tabPanel("Productos más vendidos", value = "productos", 
                                    DTOutput("tabla_productos") %>% withSpinner()),
                           tabPanel("Ventas por mes", value = "mes", 
                                    plotlyOutput("grafico_mes") %>% withSpinner()),
                           tabPanel("Día de la semana", value = "dia", 
                                    plotOutput("plot_dia") %>% withSpinner()),
                           tabPanel("Producto por mes", value = "producto_mes", 
                                    plotOutput("plot_producto_mes") %>% withSpinner()),
                           tabPanel("Historial de entradas", value = "historial",
                                    fluidRow(
                                      column(6,
                                             h4("Resumen del cliente seleccionado"),
                                             verbatimTextOutput("info_cliente_historial"),
                                             actionButton("guardar_entrada", "Guardar entrada", 
                                                          icon = icon("save"),
                                                          style = "background-color: #005FAA; color: white;"),
                                             actionButton("borrar_historial", "Borrar historial",
                                                          icon = icon("trash")),
                                             downloadButton("descargar_historial", "Descargar historial",
                                                            style = "background-color: #E6001F; color: white;")
                                      ),
                                      column(6,
                                             DTOutput("tabla_historial") %>% withSpinner()
                                      )
                                    )
                           )
               )
             )
           )
  ),
  
  tabPanel("Visualización de cada cluster",
           fluidPage(
             h3("Visualización de clusters", style = "color: #005FAA;"),
             tabsetPanel(
               tabPanel("Variables numéricas",
                        fluidRow(
                          column(6, plotOutput("plot_semana") %>% withSpinner()),
                          column(6, plotOutput("plot_findesemana") %>% withSpinner())
                        ),
                        br(),
                        fluidRow(
                          column(6, plotOutput("plot_total") %>% withSpinner()),
                          column(6, plotOutput("plot_variedad") %>% withSpinner())
                        )
               ),
               tabPanel("Categorías clave",
                        sidebarLayout(
                          sidebarPanel(
                            selectInput("var_categoria", "Selecciona variable de categoría:", 
                                        choices = names(df_cluster)[names(df_cluster) %in% c("carne", "congelados", "desayuno", "lacteos", "gluten", "higiene", "bebida", "embutido", "latas", "precocinados", "sepe", "vegetales", "pescaderia")]),
                            radioButtons("tipo_grafico", "Tipo de gráfico:",
                                         choices = c("Boxplot interactivo (plotly)" = "plotly", "Boxplot clásico (ggplot2)" = "ggplot"))
                          ),
                          mainPanel(
                            tabsetPanel(
                              tabPanel("Gráfico", plotlyOutput("box_categoria") %>% withSpinner()),
                              tabPanel("Resumen", tableOutput("media_categoria"))
                            )
                          )
                        )
               ),
               tabPanel("Resumen por clúster",
                        h4("Media de variables clave por clúster"),
                        tableOutput("resumen_clusters"),
                        br(),
                        h4("Visualización gráfica"),
                        plotlyOutput("grafico_resumen_clusters") %>% withSpinner()
               )
             )
           )
  ),
  
  tabPanel("Resultado de modelización",
           fluidPage(
             h3("Modelización de comportamiento"),
             verbatimTextOutput("modelo_placeholder")
           )
  )
)

# SERVER
server <- function(input, output, session) {
  
  # Resumen de clusters
  output$resumen_clusters <- renderTable({
    df_cluster %>%
      group_by(Cluster) %>%
      summarise(across(where(is.numeric), ~round(mean(.x, na.rm = TRUE), 2)), .groups = "drop")
  })
  
  # Info general
  output$info_general <- renderPrint({
    cat("Total de tickets:", nrow(df), "\n")
    cat("Clientes distintos:", n_distinct(df$id_cliente_enc))
  })
  
  # Actualizar selección de clientes
  observe({
    clientes <- sort(unique(df$id_cliente_enc))
    updateSelectizeInput(session, "cliente_select", choices = clientes, server = TRUE)
    updateSelectInput(session, "cliente_historial", choices = clientes)
    updateSelectInput(session, "producto_seleccionado", choices = sort(unique(df$descripcion)))
  })
  
  # Info del cliente en formato UI mejorado
  output$cliente_info_ui <- renderUI({
    req(input$cliente_select)
    cliente_df <- df %>% filter(id_cliente_enc == input$cliente_select)
    top_prod <- cliente_df %>% 
      count(descripcion, sort = TRUE) %>% 
      slice_head(n = 3)
    
    if(nrow(top_prod) == 0) {
      return(tags$p("No hay datos de compras para este cliente."))
    }
    
    tags$div(
      style = "background-color: #F7F7F7; padding: 10px; border-radius: 5px;",
      h5("Productos más conocidos por el cliente:", style = "color: #005FAA;"),
      tags$ul(
        style = "margin-left: 20px;",
        lapply(1:nrow(top_prod), function(i) {
          tags$li(paste0(top_prod$descripcion[i], " (", top_prod$n[i], " veces)"))
        })
      )
    )
  })
  
  # Sincronizar selección de cliente entre pestañas
  observeEvent(input$cliente_select, {
    updateSelectInput(session, "cliente_historial", selected = input$cliente_select)
  })
  
  observeEvent(input$cliente_historial, {
    updateSelectizeInput(session, "cliente_select", selected = input$cliente_historial)
  })
  
  # Tabla de productos más vendidos
  output$tabla_productos <- renderDT({
    top_n <- input$n_productos
    productos_top <- df %>%
      group_by(descripcion) %>%
      summarise(frecuencia = n()) %>%
      arrange(desc(frecuencia)) %>%
      slice_head(n = top_n)
    datatable(productos_top, options = list(pageLength = top_n), rownames = FALSE)
  })
  
  # Gráfico de ventas por mes
  output$grafico_mes <- renderPlotly({
    colores_usados <- rep(eroski_colores, length.out = nrow(ventas_por_mes))
    plot_ly(
      ventas_por_mes,
      labels = ~Mes,
      values = ~Total,
      type = "pie",
      marker = list(colors = colores_usados),
      textposition = 'inside',
      textinfo = 'label+percent',
      hoverinfo = 'text',
      text = ~paste("Mes:", Mes, "<br>Total:", Total)
    ) %>%
      layout(title = "Total de productos por mes", showlegend = TRUE)
  })
  
  # Gráfico de ventas por día de la semana
  output$plot_dia <- renderPlot({
    ggplot(ventas_por_dia, aes(x = dia_semana, y = cantidad)) +
      geom_col(fill = eroski_colores[2]) +
      labs(title = "Compras por día de la semana", x = "Día", y = "Cantidad") +
      theme_minimal()
  })
  
  # Gráfico de producto por mes
  output$plot_producto_mes <- renderPlot({
    req(input$producto_seleccionado)
    datos <- df %>%
      filter(descripcion == input$producto_seleccionado) %>%
      mutate(Mes = month(dia, label = TRUE, abbr = FALSE)) %>%
      group_by(Mes) %>%
      summarise(Frecuencia = n())
    ggplot(datos, aes(x = Mes, y = Frecuencia)) +
      geom_col(fill = eroski_colores[6]) +
      labs(title = paste("Compras por mes del producto:", input$producto_seleccionado),
           x = "Mes", y = "Cantidad") +
      theme_minimal()
  })
  
  # Historial de entradas
  historial <- reactiveVal(data.frame(
    Timestamp = numeric(),
    ID_Cliente = character(),
    Total_Productos = numeric(),
    Distintos = numeric(),
    Tickets = numeric(),
    stringsAsFactors = FALSE
  ))
  
  # Info del cliente para el historial
  output$info_cliente_historial <- renderText({
    req(input$cliente_historial)
    datos <- df %>% filter(id_cliente_enc == input$cliente_historial)
    paste0("Total de tickets: ", nrow(datos), 
           "\nProductos distintos: ", n_distinct(datos$descripcion),
           "\nTotal productos: ", sum(datos$cantidad, na.rm = TRUE))
  })
  
  # Guardar entrada al historial
  observeEvent(input$guardar_entrada, {
    req(input$cliente_historial)
    datos <- df %>% filter(id_cliente_enc == input$cliente_historial)
    nueva_fila <- data.frame(
      Timestamp = as.numeric(Sys.time()),
      ID_Cliente = input$cliente_historial,
      Total_Productos = sum(datos$cantidad, na.rm = TRUE),
      Distintos = n_distinct(datos$descripcion),
      Tickets = nrow(datos),
      stringsAsFactors = FALSE
    )
    historial(rbind(historial(), nueva_fila))
  })
  
  # Borrar historial
  observeEvent(input$borrar_historial, {
    historial(data.frame(
      Timestamp = numeric(),
      ID_Cliente = character(),
      Total_Productos = numeric(),
      Distintos = numeric(),
      Tickets = numeric(),
      stringsAsFactors = FALSE
    ))
  })
  
  # Tabla del historial
  output$tabla_historial <- DT::renderDataTable({
    DT::datatable(historial(), options = list(pageLength = 5))
  })
  
  # Descargar historial
  output$descargar_historial <- downloadHandler(
    filename = function() {
      paste("historial_clientes_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(historial(), file, row.names = FALSE)
    }
  )
  
  # Gráficos de clusters
  output$plot_semana <- renderPlot({
    ggplot(df_cluster, aes(x = Cluster, y = compras_entre_semana, fill = Cluster)) +
      geom_boxplot() +
      labs(title = "Compras entre semana por Cluster", x = "Cluster", y = "Productos distintos") +
      scale_fill_manual(values = paleta3) +
      theme_minimal()
  })
  
  output$plot_findesemana <- renderPlot({
    ggplot(df_cluster, aes(x = Cluster, y = compras_fin_de_semana, fill = Cluster)) +
      geom_boxplot() +
      labs(title = "Compras en fin de semana por Cluster", x = "Cluster", y = "Productos distintos") +
      scale_fill_manual(values = paleta3) +
      theme_minimal()
  })
  
  output$plot_total <- renderPlot({
    ggplot(df_cluster, aes(x = Cluster, y = total_productos, fill = Cluster)) +
      geom_boxplot() +
      labs(title = "Total productos comprados por Cluster", x = "Cluster", y = "Total") +
      scale_fill_manual(values = paleta3) +
      theme_minimal()
  })
  
  output$plot_variedad <- renderPlot({
    ggplot(df_cluster, aes(x = Cluster, y = productos_distintos, fill = Cluster)) +
      geom_boxplot() +
      labs(title = "Productos distintos comprados por Cluster", x = "Cluster", y = "Variedad") +
      scale_fill_manual(values = paleta3) +
      theme_minimal()
  })
  
  output$box_categoria <- renderPlotly({
    req(input$var_categoria)
    plot_ly(df_cluster, x = ~Cluster, y = as.numeric(df_cluster[[input$var_categoria]]),
            type = "box", color = ~Cluster, colors = paleta3) %>%
      layout(title = paste("Distribución de", input$var_categoria, "por Cluster"),
             yaxis = list(title = "Valor"), boxmode = "group")
  })
  
  output$media_categoria <- renderTable({
    req(input$var_categoria)
    df_cluster %>%
      group_by(Cluster) %>%
      summarise(Media = mean(.data[[input$var_categoria]], na.rm = TRUE)) %>%
      rename(`Media por Cluster` = Media)
  })
  
  output$modelo_placeholder <- renderPrint({
    cat("Aquí irán los resultados del modelo de predicción o clasificación.")
  })
  
  output$grafico_resumen_clusters <- renderPlotly({
    resumen <- df_cluster %>%
      group_by(Cluster) %>%
      summarise(
        total_productos = mean(total_productos, na.rm = TRUE),
        productos_distintos = mean(productos_distintos, na.rm = TRUE),
        congelados = mean(congelados, na.rm = TRUE),
        higiene = mean(higiene, na.rm = TRUE),
        bebida = mean(bebida, na.rm = TRUE)
      )
    
    resumen_long <- resumen %>%
      tidyr::pivot_longer(-Cluster, names_to = "Variable", values_to = "Media")
    
    plot_ly(resumen_long, 
            x = ~Variable, 
            y = ~Media, 
            color = ~factor(Cluster), 
            colors = paleta3,
            type = 'bar',
            text = ~paste("Cluster", Cluster, "<br>", round(Media, 2)),
            hoverinfo = "text",
            barmode = 'group') %>%
      layout(title = "Media de variables clave por clúster",
             xaxis = list(title = ""),
             yaxis = list(title = "Media"))
  })
}

# EJECUTAR
shinyApp(ui = ui, server = server)


















