# LIBRERÍAS
library(shiny)
library(dplyr)
library(ggplot2)
library(lubridate)
library(DT)
library(plotly)
if (!require(RColorBrewer)) install.packages("RColorBrewer")
library(RColorBrewer)

# PALETA DE COLORES
eroski_colores <- c("#E6001F", "#CC001C", "#FF3347", "#B30019", "#990015", "#005FAA", "#004C88")
paleta3 <- c("#FF3347", "#FFFFFF", "#005FAA")
paleta<- c("#FF3347",  "#990015", "#005FAA")

# CARGA DE DATOS
df_ticket <- read.csv("Datos/Transformados/tickets_enc_Bien.csv")
df_maestro <- read.csv("Datos/Transformados/maestroestr.csv")
df <- merge(df_ticket, df_maestro, by = "cod_est")
df$dia <- as.Date(df$dia)

df_cluster <- read.csv("Resultados/clientes_con_cluster.csv")
df_cluster$Cluster <- as.factor(df_cluster$Cluster)
df_cluster <- df_cluster[, -1]

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
  
  tabPanel("Análisis exploratorio",
           sidebarLayout(
             sidebarPanel(
               h4("Resumen de datos"),
               verbatimTextOutput("info_general"),
               selectInput("cliente_select", "Selecciona cliente:", choices = NULL),
               verbatimTextOutput("cliente_resumen"),
               conditionalPanel("input.subtab == 'productos'",
                                sliderInput("n_productos", "Número de productos a mostrar:", 5, 30, 10)),
               conditionalPanel("input.subtab == 'producto_mes'",
                                selectInput("producto_seleccionado", "Selecciona un producto:", choices = NULL))
             ),
             mainPanel(
               tabsetPanel(id = "subtab",
                           tabPanel("Productos más vendidos", value = "productos", DTOutput("tabla_productos")),
                           tabPanel("Ventas por mes", value = "mes", plotlyOutput("grafico_mes")),
                           tabPanel("Día de la semana", value = "dia", plotOutput("plot_dia")),
                           tabPanel("Producto por mes", value = "producto_mes", plotOutput("plot_producto_mes"))
               )
             )
           )
  ),
  
  tabPanel("Visualización de cada cluster",
           fluidPage(
             h3("Visualización de clusters", style = "color: #005FAA;"),
             fluidRow(
               column(12,
                      h4("¿Cómo se ha realizado la segmentación?"),
                      tags$div(style = "background-color:#f0f0f0; padding:20px; border-radius:10px; font-size:15px;",
                               HTML("
                                 <p><b>Metodología:</b> Se analizaron variables como el total de productos comprados, variedad, frecuencia semanal y diferenciación entre semana vs. fin de semana.</p>
                                 <p>Se imputaron outliers con <i>kNN</i> y se redujo la dimensionalidad mediante <b>PCA</b>. Posteriormente, se aplicó <b>k-means</b> con 3 clústeres.</p>
                                 <ul>
                                   <li><b>Clúster 1:</b> Familias con alto consumo y variedad.</li>
                                   <li><b>Clúster 2:</b> Compradores ocasionales, bajo volumen.</li>
                                   <li><b>Clúster 3:</b> Parejas jóvenes, consumo medio.</li>
                                 </ul>
                               ")
                      )
               )
             ),
             br(),
             fluidRow(
               column(6, plotOutput("plot_semana")),
               column(6, plotOutput("plot_findesemana"))
             ),
             br(),
             fluidRow(
               column(6, plotOutput("plot_total")),
               column(6, plotOutput("plot_variedad"))
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
  
  output$info_general <- renderPrint({
    cat("Total de tickets:", nrow(df), "\n")
    cat("Clientes distintos:", n_distinct(df$id_cliente_enc))
  })
  
  observe({
    updateSelectInput(session, "cliente_select", choices = sort(unique(df$id_cliente_enc)))
    updateSelectInput(session, "producto_seleccionado", choices = sort(unique(df$descripcion)))
  })
  
  output$cliente_resumen <- renderPrint({
    req(input$cliente_select)
    cliente_df <- df %>% filter(id_cliente_enc == input$cliente_select)
    top_prod <- cliente_df %>% count(descripcion, sort = TRUE) %>% slice_head(n = 3)
    cat("Productos más comprados por el cliente:\n")
    apply(top_prod, 1, function(row) cat("- ", row[1], "(", row[2], " veces)\n"))
  })
  
  output$tabla_productos <- renderDT({
    top_n <- input$n_productos
    productos_top <- df %>%
      group_by(descripcion) %>%
      summarise(frecuencia = n()) %>%
      arrange(desc(frecuencia)) %>%
      slice_head(n = top_n)
    datatable(productos_top, options = list(pageLength = top_n), rownames = FALSE)
  })
  
  # GRAFICO DE PASTEL MEJORADO
  output$grafico_mes <- renderPlotly({
    n_colores <- nrow(ventas_por_mes)
    colores_usados <- rep(paleta, length.out = n_colores)
    
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
  
  
  output$plot_dia <- renderPlot({
    ggplot(ventas_por_dia, aes(x = dia_semana, y = cantidad)) +
      geom_col(fill = eroski_colores[2]) +
      labs(title = "Compras por día de la semana", x = "Día", y = "Cantidad") +
      theme_minimal()
  })
  
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
  
  # GRÁFICOS DE CLUSTERS
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
  
  output$modelo_placeholder <- renderPrint({
    cat("Aquí irán los resultados del modelo de predicción o clasificación.")
  })
}

# LANZAR APP
shinyApp(ui = ui, server = server)














##
select_var_num = function(df){
  df_num = df %>% select(where(is.numeric))
  return(df_num)
}
df_cluster$Cluster<- as.factor(df_cluster$Cluster)
df_cluster<- df_cluster[,-1]
df_names = colnames(df_cluster)
df_num = select_var_num(df_cluster)
df_cha = df_cluster %>% select(-all_of(colnames(df_num)))
str(df_cluster)


paleta = c("#E6001F", "#CC001C", "#FF3347", "#B30019", "#990015" ,"#005FAA", "#004C88")
paleta3 = c("#FF3347", "#FFFFFF", "#005FAA")

ggplot(df_cluster, aes(x = Cluster, y = total_productos, fill = Cluster)) +
  geom_boxplot() +
  labs(title = "Total de productos comprados por Cluster",
       x = "Cluster", 
       y = "Total de productos comprados") +
  scale_fill_manual("Cluster", values = paleta3) +
  scale_y_continuous(breaks = seq(0,325, 25))

ggplot(df_cluster, aes(x = Cluster, y = productos_distintos, fill = Cluster)) +
  geom_violin() +
  labs(title = "Cantidad de productos distintos comprados por Cluster",
       x = "Cluster", 
       y = "Cantidad de productos distintos comprados") +
  scale_fill_manual("Cluster", values = paleta3) +
  scale_y_continuous(breaks = seq(0, 175, 25))

damedia = df_cluster %>% group_by(Cluster) %>% summarise(media = mean(dias_activos))

ggplot(damedia, aes(x = Cluster, y = media, fill = Cluster)) +
  geom_col(color = "#000000") +
  labs(title = "Media de días activos por Cluster",
       x = "Cluster", 
       y = "Media de días activos") +
  scale_fill_manual("Cluster", values = paleta3) +
  scale_y_continuous(breaks = seq(0,3,1))

ggplot(df_cluster, aes(x = Cluster, y = media_compras_por_semana, fill = Cluster)) +
  geom_boxplot() +
  labs(title = "Media de productos diferentes comprados por semana por Cluster",
       x = "Cluster", 
       y = "Media de productos diferentes comprados por semana") +
  scale_fill_manual("Cluster", values = paleta3) 

ggplot(df_cluster, aes(x = Cluster, y = compras_entre_semana, fill = Cluster)) +
  geom_boxplot() +
  labs(title = "Productos diferentes comprados de lunes a jueves por Cluster",
       x = "Cluster", 
       y = "Productos diferentes comprados de lunes a jueves") +
  scale_fill_manual("Cluster", values = paleta3) +
  scale_y_continuous(breaks = seq(0, 275, 25))

ggplot(df_cluster, aes(x = Cluster, y = compras_fin_de_semana, fill = Cluster)) +
  geom_boxplot() +
  labs(title = "Productos diferentes comprados de viernes a domingo por Cluster",
       x = "Cluster", 
       y = "Productos diferentes comprados de viernes a domingo") +
  scale_fill_manual("Cluster", values = paleta3) +
  scale_y_continuous(breaks = seq(0, 75, 25))

