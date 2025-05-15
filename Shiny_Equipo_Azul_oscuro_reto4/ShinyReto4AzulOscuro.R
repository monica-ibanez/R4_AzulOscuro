library(shiny)
library(dplyr)
library(ggplot2)
library(lubridate)
library(DT)
library(plotly)

# Paletas de colores
eroski_colores <- c(
  "#E6001F", "#CC001C", "#FF3347", "#B30019", "#990015",
  "#005FAA", "#004C88"
)
paleta3 <- c("#FF3347", "#FFFFFF", "#005FAA")

# Carga de datos
df_ticket <- read.csv("Datos/Transformados/tickets_enc_Bien.csv")
df_maestro <- read.csv("Datos/Transformados/maestroestr.csv")
df <- merge(df_ticket, df_maestro, by = "cod_est")
df$dia <- as.Date(df$dia)
df_cluster <- read.csv("Resultados/clientes_con_cluster.csv")

# Preparar clustering
df_cluster$Cluster <- as.factor(df_cluster$Cluster)
df_cluster <- df_cluster[,-1]

# Datos preprocesados
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
               
               conditionalPanel(
                 condition = "input.subtab == 'productos'",
                 sliderInput("n_productos", "Número de productos a mostrar:", min = 5, max = 30, value = 10)
               ),
               conditionalPanel(
                 condition = "input.subtab == 'producto_mes'",
                 selectInput("producto_seleccionado", "Selecciona un producto:", choices = NULL)
               )
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
             h3("Visualización de clusters"),
             selectInput("variable_cluster", "Selecciona variable:",
                         choices = c("total_productos", "productos_distintos", "dias_activos",
                                     "media_compras_por_semana", "compras_entre_semana", "compras_fin_de_semana")),
             plotOutput("plot_variable_cluster")
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
  # Información general
  output$info_general <- renderPrint({
    total_tickets <- nrow(df)
    total_clientes <- n_distinct(df$id_cliente_enc)
    cat("Total de tickets:", total_tickets, "\n")
    cat("Clientes distintos:", total_clientes)
  })
  
  # Lista de clientes
  observe({
    updateSelectInput(session, "cliente_select",
                      choices = sort(unique(df$id_cliente_enc)))
  })
  
  # Resumen del cliente
  output$cliente_resumen <- renderPrint({
    req(input$cliente_select)
    cliente_df <- df %>% filter(id_cliente_enc == input$cliente_select)
    top_prod <- cliente_df %>%
      count(descripcion, sort = TRUE) %>%
      slice_head(n = 3)
    cat("Productos más comprados por el cliente:\n")
    apply(top_prod, 1, function(row) cat("- ", row[1], "(", row[2], " veces)\n"))
  })
  
  # Tabla productos
  output$tabla_productos <- renderDT({
    top_n <- input$n_productos
    productos_top <- df %>%
      group_by(descripcion) %>%
      summarise(frecuencia = n()) %>%
      arrange(desc(frecuencia)) %>%
      slice_head(n = top_n)
    datatable(productos_top, options = list(pageLength = top_n), rownames = FALSE)
  })
  
  # Gráfico pastel
  output$grafico_mes <- renderPlotly({
    colores_usados <- eroski_colores[1:nrow(ventas_por_mes)]
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
    ) %>% layout(title = "Total de productos por mes (Pastel)", showlegend = TRUE)
  })
  
  # Día de semana
  output$plot_dia <- renderPlot({
    ggplot(ventas_por_dia, aes(x = dia_semana, y = cantidad)) +
      geom_col(fill = eroski_colores[2]) +
      labs(title = "Compras por día de la semana", x = "Día", y = "Cantidad") +
      theme_minimal()
  })
  
  # Productos selector
  observe({
    updateSelectInput(session, "producto_seleccionado", choices = sort(unique(df$descripcion)))
  })
  
  # Producto por mes
  output$plot_producto_mes <- renderPlot({
    req(input$producto_seleccionado)
    datos <- df %>%
      filter(descripcion == input$producto_seleccionado) %>%
      mutate(Mes = month(dia, label = TRUE, abbr = FALSE)) %>%
      group_by(Mes) %>%
      summarise(Frecuencia = n()) %>%
      arrange(Mes)
    ggplot(datos, aes(x = Mes, y = Frecuencia)) +
      geom_col(fill = eroski_colores[6]) +
      labs(title = paste("Compras por mes del producto:", input$producto_seleccionado),
           x = "Mes", y = "Cantidad") +
      theme_minimal()
  })
  
  # Variable dinámica de cluster
  output$plot_variable_cluster <- renderPlot({
    req(input$variable_cluster)
    var <- input$variable_cluster
    
    if (var == "dias_activos") {
      damedia <- df_cluster %>%
        group_by(Cluster) %>%
        summarise(media = mean(dias_activos))
      ggplot(damedia, aes(x = Cluster, y = media, fill = Cluster)) +
        geom_col(color = "#000000") +
        labs(title = "Media de días activos por Cluster", x = "Cluster", y = "Media") +
        scale_fill_manual("Cluster", values = paleta3) +
        scale_y_continuous(breaks = seq(0, 3, 1))
    } else {
      ggplot(df_cluster, aes_string(x = "Cluster", y = var, fill = "Cluster")) +
        geom_boxplot() +
        labs(title = paste("Distribución de", var, "por Cluster"),
             x = "Cluster", y = var) +
        scale_fill_manual("Cluster", values = paleta3)
    }
  })
  
  # Placeholder modelado
  output$modelo_placeholder <- renderPrint({
    cat("Aquí irán los resultados del modelo de predicción o clasificación.")
  })
}

# Ejecutar app
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

