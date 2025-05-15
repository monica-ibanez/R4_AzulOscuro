eroski_derivados <- c(
  "#E6001F",  # Rojo principal
  "#CC001C",  # Rojo oscuro
  "#FF3347",  # Rojo claro vibrante
  "#B30019",  # Rojo intenso
  "#990015",  # Rojo profundo
  "#005FAA",  # Azul corporativo
  "#004C88",  # Azul oscuro
  "#337FCC",  # Azul medio
  "#669FCC",  # Azul pastel
  "#003366"   # Azul muy oscuro
)
df_ticket<- readRDS("Datos/tickets_enc.RDS")
str(df_ticket)
df_maestro<- readRDS("Datos/maestroestr.RDS")
str(df_maetro) 
unique(df_maestro$cod_est)
# Guardar como CSV
write.csv(df_maestro, file = "Datos/Transformados/maestroestr.csv", row.names = FALSE)

#Cargar csv
df_maestro<- read.csv("Datos/Transformados/maestroestr.csv")
df_ticket<- read.csv("Datos/Transformados/tickets_enc_Bien.csv")
df_maestro

#Libreria
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)

#Juntar los dos excel
df<- merge(df_ticket, df_maestro, by="cod_est")
df
colnames(df)

########ANALISIS ESTADISTICO-DESCRIPTIVO#############

#Numero de tikets
nrow(df)
n_distinct(df$num_ticket)
#Fechas
df$dia<- as.Date(df$dia)
min(df$dia)
max(df$dia)




#Productos más vendidos
productos<-df%>%group_by(descripcion)%>%summarise(numero=n())%>%
  arrange(desc(numero)) %>%
  slice_head(n = 10)
productos
g_productos<- ggplot(productos, aes(x = reorder(descripcion, numero), y = numero)) +
  geom_col(fill = "#E6001F") +  # Rojo principal
  coord_flip() +
  labs(
    title = "Top 10 productos",
    x = "Producto",
    y = "Frecuencia"
  ) +
  theme_minimal()
g_productos

#Mes que más compran
df$dia<- ymd(df$dia)
mes <- df %>%
  mutate(Mes = month(dia, label = TRUE, abbr = FALSE)) %>% group_by(Mes) %>% summarise(Total = n())
mes
g_mes<- ggplot(mes, aes(x = Mes, y = Total)) +
  geom_bar(stat = "identity", fill = "#E6001F") +  # Color rojo de tu paleta
  theme_minimal() +
  labs(title = "Total de productos por mes",
       x = "Mes",
       y = "Cantidad")
g_mes

#Media de productos al dia
media_productos_por_dia<-df

media_productos_por_dia$dia <- ymd(media_productos_por_dia$dia)

# Obtener día de la semana como nombre
media_productos_por_dia$dia_semana <- wday(media_productos_por_dia$dia, label = TRUE, abbr = FALSE, week_start = 1)

# Ordenar los días
media_productos_por_dia$dia_semana <- factor(
  media_productos_por_dia$dia_semana,
  levels = c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo")
)

# Agrupar y contar
media_productos_por_dia <- media_productos_por_dia %>%
  group_by(dia_semana) %>%
  summarise(cantidad = n())
media_productos_por_dia

# Gráfico de barras
ggplot(media_productos_por_dia, aes(x = dia_semana, y = cantidad)) +
  geom_col(fill = "#E6001F") +
  labs(
    title = "Cantidad de productos por día de la semana",
    x = "Día de la semana",
    y = "Cantidad"
  ) +
  theme_minimal()

#Clientes más activos 
clientes_activos <- df %>%
  group_by(id_cliente_enc) %>%
  summarise(n_tickets = n()) %>%
  arrange(desc(n_tickets)) %>%
  head(10)

print(clientes_activos)

g_clientes_activos<- ggplot(clientes_activos, aes(x = reorder(id_cliente_enc, n_tickets), y = n_tickets)) +
  geom_col(fill = "#005FAA") +
  coord_flip() +
  labs(title = "Top 10 clientes más activos",
       x = "ID Cliente",
       y = "Número de compras") +
  theme_minimal()
g_clientes_activos

#Productos que compraron esos clientes
top_cliente <- clientes_activos$id_cliente_enc[1]

productos_por_cliente <- df %>%
  filter(id_cliente_enc == top_cliente) %>%
  group_by(descripcion) %>%
  summarise(veces_comprado = n()) %>%
  arrange(desc(veces_comprado)) %>%
  head(10)

print(productos_por_cliente)

g_productos_por_cliente<-ggplot(productos_por_cliente, aes(x = reorder(descripcion, veces_comprado), y = veces_comprado)) +
  geom_col(fill = "#E6001F") +
  coord_flip() +
  labs(title = paste("Productos más comprados por el cliente", top_cliente),
       x = "Producto",
       y = "Cantidad") +
  theme_minimal()

g_productos_por_cliente

#Productos de compra frecuente
productos_frecuentes <- df %>%
  group_by(id_cliente_enc, descripcion) %>%
  summarise(veces = n()) %>%
  filter(veces > 1) %>%
  arrange(desc(veces))

print(head(productos_frecuentes, 20))
#Productos más repetidos
producto_favoritos <- productos_frecuentes %>%
  group_by(descripcion) %>%
  summarise(clientes_repetidores = n()) %>%
  arrange(desc(clientes_repetidores))

print(head(producto_favoritos, 10))

g_producto_favoritos<-ggplot(head(producto_favoritos, 10), aes(x = reorder(descripcion, clientes_repetidores), y = clientes_repetidores)) +
  geom_col(fill = "#CC001C") +
  coord_flip() +
  labs(title = "Top productos con clientes repetidores",
       x = "Producto",
       y = "Nº de clientes que repiten") +
  theme_minimal()

g_producto_favoritos



#########################
##############################
# ANÁLISIS DE COMPRAS - EROSKI
##############################

# --- Librerías ---
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)

# --- Paleta de colores corporativos ---
eroski_colores <- list(
  rojo_principal = "#E6001F",
  rojo_oscuro = "#CC001C",
  rojo_claro = "#FF3347",
  rojo_intenso = "#B30019",
  rojo_profundo = "#990015",
  azul_corporativo = "#005FAA",
  azul_oscuro = "#004C88",
  azul_medio = "#337FCC",
  azul_pastel = "#669FCC",
  azul_muy_oscuro = "#003366"
)

# --- Función general para gráficos de barras ---
graficar_barras <- function(data, xvar, yvar, titulo = "", 
                            xlab = NULL, ylab = NULL, color = "#E6001F", flip = TRUE, etiquetas = TRUE) {
  xlab <- xlab %||% xvar
  ylab <- ylab %||% yvar
  
  p <- ggplot(data, aes(x = reorder(.data[[xvar]], .data[[yvar]]), y = .data[[yvar]])) +
    geom_col(fill = color) +
    labs(title = titulo, x = xlab, y = ylab) +
    theme_minimal()
  
  if (etiquetas) {
    p <- p + geom_text(aes(label = .data[[yvar]]), hjust = ifelse(flip, 1.1, 0), color = "black", size = 3)
  }
  
  if (flip) {
    p <- p + coord_flip()
  }
  
  return(p)
}

# --- Carga de datos ---
df_ticket <- read.csv("Datos/Transformados/tickets_enc_Bien.csv")
df_maestro <- read.csv("Datos/Transformados/maestroestr.csv")
df <- merge(df_ticket, df_maestro, by = "cod_est")
df$dia <- as.Date(df$dia)

##############################
# 1. ANÁLISIS DESCRIPTIVO
##############################

cat("Número total de registros:", nrow(df), "\n")
cat("Número total de tickets únicos:", n_distinct(df$num_ticket), "\n")
cat("Fechas disponibles: desde", min(df$dia), "hasta", max(df$dia), "\n")
cat("Número total de clientes:", n_distinct(df$id_cliente_enc), "\n")
cat("Número total de productos:", n_distinct(df$descripcion), "\n")

##############################
# 2. PRODUCTOS MÁS VENDIDOS
##############################

productos_top <- df %>%
  group_by(descripcion) %>%
  summarise(frecuencia = n()) %>%
  arrange(desc(frecuencia)) %>%
  slice_head(n = 10)

g_productos <- ggplot(productos_top, aes(x = reorder(descripcion, frecuencia), y = frecuencia)) +
  geom_segment(aes(xend = descripcion, y = 0, yend = frecuencia), color = eroski_colores$rojo_oscuro, linewidth = 1) +
  geom_point(size = 4, color = eroski_colores$rojo_principal) +
  coord_flip() +
  labs(title = "Top 10 productos más vendidos (Lollipop)",
       x = "Producto", y = "Frecuencia") +
  theme_minimal()

print(productos_top)
print(g_productos)

##############################
# 3. DISTRIBUCIÓN MENSUAL
##############################

df <- df %>%
  mutate(Mes = month(dia, label = TRUE, abbr = FALSE))

ventas_por_mes <- df %>%
  group_by(Mes) %>%
  summarise(Total = n())

g_mes <- graficar_barras(ventas_por_mes, "Mes", "Total", 
                         titulo = "Total de productos vendidos por mes",
                         xlab = "Mes", ylab = "Cantidad",
                         color = eroski_colores$rojo_claro,
                         flip = FALSE)

print(g_mes)

##############################
# 4. FRECUENCIA POR DÍA DE LA SEMANA
##############################

df$dia_semana <- wday(df$dia, label = TRUE, abbr = FALSE, week_start = 1)
df$dia_semana <- factor(df$dia_semana, levels = c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo"))

ventas_por_dia <- df %>%
  group_by(dia_semana) %>%
  summarise(cantidad = n())

g_dia_semana <- graficar_barras(ventas_por_dia, "dia_semana", "cantidad",
                                titulo = "Ventas por día de la semana",
                                xlab = "Día", ylab = "Cantidad",
                                color = eroski_colores$rojo_intenso,
                                flip = FALSE)

print(g_dia_semana)

##############################
# 5. CLIENTES MÁS ACTIVOS
##############################

clientes_top <- df %>%
  group_by(id_cliente_enc) %>%
  summarise(n_tickets = n()) %>%
  arrange(desc(n_tickets)) %>%
  slice_head(n = 10)

g_clientes_top <- graficar_barras(clientes_top, "id_cliente_enc", "n_tickets", 
                                  titulo = "Top 10 clientes más activos",
                                  xlab = "ID Cliente", ylab = "Nº de compras",
                                  color = eroski_colores$azul_corporativo)

print(clientes_top)
print(g_clientes_top)

##############################
# 6. PRODUCTOS COMPRADOS POR EL CLIENTE MÁS ACTIVO
##############################

cliente_top <- clientes_top$id_cliente_enc[1]

productos_cliente_top <- df %>%
  filter(id_cliente_enc == cliente_top) %>%
  group_by(descripcion) %>%
  summarise(veces_comprado = n()) %>%
  arrange(desc(veces_comprado)) %>%
  slice_head(n = 10)

g_productos_cliente <- graficar_barras(productos_cliente_top, "descripcion", "veces_comprado", 
                                       titulo = paste("Top productos del cliente", cliente_top),
                                       xlab = "Producto", ylab = "Cantidad",
                                       color = eroski_colores$rojo_principal)

print(productos_cliente_top)
print(g_productos_cliente)

