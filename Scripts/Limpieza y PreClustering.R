if(!require(lubridate)){
  install.packages("lubridate")}
if(!require(dplyr)){
  install.packages("dplyr")}
if(!require(naniar)){
  install.packages("naniar")}
if(!require(tidyr)){
  install.packages("tidyr")}
if(!require(Matrix)){
  install.packages("Matrix")}
if(!require(Isa)){
  install.packages("Isa")}
if (!require(recommenderlab)){
  install.packages("recommenderlab")
} #version 1.0.6
if (!require(stringr)){
  install.packages("stringr")
}
library(lubridate)
library(dplyr)
library(naniar)
library(tidyr)
library(Matrix)
library(recommenderlab)
library(reshape2)
library(stringr)

#----------------------------------DATOS----------------------------------------
#maetrostr<- readRDS("Datos/maestroestr.RDS")
#objetivos <- readRDS("Datos/objetivos.RDS")
#tickets_enc <- readRDS("Datos/tickets_enc.RDS")

tickets_enc <- readRDS("Datos/tickets_enc.RDS")
str(tickets_enc)
tickets_enc$num_ticket<- as.character(tickets_enc$num_ticket)
tickets_enc$dia<- ymd(tickets_enc$dia)



str(tickets_enc)
summary(tickets_enc)

vis_miss(tickets_enc, `warn_large_data` = FALSE) # No se encuentran NA's

productos = readRDS("Datos/maestroestr.RDS")
str(productos)

#------------------- TRATAMIENTO DE TICKETS DUPLICADOS ---------------------------
 #IDENTIFICACION DE TICKETS DUPLICADOS
df<- tickets_enc %>% group_by(dia, num_ticket, id_cliente_enc) %>% 
  summarise(productos = list(table(cod_est)), .groups = "drop") 
  # hay tickets que se repiten esto esta mal ya que los tickets son unicos por compra
rm(df)

tickets_enc <- tickets_enc %>% 
  mutate(
    num_ticket = paste(num_ticket, id_cliente_enc))

  #comprobacion de que realmente no se repiten los numeros de ticket
df_ticket <- tickets_enc %>%
  group_by(num_ticket) %>%
  summarise(NumClientesUnicos = n_distinct(id_cliente_enc))
max(df_ticket$NumClientesUnicos)  # es 1 por lo que no se repiten
rm(df_ticket)

saveRDS(tickets_enc,"Datos/Transformados/tickets_enc_Bien.rds")

#------------------------------- CLUSTERING ------------------------------------
  #IDENTIFICACION DE DIA DE LA SEMANA DE CADA COMPRA
tickets_enc$dia_semana <- wday(tickets_enc$dia, week_start = 1)  # Usamos 1 como lunes


  #CARACTERIZACION DE LOS CLIENTES - COLUMNAS PARA CLUSTERING
# Calcular el número de días activos por cliente (días únicos de compra)

df_dias_activos <- tickets_enc %>%
  group_by(id_cliente_enc) %>%
  summarise(dias_activos = n_distinct(dia_semana))  


tickets_enc$semana <- week(tickets_enc$dia)  
tickets_enc$anio <- year(tickets_enc$dia)    

df_compras_semanales <- tickets_enc %>%
  group_by(id_cliente_enc, semana, anio) %>%
  summarise(compras_por_semana = n())

df_resultado <- df_dias_activos %>%
  left_join(df_compras_semanales %>%
              group_by(id_cliente_enc) %>%
              summarise(media_compras_semana = mean(compras_por_semana)),
            by = "id_cliente_enc")

tickets_enc <- tickets_enc %>%
  left_join(df_resultado, by = "id_cliente_enc")

rm(df_dias_activos, df_compras_semanales, df_resultado)

## PRODUCTOS POR CÓDIGO
productos = productos %>% 
  mutate(TIPO = str_sub(productos$cod_est, 1,2), .after = cod_est)  

productos = productos %>%
  mutate(TIPO = ifelse(TIPO == "01", "FRUTAS, VERDURAS Y OTROS PRODUCTOS VEGETALES", TIPO)) %>%
  mutate(TIPO = ifelse(TIPO == "02", "CARNE", TIPO)) %>%
  mutate(TIPO = ifelse(TIPO == "03", "CONGELADOS", TIPO)) %>%
  mutate(TIPO = ifelse(TIPO == "04", "EMBUTIDOS", TIPO)) %>%
  mutate(TIPO = ifelse(TIPO == "05", "LACTEOS", TIPO)) %>%
  mutate(TIPO = ifelse(TIPO == "06", "PANADERÍA, BOLLERÍA Y REPOSTERÍA", TIPO)) %>%
  mutate(TIPO = ifelse(TIPO == "07", "PESCADERÍA", TIPO)) %>%
  mutate(TIPO = ifelse(TIPO == "14", "PRODUCTOS PRECOCINADOS", TIPO)) %>%
  mutate(TIPO = ifelse(TIPO == "08", "ENLATADOS Y ENVASADOS", TIPO)) %>%
  mutate(TIPO = ifelse(TIPO == "10", "DESAYUNOS", TIPO)) %>%
  mutate(TIPO = ifelse(TIPO == "11", "BEBIDAS", TIPO)) %>%
  mutate(TIPO = ifelse(TIPO == "09", "SALSAS, ESPECIAS, PASTA Y EMBOTADOS", TIPO)) %>%
  mutate(TIPO = ifelse(TIPO == "12", "LIMPIEZA", TIPO)) %>%
  mutate(TIPO = ifelse(TIPO == "13", "HIGIENE PERSONAL", TIPO))

tickets_enc = left_join(tickets_enc, productos, by = "cod_est") %>% select(-descripcion)


## Crear df para clustering
datos_clientes <- tickets_enc %>%
  group_by(id_cliente_enc) %>%
  summarise(
    total_productos = n(),  # Número total de compras (filas en la base de datos)
    productos_distintos = n_distinct(cod_est),  # Número de productos únicos comprados
    dias_activos = unique(dias_activos),
    vegetales = sum(TIPO == "FRUTAS, VERDURAS Y OTROS PRODUCTOS VEGETALES"),
    carne = sum(TIPO == "CARNE"),
    congelados = sum(TIPO == "CONGELADOS"),
    embutido = sum(TIPO == "EMBUTIDOS"),
    lacteos = sum(TIPO == "LACTEOS"),
    gluten = sum(TIPO == "PANADERÍA, BOLLERÍA Y REPOSTERÍA"),
    pescaderia = sum(TIPO == "PESCADERÍA"),
    latas = sum(TIPO == "ENLATADOS Y ENVASADOS"),
    sepe = sum(TIPO == "SALSAS, ESPECIAS, PASTA Y EMBOTADOS"),
    desayuno = sum(TIPO == "DESAYUNOS"),
    bebida = sum(TIPO == "BEBIDAS"),
    limpieza = sum(TIPO == "LIMPIEZA"),
    higiene = sum(TIPO == "HIGIENE PERSONAL"),
    precocinados = sum(TIPO == "PRODUCTOS PRECOCINADOS")
    ) 

write.csv(datos_clientes, "Datos/Transformados/datos_para_clustering.csv")
rm(datos_clientes)


#------------------------- REDUCCION DE DIMENSIONALIDAD -----------------------------

datos <- readRDS("Datos/Transformados/tickets_enc_Bien.rds")
colnames(datos)
sum(duplicated(tickets_enc))

datos$X <- NULL

cliente_stats <- datos %>%
  group_by(id_cliente_enc) %>%
  summarise(
    total_compras = n(),
    productos_distintos = n_distinct(cod_est),
    diversidad = productos_distintos / total_compras
  )

umbral_inferior <- quantile(cliente_stats$total_compras, 0.25)
umbral_superior <- quantile(cliente_stats$total_compras, 0.95)

clientes_mas_compras <- cliente_stats %>%
  filter(total_compras > umbral_inferior & total_compras < umbral_superior)

summary(clientes_mas_compras$total_compras)

umbral_diversidad_baja <- quantile(clientes_mas_compras$diversidad, 0.25)

clientes_diversidad <- clientes_mas_compras %>%
  filter(diversidad > umbral_diversidad_baja) %>%
  pull(id_cliente_enc)

productos_frecuencia <- datos %>%
  count(cod_est, name = "frecuencia_producto")

umbral_productos <- quantile(productos_frecuencia$frecuencia_producto, 0.25)
umbral_productosa <- quantile(productos_frecuencia$frecuencia_producto, 0.75)

umbral_productos

productos_filtrado <- productos_frecuencia %>%
  filter(frecuencia_producto > umbral_productos & frecuencia_producto < umbral_productosa) %>%
  pull(cod_est)

producto_stats <- datos %>%
  group_by(cod_est) %>%
  summarise(
    total_ventas = n(),
    clientes_distintos = n_distinct(id_cliente_enc)
  )

umbral_productos2 <- quantile(producto_stats$clientes_distintos, 0.25)
umbral_productos3 <- quantile(producto_stats$total_ventas, 0.25)

productos <- producto_stats %>%
  filter(clientes_distintos > umbral_productos2 & total_ventas > umbral_productos3)


productos_filtrados <- productos %>% 
  filter(cod_est %in% productos_filtrado) %>% 
  pull(cod_est)



compras_reducido <- datos %>%
  filter(id_cliente_enc %in% clientes_diversidad,
         cod_est %in% productos_filtrados)
length(unique(datos$id_cliente_enc))
length(unique(compras_reducido$id_cliente_enc))
length(unique(datos$cod_est))

length(unique(compras_reducido$cod_est))

objetivos<- readRDS("Datos/objetivos.RDS")

clientes_protegidos <- c(objetivos$objetivo2$obj, objetivos$objetivo4$obj)

clientes_filtrados_final <- union(clientes_diversidad, clientes_protegidos)

productos_protegidos <- c(objetivos$objetivo1$obj,objetivos$objetivo3$obj)

productos_filtrados_final <- union(productos_filtrados, productos_protegidos)


compras_reducido <- datos %>%
  filter(id_cliente_enc %in% clientes_filtrados_final,
         cod_est %in% productos_filtrados_final)

rm(cliente_stats ,umbral_inferior, umbral_superior, clientes_mas_compras, umbral_diversidad_baja,
   clientes_diversidad, productos_frecuencia, umbral_productos, umbral_productosa,
   productos_filtrado, producto_stats, umbral_productos2, umbral_productos3, productos, productos_filtrados, 
   compras_reducido, objetivos, clientes_protegidos, clientes_filtrados_final, productos_protegidos, productos_filtrados_final)

#------------------------- CREACION DE LA MATRIZ -----------------------------

# adaptamos los datos para crear la matriz
set.seed(123)    
matriz_clientes_productos <- compras_reducido %>%
  group_by(id_cliente_enc, cod_est) %>%
  summarise(Frecuencia = n(), .groups = 'drop') %>%
  pivot_wider(names_from = cod_est, values_from = Frecuencia, values_fill = list(Frecuencia = 0))

head(matriz_clientes_productos)
matriz_clientes_productos <- compras_reducido %>%
  group_by(id_cliente_enc, cod_est) %>%
  summarise(Frecuencia = n(), .groups = 'drop') %>%
  pivot_wider(names_from = cod_est, values_from = Frecuencia, values_fill = list(Frecuencia = 0))

# Convertir la matriz a la clase 'realRatingMatrix'
matriz_clientes_productos_matrix <- as.matrix(matriz_clientes_productos[, -1])  # Eliminar la columna id_cliente_enc
rownames(matriz_clientes_productos_matrix) <- matriz_clientes_productos$id_cliente_enc

matriz_rrm <- as(matriz_clientes_productos_matrix, "realRatingMatrix")

matriz_rrm
saveRDS(matriz_rrm, "Datos/Transformados/matriz_rrm.RDS")

#Convertir la matriz a la clase 'sparseMatrix'
matriz_clientes_productos_matrix <- as.matrix(matriz_clientes_productos[, -1])  # Eliminar la columna id_cliente_enc
rownames(matriz_clientes_productos_matrix) <- matriz_clientes_productos$id_cliente_enc

matriz_sm <- as(matriz_clientes_productos_matrix, "sparseMatrix")

matriz_sm
saveRDS(matriz_sm, "Datos/Transformados/matriz_sm.RDS")

#Convertir la matriz a la clase 'binaryRatingMatrix'
matriz_clientes_productos_matrix <- as.matrix(matriz_clientes_productos[, -1])  # Eliminar la columna id_cliente_enc
rownames(matriz_clientes_productos_matrix) <- matriz_clientes_productos$id_cliente_enc

matriz_brm <- as(matriz_clientes_productos_matrix, "binaryRatingMatrix")

matriz_brm
saveRDS(matriz_brm, "Datos/Transformados/matriz_brm.RDS")

##Convertir la matriz a la clase binaria de Rsparse
matriz_base <- as(matriz_rrm, "matrix")
matriz_binaria <- as(matriz_base > 0, "dgCMatrix") # si partimos de realRatingMatrix

saveRDS(matriz_binaria, "Datos/Transformados/matriz_binariaRsparse.RDS")


# dividir matrices
set.seed(123)
evaluationScheem_rmm<-evaluationScheme(matriz_rrm, method = "split", train = 0.8, given = 3, goodRating = 1)
train_data_rrm<- getData(evaluationScheem_rmm, "train")
test_data_rrm<- getData(evaluationScheem_rmm, "known")

evaluationScheem_bmm<-evaluationScheme(matriz_brm, method = "split", train = 0.8, given = 3, goodRating = 1)
train_data_brm<- getData(evaluationScheem_bmm, "train")
test_data_brm<- getData(evaluationScheem_bmm, "known")


