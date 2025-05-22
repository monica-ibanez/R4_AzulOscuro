##### LIBRERÍAS ################################################################
#PLUMBER
if (!require(plumber)){
  install.packages("plumber")
  library(plumber)
} else {
  library(plumber)
}
if (!require(recommenderlab)){
  install.packages("recommenderlab")
  library(recommenderlab)
} else {
  library(recommenderlab)
}
if (!require(rsparse)){
  install.packages("rsparse")
  library(rsparse)
} else {
  library(rsparse)
}
if (!require(dplyr)){
  install.packages("dplyr")
  library(dplyr)
} else {
  library(dplyr)
}


#* @apiTitle Recomendador de productos de Eroski
#* @apiDescription Esta Interfaz de Programación de Aplicaciones tiene como objetivo recomendar productos a clientes de Eroski

#* @serializer json
#* @param id_usuario
#* @param recomendaciones
#* @post Recomendaciones

x <- function(id_usuario = as.character("004c726a1f31128ad5b9f70b9f6f000d"), recomendaciones = 1) {
  
  matriz <- readRDS("C:/Users/aritz/OneDrive - Mondragon Unibertsitatea/Escritorio/BDATA1/Github/Datos/Transformados/matriz_binariaRsparse.RDS")
  matriz <- as(matriz, "dgCMatrix")
  
  set.seed(1)
  modelo <- WRMF$new(rank = 10, lambda = 0.1, feedback = "implicit", dynamic_lambda = TRUE)
  user_emb <- modelo$fit_transform(matriz)
  
  predicciones <- modelo$predict(matriz, k = recomendaciones)
  
  user_index <- which(rownames(matriz) == id_usuario)
  if (length(user_index) == 0) stop("ID de usuario no encontrado en la matriz")
  
  objeto<-attr(predicciones,"ids")
  objeto<-as.data.frame(objeto)
  df<-objeto[id_usuario,]
  return(df)
}
  

