library(recommenderlab)
library(ggplot2)
library(dplyr)
library(rsparse)
objetivos <- readRDS("Datos/objetivos.RDS")
sparsematrix<-readRDS("Datos/Transformados/matriz_binariaRsparse.RDS")
Productos<-readRDS("Datos/maestroestr.RDS")
#Segundo objetivo
objetivos<-objetivos[[2]]

matriz_usuarios<-sparsematrix[objetivos$obj,] #Matriz filtrada
dim(matriz_usuarios)
matriz <- as(sparsematrix, "dgCMatrix")
# Paso 2: Entrenar modelo ALS
set.seed(123)
modelo <- WRMF$new(rank = 50, lambda = 0.1, max_iter = 10, nthread = parallel::detectCores() - 1)
user_emb <- modelo$fit_transform(matriz)
item_emb <- modelo$components
predicciones<-modelo$predict(matriz_usuarios, k = 1, not_recommend =  )

valoraciones<-attr(predicciones,"scores")

productos_recomendados<- as.data.frame(attr(predicciones,"ids"))
colnames(productos_recomendados)<-"cod_est"
nombres_de_productos <- inner_join(
  Productos, 
  productos_recomendados, 
  by = "cod_est"
)
df_final<-cbind(nombres_de_productos,valoraciones)
rownames(df_final)<-rownames(productos_recomendados)
