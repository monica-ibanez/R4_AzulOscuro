library(recommenderlab)
library(ggplot2)
library(dplyr)
library(rsparse)
objetivos <- readRDS("Datos/objetivos.RDS")
sparsematrix<-readRDS("Datos/Transformados/matriz_binariaRsparse.RDS")
Productos<-readRDS("Datos/maestroestr.RDS")
#Segundo objetivo
objetivos<-objetivos[[3]]

matriz <- as(sparsematrix, "dgCMatrix")

modelo <- WRMF$new(rank = 10, lambda = 0.1,dynamic_lambda = TRUE, feedback = "implicit" )
set.seed(1)
columnas_excluir<-as.matrix(which(!colnames(matriz) %in% objetivos$obj))
matriz_no_recomendar<-Matrix::sparseMatrix(i =rep(1:nrow(matriz),each = length(columnas_excluir))
                                                          , j = rep(1:nrow(columnas_excluir),times = nrow(matriz))
                                                          , x = 1,
                                                          dims =  dim(matriz))
user_emb <- modelo$fit_transform(matriz)
item_emb <- modelo$components
predicciones <- modelo$predict(matriz,not_recommend = matriz_no_recomendar, k = 1)

valoraciones<-attr(predicciones,"scores")

productos_recomendados<- as.data.frame(attr(predicciones,"ids"))

colnames(productos_recomendados)<-"cod_est"
df_final<-inner_join(productos_recomendados,Productos,  by = "cod_est")
df_final<-cbind(df_final,valoraciones)
rownames(df_final)<-rownames(matriz)
unique(df_final$descripcion)

