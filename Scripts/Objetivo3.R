library(recommenderlab)
library(ggplot2)
library(dplyr)
library(rsparse)
objetivos <- readRDS("Datos/objetivos.RDS")
sparsematrix<-readRDS("Datos/Transformados/matriz_binariaRsparse.RDS")
Productos<-readRDS("Datos/maestroestr.RDS")
#Segundo objetivo
objetivos<-objetivos[[3]]

matriz_items<-sparsematrix[,objetivos$obj] #Matriz filtrada

matriz <- as(sparsematrix, "dgCMatrix")

modelo <- WRMF$new(rank = 10, lambda = 0.1,dynamic_lambda = TRUE, feedback = "implicit" )
set.seed(1)

user_emb <- modelo$fit_transform(matriz)
item_emb <- modelo$components
predicciones <- modelo$predict(matriz_items,not_recommend = sparsematrix, k = 1)

valoraciones<-attr(predicciones,"scores")

productos_recomendados<- as.data.frame(attr(predicciones,"ids"))

colnames(productos_recomendados)<-"cod_est"
df_final<-inner_join(productos_recomendados,Productos,  by = "cod_est")

unique(df_final$descripcion)
