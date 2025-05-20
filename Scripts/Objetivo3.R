library(recommenderlab)
library(ggplot2)
library(dplyr)
library(rsparse)
objetivos <- readRDS("Datos/objetivos.RDS")
sparsematrix<-readRDS("Datos/Transformados/matriz_binariaRsparse.RDS")
Productos<-readRDS("Datos/maestroestr.RDS")
#Segundo objetivo
objetivos<-objetivos[[3]]

matriz_usuarios<-sparsematrix[,objetivos$obj] #Matriz filtrada
dim(matriz_usuarios)
matriz <- as(sparsematrix, "dgCMatrix")
ma
modelo <- WRMF$new(rank = 10, lambda = 0.1,dynamic_lambda = TRUE, feedback = "implicit" )
set.seed(123)
user_emb <- modelo$fit_transform(matriz)
item_emb <- modelo$components
predicciones<-modelo$predict(matriz_usuarios, k = 1 )