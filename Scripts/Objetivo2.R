library(recommenderlab)
library(ggplot2)
library(dplyr)
library(rsparse)
objetivos <- readRDS("Datos/objetivos.RDS")
sparsematrix<-readRDS("Datos/Transformados/matriz_recomendacion.RDS")

#Segundo objetivo
objetivos<-objetivos[[2]]

matriz_usuarios<-sparsematrix[objetivos$obj]
dim(matriz_usuarios)
row
matriz<-as(sparsematrix,"Matrix")
# Paso 2: Entrenar modelo ALS
modelo <- WRMF$new(rank = 20, lambda = 0.1, max_iter = 10, nthread = parallel::detectCores() - 1)
factores_usuario <- modelo$fit_transform(matriz)
factores_item <- modelo$components
