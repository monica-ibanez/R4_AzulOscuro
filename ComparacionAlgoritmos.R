library(recommenderlab)
library(stringr)
library(dplyr)

set.seed(123)

Matriz <- readRDS("Datos/Transformados/matriz_rrm.RDS")


muestra_pro <- sample(Otros_productos, 500)

muestra_cli <- sample(Otros_clientes, 2000)
Matriz <- Matriz[muestra_cli,muestra_pro]

head(colnames(Matriz))

esquema <- evaluationScheme(Matriz, method = "split", train = 0.7, given = 3, goodRating = 1)

algoritmos <- list(
  RANDOM = list(name = "RANDOM"),
  POPULAR = list(name = "POPULAR"),
  IBCF = list(name = "IBCF", param = list(k = 30)),
  UBCF = list(name = "UBCF", param = list(method = "Cosine", nn = 30)),
  ALS = list(name = "ALS", param = list(n_factors = 10, n_iterations = 15)),
  SVDF = list(name = "SVDF", param = list(k = 20))
)


resultados_rmse <- evaluate(esquema, algoritmos, type = "ratings")

resultados_topn <- evaluate(esquema, algoritmos, type = "topNList", n = c(1, 3, 5, 10))


metricas_df <- data.frame(
  Modelo = character(),
  Precision = numeric(),
  Recall = numeric(),
  TPR = numeric(),
  FPR = numeric(),
  Accuracy = numeric(),
  F1 = numeric(),
  Coverage = numeric(),
  stringsAsFactors = FALSE
)



for (nombre_modelo in names(resultados_topn)) {
  m <- avg(resultados_topn[[nombre_modelo]])
  
  precision <- m[, "precision"]
  recall <- m[, "recall"]
  tpr <- recall
  fpr <- m[, "FPR"]
  
  # F1 Score
  f1 <- if ((precision + recall) > 0) {
    2 * (precision * recall) / (precision + recall)
  } else {
    0
  }
  
  # Accuracy estimada
  accuracy <- precision * recall / (precision + recall - precision * recall)
  
  ## NUEVO: Calcular Coverage manualmente
  # Entrenar modelo con training set
  modelo_entrenado <- Recommender(getData(esquema, "train"), method = algoritmos[[nombre_modelo]]$name,
                                  parameter = algoritmos[[nombre_modelo]]$param)
  
  # Predecir top-5 para usuarios del conjunto test (parte conocida)
  predicciones <- predict(modelo_entrenado, getData(esquema, "known"), type = "topNList", n = 5)
  
  # Extraer todos los ítems recomendados
  lista_recomendados <- as(predicciones, "list")
  items_recomendados <- unique(unlist(lista_recomendados))
  coverage <- length(items_recomendados) / ncol(matriz)
  
  # Guardar métricas
  metricas_df <- rbind(metricas_df, data.frame(
    Modelo = nombre_modelo,
    Precision = round(precision, 3),
    Recall = round(recall, 3),
    TPR = round(tpr, 3),
    FPR = round(fpr, 3),
    Accuracy = round(accuracy, 3),
    F1 = round(f1, 3),
    Coverage = round(coverage, 3)
  ))
}



saveRDS(resultados, "Datos/Transformados/resultados_comparacion")

resultados <- readRDS("Datos/Transformados/resultados_comparacion")

sapply(resultados, avg)
plot(resultados, annotate = TRUE, legend = "topleft")

#RMSE
#precision respecto a recall


