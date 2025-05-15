library(recommenderlab)
set.seed(123)

matriz_rrm <- readRDS("Datos/Transformados/matriz_rrm.RDS")
esquema <- evaluationScheme(matriz_rrm, method = "split", train = 0.7, given = 3, goodRating = 1)
algoritmos <- list(
  RANDOM = list(name = "RANDOM"),
  POPULAR = list(name = "POPULAR"),
  IBCF = list(name = "IBCF", param = list(k = 30)),
  UBCF = list(name = "UBCF", param = list(method = "Cosine", nn = 30)),
  ALS = list(name = "ALS", param = list(n_factors = 20, regularization = 0.1, n_iterations = 15)),
  SVDF = list(name = "SVDF", param = list(k = 50, maxIter = 20, normalize = "center"))
)

metricas <- list("RMSE", "topNList")

resultados_rmse <- evaluate(esquema, algoritmos, type = "ratings")

resultados_topn <- evaluate(esquema, algoritmos, type = "topNList", n = c(1, 3, 5, 10))


resultados <- evaluate(esquema, algoritmos, type = "topNList", n = c(1, 3, 5, 10))


saveRDS(resultados, "Datos/Transformados/resultados_comparacion")

resultados <- readRDS("Datos/Transformados/resultados_comparacion")

sapply(resultados, avg)
plot(resultados, annotate = TRUE, legend = "topleft")

#RMSE
#precision respecto a recall


