library(recommenderlab)
library(stringr)
library(dplyr)
library(lubridate)
library(tidyverse)

set.seed(123)

Matriz <- readRDS("Datos/Transformados/matriz_rrm.RDS")

productos <- colnames(Matriz)
muestra_pro <- sample(productos, 500)

clientes <- rownames(Matriz)
muestra_cli <- sample(clientes, 2000)
Matriz <- Matriz[muestra_cli,muestra_pro]

head(colnames(Matriz))

esquema <- evaluationScheme(Matriz, method = "split", train = 0.8, given = 3, goodRating = 1)

algoritmos <- list(
  RANDOM = list(name = "RANDOM"),
  POPULAR = list(name = "POPULAR"),
  IBCF = list(name = "IBCF", param = list(k = 30)),
  UBCF = list(name = "UBCF", param = list(method = "Cosine", nn = 30)),
  ALS = list(name = "ALS", param = list(n_factors = 10, n_iterations = 15)),
  SVDF = list(name = "SVDF", param = list(k = 20))
)


resultados_rmse <- evaluate(esquema, algoritmos, type = "ratings")

resultados_topn <- evaluate(esquema, algoritmos, type = "topNList", n = c(1,3,5,10))

resultadosrmse <- avg(resultados_rmse)

avg_results <- avg(resultados_topn)
print(avg_results[[2]])
class(avg_results)

nombres_modelos <- names(resultados_topn)


resultados_completos <- do.call(rbind, lapply(seq_along(avg_results), function(i) {
  df <- as.data.frame(avg_results[[i]])
  df$modelo <- names(avg_results)[i]
  df$index <- seq_len(nrow(df))
  df
}))


resultados_completos <- resultados_completos[, c("modelo", "n", setdiff(names(resultados_completos), c("modelo", "n", "index")), "index")]

print(resultados_completos)

colnames(resultados_completos)

resultados_completos$F1 <- with(resultados_completos, {
  # Manejo de división por cero
  f1 <- ifelse(precision + recall == 0, NA,
               2 * (precision * recall) / (precision + recall))
  return(f1)
})

df_long <- resultados_completos %>%
  select(modelo, n, precision, recall, F1) %>%
  mutate(n = factor(n, levels = sort(unique(n)))) %>%
  pivot_longer(cols = c(precision, recall, F1), names_to = "metrica", values_to = "valor")


# Gráfico
ggplot(df_long, aes(x = n, y = valor, color = modelo, group = modelo)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  facet_wrap(~ metrica, scales = "free_y") +
  labs(title = "Comparación de métricas",
       x = "Número de recomendaciones",
       y = "Valor",
       color = "Algoritmo") +
  theme_minimal() +
  theme(legend.position = "bottom")





