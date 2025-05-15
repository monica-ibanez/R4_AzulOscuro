library(lubridate)
library(dplyr)
library(tidyr)
library(data.table)
library(Matrix)
library(rsparse)
set.seed(777)

tickets <- readRDS("Datos/tickets_enc.RDS")
objetivos <- readRDS("Datos/objetivos.RDS")
maestroestr <- readRDS("Datos/maestroestr.RDS")  # cod_est y descripcion

setDT(tickets)
setDT(maestroestr)

# Preparar datos para ALS
# Crear matriz dispersa cliente-producto (conteo de compras)
conteos <- tickets[, .N, by = .(id_cliente_enc, cod_est)]

clientes <- data.table(id_cliente_enc = unique(conteos$id_cliente_enc))
clientes[, cliente_idx := .I]

productos <- data.table(cod_est = unique(conteos$cod_est))
productos[, producto_idx := .I]

conteos <- merge(conteos, clientes, by = "id_cliente_enc")
conteos <- merge(conteos, productos, by = "cod_est")

matriz_dispersa <- sparseMatrix(
  i = conteos$cliente_idx,
  j = conteos$producto_idx,
  x = conteos$N,
  dims = c(nrow(clientes), nrow(productos))
)

# Entrenar modelo ALS (fit_transform)
modelo <- WRMF$new(rank = 20, lambda = 0.1, max_iter = 10, nthread = parallel::detectCores() - 1)
factores_usuario <- modelo$fit_transform(matriz_dispersa)
factores_item <- modelo$components

# Obtener última compra por cliente del objetivo 4
clientes_obj4 <- objetivos$objetivo4$obj

ultimas_compras <- tickets %>%
  filter(id_cliente_enc %in% clientes_obj4) %>%
  mutate(dia = ymd(dia)) %>%
  arrange(id_cliente_enc, desc(dia)) %>%
  group_by(id_cliente_enc) %>%
  slice(1) %>%
  ungroup()


# Índices de esos clientes en el modelo ALS
clientes_obj4_idx <- merge(data.table(id_cliente_enc = clientes_obj4), clientes, by = "id_cliente_enc", all.x = TRUE)
clientes_obj4_idx <- clientes_obj4_idx[!is.na(cliente_idx)]


# Predecir artículo que pudieron haber olvidado
# Obtener predicciones ALS para todos los productos
puntuaciones <- as.matrix(factores_usuario %*% factores_item)

# Para los clientes del objetivo 4, quitar los productos que ya compró en su última compra
compras_ultimas <- tickets %>%
  filter(id_cliente_enc %in% clientes_obj4_idx$id_cliente_enc) %>%
  mutate(dia = ymd(dia)) %>%
  group_by(id_cliente_enc, cod_est) %>%
  slice_max(order_by = dia, n = 1, with_ties = FALSE) %>%
  ungroup()


# Mapeo a índices (para usar las matrices del modelo)
compras_ultimas <- compras_ultimas %>%
  left_join(clientes, by = "id_cliente_enc") %>%
  left_join(productos, by = "cod_est")
# Construimos un vector de predicciones filtradas (producto que no está en última compra)
recomendaciones <- lapply(clientes_obj4_idx$cliente_idx, function(ci) {
  punt_ci <- puntuaciones[ci, ]
  productos_comprados <- compras_ultimas[cliente_idx == ci, producto_idx]
  punt_ci[productos_comprados] <- -Inf  # eliminar productos ya comprados
  mejor_idx <- which.max(punt_ci)
  data.table(
    cliente_idx = ci,
    producto_idx = mejor_idx,
    afinidad = punt_ci[mejor_idx]
  )
})



recomendaciones <- rbindlist(recomendaciones)
recomendaciones <- merge(recomendaciones, clientes, by = "cliente_idx")
recomendaciones <- merge(recomendaciones, productos, by = "producto_idx")
recomendaciones <- merge(recomendaciones, maestroestr, by = "cod_est", all.x = TRUE)

resultado <- recomendaciones[, .(id_cliente_enc, cod_est, descripcion)]
print(resultado)
