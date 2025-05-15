library(lubridate)
library(data.table)
library(Matrix)
library(rsparse)

tickets <- readRDS("Datos/tickets_enc.RDS")
objetivos <- readRDS("Datos/objetivos.RDS")
maestroestr <- readRDS("Datos/maestroestr.RDS")
setDT(tickets); setDT(maestroestr)

# Paso 1: Preparar matriz dispersa para ALS
producto_objetivo <- objetivos$objetivo1$obj
descripcion_producto <- maestroestr[cod_est == producto_objetivo, descripcion]
cat("Recomendando:", producto_objetivo, "-", descripcion_producto, "\n")

# Contar compras cliente-producto
conteo <- tickets[, .N, by = .(id_cliente_enc, cod_est)]

# Asignar índices numéricos
clientes <- data.table(id_cliente_enc = unique(conteo$id_cliente_enc))
clientes[, cliente_idx := .I]
productos <- data.table(cod_est = unique(conteo$cod_est))
productos[, producto_idx := .I]

# Combinar con los índices
conteo <- merge(conteo, clientes, by = "id_cliente_enc")
conteo <- merge(conteo, productos, by = "cod_est")

# Crear matriz dispersa
matriz <- sparseMatrix(
  i = conteo$cliente_idx,
  j = conteo$producto_idx,
  x = conteo$N,
  dims = c(nrow(clientes), nrow(productos))
)

# Paso 2: Entrenar modelo ALS
modelo <- WRMF$new(rank = 20, lambda = 0.1, max_iter = 10, nthread = parallel::detectCores() - 1)
factores_usuario <- modelo$fit_transform(matriz)
factores_item <- modelo$components


# Paso 3: Calcular afinidad al producto objetivo
idx_producto <- productos[cod_est == producto_objetivo, producto_idx]
if (length(idx_producto) == 0) stop("Producto no encontrado en datos.")

# Multiplicar factores → puntuaciones predichas
puntuaciones <- as.matrix(factores_usuario %*% factores_item)
afinidades <- puntuaciones[, idx_producto]



# Paso 4: Crear tabla de resultados
res <- data.table(cliente_idx = 1:length(afinidades), afinidad = afinidades)
res <- merge(res, clientes, by = "cliente_idx")

# Top 10 clientes con más afinidad al producto
top10 <- res[order(-afinidad)][1:10]



# Paso 5: Añadir contexto: ¿ya compraron el producto?
compras_previas <- tickets[
  id_cliente_enc %in% top10$id_cliente_enc & cod_est == producto_objetivo,
  .N, by = id_cliente_enc
]
setnames(compras_previas, "N", "Compras_Previas")

actividad <- tickets[
  id_cliente_enc %in% top10$id_cliente_enc,
  .N, by = id_cliente_enc
]
setnames(actividad, "N", "Total_Compras")

top10 <- merge(top10, compras_previas, by = "id_cliente_enc", all.x = TRUE)
top10 <- merge(top10, actividad, by = "id_cliente_enc")
top10[is.na(Compras_Previas), Compras_Previas := 0]
top10[, Ranking := rank(-afinidad, ties.method = "first")]

# Organizar columnas
resultado_final <- top10[, .(Ranking, id_cliente_enc, Afinidad_Predicha = afinidad, 
                             Compras_Previas, Total_Compras)]
setorder(resultado_final, Ranking)
print(resultado_final)


# Resumen final
cat("\n✅ Recomendación completada con ALS para producto:", producto_objetivo, "-", descripcion_producto, "\n")
cat("🛒 Ya habían comprado el producto:", sum(resultado_final$Compras_Previas > 0), "clientes\n")
cat("📈 Afinidad media:", round(mean(resultado_final$Afinidad_Predicha), 3), "\n")


