######################### OBJETIVO 0.2: CLUSTERIZACIÓN ######################### 
##### LIBRERÍAS ################################################################
#GGPLOT2
if (!require(ggplot2)){
  install.packages("ggplot2")
  library(ggplot2)
} else {
  library(ggplot2)
}
#DPLYR
if (!require(dplyr)){
  install.packages("dplyr")
  library(dplyr)
} else {
  library(dplyr)
}
#VIM
if (!require(VIM)){
  install.packages("VIM")
  library(VIM)
} else {
  library(VIM)
}
#NORTEST
if (!require(nortest)){
  install.packages("nortest")
  library(nortest)
} else {
  library(nortest)
}
#ENVSTATS
if (!require(EnvStats)){
  install.packages("EnvStats")
  library(EnvStats)
} else {
  library(EnvStats)
}
#PLOTLY
if (!require(plotly)){
  install.packages("plotly")
  library(plotly)
} else {
  library(plotly)
}
#CLUSTER
if (!require(cluster)){
  install.packages("cluster")
  library(cluster)
} else {
  library(cluster)
}
#STRINGR
if (!require(stringr)){
  install.packages("stringr")
  library(stringr)
} else {
  library(stringr)
}

##### FUNCIONES ################################################################
select_var_num = function(df){
  df_num = df %>% select(where(is.numeric))
  return(df_num)
}
outliers = function(columna, alpha = 0.05, k = 7, num.hampel = 3, num.percent = 5){
  #LIBRERÍA NORTEST
  if (!require(nortest)){
    install.packages("nortest")
    library(nortest)
  } else {
    library(nortest)
  }
  #LIBRERÍA ENVSTATS
  if (!require(EnvStats)){
    install.packages("EnvStats")
    library(EnvStats)
  } else {
    library(EnvStats)
  }
  #OUTLIERS
  columna = as.numeric(columna)
  if(length(columna) <= 50){
    normal = shapiro.test(columna)
  } else {
    normal = nortest::lillie.test(columna)
  }
  p_value = normal$p.value
  if(p_value >= alpha){
    outlier = EnvStats::rosnerTest(columna, k = k)
  } else {
    media.h = mean(columna, na.rm = TRUE)
    sdev.h = sd(columna, na.rm = TRUE)
    min.h = media.h - num.hampel * sdev.h
    max.h = media.h + num.hampel * sdev.h
    outlier.hampel = ifelse(min.h < columna & columna < max.h, FALSE, TRUE)
    min.p = quantile(columna, num.percent/100, na.rm = TRUE)
    max.p = quantile(columna, 1-(num.percent/100), na.rm = TRUE)
    outlier.percent = ifelse(min.p < columna & columna < max.p, FALSE, TRUE)
    media.b = mean(columna, na.rm = TRUE)
    IQR.b = IQR(columna, na.rm = TRUE)
    Q1.b = quantile(columna, 0.25, na.rm = TRUE)
    Q3.b = quantile(columna, 0.75, na.rm = TRUE)
    min.b = Q1.b - 1.5 * IQR.b
    max.b = Q1.b + 1.5 * IQR.b
    outlier.boxplot = ifelse(min.b < columna & columna < max.b, FALSE, TRUE)
    outlier = data.frame(Hampel = outlier.hampel, 
                         Percent = outlier.percent, 
                         Boxplot = outlier.boxplot)
  }
  return(outlier)
}
imputador = function(df){
  outliers = function(columna, alpha = 0.05, k = 7, num.hampel = 3, num.percent = 5){
    #LIBRERÍA NORTEST
    if (!require(nortest)){
      install.packages("nortest")
      library(nortest)
    } else {
      library(nortest)
    }
    #LIBRERÍA ENVSTATS
    if (!require(EnvStats)){
      install.packages("EnvStats")
      library(EnvStats)
    } else {
      library(EnvStats)
    }
    #OUTLIERS
    columna = as.numeric(columna)
    if(length(columna) <= 50){
      normal = shapiro.test(columna)
    } else {
      normal = nortest::lillie.test(columna)
    }
    p_value = normal$p.value
    if(p_value >= alpha){
      outlier = EnvStats::rosnerTest(columna, k = k)
    } else {
      media.h = mean(columna, na.rm = TRUE)
      sdev.h = sd(columna, na.rm = TRUE)
      min.h = media.h - num.hampel * sdev.h
      max.h = media.h + num.hampel * sdev.h
      outlier.hampel = ifelse(min.h < columna & columna < max.h, FALSE, TRUE)
      min.p = quantile(columna, num.percent/100, na.rm = TRUE)
      max.p = quantile(columna, 1-(num.percent/100), na.rm = TRUE)
      outlier.percent = ifelse(min.p < columna & columna < max.p, FALSE, TRUE)
      media.b = mean(columna, na.rm = TRUE)
      IQR.b = IQR(columna, na.rm = TRUE)
      Q1.b = quantile(columna, 0.25, na.rm = TRUE)
      Q3.b = quantile(columna, 0.75, na.rm = TRUE)
      min.b = Q1.b - 1.5 * IQR.b
      max.b = Q1.b + 1.5 * IQR.b
      outlier.boxplot = ifelse(min.b < columna & columna < max.b, FALSE, TRUE)
      outlier = data.frame(Hampel = outlier.hampel, 
                           Percent = outlier.percent, 
                           Boxplot = outlier.boxplot)
    }
    return(outlier)
  }
  correlador = function(matriz){
    cor_matriz = data.frame()
    for (i in 1:dim(matriz)[2]) {
      for (j in 1:dim(matriz)[1]) {
        cor_matriz[j,i] = ifelse(matriz[j,i] == 1, 0, abs(matriz[j,i]))
      }
    }
    cor_vector = c()
    for (i in 1:dim(matriz)[2]) {
      for (j in 1:dim(matriz)[1]) {
        cor_vector = c(cor_vector, cor_matriz[j,i])
      }
    }
    media = mean(cor_vector)
    return(media)
  }
  df_names = colnames(df)
  df_num = df %>% select(where(is.numeric))
  df_cha = df %>% select(where(is.character))
  df_fac = df %>% select(where(is.factor))
  df_str = cbind(df_cha, df_fac)
  outliers.list = list()
  for (i in 1:dim(df_num)[2]) {
    outliers.list[[i]] = outliers(columna = (as.data.frame(df_num[,i]))[,1])
  }
  df_num_hampel = df_num
  df_num_percent = df_num
  df_num_boxplot = df_num
  for (i in 1:dim(df_num)[2]) {
    df_num_hampel[,i] = ifelse(outliers.list[[i]][,1], NA, df_num_hampel[,i])
    df_num_percent[,i] = ifelse(outliers.list[[i]][,2], NA, df_num_percent[,i])
    df_num_boxplot[,i] = ifelse(outliers.list[[i]][,3], NA, df_num_boxplot[,i])
  }
  df_num_hampel_k_3 = kNN(data = df_num_hampel, variable = colnames(df_num_hampel), k = 3, imp_var = FALSE)
  cor_media_hampel_k_3 = correlador(cor(df_num_hampel_k_3))
  df_num_hampel_k_5 = kNN(data = df_num_hampel, variable = colnames(df_num_hampel), k = 5, imp_var = FALSE)
  cor_media_hampel_k_5 = correlador(cor(df_num_hampel_k_5))
  df_num_percent_k_3 = kNN(data = df_num_percent, variable = colnames(df_num_percent), k = 3, imp_var = FALSE)
  cor_media_percent_k_3 = correlador(cor(df_num_percent_k_3))
  df_num_percent_k_5 = kNN(data = df_num_percent, variable = colnames(df_num_percent), k = 5, imp_var = FALSE)
  cor_media_percent_k_5 = correlador(cor(df_num_percent_k_5))
  df_num_boxplot_k_3 = kNN(data = df_num_boxplot, variable = colnames(df_num_boxplot), k = 3, imp_var = FALSE)
  cor_media_boxplot_k_3 = correlador(cor(df_num_boxplot_k_3))
  df_num_boxplot_k_5 = kNN(data = df_num_boxplot, variable = colnames(df_num_boxplot), k = 5, imp_var = FALSE)
  cor_media_boxplot_k_5 = correlador(cor(df_num_boxplot_k_5))
  correlaciones = c(cor_media_hampel_k_3, cor_media_hampel_k_5, 
                    cor_media_percent_k_3, cor_media_percent_k_5, 
                    cor_media_boxplot_k_3, cor_media_boxplot_k_5)
  num_knn_mejor = which(max(correlaciones) == correlaciones)
  if(num_knn_mejor == 1){
    df_num = df_num_hampel_k_3
  } else {
    if(num_knn_mejor == 2){
      df_num = df_num_hampel_k_5
    } else {
      if(num_knn_mejor == 3){
        df_num = df_num_percent_k_3
      } else {
        if(num_knn_mejor == 4){
          df_num = df_num_percent_k_5
        } else {
          if(num_knn_mejor == 5){
            df_num = df_num_boxplot_k_3
          } else {
            df_num = df_num_boxplot_k_5
          }
        }
      }
    }
  }
  df_imp = cbind(df_num, df_str)
  df_imp = df_imp %>% select(all_of(df_names))
  return(df_imp)
}
PCA = function(df, varianza.min = 0.95){
  select_var_num = function(df){
    df_num = df %>% select(where(is.numeric))
    return(df_num)
  }
  df_num = select_var_num(df)
  pca = prcomp(df_num)
  pca_summary = summary(pca)
  varianza = pca_summary$importance[2,]
  varianza_acum = pca_summary$importance[3,]
  pca_df = data.frame(t(rbind(Componente = 1:length(varianza), Varianza = varianza, Varianza_Acumulada = varianza_acum)))
  num_pca = max(which(varianza_acum < varianza.min))
  datos_pca = pca$x[, 1:num_pca]
  output = list(Varianza_PCA = pca_df, Datos_PCA = datos_pca)
  return(output)
}
clustering_jerarquico = function(df, clust_min = 3, clust_max = 4){
  if (!require(cluster)){
    install.packages("cluster")
    library(cluster)
  } else {
    library(cluster)
  }
  if (!require(stringr)){
    install.packages("stringr")
    library(stringr)
  } else {
    library(stringr)
  }
  Resultado_jerarquico = data.frame()
  metodo = c("complete", "average", "centroid")
  distan = c("euclidean", "manhattan", "canberra", "minkowski")
  i = 1
  for (d in 1:length(distan)) {
    for (m in 1:length(metodo)) {
      for (k in clust_min:clust_max) {
        distancias = dist(df, method = distan[d])
        hc = hclust(distancias, method = metodo[m])
        clusters = cutree(hc, k = k)
        ind_sil = mean(silhouette(clusters, distancias)[, 3])
        clusters_table = data.frame(table(Cluster = clusters))
        ind_clust = clusters_table$Freq[1] - sum(clusters_table$Freq[-1])
        d_ = distan[d]
        m_ = metodo[m]
        name_instancia = str_c(d_, m_, k, sep = "_")
        instancia = c(Silueta = ind_sil, Reparto_cluster = ind_clust)
        Resultado_jerarquico = rbind(Resultado_jerarquico, instancia)
        rownames(Resultado_jerarquico)[i] = name_instancia
        i = i + 1
      }
    }
  }
  colnames(Resultado_jerarquico) = names(instancia)
  return(Resultado_jerarquico)
}

##### IMPORTACIÓN DE DATOS #####################################################
datos_habitos = read.csv("Datos/Transformados/datos_para_clustering.csv")

##### CORRECCIÓN DE FORMATOS ###################################################
head(datos_habitos)
tail(datos_habitos)
dim(datos_habitos)
summary(datos_habitos)
colnames(datos_habitos)
datos_habitos = datos_habitos %>% select(-X)

##### TRATAMIENTO DE OUTLIERS ##################################################
df_imp = imputador(datos_habitos)

##### PCA ######################################################################
var.min = 0.999
pca = PCA(df_imp, varianza.min = var.min)
datos_pca = pca$Datos_PCA

##### GRÁFICO DE VARIANZA ######################################################
varianza_PCA_grafico = ggplot(pca$Varianza_PCA, aes(x = Componente, y = Varianza_Acumulada)) +
  geom_line(color = "red", size = 1) +
  geom_point(color = "red", size = 3) +
  theme_classic() +
  geom_hline(yintercept = var.min, linetype = 5, color = "black") +
  scale_y_continuous(breaks = seq(0, 1, 0.1), limits = c(0, 1)) +
  scale_x_continuous(breaks = seq(0, dim(pca$Varianza_PCA)[1], 1)) +
  labs(title = "Varianza acumulada por componente principal",
        x = "Componente Principal",
        y = "Varianza",
        caption = "Fuente: Adaptado de datos proporcionados por Eroski")
ggplotly(varianza_PCA_grafico)

##### CLUSTERING JERÁRQUICO ####################################################
# clustering_jerarquico = function(df, clust_min = 3, clust_max = 4){
#   if (!require(cluster)){
#     install.packages("cluster")
#     library(cluster)
#   } else {
#     library(cluster)
#   }
#   if (!require(stringr)){
#     install.packages("stringr")
#     library(stringr)
#   } else {
#     library(stringr)
#   }
#   Resultado_jerarquico = data.frame()
#   metodo = c("complete", "average", "centroid")
#   distan = c("euclidean", "manhattan", "canberra", "minkowski")
#   i = 1
#   for (d in 1:length(distan)) {
#     for (m in 1:length(metodo)) {
#       for (k in clust_min:clust_max) {
#         distancias = dist(df, method = distan[d])
#         hc = hclust(distancias, method = metodo[m])
#         clusters = cutree(hc, k = k)
#         ind_sil = mean(silhouette(clusters, distancias)[, 3])
#         clusters_table = data.frame(table(Cluster = clusters))
#         ind_clust = clusters_table$Freq[1] - sum(clusters_table$Freq[-1])
#         d_ = distan[d]
#         m_ = metodo[m]
#         name_instancia = str_c(d_, m_, k, sep = "_")
#         instancia = c(Silueta = ind_sil, Reparto_cluster = ind_clust)
#         Resultado_jerarquico = rbind(Resultado_jerarquico, instancia)
#         rownames(Resultado_jerarquico)[i] = name_instancia
#         i = i + 1
#       }
#     }
#   }
#   colnames(Resultado_jerarquico) = names(instancia)
#   return(Resultado_jerarquico)
# }
# 
# ind_hc = clustering_jerarquico(datos_pca)
# TARDA MUCHO, PASAMOS A KMEANS

##### KMEANS ###################################################################
km = kmeans(datos_pca, centers = 3)
table(km$cluster)
df_cluster = cbind(df_imp, Cluster = km$cluster)
df_cluster$Cluster = as.factor(df_cluster$Cluster)
df_clust = as.data.frame(cbind(datos_pca, Clust = km$cluster))
df_clust$Clust = as.factor(df_clust$Clust)
id = df_imp$id_cliente_enc 
df_cluster = df_cluster[,-1]
centroides = rbind(
df_cluster %>% filter(Cluster == 1) %>% select(-Cluster) %>% colMeans(),
df_cluster %>% filter(Cluster == 2) %>% select(-Cluster) %>% colMeans(),
df_cluster %>% filter(Cluster == 3) %>% select(-Cluster) %>% colMeans(),
df_cluster %>% select(-Cluster) %>% colMeans()
)
rownames(centroides) = c("Cluster 1", "Cluster 2", "Cluster 3", "Fichero completo")
df_clustering = cbind(id, df_cluster)
##### GRÁFICO DE CENTROIDES ####################################################
plot_ly(df_clust, x = ~PC1, y = ~PC2, z = ~PC3, type = "scatter3d", mode = "markers", color = ~Clust)
plot_ly(as.data.frame(km$centers), x = ~PC1, y = ~PC2, z = ~PC3, type = "scatter3d", mode = "markers")
##### CARPETA RESULTADOS #######################################################
write.csv(df_clustering, file = "Resultados/clientes_con_cluster.csv")
write.csv(centroides, file = "Resultados/centroides.csv", row.names = TRUE)

##### GRAFICAR CLUSTERS ########################################################
df_cluster = df_clustering
df_names = colnames(df_cluster)
df_num = select_var_num(df_cluster)
df_cha = df_cluster %>% select(-all_of(colnames(df_num)))

paleta = c("#E6001F", "#CC001C", "#FF3347", "#B30019", "#990015" ,"#005FAA", "#004C88")
paleta3 = c("#FF3347", "#FFFFFF", "#005FAA")

ggplot(df_cluster, aes(x = Cluster, y = total_productos, fill = Cluster)) +
  geom_boxplot() +
  labs(title = "Total de productos comprados por Cluster",
       x = "Cluster", 
       y = "Total de productos comprados") +
  scale_fill_manual("Cluster", values = paleta3) +
  scale_y_continuous(breaks = seq(0,325, 25))

ggplot(df_cluster, aes(x = Cluster, y = productos_distintos, fill = Cluster)) +
  geom_violin() +
  labs(title = "Cantidad de productos distintos comprados por Cluster",
       x = "Cluster", 
       y = "Cantidad de productos distintos comprados") +
  scale_fill_manual("Cluster", values = paleta3) +
  scale_y_continuous(breaks = seq(0, 175, 25))

damedia = df_cluster %>% group_by(Cluster) %>% summarise(media = mean(dias_activos))

ggplot(damedia, aes(x = Cluster, y = media, fill = Cluster)) +
  geom_col(color = "#000000") +
  labs(title = "Media de días activos por Cluster",
                  x = "Cluster", 
                  y = "Media de días activos") +
  scale_fill_manual("Cluster", values = paleta3) +
  scale_y_continuous(breaks = seq(0,3,1))

ggplot(df_cluster, aes(x = Cluster, y = media_compras_por_semana, fill = Cluster)) +
  geom_boxplot() +
  labs(title = "Media de productos diferentes comprados por semana por Cluster",
       x = "Cluster", 
       y = "Media de productos diferentes comprados por semana") +
  scale_fill_manual("Cluster", values = paleta3) 

ggplot(df_cluster, aes(x = Cluster, y = compras_entre_semana, fill = Cluster)) +
  geom_boxplot() +
  labs(title = "Productos diferentes comprados de lunes a viernes por Cluster",
       x = "Cluster", 
       y = "Productos diferentes comprados de lunes a viernes") +
  scale_fill_manual("Cluster", values = paleta3) +
  scale_y_continuous(breaks = seq(0, 275, 25))

ggplot(df_cluster, aes(x = Cluster, y = compras_fin_de_semana, fill = Cluster)) +
  geom_boxplot() +
  labs(title = "Productos diferentes comprados de sábado a domingo por Cluster",
       x = "Cluster", 
       y = "Productos diferentes comprados de sábado a domingo") +
  scale_fill_manual("Cluster", values = paleta3) +
  scale_y_continuous(breaks = seq(0, 75, 25))



