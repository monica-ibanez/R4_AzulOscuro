library(dplyr)
library(naniar)
library(smoothmest)


df <- read.delim("Energy Census and Economic Data US 2010-2014.txt", header = TRUE)

# # Exploración inicial de los datos
print(dim(df))
head(df)
str(df)

# Conversión de columnas categóricas a valores lógicos
df$Coast <- as.logical(df$Coast)
df$Great.Lakes <- as.logical(df$Great.Lakes)

# Resumen estadístico y análisis de variables
summary(df %>% select_if(is.numeric))
table(df$Region)
table(df$Division)
table(df$Coast)
table(df$Great.Lakes)

# 1. Exploración de datos
df_sinUSA <- df[1:51,]
USA <- df[52,]

# Suma de la columna 'TotalC2010' para todos los estados
sum(df_sinUSA$TotalC2010)
USA[,7]

# Selección de columnas numéricas y cálculo de totales
estados_numerico <- df_sinUSA %>% select_if(is.numeric)
total_estados <- apply(estados_numerico, 2, sum, na.rm = TRUE)

#Extrae columnas numéricas de la fila 'USA'
USA_num <- USA %>% select_if(is.numeric)

# Combina los totales de los estados con los datos nacionales (USA)
combinar <- rbind(total_estados, USA_num)

# 2. Limpieza de datos
variable <- miss_var_summary(df)
variable[variable$n_miss < 5,]

miss_case_summary(df)

vis_miss(df, cluster = TRUE)

# Reemplazo de valores faltantes por ceros
df[is.na(df)] <- 0

# 3. Transformación de variables
df_pred_10 <- df %>%
  dplyr::select(TotalC2010, TotalP2010, TotalE2010, GDP2010)   
colnames(df_pred_10) <- c("TotalC", "TotalP", "TotalE", "GDP")  

df_pred_11 <- df %>%
  dplyr::select(TotalC2011, TotalP2011, TotalE2011, GDP2011)
colnames(df_pred_11) <- c("TotalC", "TotalP", "TotalE", "GDP")

df_pred_12 <- df %>%
  dplyr::select(TotalC2012, TotalP2012, TotalE2012, GDP2012)
colnames(df_pred_12) <- c("TotalC", "TotalP", "TotalE", "GDP")

df_pred_13 <- df %>%
  dplyr::select(TotalC2013, TotalP2013, TotalE2013, GDP2013)
colnames(df_pred_13) <- c("TotalC", "TotalP", "TotalE", "GDP")

df_pred_14 <- df %>%
  dplyr::select(TotalC2014, TotalP2014, TotalE2014, GDP2014)
colnames(df_pred_14) <- c("TotalC", "TotalP", "TotalE", "GDP")

# creamos un dataframe con la información de todos los dataframes por años
# para prepararlo una modelización
df_pred <- rbind(df_pred_10, df_pred_11, df_pred_12, df_pred_13, df_pred_14)

# 4. Privacidad Diferencial
set.seed(202)

# Selección de columnas
df_consumoE <- df %>% dplyr::select(c(1, 2, 3, 4, 7:11))

# Sumamos las columnas para obtener el consumo en una columna CET
df_consumoE$CET <- apply(df_consumoE[, c(5, 6, 7, 8, 9)], 1, sum)

# Elimina las columnas originales que contribuyen al CET
df_consumoE <- df_consumoE[, -c(5, 6, 7, 8, 9)]

# Calculamos el dato real de consumo medio por region
gasto_region <- df_consumoE %>%
  group_by(Region) %>%
  summarise(consumo_medio = mean(CET))

# Calculamos el dato real del consumo medio por division
gasto_division <- df_consumoE %>%
  group_by(Division) %>%
  summarise(consumo_medio = mean(CET))

# Implementación de ruido diferencial para los datos por región
eps <- 0.1

# Sensibilidad para las regiones
max_reg <- max(gasto_region$consumo_medio)
min_reg <- min(gasto_region$consumo_medio)
n_reg <- nrow(gasto_region)
sensibilidad <- (max_reg - min_reg) / n_reg

# Aplicación de ruido Laplaciano a los datos de consumo medio por región
consumo_medio_ruido <- rdoublex(1, gasto_region$consumo_medio, sensibilidad / eps)

# Combina los datos originales con el consumo medio con ruido
df_para_rusia1 <- cbind(gasto_region[, 1], consumo_medio_ruido)

# Sensibilidad para las divisiones
max_div <- max(gasto_division$consumo_medio)
min_div <- min(gasto_division$consumo_medio)
n_div <- nrow(gasto_division)
sensibilidad <- (max_div - min_div) / n_div

# Aplicación de ruido Laplaciano a los datos de consumo medio por división
consumo_medio_ruido <- rdoublex(1, gasto_division$consumo_medio, sensibilidad / eps)

# Combina los datos originales con el consumo medio con ruido
df_para_rusia2 <- cbind(gasto_division[, 1], consumo_medio_ruido)
