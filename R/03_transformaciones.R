
# Preparar la data que se usara en el modelo VAR
data <- reduce(lista_dfs_mes, inner_join, by = "fecha")
rm(lista_dfs_mes)
rm(lista_dfs)


#Variables Endógenas
zt <- ts(data[, c("imae", "ipc", "i", "bc", "remesas", "tc")],  start = c(2010, 1),
         frequency = 12)
# Variavles Exógenas
xt <- ts(data[, c("indpro", "pce", "fedfunds")], start = c(2010, 1), frequency = 12)


# ------------------------------------------------------------------------------
# 1. Preparación de Datos Multivariados
# ------------------------------------------------------------------------------

# ==============================================================================
#                 Augmented Dickey-Fuller Test
# REVISAR ESTACIONARIDAD (UNIT ROOTS) DE CADA SERIE SIN TRANSFORMAR
# ==============================================================================
# Descripción: antes de pasar todas las series a series de tiempo, vamos a verificar
# que estas sean estacionarias, primero vamos a evaluar gráficamente  y luego
# ejecutaremos un adf-test a cada una de ellas recordando que:
# H0: la serie tiene raíz unitaria, o sea, no es estacionaria
# H1: la serie sí es estacionaria
# Entonces en ADF, si p-value < 0.05, rechazas H0 y concluyes que hay evidencia de estacionariedad.

plot.ts(zt, main = "Dinámica Multivariada Endogenas")
summary(ur.df(diff(zt[, "imae"]), type = "trend", lags = 12))

adf.test(zt[,"imae"], k = 0) # 0.01
adf.test(zt[,"ipc"], k = 0) # 0.01
adf.test(zt[,"i"], k = 0) # 0.9647
adf.test(zt[,"bc"], k = 0) # 0.01
adf.test(zt[,"tc"], k = 0) # 0.024

adf.test(diff(zt[,"ipc"]))
adf.test(diff(zt[,"i"]))#fue necesario diferenciar 0.0361
# adf.test(diff(zt[,"tcr"])) # usar diferencia de log para interpretar como cambio porcentuallog(diff())


# si son mensuales por lo menos 12 rezagos
# trimestrales 4 rezagos

# Buscar como investigar restricciones de exclusión para forzar ceros

zt_diff <- diff(zt[, c("tc", "i", "ipc")])
plot(zt_diff)

zt_var <- cbind(
  imae      = zt[-1, "imae"],
  ipc    = zt[-1, "ipc"],
  d_i     = diff(zt[, "i"]),
  bc = zt[-1, "bc"],
  tc     = zt[-1, "tc"]
)

plot(zt_var)
plot.ts(xt, main = "Dinámica Multivariada Exogenas")
plot.ts(diff(xt), main = "Dinámica Multivariada")


adf.test(xt[,"indpro"], k = 0) # 0.0498
adf.test(xt[,"pce"], k = 0) # 0.03656
adf.test(xt[,"fedfunds"], k = 0) # 9223


# Copias para no modificar los objetos originales
xt_us <- cbind(
  indpro = xt[-1, "indpro"],
  pce = xt[-1, "pce"],
  d_fedfunds = diff(xt[, "fedfunds"])
)

plot(xt_us)
zt_gt <- zt_var


# Prefijos para distinguir claramente los bloques
colnames(xt_us) <- paste0("US_", make.names(colnames(xt_us)))
colnames(zt_gt) <- paste0("GT_", make.names(colnames(zt_gt)))

# Estados Unidos primero; Guatemala después
Y <- na.omit(cbind(xt_us, zt_gt))

us_names <- colnames(xt_us)
gt_names <- colnames(zt_gt)
