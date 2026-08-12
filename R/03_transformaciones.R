
#Variables Endógenas
zt <- ts(data[, c("i", "tc", "imae", "ipc", "bc")],  start = c(2010, 1), frequency = 12)
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

adf.test(zt[,"imae"], k = 12) # 0.01
adf.test(zt[,"ipc"], k = 12) # 0.5298
adf.test(zt[,"i"], k = 12) # 0.9647
adf.test(zt[,"bc"], k = 0) # 0.01
adf.test(zt[,"tc"], k = 1) # 0.5766

adf_i <- ur.df(
  diff(zt[, "i"]),
  type = "drift",
  lags = 12,
  selectlags = "AIC"
)
summary(adf_i)

adf_imae <- ur.df(
  diff(log(zt[, "imae"])),
  type = "trend",
  lags = 12,
  selectlags = "AIC"
)
summary(adf_imae)

adf_imae <- ur.df(
  diff(log(zt[, "imae"])),
  type = "trend",
  lags = 12,
  selectlags = "AIC"
)
summary(adf_imae)

adf_ipc <- ur.df(
  diff(log(zt[, "ipc"])),
  type = "drif",
  lags = 12,
  selectlags = "AIC"
)
summary(adf_ipc)





adf_bc <- ur.df(
  diff(log(zt[,"bc"])),
  type = "drift",
  lags = 12,
  selectlags = "AIC"
)
summary(adf_bc)



adf.test(log(zt[,"inflacion"])) 
adf.test(diff(log(zt[,"i"])))#fue necesario diferenciar 0.03614
adf.test(diff(zt[,"tc"])) 
# adf.test(diff(zt[,"tcr"])) # usar diferencia de log para interpretar como cambio porcentuallog(diff())


# si son mensuales por lo menos 12 rezagos
# trimestrales 4 rezagos

# Buscar como investigar restricciones de exclusión para forzar ceros

zt_diff <- diff(zt[, c("tc", "i", "inflacion")])
# plot(zt_diff)

zt_var <- cbind(
  dlog_imae = diff(log(zt[, "imae"])),
  dlog_ipc = diff(log(zt[,"ipc"])),
  d_i = diff(zt[,"i"]),
  dlog_bc = diff(log(zt[,"bc"])),
  tc = zt[-1,"tc"]
)

plot(zt_var)
plot.ts(xt, main = "Dinámica Multivariada Exogenas")
plot.ts(diff(xt), main = "Dinámica Multivariada")


adf.test(xt[,"indpro"], k = 12) # 0.3496
adf.test(xt[,"pce"], k = 12) # 0.1665
adf.test(xt[,"fedfunds"], k = 0) # 0.04216

adf.test(diff(log(xt[,"indpro"])), k = 0) # 0.3496
# “respuesta de las variables macroeconómicas de Guatemala ante una innovación 
# en el crecimiento de la producción industrial de Estados Unidos”.

adf.test(diff(log(xt[, "pce"])), k = 0) # 0.1665


adf_indpro <- ur.df(
  diff(log(xt[,"indpro"])),
  type = "trend",
  lags = 12,
  selectlags = "AIC"
)
summary(adf_indpro)



adf_pce <- ur.df(
  diff(log(xt[,"pce"])),
  type = "trend",
  lags = 12,
  selectlags = "AIC"
)
summary(adf_pce)



adf_fedfund <- ur.df(
  diff(xt[,"fedfunds"]),
  type = "drift",
  lags = 12,
  selectlags = "AIC"
)
summary(adf_fedfund)

# Copias para no modificar los objetos originales
xt_us <- cbind(
  dlog_indpro =  diff(log(xt[,"indpro"])),
  dlog_pce =  diff(log(xt[,"pce"])),
  d_fedfunds = diff(xt[, "fedfunds"])
)

# plot(xt_us)
zt_gt <- zt_var


# Prefijos para distinguir claramente los bloques
colnames(xt_us) <- paste0("US_", make.names(colnames(xt_us)))
colnames(zt_gt) <- paste0("GT_", make.names(colnames(zt_gt)))

# Estados Unidos primero; Guatemala después
Y <- na.omit(cbind(xt_us, zt_gt))

us_names <- colnames(xt_us)
gt_names <- colnames(zt_gt)

