# ------------------------------------------------------------------------------
# PRograma de Estudios Superiores (PES)
# Proyecto de Seminario: Transmisión de schocks reales de EE.UU. hacia Guatemala
# Fecha: 23/03/2026
# Atuor: Luis Daniel Monroy Rojas
# ------------------------------------------------------------------------------

rm(list = ls())
graphics.off()
cat("\014")
setwd("C:/Users/danie/Documents/Seminario/Proyecto")
# setwd("C:/Users/Daniel Monroy/Documents/Seminario/Proyecto")

#install.packages("tseries")
# importaciones de librerías
# tidyverse carga: ggplot2, dplyr, tidyr, readr, purrr, tibble, stringr, forcats
library(vars)
library(tidyverse)
library(readxl)
library(tseries)
library(urca)

cat("\014")

# ==============================================================================
# Datos de economía doméstica Guatemala
# Carga de datos:exportaciones, remesas, inflación y reservas internacionales
# ==============================================================================
# -----------------------------------------------------------------------------
# Datos del IMAE
# Fuente: https://banguat.gob.gt/page/indice-mensual-de-la-actividad-economica-imae-ano-de-referencia-2013
# -----------------------------------------------------------------------------
df_imae <- read_excel("input/imae_2.xlsx", sheet = 2)
df_imae <- df_imae |> dplyr::rename(fecha = 1, imae = 2)
df_imae <- df_imae |> mutate(fecha = as.Date(fecha))


# -----------------------------------------------------------------------------
# Datos de las remesas en millones de US dólares:
# Fuente: https://banguat.gob.gt/page/anos-2002-2026
# -----------------------------------------------------------------------------
meses <- c(
  "enero" = "01", "febrero" = "02", "marzo" = "03", "abril" = "04",
  "mayo" = "05", "junio" = "06", "julio" = "07", "agosto" = "08",
  "septiembre" = "09", "octubre" = "10", "noviembre" = "11", "diciembre" = "12"
)
df_temp <- read_excel("input/remesas.xlsx", sheet = 1, range = "B10:AA22")
df_remesas <- df_temp |> 
  tidyr::pivot_longer(cols = -Mes, names_to = "anio", values_to="remesas") |>
  dplyr::mutate(fecha = as.Date(paste(anio, meses[tolower(Mes)], "01", sep="-"))) |>
  dplyr::select(fecha, remesas) |>
  dplyr::arrange(fecha)
rm(df_temp)
plot(df_remesas)

# -----------------------------------------------------------------------------
# Datos de RMI netas en millones de US dólares
# Fuente: https://banguat.gob.gt/page/mensuales-1995-la-fecha
# -----------------------------------------------------------------------------

df_temp <- read_excel("input/rmi_netas.xlsx", sheet = 1, range = "A6:AG18")
df_rmi <- df_temp |>
  tidyr::pivot_longer(cols = -MES, names_to = "anio", values_to = "rmi") |>
  dplyr::mutate(fecha = as.Date(paste(anio, meses[tolower(MES)], "01", sep="-"))) |>
  dplyr::select(fecha, rmi) |>
  dplyr::arrange(fecha)
rm(df_temp)

# -----------------------------------------------------------------------------
# Datos de Índice de Precios al Consumidor (IPC) intermensual
# Fuente: https://banguat.gob.gt/page/indice-intermensual-interanual-y-acumulada
# -----------------------------------------------------------------------------
df_ipc <- read_excel("input/ipc.xlsx", sheet = 2)
df_ipc <- df_ipc |> 
  mutate(fecha = as.Date(PERIODO)) |>
  dplyr::rename(ipc = 3) |>
  dplyr::select(fecha, ipc)
  

# -----------------------------------------------------------------------------
# Datos de la Blanza Comercial (Exportaciones - Importaciones) en US dólares
# Fuente: https://banguat.gob.gt/page/serie-de-comercio-exterior-por-inciso-arancerlario-8-y-10-digitos
# -----------------------------------------------------------------------------
df_bc <- read_excel("input/balanza_comercial.xlsx", sheet = 2)
df_bc <- df_bc |>
  dplyr::mutate(fecha = as.Date(periodo), bc = exportaciones/importaciones) |>
  dplyr::select(fecha, bc)

# -----------------------------------------------------------------------------
# Datos Tasa de Interés Líder de la Política Monetaria de Guatemala
# Fuente: https://banguat.gob.gt/indicadoresgt/
# -----------------------------------------------------------------------------
df_i <- read_excel("input/tasa_lider.xlsx")
df_i <- df_i |> 
  dplyr::mutate(fecha = as.Date(fecha)) |>
  dplyr::rename(i = 7) |>
  dplyr::select(fecha, i)

# ------------------------------------------------------------------------------
# Datos Tipo de Cambio Bilateral Q-$ (TCR)
# Fuentes: https://fred.stlouisfed.org/series/CPIAUCSL
#          https://banguat.gob.gt/page/de-venta-promedio-del-mes
#          https://banguat.gob.gt/page/indice-intermensual-interanual-y-acumulada
# ------------------------------------------------------------------------------
df_tcr <- read_excel("input/construccion_tcr_bilateral.xlsx", sheet = 2)
df_tcr <- df_tcr |>
  dplyr::rename(tcr = 5) |>
  dplyr::select(fecha, tcr)
# ==============================================================================
# Datos de economía extranjeta (EE.UU.)
# ==============================================================================
# -----------------------------------------------------------------------------
# Datos de EE.UU para proxy de supply Industrial Production (index)
# Fuente: https://fred.stlouisfed.org/series/INDPRO
# -----------------------------------------------------------------------------
df_indpro <- read_excel("input/INDPRO.xlsx", sheet = 2)
df_indpro <- df_indpro |> dplyr::mutate(fecha = as.Date(observation_date)) |>
  rename(indpro = 2)

# -----------------------------------------------------------------------------
# Datos de EE.UU para proxy de shock monetario Tasa de Interes Efectiva Federal
# Fuente: https://fred.stlouisfed.org/series/DFF
# -----------------------------------------------------------------------------
df_fedfunds <- read_excel("input/FEDFUNDS.xlsx", sheet = 2)
df_fedfunds <- df_fedfunds |> dplyr::mutate(fecha = as.Date(observation_date)) |> 
  rename(fedfunds = 2)

# -----------------------------------------------------------------------------
# Datos de EE.UU para proxy de shock de Demanda Agregada Real Personal Consumption 
# Expenditures (PCEC96)
# Fuente: https://fred.stlouisfed.org/series/PCEC96
# -----------------------------------------------------------------------------

df_pce <- read_excel("input/PCEC96.xlsx", sheet = 2)
df_pce <- df_pce |> dplyr::mutate(fecha = as.Date(observation_date)) |>
  rename(pce = 2)


# ==============================================================================
# Creación del VAR: comenzando con 2002-01-01 (remesas) hasta enero 2026 (imae)
# Creación del VAR: comenzando con 2010-01-01 (IPC) hasta diciembre 2025 (bc)
# ==============================================================================
# Recordar: as.Date("yyyy-mm-dd")
fecha_ini <- as.Date("2010-01-01")
fecha_fin <- as.Date("2025-12-31")

df_bc <- df_bc |>
  dplyr::filter(fecha >= fecha_ini & fecha <= fecha_fin)

df_i <- df_i |>
  dplyr::filter(fecha >= fecha_ini & fecha <= fecha_fin)

df_imae <- df_imae |>
  dplyr::filter(fecha >= fecha_ini & fecha <= fecha_fin)

df_ipc <- df_ipc |>
  dplyr::filter(fecha >= fecha_ini & fecha <= fecha_fin)

df_remesas <- df_remesas |>
  dplyr::filter(fecha >= fecha_ini & fecha <= fecha_fin)

df_tcr <- df_tcr |>
  dplyr::filter(fecha >= fecha_ini & fecha <= fecha_fin)

df_rmi <- df_rmi |>
  dplyr::filter(fecha >= fecha_ini & fecha <= fecha_fin)

df_indpro <- df_indpro |>
  dplyr::filter(fecha >= fecha_ini & fecha <= fecha_fin)

df_pce <- df_pce |>
  dplyr::filter(fecha >= fecha_ini & fecha <= fecha_fin)

df_fedfunds <- df_fedfunds |>
  dplyr::filter(fecha >= fecha_ini & fecha <= fecha_fin)

# Pasar todas las fechas al primer día del mes
lista_dfs <- list(df_bc, df_i, df_imae, df_ipc, df_remesas, df_tcr, df_rmi,df_fedfunds, df_indpro, df_pce)

lista_dfs_mes <- map(lista_dfs, ~ .x |> 
   mutate(fecha = lubridate::floor_date(fecha, unit = "month"))
)

# Preparar la data que se usara en el modelo VAR
data <- reduce(lista_dfs_mes, inner_join, by = "fecha")
rm(lista_dfs_mes)
rm(lista_dfs)


#Variables Endógenas
zt <- ts(data[, c("imae", "ipc", "i", "bc", "remesas", "tcr")],  start = c(2010, 1),
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
adf.test(zt[,"tcr"], k = 0) # 0.1308

adf.test(diff(zt[,"i"]))#fue necesario diferenciar 0.0361
adf.test(diff(zt[,"tcr"])) # usar diferencia de log para interpretar como cambio porcentuallog(diff())


# si son mensuales por lo menos 12 rezagos
# trimestrales 4 rezagos

# Buscar como investigar restricciones de exclusión para forzar ceros

zt_diff <- diff(zt[, c("tcr", "i")])
plot(zt_diff)

zt_var <- cbind(
  imae      = zt[-1, "imae"],
  ipc    = zt[-1, "ipc"],
  d_i     = diff(zt[, "i"]),
  bc = zt[-1, "bc"],
  d_tcr     = diff(zt[, "tcr"])
)

plot(zt_var)
plot.ts(xt, main = "Dinámica Multivariada Exogenas")
plot.ts(diff(xt), main = "Dinámica Multivariada")


# Copias para no modificar los objetos originales
xt_us <- xt
zt_gt <- zt_var


# Prefijos para distinguir claramente los bloques
colnames(xt_us) <- paste0("US_", make.names(colnames(xt)))
colnames(zt_gt) <- paste0("GT_", make.names(colnames(zt_gt)))

# Estados Unidos primero; Guatemala después
Y <- na.omit(cbind(xt_us, zt_gt))

us_names <- paste(colnames(xt_us))
gt_names <- colnames(zt_gt)



# ------------------------------------------------------------------------------
# 2. Selección de Rezagos (Lag Selection)
# ------------------------------------------------------------------------------
# ¿Cuántos rezagos (p) debe tener el VAR?
lag_selection <- VARselect(zt_var, lag.max = 105, type = "const")
lag_selection$selection


seleccion <- VARselect(
  Y,
  lag.max = 12,
  type = "const"
)
p_optimo <- as.integer(seleccion$selection["SC(n)"])
p_optimo

# TIP:
# AIC tiende a sobreestimar p (menos parsimonioso).
# SC (BIC) tiende a subestimar p (mejor para inferencia/parsimonia).
# Usaremos p = 2 para el ejemplo.

# ------------------------------------------------------------------------------
# 2.5. Estimación del VAR sin restricción
# ------------------------------------------------------------------------------

var_sin_restriccion <- VAR(
  y = Y,
  p = p_optimo,
  type = "const"
)

model_names <- colnames(var_sin_restriccion$y)
gt_names <- grep(
  pattern = "^zt_gt\\.GT_",
  x = model_names,
  value = TRUE
)

us_names <- grep(
  pattern = "^xt_us\\.US_",
  x = model_names,
  value = TRUE
)

prueba_exogeneidad <- causality(
  var_sin_restriccion,
  cause = gt_names
)
prueba_exogeneidad$Granger


# ------------------------------------------------------------------------------
# 2.8 Construcción de matriz de restricciones por exclusión
# ------------------------------------------------------------------------------

K <- var_sin_restriccion$K

# Regresores del lado derecho de las ecuaciones
rhs_names <- colnames(
  var_sin_restriccion$datamat[, -(1:K), drop = FALSE]
)

# Inicialmente se conservan todos los coeficientes
R <- matrix(
  1L,
  nrow = K,
  ncol = length(rhs_names),
  dimnames = list(colnames(Y), rhs_names)
)

for (L in seq_len(p_optimo)) {
  
  columnas_gt_rezagadas <- paste0(gt_names, ".l", L)
  
  R[
    us_names,
    columnas_gt_rezagadas
  ] <- 0L
}

var_bloques <- restrict(
  var_sin_restriccion,
  method = "manual",
  resmat = R
)

R[us_names, , drop = FALSE]
R[gt_names, , drop = FALSE]

irf_us_gt <- irf(
  var_bloques,
  impulse = us_names,
  response = gt_names,
  n.ahead = 24,
  ortho = TRUE,
  boot = FALSE
)

plot(irf_us_gt)



# ------------------------------------------------------------------------------
# 3. Estimación del VAR
# ------------------------------------------------------------------------------
# Estimación del modelo VAR con la librería
# var_model <- VAR(var_data, p = 12, type = "const")



varx_model <- VAR(y = zt, exogen = xt, p = 12, type = "const")



# Muestra las ecuaciones individuales. Explicar R-cuadrado y significancia.
summary(varx_model)

# ------------------------------------------------------------------------------
# 4. Diagnóstico del Modelo (Validación)
# ------------------------------------------------------------------------------
# A. Estabilidad (Raíces del polinomio característico)
# Todas las raíces del polinomio característico deben estar dentro del círculo unitario (< 1) 
# para que el modelo sea estable (no explosivo). 

# Ojo: las raíces del polinomio característico inverso deben estar fuera del círculo unitario, 
# lo que es equivalente a que los eigenvalores de la matriz de la forma acompañante 
# tengan un módulo menor a 1. 

# Esta función devuelve los eigenvalores de la matriz de la forma acompañante. 
roots(varx_model) 

# B. Autocorrelación serial en los residuos (Portmanteau Test)

# A portmanteau test is a statistical hypothesis test used to check if a set of
# autocorrelations is simultaneously zero. It's often used in time series
# analysis to assess how well a model fits the data, checking for residual
# correlations in a flexible way. The null hypothesis is that the model is
# adequate, while the alternative hypothesis is that it is not adequate in a
# general way, rather than specifying a precise form of inadequacy. 

# H0: No hay autocorrelación serial
serial_test <- serial.test(varx_model, lags.pt = 12, type = "PT.asymptotic")
serial_test

# C. Normalidad de los residuos
norm_test <- normality.test(varx_model)
norm_test



# ------------------------------------------------------------------------------
# 5. Análisis Estructural (Día 3 - Lo más importante)
# ------------------------------------------------------------------------------

# A. Causalidad de Granger
# ¿Ayuda la variable 'Salarios' a predecir 'Empleo'?
granger_cause <- causality(varx_model, cause = "bc")
granger_cause

#1. Calcular IRF para todas las combinaciones
irf_all <- irf(varx_model, n.ahead = 10, boot = TRUE)


irf_us_gt <- irf(
  var_bloques,
  impulse = us_names,
  response = gt_names,
  n.ahead = 24,
  ortho = TRUE,
  boot = FALSE
)

irf_us_gt <- irf(
  var_bloques,
  impulse = us_names,
  response = gt_names,
  n.ahead = 24,
  ortho = TRUE,
  boot = TRUE,
  runs = 500,
  ci = 0.95
)


plot(irf_all)
# 2. Extraer datos y convertir a formato largo (Tidy)
# Función para extraer datos de IRF
source("fn_IRF.R")
source("fn_IRF_2.R")

# Extraer IRFs como un dataframe
irf_df <- extract_irf_data(irf_all)
irf_df <- extract_irf_data2(irf_us_gt)
plot(irf_us_gt)

# 3. Graficar matriz
ggplot(irf_df, aes(x = Period, y = Value)) +
  geom_line(color = "#2c3e50", linewidth = 0.8) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "#3498db", alpha = 0.2) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  facet_grid(Response ~ Impulse, scales = "free_y", switch = "y") +
  labs(title = "Matriz de Funciones Impulso-Respuesta",
       subtitle = "Columnas: Shock (Impulso) | Filas: Variable de Respuesta",
       x = "Periodos", y = NULL) +
  theme_bw() +
  theme(strip.background = element_rect(fill = "#ecf0f1"),
        strip.text = element_text(face = "bold"))


# C. Descomposición de Varianza (FEVD)

# ¿Qué porcentaje de la varianza del error de pronóstico de 'Empleo'
# se debe a sí mismo vs. a shocks en 'Prod' o 'Salarios'?
fevd_model <- fevd(varx_model, n.ahead = 10)
dev.off()
plot(fevd_model)

png(
  filename = "fevd_var.png",
  width = 1800,
  height = 1400,
  res = 180
)

par(mar = c(2, 2, 2, 1))
plot(fevd_model)

dev.off()

# Ver los valores numéricos
print(fevd_model$e)



