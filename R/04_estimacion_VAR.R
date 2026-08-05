
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
seleccion$selection

# p_optimo <- as.integer(seleccion$selection["SC(n)"])
p_optimo <- as.integer(seleccion$selection["AIC(n)"])
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
# Auqnue este resultado se rechaze Ho, siendo Ho = zt no causa xt, por la
# teoría asumimos que las variables de Guatemala no afectan a variables EEUU


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
R
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



# ------------------------------------------------------------------------------
# 3. Estimación del VAR
# ------------------------------------------------------------------------------
# Estimación del modelo VAR con la librería
# var_model <- VAR(var_data, p = 12, type = "const")
# varx_model <- VAR(y = zt, exogen = xt, p = 12, type = "const")



# Muestra las ecuaciones individuales. Explicar R-cuadrado y significancia.
summary(var_bloques)


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
roots(var_bloques) 

# B. Autocorrelación serial en los residuos (Portmanteau Test)

# A portmanteau test is a statistical hypothesis test used to check if a set of
# autocorrelations is simultaneously zero. It's often used in time series
# analysis to assess how well a model fits the data, checking for residual
# correlations in a flexible way. The null hypothesis is that the model is
# adequate, while the alternative hypothesis is that it is not adequate in a
# general way, rather than specifying a precise form of inadequacy. 

# H0: No hay autocorrelación serial
serial_test <- serial.test(var_bloques, lags.pt = 1, type = "PT.asymptotic")
serial_test

# C. Normalidad de los residuos
norm_test <- normality.test(var_bloques)
norm_test



# ------------------------------------------------------------------------------
# 5. Análisis Estructural (Día 3 - Lo más importante)
# ------------------------------------------------------------------------------

# A. Causalidad de Granger
# ¿Ayuda la variable 'Salarios' a predecir 'Empleo'?
granger_cause <- causality(var_bloques, cause = "zt_gt.GT_bc")
granger_cause
 