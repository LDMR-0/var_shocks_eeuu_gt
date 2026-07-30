# ==============================================================================
# LABORATORIO 2 y 3: Vectores Autorregresivos (VAR)
# ==============================================================================
# Limpiar espacio de trabajo
rm(list = ls())

library(vars)
library(tidyverse)

# ------------------------------------------------------------------------------
# 1. Preparación de Datos Multivariados
# ------------------------------------------------------------------------------

# Simularemos un sistema VAR de 3 variables: PIB, Inflación, Tasa de Interés

data(Canada) # Dataset clásico incluido en R
var_data <- Canada[, c("e", "prod", "rw")] # Empleo, Productividad, Salarios Reales

plot.ts(var_data, main = "Dinámica Multivariada")
plot.ts(diff(var_data), main = "Dinámica Multivariada")

# IMPORTANTE: En un VAR estándar, asumimos que las series son Estacionarias.
# Si no lo son, se deberían diferenciar primero (o usar VEC, que veremos la otra semana).
# Para este ejemplo, asumiremos que ya están transformadas/estacionarias.
var_data <- diff(Canada[, c("e", "prod", "rw")])

# ------------------------------------------------------------------------------
# 2. Selección de Rezagos (Lag Selection)
# ------------------------------------------------------------------------------
# ¿Cuántos rezagos (p) debe tener el VAR?
lag_selection <- VARselect(var_data, lag.max = 8, type = "const")
lag_selection$selection

# TIP:
# AIC tiende a sobreestimar p (menos parsimonioso).
# SC (BIC) tiende a subestimar p (mejor para inferencia/parsimonia).
# Usaremos p = 2 para el ejemplo.

# ------------------------------------------------------------------------------
# 3. Estimación del VAR
# ------------------------------------------------------------------------------
# Estimación del modelo VAR con la librería
var_model <- VAR(var_data, p = 2, type = "const")

# Muestra las ecuaciones individuales. Explicar R-cuadrado y significancia.
summary(var_model)


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
roots(var_model) 

# Si las raíces están muy cerca de la unidad, aunque técnicamente el modelo
# podría ser estacionario, el proceso exhibe una alta persistencia. Esta
# situación genera problemas en muestras finitas, ya que resulta difícil
# distinguir si la raíz es exactamente unitaria o simplemente está muy cerca de
# la unidad


# B. Autocorrelación serial en los residuos (Portmanteau Test)

# A portmanteau test is a statistical hypothesis test used to check if a set of
# autocorrelations is simultaneously zero. It's often used in time series
# analysis to assess how well a model fits the data, checking for residual
# correlations in a flexible way. The null hypothesis is that the model is
# adequate, while the alternative hypothesis is that it is not adequate in a
# general way, rather than specifying a precise form of inadequacy. 

# H0: No hay autocorrelación serial
serial_test <- serial.test(var_model, lags.pt = 10, type = "PT.asymptotic")
serial_test

# C. Normalidad de los residuos
norm_test <- normality.test(var_model)
norm_test


# ------------------------------------------------------------------------------
# 5. Análisis Estructural (Día 3 - Lo más importante)
# ------------------------------------------------------------------------------

# A. Causalidad de Granger
# ¿Ayuda la variable 'Salarios' a predecir 'Empleo'?
granger_cause <- causality(var_model, cause = "rw")
granger_cause


# B. Funciones de Impulso-Respuesta (IRF)
# ¿Cómo reacciona el 'Empleo' ante un shock en 'Prod' (Productividad)?
# n.ahead = periodos hacia adelante
# boot = TRUE genera intervalos de confianza (Bootstrapping)

irf_prod_emp <- irf(var_model, 
                    impulse = "prod",    # Variable donde ocurre el shock
                    response = "e", # Variable que responde
                    n.ahead = 10, 
                    boot = TRUE,
                    ortho = TRUE)        # Ortogonalización (Cholesky)

# Graficar
plot(irf_prod_emp, main = "Respuesta del Empleo ante Shock en Productividad")

# Función para graficar una sola IRF con ggplot2
plot_single_irf_ggplot <- function(var_model, impulse_var, response_var, n.ahead = 10) {
  # Calcular IRF internamente
  irf_obj <- irf(var_model, impulse = impulse_var, response = response_var, 
                 n.ahead = n.ahead, boot = TRUE)
  
  # Extraer datos
  period <- 0:(nrow(irf_obj$irf[[impulse_var]]) - 1)
  value <- irf_obj$irf[[impulse_var]][, response_var]
  lower <- irf_obj$Lower[[impulse_var]][, response_var]
  upper <- irf_obj$Upper[[impulse_var]][, response_var]
  
  df <- data.frame(Period = period, Value = value, Lower = lower, Upper = upper)
  
  ggplot(df, aes(x = Period, y = Value)) +
    geom_line(color = "#2c3e50", linewidth = 1) +
    geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "#3498db", alpha = 0.2) +
    geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
    labs(title = paste("Respuesta de", response_var, "ante shock en", impulse_var),
         x = "Periodos", y = "Respuesta") +
    scale_x_continuous(breaks = 0:n.ahead, minor_breaks = NULL) + # Grid en enteros
    theme_minimal() +
    theme(panel.grid.major = element_line(color = "gray80"),
          panel.grid.minor = element_line(color = "gray90", linetype = "dotted"))
}

plot_single_irf_ggplot(var_model, "prod", "e")

# TIP:
# Si el intervalo (líneas punteadas) incluye el 0, 
# el efecto NO es estadísticamente significativo en ese periodo.

# Ejercicio, generar e interpretar todas las funciones de impulso-respuesta del modelo.

# 1. Calcular IRF para todas las combinaciones
irf_all <- irf(var_model, n.ahead = 10, boot = TRUE)

# 2. Extraer datos y convertir a formato largo (Tidy)
# Función para extraer datos de IRF
source("labs/lab02/fn_IRF.R")

# Extraer IRFs como un dataframe
irf_df <- extract_irf_data(irf_all)

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
fevd_model <- fevd(var_model, n.ahead = 10)
plot(fevd_model)

# Ver los valores numéricos
print(fevd_model$e)

# ------------------------------------------------------------------------------
# Visualización de FEVD con ggplot2
# ------------------------------------------------------------------------------

# Función para graficar FEVD
plot_fevd_ggplot <- function(fevd_obj, variable_name) {
  # Extraer la matriz de descomposición para la variable deseada
  fevd_data <- fevd_obj[[variable_name]]
  
  # Convertir a dataframe y añadir columna de periodo
  df_fevd <- as.data.frame(fevd_data)
  df_fevd$Period <- 1:nrow(df_fevd)
  
  # Convertir a formato largo (tidy)
  df_long <- df_fevd %>%
    pivot_longer(cols = -Period, names_to = "Shock", values_to = "Variance_Share")
  
  # Graficar
  ggplot(df_long, aes(x = Period, y = Variance_Share, fill = Shock)) +
    geom_area(alpha = 0.8, color = "white") +
    scale_y_continuous(labels = scales::percent) +
    labs(title = paste("Descomposición de Varianza para:", variable_name),
         x = "Horizonte de Pronóstico",
         y = "Porcentaje de Varianza Explicada") +
    theme_minimal() +
    scale_fill_brewer(palette = "Set2")
}

# Generar gráficos para cada variable
plot_fevd_ggplot(fevd_model, "e")
plot_fevd_ggplot(fevd_model, "prod")
plot_fevd_ggplot(fevd_model, "rw")
