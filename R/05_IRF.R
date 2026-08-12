
#1. Calcular IRF para todas las combinaciones
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
  n.ahead = 12,
  ortho = TRUE,
  boot = TRUE,
  runs = 500,
  ci = 0.68
)


#plot(irf_us_gt)
# 2. Extraer datos y convertir a formato largo (Tidy)
# Función para extraer datos de IRF
source("fn_IRF.R")

# Extraer IRFs como un dataframe
irf_df <- extract_irf_data(irf_us_gt)
# plot(irf_us_gt)

# 3. Graficar matriz
# ggplot(irf_df, aes(x = Period, y = Value)) +
#   geom_line(color = "#2c3e50", linewidth = 0.8) +
#   geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "#3498db", alpha = 0.2) +
#   geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
#   facet_grid(Response ~ Impulse, scales = "free_y", switch = "y") +
#   labs(title = "Matriz de Funciones Impulso-Respuesta",
#        subtitle = "Columnas: Shock (Impulso) | Filas: Variable de Respuesta",
#        x = "Períodos", y = NULL) +
#   theme_bw() +
#   theme(strip.background = element_rect(fill = "#ecf0f1"),
#         strip.text = element_text(face = "bold"))



# Función para limpiar los nombres mostrados en los facets
limpiar_nombre <- function(x) {
  x <- sub("^xt_us\\.", "", x)
  x <- sub("^zt_gt\\.", "", x)
  x
}

ggplot(irf_df, aes(x = Period, y = Value)) +
  geom_ribbon(
    aes(ymin = Lower, ymax = Upper),
    fill = "#3498db",
    alpha = 0.2
  ) +
  geom_line(
    color = "#2c3e50",
    linewidth = 0.8
  ) +
  geom_hline(
    yintercept = 0,
    color = "red",
    linetype = "dashed"
  ) +
  facet_grid(
    Response ~ Impulse,
    scales = "free_y",
    switch = "y",
    labeller = labeller(
      Impulse = limpiar_nombre,
      Response = limpiar_nombre
    )
  ) +
  labs(
    title = "Matriz de Funciones Impulso-Respuesta",
    subtitle = "Columnas: Shock (Impulso) | Filas: Variable de respuesta",
    x = "Períodos",
    y = NULL
  ) +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "#ecf0f1"),
    strip.text = element_text(face = "bold")
  )



irf_cum <- irf(
  var_bloques,
  impulse = us_names,
  response = gt_names,
  n.ahead = 24,
  ortho = TRUE,
  cumulative = TRUE,
  boot = TRUE,
  runs = 5000,
  ci = 0.95,
  seed = 123
)

plot(irf_cum)
