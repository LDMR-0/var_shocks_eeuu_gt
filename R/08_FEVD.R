
# C. Descomposición de Varianza (FEVD)

# ¿Qué porcentaje de la varianza del error de pronóstico de 'Empleo'
# se debe a sí mismo vs. a shocks en 'Prod' o 'Salarios'?
fevd_model <- fevd(var_bloques, n.ahead = 10)
dev.off()
graphics.off()
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


plot(
  fevd_model,
  plot.type = "multiple",
  nc = 2,
  xlab = "Meses",
  ylab = "Proporción de la varianza"

)