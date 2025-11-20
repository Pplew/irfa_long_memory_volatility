# Establecer el directorio base del proyecto
setwd("/Users/josegamo/Documents/Quantiitative Finance/TFM/Datos/procesados")

# Cargar librerías necesarias
library(ggplot2)
library(dplyr)

# Crear carpetas de salida si no existen
# dir.create("resultados/mfdfa", showWarnings = FALSE, recursive = TRUE)
# dir.create("resultados/mfdfa/graficas", showWarnings = FALSE)

# Cargar archivo RDS con resultados MF-DFA sobre retornos
resultados <- readRDS("../Resultados/MF-DFA/Retornos/mfdfa_resultados.rds")

# Construcción del data frame usando Hq
df <- bind_rows(lapply(names(resultados), function(activo) {
  q_vals <- resultados[[activo]]$q
  hq_vals <- resultados[[activo]]$Hq
  
  if (is.numeric(q_vals) && is.numeric(hq_vals) &&
      length(q_vals) == length(hq_vals) && length(q_vals) > 0) {
    data.frame(
      Activo = activo,
      q = q_vals,
      hq = hq_vals
    )
  } else {
    message(sprintf("⚠️ Activo '%s' descartado: q = %d, Hq = %d",
                    activo, length(q_vals), length(hq_vals)))
    NULL
  }
}))

# Guardar CSV resumen
write.csv(df, "../Resultados/MF-DFA/Retornos/mfdfa_resultados_resumen.csv", row.names = FALSE)

# Gráfico combinado de h(q) para todos los activos (retornos)
plot_global <- ggplot(df, aes(x = q, y = hq, color = Activo)) +
  geom_line(size = 1) +
  geom_point() +
  geom_vline(xintercept = 2, linetype = "dashed", color = "gray50") +
  labs(
    title = "Exponente h(q) en los retornos",
    x = "q",
    y = "h(q)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("../Resultados/MF-DFA/Retornos/graficas/hq_retornos_plot.png", plot_global, width = 8, height = 5)

# Gráficos individuales por activo
activos <- unique(df$Activo)

for (a in activos) {
  df_a <- df[df$Activo == a, ]
  p <- ggplot(df_a, aes(x = q, y = hq)) +
    geom_line(color = "#2C3E50", size = 1) +
    geom_point(color = "#2C3E50") +
    geom_vline(xintercept = 2, linetype = "dashed", color = "gray50") +
    labs(
      title = paste("h(q) –", a),
      x = "q",
      y = "h(q)"
    ) +
    theme_minimal()
  
  ggsave(filename = paste0("../Resultados/MF-DFA/Retornos/graficas/hq_", a, ".png"),
         plot = p, width = 6, height = 4)
}

