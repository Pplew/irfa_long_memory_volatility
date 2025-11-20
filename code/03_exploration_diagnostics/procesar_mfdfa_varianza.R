# Establecer el directorio base del proyecto
setwd("/Users/josegamo/Documents/Quantiitative Finance/TFM/Datos/procesados")

# Cargar librerías necesarias
library(ggplot2)
library(dplyr)

# Crear carpetas de salida si no existen
#dir.create("resultados/mfdfa", showWarnings = FALSE, recursive = TRUE)
#dir.create("resultados/mfdfa/graficas", showWarnings = FALSE)

# Cargar archivo de resultados RDS
resultados <- readRDS("../Resultados/MF-DFA/mfdfa_varianza_resultados.rds")


# Procesar resultados a data frame largo
df <- bind_rows(lapply(names(resultados), function(activo) {
  data.frame(
    Activo = activo,
    q = resultados[[activo]]$q,
    hq = resultados[[activo]]$Hq
  )
}))


# Guardar resumen como CSV
write.csv(df, "../Resultados/MF-DFA/mfdfa_varianza_resultados_resumen.csv", row.names = FALSE)

# Gráfico combinado de h(q) para todos los activos
plot_global <- ggplot(df, aes(x = q, y = hq, color = Activo)) +
  geom_line(size = 1) +
  geom_point() +
  geom_vline(xintercept = 2, linetype = "dashed", color = "gray40") +
  labs(
    title = "Exponente h(q) en la varianza (retornos al cuadrado)",
    x = "q",
    y = "h(q)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Guardar gráfico combinado
ggsave("../Resultados/MF-DFA/hq_varianza_plot.png", plot_global, width = 8, height = 5)

# Generar y guardar gráficos individuales por activo
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
  
  ggsave(filename = paste0("../Resultados/MF-DFA/graficas/hq_", a, ".png"),
         plot = p, width = 6, height = 4)
}

