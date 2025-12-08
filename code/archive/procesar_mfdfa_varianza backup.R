# =====================================
# procesar_mfdfa_varianza.R (replication-ready)
# A partir de mfdfa_varianza_resultados.rds:
#   - construye un resumen (activo, q, hq)
#   - guarda CSV en data_processed/
#   - genera figuras en output/figures/mfdfa/
# =====================================

library(ggplot2)
library(dplyr)
library(here)

# Crear carpeta de figuras si no existe
dir.create(here("output", "figures", "mfdfa"), recursive = TRUE, showWarnings = FALSE)

# Cargar archivo de resultados RDS (varianza)
ruta_rds <- here("data_processed", "mfdfa_varianza_resultados.rds")
if (!file.exists(ruta_rds)) {
  stop("No se encuentra mfdfa_varianza_resultados.rds en data_processed/. Ejecuta antes mfdfa_varianza_script.R")
}

resultados <- readRDS(ruta_rds)

if (!is.list(resultados) || length(resultados) == 0) {
  stop("El objeto resultados cargado desde mfdfa_varianza_resultados.rds está vacío o no es una lista.")
}

# Procesar resultados a data frame largo
df <- bind_rows(lapply(names(resultados), function(activo) {
  data.frame(
    activo = activo,
    q      = resultados[[activo]]$q,
    hq     = resultados[[activo]]$Hq
  )
}))

if (nrow(df) == 0) {
  stop("No se han podido construir filas válidas en el resumen MF-DFA (varianza).")
}

# Guardar resumen como CSV (el que usa Evaluacion_modelos_actualizado_v2.R)
ruta_csv <- here("data_processed", "mfdfa_varianza_resultados_resumen.csv")
write.csv(df, ruta_csv, row.names = FALSE)
cat("Resumen MF-DFA (varianza) guardado en:", ruta_csv, "\n")

# Gráfico combinado de h(q) para todos los activos (varianza)
plot_global <- ggplot(df, aes(x = q, y = hq, color = activo)) +
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

ruta_fig_global <- here("output", "figures", "mfdfa", "hq_varianza_plot.png")
ggsave(ruta_fig_global, plot_global, width = 8, height = 5)
cat("Gráfico global h(q) varianza guardado en:", ruta_fig_global, "\n")

# Gráficos individuales por activo
activos <- unique(df$activo)

for (a in activos) {
  df_a <- df[df$activo == a, ]
  p <- ggplot(df_a, aes(x = q, y = hq)) +
    geom_line(color = "#2C3E50", size = 1) +
    geom_point(color = "#2C3E50") +
    geom_vline(xintercept = 2, linetype = "dashed", color = "gray50") +
    labs(
      title = paste("h(q) – varianza –", a),
      x = "q",
      y = "h(q)"
    ) +
    theme_minimal()
  
  # Nota: usamos sufijo _v para distinguir varianza
  ruta_fig_a <- here("output", "figures", "mfdfa", paste0("hq_", a, "_v.png"))
  ggsave(filename = ruta_fig_a, plot = p, width = 6, height = 4)
  cat("Gráfico individual h(q) varianza para", a, "guardado en:", ruta_fig_a, "\n")
}

cat("\nprocesar_mfdfa_varianza.R completado.\n")
