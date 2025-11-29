# =====================================
# procesar_mfdfa_retornos.R (replication-ready)
# A partir de mfdfa_resultados.rds:
#   - construye un resumen (activo, q, hq)
#   - guarda CSV en data_processed/
#   - genera figuras en output/figures/mfdfa/
# =====================================

library(ggplot2)
library(dplyr)
library(here)

# Crear carpeta de figuras si no existe
dir.create(here("output", "figures", "mfdfa"), recursive = TRUE, showWarnings = FALSE)

# Cargar archivo RDS con resultados MF-DFA sobre retornos
ruta_rds <- here("data_processed", "mfdfa_resultados.rds")
if (!file.exists(ruta_rds)) {
  stop("No se encuentra mfdfa_resultados.rds en data_processed/. Ejecuta antes Analysis_mdfa_ok.R")
}

resultados <- readRDS(ruta_rds)

if (!is.list(resultados) || length(resultados) == 0) {
  stop("El objeto resultados cargado desde mfdfa_resultados.rds está vacío o no es una lista.")
}

# Construcción del data frame usando Hq
df <- bind_rows(lapply(names(resultados), function(activo) {
  q_vals  <- resultados[[activo]]$q
  hq_vals <- resultados[[activo]]$Hq
  
  if (is.numeric(q_vals) && is.numeric(hq_vals) &&
      length(q_vals) == length(hq_vals) && length(q_vals) > 0) {
    data.frame(
      activo = activo,
      q      = q_vals,
      hq     = hq_vals
    )
  } else {
    message(sprintf("⚠️ Activo '%s' descartado: q = %d, Hq = %d",
                    activo, length(q_vals), length(hq_vals)))
    NULL
  }
}))

if (nrow(df) == 0) {
  stop("No se han podido construir filas válidas en el resumen MF-DFA (retornos).")
}

# Guardar CSV resumen en data_processed/
ruta_csv <- here("data_processed", "mfdfa_resultados_resumen.csv")
write.csv(df, ruta_csv, row.names = FALSE)
cat("Resumen MF-DFA (retornos) guardado en:", ruta_csv, "\n")

# Gráfico combinado de h(q) para todos los activos (retornos)
plot_global <- ggplot(df, aes(x = q, y = hq, color = activo)) +
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

ruta_fig_global <- here("output", "figures", "mfdfa", "hq_retornos_plot.png")
ggsave(ruta_fig_global, plot_global, width = 8, height = 5)
cat("Gráfico global h(q) guardado en:", ruta_fig_global, "\n")

# Gráficos individuales por activo
activos <- unique(df$activo)

for (a in activos) {
  df_a <- df[df$activo == a, ]
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
  
  ruta_fig_a <- here("output", "figures", "mfdfa", paste0("hq_", a, ".png"))
  ggsave(filename = ruta_fig_a, plot = p, width = 6, height = 4)
  cat("Gráfico individual h(q) para", a, "guardado en:", ruta_fig_a, "\n")
}

cat("\nprocesar_mfdfa_retornos.R completado.\n")
