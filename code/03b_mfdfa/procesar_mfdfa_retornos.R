# ============================================
# procesar_mfdfa_retornos.R
# A partir de mfdfa_resultados.rds (retornos),
# genera:
#  - replication_data/MF-DFA/Retornos/mfdfa_resultados_resumen.csv
#  - figuras h(q) global e individuales
# ============================================

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(here)
})

# --- 1) Localizar el RDS de MF-DFA en retornos ---
rds_path <- here("data_processed", "mfdfa_resultados.rds")
if (!file.exists(rds_path)) {
  stop("No se encuentra el fichero RDS con MF-DFA de retornos en: ",
       rds_path, 
       "\nEjecuta antes code/03b_mfdfa/Analysis_mfdfa_ok.R")
}

resultados <- readRDS(rds_path)

# --- 2) Carpetas de salida ---
dir_csv <- here("replication_data", "MF-DFA", "Retornos")
dir_fig <- here("output", "figures", "mfdfa", "retornos")

dir.create(dir_csv, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_fig, recursive = TRUE, showWarnings = FALSE)

# --- 3) Construir data frame largo (Activo, q, hq) ---
df <- bind_rows(lapply(names(resultados), function(activo) {
  q_vals  <- resultados[[activo]]$q
  hq_vals <- resultados[[activo]]$Hq
  
  if (is.numeric(q_vals) && is.numeric(hq_vals) &&
      length(q_vals) == length(hq_vals) && length(q_vals) > 0) {
    data.frame(
      Activo = activo,
      q      = q_vals,
      hq     = hq_vals
    )
  } else {
    message(sprintf("⚠️ Activo '%s' descartado: q = %d, Hq = %d",
                    activo, length(q_vals), length(hq_vals)))
    NULL
  }
}))

# --- 4) Guardar CSV resumen donde lo espera ARFIMA_inicializado.R ---
csv_out <- file.path(dir_csv, "mfdfa_resultados_resumen.csv")
write.csv(df, csv_out, row.names = FALSE)
cat("CSV MF-DFA retornos guardado en:\n  ", csv_out, "\n", sep = "")

# --- 5) Gráfico combinado h(q) para todos los activos ---
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

fig_global <- file.path(dir_fig, "hq_retornos_plot.png")
ggsave(fig_global, plot_global, width = 8, height = 5)
cat("Gráfico global h(q) guardado en:\n  ", fig_global, "\n", sep = "")

# --- 6) Gráficos individuales por activo ---
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
  
  f_out <- file.path(dir_fig, paste0("hq_", a, ".png"))
  ggsave(filename = f_out, plot = p, width = 6, height = 4)
  cat("Gráfico individual h(q) guardado en:\n  ", f_out, "\n", sep = "")
}

cat("\nprocesar_mfdfa_retornos.R completado.\n")
