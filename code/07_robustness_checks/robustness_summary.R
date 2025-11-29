# ==========================================================
# robustness_summary.R
#
# Objetivo:
#   - Reunir y estandarizar los resultados de robustez de:
#       * ARFIMA (media): robustez_parametrica_d.csv
#       * FIGARCH (varianza): robustez_figarch_d_submuestras.csv
#   - Copiarlos a replication_data/robustness/
#   - Construir una tabla resumen combinada por activo
#
# NO recalcula modelos ni d; solo organiza resultados existentes.
# ==========================================================

suppressPackageStartupMessages({
  library(here)
  library(data.table)
})

# --------------------------------------------
# 1) Carpeta de salida para robustez
# --------------------------------------------
dir_rob <- here("replication_data", "robustness")
dir.create(dir_rob, recursive = TRUE, showWarnings = FALSE)

# --------------------------------------------
# 2) Localizar robustez ARFIMA (media)
#    Intentamos primero en replication_data, luego en Resultados
# --------------------------------------------
candidates_arf <- c(
  here("replication_data", "mean_models", "ARFIMA_inicializado",
       "robustez_parametrica_d.csv"),
  here("Resultados", "ARFIMA_inicializado", "robustez_parametrica_d.csv")
)

path_arf <- candidates_arf[file.exists(candidates_arf)][1]

if (is.na(path_arf)) {
  stop("No se ha encontrado 'robustez_parametrica_d.csv'.\n",
       "Ejecuta antes el script de evaluación de la media (ARFIMA vs ARIMA).")
}

cat("Usando robustez ARFIMA desde:\n  ", path_arf, "\n", sep = "")
rob_arf <- fread(path_arf)

# Copia directa a carpeta de robustness
out_arf <- file.path(dir_rob, "robustez_ARFIMA_param_d.csv")
fwrite(rob_arf, out_arf)
cat("Copiado robustez ARFIMA a:\n  ", out_arf, "\n", sep = "")

# --------------------------------------------
# 3) Localizar robustez FIGARCH (varianza)
#    Esta viene del bloque 5: Evaluacion_modelos_actualizado_v2.R
# --------------------------------------------
path_fig <- here("replication_data", "variance_models",
                 "robustez_figarch_d_submuestras.csv")

if (!file.exists(path_fig)) {
  stop("No se ha encontrado 'robustez_figarch_d_submuestras.csv'.\n",
       "Ejecuta antes el script del bloque 5: ",
       "code/05_VARIANCE_MODELS_GARCH_FIGARCH/Evaluacion_modelos_actualizado_v2.R")
}

cat("Usando robustez FIGARCH desde:\n  ", path_fig, "\n", sep = "")
rob_fig <- fread(path_fig)

out_fig <- file.path(dir_rob, "robustez_FIGARCH_d_submuestras.csv")
fwrite(rob_fig, out_fig)
cat("Copiado robustez FIGARCH a:\n  ", out_fig, "\n", sep = "")

# --------------------------------------------
# 4) Tabla combinada ARFIMA + FIGARCH por activo
#    (solo join, sin redefinir criterios de robustez)
# --------------------------------------------
# Normalizamos nombre de columna "Activo" por si acaso
if (!"Activo" %in% names(rob_arf)) {
  stop("La tabla de ARFIMA no contiene columna 'Activo'.")
}
if (!"Activo" %in% names(rob_fig)) {
  stop("La tabla de FIGARCH no contiene columna 'Activo'.")
}

arf2 <- copy(rob_arf)
fig2 <- copy(rob_fig)

# Merge por Activo (all = TRUE para ver perfectamente qué hay en cada lado)
rob_comb <- merge(arf2, fig2, by = "Activo", all = TRUE)

out_comb <- file.path(dir_rob, "robustez_resumen_ARFIMA_FIGARCH.csv")
fwrite(rob_comb, out_comb)
cat("Tabla combinada ARFIMA + FIGARCH guardada en:\n  ", out_comb, "\n", sep = "")

cat("\nrobustness_summary.R completado.\n")
