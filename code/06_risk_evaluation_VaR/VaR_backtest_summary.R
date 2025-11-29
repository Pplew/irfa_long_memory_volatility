# ==========================================================
# VaR_backtest_summary.R
# 
# Objetivo:
#   - Tomar los resultados de GARCH/FIGARCH del bloque 5
#   - Extraer las violaciones y p-values de VaR (1% y 5%)
#   - Generar tablas limpias para el replication package:
#       * replication_data/risk/VaR_backtest_long.csv
#       * replication_data/risk/VaR_backtest_wide.csv
#
# NO recalcula modelos ni VaR. Solo resume lo ya generado.
# ==========================================================

suppressPackageStartupMessages({
  library(here)
  library(data.table)
})

# 1) Localizar el fichero maestro del bloque 5
in_path <- here("replication_data", "variance_models",
                "resultados_modelos_fraccionales_v2_2_oroSoloMedia.csv")

if (!file.exists(in_path)) {
  stop("No encuentro el fichero de resultados de varianza en:\n  ",
       in_path,
       "\nEjecuta antes el script del bloque 5: ",
       "code/05_VARIANCE_MODELS_GARCH_FIGARCH/Evaluacion_modelos_actualizado_v2.R")
}

res_var <- fread(in_path)

# Comprobar que están las columnas esperadas de VaR
cols_needed <- c(
  "Activo",
  "VaR1_viol_GARCH", "VaR1_pval_GARCH",
  "VaR1_viol_FIGARCH", "VaR1_pval_FIGARCH",
  "VaR5_viol_GARCH", "VaR5_pval_GARCH",
  "VaR5_viol_FIGARCH", "VaR5_pval_FIGARCH"
)

if (!all(cols_needed %in% names(res_var))) {
  stop("Faltan columnas de VaR en el fichero de varianza.\n",
       "Esperaba al menos:\n  ", paste(cols_needed, collapse = ", "))
}

# 2) Crear carpeta de salida para riesgo
dir_risk <- here("replication_data", "risk")
dir.create(dir_risk, recursive = TRUE, showWarnings = FALSE)

# 3) Formato LONG: una fila por (Activo, Modelo, Nivel)
long_list <- list()

for (i in seq_len(nrow(res_var))) {
  act <- res_var$Activo[i]
  
  long_list[[length(long_list) + 1]] <- data.table(
    Activo      = act,
    Modelo      = "GARCH",
    Nivel       = "1%",
    Violaciones = res_var$VaR1_viol_GARCH[i],
    p_value     = res_var$VaR1_pval_GARCH[i]
  )
  long_list[[length(long_list) + 1]] <- data.table(
    Activo      = act,
    Modelo      = "FIGARCH",
    Nivel       = "1%",
    Violaciones = res_var$VaR1_viol_FIGARCH[i],
    p_value     = res_var$VaR1_pval_FIGARCH[i]
  )
  long_list[[length(long_list) + 1]] <- data.table(
    Activo      = act,
    Modelo      = "GARCH",
    Nivel       = "5%",
    Violaciones = res_var$VaR5_viol_GARCH[i],
    p_value     = res_var$VaR5_pval_GARCH[i]
  )
  long_list[[length(long_list) + 1]] <- data.table(
    Activo      = act,
    Modelo      = "FIGARCH",
    Nivel       = "5%",
    Violaciones = res_var$VaR5_viol_FIGARCH[i],
    p_value     = res_var$VaR5_pval_FIGARCH[i]
  )
}

VaR_long <- rbindlist(long_list, fill = TRUE)

# 4) Guardar formato LONG
out_long <- file.path(dir_risk, "VaR_backtest_long.csv")
fwrite(VaR_long, out_long)
cat("Tabla LONG de VaR guardada en:\n  ", out_long, "\n", sep = "")

# 5) Formato WIDE: una fila por activo, columnas por modelo y nivel
VaR_wide <- dcast(
  VaR_long,
  Activo ~ Modelo + Nivel,
  value.var = "Violaciones"
)

# (Opcional) añadir sufijo para aclarar que son violaciones
setnames(
  VaR_wide,
  old = setdiff(names(VaR_wide), "Activo"),
  new = paste0(setdiff(names(VaR_wide), "Activo"), "_viol")
)

out_wide <- file.path(dir_risk, "VaR_backtest_wide.csv")
fwrite(VaR_wide, out_wide)
cat("Tabla WIDE de VaR guardada en:\n  ", out_wide, "\n", sep = "")

cat("\nVaR_backtest_summary.R completado.\n")
