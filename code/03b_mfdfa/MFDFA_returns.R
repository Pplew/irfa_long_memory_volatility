# =====================================
# MFDFA_returns.R
# Calcula MF-DFA sobre retornos logarítmicos
# Input:  data_processed/<activo>_ret.csv
# Output: data_processed/mfdfa_resultados_resumen.csv
# =====================================

library(here)

activos <- c("sp500", "ibex35", "bovespa", "oro", "bitcoin", "tesla")

resultados_list <- list()

for (activo in activos) {
  cat("\nMF-DFA retornos:", activo, "\n")
  
  ruta_ret <- here("data_processed", paste0(activo, "_ret.csv"))
  if (!file.exists(ruta_ret)) {
    cat("  -> No se encuentra", ruta_ret, "\n")
    next
  }
  
  df_ret <- read.csv(ruta_ret, stringsAsFactors = FALSE)
  serie <- as.numeric(df_ret$retorno)
  serie <- serie[is.finite(serie)]
  
  # ==============================
  # TODO: Enchufar tu código MF-DFA real aquí
  # Debe producir:
  #   q_vals  (vector de q)
  #   hq_vals (vector de H(q) para cada q)
  # ==============================
  
  q_vals  <- seq(-5, 5, by = 1)           # Placeholder temporal
  hq_vals <- rep(0.5, length(q_vals))     # Placeholder temporal
  
  res_activo <- data.frame(
    activo = activo,
    q      = q_vals,
    hq     = hq_vals
  )
  
  resultados_list[[activo]] <- res_activo
}

# Guardar CSV unificado para todos los activos
if (length(resultados_list) > 0) {
  mfdfa_resumen <- do.call(rbind, resultados_list)
  ruta_out <- here("data_processed", "mfdfa_resultados_resumen.csv")
  write.csv(mfdfa_resumen, file = ruta_out, row.names = FALSE)
  cat("\nMF-DFA retornos guardado en:", ruta_out, "\n")
} else {
  cat("\nNo se generaron resultados de MF-DFA sobre retornos.\n")
}

cat("\nMFDFA_returns.R completado.\n")
