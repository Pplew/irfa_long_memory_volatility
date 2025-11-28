# =====================================
# Preprocessing_returns.R
# Crea log-retornos a partir de data_raw/
# Salida: data_processed/<activo>_ret.csv y <activo>_price.csv
# =====================================

library(zoo)
library(here)

# -------------------------------------
# Activos considerados en el proyecto
# -------------------------------------
activos <- c("sp500", "ibex35", "bovespa", "oro", "bitcoin", "tesla")

# Crear carpeta de salida si no existe
if (!dir.exists(here("data_processed"))) {
  dir.create(here("data_processed"), recursive = TRUE)
}

# -------------------------------------
# Función principal de procesamiento
# -------------------------------------
procesar_serie <- function(nombre) {
  cat("\nProcesando:", nombre, "\n")
  
  # Ruta del archivo crudo desde data_raw
  ruta_raw <- here("data_raw", paste0(nombre, ".csv"))
  
  if (!file.exists(ruta_raw)) {
    cat("  -> Archivo no encontrado:", ruta_raw, "\n")
    return(NULL)
  }
  
  # Leer datos crudos (fecha, precio_ajustado)
  precios <- read.zoo(ruta_raw, sep = ",", header = TRUE, format = "%Y-%m-%d")
  precios <- na.omit(precios)
  
  fechas <- index(precios)
  precios_vec <- coredata(precios)
  
  # Log-precios
  logprecios <- log(precios_vec)
  serie_logprecios <- zoo(logprecios, order.by = fechas)
  
  # Log-retornos
  retornos <- diff(logprecios)
  fechas_ret <- fechas[-1]
  serie_ret <- zoo(retornos, order.by = fechas_ret)
  
  # Guardar precios logarítmicos
  df_logp <- data.frame(
    fecha      = index(serie_logprecios),
    log_precio = as.numeric(coredata(serie_logprecios))
  )
  ruta_price <- here("data_processed", paste0(nombre, "_price.csv"))
  write.csv(df_logp, file = ruta_price, row.names = FALSE)
  
  # Guardar retornos
  df_ret <- data.frame(
    fecha   = index(serie_ret),
    retorno = as.numeric(coredata(serie_ret))
  )
  ruta_ret <- here("data_processed", paste0(nombre, "_ret.csv"))
  write.csv(df_ret, file = ruta_ret, row.names = FALSE)
  
  cat("  -> Precio log guardado en:", ruta_price, "\n")
  cat("  -> Retornos guardados en:", ruta_ret, "\n")
  
  invisible(df_ret)
}

# -------------------------------------
# Ejecutar para todos los activos
# -------------------------------------
invisible(lapply(activos, procesar_serie))

cat("\nPreprocessing completado.\n")
