# =====================================
# Data_Collection.R (replication-ready)
# Descarga de precios diarios desde Yahoo Finance
# Activos: S&P 500, IBEX 35, BOVESPA, oro, Bitcoin y Tesla
# Salida: CSVs en data_raw/ con fecha y precio ajustado
# =====================================

library(quantmod)
library(here)  # Rutas relativas basadas en el proyecto

# -------------------------------------
# Fechas
# -------------------------------------
fecha_inicio <- as.Date("2000-01-03")
fecha_btc    <- as.Date("2014-01-01")
fecha_tsla   <- as.Date("2013-01-02")
fecha_fin    <- as.Date("2024-12-31")

# -------------------------------------
# Lista de activos y fechas personalizadas
# -------------------------------------
activos <- list(
  sp500   = list(ticker = "^GSPC",   desde = fecha_inicio),
  ibex35  = list(ticker = "^IBEX",   desde = fecha_inicio),
  bovespa = list(ticker = "^BVSP",   desde = fecha_inicio),
  oro     = list(ticker = "GC=F",    desde = fecha_inicio),
  bitcoin = list(ticker = "BTC-USD", desde = fecha_btc),
  tesla   = list(ticker = "TSLA",    desde = fecha_tsla)
)

# -------------------------------------
# Función para descargar y guardar CSV
# -------------------------------------
descargar_y_guardar <- function(nombre, info) {
  cat("\nDescargando:", nombre, "desde", info$desde, "hasta", fecha_fin, "\n")
  
  datos <- try(
    getSymbols(
      info$ticker,
      src         = "yahoo",
      from        = info$desde,
      to          = fecha_fin,
      auto.assign = FALSE
    ),
    silent = TRUE
  )
  
  if (inherits(datos, "try-error")) {
    cat("  -> ERROR al descargar:", nombre, "\n")
    return(NULL)
  }
  
  # Precio ajustado (Adjusted Close)
  precios <- Ad(datos)
  
  # Data frame limpio: fecha + precio_ajustado
  df <- data.frame(
    fecha           = as.Date(index(precios)),
    precio_ajustado = as.numeric(precios)
  )
  
  # Nombre de archivo y ruta (usando here + data_raw)
  nombre_archivo <- paste0(nombre, ".csv")
  ruta_archivo   <- here("data_raw", nombre_archivo)
  
  # Guardar CSV en data_raw/
  write.csv(df, file = ruta_archivo, row.names = FALSE)
  
  cat("  -> Guardado en:", ruta_archivo, "\n")
  invisible(df)
}

# -------------------------------------
# Ejecutar la descarga para todos los activos
# -------------------------------------
lapply(names(activos), function(n) {
  descargar_y_guardar(n, activos[[n]])
})

cat("\nDescarga completada.\n")
