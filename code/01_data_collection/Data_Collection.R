#Obtención de datos históricos
# Objetivo: Descargar series de precios diarios de S&P 500, IBEX 35, bovespa, oro, Bitcoin y Tesla
#desde Yahoo Finance.

# Cargar librería necesaria
library(quantmod)


setwd("/Users/josegamo/Documents/Quantiitative Finance/TFM/Datos/crudos")
# Definir fechas
fecha_inicio <- as.Date("2000-01-03")
fecha_btc   <- as.Date("2014-01-01")
fecha_tsla  <- as.Date("2013-01-02")
fecha_fin   <- as.Date("2024-12-31")

# Lista de activos y fechas personalizadas
activos <- list(
  sp500   = list(ticker = "^GSPC",  desde = fecha_inicio),
  ibex35  = list(ticker = "^IBEX",  desde = fecha_inicio),
  bovespa = list(ticker = "^BVSP",  desde = fecha_inicio),
  oro     = list(ticker = "GC=F",   desde = fecha_inicio),
  bitcoin = list(ticker = "BTC-USD", desde = fecha_btc),
  tesla   = list(ticker = "TSLA",    desde = fecha_tsla)
)

# Función para descargar y guardar CSV
descargar_y_guardar <- function(nombre, info) {
  cat(paste("Descargando:", nombre, "desde", info$desde, "\n"))
  datos <- try(getSymbols(info$ticker, src = "yahoo", from = info$desde,
                          to = fecha_fin, auto.assign = FALSE))
  if (inherits(datos, "try-error")) {
    cat(paste("Error al descargar:", nombre, "\n"))
    return(NULL)
  }
  datos_ajustados <- Ad(datos)  # Precio ajustado (Adjusted Close)
  nombre_archivo <- paste0(nombre, ".csv") #zoo lo importa quantmod
  cat(paste("Guardado en:", nombre_archivo, "\n\n"))
}

# Ejecutar la descarga para todos los activos
lapply(names(activos), function(n) descargar_y_guardar(n, activos[[n]]))

