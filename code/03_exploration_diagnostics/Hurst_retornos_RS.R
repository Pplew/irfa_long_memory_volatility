# =====================================
# Hurst_retornos_RS.R (replication-ready)
# Cálculo del exponente de Hurst clásico (R/S)
# Evaluado sobre retornos logarítmicos.
#
# Input:  data_processed/<activo>_ret.csv
# Output: replication_data/diagnostics/hurst_clasico_RS.csv
# =====================================

library(pracma)
library(zoo)
library(here)

# Crear carpeta de salida si no existe
dir.create(here("replication_data", "diagnostics"), recursive = TRUE, showWarnings = FALSE)

# Lista de activos y nombres legibles
activos <- c("bitcoin", "ibex35", "bovespa", "oro", "sp500", "tesla")
nombres <- c("Bitcoin", "IBEX 35", "BOVESPA", "Oro", "S&P 500", "Tesla")

# Inicializar tabla de resultados
resultados <- data.frame(
  Activo       = character(),
  Hurst_RS_ret = numeric(),
  stringsAsFactors = FALSE
)

# Bucle principal
for (i in seq_along(activos)) {
  activo         <- activos[i]
  nombre_legible <- nombres[i]
  
  # Archivo de retornos en data_processed
  ruta_ret <- here("data_processed", paste0(activo, "_ret.csv"))
  if (!file.exists(ruta_ret)) {
    warning("No se encuentra el archivo de retornos: ", ruta_ret, " — se omite ", nombre_legible)
    next
  }
  
  datos_ret <- read.csv(ruta_ret)
  serie_ret <- na.omit(as.numeric(datos_ret[, 2]))
  
  # Cálculo de Hurst (método clásico R/S)
  hurst_ret <- hurstexp(serie_ret, display = FALSE)$Hs
  
  # Añadir fila
  resultados <- rbind(
    resultados,
    data.frame(
      Activo       = nombre_legible,
      Hurst_RS_ret = round(hurst_ret, 4),
      stringsAsFactors = FALSE
    )
  )
}

# Mostrar resultados
print(resultados)

# Guardar en CSV en replication_data/diagnostics/
ruta_out <- here("replication_data", "diagnostics", "hurst_clasico_RS.csv")
write.csv(resultados, file = ruta_out, row.names = FALSE)

cat("\nHurst (R/S) sobre retornos guardado en:", ruta_out, "\n")
