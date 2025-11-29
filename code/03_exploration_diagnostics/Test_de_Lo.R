# =====================================
# Test_de_Lo.R (replication-ready)
# Test de Lo para memoria larga en:
#   - retornos (media)
#   - retornos^2 (varianza)
#
# Input:  data_processed/<activo>_ret.csv
# Output: replication_data/diagnostics/test_lo_media_varianza.csv
# =====================================

library(zoo)
library(here)

# -------------------------------------
# Función para calcular el estadístico V(q) de Lo
# -------------------------------------
test_lo <- function(series, q = 10) {
  n <- length(series)
  series <- series - mean(series)
  Z <- cumsum(series)
  R <- max(Z) - min(Z)
  S2 <- var(series)
  
  # Estimador Newey-West de la varianza
  gamma0 <- S2
  gammas <- sapply(1:q, function(lag) {
    cov(series[-(1:lag)], series[-((n - lag + 1):n)])
  })
  S_hat2 <- gamma0 + 2 * sum((1 - (1:q) / (q + 1)) * gammas)
  
  Vq <- (R / sqrt(n * S_hat2))
  return(Vq)
}

# -------------------------------------
# Crear carpeta de salida
# -------------------------------------
dir.create(here("replication_data", "diagnostics"), recursive = TRUE, showWarnings = FALSE)

# Lista de archivos y nombres legibles
archivos <- c("bitcoin_ret.csv", "ibex35_ret.csv", "bovespa_ret.csv", "oro_ret.csv",
              "sp500_ret.csv", "tesla_ret.csv")

nombres  <- c("Bitcoin", "IBEX 35", "BOVESPA", "Oro", "S&P 500", "Tesla")

# Inicializar resultados
resultados_lo <- data.frame(
  Activo            = character(),
  Vq_media          = numeric(),
  Concluye_media    = character(),
  Vq_varianza       = numeric(),
  Concluye_varianza = character(),
  stringsAsFactors  = FALSE
)

# Valor crítico para 5% (aprox. normal estándar)
critico <- 1.65

# -------------------------------------
# Calcular test de Lo para cada activo
# -------------------------------------
for (i in seq_along(archivos)) {
  ruta_csv <- here("data_processed", archivos[i])
  
  if (!file.exists(ruta_csv)) {
    warning("No se encuentra el archivo de retornos: ", ruta_csv, " — se omite ", nombres[i])
    next
  }
  
  datos  <- read.csv(ruta_csv, header = TRUE)
  serie  <- as.numeric(datos[, 2])
  serie  <- na.omit(serie)
  serie2 <- serie^2
  
  Vq_media <- test_lo(serie,  q = 10)
  Vq_var   <- test_lo(serie2, q = 10)
  
  concl_media <- ifelse(abs(Vq_media) > critico, "Memoria larga", "No")
  concl_var   <- ifelse(abs(Vq_var)   > critico, "Memoria larga", "No")
  
  resultados_lo <- rbind(
    resultados_lo,
    data.frame(
      Activo            = nombres[i],
      Vq_media          = round(Vq_media, 4),
      Concluye_media    = concl_media,
      Vq_varianza       = round(Vq_var, 4),
      Concluye_varianza = concl_var,
      stringsAsFactors  = FALSE
    )
  )
}

# Mostrar resultados
print(resultados_lo)

# Guardar resultados en CSV dentro de replication_data/diagnostics
ruta_out <- here("replication_data", "diagnostics", "test_lo_media_varianza.csv")
write.csv(resultados_lo, file = ruta_out, row.names = FALSE)

cat("\nResultados test de Lo guardados en:", ruta_out, "\n")
