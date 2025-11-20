# Paquetes necesarios

library(zoo)

# Función para calcular el estadístico V(q) de Lo
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
  S_hat2 <- gamma0 + 2 * sum((1 - (1:q)/(q+1)) * gammas)
  
  Vq <- (R / sqrt(n * S_hat2))
  return(Vq)
}

# Directorio
setwd("/Users/josegamo/Documents/Quantiitative Finance/TFM/Datos/procesados")

# Lista de archivos
archivos <- c("bitcoin_ret.csv", "ibex35_ret.csv", "bovespa_ret.csv", "oro_ret.csv", "sp500_ret.csv", "tesla_ret.csv")
nombres <- c("Bitcoin", "IBEX 35", "BOVESPA", "oro", "S&P 500", "Tesla")

# Inicializar resultados
resultados_lo <- data.frame(
  Activo = character(),
  Vq_media = numeric(),
  Concluye_media = character(),
  Vq_varianza = numeric(),
  Concluye_varianza = character(),
  stringsAsFactors = FALSE
)

# Valor crítico para 5% (aprox. normal estándar)
critico <- 1.65

# Calcular test de Lo para cada activo
for (i in seq_along(archivos)) {
  datos <- read.csv(archivos[i], header = TRUE)
  serie <- as.numeric(datos[, 2])
  serie <- na.omit(serie)
  serie2 <- serie^2
  
  Vq_media <- test_lo(serie, q = 10)
  Vq_var   <- test_lo(serie2, q = 10)
  
  concl_media <- ifelse(abs(Vq_media) > critico, "Memoria larga", "No")
  concl_var   <- ifelse(abs(Vq_var) > critico, "Memoria larga", "No")
  
  resultados_lo <- rbind(resultados_lo, data.frame(
    Activo = nombres[i],
    Vq_media = round(Vq_media, 4),
    Concluye_media = concl_media,
    Vq_varianza = round(Vq_var, 4),
    Concluye_varianza = concl_var
  ))
}

# Mostrar resultados
print(resultados_lo)

# Guardar resultados
write.csv(resultados_lo, "../resultados/test_lo_media_varianza.csv", row.names = FALSE)

