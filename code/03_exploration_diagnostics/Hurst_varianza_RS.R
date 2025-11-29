# =====================================
# Hurst_varianza_RS.R (replication-ready)
# Cálculo del exponente de Hurst (R/S) sobre retornos^2
# Input:  data_processed/*_ret.csv
# Output: data_processed/hurst_varianza_RS.csv
# =====================================

library(pracma)
library(here)

# Lista de archivos de retornos y nombres legibles
archivos <- c("bitcoin_ret.csv", "ibex35_ret.csv", "bovespa_ret.csv", "oro_ret.csv",
              "sp500_ret.csv", "tesla_ret.csv")

nombres  <- c("Bitcoin", "IBEX 35", "BOVESPA", "Oro", "S&P 500", "Tesla")

# Inicializar data frame para resultados
resultados_varianza <- data.frame(
  Activo        = character(),
  Hurst_RS_ret2 = numeric(),
  stringsAsFactors = FALSE
)

# Cálculo del Hurst para los retornos al cuadrado
for (i in seq_along(archivos)) {
  ruta_csv <- here("data_processed", archivos[i])
  
  if (!file.exists(ruta_csv)) {
    warning("No se encuentra el archivo de retornos: ", ruta_csv, " — se omite ", nombres[i])
    next
  }
  
  datos      <- read.csv(ruta_csv, header = TRUE)
  serie_ret  <- na.omit(as.numeric(datos[, 2]))
  serie_ret2 <- serie_ret^2
  
  hurst <- hurstexp(serie_ret2, display = FALSE)
  H_RS  <- hurst$Hs
  
  resultados_varianza <- rbind(
    resultados_varianza,
    data.frame(
      Activo        = nombres[i],
      Hurst_RS_ret2 = round(H_RS, 4),
      stringsAsFactors = FALSE
    )
  )
}

# Mostrar resultados por pantalla
print(resultados_varianza)

# Guardar resultados en CSV dentro de data_processed
ruta_out <- here("data_processed", "hurst_varianza_RS.csv")
write.csv(resultados_varianza, file = ruta_out, row.names = FALSE)
cat("\nResultados Hurst (R/S) sobre retornos^2 guardados en:", ruta_out, "\n")
