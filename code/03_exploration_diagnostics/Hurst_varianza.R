# Script: Cálculo del exponente de Hurst (R/S) aplicado a la varianza (retornos al cuadrado)

# Cargar librería necesaria
library(pracma)

# Establecer directorio de trabajo
setwd("/Users/josegamo/Documents/Quantiitative Finance/TFM/Datos/procesados")

# Lista de archivos de retornos
archivos <- c("bitcoin_ret.csv", "ibex35_ret.csv", "bovespa_ret.csv", "oro_ret.csv", 
              "sp500_ret.csv", "tesla_ret.csv")
nombres <- c("Bitcoin", "IBEX 35", "BOVESPA", "Oro",  "S&P 500", "Tesla")

# Inicializar data frame para resultados
resultados_varianza <- data.frame(
  Activo = character(),
  Hurst_RS_ret2 = numeric(),
  stringsAsFactors = FALSE
)

# Cálculo del Hurst para los retornos al cuadrado
for (i in seq_along(archivos)) {
  datos <- read.csv(archivos[i], header = TRUE)
  serie_ret <- na.omit(as.numeric(datos[, 2]))
  serie_ret2 <- serie_ret^2
  
  hurst <- hurstexp(serie_ret2, display = FALSE)
  H_RS <- hurst$Hs
  
  resultados_varianza <- rbind(resultados_varianza, data.frame(
    Activo = nombres[i],
    Hurst_RS_ret2 = round(H_RS, 4)
  ))
}

# Mostrar resultados por pantalla
print(resultados_varianza)

# Guardar resultados en CSV
write.csv(resultados_varianza, file = "../Resultados/hurst_varianza_RS.csv", row.names = FALSE)
