# Script: Cálculo del exponente de Hurst clásico (Rescaled Range - R/S)
# Evaluado sobre retornos. Opción para incluir precios si se desea.

# Paquete necesario
# if (!require("pracma")) install.packages("pracma")
library(pracma)
library(zoo)

# Directorio con los retornos y precios
setwd("/Users/josegamo/Documents/Quantiitative Finance/TFM/Datos/procesados")

# Lista de activos y nombres legibles
activos <- c("bitcoin", "ibex35", "bovespa", "oro", "sp500", "tesla")
nombres <- c("Bitcoin", "IBEX 35", "BOVESPA", "Oro", "S&P 500", "Tesla")

# Inicializar tabla de resultados
resultados <- data.frame(
  Activo = character(),
  Hurst_RS_ret = numeric(),
  # Hurst_RS_price = numeric(),  # Descomentar si se desea también sobre precios
  stringsAsFactors = FALSE
)

# Bucle principal
for (i in seq_along(activos)) {
  activo <- activos[i]
  nombre_legible <- nombres[i]
  
  # Leer retornos
  archivo_ret <- paste0(activo, "_ret.csv")
  datos_ret <- read.csv(archivo_ret)
  serie_ret <- na.omit(as.numeric(datos_ret[, 2]))
  
  # Calcular Hurst sobre retornos
  hurst_ret <- hurstexp(serie_ret, display = FALSE)$Hs
  
  # Si deseas calcular también sobre precios:
  # archivo_price <- paste0(activo, "_price.csv")
  # if (file.exists(archivo_price)) {
  #   datos_price <- read.csv(archivo_price)
  #   serie_price <- na.omit(as.numeric(datos_price[, 2]))
  #   hurst_price <- hurstexp(serie_price, display = FALSE)$Hs
  # } else {
  #   hurst_price <- NA
  # }
  
  # Guardar resultados
  resultados <- rbind(resultados, data.frame(
    Activo = nombre_legible,
    Hurst_RS_ret = round(hurst_ret, 4)
    # , Hurst_RS_price = round(hurst_price, 4)
  ))
}

# Mostrar
print(resultados)

# Guardar en CSV
write.csv(resultados, file = "../Resultados/hurst_clasico_RS.csv", row.names = FALSE)
