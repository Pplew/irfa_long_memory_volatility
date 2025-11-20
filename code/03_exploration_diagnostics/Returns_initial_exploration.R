# Transformación y exploración (versión local desde carpeta crudos)

# Establecer directorio de trabajo explícitamente
setwd("/Users/josegamo/Documents/Quantiitative Finance/TFM/Datos/crudos")

# Cargar librerías necesarias
library(zoo)
library(moments)

# Lista de activos
activos <- c("sp500", "ibex35", "bovespa", "oro", "bitcoin", "tesla")

# Función de procesamiento
procesar_serie <- function(nombre) {
  cat(paste("Procesando:", nombre, "\n"))
  ruta <- paste0(nombre, ".csv")
  if (!file.exists(ruta)) {
    cat("Archivo no encontrado.\n\n")
    return(NULL)
  }
  
  precios <- read.zoo(ruta, sep = ",", header = TRUE, format = "%Y-%m-%d")
  na_iniciales <- sum(is.na(precios))
  cat(paste("NA detectados en", nombre, ":", na_iniciales, "\n"))
  precios <- na.omit(precios)
  
  fechas <- index(precios)
  precios_vec <- coredata(precios)
  
  # Precios logarítmicos
  logprecios <- log(precios_vec)
  serie_logprecios <- zoo(logprecios, order.by = fechas)
  
  # Retornos logarítmicos
  retornos <- diff(logprecios)
  fechas_ret <- fechas[-1]
  serie_ret <- zoo(retornos, order.by = fechas_ret)
  
  # Guardar precios logarítmicos
  df_logp <- data.frame(
    fecha = index(serie_logprecios),
    log_precio = coredata(serie_logprecios)
  )
  write.csv(df_logp, file = paste0("../procesados/", nombre, "_price.csv"), row.names = FALSE)
  
  # Guardar retornos
  df <- data.frame(
    fecha = index(serie_ret),
    retorno = coredata(serie_ret)
  )
  write.csv(df, file = paste0("../procesados/", nombre, "_ret.csv"), row.names = FALSE)
  
  # Estadísticas
  resumen <- data.frame(
    Activo = nombre,
    Observaciones = length(precios_vec),
    Desde = as.character(min(fechas)),
    Hasta = as.character(max(fechas)),
    NA_detectados = na_iniciales,
    Media = mean(retornos, na.rm = TRUE),
    Desviacion = sd(retornos, na.rm = TRUE),
    Min = min(retornos, na.rm = TRUE),
    Max = max(retornos, na.rm = TRUE),
    Skewness = skewness(retornos, na.rm = TRUE),
    Kurtosis = kurtosis(retornos, na.rm = TRUE)
  )
  write.csv(resumen, file = paste0("../descriptivos/resumen_", nombre, ".csv"), row.names = FALSE)
  
  # Gráficos
  png(paste0("../descriptivos/histogramas/", nombre, "_precio.png"), width = 800, height = 400)
  plot(precios, main = paste("Precio de cierre -", toupper(nombre)), xlab = "Fecha", ylab = "Precio", col = "blue")
  dev.off()
  
  png(paste0("../descriptivos/histogramas/", nombre, "_retornos.png"), width = 800, height = 400)
  plot(serie_ret, main = paste("Retornos logarítmicos -", toupper(nombre)), xlab = "Fecha", ylab = "Retorno", col = "darkgreen")
  dev.off()
  
  png(paste0("../descriptivos/histogramas/", nombre, "_hist.png"), width = 600, height = 400)
  hist(retornos, breaks = 60, main = paste("Histograma de retornos -", toupper(nombre)),
       xlab = "Retorno", col = "lightblue", border = "white")
  dev.off()
  
  cat("Listo.\n\n")
  return(resumen)
}

# Ejecutar para todos los activos
resumenes <- do.call(rbind, lapply(activos, procesar_serie))
write.csv(resumenes, file = "../descriptivos/resumen_general.csv", row.names = FALSE)
