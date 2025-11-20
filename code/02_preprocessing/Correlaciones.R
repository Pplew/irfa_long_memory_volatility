# Establece el directorio donde están los CSV de retornos
setwd("/Users/josegamo/Documents/Quantiitative Finance/TFM/Datos/procesados")

# Cargar librerías necesarias
library(zoo)

# Carpeta donde guardar gráficos
output_dir <- "../../descriptivos/autocorrelacion"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Listar los archivos de retornos
archivos <- list.files(pattern = "_ret\\.csv$")

# Función de análisis de autocorrelación
analizar_autocorrelacion <- function(nombre_archivo) {
  datos <- read.csv(nombre_archivo)
  names(datos) <- tolower(names(datos))  # Uniformiza los nombres
  
  if (!all(c("fecha", "retorno") %in% names(datos))) {
    cat("Saltando", nombre_archivo, "- columnas incorrectas\n")
    return(NULL)
  }
  
  # Elimina filas con NA
  datos <- datos[complete.cases(datos), ]
  
  # Asegura tipo fecha
  datos$fecha <- as.Date(datos$fecha)
  
  if (nrow(datos) < 10) {
    cat("Saltando", nombre_archivo, "- muy pocos datos\n")
    return(NULL)
  }
  
  nombre_activo <- tools::file_path_sans_ext(nombre_archivo)
  retorno <- zoo(datos$retorno, order.by = datos$fecha)
  retorno2 <- retorno^2
  
  # Gráficos ACF y PACF de retornos
  png(file = file.path(output_dir, paste0(nombre_activo, "_acf_retornos.png")), width = 1000, height = 800)
  par(mfrow = c(2,1))
  acf(coredata(retorno), main = paste("ACF -", nombre_activo, "(retornos)"))
  pacf(coredata(retorno), main = paste("PACF -", nombre_activo, "(retornos)"))
  dev.off()
  
  # Gráficos ACF y PACF de retornos al cuadrado
  png(file = file.path(output_dir, paste0(nombre_activo, "_acf_retornos2.png")), width = 1000, height = 800)
  par(mfrow = c(2,1))
  acf(coredata(retorno2), main = paste("ACF -", nombre_activo, "(retornos²)"))
  pacf(coredata(retorno2), main = paste("PACF -", nombre_activo, "(retornos²)"))
  dev.off()
  
  cat("✓ Procesado", nombre_archivo, "\n")
}

# Ejecutar el análisis para todos
lapply(archivos, analizar_autocorrelacion)

