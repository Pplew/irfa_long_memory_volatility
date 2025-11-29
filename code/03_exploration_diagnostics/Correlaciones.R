# =====================================
# Correlaciones.R (replication-ready)
# ACF y PACF de retornos y retornos^2
#
# Input:  data_processed/*_ret.csv
# Output: output/figures/diagnostics/autocorrelacion/*.png
# =====================================

library(zoo)
library(here)

# Carpeta donde guardar gráficos
output_dir <- here("output", "figures", "diagnostics", "autocorrelacion")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Directorio de entrada con retornos
input_dir <- here("data_processed")

# Listar los archivos de retornos
archivos <- list.files(input_dir, pattern = "_ret\\.csv$", full.names = TRUE)

# Función de análisis de autocorrelación
analizar_autocorrelacion <- function(ruta_archivo) {
  datos <- read.csv(ruta_archivo)
  names(datos) <- tolower(names(datos))  # Uniformiza los nombres
  
  if (!all(c("fecha", "retorno") %in% names(datos))) {
    cat("Saltando", ruta_archivo, "- columnas incorrectas\n")
    return(NULL)
  }
  
  # Elimina filas con NA
  datos <- datos[complete.cases(datos), ]
  
  # Asegura tipo fecha
  datos$fecha <- as.Date(datos$fecha)
  
  if (nrow(datos) < 10) {
    cat("Saltando", ruta_archivo, "- muy pocos datos\n")
    return(NULL)
  }
  
  # Nombre del activo a partir del nombre del fichero sin extensión
  nombre_archivo <- basename(ruta_archivo)
  nombre_activo  <- tools::file_path_sans_ext(nombre_archivo)
  
  retorno  <- zoo(datos$retorno, order.by = datos$fecha)
  retorno2 <- retorno^2
  
  # Gráficos ACF y PACF de retornos
  png(file = file.path(output_dir, paste0(nombre_activo, "_acf_retornos.png")),
      width = 1000, height = 800)
  par(mfrow = c(2, 1))
  acf(coredata(retorno), main = paste("ACF -", nombre_activo, "(retornos)"))
  pacf(coredata(retorno), main = paste("PACF -", nombre_activo, "(retornos)"))
  dev.off()
  
  # Gráficos ACF y PACF de retornos al cuadrado
  png(file = file.path(output_dir, paste0(nombre_activo, "_acf_retornos2.png")),
      width = 1000, height = 800)
  par(mfrow = c(2, 1))
  acf(coredata(retorno2), main = paste("ACF -", nombre_activo, "(retornos²)"))
  pacf(coredata(retorno2), main = paste("PACF -", nombre_activo, "(retornos²)"))
  dev.off()
  
  cat("✓ Procesado", nombre_archivo, "\n")
}

# Ejecutar el análisis para todos
lapply(archivos, analizar_autocorrelacion)

cat("\nCorrelaciones.R completado.\n")
