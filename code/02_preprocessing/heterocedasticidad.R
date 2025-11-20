
# Script de análisis de homocedasticidad: varianza móvil + test de ARCH

# Librerías necesarias
library(zoo)
library(FinTS)

# Establecer el directorio de trabajo donde están los retornos procesados
setwd("/Users/josegamo/Documents/Quantiitative Finance/TFM/Datos/procesados")

# Listar archivos *_ret.csv
archivos <- list.files(pattern = "_ret.csv$")

# Crear carpeta para guardar gráficos si no existe
if (!dir.exists("../graficos_varianza")) {
  dir.create("../graficos_varianza")
}

# Inicializar tabla de resultados
resultados_arch <- data.frame(
  Activo = character(),
  Estadistico = numeric(),
  Pvalor = numeric(),
  Heterocedasticidad = character(),
  stringsAsFactors = FALSE
)

# Loop por archivo
for (archivo in archivos) {
  datos <- read.csv(archivo)
  names(datos) <- c("Fecha", "Retorno")
  serie <- zoo(datos$Retorno, order.by = as.Date(datos$Fecha))
  serie <- serie[complete.cases(serie)]

  # Gráfico de varianza móvil
  var_movil <- rollapply(serie, width = 100, FUN = var, fill = NA, align = "right")
  png(paste0("../graficos_varianza/", tools::file_path_sans_ext(archivo), "_varmovil.png"), width = 800, height = 400)
  plot(var_movil, type = "l", col = "darkred", lwd = 2, main = paste("Varianza móvil (100 días) -", archivo),
       ylab = "Varianza", xlab = "Fecha")
  dev.off()

  # Test ARCH
  resultado <- ArchTest(coredata(serie), lags = 10)
  decision <- ifelse(resultado$p.value < 0.05, "Sí", "No")

  resultados_arch <- rbind(resultados_arch, data.frame(
    Activo = tools::file_path_sans_ext(archivo),
    Estadistico = round(resultado$statistic, 4),
    Pvalor = round(resultado$p.value, 5),
    Heterocedasticidad = decision
  ))
}

# Mostrar resultados
print(resultados_arch)
write.csv(resultados_arch, "heterocedasticidad_arch.csv", row.names = FALSE)
