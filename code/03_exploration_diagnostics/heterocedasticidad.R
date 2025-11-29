# =====================================
# Heterocedasticidad.R (replication-ready)
# Análisis de heterocedasticidad:
#   - Varianza móvil (window 100)
#   - Test ARCH LM (FinTS::ArchTest)
#
# Input:  data_processed/*_ret.csv
# Output:
#   - output/figures/diagnostics/varianza_movil/<activo>_varmovil.png
#   - replication_data/diagnostics/heterocedasticidad_arch.csv
# =====================================

library(zoo)
library(FinTS)
library(here)

# -------------------------------------
# Crear carpetas de salida
# -------------------------------------
dir.create(here("output", "figures", "diagnostics", "varianza_movil"),
           recursive = TRUE, showWarnings = FALSE)

dir.create(here("replication_data", "diagnostics"),
           recursive = TRUE, showWarnings = FALSE)

# Directorio de entrada con retornos
input_dir <- here("data_processed")

# Listar archivos *_ret.csv
archivos <- list.files(input_dir, pattern = "_ret\\.csv$", full.names = TRUE)

# Inicializar tabla de resultados
resultados_arch <- data.frame(
  Activo            = character(),
  Estadistico       = numeric(),
  Pvalor            = numeric(),
  Heterocedasticidad= character(),
  stringsAsFactors  = FALSE
)

# -------------------------------------
# Loop por archivo
# -------------------------------------
for (ruta in archivos) {
  datos <- read.csv(ruta)
  names(datos) <- tolower(names(datos))
  
  if (!all(c("fecha", "retorno") %in% names(datos))) {
    cat("Saltando", ruta, "- columnas incorrectas\n")
    next
  }
  
  datos <- datos[complete.cases(datos), ]
  if (nrow(datos) < 100) {
    cat("Saltando", ruta, "- muy pocos datos para varianza móvil (n < 100)\n")
    next
  }
  
  fecha_vec <- as.Date(datos$fecha)
  serie_zoo <- zoo(datos$retorno, order.by = fecha_vec)
  
  # Gráfico de varianza móvil
  var_movil <- rollapply(serie_zoo, width = 100, FUN = var,
                         fill = NA, align = "right")
  
  nombre_archivo <- basename(ruta)
  activo_id      <- tools::file_path_sans_ext(nombre_archivo)
  
  ruta_png <- here("output", "figures", "diagnostics",
                   "varianza_movil",
                   paste0(activo_id, "_varmovil.png"))
  
  png(ruta_png, width = 800, height = 400)
  plot(var_movil, type = "l", col = "darkred", lwd = 2,
       main = paste("Varianza móvil (100 días) -", activo_id),
       ylab = "Varianza", xlab = "Fecha")
  dev.off()
  
  # Test ARCH
  resultado <- ArchTest(coredata(serie_zoo), lags = 10)
  decision  <- ifelse(resultado$p.value < 0.05, "Sí", "No")
  
  resultados_arch <- rbind(
    resultados_arch,
    data.frame(
      Activo            = activo_id,
      Estadistico       = round(unname(resultado$statistic), 4),
      Pvalor            = round(resultado$p.value, 5),
      Heterocedasticidad= decision,
      stringsAsFactors  = FALSE
    )
  )
  
  cat("✓ Procesado", nombre_archivo, "\n")
}

# Mostrar resultados
print(resultados_arch)

# Guardar resultados en CSV
ruta_out <- here("replication_data", "diagnostics", "heterocedasticidad_arch.csv")
write.csv(resultados_arch, ruta_out, row.names = FALSE)

cat("\nResultados ARCH LM guardados en:", ruta_out, "\n")
cat("Heterocedasticidad.R completado.\n")
