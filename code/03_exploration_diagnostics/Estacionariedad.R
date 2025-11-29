# =====================================
# Estacionariedad.R (replication-ready)
# Tests ADF de estacionariedad en:
#   - retornos
#   - retornos^2
#
# Input:  data_processed/*_ret.csv
# Output:
#   - replication_data/diagnostics/resultados_estacionariedad.csv
#   - replication_data/diagnostics/estacionariedad_retornos2.csv
# =====================================

library(tseries)
library(here)

# Crear carpeta de salida
dir.create(here("replication_data", "diagnostics"), recursive = TRUE, showWarnings = FALSE)

# Directorio de entrada con retornos
input_dir <- here("data_processed")

# Listar archivos de retornos
archivos <- list.files(input_dir, pattern = "_ret\\.csv$", full.names = TRUE)

# -------------------------------------
# ADF en retornos
# -------------------------------------
resultados <- data.frame(
  Activo                    = character(),
  Estadistico               = numeric(),
  Pvalor                    = numeric(),
  Concluye_estacionariedad  = character(),
  stringsAsFactors          = FALSE
)

for (ruta in archivos) {
  datos <- read.csv(ruta)
  names(datos) <- tolower(names(datos))
  
  if (!"retorno" %in% names(datos)) {
    cat("Saltando", ruta, "- columna 'retorno' no encontrada\n")
    next
  }
  
  serie <- datos$retorno
  serie <- serie[is.finite(serie)]
  
  if (length(serie) < 10) {
    cat("Saltando", ruta, "- muy pocos datos\n")
    next
  }
  
  resultado <- adf.test(serie)
  activo_id <- tools::file_path_sans_ext(basename(ruta))
  
  decision <- ifelse(resultado$p.value < 0.05, "Sí", "No")
  
  resultados <- rbind(
    resultados,
    data.frame(
      Activo                   = activo_id,
      Estadistico              = round(unname(resultado$statistic), 4),
      Pvalor                   = round(resultado$p.value, 5),
      Concluye_estacionariedad = decision,
      stringsAsFactors         = FALSE
    )
  )
}

# Mostrar resultados por consola
print(resultados)

# Guardar resultados en CSV (retornos)
ruta_out1 <- here("replication_data", "diagnostics", "resultados_estacionariedad.csv")
write.csv(resultados, file = ruta_out1, row.names = FALSE)
cat("\nResultados ADF en retornos guardados en:", ruta_out1, "\n")

# -------------------------------------
# ADF en retornos^2
# -------------------------------------
resultados_ret2 <- data.frame(
  Activo     = character(),
  Test       = character(),
  Estadistico= numeric(),
  Pvalor     = numeric(),
  Decision   = character(),
  stringsAsFactors = FALSE
)

for (ruta in archivos) {
  datos <- read.csv(ruta, stringsAsFactors = FALSE)
  names(datos) <- tolower(names(datos))
  
  if (!"retorno" %in% names(datos)) {
    cat("Saltando", ruta, "- columna 'retorno' no encontrada\n")
    next
  }
  
  serie <- datos$retorno
  serie <- serie[is.finite(serie)]
  if (length(serie) < 10) {
    cat("Saltando", ruta, "- muy pocos datos (retornos^2)\n")
    next
  }
  
  ret2 <- serie^2
  
  adf2 <- tryCatch(adf.test(ret2), error = function(e) NULL)
  if (!is.null(adf2)) {
    resultados_ret2 <- rbind(
      resultados_ret2,
      data.frame(
        Activo      = tools::file_path_sans_ext(basename(ruta)),
        Test        = "ADF",
        Estadistico = unname(adf2$statistic),
        Pvalor      = adf2$p.value,
        Decision    = ifelse(adf2$p.value < 0.05,
                             "Rechaza H0 (estacionaria)",
                             "No rechaza H0"),
        stringsAsFactors = FALSE
      )
    )
  }
}

ruta_out2 <- here("replication_data", "diagnostics", "estacionariedad_retornos2.csv")
write.csv(resultados_ret2, ruta_out2, row.names = FALSE)
cat("Resultados ADF en retornos^2 guardados en:", ruta_out2, "\n")

cat("\nEstacionariedad.R completado.\n")
