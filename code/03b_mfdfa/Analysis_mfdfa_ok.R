# =====================================
# Analysis_mdfa_ok.R (replication-ready)
# Aplica MF-DFA a los retornos logarítmicos de todos los activos
# Input:  data_processed/*_ret.csv
# Output: data_processed/mfdfa_resultados.rds
# =====================================

library(MFDFA)
library(zoo)
library(here)

# Directorio donde están los retornos procesados
data_dir <- here("data_processed")

# Listar archivos de retornos procesados
archivos <- list.files(path = data_dir, pattern = "_ret.csv", full.names = TRUE)

if (length(archivos) == 0) {
  stop("No se han encontrado archivos *_ret.csv en data_processed/. Ejecuta antes Preprocessing_returns.R")
}

# Parámetros MF-DFA
q_values   <- c(-4, -3, -2, -1, 1, 2, 3, 4)
max_escala <- 0.1 * 3000  # igual que en tu versión original
escalas    <- floor(2^seq(2, log2(max_escala), length.out = 20))

# Objeto para almacenar resultados
resultados_mfdfa <- list()

# Loop principal
for (archivo in archivos) {
  datos <- read.csv(archivo)
  nombre_archivo <- basename(archivo)
  nombre <- sub("_ret.csv", "", nombre_archivo)
  
  # Convertir a serie temporal tipo zoo
  serie <- zoo(datos[, 2], order.by = as.Date(datos[, 1]))
  
  # Aplicar MF-DFA
  resultado <- MFDFA(coredata(serie), q = q_values, scale = escalas)
  
  # Guardar también los q utilizados
  resultado$q <- q_values
  
  # Añadir al listado
  resultados_mfdfa[[nombre]] <- resultado
  
  cat("✓ Procesado", nombre_archivo, "\n")
}

# Guardar el resultado completo en data_processed/
ruta_rds <- here("data_processed", "mfdfa_resultados.rds")
saveRDS(resultados_mfdfa, ruta_rds)
cat("\nResultados MF-DFA (retornos) guardados en:", ruta_rds, "\n")
