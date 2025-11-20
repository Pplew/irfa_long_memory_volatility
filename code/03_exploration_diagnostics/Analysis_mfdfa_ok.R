# Carga de paquetes
library(MFDFA)
library(zoo)

# Definir el directorio de trabajo
setwd("/Users/josegamo/Documents/Quantiitative Finance/TFM/Datos/procesados")

# Listar archivos de retornos procesados
archivos <- list.files(pattern = "_ret.csv")

# Parámetros MF-DFA
q_values <- c(-4, -3, -2, -1, 1, 2, 3, 4)
max_escala <- 0.1 * 3000  # para series largas, se puede ajustar
escalas <- floor(2^seq(2, log2(max_escala), length.out = 20))

# Objeto para almacenar resultados
resultados_mfdfa <- list()

# Loop principal
for (archivo in archivos) {
  datos <- read.csv(archivo)
  nombre <- sub("_ret.csv", "", archivo)
  
  # Convertir a serie temporal tipo zoo
  serie <- zoo(datos[, 2], order.by = as.Date(datos[, 1]))
  
  # Aplicar MF-DFA
  resultado <- MFDFA(coredata(serie), q = q_values, scale = escalas)
  
  # Guardar también los q utilizados
  resultado$q <- q_values
  
  # Añadir al listado
  resultados_mfdfa[[nombre]] <- resultado
  
  cat("✓ Procesado", archivo, "\n")
}

# Guardar el resultado completo
saveRDS(resultados_mfdfa, "mfdfa_resultados.rds")


