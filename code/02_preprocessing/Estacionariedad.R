# Establecer directorio de trabajo
setwd("/Users/josegamo/Documents/Quantiitative Finance/TFM/Datos/procesados")

# Cargar librería necesaria
library(tseries)

# Listar archivos de retornos
archivos <- list.files(pattern = "_ret\\.csv$")

# Inicializar lista de resultados
resultados <- data.frame(
  Activo = character(),
  Estadistico = numeric(),
  Pvalor = numeric(),
  Concluye_estacionariedad = character(),
  stringsAsFactors = FALSE
)

# Loop sobre los archivos
for (archivo in archivos) {
  datos <- read.csv(archivo)
  names(datos) <- tolower(names(datos))
  serie <- datos$retorno
  serie <- serie[complete.cases(serie)]
  
  resultado <- adf.test(serie)
  
  activos <- tools::file_path_sans_ext(archivo)
  
  decision <- ifelse(resultado$p.value < 0.05, "Sí", "No")
  
  resultados <- rbind(resultados, data.frame(
    Activo = activos,
    Estadistico = round(resultado$statistic, 4),
    Pvalor = round(resultado$p.value, 5),
    Concluye_estacionariedad = decision
  ))
}

# Mostrar resultados por consola
print(resultados)

# Guardar resultados en CSV
write.csv(resultados, file = "resultados_estacionariedad.csv", row.names = FALSE)
# --- Tests de estacionariedad en retornos al cuadrado (retorno2) ---
suppressWarnings(library(tseries))
suppressWarnings(library(urca))

resultados_ret2 <- data.frame(
  Activo = character(),
  Test   = character(),
  Estadistico = numeric(),
  Pvalor = numeric(),
  Decision = character(),
  stringsAsFactors = FALSE
)

for (archivo in archivos) {
  datos <- read.csv(archivo, stringsAsFactors = FALSE)
  names(datos) <- tolower(names(datos))
  serie <- datos$retorno
  serie <- serie[is.finite(serie)]          # limpia NA/NaN/Inf
  ret2  <- serie^2
  
  # ADF (tseries::adf.test) -> H0: raíz unitaria (no estacionaria)
  adf2 <- tryCatch(adf.test(ret2), error = function(e) NULL)
  if (!is.null(adf2)) {
    resultados_ret2 <- rbind(resultados_ret2, data.frame(
      Activo = tools::file_path_sans_ext(archivo),
      Test = "ADF",
      Estadistico = unname(adf2$statistic),
      Pvalor = adf2$p.value,
      Decision = ifelse(adf2$p.value < 0.05, "Rechaza H0 (estacionaria)", "No rechaza H0")
    ))
  }
}

# Guardar resultados
write.csv(resultados_ret2, "estacionariedad_retornos2.csv", row.names = FALSE)

