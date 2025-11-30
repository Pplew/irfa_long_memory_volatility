# =====================================
# Exploratory_descriptives.R
# Genera tablas descriptivas y figuras básicas
# Input: data_processed/<activo>_price.csv y <activo>_ret.csv
# Output:
#   - replication_data/exploration/resumen_<activo>.csv
#   - replication_data/exploration/resumen_general.csv
#   - output/figures/exploration/*.png
# =====================================

library(zoo)
library(moments)
library(here)

# -------------------------------------
# Activos
# -------------------------------------
activos <- c("sp500", "ibex35", "bovespa", "oro", "bitcoin", "tesla")

# Crear carpetas de salida si no existen
dir.create(here("replication_data", "exploration"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("output", "figures", "exploration"), recursive = TRUE, showWarnings = FALSE)

# -------------------------------------
# Función de análisis por activo
# -------------------------------------
analizar_activo <- function(nombre) {
  cat("\nAnalizando:", nombre, "\n")
  
  ruta_price <- here("data_processed", paste0(nombre, "_price.csv"))
  ruta_ret   <- here("data_processed", paste0(nombre, "_ret.csv"))
  
  if (!file.exists(ruta_price) || !file.exists(ruta_ret)) {
    cat("  -> Faltan archivos procesados para", nombre, "\n")
    return(NULL)
  }
  
  # Leer series procesadas
  df_price <- read.csv(ruta_price, stringsAsFactors = FALSE)
  df_ret   <- read.csv(ruta_ret,   stringsAsFactors = FALSE)
  
  df_price$fecha <- as.Date(df_price$fecha)
  df_ret$fecha   <- as.Date(df_ret$fecha)
  
  serie_logprecios <- zoo(df_price$log_precio, order.by = df_price$fecha)
  serie_ret        <- zoo(df_ret$retorno,      order.by = df_ret$fecha)
  
  # NA detectados en datos crudos (si queremos aproximar la métrica original)
  ruta_raw <- here("data_raw", paste0(nombre, ".csv"))
  if (file.exists(ruta_raw)) {
    precios_raw <- read.zoo(ruta_raw, sep = ",", header = TRUE, format = "%Y-%m-%d")
    na_detectados <- sum(is.na(precios_raw))
  } else {
    na_detectados <- NA_integer_
  }
  
  retornos <- coredata(serie_ret)
  fechas_ret <- index(serie_ret)
  
  # Estadísticos
  resumen <- data.frame(
    Activo        = nombre,
    Observaciones = length(retornos),
    Desde         = as.character(min(fechas_ret)),
    Hasta         = as.character(max(fechas_ret)),
    NA_detectados = na_detectados,
    Media         = mean(retornos, na.rm = TRUE),
    Desviacion    = sd(retornos,   na.rm = TRUE),
    Min           = min(retornos,  na.rm = TRUE),
    Max           = max(retornos,  na.rm = TRUE),
    Skewness      = skewness(retornos, na.rm = TRUE),
    Kurtosis      = kurtosis(retornos, na.rm = TRUE)
  )
  
  # Guardar tabla individual
  ruta_res_ind <- here("replication_data", "exploration",
                       paste0("resumen_", nombre, ".csv"))
  write.csv(resumen, file = ruta_res_ind, row.names = FALSE)
  cat("  -> Resumen guardado en:", ruta_res_ind, "\n")
  
  # Figuras
  # Precio (a partir de log-precios)
  ruta_fig_precio <- here("output", "figures", "exploration",
                          paste0(nombre, "_precio.png"))
  png(ruta_fig_precio, width = 800, height = 400)
  plot(exp(serie_logprecios),
       main = paste("Precio de cierre -", toupper(nombre)),
       xlab = "Fecha", ylab = "Precio ajustado")
  dev.off()
  cat("  -> Figura precio:", ruta_fig_precio, "\n")
  
  # Retornos en el tiempo
  ruta_fig_ret <- here("output", "figures", "exploration",
                       paste0(nombre, "_retornos.png"))
  png(ruta_fig_ret, width = 800, height = 400)
  plot(serie_ret,
       main = paste("Retornos logarítmicos -", toupper(nombre)),
       xlab = "Fecha", ylab = "Retorno")
  dev.off()
  cat("  -> Figura retornos:", ruta_fig_ret, "\n")
  
  # Histograma de retornos
  ruta_fig_hist <- here("output", "figures", "exploration",
                        paste0(nombre, "_hist_retornos.png"))
  png(ruta_fig_hist, width = 600, height = 400)
  hist(retornos, breaks = 60,
       main = paste("Histograma de retornos -", toupper(nombre)),
       xlab = "Retorno", col = "lightblue", border = "white")
  dev.off()
  cat("  -> Histograma retornos:", ruta_fig_hist, "\n")
  
  cat("Listo.\n")
  resumen
}

# -------------------------------------
# Ejecutar para todos los activos
# -------------------------------------
resumenes <- do.call(rbind, lapply(activos, analizar_activo))

# Guardar resumen general
ruta_res_gen <- here("replication_data", "exploration", "resumen_general.csv")
write.csv(resumenes, file = ruta_res_gen, row.names = FALSE)
cat("\nResumen general guardado en:", ruta_res_gen, "\n")

# ==========================================
# Save R session info for reproducibility
# ==========================================
suppressWarnings(dir.create(here::here("output", "logs"),
                            recursive = TRUE, showWarnings = FALSE))

sink(here::here("output", "logs", "sessionInfo_R.txt"))
cat("R session info for full replication pipeline\n\n")
print(sessionInfo())
sink()

cat("\nExploratory descriptives completado.\n")
