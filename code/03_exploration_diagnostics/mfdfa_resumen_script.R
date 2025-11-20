
# Script para análisis de multifractalidad de los activos

# Librerías necesarias
library(fractaldim)
library(tibble)

# Cargar resultados guardados
setwd("/Users/josegamo/Documents/Quantiitative Finance/TFM/Datos/procesados")
resultados_mfdfa <- readRDS("mfdfa_resultados.rds")

# Función para graficar h(q)
plot_hq <- function(resultados, nombre_activo) {
  if (!nombre_activo %in% names(resultados)) {
    stop(paste("Activo", nombre_activo, "no encontrado."))
  }

  hq <- resultados[[nombre_activo]]$Hq
  q_vals <- resultados[[nombre_activo]]$q

  if (is.null(hq) || is.null(q_vals)) {
    stop("h(q) o q no están definidos para este activo.")
  }

  plot(q_vals, hq, type = "b", pch = 19, col = "blue",
       xlab = "q", ylab = "h(q)",
       main = paste("Multifractalidad -", nombre_activo))
}

# Generar las gráficas
png("bitcoin_hq.png")
plot_hq(resultados_mfdfa, "bitcoin")
dev.off()

png("ibex35_hq.png")
plot_hq(resultados_mfdfa, "ibex35")
dev.off()

png("oro_hq.png")
plot_hq(resultados_mfdfa, "oro")
dev.off()

png("sp500_hq.png")
plot_hq(resultados_mfdfa, "sp500")
dev.off()

png("bovespa_hq.png")
plot_hq(resultados_mfdfa, "bovespa")
dev.off()

png("tesla_hq.png")
plot_hq(resultados_mfdfa, "tesla")
dev.off()

# Exportar resultados numéricos a CSV
hq_summary <- tibble(
  Activo = character(),
  q = numeric(),
  hq = numeric()
)

for (activo in names(resultados_mfdfa)) {
  fila <- tibble(
    Activo = rep(activo, length(resultados_mfdfa[[activo]]$q)),
    q = resultados_mfdfa[[activo]]$q,
    hq = resultados_mfdfa[[activo]]$Hq
  )
  hq_summary <- rbind(hq_summary, fila)
}

write.csv(hq_summary, file = "mfdfa_resultados_resumen.csv", row.names = FALSE)

