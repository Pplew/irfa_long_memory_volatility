# ---------------------------------------------
# Estadisticos_datos_y_outliers.R (etiquetas horizontales)
# ---------------------------------------------
setwd("/Users/josegamo/Documents/Quantiitative Finance/TFM/Datos/crudos")
suppressWarnings(library(zoo))

# --- Parámetros ---
activos <- c("sp500", "ibex35", "bovespa", "oro", "bitcoin", "tesla")
umbral_extremo <- 0.20
dir_desc  <- "../descriptivos"
dir_plots <- file.path(dir_desc, "plots")
dir.create(dir_desc,  showWarnings = FALSE, recursive = TRUE)
dir.create(dir_plots, showWarnings = FALSE, recursive = TRUE)

# --- Utilidad para leer retornos ---
read_retornos_df <- function(path) {
  df <- read.csv(path, stringsAsFactors = FALSE)
  names(df) <- tolower(names(df))
  if (!all(c("fecha","retorno") %in% names(df))) {
    stop("Estructura inesperada en ", path, " (se esperan columnas: fecha, retorno).")
  }
  df$fecha   <- as.Date(df$fecha)
  df$retorno <- as.numeric(df$retorno)
  df[is.finite(df$retorno) & !is.na(df$fecha), c("fecha","retorno")]
# Devuelve un dataframe limpio con dos columnas (fecha, retorno), 
# eliminando cualquier fila con retorno no válido o con fecha ausente.
}

# ---------------------------------------------------
# 1) BOXPLOT COMPARATIVO (todos los activos, base R)
#     - etiquetas horizontales en eje X
#     - ylim = cuantiles 1–99% extendidos para Tesla
#     - outliers visibles (puntos pequeños)
# ---------------------------------------------------
retornos_stack <- data.frame()
for (a in activos) {
  path <- paste0("../procesados/", a, "_ret.csv")
  if (!file.exists(path)) next
  df <- read_retornos_df(path)
  if (nrow(df) == 0) next
  retornos_stack <- rbind(
    retornos_stack,
    data.frame(fecha = df$fecha, retorno = df$retorno, activo = toupper(a),
               stringsAsFactors = FALSE)
  )
}

if (nrow(retornos_stack) > 0) {
  # Límites base por cuantiles 1–99% globales
  q_all <- as.numeric(quantile(retornos_stack$retorno, c(0.01, 0.99), na.rm = TRUE))
  # Extender para incluir min/max reales de TESLA si existe
  tesla_vals <- subset(retornos_stack, activo == "TESLA")$retorno
  if (length(tesla_vals) > 0) {
    tesla_min <- suppressWarnings(min(tesla_vals, na.rm = TRUE))
    tesla_max <- suppressWarnings(max(tesla_vals, na.rm = TRUE))
    ylim_use  <- c(min(q_all[1], tesla_min), max(q_all[2], tesla_max))
  } else {
    ylim_use <- q_all
  }
#necesario para compatibilizar en el mismo gráfico Tesla (retornos extremos) y resto de activos
  
  # Ordenar y preparar etiquetas (mantener orden alfabético simple)
  labs <- sort(unique(retornos_stack$activo))
  retornos_stack$activo <- factor(retornos_stack$activo, levels = labs)
  
  png(file.path(dir_plots, "boxplot_retornos.png"), width = 1400, height = 800, res = 150)
  par(mar = c(6, 4, 2, 1), cex.axis = 0.9)  # margen inferior estándar
  
  boxplot(
    retorno ~ activo,
    data    = retornos_stack,
    outline = TRUE,                 # mostrar outliers
    range   = 1.5,                  # whiskers canónicos
    ylim    = ylim_use,
    xaxt    = "n",                  # no dibujar eje X automático
    main    = "Distribución de retornos diarios por activo",
    ylab    = "Retorno logarítmico diario",
    pars    = list(
      outpch = 20,                  # puntos sólidos
      outcex = 0.4,                 # outliers pequeños
      boxwex = 0.6,
      staplewex = 0.5
    )
  )
  # Etiquetas HORIZONTALES
  axis(1, at = seq_along(labs), labels = labs, las = 1, cex.axis = 0.9)
  abline(h = 0, lty = 2, col = "gray40")
  dev.off()
  
  message("Boxplot guardado en: ", file.path(dir_plots, "boxplot_retornos.png"))
} else {
  message("No hay datos de retornos para generar el boxplot.")
}

# ---------------------------------------------------
# 2) EXTREMOS: tabla (CSV) + scatter (base R)
# ---------------------------------------------------
extremos_lista <- list()
for (a in activos) {
  path <- paste0("../procesados/", a, "_ret.csv")
  if (!file.exists(path)) next
  df <- read_retornos_df(path)
  if (nrow(df) == 0) next
  sub <- df[abs(df$retorno) > umbral_extremo, , drop = FALSE]
  if (nrow(sub) > 0) {
    extremos_lista[[a]] <- data.frame(
      Fecha = sub$fecha,
      Retorno = sub$retorno,
      Activo = toupper(a),
      stringsAsFactors = FALSE
    )
  }
}

if (length(extremos_lista) > 0) {
  tabla_extremos <- do.call(rbind, extremos_lista)
  rownames(tabla_extremos) <- NULL
  write.csv(tabla_extremos, file = file.path(dir_desc, "retornos_extremos.csv"), row.names = FALSE)
  message("Extremos guardados en: ", file.path(dir_desc, "retornos_extremos.csv"))
  
  png(file.path(dir_plots, "extremos_scatter.png"), width = 1600, height = 800, res = 150)
  par(mar = c(5,4,2,1))
  cols <- as.factor(tabla_extremos$Activo)
  plot(tabla_extremos$Fecha, tabla_extremos$Retorno, pch = 16,
       xlab = "Fecha", ylab = "Retorno",
       main = paste0("Eventos extremos por activo (|r| > ", umbral_extremo, ")"),
       col = cols)
  abline(h = 0, lty = 2, col = "gray40")
  legend("topright", legend = levels(cols), col = 1:length(levels(cols)), pch = 16, cex = 0.8)
  dev.off()
  message("Scatter de extremos guardado en: ", file.path(dir_plots, "extremos_scatter.png"))
} else {
  message("No se detectaron retornos extremos con umbral ±", umbral_extremo)
}

message("== Fin del script ==")
