
# Análisis multifractal para la varianza (retornos al cuadrado)

# Cargar librerías necesarias
library(zoo)

# Definir función MF-DFA corregida (igual que en análisis anterior)
mfdfa <- function(ts, scale = NULL, q = seq(-4, 4, 1), m = 1) {
  if (is.null(scale)) {
    scale <- floor(2^seq(4, log2(length(ts) / 4), length.out = 20))
  }

  Fq <- matrix(NA, nrow = length(q), ncol = length(scale))
  for (s in 1:length(scale)) {
    seg_len <- scale[s]
    N <- floor(length(ts) / seg_len)
    ts_seg <- matrix(ts[1:(N * seg_len)], nrow = seg_len, ncol = N)
    X <- apply(ts_seg, 2, cumsum)
    Y <- apply(X, 2, function(x) {
      fit <- lm(x ~ poly(1:seg_len, m, raw = TRUE))
      res <- x - fitted(fit)
      return(mean(res^2))
    })

    for (j in 1:length(q)) {
      if (q[j] == 0) {
        Fq[j, s] <- exp(0.5 * mean(log(Y)))
      } else {
        Fq[j, s] <- (mean(Y^(q[j] / 2)))^(1 / q[j])
      }
    }
  }

  Hq <- numeric(length(q))
  for (j in 1:length(q)) {
    fit <- lm(log(Fq[j, ]) ~ log(scale))
    Hq[j] <- coef(fit)[2]
  }

  list(Hq = round(Hq, 4), q = q, scale = scale, Fq = Fq)
}

# Directorio con los archivos procesados
setwd("/Users/josegamo/Documents/Quantiitative Finance/TFM/Datos/procesados")

# Nombres de archivos
activos <- c("bitcoin", "ibex35", "oro", "sp500", "bovespa", "tesla")

# Parámetros MF-DFA
q_vals <- seq(-4, 4, 1)
s_vals <- floor(2^seq(4, log2(500), length.out = 20))  # para escalas razonables

# Función para analizar cada archivo de retornos al cuadrado
analizar_mfdfa_var <- function(nombre_archivo) {
  datos <- read.csv(paste0(nombre_archivo, "_ret.csv"))
  serie <- as.numeric(na.omit(datos[, 2])^2)  # Retornos al cuadrado
  mfdfa(serie, scale = s_vals, q = q_vals)
}

# Ejecutar análisis para cada activo
resultados_mfdfa_var <- lapply(activos, analizar_mfdfa_var)
names(resultados_mfdfa_var) <- activos

# Guardar resultados
saveRDS(resultados_mfdfa_var, file = "mfdfa_varianza_resultados.rds")
# --- CSVs por activo: tabla h(q)
for (activo in activos) {
  tab <- data.frame(q = resultados_mfdfa_var[[activo]]$q,
                    hq = resultados_mfdfa_var[[activo]]$Hq)
  write.csv(tab, file = paste0(activo, "_mfdfa_varianza_table.csv"),
            row.names = FALSE)
}

# --- CSV consolidado ancho (opcional, solo h(q))
all_q <- resultados_mfdfa_var[[1]]$q
wide <- data.frame(q = all_q)
for (activo in activos) {
  wide[[activo]] <- resultados_mfdfa_var[[activo]]$Hq
}
write.csv(wide, file = "mfdfa_varianza_hq_consolidado.csv", row.names = FALSE)

# Graficar h(q) para cada activo
plot_hq <- function(result, nombre) {
  plot(result$q, result$Hq, type = "b", col = "blue", pch = 19,
       xlab = "q", ylab = "h(q)", main = paste("h(q) -", nombre))
  abline(h = result$Hq[result$q == 2], col = "red", lty = 2)
}

png("mfdfa_varianza_hq.png", width = 1000, height = 800)
par(mfrow = c(3, 2))

for (activo in activos) {
  plot_hq(resultados_mfdfa_var[[activo]], activo)
}
dev.off()
# Gráfico comparativo: todas las curvas h(q) en un único panel (con estilos y leyenda)
png("hq_varianza_plot.png", width = 1000, height = 800)

cols <- 1:6                      # colores base R
ltys <- c(1, 2, 3, 4, 5, 6)      # tipos de línea distintos
pchs <- c(16, 17, 15, 3, 4, 8)   # marcadores distintos

primer <- 1
plot(resultados_mfdfa_var[[activos[primer]]]$q,
     resultados_mfdfa_var[[activos[primer]]]$Hq,
     type = "l", lwd = 2, col = cols[primer], lty = ltys[primer],
     xlab = "q", ylab = "h(q)",
     main = "h(q) comparativo - varianza (retornos^2)")
points(resultados_mfdfa_var[[activos[primer]]]$q,
       resultados_mfdfa_var[[activos[primer]]]$Hq,
       pch = pchs[primer], col = cols[primer])

for (i in 2:length(activos)) {
  lines(resultados_mfdfa_var[[activos[i]]]$q,
        resultados_mfdfa_var[[activos[i]]]$Hq,
        lwd = 2, col = cols[i], lty = ltys[i])
  points(resultados_mfdfa_var[[activos[i]]]$q,
         resultados_mfdfa_var[[activos[i]]]$Hq,
         pch = pchs[i], col = cols[i])
}

legend("bottomleft", legend = activos,
       col = cols, lty = ltys, pch = pchs, lwd = 2, cex = 0.9, bty = "n")

dev.off()
