# =====================================
# mfdfa_varianza_script.R (replication-ready)
# MF-DFA aplicado a la varianza (retornos^2)
# Input:  data_processed/<activo>_ret.csv
# Output: data_processed/mfdfa_varianza_resultados.rds
# =====================================

library(zoo)
library(here)

# --- Función MF-DFA (tu versión corregida) ---
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
      mean(res^2)
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

# --- Activos a analizar (varianza) ---
activos <- c("bitcoin", "ibex35", "oro", "sp500", "bovespa", "tesla")

# Parámetros MF-DFA
q_vals <- seq(-4, 4, 1)
s_vals <- floor(2^seq(4, log2(500), length.out = 20))  # mismo criterio que tu script

# Función para analizar cada activo (retornos^2)
analizar_mfdfa_var <- function(nombre_activo) {
  ruta_csv <- here("data_processed", paste0(nombre_activo, "_ret.csv"))
  
  if (!file.exists(ruta_csv)) {
    warning("No se encuentra el archivo de retornos: ", ruta_csv,
            " — se omite ", nombre_activo)
    return(NULL)
  }
  
  datos <- read.csv(ruta_csv)
  serie_ret <- as.numeric(na.omit(datos[, 2]))
  serie_var <- serie_ret^2  # retornos al cuadrado
  
  mfdfa(serie_var, scale = s_vals, q = q_vals)
}

# --- Ejecutar análisis para cada activo ---
resultados_mfdfa_var <- lapply(activos, analizar_mfdfa_var)
names(resultados_mfdfa_var) <- activos

# Filtrar posibles NULL
resultados_mfdfa_var <- resultados_mfdfa_var[!vapply(resultados_mfdfa_var, is.null, logical(1))]

# Guardar resultados RDS en data_processed
ruta_rds <- here("data_processed", "mfdfa_varianza_resultados.rds")
saveRDS(resultados_mfdfa_var, file = ruta_rds)
cat("Resultados MF-DFA (varianza) guardados en:", ruta_rds, "\n")

# (Opcional) CSVs por activo y consolidado (si quieres mantenerlos)
# ---------------------------------------------------------
for (activo in names(resultados_mfdfa_var)) {
  tab <- data.frame(
    q  = resultados_mfdfa_var[[activo]]$q,
    hq = resultados_mfdfa_var[[activo]]$Hq
  )
  ruta_tab <- here("data_processed", paste0(activo, "_mfdfa_varianza_table.csv"))
  write.csv(tab, file = ruta_tab, row.names = FALSE)
}

if (length(resultados_mfdfa_var) > 0) {
  all_q <- resultados_mfdfa_var[[1]]$q
  wide <- data.frame(q = all_q)
  for (activo in names(resultados_mfdfa_var)) {
    wide[[activo]] <- resultados_mfdfa_var[[activo]]$Hq
  }
  ruta_wide <- here("data_processed", "mfdfa_varianza_hq_consolidado.csv")
  write.csv(wide, file = ruta_wide, row.names = FALSE)
}
# ---------------------------------------------------------

cat("mfdfa_varianza_script.R completado.\n")
