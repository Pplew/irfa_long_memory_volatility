# ============================================
# Script: estimacion_arfima_inicializado.R
# Objetivo: Estimar ARFIMA(p,d,q) inicializando d
#           con H(2) del MF-DFA para mejorar convergencia
# Versión IRFA: rutas relativas y sin rm(list=ls())
# ============================================

options(stringsAsFactors = FALSE)

suppressPackageStartupMessages({
  library(data.table)
  library(forecast)
  library(fracdiff)
  library(here)
})

# --- 0) Rutas de salida (replication package) ---
dir_out <- here("replication_data", "mean_models", "ARFIMA_inicializado")
dir.create(dir_out, showWarnings = FALSE, recursive = TRUE)

# --- 1) Cargar H(2) desde MF-DFA ---
# CSV con columnas: Activo, q, hq
mfdfa_path <- here("replication_data", "MF-DFA", "Retornos",
                   "mfdfa_resultados_resumen.csv")

if (!file.exists(mfdfa_path)) {
  stop("No se encuentra el fichero MF-DFA resumen en: ", mfdfa_path,
       "\nEjecuta antes el script de MF-DFA de retornos.")
}

mfdfa_res <- fread(mfdfa_path)
H2_vals   <- mfdfa_res[q == 2, .(Activo, H2 = hq)]

# --- 2) Función de estimación ARFIMA con inicialización desde H(2) ---
estimacion_arfima_inicializada <- function(serie, H2, nar = 1, nma = 1) {
  # Semilla de d a partir de H(2)
  d0 <- max(min(H2 - 0.5, 0.49), -0.49)
  
  # Rejilla corta alrededor de d0
  grid <- d0 + c(-0.10, -0.05, 0, 0.05, 0.10)
  grid <- grid[grid > -0.49 & grid < 0.49]
  
  # Perfil local (usar AIC con k=log(n) como BIC)
  score <- sapply(grid, function(dg) {
    xf <- try(fracdiff::diffseries(serie, dg), silent = TRUE)
    if (inherits(xf, "try-error")) return(Inf)
    xfn <- na.omit(xf)
    fit <- try(forecast::Arima(xfn, order = c(nar, 0, nma), include.mean = TRUE),
               silent = TRUE)
    if (inherits(fit, "try-error")) return(Inf)
    n <- length(xfn)
    as.numeric(AIC(fit, k = log(n)))  # equivalente a BIC
  })
  
  if (all(is.infinite(score))) {
    return(list(
      convergencia = FALSE,
      d_inicial = d0,
      d_perfil  = NA,
      d_final   = NA,
      logLik    = NA
    ))
  }
  
  d_star <- grid[which.min(score)]
  
  # Ajuste final ARFIMA con banda estrecha en d
  fit_fd <- try(
    fracdiff(serie, nar = nar, nma = nma,
             drange = c(d_star - 0.05, d_star + 0.05)),
    silent = TRUE
  )
  
  if (!inherits(fit_fd, "try-error")) {
    list(
      convergencia = TRUE,
      d_inicial    = d0,
      d_perfil     = d_star,
      d_final      = fit_fd$d,
      logLik       = fit_fd$log.likelihood
    )
  } else {
    list(
      convergencia = FALSE,
      d_inicial    = d0,
      d_perfil     = d_star,
      d_final      = NA,
      logLik       = NA
    )
  }
}

# --- 3) Loop sobre activos (usa data_processed del replication package) ---
archivos   <- list.files(
  here("data_processed"),
  pattern = "_ret\\.csv$",
  full.names = TRUE
)

resultados <- list()

for (archivo in archivos) {
  base <- sub("_ret\\.csv$", "", basename(archivo))  # p.ej. "bitcoin"
  cat("\nProcesando:", base, "\n")
  
  # Cargar retornos (columna 'retorno')
  datos <- fread(archivo)
  names(datos) <- tolower(names(datos))
  if (!"retorno" %in% names(datos)) {
    cat("  -> Saltando", base, ": no encuentro columna 'retorno'\n")
    next
  }
  serie <- datos$retorno
  serie <- serie[complete.cases(serie)]
  
  # H(2) para este activo (match exacto con 'Activo' del CSV MF-DFA)
  H2 <- H2_vals[Activo == base, H2]
  if (length(H2) == 0 || is.na(H2)) {
    cat("  -> No hay H2 (q=2) para", base, "- salto.\n")
    next
  }
  
  # Estimación
  res <- estimacion_arfima_inicializada(serie, H2)
  resultados[[base]] <- res
}

# --- 4) Guardar resultados en la carpeta del replication package ---
if (length(resultados) == 0L) {
  stop("No se ha podido estimar ARFIMA para ningún activo.")
}

res_dt <- rbindlist(lapply(names(resultados), function(act) {
  r <- resultados[[act]]
  data.table(
    Activo       = act,
    Convergencia = r$convergencia,
    d_inicial    = r$d_inicial,
    d_perfil     = r$d_perfil,
    d_final      = r$d_final,
    logLik       = r$logLik
  )
}), fill = TRUE)

out_csv <- file.path(dir_out, "resumen_arfima_inicializado.csv")
fwrite(res_dt, out_csv)
cat("\nProceso finalizado. Resultados en:\n  ", out_csv, "\n", sep = "")
print(res_dt)
