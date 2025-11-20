# ==========================================================
# evaluacion_media_completa.R  (CORREGIDO)
# ARIMA vs ARFIMA (inicializado) vs Ruido Blanco en RETORNOS
# - In-sample: AIC, BIC, RMSE
# - Out-of-sample (80/20): RMSE, MAE
# - VaR backtest (1% y 5%) en test (predicción 1-paso); sigma = sd(entrenamiento)
# - Coste computacional (tiempos de estimación)
# - Robustez de d (submuestras 60% inicial y 60% final)
# I S O L A D O : escribe SOLO en Resultados/ARFIMA_inicializado/
# ==========================================================

suppressPackageStartupMessages({
  library(data.table)
  library(forecast)
  library(fracdiff)
})

# --------- utilidades generales ---------
dir_out <- "Resultados/ARFIMA_inicializado"
dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)

norm_key <- function(x) tolower(gsub("[^a-z0-9]", "", trimws(x)))

# Canonización de nombres (alias robustos)
canon_name <- function(x){
  k <- norm_key(x)
  k <- sub("ret$", "", k)                    # quita sufijo _ret si aparece
  k <- gsub("^\\^gspc$|^sandp500$|^snp500$|^spx$|^s&p500$|^snp$", "sp500", k)
  k <- gsub("^bvsp$|^ibovespa$|^bovespaindex$", "bovespa", k)
  k <- gsub("^btc$|^bitcoinusd$|^btcusd$", "bitcoin", k)
  k <- gsub("^gold$|^gc$|^xauusd$", "oro", k)
  k <- gsub("^tsla$|^teslainc$", "tesla", k)
  k <- gsub("^ibex_?35$", "ibex35", k)
  k
}

bic_from_aic <- function(aic_val, n, k) aic_val + (log(n) - 2) * k

aic_bic_fracdiff <- function(fit, n){
  ll  <- suppressWarnings(as.numeric(fit$log.likelihood))
  if (!is.finite(ll)) return(list(AIC=NA_real_, BIC=NA_real_, ll=NA_real_))
  kar <- if (is.null(fit$ar)) 0 else length(fit$ar)
  kma <- if (is.null(fit$ma)) 0 else length(fit$ma)
  k   <- 1 + kar + kma + 1  # d + ar + ma + sigma^2
  list(AIC = -2*ll + 2*k, BIC = -2*ll + log(n)*k, ll = ll)
}

safe_fracdiff <- function(y, dstar){
  lo <- max(-0.49, dstar - 0.05)
  hi <- min( 0.49, dstar + 0.05)
  try(fracdiff(y, nar=0, nma=0, drange=c(lo, hi)), silent = TRUE)
}

eval_rb_fit <- function(y){
  fit <- Arima(y, order = c(0,0,0), include.mean = FALSE)
  n   <- length(y)
  k   <- length(coef(fit)) + 1   # +1 por sigma^2
  aic <- as.numeric(AIC(fit))
  bic <- bic_from_aic(aic, n, k)
  rmse <- sqrt(mean(residuals(fit)^2, na.rm = TRUE))
  list(fit=fit, aic=aic, bic=bic, rmse=rmse)
}

oos_split_idx <- function(n, prop_train = 0.8) {
  n_train <- max(100, floor(n * prop_train))
  list(train = 1:n_train, test = (n_train+1):n)
}

forecast_oos <- function(model_type, y_train, y_test, fit_obj=NULL){
  h <- length(y_test)
  if (h <= 0) return(rep(NA_real_, 0))
  if (model_type == "ARIMA"){
    fit <- if (is.null(fit_obj)) auto.arima(y_train, seasonal=FALSE, stepwise=TRUE, approximation=FALSE) else fit_obj
    as.numeric(forecast::forecast(fit, h=h)$mean)
  } else if (model_type == "ARFIMA"){
    if (!is.null(fit_obj) && inherits(fit_obj, "fracdiff")){
      as.numeric(predict(fit_obj, n.ahead=h)$pred)
    } else {
      rep(0, h) # fallback conservador
    }
  } else if (model_type == "RB"){
    rep(0, h)  # media 0
  } else {
    rep(NA_real_, h)
  }
}

count_var_violations <- function(y_test, mu_hat, sigma_ref, alpha_vec = c(0.01, 0.05)){
  if (!is.finite(sigma_ref) || sigma_ref <= 0) return(c(NA_integer_, NA_integer_))
  z <- qnorm(alpha_vec)
  # VaR unilateral por el lado izquierdo (pérdidas); usamos igual sigma para los 3 modelos
  var_levels <- rbind(mu_hat + z[1]*sigma_ref, mu_hat + z[2]*sigma_ref)
  v1 <- sum(y_test < var_levels[1, ])
  v5 <- sum(y_test < var_levels[2, ])
  c(v1, v5)
}

# --------- leer inicialización ARFIMA (d) ---------
init_path <- file.path(dir_out, "resumen_arfima_inicializado.csv")
if (!file.exists(init_path)) {
  stop("Falta ", init_path, " (necesario para d_inicial/d_perfil)")
}
arf_init_raw <- fread(init_path)

# mapa robusto: clave canonizada
map_init <- arf_init_raw[, .(Activo, d_inicial, d_perfil)]
map_init[, key := canon_name(Activo)]

# --------- datos disponibles ---------
archivos <- list.files("Datos/procesados", pattern = "_ret\\.csv$", full.names = TRUE)

# --------- salidas ---------
out_master <- file.path(dir_out, "evaluacion_media_completa.csv")
out_robust <- file.path(dir_out, "robustez_parametrica_d.csv")

res_master <- data.table(
  Activo = character(),
  # in-sample
  AIC_ARFIMA = numeric(), BIC_ARFIMA = numeric(), RMSE_ARFIMA_in = numeric(),
  AIC_ARIMA  = numeric(), BIC_ARIMA  = numeric(), RMSE_ARIMA_in  = numeric(),
  AIC_RB     = numeric(), BIC_RB     = numeric(), RMSE_RB_in     = numeric(),
  # out-of-sample
  RMSE_ARFIMA_oos = numeric(), MAE_ARFIMA_oos = numeric(),
  RMSE_ARIMA_oos  = numeric(), MAE_ARIMA_oos  = numeric(),
  RMSE_RB_oos     = numeric(), MAE_RB_oos     = numeric(),
  # VaR (test)
  VaR1_ARFIMA = integer(), VaR5_ARFIMA = integer(),
  VaR1_ARIMA  = integer(), VaR5_ARIMA  = integer(),
  VaR1_RB     = integer(), VaR5_RB     = integer(),
  # tiempos
  t_ARFIMA = numeric(), t_ARIMA = numeric(), t_RB = numeric(),
  # d y convergencia
  d_inicial = numeric(), d_perfil = numeric(), d_final = numeric(), valid_loglik = logical()
)

res_robust <- data.table(
  Activo = character(),
  d_final_full = numeric(),
  d_first60 = numeric(), d_last60 = numeric(),
  diff_abs = numeric(), robust_flag = logical()
)

# --------- LOOP PRINCIPAL ---------
for (archivo in archivos){
  base <- tools::file_path_sans_ext(basename(archivo))  # p.ej. "bitcoin_ret"
  vis  <- sub("_ret$", "", base)                        # "bitcoin"
  key_cur <- canon_name(vis)
  
  row_init <- map_init[key == key_cur]
  if (nrow(row_init) != 1L){
    cat("Sin d_inicial/d_perfil para", vis, "(key:", key_cur, ")\n",
        "Claves disponibles en init:\n",
        paste(unique(map_init$key), collapse=", "), "\n")
    next
  }
  d0    <- row_init$d_inicial[1]
  dstar <- if (!is.na(row_init$d_perfil[1])) row_init$d_perfil[1] else d0
  
  dt <- fread(archivo)
  if (!"retorno" %in% names(dt)) { cat("Falta 'retorno' en", archivo, "- salto.\n"); next }
  y <- dt$retorno
  y <- y[is.finite(y)]
  n <- length(y)
  if (n < 300) { cat("Serie corta para", vis, "(n=", n, ") - salto.\n"); next }
  
  idx <- oos_split_idx(n, 0.8)
  y_tr <- y[idx$train]; y_ts <- y[idx$test]
  
  # --- Ruido Blanco ---
  t_rb <- system.time({ rb_fit <- eval_rb_fit(y) })["elapsed"]
  
  # --- ARIMA baseline ---
  t_arima <- system.time({
    fit_arima <- auto.arima(y, max.p=5, max.q=5, max.d=1,
                            stationary=TRUE, seasonal=FALSE, stepwise=TRUE)
    k_arima    <- length(coef(fit_arima)) + 1
    aic_arima  <- as.numeric(AIC(fit_arima))
    bic_arima  <- bic_from_aic(aic_arima, n, k_arima)
    rmse_arima_in <- sqrt(mean(residuals(fit_arima)^2, na.rm=TRUE))
  })["elapsed"]
  
  # --- ARFIMA inicializado ---
  t_arfima <- system.time({
    fd_fit <- if (!is.na(dstar)) safe_fracdiff(y, dstar) else NA
  })["elapsed"]
  
  if (inherits(fd_fit, "try-error") || is.na(fd_fit)[1]){
    aic_fd <- NA_real_; bic_fd <- NA_real_; rmse_fd_in <- NA_real_
    d_final <- NA_real_; valid_ll <- FALSE
  } else {
    ab <- aic_bic_fracdiff(fd_fit, n)
    aic_fd <- ab$AIC; bic_fd <- ab$BIC
    rmse_fd_in <- sqrt(mean(fd_fit$residuals^2, na.rm=TRUE))
    d_final <- as.numeric(fd_fit$d)
    valid_ll <- is.finite(ab$ll)
  }
  
  # --- OUT-OF-SAMPLE (80/20, 1-step ahead) ---
  fit_arima_tr <- auto.arima(y_tr, seasonal=FALSE, stepwise=TRUE, approximation=FALSE)
  f_arima <- forecast_oos("ARIMA", y_tr, y_ts, fit_arima_tr)
  rmse_arima_oos <- sqrt(mean((y_ts - f_arima)^2, na.rm=TRUE))
  mae_arima_oos  <- mean(abs(y_ts - f_arima), na.rm=TRUE)
  
  if (!is.na(d_final)) {
    fd_fit_tr <- safe_fracdiff(y_tr, d_final)
    if (!(inherits(fd_fit_tr, "try-error") || is.na(fd_fit_tr)[1])){
      f_arfima <- forecast_oos("ARFIMA", y_tr, y_ts, fd_fit_tr)
    } else {
      f_arfima <- rep(0, length(y_ts))
    }
  } else {
    f_arfima <- rep(0, length(y_ts))
  }
  rmse_arfima_oos <- sqrt(mean((y_ts - f_arfima)^2, na.rm=TRUE))
  mae_arfima_oos  <- mean(abs(y_ts - f_arfima), na.rm=TRUE)
  
  f_rb <- forecast_oos("RB", y_tr, y_ts, NULL)
  rmse_rb_oos <- sqrt(mean((y_ts - f_rb)^2, na.rm=TRUE))
  mae_rb_oos  <- mean(abs(y_ts - f_rb), na.rm=TRUE)
  
  # --- VaR en test (sigma fija de entrenamiento) ---
  sd_tr <- sd(y_tr, na.rm=TRUE)
  v_arf <- count_var_violations(y_ts, f_arfima, sd_tr, c(0.01, 0.05))
  v_ari <- count_var_violations(y_ts, f_arima,  sd_tr, c(0.01, 0.05))
  v_rb  <- count_var_violations(y_ts, f_rb,     sd_tr, c(0.01, 0.05))
  
  # --- Robustez de d (60% inicial y 60% final) ---
  n60 <- max(200, floor(0.6 * n))
  y_first <- y[1:n60]
  y_last  <- y[(n - n60 + 1):n]
  d_first <- d_last <- NA_real_
  if (!is.na(dstar)) {
    f1 <- safe_fracdiff(y_first, dstar)
    f2 <- safe_fracdiff(y_last,  dstar)
    if (!(inherits(f1, "try-error") || is.na(f1)[1])) d_first <- as.numeric(f1$d)
    if (!(inherits(f2, "try-error") || is.na(f2)[1])) d_last  <- as.numeric(f2$d)
  }
  diff_abs <- if (is.finite(d_first) && is.finite(d_last)) abs(d_first - d_last) else NA_real_
  robust_flag <- if (is.finite(diff_abs)) diff_abs < 0.02 else FALSE
  
  # --- acumular maestro ---
  res_master <- rbind(res_master, data.table(
    Activo = vis,
    AIC_ARFIMA = aic_fd, BIC_ARFIMA = bic_fd, RMSE_ARFIMA_in = rmse_fd_in,
    AIC_ARIMA  = aic_arima, BIC_ARIMA  = bic_arima, RMSE_ARIMA_in  = rmse_arima_in,
    AIC_RB     = rb_fit$aic, BIC_RB     = rb_fit$bic, RMSE_RB_in     = rb_fit$rmse,
    RMSE_ARFIMA_oos = rmse_arfima_oos, MAE_ARFIMA_oos = mae_arfima_oos,
    RMSE_ARIMA_oos  = rmse_arima_oos,  MAE_ARIMA_oos  = mae_arima_oos,
    RMSE_RB_oos     = rmse_rb_oos,     MAE_RB_oos     = mae_rb_oos,
    VaR1_ARFIMA = v_arf[1], VaR5_ARFIMA = v_arf[2],
    VaR1_ARIMA  = v_ari[1], VaR5_ARIMA  = v_ari[2],
    VaR1_RB     = v_rb[1],  VaR5_RB     = v_rb[2],
    t_ARFIMA = as.numeric(t_arfima), t_ARIMA = as.numeric(t_arima), t_RB = as.numeric(t_rb),
    d_inicial = d0, d_perfil = dstar, d_final = d_final, valid_loglik = isTRUE(valid_ll)
  ))
  
  # --- acumular robustez ---
  res_robust <- rbind(res_robust, data.table(
    Activo = vis, d_final_full = d_final,
    d_first60 = d_first, d_last60 = d_last,
    diff_abs = diff_abs, robust_flag = robust_flag
  ))
}

# --------- guardar ----------
fwrite(res_master, out_master)
fwrite(res_robust, out_robust)
cat("\nGuardados:\n - ", out_master, "\n - ", out_robust, "\n")
