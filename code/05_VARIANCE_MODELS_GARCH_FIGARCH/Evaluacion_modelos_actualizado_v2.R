# =====================================
# SCRIPT DE EVALUACIÓN PARA TODOS LOS ACTIVOS — v2.2 (arreglos) + Oro solo media
# Versión replication package con rutas relativas, salida ordenada y robustez separada
# =====================================

library(rugarch)
library(forecast)
library(fracdiff)
library(PerformanceAnalytics)
library(zoo)
library(here)   # CHANGE: rutas relativas

# -------------------------
# PARÁMETROS
# -------------------------
activos <- c("bitcoin", "sp500", "ibex35", "oro", "tesla", "bovespa")

# CHANGE: base_path apunta a data_processed del proyecto
base_path <- here("data_processed")

resultado <- data.frame()

# CHANGE: Oro solo media
activos_media <- activos
activos_var   <- setdiff(activos, "oro")

# -------------------------
# FUNCIONES AUXILIARES
# -------------------------
VaR_test <- function(realized, VaR, alpha) {
  x <- sum(realized < -VaR)
  T <- length(realized)
  p_hat <- x / T
  LR <- -2 * (log(((1 - alpha)^(T - x)) * (alpha^x)) -
                log(((1 - p_hat)^(T - x)) * (p_hat^x)))
  pval <- 1 - pchisq(LR, df = 1)
  return(c(violaciones = x, pvalue = pval))
}

.rmse <- function(y, yhat) sqrt(mean((y - yhat)^2, na.rm = TRUE))

.aic_bic_rb <- function(x) {
  x <- stats::na.omit(as.numeric(x))
  n <- length(x)
  if (n < 3) return(list(AIC_RB = NA_real_, BIC_RB = NA_real_))
  s2 <- stats::var(x)
  loglik <- -0.5 * n * (log(2*pi*s2) + 1)
  k <- 1
  list(AIC_RB = -2*loglik + 2*k, BIC_RB = -2*loglik + k*log(n))
}

.ic_from_arima <- function(fit, n) {
  ll <- as.numeric(logLik(fit)); k <- attr(logLik(fit), "df")
  list(AIC = -2*ll + 2*k, BIC = -2*ll + k*log(n))
}

.model_str_from_arima <- function(fit) {
  if (is.null(fit)) return(NA_character_)
  ord <- forecast::arimaorder(fit)
  paste0("ARIMA(", ord["p"], ",", ord["d"], ",", ord["q"], ")")
}

.ic_from_ugarchfit <- function(fit, n) {
  if (is.null(fit)) return(list(AIC=NA_real_, BIC=NA_real_))
  ic <- tryCatch(infocriteria(fit), error=function(e) NULL)
  if (!is.null(ic) && is.finite(ic["Akaike"]) && is.finite(ic["Bayes"])) {
    return(list(AIC=as.numeric(ic["Akaike"]), BIC=as.numeric(ic["Bayes"])))
  }
  ll <- tryCatch(likelihood(fit), error=function(e) NA_real_)
  k  <- tryCatch(length(coef(fit)),  error=function(e) NA_integer_)
  if (!is.finite(ll) || !is.finite(k) || !is.finite(n)) return(list(AIC=NA_real_, BIC=NA_real_))
  list(AIC = -2*ll + 2*k, BIC = -2*ll + k*log(n))
}

estimar_arfima_fracdiff <- function(serie) {
  d <- NA_real_; aic <- NA_real_; bic <- NA_real_
  x <- as.numeric(serie); x <- x[is.finite(x)]
  if (length(x) < 10) return(list(d = d, aic = aic, bic = bic))
  x <- ts(x - mean(x, na.rm = TRUE))
  fit <- tryCatch(fracdiff(x, nar = 0, nma = 0), error = function(e) NULL)
  if (is.null(fit)) return(list(d = d, aic = aic, bic = bic))
  if (!is.null(fit$d)) d <- as.numeric(fit$d)
  if (!is.null(fit$aic) && is.finite(fit$aic)) aic <- as.numeric(fit$aic)
  if (!is.null(fit$bic) && is.finite(fit$bic)) bic <- as.numeric(fit$bic)
  if (!is.na(aic) && !is.na(bic)) return(list(d = d, aic = aic, bic = bic))
  n <- length(x)
  ll <- if (!is.null(fit$log.likelihood) && is.finite(fit$log.likelihood)) {
    as.numeric(fit$log.likelihood)
  } else {
    sse <- NA_real_
    for (cand in c(fit$ssd, fit$SSE, fit$RSS)) if (!is.null(cand) && is.finite(cand)) { sse <- as.numeric(cand); break }
    if (is.na(sse)) sse <- sum(as.numeric(x)^2)
    sigma2 <- sse / n
    -0.5 * n * (log(2*pi*sigma2) + 1)
  }
  k <- 1L
  list(d = d, aic = -2*ll + 2*k, bic = -2*ll + k*log(n))
}

.extract_d_figarch <- function(fit_primary, fit_backup=NULL) {
  get_d <- function(fit) {
    if (is.null(fit)) return(NA_real_)
    cf <- tryCatch(coef(fit), error=function(e) NULL)
    if (!is.null(cf)) {
      nms <- tolower(names(cf))
      pos <- which(nms %in% c("d","delta","fi"))
      if (length(pos)) return(as.numeric(cf[pos[1]]))
    }
    mc <- tryCatch(fit@fit$matcoef, error=function(e) NULL)
    if (!is.null(mc)) {
      rn <- tolower(rownames(mc))
      pos <- which(rn %in% c("d","delta","fi") | grepl("^d($|[^a-z])", rn))
      if (length(pos)) return(as.numeric(mc[pos[1],1]))
    }
    pars <- tryCatch(fit@model$pars, error=function(e) NULL)
    if (!is.null(pars)) {
      rn <- tolower(rownames(pars))
      pos <- which(rn %in% c("d","delta","fi"))
      if (length(pos)) {
        cf2 <- tryCatch(coef(fit), error=function(e) NULL)
        if (!is.null(cf2)) {
          idx <- tryCatch(pars[pos[1], "idx"], error=function(e) NA)
          if (is.finite(idx)) {
            vals <- as.numeric(cf2)
            if (idx >= 1 && idx <= length(vals)) return(vals[idx])
          }
        }
      }
    }
    NA_real_
  }
  d <- get_d(fit_primary)
  if (!is.finite(d)) d <- get_d(fit_backup)
  d
}

.get_hq2 <- function(df_mfdfa, activo_chr) {
  if (is.null(df_mfdfa) || nrow(df_mfdfa)==0) return(NA_real_)
  df <- df_mfdfa
  colnames(df) <- tolower(colnames(df))
  if (!all(c("activo","q","hq") %in% colnames(df))) return(NA_real_)
  df$activo <- tolower(as.character(df$activo))
  cand <- df[df$activo == tolower(activo_chr), ]
  if (nrow(cand) == 0) return(NA_real_)
  if (any(cand$q == 2)) {
    return(as.numeric(cand[cand$q==2, "hq"][1]))
  } else {
    j <- which.min(abs(cand$q - 2))
    return(as.numeric(cand$hq[j]))
  }
}

# -------------------------
# BUCLE PRINCIPAL
# -------------------------
for (activo in activos) {
  cat("Procesando", activo, "\n")
  
  # CHANGE: ruta robusta
  ruta_csv <- file.path(base_path, paste0(activo, "_ret.csv"))
  if (!file.exists(ruta_csv)) {
    warning("No existe el fichero de retornos: ", ruta_csv, 
            " — se omite ", activo)
    next
  }
  datos <- read.csv(ruta_csv)
  ret <- ts(datos$retorno)
  
  # Oro solo media
  solo_media <- !(activo %in% activos_var)
  
  # --- MEDIA ---
  fit_arima <- tryCatch(
    auto.arima(
      y = as.numeric(ret), seasonal = FALSE,
      stepwise = TRUE, approximation = FALSE,
      max.p = 5, max.q = 5, max.d = 1,
      allowdrift = FALSE, allowmean = FALSE
    ),
    error = function(e) NULL
  )
  modelo_arima_chr <- .model_str_from_arima(fit_arima)
  if (!is.null(fit_arima)) {
    ic_arima <- .ic_from_arima(fit_arima, n = length(na.omit(ret)))
    aic_arima <- ic_arima$AIC; bic_arima <- ic_arima$BIC
    fitted_vals <- tryCatch(as.numeric(fitted(fit_arima)), error = function(e) NA)
    if (length(fitted_vals) != length(ret) || any(is.na(fitted_vals))) {
      fitted_vals <- as.numeric(ret) - as.numeric(residuals(fit_arima))
    }
    rmse_arima <- .rmse(as.numeric(ret), fitted_vals)
  } else {
    aic_arima <- NA_real_; bic_arima <- NA_real_; rmse_arima <- NA_real_
  }
  rb_info <- .aic_bic_rb(ret)
  aic_rb <- rb_info$AIC_RB; bic_rb <- rb_info$BIC_RB
  rmse_rb <- sqrt(var(as.numeric(ret), na.rm = TRUE))
  
  # --- VARIANZA ---
  if (!solo_media) {
    spec_garch <- ugarchspec(
      variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
      mean.model     = list(armaOrder = c(0,0), include.mean = FALSE),
      distribution.model = "norm"
    )
    spec_figarch <- ugarchspec(
      variance.model = list(model = "fiGARCH", garchOrder = c(1,1)),
      mean.model     = list(armaOrder = c(0,0), include.mean = FALSE),
      distribution.model = "norm"
    )
    
    fit_garch   <- tryCatch(ugarchfit(spec = spec_garch,   data = ret, solver="hybrid"), error = function(e) NULL)
    fit_figarch <- tryCatch(ugarchfit(spec = spec_figarch, data = ret, solver="hybrid"), error = function(e) NULL)
    
    n_eff <- length(na.omit(ret))
    ics_g <- .ic_from_ugarchfit(fit_garch,   n_eff)
    ics_f <- .ic_from_ugarchfit(fit_figarch, n_eff)
    aic_garch <- ics_g$AIC;  bic_garch <- ics_g$BIC
    aic_figarch <- ics_f$AIC; bic_figarch <- ics_f$BIC
    
    # CHANGE: MF-DFA varianza desde data_processed
    ruta_mfdfa_var <- file.path(base_path, "mfdfa_varianza_resultados_resumen.csv")
    mfdfa_var <- tryCatch(read.csv(ruta_mfdfa_var), error=function(e) NULL)
    hq2_var <- .get_hq2(mfdfa_var, activo)
    d_mfdfa_var <- if (!is.na(hq2_var)) hq2_var - 0.5 else NA_real_
    
    d_figarch_full <- .extract_d_figarch(fit_figarch, NULL)
    
    # Rolling d
    ventana <- floor(length(ret) * 0.70)
    paso <- max(1, floor((length(ret) - ventana) / 4))
    d_vals <- rep(NA_real_, 5)
    for (i in 0:4) {
      idx_ini <- 1 + i * paso
      idx_fin <- idx_ini + ventana - 1
      if (idx_fin > length(ret)) break
      subserie <- as.numeric(ret[idx_ini:idx_fin])
      aj <- tryCatch(ugarchfit(spec_figarch, data = subserie, solver="hybrid"), error=function(e) NULL)
      if (is.null(aj)) aj <- tryCatch(ugarchfit(spec_figarch, data = subserie, solver="nlminb"), error=function(e) NULL)
      if (is.null(aj)) aj <- tryCatch(ugarchfit(spec_figarch, data = subserie, solver="solnp", solver.control=list(trace=0)), error=function(e) NULL)
      if (!is.null(aj)) {
        d_tmp <- .extract_d_figarch(aj, NULL)
        if (is.finite(d_tmp)) d_vals[i+1] <- d_tmp
      }
    }
    
    # Forecast 1-step OOS
    n <- length(ret); n_train <- floor(n * 0.9); ret_test <- ret[(n_train+1):n]
    fit_garch_train   <- tryCatch(ugarchfit(spec_garch,  data = ret, out.sample = length(ret_test), solver="hybrid"), error = function(e) NULL)
    fit_figarch_train <- tryCatch(ugarchfit(spec_figarch, data = ret, out.sample = length(ret_test), solver="hybrid"), error = function(e) NULL)
    d_figarch <- if (is.finite(d_figarch_full)) d_figarch_full else .extract_d_figarch(fit_figarch_train, NULL)
    
    fc_garch <- if (!is.null(fit_garch_train))  tryCatch(ugarchforecast(fit_garch_train,  n.ahead = 1, n.roll = length(ret_test) - 1), error=function(e) NULL) else NULL
    fc_figarch<- if (!is.null(fit_figarch_train)) tryCatch(ugarchforecast(fit_figarch_train, n.ahead = 1, n.roll = length(ret_test) - 1), error=function(e) NULL) else NULL
    sigma_garch <- if (!is.null(fc_garch))  as.numeric(sigma(fc_garch))  else rep(NA_real_, length(ret_test))
    sigma_figarch<- if (!is.null(fc_figarch)) as.numeric(sigma(fc_figarch)) else rep(NA_real_, length(ret_test))
    realized_vol <- abs(ret_test)
    
    # CHANGE: guardar volatilidades condicionales en replication_data/variance_models
    vol_series <- data.frame(
      fecha = tail(datos$fecha, length(sigma_garch)),
      volatilidad_empirica = realized_vol,
      sigma_garch = sigma_garch,
      sigma_figarch = sigma_figarch
    )
    write.csv(
      vol_series,
      here("replication_data", "variance_models", paste0(activo, "_volatilidades_condicionales.csv")),
      row.names = FALSE
    )
    
    rmse_garch <- sqrt(mean((realized_vol - sigma_garch)^2, na.rm = TRUE))
    rmse_figarch <- sqrt(mean((realized_vol - sigma_figarch)^2, na.rm = TRUE))
    
    var1_g <- VaR_test(ret_test, -qnorm(0.01)*sigma_garch, 0.01)
    var1_f <- VaR_test(ret_test, -qnorm(0.01)*sigma_figarch, 0.01)
    var5_g <- VaR_test(ret_test, -qnorm(0.05)*sigma_garch, 0.05)
    var5_f <- VaR_test(ret_test, -qnorm(0.05)*sigma_figarch, 0.05)
    
    t_garch <- tryCatch(system.time(ugarchfit(spec_garch, data = ret, solver="hybrid"))["elapsed"], error = function(e) NA)
    t_figarch<- tryCatch(system.time(ugarchfit(spec_figarch, data = ret, solver="hybrid"))["elapsed"], error = function(e) NA)
    
  } else {
    aic_garch <- bic_garch <- aic_figarch <- bic_figarch <- NA_real_
    d_mfdfa_var <- NA_real_
    d_figarch <- NA_real_
    rmse_garch <- rmse_figarch <- NA_real_
    var1_g <- c(violaciones = NA_real_, pvalue = NA_real_)
    var1_f <- c(violaciones = NA_real_, pvalue = NA_real_)
    var5_g <- c(violaciones = NA_real_, pvalue = NA_real_)
    var5_f <- c(violaciones = NA_real_, pvalue = NA_real_)
    t_garch <- t_figarch <- NA_real_
    d_vals <- rep(NA_real_, 5)
  }
  
  # MF-DFA retornos
  ruta_mfdfa_ret <- file.path(base_path, "mfdfa_resultados_resumen.csv")
  mfdfa_ret <- tryCatch(read.csv(ruta_mfdfa_ret), error=function(e) NULL)
  hq2_ret <- .get_hq2(mfdfa_ret, activo)
  d_mfdfa_ret <- if (!is.na(hq2_ret)) hq2_ret - 0.5 else NA_real_
  
  resultado <- rbind(resultado, data.frame(
    Activo = activo,
    Modelo_ARIMA = modelo_arima_chr,
    AIC_ARIMA = if (!is.null(fit_arima)) .ic_from_arima(fit_arima, length(na.omit(ret)))$AIC else NA,
    BIC_ARIMA = if (!is.null(fit_arima)) .ic_from_arima(fit_arima, length(na.omit(ret)))$BIC else NA,
    RMSE_ARIMA = if (!is.null(fit_arima)) .rmse(as.numeric(ret), as.numeric(if (length(fitted(fit_arima))==length(ret)) fitted(fit_arima) else ret - residuals(fit_arima))) else NA,
    AIC_RB = .aic_bic_rb(ret)$AIC_RB, BIC_RB = .aic_bic_rb(ret)$BIC_RB, RMSE_RB = sqrt(var(as.numeric(ret), na.rm = TRUE)),
    d_ARFIMA = estimar_arfima_fracdiff(ret)$d,
    AIC_ARFIMA = estimar_arfima_fracdiff(ret)$aic,
    BIC_ARFIMA = estimar_arfima_fracdiff(ret)$bic,
    d_MFDFA_ret = d_mfdfa_ret,
    d_FIGARCH = d_figarch,
    d_MFDFA_var = d_mfdfa_var,
    RMSE_GARCH = rmse_garch,
    RMSE_FIGARCH = rmse_figarch,
    AIC_GARCH = aic_garch, BIC_GARCH = bic_garch,
    AIC_FIGARCH = aic_figarch, BIC_FIGARCH = bic_figarch,
    VaR1_viol_GARCH = var1_g["violaciones"], VaR1_pval_GARCH = var1_g["pvalue"],
    VaR1_viol_FIGARCH = var1_f["violaciones"], VaR1_pval_FIGARCH = var1_f["pvalue"],
    VaR5_viol_GARCH = var5_g["violaciones"], VaR5_pval_GARCH = var5_g["pvalue"],
    VaR5_viol_FIGARCH = var5_f["violaciones"], VaR5_pval_FIGARCH = var5_f["pvalue"],
    Tiempo_GARCH = t_garch, Tiempo_FIGARCH = t_figarch,
    d_roll_1 = d_vals[1], d_roll_2 = d_vals[2], d_roll_3 = d_vals[3], d_roll_4 = d_vals[4], d_roll_5 = d_vals[5]
  ))
}

# -------------------------
# SALIDAS
# -------------------------

# CHANGE: Resultados principales (un único CSV fijo)
out_file <- here("replication_data", "variance_models",
                 "resultados_modelos_fraccionales_v2_2_oroSoloMedia.csv")
write.csv(resultado, file = out_file, row.names = FALSE)
cat("Resultados guardados en:", out_file, "\n")

# CHANGE: Robustez (d_roll) en CSV independiente
robustez_df <- resultado[, c("Activo", "d_roll_1", "d_roll_2", "d_roll_3", "d_roll_4", "d_roll_5")]
write.csv(
  robustez_df,
  here("replication_data", "variance_models", "robustez_figarch_d_submuestras.csv"),
  row.names = FALSE
)
cat("Robustez FIGARCH guardada en:",
    here("replication_data", "variance_models", "robustez_figarch_d_submuestras.csv"),
    "\n")
