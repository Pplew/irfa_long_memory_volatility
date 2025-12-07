# Replication package DOI: https://doi.org/10.5281/zenodo.17771383
# ==========================================================
# run_all_replication.R
#
# Master script for the replication package:
# Runs Blocks 01–07 in sequence, using relative paths (here::here)
#
# IMPORTANT:
# - Block 01 (data collection) uses Yahoo Finance via quantmod.
#   It is set to FALSE by default to avoid unnecessary re-downloads.
# - All other blocks assume that:
#     * data_raw/ contains the raw CSVs (or you run Block 01 first)
#     * data_processed/ is either created by Block 02 or already present
#
# To run everything (except data download), from the project root:
#   source("run_all_replication.R")
# ==========================================================

packages <- c("quantmod","rugarch","fracdiff","forecast","zoo","tidyverse")
new <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new)) install.packages(new)
lapply(packages, library, character.only = TRUE)

suppressPackageStartupMessages({
  library(here)
})

# -------------------------------
# Configuration: which blocks to run
# -------------------------------
run_block_01 <- FALSE   # Data collection from Yahoo (set TRUE if needed)
run_block_02 <- TRUE    # Preprocessing: prices/returns
run_block_03 <- TRUE    # Exploration & diagnostics
run_block_03b <- TRUE   # MF-DFA (returns + variance)
run_block_04 <- TRUE    # Mean models (ARFIMA vs ARIMA vs RB)
run_block_05 <- TRUE    # Variance models (GARCH vs FIGARCH)
run_block_06 <- TRUE    # Risk evaluation (VaR summary)
run_block_07 <- TRUE    # Robustness summary

# -------------------------------
# Small helper to run blocks with messages and error handling
# -------------------------------
run_step <- function(label, expr) {
  cat("\n==================================================\n")
  cat(">>> Running:", label, "\n")
  cat("==================================================\n")
  tryCatch(
    {
      force(expr)
      cat(">>> OK:", label, "\n")
    },
    error = function(e) {
      cat(">>> ERROR in", label, ":\n   ", conditionMessage(e), "\n")
      stop(e)
    }
  )
}

# Ensure we are at the project root (where .Rproj lives)
cat("Project root (here()):\n  ", here(), "\n", sep = "")

# ==========================================================
# Block 01 – Data collection (optional / Yahoo Finance)
# ==========================================================
if (run_block_01) {
  run_step("Block 01 - Data collection (Yahoo Finance)", {
    source(here("code", "01_data_collection", "Data_collection.R"))
  })
} else {
  cat("\n[Skipped] Block 01 - Data collection (run_block_01 = FALSE)\n")
}

# ==========================================================
# Block 02 – Preprocessing (prices/returns)
# ==========================================================
if (run_block_02) {
  run_step("Block 02 - Preprocessing (prices and returns)", {
    source(here("code", "02_preprocessing", "Preprocessing_returns.R"))
  })
} else {
  cat("\n[Skipped] Block 02 - Preprocessing (run_block_02 = FALSE)\n")
}

# ==========================================================
# Block 03 – Exploration & diagnostics
# ==========================================================
if (run_block_03) {
  # 3.1 Exploratory descriptives
  run_step("Block 03.1 - Exploratory descriptives", {
    source(here("code", "03_exploration_diagnostics", "Exploratory_descriptives.R"))
  })
  
  # 3.2 Boxplots + extremos
  run_step("Block 03.2 - Estadísticos y outliers", {
    source(here("code", "03_exploration_diagnostics", "Estadisticos_datos_y_outliers.R"))
  })
  
  # 3.3 Correlations (ACF/PACF)
  run_step("Block 03.3 - Autocorrelaciones", {
    source(here("code", "03_exploration_diagnostics", "Correlaciones.R"))
  })
  
  # 3.4 Stationarity tests (ADF)
  run_step("Block 03.4 - Estacionariedad (ADF)", {
    source(here("code", "03_exploration_diagnostics", "Estacionariedad.R"))
  })
  
  # 3.5 Heteroskedasticity (ARCH LM + var. móvil)
  run_step("Block 03.5 - Heterocedasticidad ARCH", {
    source(here("code", "03_exploration_diagnostics", "Heterocedasticidad.R"))
  })
  
  # 3.6 Hurst (returns)
  run_step("Block 03.6 - Hurst RS sobre retornos", {
    source(here("code", "03_exploration_diagnostics", "Hurst_retornos_RS.R"))
  })
  
  # 3.7 Hurst (variance)
  run_step("Block 03.7 - Hurst RS sobre varianza (ret^2)", {
    source(here("code", "03_exploration_diagnostics", "Hurst_varianza_RS.R"))
  })
  
  # 3.8 Lo test (media y varianza)
  run_step("Block 03.8 - Test de Lo (memoria larga media/varianza)", {
    source(here("code", "03_exploration_diagnostics", "Test_de_Lo.R"))
  })
  
} else {
  cat("\n[Skipped] Block 03 - Exploration & diagnostics (run_block_03 = FALSE)\n")
}

# ==========================================================
# Block 03b – MF-DFA (returns and variance)
# ==========================================================
if (run_block_03b) {
  # 3b.1 MF-DFA on returns -> mfdfa_resultados.rds
  run_step("Block 03b.1 - MF-DFA on returns (RDS)", {
    source(here("code", "03b_mfdfa", "Analysis_mfdfa_ok.R"))
  })
  
  # 3b.2 Process MF-DFA returns -> CSV + plots in replication_data/output
  run_step("Block 03b.2 - Process MF-DFA returns (CSV + figures)", {
    source(here("code", "03b_mfdfa", "procesar_mfdfa_retornos.R"))
  })
  
  # 3b.3 MF-DFA on variance (ret^2) -> RDS + tables
  run_step("Block 03b.3 - MF-DFA on variance (ret^2)", {
    source(here("code", "03b_mfdfa", "mfdfa_varianza_script.R"))
  })
  
  # 3b.4 Process MF-DFA variance -> CSV + plots
  run_step("Block 03b.4 - Process MF-DFA variance (CSV + figures)", {
    source(here("code", "03b_mfdfa", "procesar_mfdfa_varianza.R"))
  })
  
} else {
  cat("\n[Skipped] Block 03b - MF-DFA (run_block_03b = FALSE)\n")
}

# ==========================================================
# Block 04 – Mean models (ARIMA vs ARFIMA vs RB)
# ==========================================================
if (run_block_04) {
  # 4.1 ARFIMA initialization using MF-DFA H(2)
  run_step("Block 04.1 - ARFIMA initialization (d from MF-DFA)", {
    source(here("code", "04_mean_models_ARIMA_ARFIMA", "ARFIMA_inicializado.R"))
  })
  
  # 4.2 Full mean-model evaluation (ARIMA, ARFIMA, RB)
  run_step("Block 04.2 - Mean-model evaluation (ARIMA vs ARFIMA vs RB)", {
    source(here("code", "04_mean_models_ARIMA_ARFIMA", "evaluacion_media_completa.R"))
  })
  
} else {
  cat("\n[Skipped] Block 04 - Mean models (run_block_04 = FALSE)\n")
}

# ==========================================================
# Block 05 – Variance models (GARCH vs FIGARCH)
# ==========================================================
if (run_block_05) {
  run_step("Block 05 - Variance models (GARCH vs FIGARCH)", {
    source(here("code", "05_VARIANCE_MODELS_GARCH_FIGARCH",
                "Evaluacion_modelos_actualizado_v2.R"))
  })
} else {
  cat("\n[Skipped] Block 05 - Variance models (run_block_05 = FALSE)\n")
}

# ==========================================================
# Block 06 – Risk evaluation (VaR backtesting summary)
# ==========================================================
if (run_block_06) {
  run_step("Block 06 - Risk evaluation (VaR summary)", {
    source(here("code", "06_risk_evaluation_VaR", "VaR_backtest_summary.R"))
  })
} else {
  cat("\n[Skipped] Block 06 - Risk evaluation (run_block_06 = FALSE)\n")
}

# ==========================================================
# Block 07 – Robustness checks
# ==========================================================
if (run_block_07) {
  run_step("Block 07 - Robustness summary (ARFIMA + FIGARCH)", {
    source(here("code", "07_robustness_checks", "robustness_summary.R"))
  })
} else {
  cat("\n[Skipped] Block 07 - Robustness checks (run_block_07 = FALSE)\n")
}

cat("\n==================================================\n")
cat(">>> FULL REPLICATION PIPELINE COMPLETED (01–07)\n")
cat("==================================================\n")
