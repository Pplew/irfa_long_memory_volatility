# irfa_long_memory_volatility

# Replication Package  
## Long-Memory Volatility Modelling: GARCH vs FIGARCH and Risk-Based Evaluation

This repository contains the replication package for the paper:

**Gamo, J. L. (2025)**  
*Modelling long-memory in financial volatility: GARCH vs FIGARCH and risk-based evaluation.*

The aim is to evaluate whether fractional volatility models (FIGARCH) deliver  
structural and practical improvements relative to GARCH, using:

- Daily financial returns (S&P 500, IBEX 35, BOVESPA, Bitcoin, Tesla).  
- Long-memory diagnostics (Hurst exponent, Lo’s modified R/S, MF-DFA).  
- One-step-ahead volatility forecasts.  
- Value-at-Risk (VaR) backtesting (Kupiec).  

All analysis is performed in **R** using only **reproducible, relative paths** (`here` package).  
This package is designed so that a referee can fully replicate the empirical results.

---

## 1. Repository Structure

irfa_long_memory_volatility/
├── code/
│ ├── 01_data_collection/
│ ├── 02_preprocessing/
│ ├── 03_exploration_diagnostics/
│ ├── 04_mean_models_ARIMA_ARFIMA/
│ ├── 05_variance_models_GARCH_FIGARCH/
│ ├── 06_risk_evaluation_VaR/
│ └── 07_robustness_checks/
│
├── data_raw/ # Raw files (not distributed if restricted)
├── data_processed/ # Cleaned returns and MF-DFA summaries
├── replication_data/
│ └── variance_models/ # Main output tables for the paper
│
├── output/
│ ├── figures/
│ ├── tables/
│ └── logs/
│
└── docs/
└── IRFA_version_paper.docx


---

## 2. Software Environment

- **R version:** ≥ 4.2  
- Key packages:
  - `rugarch` — GARCH/FIGARCH estimation  
  - `fracdiff` — ARFIMA(0,d,0) estimation  
  - `forecast` — ARIMA modelling  
  - `PerformanceAnalytics`  
  - `zoo`, `xts`  
  - `here` — relative paths  
  - `data.table` or `dplyr` (optional)

The exact environment used to run the models is stored in:

output/logs/sessionInfo_R.txt


---

## 3. Data

### 3.1 Assets

The analysis uses daily returns for:  
- Bitcoin (BTC/USD)  
- S&P 500  
- IBEX 35  
- BOVESPA  
- Tesla  
- Gold (only for mean models)

### 3.2 Raw Data

Raw price data are retrieved from **Yahoo Finance**.  
Due to licensing restrictions, raw files may not be redistributed.  

The script in:

code/01_data_collection/Data_Collection.R re-downloads all raw series and stores them in:

data_raw/

### 3.3 Processed Data

All scripts consume returns in data_processed/<activo>_ret.csv


Each `<activo>_ret.csv` must contain:

- `fecha` (Date)  
- `retorno` (numeric daily return)

MF-DFA summaries must also be located in:

data_processed/mfdfa_resultados_resumen.csv
data_processed/mfdfa_varianza_resultados_resumen.csv


---

## 4. Code Overview

### 4.1 Mean Models (ARIMA / ARFIMA)

code/04_mean_models_ARIMA_ARFIMA/ARIMA_ARFIMA_comp_completa.R


- Estimates ARIMA and ARFIMA models for all assets.  
- Computes AIC, BIC, RMSE (in-sample / out-of-sample).  
- Gold is included only for the mean, as a complementary diagnostic.

### 4.2 Long-Memory Diagnostics

Scripts in code/03_exploration_diagnostics/ implement:

- Hurst exponent  
- Lo’s modified R/S test  
- MF-DFA on returns  
- MF-DFA on squared returns (variance proxy)

### 4.3 Variance Models (GARCH vs FIGARCH)

**Main script of the replication package:**

code/05_variance_models_GARCH_FIGARCH/Evaluacion_modelos_actualizado_v2.R


This script:

1. Loads all returns  
2. Fits:
   - GARCH(1,1)
   - FIGARCH(1,d,1)
3. Extracts:
   - `d` parameter  
   - AIC / BIC  
   - RMSE of volatility forecasts  
4. Performs 1-step-ahead forecasts  
5. Runs VaR tests (1% & 5%)  
6. Computes robustness of `d` via subsamples  
7. Saves all results to `replication_data/variance_models/`

This is the script a referee only needs to reproduce all volatility results.

### 4.4 Outputs Generated

The script generates:

- **Main comparison table:** in

replication_data/variance_models/resultados_modelos_fraccionales_v2_2_oroSoloMedia.csv


- **Robustness of the FIGARCH parameter d:**. in

replication_data/variance_models/robustez_figarch_d_submuestras.csv


- **Conditional volatility series** for each asset:

replication_data/variance_models/<activo>_volatilidades_condicionales.csv


These outputs correspond exactly to the tables and results presented in the paper.

---

## 5. How to Reproduce the Results

Open the project (`irfa_long_memory_volatility.Rproj`) and run:

```r
source("code/05_variance_models_GARCH_FIGARCH/Evaluacion_modelos_actualizado_v2.R")
````
## 6. License

## 7. Contact

For any issues regarding replication:

Jose L. Gamo
jose.luis.gamo@gmail.com
