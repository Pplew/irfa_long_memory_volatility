[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.17771383.svg)](https://doi.org/10.5281/zenodo.17771383)

Replication Package

This repository contains the full replication materials for:

Gamo, J. (2025). Long-Memory in Financial Volatility:
A Comparative Analysis of ARIMA/GARCH vs. ARFIMA/FIGARCH Models. Working paper.

It reproduces all empirical results reported in the study, including data preprocessing, diagnostics, long-memory estimation, mean and variance modelling, VaR evaluation, and robustness checks.

1. Quick Start

Run the entire analysis with:

source("run_all_replication.R")


Switches inside the wrapper (run_block_02, run_block_03, …) allow running individual blocks.

2. Repository Structure
code/                     # All R scripts
data_raw/                 # Raw price data (NOT distributed)
data_processed/           # Clean prices and returns
replication_data/         # Numerical outputs used in the paper
output/figures/           # All figures
run_all_replication.R     # Master wrapper

3. Requirements

The analysis uses base R plus:

forecast, tseries, lmtest, FinTS, fracdiff,
rugarch, MFDFA, pracma,
data.table, zoo, xts, here, ggplot2.

Check your environment with:

sessionInfo()

4. Data Policy

Raw Yahoo Finance data cannot be redistributed.
Users must provide price series in data_raw/ or run Block 01 locally.

Processed files are created automatically in data_processed/.

5. Workflow Overview
Block 02 – Preprocessing

Clean prices

Compute log-prices and log-returns

Save to data_processed/<asset>_price.csv and <asset>_ret.csv

Block 03 – Diagnostics

Descriptive statistics

Extreme-return analysis

ACF/PACF of returns and squared returns

ADF tests

ARCH LM tests

Hurst (R/S) and Lo modified R/S tests

Outputs: replication_data/exploration/ and replication_data/diagnostics/

Block 03b – MF-DFA

MF-DFA on returns and on variance (r²)

Summary tables: h(q) for q ∈ {−4,…,+4}

Outputs: replication_data/MF-DFA/

Block 04 – Mean Models

ARFIMA initialisation based on h(2)

Comparison: White Noise vs ARIMA vs ARFIMA

Metrics: AIC, BIC, RMSE, MAE, d-robustness

Outputs: replication_data/mean_models/

Block 05 – Variance Models

GARCH(1,1) vs FIGARCH(1,d,1)

In- and out-of-sample volatility forecasts

VaR(1%,5%) from model-implied σₜ

Rolling d for robustness

Outputs: replication_data/variance_models/

Block 06 – Risk Evaluation

Organisation of VaR results

Violation counts and unconditional coverage

Block 07 – Robustness

Consolidation of ARFIMA and FIGARCH stability metrics

Outputs: replication_data/robustness/

6. Citation

If using this repository, please cite:

Gamo, J. (2025). Long-Memory in Financial Volatility:
A Comparative Analysis of ARIMA/GARCH vs. ARFIMA/FIGARCH Models.
Working paper.


(BibTeX provided below.)

7. Contact

José Luis Gamo
Email:jose.luis.gamo@gmail.com

GitHub: https://github.com/Pplew

8. Acknowledgments

The author thanks José Ignacio Olmeda for guidance, and feedback throughout the development of this research.
