# French Economy Business Cycle and GDP Forecasting

[cite_start]This repository contains the data, models, and methodology used to construct Composite Leading Indicators (CLI) and Composite Coincident Indicators (CCI) for the French economy[cite: 39, 132]. [cite_start]The ultimate goal of this project is to track French business cycles and forecast quarterly and annual GDP growth using advanced econometric and time-series techniques[cite: 132, 133].

**Note on Code Usage:** The provided R scripts and models are hardcoded for the specific dataset included in this repository. If you intend to use this code for other datasets, you will need to adapt the data ingestion and variable selection steps accordingly.

## Project Overview

[cite_start]The French economy is the second largest in the Eurozone, heavily driven by services and a highly competitive industrial sector[cite: 317, 318]. Tracking its business cycle accurately is crucial for economic analysis. 

This project undertakes a comprehensive methodology to:
1. [cite_start]Extract the underlying trends from monthly economic indicators[cite: 14].
2. [cite_start]Classify variables as leading or coincident based on turning points and cross-correlation[cite: 20].
3. [cite_start]Extract common non-stationary factors to build the CLI and CCI[cite: 40].
4. [cite_start]Forecast GDP using Factor Linear Dynamic Regressions and compare out-of-sample performance against benchmark Autoregressive (AR) models[cite: 133, 134].

## Data Sources

[cite_start]The dataset comprises monthly indicators capturing key dimensions of French economic activity[cite: 4]. Data was sourced from:
* [cite_start]**INSEE:** Started dwellings, hotel occupancy rates, and air passengers[cite: 349].
* [cite_start]**OECD:** Manufacturing production/employment, services employment, and demand evolution[cite: 351].
* [cite_start]**Eurostat:** Quarterly National Accounts and initial monthly confidence indicators[cite: 348].

### Selected Indicators
* [cite_start]**Leading:** Started Dwellings (13-month lead), Air Passengers (14-month lead), Services Confidence (4-month lead), and Manufacturing Employment (9-month lead)[cite: 28, 29, 30, 31, 32, 33].
* [cite_start]**Coincident:** HICP (1-month lead), Order Book Levels (2-month lead), Construction Confidence (1-month lead), and Export Order Book Levels (0-month lead)[cite: 35, 36, 37, 38].

## Methodology

### 1. Trend Extraction and Linearization
* [cite_start]**Models:** Linear Dynamic Harmonic Regression (LDHR) and the Basic Structural Model (BSM) were used to compute trends[cite: 15].
* [cite_start]**Linearization:** TRAMO was applied to linearize variables only when it significantly improved the smoothness of the trend[cite: 16, 17]. 

### 2. Composite Indicator Construction
* [cite_start]Built using a non-stationary dynamic factor model following Bujosa, Garcia-Ferrer, and de Juan (2013)[cite: 40, 358].
* [cite_start]The covariance matrix was weighted by a scaling factor to neutralize explosive trends, converging to a random matrix with pervasive eigenvalues[cite: 53, 55]. [cite_start]The first non-stationary factor became the Composite Indicator[cite: 56].

### 3. Diagnostics
* [cite_start]**Factor Extraction:** Bartlett's sphericity test confirmed strong common factors among the selected variables (p-value = 0.00)[cite: 87, 91].
* [cite_start]**Anticipation:** Cross-Correlation Functions (CCF) and Granger Causality tests confirmed that the CLI reliably leads the CCI by 4 months (r = 0.659)[cite: 101, 104, 105].

### 4. GDP Forecasting Models
Four models were tested for in-sample fit and out-of-sample forecasting:
* [cite_start]**M1:** Benchmark AR(4) model[cite: 135, 136].
* [cite_start]**M2:** Factor Linear Dynamic Regression (GDP growth explained by CLI lags)[cite: 149].
* [cite_start]**M3:** Full Specification (M2 augmented with a COVID-19 shock dummy)[cite: 157].
* [cite_start]**M4:** Full Specification + AR component[cite: 167].

## Key Results

* [cite_start]**Leading Power:** The constructed CLI acts as a solid 4-month early warning system for the broader economy (CCI)[cite: 105].
* [cite_start]**Forecasting Accuracy:** Evaluated using RMSE, MAPE, and the Diebold-Mariano test[cite: 188, 193]. [cite_start]While the Diebold-Mariano test showed no statistically significant differences between the forecast errors of the models, the Factor Linear Dynamic Regression (M2) and the Full Specification (M3) clearly outperformed the benchmark AR models for annual GDP forecasting[cite: 223, 224, 243, 246].

## References

* [cite_start]Bartlett, M. S. (1950): Tests of significance in factor analysis[cite: 354].
* [cite_start]Bujosa, M., Garcia-Ferrer, A., & Young, P. C. (2007): Linear dynamic harmonic regression[cite: 356].
* [cite_start]Bujosa, M., Garcia-Ferrer, A., & de Juan, A. (2013): Non-stationary Dynamic Factor Model[cite: 358].
* [cite_start]Bujosa, M., Garcia-Ferrer, A., de Juan, A., & Martin Arroyo, A. (2018): Factor Extraction diagnostics and Anticipation diagnostics[cite: 360].
* [cite_start]Diebold, F. X., & Mariano, R. S. (1995): Comparing Predictive Accuracy[cite: 362].
* [cite_start]Gomez, V., & Maravall, A. (1996): Programs TRAMO and SEATS[cite: 364].
* [cite_start]Granger, C. W. J. (1969): Investigating Causal Relations by Econometric Models and Cross-spectral Methods[cite: 366].
* [cite_start]Harvey, A. C. (1990): Forecasting, Structural Time Series Models and the Kalman Filter[cite: 368].
* [cite_start]Hodrick, R. J., & Prescott, E. C. (1997): Postwar U.S. Business Cycles: An Empirical Investigation[cite: 370].
* [cite_start]Pena, D., & Poncela, P. (2006): Convergence of the weighted covariance matrix[cite: 372].
* [cite_start]Young, P. C., Pedregal, D. J., & Tych, W. (1999): Dynamic harmonic regression[cite: 374].
* [cite_start]Institutions: Eurostat, INSEE, OECD, CEPR, NBER, and the French Business Cycle Dating Committee[cite: 348, 349, 351, 352, 353].
