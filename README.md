# French Economy Business Cycle and GDP Forecasting

This repository contains the data, models, and methodology used to construct Composite Leading Indicators (CLI) and Composite Coincident Indicators (CCI) for the French economy. The ultimate goal of this project is to track French business cycles and forecast quarterly and annual GDP growth using advanced econometric and time-series techniques.

Note on Code Usage: The provided R scripts and models are hardcoded for the specific dataset included in this repository. If you intend to use this code for other datasets, you will need to adapt the data ingestion and variable selection steps accordingly.

## Project Overview

The French economy is the second largest in the Eurozone, heavily driven by services and a highly competitive industrial sector. Tracking its business cycle accurately is crucial for economic analysis. 

This project undertakes a comprehensive methodology to:
1. Extract the underlying trends from monthly economic indicators.
2. Classify variables as leading or coincident based on turning points and cross-correlation.
3. Extract common non-stationary factors to build the CLI and CCI.
4. Forecast GDP using Factor Linear Dynamic Regressions and compare out-of-sample performance against benchmark Autoregressive (AR) models.

## Data Sources

The dataset comprises monthly indicators capturing key dimensions of French economic activity. Data was sourced from:
* INSEE: Started dwellings, hotel occupancy rates, and air passengers.
* OECD: Manufacturing production and employment, services employment, and demand evolution.
* Eurostat: Quarterly National Accounts and initial monthly confidence indicators.

### Selected Indicators
* Leading: Started Dwellings (13-month lead), Air Passengers (14-month lead), Services Confidence (4-month lead), and Manufacturing Employment (9-month lead).
* Coincident: HICP (1-month lead), Order Book Levels (2-month lead), Construction Confidence (1-month lead), and Export Order Book Levels (0-month lead).

## Methodology

### 1. Trend Extraction and Linearization
* Models: Linear Dynamic Harmonic Regression (LDHR) and the Basic Structural Model (BSM) were used to compute trends.
* Linearization: TRAMO was applied to linearize variables only when it significantly improved the smoothness of the trend. 

### 2. Composite Indicator Construction
* Built using a non-stationary dynamic factor model following Bujosa, Garcia-Ferrer, and de Juan (2013).
* The covariance matrix was weighted by a scaling factor to neutralize explosive trends, converging to a random matrix with pervasive eigenvalues. The first non-stationary factor became the Composite Indicator.

### 3. Diagnostics
* Factor Extraction: Bartlett's sphericity test confirmed strong common factors among the selected variables.
* Anticipation: Cross-Correlation Functions (CCF) and Granger Causality tests confirmed that the CLI reliably leads the CCI by 4 months (r = 0.659).

### 4. GDP Forecasting Models
Four models were tested for in-sample fit and out-of-sample forecasting:
* M1: Benchmark AR(4) model.
* M2: Factor Linear Dynamic Regression (GDP growth explained by CLI lags).
* M3: Full Specification (M2 augmented with a COVID-19 shock dummy).
* M4: Full Specification + AR component.

## Key Results

* Leading Power: The constructed CLI acts as a solid 4-month early warning system for the broader economy (CCI).
* Forecasting Accuracy: Evaluated using RMSE, MAPE, and the Diebold-Mariano test. While the Diebold-Mariano test showed no statistically significant differences between the forecast errors of the models, the Factor Linear Dynamic Regression (M2) and the Full Specification (M3) clearly outperformed the benchmark AR models for annual GDP forecasting.

## References

* Bartlett, M. S. (1950): Tests of significance in factor analysis.
* Bujosa, M., Garcia-Ferrer, A., & Young, P. C. (2007): Linear dynamic harmonic regression.
* Bujosa, M., Garcia-Ferrer, A., & de Juan, A. (2013): Non-stationary Dynamic Factor Model.
* Bujosa, M., Garcia-Ferrer, A., de Juan, A., & Martin Arroyo, A. (2018): Factor Extraction diagnostics and Anticipation diagnostics.
* Diebold, F. X., & Mariano, R. S. (1995): Comparing Predictive Accuracy.
* Gomez, V., & Maravall, A. (1996): Programs TRAMO and SEATS.
* Granger, C. W. J. (1969): Investigating Causal Relations by Econometric Models and Cross-spectral Methods.
* Harvey, A. C. (1990): Forecasting, Structural Time Series Models and the Kalman Filter.
* Hodrick, R. J., & Prescott, E. C. (1997): Postwar U.S. Business Cycles: An Empirical Investigation.
* Pena, D., & Poncela, P. (2006): Convergence of the weighted covariance matrix.
* Young, P. C., Pedregal, D. J., & Tych, W. (1999): Dynamic harmonic regression.
* Institutions: Eurostat, INSEE, OECD, CEPR, NBER, and the French Business Cycle Dating Committee.
