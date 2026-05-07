# ==============================================================================
# GDP FORECASTING USING THE COMPOSITE LEADING INDICATOR (CLI)
# Features: AIC Lag, Bujosa Lags, OOS Fancharts, DM OLS, 2021 OOS Split
# ==============================================================================

library(ggplot2)
library(patchwork)
library(openxlsx)
library(dplyr)
library(tidyr)

# ==============================================================================
# 0 — PATHS & HELPERS
# ==============================================================================
data_dir    <- paste0(
  "C:/Users/Alejandro/Documents/Msc Analisis Economico Cuantitativo UAM/",
  "Courses/Quantitative Methods for Macroeconomics/",
  "France Business Cycle/data")
trends_file <- file.path(data_dir, "trends_selected.csv")
gdp_file    <- file.path(data_dir, "france_gdp.csv")

out_dir <- paste0(
  "C:/Users/Alejandro/Documents/Msc Analisis Economico Cuantitativo UAM/",
  "Courses/Quantitative Methods for Macroeconomics/",
  "France Business Cycle/assigment 4 plots/GDP_forecasting")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

rec_start <- as.Date(c("1980-02-01","1992-02-01","2008-02-01","2019-11-01"))
rec_end   <- as.Date(c("1980-11-01","1993-02-01","2009-05-01","2020-05-01"))

add_rec <- function(p, x_dates) {
  for (i in seq_along(rec_start)) {
    rs <- max(rec_start[i], min(x_dates, na.rm=TRUE))
    re <- min(rec_end[i],   max(x_dates, na.rm=TRUE))
    if (rs < re)
      p <- p + annotate("rect", xmin=rs, xmax=re, ymin=-Inf, ymax=Inf, fill="grey75", alpha=0.45)
  }
  return(p)
}

save_fig <- function(p, fname, w=28, h=16) {
  ggsave(file.path(out_dir, fname), plot=p, width=w, height=h, units="cm", dpi=150)
}

# Shared ggplot theme for all figures
theme_gdp <- function() {
  theme_bw(base_size = 11) +
    theme(
      plot.title       = element_text(face = "bold", size = 12, margin = margin(b = 4)),
      plot.subtitle    = element_text(size = 10, color = "grey40", margin = margin(b = 8)),
      axis.title       = element_text(size = 10),
      axis.text        = element_text(size = 9),
      legend.position  = "bottom",
      legend.title     = element_blank(),
      legend.text      = element_text(size = 9),
      legend.key.width = unit(1.8, "cm"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey92"),
      strip.background = element_rect(fill = "grey95", color = "grey70"),
      strip.text       = element_text(face = "bold", size = 10),
      plot.margin      = margin(8, 10, 6, 8)
    )
}

lag_vec <- function(x, k) c(rep(NA_real_, k), x[seq_len(length(x)-k)])

# ==============================================================================
# 1 — COMPUTE CLI AND PREPARE DATA
# ==============================================================================
cat("\nLoading CLI trends and computing composite...\n")
tr_wide <- read.csv(trends_file, stringsAsFactors=FALSE, na.strings="NA", check.names=FALSE)
tr_wide$date <- as.Date(tr_wide$date)
for (col in setdiff(names(tr_wide), "date")) {
  tr_wide[[col]] <- suppressWarnings(as.numeric(tr_wide[[col]]))
}

CLI_vars <- c(       "started_dwellings",
                     "air_passangers",
                     "services_conf",
                     "employment_manufacturing"
)

balanced <- function(df, vars) {
  sub <- df[,c("date",vars),drop=FALSE];  ok <- complete.cases(sub)
  list(dates=sub$date[ok], data=as.matrix(sub[ok,vars,drop=FALSE]))
}

compute_composite <- function(DATA_lev, k_lag=5) {
  T_n <- nrow(DATA_lev);  b <- t(DATA_lev)
  B   <- b - rowMeans(b, na.rm=TRUE)
  Cyk <- B[,1:(T_n-k_lag)] %*% t(B[,(k_lag+1):T_n]) / T_n
  ev  <- eigen(Cyk);  ix <- which.max(Re(ev$values))
  w   <- Re(ev$vectors[,ix]);  if(sum(w)<0) w <- -w;  w <- w/sum(w)
  list(composite=as.numeric(t(b)%*%w), weights=w)
}

cli_cs  <- balanced(tr_wide, CLI_vars)
cli_res <- compute_composite(cli_cs$data, 5)
CLI     <- cli_res$composite
dCLI_m  <- c(NA_real_, diff(CLI))

q_of <- function(d) {
  as.Date(paste0(format(d,"%Y"),"-", sprintf("%02d",(as.integer(format(d,"%m"))-1)%/%3*3+1),"-01"))
}

df_cli_m <- data.frame(date=cli_cs$dates, dCLI=dCLI_m)
df_cli_m <- df_cli_m[!is.na(df_cli_m$dCLI),]
df_cli_m$qdate <- q_of(df_cli_m$date)

# MONTHLY TO QUARTERLY AGGREGATION
cli_q_agg <- aggregate(dCLI ~ qdate, df_cli_m, mean, na.rm=TRUE)
names(cli_q_agg)[1] <- "date"

gdp_raw <- read.csv(gdp_file, stringsAsFactors=FALSE, na.strings="NA")
names(gdp_raw)[1:2] <- c("date", "gdp_growth")
gdp_raw$date <- as.Date(gdp_raw$date)

df_main <- merge(gdp_raw, cli_q_agg, by="date", all.x=TRUE)
df_main <- df_main[order(df_main$date),]

qdates  <- df_main$date
y_q     <- df_main$gdp_growth
x_q     <- df_main$dCLI
covid_d <- as.integer(qdates == as.Date("2020-04-01"))

# ==============================================================================
# 2 — BUILD STRICTLY ALIGNED DATASET (MAX LAG = 8)
# ==============================================================================
df_model <- data.frame(date = qdates, Y = y_q, X = x_q, covid = covid_d)

for(i in 1:8) df_model[[paste0("Y_lag", i)]] <- lag_vec(y_q, i)
for(i in 1:8) df_model[[paste0("X_lag", i)]] <- lag_vec(x_q, i)

df_model <- drop_na(df_model) 

# ==============================================================================
# 3 -- AIC LAG SELECTION & IN-SAMPLE ESTIMATION
# ==============================================================================
cat("\n=== AIC LAG SELECTION ===\n")

# M1: Pure AR 
best_ar_aic <- Inf;  best_ar_p <- 1
for (p in 1:8) {
  form <- as.formula(paste("Y ~", paste0("Y_lag", 1:p, collapse = " + ")))
  mod <- lm(form, data = df_model)
  aic <- AIC(mod)
  if (aic < best_ar_aic) { best_ar_aic <- aic;  best_ar_p <- p }
}
cat(sprintf("M1 Best AR: AR lags 1:%d  (AIC: %.2f)\n", best_ar_p, best_ar_aic))

# M2: Factor Linear Dynamic Regression 
best_m2_aic <- Inf;  best_m2_p <- 1
for (p in 1:8) {
  form <- as.formula(paste("Y ~", paste0("X_lag", 1:p, collapse = " + ")))
  mod <- lm(form, data = df_model)
  aic <- AIC(mod)
  if (aic < best_m2_aic) { best_m2_aic <- aic;  best_m2_p <- p }
}
cat(sprintf("M2 Factor LDR: CLI lags 1:%d  (AIC: %.2f)\n", best_m2_p, best_m2_aic))

# M3: Full Specification -- CLI lags x covid (no AR; Bujosa approach)
best_m3_aic  <- Inf
best_m3_cli_p <- 1;  best_m3_covid <- FALSE
for (p_cli in 1:8) {
  for (use_covid in c(FALSE, TRUE)) {
    cli_terms <- paste0("X_lag", 1:p_cli, collapse = " + ")
    rhs  <- paste(c(cli_terms, if (use_covid) "covid"), collapse = " + ")
    mod  <- lm(as.formula(paste("Y ~", rhs)), data = df_model)
    aic <- AIC(mod)
    if (aic < best_m3_aic) {
      best_m3_aic  <- aic
      best_m3_cli_p <- p_cli
      best_m3_covid <- use_covid
    }
  }
}
cat(sprintf("M3 Full Specification: CLI lags 1:%d, covid = %s  (AIC: %.2f)\n",
            best_m3_cli_p, best_m3_covid, best_m3_aic))

# M4: Full Specification + AR -- CLI lags x AR lags x covid (all AIC-selected)
best_m4_aic  <- Inf
best_m4_cli_p <- 1;  best_m4_ar_p <- 1;  best_m4_covid <- FALSE
for (p_cli in 1:8) {
  for (p_ar in 1:8) {
    for (use_covid in c(FALSE, TRUE)) {
      cli_terms <- paste0("X_lag", 1:p_cli, collapse = " + ")
      ar_terms  <- paste0("Y_lag", 1:p_ar,  collapse = " + ")
      rhs <- paste(c(cli_terms, ar_terms, if (use_covid) "covid"), collapse = " + ")
      mod  <- lm(as.formula(paste("Y ~", rhs)), data = df_model)
      aic <- AIC(mod)
      if (aic < best_m4_aic) {
        best_m4_aic  <- aic
        best_m4_cli_p <- p_cli
        best_m4_ar_p  <- p_ar
        best_m4_covid <- use_covid
      }
    }
  }
}
cat(sprintf("M4 Full Spec + AR: CLI lags 1:%d, AR lags 1:%d, covid = %s  (AIC: %.2f)\n",
            best_m4_cli_p, best_m4_ar_p, best_m4_covid, best_m4_aic))

# -- Fit all models on full sample --
m1_form <- as.formula(paste("Y ~", paste0("Y_lag", 1:best_ar_p, collapse = " + ")))
m1_fit  <- lm(m1_form, data = df_model)

m2_form <- as.formula(paste("Y ~", paste0("X_lag", 1:best_m2_p, collapse = " + ")))
m2_fit  <- lm(m2_form, data = df_model)

{
  cli_terms <- paste0("X_lag", 1:best_m3_cli_p, collapse = " + ")
  rhs <- paste(c(cli_terms, if (best_m3_covid) "covid"), collapse = " + ")
  m3_form <- as.formula(paste("Y ~", rhs))
}
m3_fit  <- lm(m3_form, data = df_model)

{
  cli_terms <- paste0("X_lag", 1:best_m4_cli_p, collapse = " + ")
  ar_terms  <- paste0("Y_lag", 1:best_m4_ar_p,  collapse = " + ")
  rhs <- paste(c(cli_terms, ar_terms, if (best_m4_covid) "covid"), collapse = " + ")
  m4_form <- as.formula(paste("Y ~", rhs))
}
m4_fit  <- lm(m4_form, data = df_model)

df_model$M1_fit <- predict(m1_fit)
df_model$M2_fit <- predict(m2_fit)
df_model$M3_fit <- predict(m3_fit)
df_model$M4_fit <- predict(m4_fit)

# ==============================================================================
# 4 — IN-SAMPLE PLOTS
# ==============================================================================
plot_insample <- function(df, fit_col, model_name, r2_val, col_hex,
                          cli_lag_cols = NULL, show_covid = FALSE) {
  p <- ggplot(df, aes(x = date))
  if (show_covid) {
    covid_dates <- df$date[df$covid == 1]
    if (length(covid_dates) > 0) {
      for (cd in covid_dates) {
        cd <- as.Date(cd, origin = "1970-01-01")
        p <- p + annotate("rect", xmin = cd, xmax = cd + 90, ymin = -Inf, ymax = Inf, fill = "#E74C3C", alpha = 0.12)
      }
    }
  }
  if (!is.null(cli_lag_cols)) {
    for (j in seq_along(cli_lag_cols)) {
      lag_label <- paste0("CLI lag ", gsub("X_lag", "", cli_lag_cols[j]))
      p <- p + geom_line(aes_string(y = cli_lag_cols[j], colour = shQuote(lag_label)), linewidth = 0.55, linetype = "dotted", alpha = 0.85)
    }
  }
  sub_parts <- sprintf("R² = %.4f", r2_val)
  if (!is.null(cli_lag_cols)) sub_parts <- paste0(sub_parts, "  |  CLI lags: ", paste(gsub("X_lag", "", cli_lag_cols), collapse = ", "))
  if (show_covid) sub_parts <- paste0(sub_parts, "  |  Covid dummy included")
  
  p <- p + geom_hline(yintercept = 0, color = "grey65", linewidth = 0.35) +
    geom_line(aes(y = Y, color = "Actual GDP"), linewidth = 1.1) +
    geom_line(aes_string(y = fit_col, color = shQuote(model_name)), linewidth = 0.85, linetype = "dashed") +
    scale_color_manual(values = c("Actual GDP" = "black", setNames(col_hex, model_name)), breaks = c("Actual GDP", model_name)) +
    labs(title = sprintf("%s -- In-Sample Fit", model_name), subtitle = sub_parts, x = NULL, y = "GDP growth (q-o-q %)", color = "") +
    theme_gdp()
  return(add_rec(p, df$date))
}

p_m1_is <- plot_insample(df_model, "M1_fit", "M1: Best AR",
                         summary(m1_fit)$r.squared, "#2471A3")
p_m2_is <- plot_insample(df_model, "M2_fit", "M2: Factor Linear Dynamic Regression",
                         summary(m2_fit)$r.squared, "#E67E22",
                         cli_lag_cols = paste0("X_lag", 1:best_m2_p))
p_m3_is <- plot_insample(df_model, "M3_fit", "M3: Full Specification",
                         summary(m3_fit)$r.squared, "#27AE60",
                         cli_lag_cols = paste0("X_lag", 1:best_m3_cli_p),
                         show_covid   = best_m3_covid)
p_m4_is <- plot_insample(df_model, "M4_fit", "M4: Full Spec + AR",
                         summary(m4_fit)$r.squared, "#8E44AD",
                         cli_lag_cols = paste0("X_lag", 1:best_m4_cli_p),
                         show_covid   = best_m4_covid)

save_fig(p_m1_is, "Fig_01_InSample_M1.png")
save_fig(p_m2_is, "Fig_02_InSample_M2.png")
save_fig(p_m3_is, "Fig_03_InSample_M3.png")
save_fig(p_m4_is, "Fig_04_InSample_M4.png")

# ==============================================================================
# 5 -- OUT-OF-SAMPLE TRAIN/TEST SPLIT & FORECASTING
# ==============================================================================
cat("\n=== OUT-OF-SAMPLE FORECASTING (QUARTERLY) ===\n")
# MODIFIED: Explicitly split data at 2021-01-01
cutoff_date <- as.Date("2021-01-01")
train_idx   <- which(df_model$date < cutoff_date)
test_idx    <- which(df_model$date >= cutoff_date)

df_train <- df_model[train_idx, ]
df_test  <- df_model[test_idx,  ]

m1_train <- lm(m1_form, data = df_train)
m2_train <- lm(m2_form, data = df_train)
m3_train <- lm(m3_form, data = df_train)
m4_train <- lm(m4_form, data = df_train)

# ---- Historical OOS predictions -----------
pred1 <- predict(m1_train, df_test, se.fit = TRUE)
pred2 <- predict(m2_train, df_test, se.fit = TRUE)
pred3 <- predict(m3_train, df_test, se.fit = TRUE)
pred4 <- predict(m4_train, df_test, se.fit = TRUE)

df_test$M1_fcast <- pred1$fit; df_test$M1_se <- pred1$se.fit
df_test$M2_fcast <- pred2$fit; df_test$M2_se <- pred2$se.fit
df_test$M3_fcast <- pred3$fit; df_test$M3_se <- pred3$se.fit
df_test$M4_fcast <- pred4$fit; df_test$M4_se <- pred4$se.fit

e1 <- df_test$Y - df_test$M1_fcast
e2 <- df_test$Y - df_test$M2_fcast
e3 <- df_test$Y - df_test$M3_fcast
e4 <- df_test$Y - df_test$M4_fcast

df_test$e1 <- e1; df_test$e2 <- e2; df_test$e3 <- e3; df_test$e4 <- e4

calc_metrics <- function(e, y, label) {
  rmse <- sqrt(mean(e^2, na.rm = TRUE))
  mape <- mean(abs(e / y), na.rm = TRUE)
  data.frame(Model = label, RMSE = round(rmse, 4), MAPE = round(mape, 4))
}
metrics_df <- rbind(
  calc_metrics(e1, df_test$Y, "M1: AR"),
  calc_metrics(e2, df_test$Y, "M2: Factor Linear Dynamic Regression"),
  calc_metrics(e3, df_test$Y, "M3: Full Specification"),
  calc_metrics(e4, df_test$Y, "M4: Full Spec + AR")
)
print(metrics_df)

# ---- Extended iterative forecast to 2026Q4 -----------------------------------
last_date    <- max(df_model$date)
last_q_yr    <- as.integer(format(last_date, "%Y"))
last_q_mo    <- as.integer(format(last_date, "%m"))

# NOTE: We generate the sequence up to 2026 Q4 so that the annual rollup for 2026 is complete
future_dates <- seq(
  from = as.Date(sprintf("%04d-%02d-01",
                         last_q_yr + (last_q_mo + 3 - 1) %/% 12,
                         ((last_q_mo + 3 - 1) %% 12) + 1)),
  by   = "quarter",
  to   = as.Date("2026-10-01") 
)

y_buf <- df_model$Y
x_buf <- df_model$X
last_x <- tail(x_buf[!is.na(x_buf)], 1) 

sigma_m1 <- summary(m1_train)$sigma
sigma_m2 <- summary(m2_train)$sigma
sigma_m3 <- summary(m3_train)$sigma
sigma_m4 <- summary(m4_train)$sigma

iterative_forecast <- function(model_fit, ar_p, cli_p, y_hist, x_hist, future_dates, sigma_resid, covid_active = FALSE) {
  n_fut  <- length(future_dates)
  y_ext  <- y_hist
  x_ext  <- x_hist
  fcast  <- numeric(n_fut)
  fcast_se <- numeric(n_fut)

  for (h in seq_len(n_fut)) {
    n_cur <- length(y_ext)
    nd <- data.frame(covid = as.integer(covid_active))
    if (ar_p > 0) {
      for (j in 1:ar_p)
        nd[[paste0("Y_lag", j)]] <- if (n_cur - j >= 1) y_ext[n_cur - j + 1] else NA_real_
    }
    if (cli_p > 0) {
      for (j in 1:cli_p) {
        idx_x <- length(x_ext) - j + 1
        nd[[paste0("X_lag", j)]] <- if (idx_x >= 1) x_ext[idx_x] else last_x
      }
    }
    pr          <- predict(model_fit, newdata = nd, se.fit = TRUE)
    fcast[h]    <- pr$fit
    fcast_se[h] <- sqrt(pr$se.fit^2 + h * sigma_resid^2)
    y_ext <- c(y_ext, fcast[h])
    x_ext <- c(x_ext, last_x)
  }
  list(fcast = fcast, se = fcast_se)
}

ar_p_m1 <- best_ar_p;    cli_p_m1 <- 0
ar_p_m2 <- 0;            cli_p_m2 <- best_m2_p
ar_p_m3 <- 0;            cli_p_m3 <- best_m3_cli_p
ar_p_m4 <- best_m4_ar_p; cli_p_m4 <- best_m4_cli_p

fut1 <- iterative_forecast(m1_train, ar_p_m1, cli_p_m1, y_buf, x_buf, future_dates, sigma_m1)
fut2 <- iterative_forecast(m2_train, ar_p_m2, cli_p_m2, y_buf, x_buf, future_dates, sigma_m2)
fut3 <- iterative_forecast(m3_train, ar_p_m3, cli_p_m3, y_buf, x_buf, future_dates, sigma_m3, covid_active = FALSE)
fut4 <- iterative_forecast(m4_train, ar_p_m4, cli_p_m4, y_buf, x_buf, future_dates, sigma_m4, covid_active = FALSE)

df_future <- data.frame(
  date     = future_dates, Y = NA_real_,
  M1_fcast = fut1$fcast, M1_se = fut1$se,
  M2_fcast = fut2$fcast, M2_se = fut2$se,
  M3_fcast = fut3$fcast, M3_se = fut3$se,
  M4_fcast = fut4$fcast, M4_se = fut4$se
)

# ==============================================================================
# 6 -- PLOTS: OOS COMBINED (2021+) & INDIVIDUAL FANCHARTS
# ==============================================================================
# Quarterly cutoff
cutoff_val  <- df_model$Y[df_model$date == cutoff_date]
horizon_end_q <- as.Date("2025-04-01") # Restrict quarterly plot up to 2025 Q2

df_oos_all <- rbind(
  df_test[,   c("date","Y","M1_fcast","M1_se","M2_fcast","M2_se","M3_fcast","M3_se","M4_fcast","M4_se")],
  df_future[, c("date","Y","M1_fcast","M1_se","M2_fcast","M2_se","M3_fcast","M3_se","M4_fcast","M4_se")]
)

# Limit the plot data up to 2025 Q2
df_oos_q_plot <- df_oos_all[df_oos_all$date <= horizon_end_q, ]
df_recent <- df_model[df_model$date >= as.Date("2018-01-01") & df_model$date <= horizon_end_q, ]

p_oos_combined <- ggplot() +
  geom_line(data = df_recent, aes(x = date, y = Y, color = "Actual GDP"), linewidth = 1.2) +
  geom_vline(xintercept = as.numeric(cutoff_date), linetype = "dashed", color = "red", linewidth = 0.7) +
  geom_point(aes(x = cutoff_date, y = cutoff_val), color = "red", size = 2.5, shape = 21, fill = "white", stroke = 1.2) +
  geom_line(data = df_oos_q_plot, aes(x = date, y = M1_fcast, color = "M1: AR"),                              linetype = "dashed", linewidth = 0.75) +
  geom_line(data = df_oos_q_plot, aes(x = date, y = M2_fcast, color = "M2: Factor Linear Dynamic Regression"), linetype = "dashed", linewidth = 0.75) +
  geom_line(data = df_oos_q_plot, aes(x = date, y = M3_fcast, color = "M3: Full Specification"),               linetype = "dashed", linewidth = 0.75) +
  geom_line(data = df_oos_q_plot, aes(x = date, y = M4_fcast, color = "M4: Full Spec + AR"),                   linetype = "dashed", linewidth = 0.75) +
  geom_hline(yintercept = 0, color = "grey65", linewidth = 0.35) +
  scale_color_manual(
    values = c("Actual GDP" = "black", "M1: AR" = "#2471A3",
               "M2: Factor Linear Dynamic Regression" = "#E67E22",
               "M3: Full Specification" = "#27AE60",
               "M4: Full Spec + AR"     = "#8E44AD"),
    breaks = c("Actual GDP","M1: AR","M2: Factor Linear Dynamic Regression",
               "M3: Full Specification","M4: Full Spec + AR")
  ) +
  labs(title    = "All Models -- Out-of-Sample Forecasts (2021 -- 2025Q2)",
       subtitle = "Red line = OOS start (2021)  |  Dashed lines = model forecasts",
       x = NULL, y = "GDP growth (q-o-q %)", color = "") +
  theme_gdp()

p_oos_combined <- add_rec(p_oos_combined, df_recent$date)
save_fig(p_oos_combined, "Fig_05_OOS_Combined_2021_2025Q2.png", w = 32, h = 16)


# ==============================================================================
# 6.5 -- ANNUAL FORECAST AGGREGATION & COMBINED PLOT
# ==============================================================================
cat("\n=== AGGREGATING & PLOTTING ANNUAL FORECASTS ===\n")

# Aggregate Actual historical quarterly data to Annual
df_annual_actual <- df_model %>%
  mutate(Year = as.integer(format(date, "%Y"))) %>%
  group_by(Year) %>%
  summarise(Y_annual = sum(Y, na.rm = FALSE), n_q = n()) %>%
  filter(n_q == 4) # Only full years

# Aggregate Forecasted quarterly data to Annual
# Note: df_oos_all contains data generated up to 2026 Q4 to support this rollup
df_annual_fcast <- df_oos_all %>%
  mutate(Year = as.integer(format(date, "%Y"))) %>%
  group_by(Year) %>%
  summarise(
    M1_annual = sum(M1_fcast, na.rm = FALSE),
    M2_annual = sum(M2_fcast, na.rm = FALSE),
    M3_annual = sum(M3_fcast, na.rm = FALSE),
    M4_annual = sum(M4_fcast, na.rm = FALSE),
    n_q = n()
  ) %>%
  filter(n_q == 4) # Only full years

cutoff_year <- 2021
future_yr_start <- as.integer(format(min(df_future$date), "%Y"))

# Combined Annual Plot
p_annual <- ggplot() +
  geom_hline(yintercept = 0, color = "grey65", linewidth = 0.35) +
  annotate("rect", xmin = future_yr_start - 0.5, xmax = 2026.5, ymin = -Inf, ymax = Inf, fill = "grey97", alpha = 1) +
  geom_line(data = df_annual_actual %>% filter(Year >= 2016), 
            aes(x = Year, y = Y_annual, color = "Actual GDP"), linewidth = 1.3) +
  geom_line(data = df_annual_fcast, 
            aes(x = Year, y = M1_annual, color = "M1: AR"), linetype = "dashed", linewidth = 0.95) +
  geom_line(data = df_annual_fcast, 
            aes(x = Year, y = M2_annual, color = "M2: Factor Linear Dynamic Regression"), linetype = "dashed", linewidth = 0.95) +
  geom_line(data = df_annual_fcast,
            aes(x = Year, y = M3_annual, color = "M3: Full Specification"), linetype = "dashed", linewidth = 0.95) +
  geom_line(data = df_annual_fcast,
            aes(x = Year, y = M4_annual, color = "M4: Full Spec + AR"),     linetype = "dashed", linewidth = 0.95) +
  geom_vline(xintercept = cutoff_year, linetype = "dashed", color = "red", linewidth = 0.7) +
  scale_color_manual(
    values = c("Actual GDP" = "black", "M1: AR" = "#2471A3",
               "M2: Factor Linear Dynamic Regression" = "#E67E22",
               "M3: Full Specification" = "#27AE60",
               "M4: Full Spec + AR"     = "#8E44AD"),
    breaks = c("Actual GDP","M1: AR","M2: Factor Linear Dynamic Regression",
               "M3: Full Specification","M4: Full Spec + AR")
  ) +
  scale_x_continuous(breaks = seq(2016, 2026, 2)) +
  labs(
    title    = "Annual GDP Growth Forecast (2018 -- 2026)",
    subtitle = "Calculated as the sum of quarterly growth rates (Q4/Q4 proxy) | Red line marks OOS start (2021)",
    x = "Year", y = "Annual Growth (%)", color = ""
  ) +
  theme_gdp()

save_fig(p_annual, "Fig_06_Annual_Forecast_Combined.png", w = 28, h = 14)


# ==============================================================================
# 7 -- DIEBOLD-MARIANO TEST (STRICT OLS)
# ==============================================================================
cat("\n=== DIEBOLD-MARIANO OLS TESTS ===\n")
dm_ols_test <- function(err1, err2, name1, name2) {
  L       <- (err1^2) - (err2^2)
  dm_mod  <- lm(L ~ 1)
  dm_sum  <- summary(dm_mod)
  alpha   <- dm_sum$coefficients[1, "Estimate"]
  pval    <- dm_sum$coefficients[1, "Pr(>|t|)"]
  tstat   <- dm_sum$coefficients[1, "t value"]
  interp  <- if (pval < 0.05) {
    if (alpha > 0) paste(name1, "is significantly worse") else paste(name2, "is significantly worse")
  } else "No significant difference"
  data.frame(Comparison      = paste(name1, "vs", name2),
             Alpha_Intercept = round(alpha, 4),
             T_Stat          = round(tstat, 4),
             P_Value         = round(pval,  4),
             Result          = interp)
}

dm_results <- rbind(
  dm_ols_test(e1, e2, "M1 AR",       "M2 Factor LDR"),
  dm_ols_test(e1, e3, "M1 AR",       "M3 Full Spec"),
  dm_ols_test(e1, e4, "M1 AR",       "M4 Full Spec+AR"),
  dm_ols_test(e2, e3, "M2 Factor LDR","M3 Full Spec"),
  dm_ols_test(e2, e4, "M2 Factor LDR","M4 Full Spec+AR"),
  dm_ols_test(e3, e4, "M3 Full Spec", "M4 Full Spec+AR")
)
print(dm_results)

# ==============================================================================
# 8 -- SAVE EVERYTHING TO EXCEL
# ==============================================================================
cat("\nSaving results to Excel...\n")
wb <- createWorkbook()

addWorksheet(wb, "Metrics_OOS")
writeData(wb, "Metrics_OOS", metrics_df)

addWorksheet(wb, "Diebold_Mariano_OLS")
writeData(wb, "Diebold_Mariano_OLS", dm_results)

addWorksheet(wb, "InSample_R2")
insample_r2 <- data.frame(
  Model = c("M1: AR","M2: Factor Linear Dynamic Regression",
            "M3: Full Specification","M4: Full Spec + AR"),
  R2    = c(summary(m1_fit)$r.squared, summary(m2_fit)$r.squared,
            summary(m3_fit)$r.squared, summary(m4_fit)$r.squared))
writeData(wb, "InSample_R2", insample_r2)

addWorksheet(wb, "OOS_Predictions_Q")
export_oos <- df_test[, c("date","Y","M1_fcast","M2_fcast","M3_fcast","M4_fcast","e1","e2","e3","e4")]
writeData(wb, "OOS_Predictions_Q", export_oos)

addWorksheet(wb, "Extended_Forecast_Q")
export_fut <- df_future[, c("date","M1_fcast","M1_se","M2_fcast","M2_se","M3_fcast","M3_se","M4_fcast","M4_se")]
writeData(wb, "Extended_Forecast_Q", export_fut)

addWorksheet(wb, "Annual_Forecasts")
export_ann <- merge(
  df_annual_actual[, c("Year", "Y_annual")],
  df_annual_fcast[, c("Year","M1_annual","M2_annual","M3_annual","M4_annual")],
  by = "Year", all = TRUE
)
writeData(wb, "Annual_Forecasts", export_ann)

saveWorkbook(wb, file.path(out_dir, "Evaluation_GDP_Forecasting.xlsx"), overwrite = TRUE)
cat(sprintf("\nDone! Results and plots exported to:\n%s\n", out_dir))