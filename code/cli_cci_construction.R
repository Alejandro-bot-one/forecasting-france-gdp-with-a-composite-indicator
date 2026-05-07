# ==============================================================================
# CLI / CCI  COMPOSITE INDICATOR CONSTRUCTION
# France Business Cycle  —  Bujosa, García-Ferrer & de Juan (2013)
#
# CLI  (5 leading series):
#   car_registrations, started_dwellings, econ_sit_next_12m,
#   major_purchases_now, services_conf
#
# CCI  (6 coincident series — long-coverage only):
#   industrial_conf, order_book_level, construction_conf,
#   hicp_yoy, stocks_finished, export_order_book_level
#
# Outputs saved to assigment 4 plots/:
#   Fig01_dCLI_dCCI.png            — first differences + recession shading
#   Fig02_CLI_levels.png           — CLI level + GDP comparison
#   Fig03_CCI_levels.png           — CCI index (CCI peak = 100) + GDP comparison
#   Fig04_CLI_CCI_vs_GDP.png       — CLI & CCI vs GDP (combined)
#   Fig05_CCF_CLI_vs_CCI.png       — anticipation CCF bar chart
#   Fig06_CCF_CLI_vars_vs_CLI.png  — per-variable CCF (CLI vs own composite)
#   Fig07_CCF_CCI_vars_vs_CCI.png  — per-variable CCF (CCI vs own composite)
#   Fig08_CCF_CLI_vars_vs_CCI.png  — per-variable CCF (CLI vars vs CCI)
#   Diagnostics_CLI_CCI.xlsx       — all diagnostic tables
#     └ CLI_index_avg2019 sheet    — CLI index, avg(start–2019M12) = 100 (standalone)
#     └ CCI_index_avg2019 sheet    — CCI index, avg(start–2019M12) = 100 (standalone)
# ==============================================================================

library(ggplot2)
library(patchwork)
library(openxlsx)
library(dplyr)
library(tidyr)

# ==============================================================================
# 0 — PATHS
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
  "France Business Cycle/assigment 4 plots")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# French recession dates
rec_start <- as.Date(c("1980-02-01","1992-02-01","2008-02-01","2019-11-01"))
rec_end   <- as.Date(c("1980-11-01","1993-02-01","2009-05-01","2020-05-01"))

# ==============================================================================
# 1 — VARIABLE GROUPS
# ==============================================================================
CLI_vars <- c(
  "started_dwellings",
  "air_passangers",
  "services_conf",
  "employment_manufacturing"
  
)
CCI_vars <- c(
  "industrial_conf",
  "hicp_yoy",                 # ~1-month lead
  "export_order_book_level",   # ~0-month lead
  "stocks_finished"
)

# ==============================================================================
# 2 — HELPERS
# ==============================================================================
add_rec_shading <- function(p, x_dates) {
  for (i in seq_along(rec_start)) {
    rs <- max(rec_start[i], min(x_dates, na.rm = TRUE))
    re <- min(rec_end[i],   max(x_dates, na.rm = TRUE))
    if (rs < re)
      p <- p + annotate("rect", xmin = rs, xmax = re,
                        ymin = -Inf, ymax = Inf, fill = "grey70", alpha = 0.45)
  }
  p
}

save_plot <- function(p, fname, w = 28, h = 16)
  ggsave(file.path(out_dir, fname), plot = p,
         width = w, height = h, units = "cm", dpi = 150)

# NS-DFM composite  (Bujosa et al. 2013)
compute_composite <- function(DATA_lev, k_lag = 5) {
  T_n <- nrow(DATA_lev)
  b   <- t(DATA_lev)
  B   <- b - rowMeans(b, na.rm = TRUE)
  Cyk <- B[, 1:(T_n - k_lag)] %*% t(B[, (k_lag + 1):T_n]) / T_n
  ev  <- eigen(Cyk)
  ix  <- which.max(Re(ev$values))
  w   <- Re(ev$vectors[, ix])
  if (sum(w) < 0) w <- -w
  w   <- w / sum(w)
  list(composite   = as.numeric(t(b) %*% w),
       weights      = w,
       eigenvalues  = Re(ev$values),
       eigenvectors = Re(ev$vectors))
}

# Balanced (complete-case) sample
balanced <- function(df, vars) {
  sub <- df[, c("date", vars), drop = FALSE]
  ok  <- complete.cases(sub)
  list(dates = sub$date[ok],
       data  = as.matrix(sub[ok, vars, drop = FALSE]),
       n     = sum(ok),
       start = min(sub$date[ok]),
       end   = max(sub$date[ok]))
}

# Cross-correlation function: h > 0 means x leads y
compute_ccf_vec <- function(x, y, H) {
  vapply(-H:H, function(h) {
    n_obs <- length(x)
    
    # SAFETY CHECK: If the lag is greater than or equal to the number 
    # of observations, we cannot compute a correlation. Return NA.
    if (n_obs <= abs(h)) {
      return(NA_real_)
    }
    
    if      (h == 0) cor(x, y, use = "complete.obs")
    else if (h  > 0) cor(x[seq_len(n_obs-h)], y[seq(h+1,n_obs)], use = "complete.obs")
    else             cor(x[seq(-h+1,n_obs)],  y[seq_len(n_obs+h)], use = "complete.obs")
  }, numeric(1))
}

# Granger F-test: does x Granger-cause y?
granger_f <- function(y, x, p) {
  T_p <- length(y) - p
  Y   <- y[(p+1):length(y)]
  Xr  <- matrix(0, T_p, p);  Xur <- matrix(0, T_p, 2*p)
  for (lag in seq_len(p)) {
    Xr[,lag]    <- y[(p+1-lag):(length(y)-lag)]
    Xur[,lag]   <- y[(p+1-lag):(length(y)-lag)]
    Xur[,p+lag] <- x[(p+1-lag):(length(x)-lag)]
  }
  Xr  <- cbind(1, Xr);  Xur <- cbind(1, Xur)
  rr  <- lm.fit(Xr,  Y)$residuals
  rur <- lm.fit(Xur, Y)$residuals
  Fst <- ((sum(rr^2)-sum(rur^2))/p) / (sum(rur^2)/(T_p-ncol(Xur)))
  pv  <- pf(Fst, p, T_p-ncol(Xur), lower.tail = FALSE)
  list(F = Fst, p = pv)
}

sig_stars <- function(p)
  ifelse(p < 0.01, "*** (1%)", ifelse(p < 0.05, "**  (5%)",
  ifelse(p < 0.10, "*  (10%)", "Not sig.")))

# ==============================================================================
# 3 — LOAD DATA  (robust import: na.strings + force numeric)
# ==============================================================================
cat("Loading trends:", trends_file, "\n")
tr_wide <- read.csv(trends_file,
                    stringsAsFactors = FALSE,
                    na.strings       = "NA",
                    check.names      = FALSE)
tr_wide$date <- as.Date(tr_wide$date)
for (col in setdiff(names(tr_wide), "date"))
  tr_wide[[col]] <- suppressWarnings(as.numeric(tr_wide[[col]]))

cat(sprintf("  Loaded: %d rows x %d columns  (%s to %s)\n\n",
            nrow(tr_wide), ncol(tr_wide),
            min(tr_wide$date), max(tr_wide$date)))

# ==============================================================================
# 4 — EXTRACT BALANCED SAMPLES & BUILD COMPOSITES
# ==============================================================================
cli_cs  <- balanced(tr_wide, CLI_vars)
cci_cs  <- balanced(tr_wide, CCI_vars)

cat(sprintf("CLI balanced sample: %s to %s  (%d obs)\n", cli_cs$start, cli_cs$end, cli_cs$n))
cat(sprintf("CCI balanced sample: %s to %s  (%d obs)\n\n", cci_cs$start, cci_cs$end, cci_cs$n))

K_LAG   <- 5
cli_res <- compute_composite(cli_cs$data, K_LAG)
cci_res <- compute_composite(cci_cs$data, K_LAG)

CLI  <- cli_res$composite;  dCLI <- c(NA_real_, diff(CLI))
CCI  <- cci_res$composite;  dCCI <- c(NA_real_, diff(CCI))

# ==============================================================================
# 2b — CLI & CCI INDEXES  (mean = 100 over start–2019M12)
# ==============================================================================
norm_mean_100 <- function(series, dates) {
  base_mask <- dates <= as.Date("2019-12-01")
  series / mean(series[base_mask], na.rm = TRUE) * 100
}

CLI_INDEX <- norm_mean_100(CLI, cli_cs$dates)
CCI_INDEX <- norm_mean_100(CCI, cci_cs$dates)

# Save indexes as standalone data frames (separate from the composites)
cli_index_df <- data.frame(date = cli_cs$dates, CLI_index = CLI_INDEX)
cci_index_df <- data.frame(date = cci_cs$dates, CCI_index = CCI_INDEX)

cat("=== CLI WEIGHTS ===\n")
for (i in seq_along(CLI_vars))
  cat(sprintf("  %-30s : %+.4f\n", CLI_vars[i], cli_res$weights[i]))
cat("\n=== CCI WEIGHTS ===\n")
for (i in seq_along(CCI_vars))
  cat(sprintf("  %-30s : %+.4f\n", CCI_vars[i], cci_res$weights[i]))
cat("\n")

# ==============================================================================
# 5 — DIAGNOSTICS
# ==============================================================================

## 5a  Bartlett sphericity test
bartlett_test <- function(DATA, label) {
  n   <- nrow(DATA);  p <- ncol(DATA)
  R   <- cor(DATA, use = "pairwise.complete.obs")
  chi2 <- -(n - 1 - (2*p+5)/6) * log(det(R))
  df   <- p*(p-1)/2
  pval <- pchisq(chi2, df, lower.tail = FALSE)
  res  <- ifelse(pval < 0.01, "*** Strong common factor",
          ifelse(pval < 0.05, "**  Common factor exists",
          ifelse(pval < 0.10, "*   Weak common factor", "No common factor")))
  cat(sprintf("  Bartlett %-6s: chi2=%8.3f  df=%2d  p=%.4f  %s\n",
              label, chi2, df, pval, res))
  data.frame(Group=label, Chi2=round(chi2,4), df=df,
             p_value=round(pval,6), Result=res, stringsAsFactors=FALSE)
}

cat("=== TEST 1: BARTLETT SPHERICITY ===\n")
cat("H0: variables are uncorrelated (no common factor)\n\n")
bt_tbl <- rbind(bartlett_test(cli_cs$data, "CLI"),
                bartlett_test(cci_cs$data, "CCI"))
cat("\n")

## 5b  Eigenvalue shares
eig_share_tbl <- function(DATA, label) {
  Ds  <- scale(DATA)
  R   <- t(Ds) %*% Ds / (nrow(Ds)-1)
  lam <- sort(eigen(R, only.values=TRUE)$values, decreasing=TRUE)
  lam <- lam[lam > 0]
  shr <- lam / sum(lam) * 100;  cum <- cumsum(shr)
  
  # Print the corrected header
  cat(sprintf("\n  %-12s  %-12s  %-12s\n", "Eigenvalue", "Share (%)", "Cumul (%)"))
  cat("  ", strrep("-", 40), "\n", sep="")
  
  # Print the values in a loop
  for (i in seq_along(lam)) {
    cat(sprintf("  %-12.4f  %-12.2f  %-12.2f%s\n",
                lam[i], shr[i], cum[i], if(i==1) "  <-- dominant" else ""))
  }
  
  data.frame(Group=label, Eigenvector=seq_along(lam),
             Eigenvalue=round(lam,4), Share_pct=round(shr,2),
             Cumul_pct=round(cum,2), stringsAsFactors=FALSE)
}

cat("=== TEST 2: EIGENVALUE SHARES ===\n")
eig_tbl <- rbind(
  { cat("\nCLI:\n"); eig_share_tbl(cli_cs$data, "CLI") },
  { cat("\nCCI:\n"); eig_share_tbl(cci_cs$data, "CCI") }
)
cat("\n")

## 5c  Correlation of each variable with own composite
corr_composite <- function(DATA, composite, varnames, label) {
  # Corrected header print statement
  cat(sprintf("\n  %-28s  %-8s\n", "Variable", "Corr"))
  cat("  ", strrep("-", 38), "\n", sep="")
  
  rows <- lapply(seq_along(varnames), function(i) {
    r <- cor(DATA[,i], composite, use = "complete.obs")
    cat(sprintf("  %-28s  %+.4f\n", varnames[i], r))
    data.frame(Group=label, Variable=varnames[i],
               Corr_with_composite=round(r,4), stringsAsFactors=FALSE)
  })
  do.call(rbind, rows)
}
cat("=== TEST 3: CORRELATION WITH OWN COMPOSITE ===\n")
corr_tbl <- rbind(
  { cat("\nCLI:\n"); corr_composite(cli_cs$data, CLI, CLI_vars, "CLI") },
  { cat("\nCCI:\n"); corr_composite(cci_cs$data, CCI, CCI_vars, "CCI") }
)
cat("\n")

## 5d  Per-variable CCF vs own composite + Granger
H_ccf <- 18;  p_gran <- 6;  h_ccf <- -H_ccf:H_ccf

run_self_ccf <- function(DATA, composite, varnames, group, color) {
  dY   <- diff(composite);  T_n <- length(dY);  ci95 <- 1.96/sqrt(T_n)
  cat(sprintf("\n  %s  (ci95 = ±%.4f)\n", group, ci95))
  cat(sprintf("  %-26s  %6s  %8s  %6s  %10s  %8s  %s\n",
              "Variable","h*","r(h*)","Sig?","Granger-F","p-val","Stars"))
  cat("  ", strrep("-",80), "\n", sep="")
  rows <- list();  plots <- list()
  for (v in seq_along(varnames)) {
    dX   <- diff(DATA[,v])
    T_u  <- min(length(dX), T_n)
    dX_u <- dX[seq_len(T_u)];  dY_u <- dY[seq_len(T_u)]
    ok   <- !is.na(dX_u) & !is.na(dY_u)
    ccf_v <- compute_ccf_vec(dX_u[ok], dY_u[ok], H_ccf)
    ix    <- which.max(abs(ccf_v));  h_star <- h_ccf[ix];  r_star <- ccf_v[ix]
    sig   <- abs(r_star) > ci95
    gran  <- tryCatch(granger_f(dY_u[ok], dX_u[ok], p_gran),
                      error=function(e) list(F=NA,p=NA))
    stars <- if (!is.na(gran$p)) sig_stars(gran$p) else "n/a"
    cat(sprintf("  %-26s  %+6d  %+8.4f  %-6s  %10.4f  %8.4f  %s\n",
                varnames[v], h_star, r_star, ifelse(sig,"YES","NO"),
                if(!is.na(gran$F))gran$F else NA,
                if(!is.na(gran$p))gran$p else NA, stars))
    rows[[v]] <- data.frame(Group=group, Variable=varnames[v],
                            h_star=h_star, r_at_hstar=round(r_star,4),
                            Significant=ifelse(sig,"YES","NO"),
                            Granger_F=round(if(!is.na(gran$F))gran$F else NA,4),
                            Granger_p=round(if(!is.na(gran$p))gran$p else NA,6),
                            Granger_result=stars, stringsAsFactors=FALSE)
    df_ccf <- data.frame(lag=h_ccf, r=ccf_v)
    plots[[v]] <- ggplot(df_ccf, aes(lag, r)) +
      geom_col(fill=color, width=0.6) +
      geom_hline(yintercept= ci95, linetype="dashed", color="red", linewidth=0.7) +
      geom_hline(yintercept=-ci95, linetype="dashed", color="red", linewidth=0.7) +
      geom_hline(yintercept=0, color="black", linewidth=0.4) +
      geom_vline(xintercept=0, linetype="dotted", linewidth=0.5) +
      annotate("point", x=h_star, y=r_star, color="black", size=2, shape=25, fill="black") +
      labs(title=sprintf("%s vs %s  (h*=%+d, r=%.3f)",
                         gsub("_","\n",varnames[v]), group, h_star, r_star),
           x="Lag h", y="CCF") +
      theme_bw(base_size=8) + theme(plot.title=element_text(size=7,face="bold"))
  }
  list(table=do.call(rbind,rows), plots=plots,
       nr=ceiling(sqrt(length(varnames))),
       nc=ceiling(length(varnames)/ceiling(sqrt(length(varnames)))))
}

cat("=== TESTS 4-5: CCF & GRANGER — EACH VARIABLE vs OWN COMPOSITE ===\n")
self_cli <- run_self_ccf(cli_cs$data, CLI, CLI_vars, "CLI", "#C0392B")
self_cci <- run_self_ccf(cci_cs$data, CCI, CCI_vars, "CCI", "#2471A3")
cat("\n")
## 5e  CLI anticipation test (CLI → CCI)
H_ant <- 24;  h_ant <- -H_ant:H_ant

# FIXED: Safely extracting common dates without losing the Date class
common_d  <- cli_cs$dates[cli_cs$dates %in% cci_cs$dates]

cli_align <- CLI[cli_cs$dates %in% common_d]
cci_align <- CCI[cci_cs$dates %in% common_d]
dCLI_a    <- diff(cli_align);  dCCI_a <- diff(cci_align)
T_ab      <- length(dCLI_a);   ci95_ant <- 1.96/sqrt(T_ab)
ccf_ant   <- compute_ccf_vec(dCLI_a, dCCI_a, H_ant)
opt_ix    <- H_ant + 1 + which.max(ccf_ant[(H_ant+1):(2*H_ant+1)]) - 1
opt_lead  <- h_ant[opt_ix];  max_r_ant <- ccf_ant[opt_ix]

# AIC lag selection (dynamically scaled for short samples)
max_lag <- min(12, max(1, floor((T_ab - 2) / 3)))

p_opt_ant <- which.min(sapply(1:max_lag, function(pp) {
  T_p <- T_ab - pp
  Y_p <- dCCI_a[(pp+1):T_ab]
  
  # Initialize matrix safely
  Xur <- cbind(1, matrix(0, T_p, 2*pp))
  
  for (lag in seq_len(pp)) {
    Xur[,lag+1]    <- dCCI_a[(pp+1-lag):(T_ab-lag)]
    Xur[,pp+lag+1] <- dCLI_a[(pp+1-lag):(T_ab-lag)]
  }
  
  # Try to fit the model, returning infinity if degrees of freedom fail
  tryCatch({
    r <- lm.fit(Xur, Y_p)$residuals
    log(sum(r^2)/T_p) + 2*(2*pp+1)/T_p
  }, error = function(e) Inf)
}))

gran_ant <- granger_f(dCCI_a, dCLI_a, p_opt_ant)

cat("=== TEST 6: ANTICIPATION TEST — CLI vs CCI ===\n")
cat(sprintf("  h* = %d months  r = %.4f  CI95 = ±%.4f\n", opt_lead, max_r_ant, ci95_ant))
cat(sprintf("  RESULT: CLI %s CCI\n",
            if(opt_lead>0) sprintf("leads by %d month(s)", opt_lead)
            else if(opt_lead==0) "is contemporaneous with"
            else "lags behind"))
cat(sprintf("  Granger CLI->CCI: F=%.4f  p=%.4f  lags=%d  %s\n\n",
            gran_ant$F, gran_ant$p, p_opt_ant, sig_stars(gran_ant$p)))
## 5f  Per-CLI-variable CCF vs CCI
cat("=== TEST 7: EACH CLI VARIABLE vs CCI COMPOSITE ===\n")
cat("Expected: h* > 0 (CLI variable leads CCI)\n")

# FIXED: Safely extracting common dates for the cross-correlation
common_cross <- cli_cs$dates[cli_cs$dates %in% cci_cs$dates]

A_sub <- cli_cs$data[cli_cs$dates %in% common_cross, , drop=FALSE]
B_dif <- diff(CCI[cci_cs$dates %in% common_cross])
T_cb  <- length(B_dif);  ci95_c <- 1.96/sqrt(T_cb)
cat(sprintf("\n  Common sample: %d obs  %-7s – %s\n  %-26s  %6s  %8s  %6s  %10s  %8s  %s\n",
            T_cb, min(common_cross[-1]), max(common_cross[-1]),
            "Variable","h*","r(h*)","Sig?","Granger-F","p-val","Stars"))
cat("  ", strrep("-",80), "\n", sep="")

cross_rows <- list();  cross_plots <- list()
for (v in seq_along(CLI_vars)) {
  dA   <- diff(A_sub[,v])
  T_u  <- min(length(dA), T_cb)
  dA_u <- dA[seq_len(T_u)];  dB_u <- B_dif[seq_len(T_u)]
  ok   <- !is.na(dA_u) & !is.na(dB_u)
  ccf_v <- compute_ccf_vec(dA_u[ok], dB_u[ok], H_ccf)
  ix    <- H_ccf + 1 + which.max(ccf_v[(H_ccf+1):(2*H_ccf+1)]) - 1
  h_star <- h_ccf[ix];  r_star <- ccf_v[ix]
  sig   <- abs(r_star) > ci95_c
  gran  <- tryCatch(granger_f(dB_u[ok], dA_u[ok], p_gran),
                    error=function(e) list(F=NA,p=NA))
  stars <- if (!is.na(gran$p)) sig_stars(gran$p) else "n/a"
  cat(sprintf("  %-26s  %+6d  %+8.4f  %-6s  %10.4f  %8.4f  %s\n",
              CLI_vars[v], h_star, r_star, ifelse(sig,"YES","NO"),
              if(!is.na(gran$F))gran$F else NA,
              if(!is.na(gran$p))gran$p else NA, stars))
  cross_rows[[v]] <- data.frame(CLI_Variable=CLI_vars[v], vs="CCI",
                                h_star=h_star, r_at_hstar=round(r_star,4),
                                Significant=ifelse(sig,"YES","NO"),
                                Granger_F=round(if(!is.na(gran$F))gran$F else NA,4),
                                Granger_p=round(if(!is.na(gran$p))gran$p else NA,6),
                                Granger_result=stars, stringsAsFactors=FALSE)
  df_ccf <- data.frame(lag=h_ccf, r=ccf_v)
  cross_plots[[v]] <- ggplot(df_ccf, aes(lag, r)) +
    geom_col(fill="#C0392B", width=0.6) +
    geom_hline(yintercept= ci95_c, linetype="dashed", color="red", linewidth=0.7) +
    geom_hline(yintercept=-ci95_c, linetype="dashed", color="red", linewidth=0.7) +
    geom_hline(yintercept=0, color="black", linewidth=0.4) +
    geom_vline(xintercept=0, linetype="dotted", linewidth=0.5) +
    annotate("point", x=h_star, y=r_star, color="black", size=2, shape=25, fill="black") +
    labs(title=sprintf("%s vs CCI  (h*=%+d, r=%.3f)",
                       gsub("_","\n",CLI_vars[v]), h_star, r_star),
         x="Lag h (h>0: CLI leads CCI)", y="CCF") +
    theme_bw(base_size=8) + theme(plot.title=element_text(size=7,face="bold"))
}
cross_tbl <- do.call(rbind, cross_rows)
cat("\n")
# ==============================================================================
# 6 — PLOTS
# ==============================================================================
cat("Generating plots...\n")

# Fig 1: dCLI and dCCI
p1a <- add_rec_shading(
  ggplot(data.frame(date=cli_cs$dates[-1], v=dCLI[-1]), aes(date,v)) +
    geom_line(color="#C0392B", linewidth=0.9) +
    geom_hline(yintercept=0, linetype="dashed", color="grey40", linewidth=0.5) +
    labs(title="ΔCLI — Composite Leading Indicator",
         subtitle=paste(CLI_vars, collapse=" | "), x="", y="ΔCLI") +
    theme_bw() + theme(plot.subtitle=element_text(size=7.5,color="grey40")),
  cli_cs$dates[-1])

p1b <- add_rec_shading(
  ggplot(data.frame(date=cci_cs$dates[-1], v=dCCI[-1]), aes(date,v)) +
    geom_line(color="#2471A3", linewidth=0.9) +
    geom_hline(yintercept=0, linetype="dashed", color="grey40", linewidth=0.5) +
    labs(title="ΔCCI — Composite Coincident Indicator",
         subtitle=paste(CCI_vars, collapse=" | "), x="Date", y="ΔCCI") +
    theme_bw() + theme(plot.subtitle=element_text(size=7.5,color="grey40")),
  cci_cs$dates[-1])

save_plot((p1a/p1b) + plot_annotation(
  title    = "France — CLI and CCI (First Differences)",
  subtitle = "Grey bands = French recessions  |  NS-DFM, Bujosa et al. (2013)  |  k=5",
  theme    = theme(plot.title=element_text(face="bold",size=13),
                   plot.subtitle=element_text(size=9,color="grey40"))),
  "Fig01_dCLI_dCCI.png", h=22)
cat("  Saved: Fig01_dCLI_dCCI.png\n")

# Load GDP (optional)
gdp_avail <- file.exists(gdp_file)
if (gdp_avail) {
  gdp_raw <- read.csv(gdp_file, stringsAsFactors=FALSE, na.strings="NA")
  if ("Date" %in% names(gdp_raw)) names(gdp_raw)[names(gdp_raw)=="Date"] <- "date"
  gcol <- grep("gdp|GDP|growth", names(gdp_raw), value=TRUE, ignore.case=TRUE)[1]
  if (!is.na(gcol)) names(gdp_raw)[names(gdp_raw)==gcol] <- "gdp_growth"
  gdp_raw$date <- as.Date(gdp_raw$date)
  gdp_raw <- gdp_raw[!is.na(gdp_raw$gdp_growth), c("date","gdp_growth")]
  q_of   <- function(d)
    as.Date(paste0(format(d,"%Y"),"-",sprintf("%02d",(as.integer(format(d,"%m"))-1)%/%3*3+1),"-01"))
  to_q   <- function(dates_m, dval) {
    df <- data.frame(q=q_of(dates_m[-1]), v=dval[-1])
    df <- df[!is.na(df$v),]
    aggregate(v~q, df, mean, na.rm=TRUE)
  }
  std_to <- function(x, ref)
    (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)*sd(ref,na.rm=TRUE)+mean(ref,na.rm=TRUE)
  cli_q <- to_q(cli_cs$dates, dCLI);  names(cli_q) <- c("date","dCLI")
  cci_q <- to_q(cci_cs$dates, dCCI);  names(cci_q) <- c("date","dCCI")
} else {
  cat("  NOTE: france_gdp.csv not found — GDP panels skipped.\n")
  cat("  Place file with columns: date (YYYY-MM-DD quarterly), gdp_growth (%)\n")
}

# Fig 2: CLI index (mean 2015-2019 = 100) + GDP
pCLI_lev <- add_rec_shading(
  ggplot(cli_index_df, aes(date, CLI_index)) +
    geom_line(color="#C0392B", linewidth=1) +
    geom_hline(yintercept=100, linetype="dashed", color="grey30", linewidth=0.7) +
    labs(title="CLI — Index (avg. start–2019 = 100)", x="", y="CLI index") + theme_bw(),
  cli_index_df$date)

if (gdp_avail) {
  m <- merge(gdp_raw, cli_q, by="date")
  m$cli_s <- std_to(m$dCLI, m$gdp_growth)
  df_long <- pivot_longer(m[,c("date","gdp_growth","cli_s")],
                          -date, names_to="s", values_to="v")
  df_long$s <- factor(df_long$s, levels=c("gdp_growth","cli_s"),
                      labels=c("GDP growth (q-o-q %)","ΔCLI (standardised)"))
  pCLI_gdp <- add_rec_shading(
    ggplot(df_long, aes(date,v,color=s,linewidth=s)) + geom_line() +
      geom_hline(yintercept=0,linetype="dotted",color="grey50",linewidth=0.4) +
      scale_color_manual(values=c("GDP growth (q-o-q %)"="black","ΔCLI (standardised)"="#C0392B")) +
      scale_linewidth_manual(values=c("GDP growth (q-o-q %)"=1.1,"ΔCLI (standardised)"=0.85)) +
      labs(title="ΔCLI vs GDP growth (standardised)",x="Date",y="%",color="",linewidth="") +
      theme_bw() + theme(legend.position="bottom"),
    m$date)
  save_plot((pCLI_lev/pCLI_gdp)+plot_annotation(title="CLI — France",
    theme=theme(plot.title=element_text(face="bold",size=13))),
    "Fig02_CLI_levels.png", h=20)
} else {
  save_plot(pCLI_lev, "Fig02_CLI_levels.png", h=12)
}
cat("  Saved: Fig02_CLI_levels.png\n")

# Fig 3: CCI index (avg. start–2019 = 100) + GDP
pCCI_lev <- add_rec_shading(
  ggplot(cci_index_df, aes(date, CCI_index)) +
    geom_line(color="#2471A3", linewidth=1) +
    geom_hline(yintercept=100, linetype="dashed", color="grey30", linewidth=0.7) +
    labs(title="CCI — Index (avg. start–2019 = 100)", x="", y="CCI index") + theme_bw(),
  cci_index_df$date)

if (gdp_avail) {
  m2 <- merge(gdp_raw, cci_q, by="date")
  m2$cci_s <- std_to(m2$dCCI, m2$gdp_growth)
  df_long2 <- pivot_longer(m2[,c("date","gdp_growth","cci_s")],
                           -date, names_to="s", values_to="v")
  df_long2$s <- factor(df_long2$s, levels=c("gdp_growth","cci_s"),
                       labels=c("GDP growth (q-o-q %)","ΔCCI (standardised)"))
  pCCI_gdp <- add_rec_shading(
    ggplot(df_long2, aes(date,v,color=s,linewidth=s)) + geom_line() +
      geom_hline(yintercept=0,linetype="dotted",color="grey50",linewidth=0.4) +
      scale_color_manual(values=c("GDP growth (q-o-q %)"="black","ΔCCI (standardised)"="#2471A3")) +
      scale_linewidth_manual(values=c("GDP growth (q-o-q %)"=1.1,"ΔCCI (standardised)"=0.85)) +
      labs(title="ΔCCI vs GDP growth (standardised)",x="Date",y="%",color="",linewidth="") +
      theme_bw() + theme(legend.position="bottom"),
    m2$date)
  save_plot((pCCI_lev/pCCI_gdp)+plot_annotation(title="CCI — France",
    theme=theme(plot.title=element_text(face="bold",size=13))),
    "Fig03_CCI_levels.png", h=20)
} else {
  save_plot(pCCI_lev, "Fig03_CCI_levels.png", h=12)
}
cat("  Saved: Fig03_CCI_levels.png\n")

# Fig 4: CLI + CCI vs GDP combined
if (gdp_avail) {
  mb <- Reduce(function(a,b) merge(a,b,by="date"),
               list(gdp_raw,
                    setNames(cli_q,c("date","dCLI")),
                    setNames(cci_q,c("date","dCCI"))))
  mb$cli_s <- std_to(mb$dCLI, mb$gdp_growth)
  mb$cci_s <- std_to(mb$dCCI, mb$gdp_growth)
  df_b <- pivot_longer(mb[,c("date","gdp_growth","cli_s","cci_s")],
                       -date, names_to="s", values_to="v")
  df_b$s <- factor(df_b$s, levels=c("gdp_growth","cli_s","cci_s"),
                   labels=c("GDP growth","ΔCLI (std)","ΔCCI (std)"))
  p4 <- add_rec_shading(
    ggplot(df_b, aes(date,v,color=s,linewidth=s,linetype=s)) + geom_line() +
      geom_hline(yintercept=0,linetype="dotted",color="grey50",linewidth=0.4) +
      scale_color_manual(values=c("GDP growth"="black","ΔCLI (std)"="#C0392B","ΔCCI (std)"="#2471A3")) +
      scale_linewidth_manual(values=c("GDP growth"=1.2,"ΔCLI (std)"=0.85,"ΔCCI (std)"=0.85)) +
      scale_linetype_manual(values=c("GDP growth"="solid","ΔCLI (std)"="solid","ΔCCI (std)"="dashed")) +
      labs(title="CLI & CCI vs GDP Growth — France",
           subtitle="All series standardised to GDP scale  |  Grey = recessions",
           x="Date", y="GDP growth (%) / standardised indicator",
           color="",linewidth="",linetype="") +
      theme_bw() + theme(legend.position="bottom",
                         plot.title=element_text(face="bold",size=12),
                         plot.subtitle=element_text(size=9,color="grey40")),
    mb$date)
  save_plot(p4, "Fig04_CLI_CCI_vs_GDP.png", h=14)
  cat("  Saved: Fig04_CLI_CCI_vs_GDP.png\n")
}

# Fig 5: Anticipation CCF
df5 <- data.frame(lag=h_ant, r=ccf_ant)
p5  <- ggplot(df5, aes(lag,r)) +
  geom_col(fill="#5D6D7E", width=0.7) +
  geom_hline(yintercept= ci95_ant,linetype="dashed",color="red",linewidth=0.8) +
  geom_hline(yintercept=-ci95_ant,linetype="dashed",color="red",linewidth=0.8) +
  geom_hline(yintercept=0,color="black",linewidth=0.5) +
  annotate("point",x=opt_lead,y=max_r_ant,color="red",size=4,shape=25,fill="red") +
  labs(title=sprintf("Anticipation Test: CLI leads CCI by h*=%d months  (r=%.3f)",
                     opt_lead, max_r_ant),
       subtitle=sprintf("Granger CLI→CCI: F=%.4f  p=%.4f  %s  (lags=%d AIC)",
                        gran_ant$F, gran_ant$p, sig_stars(gran_ant$p), p_opt_ant),
       x="Lead h (months)  |  h>0: CLI leads CCI", y="Cross-correlation") +
  theme_bw()+theme(plot.title=element_text(face="bold",size=11),
                   plot.subtitle=element_text(size=9,color="grey40"))
save_plot(p5, "Fig05_CCF_CLI_vs_CCI.png", w=22, h=12)
cat("  Saved: Fig05_CCF_CLI_vs_CCI.png\n")

# Helper: save CCF panel figure
save_panels <- function(plots, nr, nc, fname, title_str, w=28, h=18) {
  p_all <- wrap_plots(plots, nrow=nr, ncol=nc) +
    plot_annotation(title=title_str,
                    theme=theme(plot.title=element_text(face="bold",size=12)))
  save_plot(p_all, fname, w=w, h=h)
}

save_panels(self_cli$plots, self_cli$nr, self_cli$nc,
            "Fig06_CCF_CLI_vars_vs_CLI.png",
            "CCF: Each CLI Variable vs CLI Composite  (h*≈0 = internal coherence)")
cat("  Saved: Fig06_CCF_CLI_vars_vs_CLI.png\n")

save_panels(self_cci$plots, self_cci$nr, self_cci$nc,
            "Fig07_CCF_CCI_vars_vs_CCI.png",
            "CCF: Each CCI Variable vs CCI Composite  (h*≈0 = coincident behaviour)")
cat("  Saved: Fig07_CCF_CCI_vars_vs_CCI.png\n")

nr_c <- ceiling(sqrt(length(CLI_vars)))
nc_c <- ceiling(length(CLI_vars)/nr_c)
save_panels(cross_plots, nr_c, nc_c,
            "Fig08_CCF_CLI_vars_vs_CCI.png",
            "CCF: Each CLI Variable vs CCI Composite  (h*>0 = CLI leads CCI)")
cat("  Saved: Fig08_CCF_CLI_vars_vs_CCI.png\n")

# ==============================================================================
# 7 — EXCEL DIAGNOSTICS
# ==============================================================================
cat("\nWriting Excel diagnostics...\n")
wb  <- createWorkbook()
hdr <- createStyle(textDecoration="bold", fgFill="#D6E4F0",
                   border="Bottom", borderStyle="medium")
add_ws <- function(wb, name, df) {
  addWorksheet(wb, name)
  writeData(wb, name, df)
  addStyle(wb, name, hdr, rows=1, cols=seq_len(ncol(df)), gridExpand=TRUE)
  setColWidths(wb, name, cols=seq_len(ncol(df)), widths="auto")
}

add_ws(wb, "Metadata", data.frame(
  Item=c("CLI variables","CLI sample","CLI obs",
         "CCI variables","CCI sample","CCI obs",
         "Methodology","k (cross-covariance lag)","Reference"),
  Value=c(paste(CLI_vars,collapse="; "),
          paste(cli_cs$start,"-",cli_cs$end), cli_cs$n,
          paste(CCI_vars,collapse="; "),
          paste(cci_cs$start,"-",cci_cs$end), cci_cs$n,
          "NS-DFM dominant eigenvector of C_y(k) = B·B(k)'/T",
          K_LAG,
          "Bujosa, Garcia-Ferrer & de Juan (2013); Pena & Poncela (2006)"),
  stringsAsFactors=FALSE))

add_ws(wb, "Weights", data.frame(
  Group=c(rep("CLI",length(CLI_vars)),rep("CCI",length(CCI_vars))),
  Variable=c(CLI_vars,CCI_vars),
  Weight=round(c(cli_res$weights,cci_res$weights),6),
  stringsAsFactors=FALSE))

add_ws(wb, "Bartlett_Test",       bt_tbl)
add_ws(wb, "Eigenvalue_Shares",   eig_tbl)
add_ws(wb, "Corr_with_Composite", corr_tbl)
add_ws(wb, "CCF_CLIvars_vs_CLI",  self_cli$table)
add_ws(wb, "CCF_CCIvars_vs_CCI",  self_cci$table)
add_ws(wb, "CCF_CLIvars_vs_CCI",  cross_tbl)

add_ws(wb, "Anticipation_Test", data.frame(
  Metric=c("Common sample obs","Optimal lead h* (months)",
           "Max r at h*","95% CI","CLI leads CCI?",
           "Granger F","Granger p","Granger lags (AIC)","Granger result"),
  Value=c(T_ab, opt_lead, round(max_r_ant,4), round(ci95_ant,4),
          ifelse(opt_lead>0,"YES","NO"),
          round(gran_ant$F,4), round(gran_ant$p,6),
          p_opt_ant, sig_stars(gran_ant$p)),
  stringsAsFactors=FALSE))

# CLI and CCI series
add_ws(wb, "CLI_series",
  setNames(cbind(data.frame(date=cli_cs$dates,CLI=round(CLI,6),dCLI=round(dCLI,6)),
                 as.data.frame(round(cli_cs$data,6))),
           c("date","CLI","dCLI",paste0(CLI_vars,"_trend"))))

# CLI & CCI indexes saved as dedicated, standalone sheets
add_ws(wb, "CLI_index_avg2019",
  data.frame(date      = cli_index_df$date,
             CLI_index = round(cli_index_df$CLI_index, 6),
             stringsAsFactors = FALSE))

add_ws(wb, "CCI_index_avg2019",
  data.frame(date      = cci_index_df$date,
             CCI_index = round(cci_index_df$CCI_index, 6),
             stringsAsFactors = FALSE))

add_ws(wb, "CCI_series",
  setNames(cbind(data.frame(date=cci_cs$dates,CCI=round(CCI,6),dCCI=round(dCCI,6)),
                 as.data.frame(round(cci_cs$data,6))),
           c("date","CCI","dCCI",paste0(CCI_vars,"_trend"))))

excel_out <- file.path(out_dir, "Diagnostics_CLI_CCI.xlsx")
saveWorkbook(wb, excel_out, overwrite=TRUE)
cat(sprintf("  Excel saved: %s\n", excel_out))

# ==============================================================================
# 8 — SUMMARY
# ==============================================================================
cat("\n",strrep("=",60),"\n  DONE\n",strrep("=",60),"\n",sep="")
cat(sprintf("  CLI: %d obs  %s – %s\n", cli_cs$n, cli_cs$start, cli_cs$end))
cat(sprintf("  CCI: %d obs  %s – %s\n", cci_cs$n, cci_cs$start, cci_cs$end))
cat(sprintf("  Anticipation: CLI leads CCI by %d month(s)  (r=%.3f, %s)\n",
            opt_lead, max_r_ant, sig_stars(gran_ant$p)))
cat(sprintf("  Output folder: %s\n", out_dir))
cat(strrep("=",60),"\n",sep="")
