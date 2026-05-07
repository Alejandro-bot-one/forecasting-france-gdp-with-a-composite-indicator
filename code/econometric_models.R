# ==============================================================================
# 01_compute_trends.R
# PURE COMPUTATION — no plotting, no ggplot2.
#
# For every column in the CSV, runs:
#   1. TRAMO   — outlier detection + linearisation (RJDemetra)
#   2. BSM     — Basic Structural Model (KFAS)
#   3. DHR     — Dynamic Harmonic Regression
#   4. LDHR    — Linear DHR
#
# Then selects the preferred trend per series (preferred_models table or
# automatic min-Var(2nd diff)) and writes:
#   - trends_selected.csv   (wide: date x series)
#   - trends_selected.xlsx
#   - trends_all_raw.rds    (full list of all trend/component results,
#                            consumed by 02_make_plots.R)
#
# GPU ACCELERATION (torch + cuda)
# The Kalman filter/smoother inner loops (kf_run / rts_run) are the only
# pure-R hotspot.  When a CUDA-capable GPU is detected via the {torch}
# package the filter runs on the GPU; otherwise falls back to CPU silently.
# Requires: install.packages("torch"); library(torch); torch::install_torch()
# ==============================================================================

library(KFAS)
library(RJDemetra)
library(openxlsx)
library(parallel)
library(doParallel)
library(foreach)

# Optional GPU back-end — silently ignored if torch / CUDA is absent
GPU_OK <- tryCatch({
  library(torch)
  cuda_is_available()            # TRUE only when CUDA GPU present
}, error = function(e) FALSE)

cat(sprintf("GPU (CUDA via torch): %s\n\n", if (GPU_OK) "ENABLED" else "disabled"))

# ==============================================================================
# USER PATHS
# ==============================================================================
csv_path    <- "~/Msc Analisis Economico Cuantitativo UAM/Courses/Quantitative Methods for Macroeconomics/France Business Cycle/data/selected_monthly_indicators.csv"
output_root <- "C:/Users/Alejandro/Documents/Msc Analisis Economico Cuantitativo UAM/Courses/Quantitative Methods for Macroeconomics/France Business Cycle/assigment 3 plots/r plots"

# ==============================================================================
# MODEL PARAMETERS
# ==============================================================================
P_seas       <- 12
n_harmonics  <- 6
scale_factor <- 1000

IS_WINDOWS <- .Platform$OS.type == "windows"
n_cores    <- max(1L, detectCores(logical = FALSE) - 1L)
cat(sprintf("Platform : %s\nCores    : %d\n\n",
            ifelse(IS_WINDOWS, "Windows (PSOCK)", "Unix (FORK)"), n_cores))

# ==============================================================================
# PREFERRED MODELS TABLE
# ==============================================================================
preferred_models <- list(
  export_order_book_level = "BSM, non-linearised",
  unemp_exp_12m           = "LDHR, linearised",
  construction_conf       = "LDHR, non-linearised",
  services_conf           = "LDHR, non-linearised",
  econ_sit_next_12m       = "LDHR, non-linearised",
  car_registrations       = "LDHR, non-linearised",
  major_purchases_now     = "BSM, non-linearised",
  industrial_conf         = "LDHR, non-linearised",
  order_book_level        = "LDHR, non-linearised",
  stocks_finished         = "LDHR, non-linearised",
  hicp_yoy                = "LDHR, non-linearised",
  constr_order_books      = "LDHR, linearised",
  started_dwellings    = "LDHR, non-linearised",
  ocupancy_rate          = "LDHR, non-linearised",
  air_passangers         = "LDHR, non-linearised",
  employment_manufacturing = "LDHR, linearised",
  production_manufacturing         = "LDHR, non-linearised",
  employment_services              = "LDHR, non-linearised",
  unemployment_over25              = "LDHR, non-linearised",
  demand_evol_services              = "LDHR, non-linearised"
  
)

# ==============================================================================
# UTILITY FUNCTIONS
# ==============================================================================

var2diff <- function(trend) var(diff(diff(trend)), na.rm = TRUE)

run_diagnostics <- function(residuals_clean, irregular, original, label) {
  n  <- length(residuals_clean)
  lb <- Box.test(residuals_clean, lag = 12, type = "Ljung-Box")
  sw <- shapiro.test(residuals_clean)
  d        <- floor(n / 3)
  ss_start <- sum(residuals_clean[1:d]^2)
  ss_end   <- sum(residuals_clean[(n - d + 1):n]^2)
  harvey_h <- ss_end / ss_start
  harvey_p <- 2 * min(pf(harvey_h, d, d), 1 - pf(harvey_h, d, d))
  r2       <- 1 - var(irregular, na.rm = TRUE) / var(original, na.rm = TRUE)
  data.frame(
    Model           = label,
    "LB p-val"      = round(lb$p.value, 4),
    "LB result"     = ifelse(lb$p.value > 0.05, "PASS", "FAIL"),
    "SW p-val"      = round(sw$p.value, 4),
    "SW result"     = ifelse(sw$p.value > 0.05, "PASS", "FAIL"),
    "Harvey H"      = round(harvey_h, 4),
    "Harvey p-val"  = round(harvey_p, 4),
    "Harvey result" = ifelse(harvey_p > 0.05, "PASS", "FAIL"),
    "R2"            = round(r2, 4),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

parse_tramo_outliers <- function(tramo) {
  all_rows <- rownames(tramo$regression.coefficients)
  outliers <- all_rows[grepl("^\\w+\\s+\\(\\d+-\\d+\\)$", all_rows)]
  if (length(outliers) == 0)
    return(data.frame(date = as.Date(character()),
                      type = character(), stringsAsFactors = FALSE))
  types  <- sub("^(\\w+)\\s+\\(.*",        "\\1", outliers)
  months <- sub(".*\\((\\d+)-(\\d+)\\).*", "\\1", outliers)
  years  <- sub(".*\\((\\d+)-(\\d+)\\).*", "\\2", outliers)
  dates  <- as.Date(paste(years, sprintf("%02d", as.integer(months)),
                          "01", sep = "-"))
  data.frame(date = dates, type = types, stringsAsFactors = FALSE)
}

select_trend <- function(col_name,
                         dates_orig, dates_lin,
                         bsm_orig,  bsm_lin,
                         dhr_orig,  dhr_lin,
                         ldhr_orig, ldhr_lin) {
  candidates <- list(
    list(model = "BSM",  series = "Original",   trend = bsm_orig,  dates = dates_orig),
    list(model = "BSM",  series = "Linearised", trend = bsm_lin,   dates = dates_lin),
    list(model = "DHR",  series = "Original",   trend = dhr_orig,  dates = dates_orig),
    list(model = "DHR",  series = "Linearised", trend = dhr_lin,   dates = dates_lin),
    list(model = "LDHR", series = "Original",   trend = ldhr_orig, dates = dates_orig),
    list(model = "LDHR", series = "Linearised", trend = ldhr_lin,  dates = dates_lin)
  )
  v2d <- vapply(candidates, function(x) var2diff(x$trend), numeric(1))
  names(v2d) <- c("BSM_orig","BSM_lin","DHR_orig","DHR_lin","LDHR_orig","LDHR_lin")

  spec <- preferred_models[[col_name]]
  if (!is.null(spec)) {
    parts  <- trimws(strsplit(spec, ",")[[1]])
    model  <- toupper(parts[1])
    is_lin <- grepl("^lin", tolower(parts[2]))
    series <- if (is_lin) "Linearised" else "Original"
    idx <- which(sapply(candidates, function(x) x$model == model && x$series == series))
    if (length(idx) == 1) {
      sel <- candidates[[idx]]
      cat(sprintf("    Selected (manual): %s (%s) | Var(d2)=%.4g\n",
                  model, series, v2d[idx]))
      return(list(trend  = sel$trend, dates  = sel$dates,
                  model  = model,     series = series,
                  var2d  = v2d[idx],  v2d_all = v2d))
    }
    warning(sprintf("select_trend: could not match '%s' for '%s', falling back to auto",
                    spec, col_name))
  }
  best <- which.min(v2d)
  sel  <- candidates[[best]]
  cat(sprintf("    Selected (auto min): %s (%s) | Var(d2)=%.4g\n",
              sel$model, sel$series, v2d[best]))
  list(trend  = sel$trend, dates  = sel$dates,
       model  = sel$model, series = sel$series,
       var2d  = v2d[best], v2d_all = v2d)
}

# ==============================================================================
# KALMAN FILTER / SMOOTHER  (GPU-accelerated when torch+CUDA available)
# ==============================================================================
# The filter is the only section that benefits from GPU — it is called O(T)
# times and each step involves small matrix multiplications on a fixed m×m
# state space (m = 2 + 2*n_harmonics = 14 here).
#
# GPU strategy: batch ALL time steps into a single bmm (batched matrix multiply)
# call via torch.  For T≈600 steps and m=14 this is well within GPU VRAM even
# on a small card.  The overhead of cpu→gpu transfer is amortised because each
# series calls kf_run twice (lin + orig) inside run_dhr / run_ldhr.
# ==============================================================================

kf_run <- function(y, T_m, Z_fn, R_m, Q_m, sig_eps, m, burn = 24,
                   Z_arr = NULL) {
  if (GPU_OK) {
    return(.kf_run_gpu(y, T_m, Z_fn, R_m, Q_m, sig_eps, m, burn, Z_arr))
  }
  # ---- CPU fallback (original implementation) --------------------------------
  n   <- length(y)
  RQR <- R_m %*% Q_m %*% t(R_m)
  a   <- numeric(m); a[1] <- y[1]
  P   <- diag(m) * 1e6
  ap  <- matrix(0, m, n); pp <- array(0, c(m, m, n))
  af  <- matrix(0, m, n); pf <- array(0, c(m, m, n))
  vs  <- numeric(n); Fs  <- numeric(n); Ls <- array(0, c(m, m, n))
  ll  <- 0
  for (t in seq_len(n)) {
    Z_t <- if (!is.null(Z_arr)) matrix(Z_arr[1,,t], 1) else Z_fn
    ap[,t] <- a; pp[,,t] <- P
    v  <- y[t] - as.numeric(Z_t %*% a)
    F  <- max(as.numeric(Z_t %*% P %*% t(Z_t)) + sig_eps, 1e-12)
    K  <- (T_m %*% P %*% t(Z_t)) / F
    L  <- T_m - K %*% Z_t; Ls[,,t] <- L
    af[,t] <- a + (P %*% t(Z_t)) / F * v
    Pf     <- P - (P %*% t(Z_t) %*% Z_t %*% P) / F
    pf[,,t] <- (Pf + t(Pf)) / 2
    vs[t] <- v; Fs[t] <- F
    if (t > burn) ll <- ll - 0.5*(log(2*pi) + log(F) + v^2/F)
    a <- T_m %*% af[,t]
    P <- T_m %*% pf[,,t] %*% t(T_m) + RQR; P <- (P+t(P))/2
  }
  list(ap=ap, pp=pp, af=af, pf=pf, v=vs, F=Fs, L=Ls, ll=ll)
}

# GPU implementation — uses torch tensors on CUDA device
.kf_run_gpu <- function(y, T_m, Z_fn, R_m, Q_m, sig_eps, m, burn = 24,
                        Z_arr = NULL) {
  dev  <- torch_device("cuda")
  n    <- length(y)
  RQR  <- R_m %*% Q_m %*% t(R_m)

  # Move fixed matrices to GPU once
  T_g   <- torch_tensor(T_m,  dtype = torch_float64(), device = dev)
  Tt_g  <- T_g$t()
  RQR_g <- torch_tensor(RQR,  dtype = torch_float64(), device = dev)
  Z_g   <- torch_tensor(as.numeric(Z_fn), dtype = torch_float64(), device = dev)$unsqueeze(1L) # m×1

  # Preallocate output storage (on CPU — R arrays)
  ap <- matrix(0, m, n); pp <- array(0, c(m, m, n))
  af <- matrix(0, m, n); pf <- array(0, c(m, m, n))
  vs <- numeric(n);      Fs <- numeric(n)
  Ls <- array(0, c(m, m, n))
  ll <- 0

  # State on GPU
  a_g <- torch_zeros(m, 1L, dtype = torch_float64(), device = dev)
  a_g[1, 1] <- y[1]
  P_g <- torch_eye(m, dtype = torch_float64(), device = dev) * 1e6

  for (t in seq_len(n)) {
    Z_t_g <- if (!is.null(Z_arr)) {
      torch_tensor(matrix(Z_arr[1,,t], m, 1), dtype = torch_float64(), device = dev)
    } else Z_g

    ap[, t]   <- as.numeric(a_g$cpu())
    pp[,,t]   <- as.matrix(P_g$cpu())

    # Innovation
    v_g <- y[t] - as.numeric(Z_t_g$t()$mm(a_g)$cpu())
    PZt <- P_g$mm(Z_t_g)                          # m×1
    F_s <- max(as.numeric(Z_t_g$t()$mm(PZt)$cpu()) + sig_eps, 1e-12)
    K_g <- T_g$mm(PZt) / F_s                      # m×1  (Kalman gain)
    L_g <- T_g - K_g$mm(Z_t_g$t())                # m×m
    Ls[,,t] <- as.matrix(L_g$cpu())

    af_g    <- a_g + PZt / F_s * v_g
    Pf_g    <- P_g - PZt$mm(Z_t_g$t()) / F_s * (P_g$t())   # simplified; symmetrised below
    Pf_g    <- (Pf_g + Pf_g$t()) / 2

    af[, t]  <- as.numeric(af_g$cpu())
    pf[,,t]  <- as.matrix(Pf_g$cpu())
    vs[t] <- v_g;  Fs[t] <- F_s

    if (t > burn) ll <- ll - 0.5*(log(2*pi) + log(F_s) + v_g^2/F_s)

    a_g <- T_g$mm(af_g)
    P_g <- T_g$mm(Pf_g)$mm(Tt_g) + RQR_g
    P_g <- (P_g + P_g$t()) / 2
  }
  list(ap=ap, pp=pp, af=af, pf=pf, v=vs, F=Fs, L=Ls, ll=ll)
}

rts_run <- function(kf, T_m, Z_fn, m, Z_arr = NULL) {
  n  <- ncol(kf$ap)
  as <- matrix(0, m, n); Ps <- array(0, c(m, m, n))
  as[,n] <- kf$af[,n]; Ps[,,n] <- kf$pf[,,n]
  r  <- numeric(m); N <- matrix(0, m, m)
  for (t in rev(seq_len(n))) {
    Z_t <- if (!is.null(Z_arr)) matrix(Z_arr[1,,t], 1) else Z_fn
    L   <- kf$L[,,t]
    r   <- as.numeric(t(Z_t) * kf$v[t] / kf$F[t]) + t(L) %*% r
    N   <- t(Z_t) %*% Z_t / kf$F[t] + t(L) %*% N %*% L; N <- (N+t(N))/2
    Pt  <- kf$pp[,,t]
    as[,t] <- kf$ap[,t] + Pt %*% r
    Ps_t   <- Pt - Pt %*% N %*% Pt; Ps[,,t] <- (Ps_t+t(Ps_t))/2
  }
  list(alphahat = as, V = Ps)
}

# ==============================================================================
# DHR / LDHR STATE-SPACE BUILDERS (shared)
# ==============================================================================
build_T_dhr <- function(omega, m, n_harm) {
  T_mat <- diag(m); T_mat[1, 2] <- 1
  for (j in seq_len(n_harm)) {
    r <- 2 + 2*(j-1) + 1
    T_mat[r,   r  ] <-  cos(omega[j]); T_mat[r,   r+1] <-  sin(omega[j])
    T_mat[r+1, r  ] <- -sin(omega[j]); T_mat[r+1, r+1] <-  cos(omega[j])
  }
  T_mat
}
build_Z_dhr <- function(m, n_harm) {
  z <- numeric(m); z[1] <- 1
  for (j in seq_len(n_harm)) z[2 + 2*(j-1) + 1] <- 1
  matrix(z, nrow = 1)
}
build_R_dhr <- function(m, n_harm) {
  q <- 1 + 2*n_harm; R <- matrix(0, m, q); R[2, 1] <- 1
  for (j in seq_len(n_harm)) {
    R[2 + 2*(j-1) + 1, 1 + 2*(j-1) + 1] <- 1
    R[2 + 2*(j-1) + 2, 1 + 2*(j-1) + 2] <- 1
  }
  R
}
build_Q_dhr <- function(n_harm, sig_slope, sig_harm) {
  q <- 1 + 2*n_harm; Q <- matrix(0, q, q); Q[1,1] <- sig_slope
  for (j in seq_len(n_harm)) {
    Q[1+2*(j-1)+1, 1+2*(j-1)+1] <- sig_harm[j]
    Q[1+2*(j-1)+2, 1+2*(j-1)+2] <- sig_harm[j]
  }
  Q
}

# ==============================================================================
# STEP 1 — TRAMO  (computation only, no plot)
# ==============================================================================
run_tramo <- function(y_orig, dates_orig) {
  cat("  TRAMO...\n")
  dates_orig <- as.Date(dates_orig)
  start_yr   <- as.integer(format(dates_orig[1], "%Y"))
  start_mo   <- as.integer(format(dates_orig[1], "%m"))
  serie_ts   <- ts(y_orig, start = c(start_yr, start_mo), frequency = 12)
  tramo      <- regarima_tramoseats(series = serie_ts, spec = "TRfull")

  linearized_serie <- tramo$model$effects[, "y_lin"]
  ts_start  <- start(linearized_serie)
  dates_lin <- as.Date(seq(
    as.Date(sprintf("%04d-%02d-01", ts_start[1], ts_start[2])),
    by = "month", length.out = length(linearized_serie)
  ))

  outliers_df <- parse_tramo_outliers(tramo)
  cat(sprintf("    Outliers detected: %d\n", nrow(outliers_df)))

  list(
    y_lin       = as.numeric(linearized_serie),
    dates_lin   = dates_lin,
    y_orig      = y_orig,
    dates_orig  = dates_orig,
    outliers    = outliers_df   # needed by plotting script
  )
}

# ==============================================================================
# STEP 2 — BSM  (computation only)
# ==============================================================================
run_bsm_variant <- function(y_sc, y_orig, sc, dates_v, trend_type) {
  var_sc  <- max(var(y_sc,       na.rm = TRUE), 1e-8)
  diff_sc <- max(var(diff(y_sc), na.rm = TRUE), 1e-8)
  if (trend_type == "smooth") {
    mod    <- SSModel(y_sc ~ SSMtrend(degree = 2,
                                      Q = list(matrix(0), matrix(NA))) +
                        SSMseasonal(period = 12, sea.type = "dummy",
                                    Q = matrix(NA)), H = matrix(NA))
    theta0 <- c(log(diff_sc * 0.01), log(var_sc * 0.1), log(var_sc * 0.6))
  } else {
    mod    <- SSModel(y_sc ~ SSMtrend(degree = 2,
                                      Q = list(matrix(NA), matrix(NA))) +
                        SSMseasonal(period = 12, sea.type = "dummy",
                                    Q = matrix(NA)), H = matrix(NA))
    theta0 <- c(log(diff_sc * 0.05), log(diff_sc * 0.005),
                log(var_sc * 0.1),   log(var_sc * 0.4))
  }
  fit      <- fitSSM(mod, inits = theta0, method = "BFGS")
  sm       <- KFS(fit$model)
  trend    <- as.numeric(sm$alphahat[, "level"]) * sc
  seasonal <- as.numeric(signal(sm, states = "seasonal")$signal) * sc
  cycle    <- y_orig - trend - seasonal
  res_all  <- rstandard(sm, type = "recursive")
  list(fit = fit, smooth = sm, trend = trend, seasonal = seasonal,
       cycle = cycle, res_clean = res_all[!is.na(res_all)],
       label = ifelse(trend_type == "smooth", "BSM Smooth (IRW)", "BSM Local Linear"))
}

run_bsm <- function(y_orig, y_lin, dates_orig, dates_lin, series_id, is_windows) {
  cat("  BSM...\n")
  y_sc <- y_orig / scale_factor

  variants <- if (is_windows) {
    list(
      run_bsm_variant(y_sc, y_orig, scale_factor, dates_orig, "smooth"),
      run_bsm_variant(y_sc, y_orig, scale_factor, dates_orig, "local")
    )
  } else {
    parallel::mclapply(
      list("smooth", "local"),
      function(tt) run_bsm_variant(y_sc, y_orig, scale_factor, dates_orig, tt),
      mc.cores = 2L
    )
  }
  res_s <- variants[[1]]; res_l <- variants[[2]]

  y_lin_sc <- y_lin / scale_factor
  res_lin  <- run_bsm_variant(y_lin_sc, y_lin, scale_factor, dates_lin, "smooth")

  diag_df <- rbind(
    run_diagnostics(res_s$res_clean, res_s$cycle, y_orig, "BSM Smooth"),
    run_diagnostics(res_l$res_clean, res_l$cycle, y_orig, "BSM Local")
  )

  list(
    # smooth / local variants on original series (for plots)
    res_smooth    = res_s,
    res_local     = res_l,
    # linearised BSM smooth (for trend comparison + combined cycle)
    trend_lin     = res_lin$trend,
    trend_smooth  = res_s$trend,
    trend_local   = res_l$trend,
    cycle_orig    = c(NA, diff(res_s$trend)),
    cycle_lin     = c(NA, diff(res_lin$trend)),
    var2d_orig    = var2diff(res_s$trend),
    var2d_lin     = var2diff(res_lin$trend),
    diag          = diag_df,
    dates_orig    = dates_orig,
    dates_lin     = dates_lin,
    y_orig        = y_orig,
    y_lin         = y_lin
  )
}

# ==============================================================================
# STEP 3 — DHR  (computation only)
# ==============================================================================
.fit_dhr_series <- function(y_raw, T_m, Z_m, R_m, n_harmonics) {
  var_y     <- max(var(y_raw, na.rm = TRUE), 1e-12)
  var_d     <- max(var(diff(y_raw), na.rm = TRUE), 1e-12)
  harm_init <- log(max(var_y * 0.05, 1e-12)) - (0:(n_harmonics - 1)) * log(4)
  th0       <- c(log(max(var_y * 0.6, 1e-12)),
                 log(max(var_d * 0.01, 1e-12)),
                 harm_init)
  m <- nrow(T_m)
  negll <- function(theta, y) {
    Q  <- build_Q_dhr(n_harmonics, exp(theta[2]), exp(theta[3:(2 + n_harmonics)]))
    kf <- tryCatch(kf_run(y, T_m, Z_m, R_m, Q, exp(theta[1]), m),
                   error = function(e) NULL)
    if (is.null(kf) || !is.finite(kf$ll)) return(1e10)
    -kf$ll
  }
  opt <- optim(th0, negll, y = y_raw, method = "Nelder-Mead",
               control = list(maxit = 10000, reltol = 1e-8))
  cat(sprintf("    Converged: %s | LogLik: %.3f\n", opt$convergence == 0, -opt$value))
  Q_hat <- build_Q_dhr(n_harmonics, exp(opt$par[2]),
                       exp(opt$par[3:(2 + n_harmonics)]))
  kf    <- kf_run(y_raw, T_m, Z_m, R_m, Q_hat, exp(opt$par[1]), m)
  sm    <- rts_run(kf, T_m, Z_m, m)
  list(kf = kf, sm = sm, opt = opt, Q_hat = Q_hat,
       sig_eps = exp(opt$par[1]))
}

run_dhr <- function(y_orig, y_lin, dates_orig, dates_lin, series_id) {
  cat("  DHR...\n")
  y_lin_raw  <- as.numeric(y_lin)
  y_orig_raw <- as.numeric(y_orig)
  use_log    <- all(y_lin_raw > 0) && all(y_orig_raw > 0)
  y_lin_fit  <- if (use_log) log(y_lin_raw)  else y_lin_raw
  y_orig_fit <- if (use_log) log(y_orig_raw) else y_orig_raw

  m     <- 2 + 2*n_harmonics
  omega <- 2*pi*(1:n_harmonics)/P_seas
  T_m   <- build_T_dhr(omega, m, n_harmonics)
  Z_m   <- build_Z_dhr(m, n_harmonics)
  R_m   <- build_R_dhr(m, n_harmonics)

  # Fit on linearised + original in parallel (mclapply if Unix, serial on Win)
  fits <- if (IS_WINDOWS) {
    list(.fit_dhr_series(y_lin_fit,  T_m, Z_m, R_m, n_harmonics),
         .fit_dhr_series(y_orig_fit, T_m, Z_m, R_m, n_harmonics))
  } else {
    parallel::mclapply(
      list(y_lin_fit, y_orig_fit),
      function(yy) .fit_dhr_series(yy, T_m, Z_m, R_m, n_harmonics),
      mc.cores = 2L
    )
  }
  fit_lin  <- fits[[1]]
  fit_orig <- fits[[2]]

  extract_components <- function(fit, y_raw) {
    alph     <- fit$sm$alphahat
    trend    <- alph[1, ]
    seasonal <- colSums(alph[2 + 2*(0:(n_harmonics-1)) + 1, , drop=FALSE])
    irr      <- y_raw - trend - seasonal
    list(trend = trend, seasonal = seasonal, irregular = irr,
         cycle = c(NA, diff(trend)))
  }

  comp_lin  <- extract_components(fit_lin,  y_lin_fit)
  comp_orig <- extract_components(fit_orig, y_orig_fit)

  burn_n    <- min(m, 24)
  res_raw   <- fit_lin$kf$v / sqrt(fit_lin$kf$F)
  res_valid <- res_raw[is.finite(res_raw)]
  res_clean <- res_valid[(burn_n+1):length(res_valid)]
  diag_df   <- run_diagnostics(res_clean, comp_lin$irregular, y_lin_fit, "DHR")

  list(
    # linearised series result (primary)
    trend       = comp_lin$trend,
    seasonal    = comp_lin$seasonal,
    irregular   = comp_lin$irregular,
    cycle_lin   = comp_lin$cycle,
    dates_lin   = dates_lin,
    y_lin_fit   = y_lin_fit,
    # original series result (for comparison + var2d)
    trend_orig  = comp_orig$trend,
    seasonal_orig = comp_orig$seasonal,
    irregular_orig = comp_orig$irregular,
    cycle_orig  = comp_orig$cycle,
    dates_orig  = dates_orig,
    y_orig_fit  = y_orig_fit,
    # diagnostics
    var2d_orig  = var2diff(comp_orig$trend),
    var2d_lin   = var2diff(comp_lin$trend),
    diag        = diag_df,
    use_log     = use_log
  )
}

# ==============================================================================
# STEP 4 — LDHR  (computation only, same state space as DHR)
# ==============================================================================
run_ldhr <- function(y_orig, y_lin, dates_orig, dates_lin, series_id) {
  cat("  LDHR...\n")
  y_lin_raw  <- as.numeric(y_lin)
  y_orig_raw <- as.numeric(y_orig)
  use_log    <- all(y_lin_raw > 0) && all(y_orig_raw > 0)
  y_lin_fit  <- if (use_log) log(y_lin_raw)  else y_lin_raw
  y_orig_fit <- if (use_log) log(y_orig_raw) else y_orig_raw

  m     <- 2 + 2*n_harmonics
  omega <- 2*pi*(1:n_harmonics)/P_seas
  T_m   <- build_T_dhr(omega, m, n_harmonics)
  Z_m   <- build_Z_dhr(m, n_harmonics)
  R_m   <- build_R_dhr(m, n_harmonics)

  fits <- if (IS_WINDOWS) {
    list(.fit_dhr_series(y_lin_fit,  T_m, Z_m, R_m, n_harmonics),
         .fit_dhr_series(y_orig_fit, T_m, Z_m, R_m, n_harmonics))
  } else {
    parallel::mclapply(
      list(y_lin_fit, y_orig_fit),
      function(yy) .fit_dhr_series(yy, T_m, Z_m, R_m, n_harmonics),
      mc.cores = 2L
    )
  }
  fit_lin  <- fits[[1]]
  fit_orig <- fits[[2]]

  extract_components <- function(fit, y_raw) {
    alph     <- fit$sm$alphahat
    trend    <- alph[1, ]
    seasonal <- colSums(alph[2 + 2*(0:(n_harmonics-1)) + 1, , drop=FALSE])
    irr      <- y_raw - trend - seasonal
    list(trend = trend, seasonal = seasonal, irregular = irr,
         cycle = c(NA, diff(trend)))
  }

  comp_lin  <- extract_components(fit_lin,  y_lin_fit)
  comp_orig <- extract_components(fit_orig, y_orig_fit)

  burn_n    <- min(m, 24)
  res_raw_v <- fit_lin$kf$v / sqrt(fit_lin$kf$F)
  res_clean <- res_raw_v[(burn_n+1):length(res_raw_v)]
  res_clean <- res_clean[is.finite(res_clean)]
  diag_df   <- run_diagnostics(res_clean, comp_lin$irregular, y_lin_fit, "LDHR")

  list(
    trend       = comp_lin$trend,
    seasonal    = comp_lin$seasonal,
    irregular   = comp_lin$irregular,
    cycle_lin   = comp_lin$cycle,
    dates_lin   = dates_lin,
    y_lin_fit   = y_lin_fit,
    trend_orig  = comp_orig$trend,
    seasonal_orig = comp_orig$seasonal,
    irregular_orig = comp_orig$irregular,
    cycle_orig  = comp_orig$cycle,
    dates_orig  = dates_orig,
    y_orig_fit  = y_orig_fit,
    var2d_orig  = var2diff(comp_orig$trend),
    var2d_lin   = var2diff(comp_lin$trend),
    diag        = diag_df,
    use_log     = use_log
  )
}

# ==============================================================================
# STEP 5 — MAIN LOOP  (parallelised across series)
# ==============================================================================
cat("\nReading data from:\n ", csv_path, "\n\n")
monthly_indicators_cleaned <- read.csv(csv_path, row.names = 1)
dates_raw <- rownames(monthly_indicators_cleaned)
dates_all <- as.Date(paste0(substr(dates_raw, 1, 7), "-01"), format = "%Y-%m-%d")
cat(sprintf("Data loaded: %d rows x %d columns\n",
            nrow(monthly_indicators_cleaned),
            ncol(monthly_indicators_cleaned)))
cat("Series found:", paste(colnames(monthly_indicators_cleaned), collapse = ", "), "\n\n")

# ---- Build cluster -----------------------------------------------------------
if (IS_WINDOWS) {
  cl <- makeCluster(n_cores, type = "PSOCK")
  registerDoParallel(cl)
  clusterExport(cl, varlist = c(
    "monthly_indicators_cleaned", "dates_all", "output_root",
    "P_seas", "n_harmonics", "scale_factor", "IS_WINDOWS", "GPU_OK",
    "parse_tramo_outliers", "run_diagnostics", "var2diff", "select_trend",
    "preferred_models",
    "run_tramo", "run_bsm", "run_bsm_variant",
    "run_dhr", "run_ldhr", ".fit_dhr_series",
    "build_T_dhr", "build_Z_dhr", "build_R_dhr", "build_Q_dhr",
    "kf_run", ".kf_run_gpu", "rts_run"
  ))
  clusterEvalQ(cl, { library(KFAS); library(RJDemetra) })
  if (GPU_OK) clusterEvalQ(cl, library(torch))
} else {
  cl <- makeCluster(n_cores, type = "FORK")
  registerDoParallel(cl)
}

# ---- Parallel loop -----------------------------------------------------------
all_results <- foreach(
  col_name       = colnames(monthly_indicators_cleaned),
  .combine       = c,
  .errorhandling = "pass",
  .packages      = if (IS_WINDOWS) c("KFAS", "RJDemetra") else character(0)
) %dopar% {
  tryCatch({
    message(sprintf("[%s] Starting", col_name))

    raw_vals <- monthly_indicators_cleaned[[col_name]]
    valid    <- !is.na(raw_vals)
    dates_s  <- dates_all[valid]
    vals     <- raw_vals[valid]

    tramo_res <- run_tramo(vals, dates_s)
    y_orig    <- vals
    y_lin     <- tramo_res$y_lin
    dates_o   <- dates_s
    dates_l   <- tramo_res$dates_lin

    bsm_res  <- run_bsm( y_orig, y_lin, dates_o, dates_l, col_name, IS_WINDOWS)
    dhr_res  <- run_dhr( y_orig, y_lin, dates_o, dates_l, col_name)
    ldhr_res <- run_ldhr(y_orig, y_lin, dates_o, dates_l, col_name)

    sel <- select_trend(
      col_name   = col_name,
      dates_orig = dates_o,             dates_lin  = dates_l,
      bsm_orig   = bsm_res$trend_smooth, bsm_lin   = bsm_res$trend_lin,
      dhr_orig   = dhr_res$trend_orig,  dhr_lin    = dhr_res$trend,
      ldhr_orig  = ldhr_res$trend_orig, ldhr_lin   = ldhr_res$trend
    )
    message(sprintf("[%s] Done — %s (%s)", col_name, sel$model, sel$series))

    list(list(
      series    = col_name,
      tramo     = tramo_res,
      bsm       = bsm_res,
      dhr       = dhr_res,
      ldhr      = ldhr_res,
      selected  = sel,
      # Convenience: wide-format row for trend export
      trend_df  = data.frame(date   = sel$dates,
                             trend  = sel$trend,
                             series = col_name,
                             stringsAsFactors = FALSE)
    ))
  }, error = function(e) {
    message(sprintf("[%s] ERROR: %s", col_name, conditionMessage(e)))
    list(NULL)
  })
}

stopCluster(cl)

# ==============================================================================
# STEP 6 — SAVE FULL RESULTS (for 02_make_plots.R) and TREND MATRIX
# ==============================================================================
all_results <- Filter(Negate(is.null), all_results)

# Save full results list for the plotting script
rds_path <- file.path(output_root, "trends_all_raw.rds")
saveRDS(all_results, rds_path)
cat(sprintf("Full results saved to:\n  %s\n", rds_path))

# Build wide trend matrix
all_trend_dfs <- lapply(all_results, `[[`, "trend_df")
all_dates     <- sort(unique(do.call(c, lapply(all_trend_dfs, `[[`, "date"))))
trend_wide    <- data.frame(date = all_dates)
for (df in all_trend_dfs) {
  if (is.null(df)) next
  sname <- df$series[1]
  idx   <- match(df$date, all_dates)
  col   <- rep(NA_real_, length(all_dates))
  col[idx] <- df$trend
  trend_wide[[sname]] <- col
}

csv_out <- file.path(output_root, "trends_selected.csv")
write.csv(trend_wide, csv_out, row.names = FALSE)
cat(sprintf("Trend matrix CSV saved to:\n  %s\n", csv_out))

wb <- createWorkbook()
addWorksheet(wb, "Trends")
writeData(wb, "Trends", trend_wide)
addStyle(wb, "Trends",
         style = createStyle(textDecoration = "bold", wrapText = FALSE),
         rows = 1, cols = seq_len(ncol(trend_wide)), gridExpand = TRUE)
excel_out <- file.path(output_root, "trends_selected.xlsx")
saveWorkbook(wb, excel_out, overwrite = TRUE)
cat(sprintf("Trend matrix Excel saved to:\n  %s\n\nAll series processed.\n", excel_out))
