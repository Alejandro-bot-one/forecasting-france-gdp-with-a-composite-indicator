# =============================================================================
#  TREND-CYCLE DECOMPOSITION — French Monthly Indicators
#  All methods: MA, Moving Medians, LOESS, Classical, STL, X-13, X-11, TRAMO/SEATS
#  Loops over every variable in selected_monthly_indicators.csv
# =============================================================================

# ---- Libraries ---------------------------------------------------------------
library(tidyverse)
library(RJDemetra)
library(forecast)
library(zoo)
library(patchwork)
library(seasonal)

# ---- Paths -------------------------------------------------------------------
csv_path  <- "C:/Users/Alejandro/Documents/Msc Analisis Economico Cuantitativo UAM/Courses/Quantitative Methods for Macroeconomics/France Business Cycle/data/selected_monthly_indicators.csv"
root_dir  <- "C:/Users/Alejandro/Documents/Msc Analisis Economico Cuantitativo UAM/Courses/Quantitative Methods for Macroeconomics/France Business Cycle/assigment 2 plots"

# ---- French recession periods (quarter-based) --------------------------------
# peak_quarters   = 1980Q1, 1992Q1, 2008Q1, 2019Q4
# trough_quarters = 1980Q4, 1993Q1, 2009Q2, 2020Q2
# start = first month of peak quarter; end = last month of trough quarter
recession_periods <- data.frame(z
  start = as.Date(c("1980-01-01", "1992-01-01", "2008-01-01", "2019-10-01")),
  end   = as.Date(c("1980-12-01", "1993-03-01", "2009-06-01", "2020-06-01"))
)

# ---- Shared ggplot theme -----------------------------------------------------
theme_france <- theme_bw() +
  theme(
    plot.title    = element_text(size = 11, face = "bold"),
    plot.subtitle = element_text(size = 9, color = "gray40"),
    legend.position = "bottom"
  )

# Helper to add recession shading to a ggplot
add_recessions <- function(p) {
  p + geom_rect(
    data        = recession_periods,
    aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
    fill        = "steelblue",
    alpha       = 0.15,
    inherit.aes = FALSE
  )
}

# First-difference helper (prepends NA to maintain length)
fd <- function(x) c(NA, diff(x))

# ---- Load CSV ----------------------------------------------------------------
cat("Loading data from:", csv_path, "\n")

raw <- read.csv(csv_path, check.names = FALSE, stringsAsFactors = FALSE)

# First column = dates (yyyy-mm); remaining columns = variables
date_strings <- raw[[1]]
dates_all    <- as.Date(paste0(date_strings, "-01"))   # yyyy-mm -> yyyy-mm-dd
var_names    <- colnames(raw)[-1]
n_vars       <- length(var_names)

cat(sprintf("Variables detected (%d): %s\n\n", n_vars, paste(var_names, collapse = ", ")))

# =============================================================================
#  MAIN LOOP — one iteration per variable
# =============================================================================
for (vn in var_names) {
  
  cat(rep("=", 60), "\n", sep = "")
  cat("[Variable]", vn, "\n")
  cat(rep("=", 60), "\n", sep = "")
  
  # --------------------------------------------------------------------------
  # 1. Extract series — drop NaNs, keep matching dates
  # --------------------------------------------------------------------------
  raw_col <- as.numeric(raw[[vn]])
  valid   <- !is.na(raw_col)
  y_raw   <- raw_col[valid]
  dates   <- dates_all[valid]
  
  if (length(y_raw) < 24) {
    cat("  Too few observations (", length(y_raw), "). Skipping.\n\n")
    next
  }
  
  # Infer ts start from first valid date
  ts_year  <- as.integer(format(dates[1], "%Y"))
  ts_month <- as.integer(format(dates[1], "%m"))
  serie    <- ts(y_raw, start = c(ts_year, ts_month), frequency = 12)
  
  cat(sprintf("  Obs: %d  (%s to %s)\n",
              length(y_raw),
              format(dates[1],    "%b %Y"),
              format(dates[length(dates)], "%b %Y")))
  
  # --------------------------------------------------------------------------
  # 2. Create per-variable output folder
  # --------------------------------------------------------------------------
  var_dir <- file.path(root_dir, vn)
  if (!dir.exists(var_dir)) dir.create(var_dir, recursive = TRUE)
  
  # Convenience save wrapper
  save_plot <- function(p, filename, w = 10, h = 6) {
    ggsave(filename  = file.path(var_dir, filename),
           plot      = p,
           width     = w,
           height    = h,
           dpi       = 300)
  }
  
  # ============================================================
  #  A. MOVING AVERAGES  (12, 36, 60 months)
  # ============================================================
  MA12 <- stats::filter(serie, rep(1/12, 12), sides = 2)
  MA36 <- stats::filter(serie, rep(1/36, 36), sides = 2)
  MA60 <- stats::filter(serie, rep(1/60, 60), sides = 2)
  
  df_ma <- data.frame(
    date  = dates,
    serie = as.numeric(serie),
    MA12  = as.numeric(MA12),
    MA36  = as.numeric(MA36),
    MA60  = as.numeric(MA60)
  )
  
  p_ma <- add_recessions(ggplot(df_ma, aes(x = date))) +
    geom_line(aes(y = serie, color = "Original"),     linewidth = 0.7) +
    geom_line(aes(y = MA12,  color = "MA 12"),        linewidth = 1) +
    geom_line(aes(y = MA36,  color = "MA 36"),        linewidth = 1) +
    geom_line(aes(y = MA60,  color = "MA 60"),        linewidth = 1) +
    scale_color_manual(values = c(
      "Original" = "grey40", "MA 12" = "blue", "MA 36" = "green", "MA 60" = "red"
    )) +
    labs(title = paste(vn, "— Moving Averages"), x = "Date", y = "Value", color = "") +
    theme_france
  
  save_plot(p_ma, paste0(vn, "_MA.png"))
  cat("  Saved: MA\n")
  
  # ============================================================
  #  B. MOVING MEDIANS  (~12, ~36, ~60 months)
  # ============================================================
  MM12 <- stats::runmed(serie, k = 13,      endrule = "constant")
  MM36 <- stats::runmed(serie, k = 13 * 3,  endrule = "constant")
  MM60 <- stats::runmed(serie, k = 13 * 5,  endrule = "constant")
  
  df_mm <- data.frame(
    date  = dates,
    serie = as.numeric(serie),
    MM12  = as.numeric(MM12),
    MM36  = as.numeric(MM36),
    MM60  = as.numeric(MM60)
  )
  
  p_mm <- add_recessions(ggplot(df_mm, aes(x = date))) +
    geom_line(aes(y = serie, color = "Original"),      linewidth = 0.7) +
    geom_line(aes(y = MM12,  color = "Median 12"),     linewidth = 1) +
    geom_line(aes(y = MM36,  color = "Median 36"),     linewidth = 1) +
    geom_line(aes(y = MM60,  color = "Median 60"),     linewidth = 1) +
    scale_color_manual(values = c(
      "Original" = "grey40", "Median 12" = "blue", "Median 36" = "green", "Median 60" = "red"
    )) +
    labs(title = paste(vn, "— Moving Medians"), x = "Date", y = "Value", color = "") +
    theme_france
  
  save_plot(p_mm, paste0(vn, "_MM.png"))
  cat("  Saved: Moving Medians\n")
  
  # ============================================================
  #  C. LOESS  (spans: 10%, 25%, 50%)
  # ============================================================
  df_loess_input <- data.frame(t = as.numeric(time(serie)), y = as.numeric(serie))
  
  loess_10 <- loess(y ~ t, data = df_loess_input, span = 0.10)
  loess_25 <- loess(y ~ t, data = df_loess_input, span = 0.25)
  loess_50 <- loess(y ~ t, data = df_loess_input, span = 0.50)
  
  df_loess <- data.frame(
    date  = dates,
    serie = df_loess_input$y,
    L10   = predict(loess_10),
    L25   = predict(loess_25),
    L50   = predict(loess_50)
  )
  
  p_loess <- add_recessions(ggplot(df_loess, aes(x = date))) +
    geom_line(aes(y = serie, color = "Original"),  linewidth = 0.7) +
    geom_line(aes(y = L10,   color = "LOESS 10%"), linewidth = 1) +
    geom_line(aes(y = L25,   color = "LOESS 25%"), linewidth = 1) +
    geom_line(aes(y = L50,   color = "LOESS 50%"), linewidth = 1) +
    scale_color_manual(values = c(
      "Original" = "grey40", "LOESS 10%" = "blue", "LOESS 25%" = "green", "LOESS 50%" = "red"
    )) +
    labs(title = paste(vn, "— LOESS Smoothing"), x = "Date", y = "Value", color = "") +
    theme_france
  
  save_plot(p_loess, paste0(vn, "_LOESS.png"))
  cat("  Saved: LOESS\n")
  
  # ============================================================
  #  D. CLASSICAL DECOMPOSITION  (additive + multiplicative)
  # ============================================================
  # Additive
  decomp_add <- decompose(serie, type = "additive")
  df_classic_add <- data.frame(
    date      = dates,
    original  = as.numeric(serie),
    trend     = as.numeric(decomp_add$trend),
    seasonal  = as.numeric(decomp_add$seasonal),
    remainder = as.numeric(decomp_add$random)
  )
  
  p_classic_add_t <- add_recessions(ggplot(df_classic_add, aes(x = date))) +
    geom_line(aes(y = original), color = "grey40", linewidth = 0.7) +
    geom_line(aes(y = trend),    color = "blue",   linewidth = 1) +
    labs(title = paste(vn, "— Classical Additive: Trend"), x = "", y = "Trend") +
    theme_france
  p_classic_add_s <- ggplot(df_classic_add, aes(x = date, y = seasonal)) +
    geom_line(color = "darkgreen", linewidth = 1) +
    labs(title = "Seasonal", x = "", y = "Seasonal") + theme_france
  p_classic_add_r <- ggplot(df_classic_add, aes(x = date, y = remainder)) +
    geom_line(color = "red", linewidth = 1) +
    labs(title = "Remainder", x = "Date", y = "Remainder") + theme_france
  
  save_plot(p_classic_add_t / p_classic_add_s / p_classic_add_r,
            paste0(vn, "_ClassicAdditive.png"), h = 9)
  
  # Additive on log-transformed series
  if (all(y_raw > 0)) {
    decomp_add_log <- decompose(log(serie), type = "additive")
    df_classic_log <- data.frame(
      date      = dates,
      original  = log(as.numeric(serie)),
      trend     = as.numeric(decomp_add_log$trend),
      seasonal  = as.numeric(decomp_add_log$seasonal),
      remainder = as.numeric(decomp_add_log$random)
    )
    p_log_t <- add_recessions(ggplot(df_classic_log, aes(x = date))) +
      geom_line(aes(y = original), color = "grey40", linewidth = 0.7) +
      geom_line(aes(y = trend),    color = "blue",   linewidth = 1) +
      labs(title = paste(vn, "— Classical Additive Log: Trend"), x = "", y = "log(Trend)") +
      theme_france
    p_log_s <- ggplot(df_classic_log, aes(x = date, y = seasonal)) +
      geom_line(color = "darkgreen", linewidth = 1) +
      labs(title = "Seasonal (log)", x = "", y = "Seasonal") + theme_france
    p_log_r <- ggplot(df_classic_log, aes(x = date, y = remainder)) +
      geom_line(color = "red", linewidth = 1) +
      labs(title = "Remainder (log)", x = "Date", y = "Remainder") + theme_france
    
    save_plot(p_log_t / p_log_s / p_log_r,
              paste0(vn, "_ClassicAdditiveLog.png"), h = 9)
  }
  
  # Multiplicative
  if (all(y_raw > 0)) {
    decomp_mult <- decompose(serie, type = "multiplicative")
    df_classic_mult <- data.frame(
      date      = dates,
      original  = as.numeric(serie),
      trend     = as.numeric(decomp_mult$trend),
      seasonal  = as.numeric(decomp_mult$seasonal),
      remainder = as.numeric(decomp_mult$random)
    )
    p_mult_t <- add_recessions(ggplot(df_classic_mult, aes(x = date))) +
      geom_line(aes(y = original), color = "grey40", linewidth = 0.7) +
      geom_line(aes(y = trend),    color = "blue",   linewidth = 1) +
      labs(title = paste(vn, "— Classical Multiplicative: Trend"), x = "", y = "Trend") +
      theme_france
    p_mult_s <- ggplot(df_classic_mult, aes(x = date, y = seasonal)) +
      geom_line(color = "darkgreen", linewidth = 1) +
      labs(title = "Seasonal", x = "", y = "Seasonal") + theme_france
    p_mult_r <- ggplot(df_classic_mult, aes(x = date, y = remainder)) +
      geom_line(color = "red", linewidth = 1) +
      labs(title = "Remainder", x = "Date", y = "Remainder") + theme_france
    
    save_plot(p_mult_t / p_mult_s / p_mult_r,
              paste0(vn, "_ClassicMultiplicative.png"), h = 9)
  }
  
  trend_classic <- as.numeric(decomp_add$trend)
  cat("  Saved: Classical decomposition\n")
  
  # ============================================================
  #  E. STL DECOMPOSITION
  #  Fixed specification per instructions:
  #    - MA/Median window  = 36 periods
  #    - LOESS span        = 50%  → t.window = round(0.50 * length(serie)) | odd
  #    - Seasonal window   = 12   (s.window = 12)
  #    - Trend window      = "periodic" (large/stable)
  # ============================================================
  n_obs      <- length(serie)
  t_win_50   <- round(0.50 * n_obs)
  if (t_win_50 %% 2 == 0) t_win_50 <- t_win_50 + 1   # must be odd
  
  # Default STL (mstl automatic parameters) — kept for the default overview plot
  stl_default <- mstl(serie, lambda = NULL)
  
  # Fixed-spec STL  (the one used for cycle computation)
  stl_fixed <- tryCatch(
    mstl(serie,
         s.window = 12,
         t.window = t_win_50),
    error = function(e) {
      message("  STL fixed spec failed (", e$message, "). Falling back to default.")
      mstl(serie, lambda = NULL)
    }
  )
  
  # Detect the correct seasonal column name (mstl names it "Seasonal12" for monthly)
  stl_cols <- colnames(stl_fixed)
  seas_col  <- stl_cols[grep("^Seasonal", stl_cols)][1]
  
  df_stl <- data.frame(
    date      = dates,
    original  = as.numeric(serie),
    trend     = as.numeric(stl_fixed[, "Trend"]),
    seasonal  = as.numeric(stl_fixed[, seas_col]),
    remainder = as.numeric(stl_fixed[, "Remainder"])
  )
  
  # Trend + original
  p_stl_t <- add_recessions(ggplot(df_stl, aes(x = date))) +
    geom_line(aes(y = original), color = "grey40", linewidth = 0.7) +
    geom_line(aes(y = trend),    color = "blue",   linewidth = 1) +
    labs(title = paste(vn, "— STL Trend (s.window=12, t.window=50%)"),
         x = "", y = "Trend / Original") +
    theme_france
  p_stl_s <- ggplot(df_stl, aes(x = date, y = seasonal)) +
    geom_line(color = "darkgreen", linewidth = 1) +
    labs(title = "Seasonal", x = "", y = "Seasonal") + theme_france
  p_stl_r <- ggplot(df_stl, aes(x = date, y = remainder)) +
    geom_line(color = "red", linewidth = 1) +
    labs(title = "Remainder", x = "Date", y = "Remainder") + theme_france
  
  save_plot(p_stl_t / p_stl_s / p_stl_r, paste0(vn, "_STL.png"), h = 9)
  cat("  Saved: STL\n")
  
  # ACF / PACF
  p_acf  <- ggAcf(as.numeric(serie),  lag.max = 36) +
    labs(title = paste(vn, "— ACF")) + theme_france
  p_pacf <- ggPacf(as.numeric(serie), lag.max = 36) +
    labs(title = paste(vn, "— PACF")) + theme_france
  save_plot(p_acf / p_pacf, paste0(vn, "_ACF_PACF.png"), h = 7)
  cat("  Saved: ACF/PACF\n")
  
  # ============================================================
  #  F. X-13ARIMA-SEATS  (RJDemetra)
  # ============================================================
  df_x13 <- NULL
  tryCatch({
    fit_x13 <- x13(serie, spec = "RSA5c")
    
    trend_x13     <- fit_x13$final$s[, "t"]
    seasonal_x13  <- fit_x13$final$s[, "s"]
    irregular_x13 <- fit_x13$final$s[, "i"]
    
    df_x13 <- data.frame(
      date      = dates,
      original  = as.numeric(serie),
      trend     = as.numeric(trend_x13),
      seasonal  = as.numeric(seasonal_x13),
      remainder = as.numeric(irregular_x13)
    )
    
    p_x13_t <- add_recessions(ggplot(df_x13, aes(x = date))) +
      geom_line(aes(y = original), color = "grey60", linewidth = 0.7) +
      geom_line(aes(y = trend),    color = "blue",   linewidth = 1) +
      labs(title = paste(vn, "— X-13 Trend"), x = "", y = "Trend") + theme_france
    p_x13_s <- ggplot(df_x13, aes(x = date, y = seasonal)) +
      geom_line(color = "darkgreen", linewidth = 1) +
      labs(title = "X-13 Seasonal", x = "", y = "Seasonal") + theme_france
    p_x13_r <- ggplot(df_x13, aes(x = date, y = remainder)) +
      geom_line(color = "red", linewidth = 1) +
      labs(title = "X-13 Irregular", x = "Date", y = "Irregular") + theme_france
    
    save_plot(p_x13_t / p_x13_s / p_x13_r, paste0(vn, "_X13.png"), h = 9)
    
    # Outlier plot from X-13
    out_x13 <- tryCatch({
      regarima_summary <- summary(fit_x13$regarima)
      # Extract outlier table if available
      if (!is.null(fit_x13$regarima$regression.coefficients)) {
        coefs <- rownames(fit_x13$regarima$regression.coefficients)
        out_rows <- coefs[grepl("^(AO|LS|TC)", coefs)]
        if (length(out_rows) > 0) {
          # Parse type and date from coefficient names like "AO2020.Mar"
          parse_outlier <- function(s) {
            type <- substr(s, 1, 2)
            rest <- sub("^(AO|LS|TC)", "", s)
            yr   <- as.integer(substr(rest, 1, 4))
            mo_str <- sub("^[0-9]{4}\\.", "", rest)
            mo   <- match(mo_str,
                          c("Jan","Feb","Mar","Apr","May","Jun",
                            "Jul","Aug","Sep","Oct","Nov","Dec"))
            if (is.na(mo)) return(NULL)
            data.frame(type = type,
                       date = as.Date(sprintf("%d-%02d-01", yr, mo)),
                       stringsAsFactors = FALSE)
          }
          do.call(rbind, lapply(out_rows, parse_outlier))
        } else NULL
      } else NULL
    }, error = function(e) NULL)
    
    if (!is.null(out_x13) && nrow(out_x13) > 0) {
      out_x13 <- out_x13 %>% left_join(df_x13 %>% select(date, original), by = "date")
      p_out_x13 <- add_recessions(ggplot(df_x13, aes(x = date))) +
        geom_line(aes(y = original), color = "grey60", linewidth = 0.7, alpha = 0.8) +
        geom_line(aes(y = trend),    color = "blue",   linewidth = 1) +
        geom_point(data = out_x13, aes(x = date, y = original, color = type),
                   shape = 3, size = 5, stroke = 2) +
        geom_text(data = out_x13, aes(x = date, y = original, label = type, color = type),
                  hjust = -0.3, vjust = -0.8, size = 3.5, fontface = "bold") +
        scale_color_manual(values = c("AO" = "red", "LS" = "orange", "TC" = "purple")) +
        labs(title    = paste(vn, "— X-13 Outlier Detection"),
             subtitle = "AO = Additive Outlier | LS = Level Shift | TC = Transitory Change",
             x = "Date", y = vn, color = "Outlier Type") +
        theme_france
      save_plot(p_out_x13, paste0(vn, "_X13_Outliers.png"))
    }
    
    cat("  Saved: X-13\n")
  }, error = function(e) {
    cat("  WARNING: X-13 failed for", vn, "—", e$message, "\n")
  })
  
  # ============================================================
  #  G. X-11  (seasonal package — pure X-11, no ARIMA)
  # ============================================================
  df_x11 <- NULL
  tryCatch({
    fit_x11 <- seas(
      x = serie,
      x11 = "",
      regression.variables = NULL,
      outlier = NULL,
      transform.function = "none"
    )
    
    df_x11 <- data.frame(
      date      = dates,
      original  = as.numeric(serie),
      trend     = as.numeric(trend(fit_x11)),
      seasonal  = as.numeric(seasonal(fit_x11)),
      remainder = as.numeric(irregular(fit_x11))
    )
    
    p_x11_t <- add_recessions(ggplot(df_x11, aes(x = date))) +
      geom_line(aes(y = original), color = "grey60", linewidth = 0.7) +
      geom_line(aes(y = trend),    color = "blue",   linewidth = 1) +
      labs(title = paste(vn, "— X-11 Trend"), x = "", y = "Trend") + theme_france
    p_x11_s <- ggplot(df_x11, aes(x = date, y = seasonal)) +
      geom_line(color = "darkgreen", linewidth = 1) +
      labs(title = "X-11 Seasonal", x = "", y = "Seasonal") + theme_france
    p_x11_r <- ggplot(df_x11, aes(x = date, y = remainder)) +
      geom_line(color = "red", linewidth = 1) +
      labs(title = "X-11 Irregular", x = "Date", y = "Irregular") + theme_france
    
    save_plot(p_x11_t / p_x11_s / p_x11_r, paste0(vn, "_X11.png"), h = 9)
    cat("  Saved: X-11\n")
  }, error = function(e) {
    cat("  WARNING: X-11 failed for", vn, "—", e$message, "\n")
  })
  
  # ============================================================
  #  H. TRAMO/SEATS  (RJDemetra)
  # ============================================================
  df_ts <- NULL
  tryCatch({
    tramo_seats <- tramoseats(series = serie, spec = "RSAfull")
    
    trend_st     <- tramo_seats$final$s[, "t"]
    seasonal_st  <- tramo_seats$final$s[, "s"]
    irregular_st <- tramo_seats$final$s[, "i"]
    
    df_ts <- data.frame(
      date      = dates,
      original  = as.numeric(serie),
      trend     = as.numeric(trend_st),
      seasonal  = as.numeric(seasonal_st),
      remainder = as.numeric(irregular_st)
    )
    
    p_ts_t <- add_recessions(ggplot(df_ts, aes(x = date))) +
      geom_line(aes(y = original), color = "grey60", linewidth = 0.7) +
      geom_line(aes(y = trend),    color = "blue",   linewidth = 1) +
      labs(title = paste(vn, "— TRAMO/SEATS Trend"), x = "", y = "Trend") + theme_france
    p_ts_s <- ggplot(df_ts, aes(x = date, y = seasonal)) +
      geom_line(color = "darkgreen", linewidth = 1) +
      labs(title = "TRAMO/SEATS Seasonal", x = "", y = "Seasonal") + theme_france
    p_ts_r <- ggplot(df_ts, aes(x = date, y = remainder)) +
      geom_line(color = "red", linewidth = 1) +
      labs(title = "TRAMO/SEATS Irregular", x = "Date", y = "Irregular") + theme_france
    
    save_plot(p_ts_t / p_ts_s / p_ts_r, paste0(vn, "_TRAMO_SEATS.png"), h = 9)
    
    # Outlier plot from TRAMO
    out_ts <- tryCatch({
      if (!is.null(tramo_seats$regarima$regression.coefficients)) {
        coefs <- rownames(tramo_seats$regarima$regression.coefficients)
        out_rows <- coefs[grepl("^(AO|LS|TC)", coefs)]
        if (length(out_rows) > 0) {
          parse_outlier <- function(s) {
            type <- substr(s, 1, 2)
            rest <- sub("^(AO|LS|TC)", "", s)
            yr   <- as.integer(substr(rest, 1, 4))
            mo_str <- sub("^[0-9]{4}\\.", "", rest)
            mo <- match(mo_str,
                        c("Jan","Feb","Mar","Apr","May","Jun",
                          "Jul","Aug","Sep","Oct","Nov","Dec"))
            if (is.na(mo)) return(NULL)
            data.frame(type = type,
                       date = as.Date(sprintf("%d-%02d-01", yr, mo)),
                       stringsAsFactors = FALSE)
          }
          do.call(rbind, lapply(out_rows, parse_outlier))
        } else NULL
      } else NULL
    }, error = function(e) NULL)
    
    if (!is.null(out_ts) && nrow(out_ts) > 0) {
      out_ts <- out_ts %>% left_join(df_ts %>% select(date, original), by = "date")
      p_out_ts <- add_recessions(ggplot(df_ts, aes(x = date))) +
        geom_line(aes(y = original), color = "grey60", linewidth = 0.7, alpha = 0.8) +
        geom_line(aes(y = trend),    color = "blue",   linewidth = 1) +
        geom_point(data = out_ts, aes(x = date, y = original, color = type),
                   shape = 3, size = 5, stroke = 2) +
        geom_text(data = out_ts, aes(x = date, y = original, label = type, color = type),
                  hjust = -0.3, vjust = -0.8, size = 3.5, fontface = "bold") +
        scale_color_manual(values = c("AO" = "red", "LS" = "orange", "TC" = "purple")) +
        labs(title    = paste(vn, "— TRAMO/SEATS Outlier Detection"),
             subtitle = "AO = Additive Outlier | LS = Level Shift | TC = Transitory Change",
             x = "Date", y = vn, color = "Outlier Type") +
        theme_france
      save_plot(p_out_ts, paste0(vn, "_TRAMO_SEATS_Outliers.png"))
    }
    
    cat("  Saved: TRAMO/SEATS\n")
  }, error = function(e) {
    cat("  WARNING: TRAMO/SEATS failed for", vn, "—", e$message, "\n")
  })
  
  # ============================================================
  #  I. ECONOMIC CYCLE — first difference of trend
  #     Only for the three main methods: STL, X-13, TRAMO/SEATS
  # ============================================================
  
  # Build cycle data frame (only available methods)
  cycle_list <- list(date = dates)
  
  cycle_list$cycle_STL          <- fd(df_stl$trend)
  if (!is.null(df_x13)) cycle_list$cycle_X13         <- fd(df_x13$trend)
  if (!is.null(df_x11)) cycle_list$cycle_X11         <- fd(df_x11$trend)
  if (!is.null(df_ts))  cycle_list$cycle_TRAMO_SEATS <- fd(df_ts$trend)
  
  cycles_df <- as.data.frame(cycle_list)
  
  # Long format for plotting
  cycles_long <- cycles_df %>%
    pivot_longer(cols = -date, names_to = "method", values_to = "cycle") %>%
    filter(!is.na(cycle))
  
  if (nrow(cycles_long) > 0) {
    # Colour and label maps
    colour_map <- c(
      "cycle_STL"          = "blue",
      "cycle_X13"          = "green",
      "cycle_X11"          = "orange",
      "cycle_TRAMO_SEATS"  = "red"
    )
    label_map <- c(
      "cycle_STL"          = "STL",
      "cycle_X13"          = "X-13",
      "cycle_X11"          = "X-11",
      "cycle_TRAMO_SEATS"  = "TRAMO/SEATS"
    )
    
    # Keep only methods present in the data
    methods_present <- unique(cycles_long$method)
    colour_map <- colour_map[names(colour_map) %in% methods_present]
    label_map  <- label_map[names(label_map)  %in% methods_present]
    
    p_cycles <- add_recessions(
      ggplot(cycles_long, aes(x = date, y = cycle, color = method))
    ) +
      geom_line(linewidth = 1) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5) +
      scale_color_manual(values = colour_map, labels = label_map) +
      labs(
        title    = paste(vn, "— Economic Cycle (ΔTrend)"),
        subtitle = "Cycle = First difference of trend | Shaded = French recessions",
        x        = "Date",
        y        = "Cycle",
        color    = "Method"
      ) +
      theme_france
    
    save_plot(p_cycles, paste0(vn, "_Cycles_Main.png"), w = 12)
    cat("  Saved: Cycle comparison (STL / X-13 / X-11 / TRAMO/SEATS)\n")
  }
  
  cat("  [Done]\n\n")
  
}  # end for-loop over variables

cat(rep("=", 60), "\n", sep = "")
cat("All variables processed.\n")
cat("Results saved to:", root_dir, "\n")