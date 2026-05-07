# ==============================================================================
# TURNING POINT ANALYSIS
# Standalone script — reads the trend matrix produced by econometric_models.R
# and computes, for each series, turning points on the FIRST DIFFERENCE of the
# trend selected by the model/treatment table below.
#
# The trend used for each variable is determined by:
#   • which model was chosen (BSM / LDHR)
#   • whether the LINEARISED or NON-LINEARISED series was used
#
# Inputs:
#   trends_selected.csv — wide CSV: rows = dates, one column per series
#                         (written by econometric_models.R Step 6)
#
# Outputs  (all written to output_root):
#   <series>_turning_points.png     — one per series
#   TurningPoints_All_Series.xlsx   — summary table
# ==============================================================================

library(ggplot2)
library(openxlsx)

# ==============================================================================
# CONFIGURATION
# ==============================================================================

trends_csv <- paste0(
  "C:/Users/Alejandro/Documents/Msc Analisis Economico Cuantitativo UAM/",
  "Courses/Quantitative Methods for Macroeconomics/",
  "France Business Cycle/assigment 3 plots/r plots/trends_selected.csv")

output_root <- paste0(
  "C:/Users/Alejandro/Documents/Msc Analisis Economico Cuantitativo UAM/",
  "Courses/Quantitative Methods for Macroeconomics/",
  "France Business Cycle/assigment 3 plots/r plots/Turning points")

if (!dir.exists(output_root)) dir.create(output_root, recursive = TRUE)

# ---- Recession periods -------------------------------------------------------
# Convention: first month of peak quarter / last month of trough quarter
rec_start  <- as.Date(c("1980-01-01","1992-01-01","2008-01-01","2019-10-01"))
rec_end    <- as.Date(c("1980-10-01","1993-01-01","2009-04-01","2020-04-01"))
rec_labels <- c("1980Q1/1980Q4","1992Q1/1993Q1","2008Q1/2009Q2","2019Q4/2020Q2")

# Search windows (months)
w_pre_start  <- 24   # months before rec_start to search for a peak
w_post_start <- 12   # months after  rec_start to search for a peak
w_pre_end    <- 12   # months before rec_end   to search for a trough
w_post_end   <- 24   # months after  rec_end   to search for a trough

# ==============================================================================
# MODEL / TREATMENT DISPATCH TABLE
# ==============================================================================
# Each row specifies:
#   var_name   : variable name as it appears in the table (user-facing label)
#   csv_col    : exact column name in trends_selected.csv
#   model      : "BSM" or "LDHR"
#   treatment  : "Non-linearised" or "Linearised"
#
# NOTE on csv_col / var_name mapping:
#   • started_dwellings  — var_name and csv_col are both "started_dwellings"
#                         (old script used "authorised_dwellings" as var_name; corrected)
#   • ocupancy_rate      — typo preserved as-is; matches column name in the CSV
#   • air_passangers     — typo preserved as-is; matches column name in the CSV
# ==============================================================================
# Each row mirrors the preferred_models table in 01_compute_trends.R exactly.
# csv_col must match the column name written to trends_selected.csv.
# var_name is the user-facing label used in plot titles and the Excel sheet.
#
# Changes vs previous version:
#   - "authorised_dwellings" renamed to "started_dwellings" (matches csv_col / preferred_models key)
#   - "occupancy_rate"  corrected to "ocupancy_rate"  (matches csv_col typo in source data)
#   - "air_passangers" treatment updated to Non-linearised (matches preferred_models)
dispatch <- data.frame(
  var_name  = c(
    "export_order_book_level",
    "major_purchases_now",
    "unemp_exp_12m",
    "econ_sit_next_12m",
    "constr_order_books",
    "construction_conf",
    "car_registrations",
    "services_conf",
    "industrial_conf",
    "order_book_level",
    "stocks_finished",
    "hicp_yoy",
    "started_dwellings",
    "ocupancy_rate",
    "air_passangers",
    "employment_manufacturing",
    "production_manufacturing",
    "employment_services",
    "unemployment_over25",
    "demand_evol_services"
  ),
  csv_col   = c(
    "export_order_book_level",
    "major_purchases_now",
    "unemp_exp_12m",
    "econ_sit_next_12m",
    "constr_order_books",
    "construction_conf",
    "car_registrations",
    "services_conf",
    "industrial_conf",
    "order_book_level",
    "stocks_finished",
    "hicp_yoy",
    "started_dwellings",   # CSV column name (preferred_models key)
    "ocupancy_rate",       # CSV column name (typo preserved from source data)
    "air_passangers",       # CSV column name (typo preserved from source data)
    "employment_manufacturing",
    "production_manufacturing",
    "employment_services",
    "unemployment_over25",
    "demand_evol_services"
  ),
  model     = c(
    "BSM",  "BSM",
    "LDHR", "LDHR", "LDHR", "LDHR", "LDHR", "LDHR",
    "LDHR", "LDHR", "LDHR", "LDHR", "LDHR", "LDHR", "LDHR","LDHR","LDHR",
    "LDHR","LDHR","LDHR"
  ),
  treatment = c(
    "Non-linearised", "Non-linearised",
    "Linearised",     "Linearised",     "Linearised",
    "Non-linearised", "Non-linearised", "Non-linearised",
    "Non-linearised", "Non-linearised", "Non-linearised", "Non-linearised",
    "Non-linearised", "Non-linearised", "Non-linearised", "Non-linearised", "Non-linearised",
    "Non-linearised", "Non-linearised", "Non-linearised"
  ),
  stringsAsFactors = FALSE
)

# ==============================================================================
# HELPERS
# ==============================================================================

add_months <- function(d, n) {
  yr <- as.integer(format(d, "%Y"))
  mo <- as.integer(format(d, "%m")) + n
  yr <- yr + (mo - 1L) %/% 12L
  mo <- ((mo - 1L) %% 12L) + 1L
  as.Date(sprintf("%04d-%02d-01", yr, mo))
}

month_diff <- function(d1, d2)
  (as.integer(format(d1,"%Y")) - as.integer(format(d2,"%Y"))) * 12L +
  (as.integer(format(d1,"%m")) - as.integer(format(d2,"%m")))

add_recession_shading <- function(p, dates_range) {
  for (i in seq_along(rec_start)) {
    rs <- max(rec_start[i], min(dates_range))
    re <- min(rec_end[i],   max(dates_range))
    if (rs <= re)
      p <- p + annotate("rect",
                        xmin = rs, xmax = re, ymin = -Inf, ymax = Inf,
                        fill = "grey80", alpha = 0.5)
  }
  p
}

save_plot <- function(p, path, width = 26, height = 14) {
  ggsave(path, plot = p, width = width, height = height,
         units = "cm", dpi = 150)
  invisible(path)
}

# Dominant local maximum: highest-valued strict local max in window.
# Fallback: global max when no strict local max exists.
find_lmax <- function(vals, dw) {
  n <- length(vals)
  if (n == 0) return(list(date = NA, val = NA_real_))
  idx <- if (n >= 3) {
    is_lm <- c(FALSE,
               vals[2:(n-1)] > vals[1:(n-2)] & vals[2:(n-1)] > vals[3:n],
               FALSE)
    if (any(is_lm)) which(is_lm) else which.max(vals)
  } else which.max(vals)
  best <- idx[which.max(vals[idx])]
  list(date = dw[best], val = vals[best])
}

# Dominant local minimum: lowest-valued strict local min in window.
# Fallback: global min when no strict local min exists.
find_lmin <- function(vals, dw) {
  n <- length(vals)
  if (n == 0) return(list(date = NA, val = NA_real_))
  idx <- if (n >= 3) {
    is_lm <- c(FALSE,
               vals[2:(n-1)] < vals[1:(n-2)] & vals[2:(n-1)] < vals[3:n],
               FALSE)
    if (any(is_lm)) which(is_lm) else which.min(vals)
  } else which.min(vals)
  best <- idx[which.min(vals[idx])]
  list(date = dw[best], val = vals[best])
}

# ==============================================================================
# LOAD TREND MATRIX
# ==============================================================================
cat(sprintf("Reading trends from:\n  %s\n\n", trends_csv))
trend_wide       <- read.csv(trends_csv, stringsAsFactors = FALSE,
                             na.strings = "NA", check.names = FALSE)
trend_wide$date  <- as.Date(trend_wide$date)
available_cols   <- names(trend_wide)

cat(sprintf("Columns in CSV (%d): %s\n\n",
            length(available_cols) - 1L,
            paste(setdiff(available_cols,"date"), collapse=", ")))

# ==============================================================================
# TURNING POINT LOOP — one row in dispatch table per variable
# ==============================================================================
all_tp_rows <- list()

for (row_i in seq_len(nrow(dispatch))) {

  var_name  <- dispatch$var_name[row_i]
  csv_col   <- dispatch$csv_col[row_i]
  model     <- dispatch$model[row_i]
  treatment <- dispatch$treatment[row_i]

  cat(sprintf("--- %-30s [%s | %s] ---\n", var_name, model, treatment))

  # ---- Guard: missing column in CSV ----------------------------------------
  if (is.na(csv_col)) {
    cat(sprintf("  SKIPPED: '%s' has no corresponding column in trends_selected.csv.\n",
                var_name))
    cat("           Add the column to the CSV and re-run to include it.\n\n")
    next
  }
  if (!(csv_col %in% available_cols)) {
    cat(sprintf("  SKIPPED: column '%s' not found in CSV ",  csv_col))
    cat(sprintf("(mapped from '%s').\n\n", var_name))
    next
  }

  # ---- Extract trend -------------------------------------------------------
  trend <- as.numeric(trend_wide[[csv_col]])
  dates <- trend_wide$date

  # Drop leading / trailing NAs
  ok    <- !is.na(trend)
  trend <- trend[ok]
  dates <- dates[ok]
  n     <- length(trend)

  if (n < 3) {
    cat(sprintf("  SKIPPED: too few observations (%d).\n\n", n))
    next
  }

  # ---- First difference of trend = cycle signal ----------------------------
  # Turning points are identified on ΔTrend, NOT on the trend level itself.
  dtrend       <- c(NA_real_, diff(trend))
  dtrend_clean <- dtrend[-1]      # drop leading NA
  dates_clean  <- dates[-1]       # align dates with dtrend_clean

  # ---- Per-recession peak / trough search ----------------------------------
  tp_rows <- lapply(seq_along(rec_start), function(r) {

    rs <- rec_start[r];  re <- rec_end[r]

    # Peak: search in [rs - w_pre_start, rs + w_post_start]
    im <- which(dates_clean >= add_months(rs, -w_pre_start) &
                dates_clean <= add_months(rs,  w_post_start))
    if (length(im) > 0) {
      mx       <- find_lmax(dtrend_clean[im], dates_clean[im])
      max_date <- if (!is.na(mx$date)) format(mx$date, "%Y-%m") else "n/a"
      max_val  <- mx$val
      lead_max <- if (!is.na(mx$date)) month_diff(rs, mx$date) else NA_integer_
    } else {
      max_date <- "n/a"; max_val <- NA_real_; lead_max <- NA_integer_
    }

    # Trough: search in [re - w_pre_end, re + w_post_end]
    ii <- which(dates_clean >= add_months(re, -w_pre_end) &
                dates_clean <= add_months(re,  w_post_end))
    if (length(ii) > 0) {
      mn       <- find_lmin(dtrend_clean[ii], dates_clean[ii])
      min_date <- if (!is.na(mn$date)) format(mn$date, "%Y-%m") else "n/a"
      min_val  <- mn$val
      lead_min <- if (!is.na(mn$date)) month_diff(re, mn$date) else NA_integer_
    } else {
      min_date <- "n/a"; min_val <- NA_real_; lead_min <- NA_integer_
    }

    cat(sprintf("  %-22s  Peak: %-8s (%+d m)   Trough: %-8s (%+d m)\n",
                rec_labels[r],
                max_date,  if (is.na(lead_max)) 0L else lead_max,
                min_date,  if (is.na(lead_min)) 0L else lead_min))

    data.frame(
      Series             = var_name,
      CSV_Column         = csv_col,
      Model              = model,
      Treatment          = treatment,
      Recession          = rec_labels[r],
      Rec_Start          = format(rs, "%Y-%m"),
      Rec_End            = format(re, "%Y-%m"),
      Peak_Date          = max_date,
      Peak_Value         = round(max_val,  6),
      Lead_to_RecStart_m = lead_max,
      Trough_Date        = min_date,
      Trough_Value       = round(min_val, 6),
      Lead_to_RecEnd_m   = lead_min,
      stringsAsFactors   = FALSE
    )
  })

  df_tp         <- do.call(rbind, tp_rows)
  all_tp_rows   <- c(all_tp_rows, list(df_tp))

  # ---- Turning point plot ---------------------------------------------------
  df_dt <- data.frame(date = dates_clean, dtrend = dtrend_clean)

  pk <- df_tp[df_tp$Peak_Date   != "n/a" & !is.na(df_tp$Peak_Value),  ]
  tr <- df_tp[df_tp$Trough_Date != "n/a" & !is.na(df_tp$Trough_Value),]

  pts <- rbind(
    if (nrow(pk) > 0)
      data.frame(date  = as.Date(paste0(pk$Peak_Date,   "-01")),
                 value = pk$Peak_Value,   type = "Peak",
                 stringsAsFactors = FALSE)
    else
      data.frame(date = as.Date(character()), value = numeric(),
                 type = character()),
    if (nrow(tr) > 0)
      data.frame(date  = as.Date(paste0(tr$Trough_Date, "-01")),
                 value = tr$Trough_Value, type = "Trough",
                 stringsAsFactors = FALSE)
    else
      data.frame(date = as.Date(character()), value = numeric(),
                 type = character())
  )

  # Subtitle line: model, treatment, CSV column (so reader knows the source)
  subtitle_str <- sprintf("Model: %s  |  Series: %s  |  CSV column: %s",
                          model, treatment, csv_col)

  p_tp <- ggplot(df_dt, aes(x = date, y = dtrend)) +
    geom_line(color = "steelblue", linewidth = 1) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
    labs(title    = paste0(var_name, " — \u0394Trend Turning Points"),
         subtitle = subtitle_str,
         x        = "Date",
         y        = expression(Delta * "Trend"),
         color    = "", shape = "") +
    theme_bw() +
    theme(legend.position = "bottom",
          plot.subtitle   = element_text(size = 8.5, color = "grey40"))

  if (nrow(pts) > 0) {
    p_tp <- p_tp +
      geom_point(data = pts,
                 aes(x = date, y = value, color = type, shape = type),
                 size = 3.5, stroke = 1.5) +
      scale_color_manual(values = c("Peak" = "darkred", "Trough" = "darkgreen")) +
      scale_shape_manual(values = c("Peak" = 25,        "Trough" = 24))
  }

  p_tp <- add_recession_shading(p_tp, dates_clean)

  # Annotate each peak/trough with its lead/lag in months
  if (nrow(df_tp) > 0) {
    for (r in seq_len(nrow(df_tp))) {
      if (df_tp$Peak_Date[r] != "n/a" && !is.na(df_tp$Peak_Value[r])) {
        pd  <- as.Date(paste0(df_tp$Peak_Date[r], "-01"))
        lbl <- sprintf("%+d m", df_tp$Lead_to_RecStart_m[r])
        p_tp <- p_tp +
          annotate("text", x = pd, y = df_tp$Peak_Value[r],
                   label = lbl, vjust = -1, hjust = 0.5,
                   size = 2.8, color = "darkred", fontface = "bold")
      }
      if (df_tp$Trough_Date[r] != "n/a" && !is.na(df_tp$Trough_Value[r])) {
        td  <- as.Date(paste0(df_tp$Trough_Date[r], "-01"))
        lbl <- sprintf("%+d m", df_tp$Lead_to_RecEnd_m[r])
        p_tp <- p_tp +
          annotate("text", x = td, y = df_tp$Trough_Value[r],
                   label = lbl, vjust = 1.8, hjust = 0.5,
                   size = 2.8, color = "darkgreen", fontface = "bold")
      }
    }
  }

  out_path <- file.path(output_root,
                        sprintf("%s_turning_points.png", var_name))
  save_plot(p_tp, out_path)
  cat(sprintf("  Plot saved: %s\n\n", out_path))
}

# ==============================================================================
# EXCEL SUMMARY
# ==============================================================================
cat("--- Writing Excel summary ---\n")

if (length(all_tp_rows) == 0) {
  cat("No turning points computed. Check that trends_selected.csv is correct.\n")
} else {
  df_all <- do.call(rbind, all_tp_rows)

  # Also add a dispatch reference sheet so the reader can verify the mapping
  wb <- createWorkbook()

  hdr_style <- createStyle(textDecoration = "bold", fgFill = "#D6E4F0",
                           border = "Bottom", borderStyle = "medium")

  # Sheet 1: Turning points
  addWorksheet(wb, "TurningPoints")
  writeData(wb, "TurningPoints", df_all)
  addStyle(wb, "TurningPoints", hdr_style,
           rows = 1, cols = seq_len(ncol(df_all)), gridExpand = TRUE)
  setColWidths(wb, "TurningPoints", cols = seq_len(ncol(df_all)), widths = "auto")

  # Sheet 2: Dispatch table (model/treatment reference)
  disp_out <- dispatch
  disp_out$csv_col[is.na(disp_out$csv_col)] <- "NOT IN CSV — series skipped"
  addWorksheet(wb, "Model_Treatment_Map")
  writeData(wb, "Model_Treatment_Map", disp_out)
  addStyle(wb, "Model_Treatment_Map", hdr_style,
           rows = 1, cols = seq_len(ncol(disp_out)), gridExpand = TRUE)
  setColWidths(wb, "Model_Treatment_Map",
               cols = seq_len(ncol(disp_out)), widths = "auto")

  excel_path <- file.path(output_root, "TurningPoints_All_Series.xlsx")
  saveWorkbook(wb, excel_path, overwrite = TRUE)
  cat(sprintf("Excel saved to:\n  %s\n\nDone.\n", excel_path))
}
