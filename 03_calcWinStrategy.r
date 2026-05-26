

rm(list = ls())
library(data.table)
library(readxl)
library(rvest)
library(dplyr)
library(purrr)
library(yahoofinancer)
library(quantmod)
library(tidyquant)

masterlist <- fread("~/Desktop/projectAI/BEC/openInsider/output/tradesPurchase_masterlistY2024.txt")
masterlist <- data.table(masterlist)

calc_win_rate <- function(dt, win_thres = 1.10, loss_thres = 0.95, max_days = 30) {
  dt <- as.data.table(dt)

  # Extract high & low columns
  high_cols <- grep("fd_high$", names(dt), value = TRUE)[-1]
  low_cols  <- grep("fd_low$",  names(dt), value = TRUE)[-1]
  ohlc_cols  <- grep("fd_OHLC$",  names(dt), value = TRUE)[-1]
  
  dt[, c("result", "win_day", "win_price", "loss_day", "loss_price") := {
    fp <- .SD[["0fd_OHLC"]]
    ohlc <- unlist(.SD[, ..ohlc_cols])
    
    win_day_idx  <- which(ohlc >= fp * win_thres)[1]
    loss_day_idx <- which(ohlc  <= fp * loss_thres)[1]
    
    win_price_val  <- if (!is.na(win_day_idx)) ohlc[win_day_idx] else NA_real_
    loss_price_val <- if (!is.na(loss_day_idx)) ohlc[loss_day_idx] else NA_real_
    
    outcome <- if (is.na(win_day_idx) & is.na(loss_day_idx)) {
      "neutral"
    } else if (!is.na(win_day_idx) && (is.na(loss_day_idx) || win_day_idx < loss_day_idx)) {
      "win"
    } else {
      "lose"
    }
    
    final_price <- if (outcome == "win") win_price_val else if (outcome == "lose") loss_price_val else NA_real_
    final_day   <- if (outcome == "win") win_day_idx else if (outcome == "lose") loss_day_idx else NA_integer_
    
    list(outcome, final_day, final_price, loss_day_idx, loss_price_val)
  }, by = seq_len(nrow(dt))]
  return(dt)  
}

masterlistRes <- calc_win_rate(masterlist) # 938 total tickers
fwrite(masterlistRes, sep ="\t", quote = F, file = "~/Desktop/projectAI/BEC/openInsider/output/tradesPurchase_masterlistY2024.txt")




