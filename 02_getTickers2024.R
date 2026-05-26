
rm(list = ls())
library(data.table)
library(readxl)
library(rvest)
library(dplyr)
library(purrr)
library(yahoofinancer)
library(quantmod)
library(tidyquant)

trades2024 <- fread("/Users/limyiyang/Desktop/Home/projectAI/BEC/openInsider/output/tradesTxt/01_tradesPurchase_Y2024.txt")
trades2024 <- data.table(trades2024)

# change some names
colnames(trades2024)[2] <- "Filing.Date"
colnames(trades2024)[3] <- "Trade.Date"
colnames(trades2024)[6] <- "Insider.Name"
colnames(trades2024)[8] <- "Trade.Type"
trades2024[,"X"] <- NULL
trades2024[,"1d"] <- NULL
trades2024[,"1w"] <- NULL
trades2024[,"1m"] <- NULL
trades2024[,"6m"] <- NULL

# get unique tickers only
trades2024 <- unique(trades2024) #9000
uniqueN(trades2024$Ticker)

# QC1) remove non tradable tickers - 8992 Tickers
noTickerPrivate <- trades2024[is.na(Ticker) | Ticker == ""]
trades2024 <- trades2024[!Ticker %in% noTickerPrivate$Ticker]

# QC2) remove trades with no purchase - 8992 Tickers
trades2024$Price <- as.numeric(gsub("[\\$,]", "", trades2024$Price))
trades2024$Qty <- as.numeric(gsub("[\\+,]", "", trades2024$Qty))
trades2024$totalValue <- as.numeric(trades2024$Price) * as.numeric(trades2024$Qty)
naTickers <- trades2024[is.na(Price)]
trades2024 <- trades2024[!Ticker %in% naTickers$Ticker]

# calculate daystofile
trades2024$Filing.Date <- as.character(trades2024$Filing.Date)
split_dates <- strsplit(trades2024$Filing.Date, " ")
trades2024$Filing.Date <- sapply(split_dates, `[`, 1)
trades2024$Filing.Time <- sapply(split_dates, `[`, 2)
trades2024$DaysToFile <- as.Date(trades2024$Filing.Date) - as.Date(trades2024$Trade.Date)

# QC3) Remove Tickers with negative deltaOwn - 8938 Tickers
table(trades2024$ΔOwn)
trades2024 <- trades2024[!grepl("-",trades2024$ΔOwn)]

# QC4) Remove Tickers < 500k
trades2024 <- trades2024[totalValue > 500000]

# QC5) Remove Tickers that have > 8 days to filing date -> Master List
trades2024 <- trades2024[DaysToFile <=8 ] # 1229

# QC6) Remove Tickers without any title information -> 1193
trades2024 <- trades2024[!grepl("See Remarks|See Explanation in Footnotes|SEE REMARKS|See remarks|See Footnote 1|Related party|Affiliated Entity|Affiliate of Adviser",trades2024$Title)]

# QC7) Remove Funds
trades2024 <- trades2024[!grepl("Fund",trades2024$`Company Name`)]
trades2024 <- trades2024[!grepl("Fund",trades2024$Insider.Name)] # 1067 tickers
trades2024 <- na.omit(trades2024)

# rename title
trades2024[, role_group := "others"][
  grepl("CEO|Chief Exec Officer", Title, ignore.case = TRUE), role_group := "ceo"][
  grepl("COB|Chairman|Chair", Title, ignore.case = TRUE), role_group := "chair"][
  grepl("10%", Title, ignore.case = TRUE), role_group := "10%"
]

# Get realtime data -> 60 days after the trade price
results <- list()
for (i in seq_len(nrow(trades2024))) {
  tmp <- trades2024[i, ]
  iTicker <- tmp$Ticker
  tmpFilingDate <- as.Date(tmp$Filing.Date)
  tmpTradeDate <- as.Date(tmp$Trade.Date)

  message("Processing ", iTicker)

  # --- Validate symbol before trying to fetch ---
  validated <- tryCatch({yahoofinancer::validate(symbol = iTicker)}, error = function(e) NULL)
  if (is.null(validated)) {message("Skipping invalid symbol: ", iTicker)
    next
    }

  # Fetch Ticker data
  tmpTickerDF <- tryCatch({data.table(Ticker$new(iTicker)$get_history(
    start = tmpFilingDate - 10, end = tmpFilingDate + 120, interval = "1d"))
  }, error = function(e) NULL)

  if (is.null(tmpTickerDF) || nrow(tmpTickerDF) == 0 || !"date" %in% names(tmpTickerDF)) {
    message("⚠️ No data for ", iTicker, " — skipping.")
    next
    }

  # Fetch SPY data
  spyDF <- tryCatch({data.table(Ticker$new("SPY")$get_history(
    start = tmpFilingDate - 10, end = tmpFilingDate + 120, interval = "1d"))
  }, error = function(e) NULL)
  spyDF[, date := as.Date(date)]

  # Fetch VIX data
  vixDF <- tryCatch({data.table(Ticker$new("^VIX")$get_history(
    start = tmpFilingDate - 10, end = tmpFilingDate + 120, interval = "1d"))
  }, error = function(e) NULL)
  vixDF[, date := as.Date(date)]
  vixDF <- na.omit(vixDF)

  # --- Add trade date prices ---
  tmpTickerDF[, date := as.Date(date)]
  tradeDateDF <- tmpTickerDF[date == tmp$Trade.Date]
  tmp$td_open = tradeDateDF$open
  tmp$td_high= tradeDateDF$high
  tmp$td_low = tradeDateDF$low
  tmp$td_close = tradeDateDF$close
  tmp$td_OHLC = (tradeDateDF$open + tradeDateDF$high + tradeDateDF$low + tradeDateDF$close) / 4
  tmpTickerDF <- tmpTickerDF[date >= tmpFilingDate]
  tmpTickerDF <- tmpTickerDF[1:40, ]

  # SPY data
  spyDF <- spyDF[date >= tmpFilingDate][1:40]
  if (nrow(spyDF) < 40) {missing_rows <- 40 - nrow(spyDF)
    spyDF <- rbind(spyDF, data.table(date = rep(NA, missing_rows), volume = NA,
                                     high = NA, low = NA, open = NA, close = NA),fill = TRUE)
    }

  # VIX data
  vixDF <- vixDF[date >= tmpFilingDate][1:40]
  if (nrow(vixDF) < 40) {missing_rows <- 40 - nrow(vixDF)
  vixDF <- rbind(vixDF, data.table(date = rep(NA, missing_rows), volume = NA,
                                   high = NA, low = NA, open = NA, close = NA),fill = TRUE)
  }

  # Replace missing data with NA
  if (nrow(tmpTickerDF) < 40) {missing_rows <- 40 - nrow(tmpTickerDF)
    tmpTickerDF <- rbind(tmpTickerDF,data.table(date = rep(NA, missing_rows), volume = NA,
                                                high = NA,low = NA, open = NA, close = NA),fill = TRUE)
  }

  # Store main ticker data with suffix "_main"
  for (j in seq_len(40)) {
    tmp[[paste0(j - 1, "fd_open")]]      <- tmpTickerDF$open[j]
    tmp[[paste0(j - 1, "fd_high")]]      <- tmpTickerDF$high[j]
    tmp[[paste0(j - 1, "fd_low")]]       <- tmpTickerDF$low[j]
    tmp[[paste0(j - 1, "fd_close")]]     <- tmpTickerDF$close[j]
    tmp[[paste0(j - 1, "fd_OHLC")]]      <- (tmpTickerDF$open[j] + tmpTickerDF$high[j] + tmpTickerDF$low[j] + tmpTickerDF$close[j]) / 4
    tmp[[paste0(j - 1, "fd_volume")]]    <- tmpTickerDF$volume[j]
  }

  # Store SPY data with prefix "spy_"
  for (j in seq_len(40)) {
    tmp[[paste0("spy_", j - 1, "d_close")]]   <- spyDF$close[j]
    tmp[[paste0("spy_", j - 1, "d_OHLC")]]   <- (spyDF$open[j] + spyDF$high[j] + spyDF$low[j] + spyDF$close[j]) / 4
    }

  # Store VIX data with prefix "vix_"
  for (j in seq_len(40)) {
    tmp[[paste0("vix_", j - 1, "d_close")]]   <- vixDF$close[j]
    tmp[[paste0("vix_", j - 1, "d_OHLC")]]   <- (vixDF$open[j] + vixDF$high[j] + vixDF$low[j] + vixDF$close[j]) / 4
  }
  results[[i]] <- tmp
}
results <- rbindlist(results, fill = TRUE)

# QC8) Remove more funds
results <- results[ !(td_open == td_high & td_high == td_low & td_low == td_close) ]
volume_cols <- grep("d_volume$", names(results), value = TRUE)
results <- results[ results[, rowSums(.SD == 0, na.rm = TRUE) == 0 , .SDcols = volume_cols ] ] # 938 trades
uniqueN(results$Ticker)


# get Tickers without validated Ticker -> yahoofinancer failed
noRTData <- results[is.na(results$`1d_open`), ]
fwrite(noRTData, sep ="\t", quote = F, file = "/Users/limyiyang/Desktop/Home/projectAI/BEC/openInsider/output/tradesTxt/archive_tradesPurchase_noRTdataY2024.txt")

# filter by 10% first, CEO, Chair
table(results$Title)
trades202410p <- results[grepl("10%",results$Title)] # 651 Tickers
trades2024Chair <- results[grepl("COB|Chairman|Chair",results$Title)] # 73 Tickers
trades2024CEO <- results[grepl("CEO|Chief Exec Officer",results$Title)] # 178 Tickers

fwrite(results, sep ="\t", quote = F, file = "/Users/limyiyang/Desktop/Home/projectAI/BEC/openInsider/output/tradesTxt/01_tradesPurchase_masterlistY2024.txt")
fwrite(trades202410p, sep ="\t", quote = F, file = "/Users/limyiyang/Desktop/Home/projectAI/BEC/openInsider/output/tradesTxt/02_tradesPurchase_10percentY2024.txt")
fwrite(trades2024Chair, sep ="\t", quote = F, file = "/Users/limyiyang/Desktop/Home/projectAI/BEC/openInsider/output/tradesTxt/02_tradesPurchase_ChairmanY2024.txt")
fwrite(trades2024CEO, sep ="\t", quote = F, file = "/Users/limyiyang/Desktop/Home/projectAI/BEC/openInsider/output/tradesTxt/02_tradesPurchase_CeoY2024.txt")
fwrite(trades202410p[1:5,], sep ="\t", quote = F, file = "/Users/limyiyang/Desktop/Home/projectAI/BEC/openInsider/output/tradesTxt/02_tradesPurchase_10percentY2024_testset.txt")




