
rm(list = ls())
require(data.table)
require(readxl)
require(rvest)
require(dplyr)
require(purrr)
require(yahoofinancer)
require(ggplot2)

tradePurchaseFile <- fread("~/Desktop/projectAI/BEC/openInsider/output/tradesPurchase_10percentY2024.txt")

BEC_getGraphs <- function( tradePurchaseFile, stopLossPct, takeProfitPct,
                 priceType = "OHLC_average", projectPath) {
  # Check director and create if missing
  outputPath = paste0(projectPath,"outputGraphs")
  if (!dir.exists(projectPath)) {
    dir.create(outputPath, recursive = TRUE)
    message("Directory created: ", outputPath)
  } else {
    message("Directory already exists: ", outputPath)
  }  
  
  # run loop
  for (irow in seq_len(nrow(tradePurchaseFile))){
    tradeDF <- tradePurchaseFile[irow,]
    tradeDate  <- tradeDF$Trade.Date
    filingDate <- tradeDF$Filing.Date
    daysToFile <- tradeDF$DaysToFile
    ticker <- tradeDF$Ticker
    title <- tradeDF$Title
    insiderName <- tradeDF$Insider.Name
    
    print(ticker)
    
    na_indices <- which(is.na(tradeDF[, 16:260]))
    if (length(na_indices) > 0) {
      lasttradeColumn <- min(na_indices) + 14
      tradeDF <- tradeDF[, 1:lasttradeColumn]
      last_col <- colnames(tradeDF)[ncol(tradeDF)]
      last_date <- strsplit(last_col, split = "d_")[[1]][1]
    }
    
    # Calculate Price - OHLC or HL
    days <- paste0(0:last_date, "d")
    avg_prices <- list()
    averagePriceDF <- data.table()
    if (priceType == "OHLC"){
      for (day in days) {
        day_high <- tradeDF[[paste0(day, "_high")]]
        day_low <- tradeDF[[paste0(day, "_low")]]
        day_open <- tradeDF[[paste0(day, "_open")]]
        day_close <- tradeDF[[paste0(day, "_close")]]
        
        avgPrice <- (day_high+day_low+day_open+day_close)/4
        avg_prices[[day]] <- avgPrice
  
        daysNumeric <- as.numeric(gsub("d", " ", names(avg_prices)))
        averagePriceDF <- data.table(day = daysNumeric, 
                                     price = unlist(avg_prices))
        priceType  <- "OHLC"
      }
    } else if (priceType == "HL"){
      for (day in days) {
        
        day_high <- tradeDF[[paste0(day, "_high")]]
        day_low <- tradeDF[[paste0(day, "_low")]]

        avgPrice <- (day_high+day_low)/2
        avg_prices[[day]] <- avgPrice
        
        cols <- paste0(day, c("_high", "_low"))
        if (all(cols %in% names(tradeDF))) {
          avg_prices[[day]] <- sum(tradeDF[, ..cols])/2
        }
        daysNumeric <- as.numeric(gsub("d", " ", names(avg_prices)))
        averagePriceDF <- data.table(day = daysNumeric, 
                                     price = unlist(avg_prices))
        priceType  <- "HL"
      }
    }
    averagePriceDF[day == 0]$price <- tradeDF$Price
    
    
    # trade, filing price
    averagePriceDF$day <- factor(averagePriceDF$day)
    tradePrice  <- round(averagePriceDF[day == 0]$price, digits = 2)            # price at trade date (always the first point)
    filingPrice <- round(averagePriceDF[day == daysToFile-1]$price, digits = 2)   # price at filing date 
    deltaPct_filingPrice <- round((filingPrice - tradePrice)/tradePrice * 100, digits = 2)
    
    # case1) same trade/filing date and same trade/filing prices
    if (tradeDate == filingDate) {
      ggVline_tradeDate <- geom_vline(xintercept = 1, linetype = "dashed", color = "grey3")
      ggtext_tradeDate  <- annotate("text", x = 1, y = max(averagePriceDF$price) * 1.01, 
                                    label = paste0("TD = FD: ", tradeDate), color = "grey3",
                                    angle = 0, vjust = -0.2, hjust =-0.2, size = 5)
      
      ggHline_tradePrice <- geom_hline(yintercept = tradePrice, linetype = "dashed", color = "dodgerblue")
      ggtext_tradePrice  <- annotate("text", x = 1, y = tradePrice,
                                     label = paste0("TP = FP: ", tradePrice, "(+0%)"), color = "dodgerblue",
                                     angle = 0, vjust = -0.2, hjust =-0.2, size = 5)
      ggVline_filingDate <- NULL
      ggtext_filingDate <- NULL
      ggHline_filingPrice <- NULL
      ggtext_filingPrice <- NULL
      
      # case2) Different trade/filing dates but same trade/filing prices
    } else if (tradeDate < filingDate & tradePrice == filingPrice){
      
      ggVline_tradeDate <- geom_vline(xintercept = 1, linetype = "dashed", color = "grey3")
      ggtext_tradeDate  <- annotate("text", x = 1, y = max(averagePriceDF$price) * 1.01, 
                                    label = paste0("TD: ", tradeDate), color = "grey3",
                                    angle = 0, vjust = -0.2, hjust =-0.1, size = 5)
      
      ggVline_filingDate <- geom_vline(xintercept = daysToFile, linetype = "dashed", color = "grey3")
      ggtext_filingDate  <- annotate("text", x = daysToFile, y = max(averagePriceDF$price) * 1.01, 
                                     label = paste0("FD: ", filingDate), color = "grey3",
                                     angle = 0, vjust = -0.2, hjust =-0.1, size = 5)
      
      ggHline_tradePrice <- geom_hline(yintercept = tradePrice, linetype = "dashed", color = "dodgerblue")
      ggtext_tradePrice  <- annotate("text", x = 0, y = tradePrice,
                                     label = paste0("TP = FP: ", tradePrice, "(+0%)"), color = "dodgerblue",
                                     angle = 0, vjust = -0.2, hjust =-0.2, size = 5)
      
      # case3) Different trade/filing dates and different trade/filing prices
    } else if (tradeDate < filingDate){
      ggVline_tradeDate <- geom_vline(xintercept = 1, linetype = "dashed", color = "grey3")
      ggtext_tradeDate  <- annotate("text", x = 1, y = max(averagePriceDF$price) * 1.01, 
                                    label = paste0("TD: ", tradeDate), color = "grey3",
                                    angle = 0, vjust = -0.2, hjust =-0.1, size = 5)
      
      ggVline_filingDate <- geom_vline(xintercept = daysToFile, linetype = "dashed", color = "grey3")
      ggtext_filingDate  <- annotate("text", x = daysToFile, y = max(averagePriceDF$price) * 1.01, 
                                     label = paste0("FD: ", filingDate), color = "grey3",
                                     angle = 0, vjust = 1.2, hjust =-0.1, size = 5)
      
      ggHline_tradePrice <- geom_hline(yintercept = tradePrice, linetype = "dashed", color = "dodgerblue")
      ggtext_tradePrice  <- annotate("text", x = 0, y = tradePrice,
                                     label = paste0("TP: ", tradePrice), color = "dodgerblue",
                                     angle = 0, vjust = -0.2, hjust =-0.2, size = 5)
      
      ggHline_filingPrice <- geom_hline(yintercept = filingPrice, linetype = "dashed", color = "dodgerblue")
      if(deltaPct_filingPrice > 0){
        deltaPct_filingPrice = paste0("+",deltaPct_filingPrice)
      }
      ggtext_filingPrice  <- annotate("text", x = daysToFile-1, y = filingPrice,
                                      label = paste0("FP: ", filingPrice, "(",deltaPct_filingPrice,"%)"), color = "dodgerblue",
                                      angle = 0, vjust = -0.2, hjust =-0.2, size = 5)
      
    }
    
    ## Strategy: stopLoss and profit price
    stopLossPrice <- round(tradePrice - tradePrice * (stopLossPct / 100) , digits =2)
    profitPrice   <- round(tradePrice * (1 + takeProfitPct / 100), digits = 2)
    
    ggHline_profitPrice   <- geom_hline(yintercept = profitPrice, linetype = "dashed", color = "red")
    ggtext_profitPrice    <- annotate("text", x = max(as.numeric(as.character(averagePriceDF$day))),
                                     y = profitPrice,
                                     label = paste0("profit: ", profitPrice, "(+",takeProfitPct,"%)"), color = "red",
                                     angle = 0, vjust = -0.2, hjust =1, size = 5)
    
    ggHline_stopLossPrice <- geom_hline(yintercept = stopLossPrice, linetype = "dashed", color = "red")
    ggtext_stopLossPrice  <- annotate("text", x = max(as.numeric(as.character(averagePriceDF$day))), 
                                       y = stopLossPrice,
                                       label = paste0("StopLoss: ", stopLossPrice, "(-",takeProfitPct,"%)"), color = "red",
                                       angle = 0, vjust = -0.2, hjust =1, size = 5)
    
    # Max price
    maxPrice  <- round(max(averagePriceDF$price), digits = 2) 
    deltaPct_maxPrice  <- round(((maxPrice - tradePrice) /tradePrice * 100) , digits =2)
    
    # case1) max price == trade price (i.e the stock dropped after trade)
    if (maxPrice == tradePrice){
      ggHline_maxPrice <- NULL
      ggtext_maxPrice  <- NULL
      
      # case2) max price > trade price (normal condition)
    } else {
      ggHline_maxPrice <-  geom_hline(yintercept = maxPrice, linetype = "dashed", color = "red")
      ggtext_maxPrice  <- annotate("text", x = max(as.numeric(as.character(averagePriceDF$day))),
                                   y = maxPrice,
                                   label = paste0("Max: ", maxPrice, "(+",deltaPct_maxPrice,"%)"), color = "red",
                                   angle = 0, vjust = -0.2, hjust =1, size = 5)
    }
    
    basePlot <- ggplot(averagePriceDF, aes(day, price)) + geom_point() + geom_path(group = 1) + 
      theme_linedraw(base_size = 20) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
      labs(title = paste0("Ticker: ", ticker, " ", title), 
           subtitle = paste0(priceType," Price over 60 days"), 
           x = " Trading Day", y = "Average Price", 
           caption = paste0("Insider Name: ",insiderName)) 
    
    ## data.table 
    outputDF <- data.table()
    tradeDF2 <- data.table(tradeDF[,1:15])
    for (i in 0: max(as.numeric(as.character(averagePriceDF$day)))){
      colName <- paste0("d", i, "_", priceType)
      tradeDF2[[colName]] <- averagePriceDF[day == i]$price
    }
    outputDF <- rbindlist(list(outputDF, tradeDF2))
    
    ## ggplot 
    ggOut <- basePlot + 
      # trade and filing dates  -> vertical lines
      ggVline_tradeDate + ggtext_tradeDate + ggVline_filingDate + ggtext_filingDate + 
      
      # trade and filing prices -> horizontal lines
      ggHline_tradePrice + ggtext_tradePrice + ggHline_filingPrice + ggtext_filingPrice +
      
      # stopLoss and profit prices
      ggHline_stopLossPrice + ggtext_stopLossPrice + ggHline_profitPrice + ggtext_profitPrice +
      
      # max price
      ggHline_maxPrice + ggtext_maxPrice
    
    # Create subfolder if missing
    outputPathDTF <- paste0(outputPath, "/DaystoFile_", daysToFile,"/")
    if (!dir.exists(outputPathDTF)) {
      dir.create(outputPathDTF, recursive = TRUE)
      message("Subdirectory created: ", outputPathDTF)
    } else {
      message("Subdirectory already exists: ", outputPathDTF)
    }
    ggsave(ggOut, height = 8, width = 12, file = paste0(outputPathDTF,"TradeDate_",tradeDate,"_",ticker,".png"))
  }
}

BEC_getGraphs(tenPercent, stopLossPct = 5, takeProfitPct = 5,
              priceType = "OHLC", projectPath = "~/Desktop/projectAI/BEC/openInsider/")

# Create subfolder if missing
csvPath <- paste0(projectPath, "outputTable_Y2024/")
if (!dir.exists(csvPath)) {
  dir.create(csvPath, recursive = TRUE)
  message("Subdirectory created: ", csvPath)
} else {
  message("Subdirectory already exists: ", csvPath)
}
fwrite(outputDF, sep = "\t", quote=F, file = paste0(csvPath,"/", priceType,"_table_10percentY2024.txt"))
