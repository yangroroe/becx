
rm(list=ls())
library(data.table)
library(readxl)
library(rvest)
library(dplyr)
library(purrr)

get_purchase_page <- function(page_num) {
  url <- paste0(
    "http://openinsider.com/screener?",
    "td=-1&",
    "tdr=01%2F01%2F2024+-+12%2F31%2F2024&",
    "xp=1&",
    "cnt=1000&",
    "page=", page_num
  )

  page <- tryCatch(read_html(url), error = function(e) return(NULL))
  if (is.null(page)) return(NULL)

  node <- page %>% html_node("table.tinytable")
  if (inherits(node, "xml_missing")) return(NULL)          # <-- fix

  table <- html_table(node, fill = TRUE)
  if (nrow(table) == 0) return(NULL)

  return(table)
}

# Loop through all pages
all_purchases <- list()
page <- 1

repeat {
  cat("Fetching page", page, "...\n")
  trades <- get_purchase_page(page)
  if (is.null(trades)) break
  all_purchases[[page]] <- trades
  page <- page + 1
  Sys.sleep(1)  # be polite to server
}

# Combine all pages
purchase_df <- bind_rows(all_purchases)

# Create the output folder inside BEC project
dir.create("output", showWarnings = FALSE)
fwrite(purchase_df, sep = "\t", quote = FALSE,
       file = "output/tradesPurchase_Y2024_new.txt")




