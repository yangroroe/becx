library(data.table)
library(ggplot2)
library(moments)  # for skewness, kurtosis

# --- Helper functions ---
# Sharpe Ratio (risk-free rate assumed 0)
sharpe_ratio <- function(returns, rf = 0) {
  mean_ret <- mean(returns, na.rm = TRUE)
  sd_ret <- sd(returns, na.rm = TRUE)
  if (sd_ret == 0) return(NA_real_)
  (mean_ret - rf) / sd_ret
}

# Max Drawdown from price vector
max_drawdown <- function(prices) {
  cummax_prices <- cummax(prices)
  drawdowns <- (prices - cummax_prices) / cummax_prices
  min(drawdowns, na.rm = TRUE)
}

# Calculate returns vector from close prices vector
calc_returns <- function(prices) {
  returns <- diff(prices) / head(prices, -1)
  return(returns)
}

# --- Your data.table ---
masterlist <- fread("/Users/limyiyang/Desktop/Home/projectAI/BEC/openInsider/output/tradesTxt/03_tradesPurchase_masterlistY2024_winRate.txt")
dt <- masterlist[, 1:(ncol(masterlist) - 4), with = FALSE]
#dt <- unique(dt, by = c("Ticker", "Insider.Name"))
#dt <- dt[result != "neutral"]


# Columns for close prices for ticker, SPY, and VIX (adjust days as needed)
ticker_close_cols <- paste0(0:30, "fd_close")  # days 0 to 7 close prices for ticker
spy_close_cols <- paste0("spy_", 0:30, "d_OHLC") # assuming OHLC close approx here; replace with actual close cols if available
vix_close_cols <- paste0("vix_", 0:30, "d_OHLC") # same as above

# --- Calculate ticker returns columns ---
dt[, paste0("ticker_ret_", 0:30, "d") := {
  ret <- calc_returns(unlist(.SD))
  as.list(ret)
}, by = 1:nrow(dt), .SDcols = ticker_close_cols]

# Calculate ticker Sharpe and Max Drawdown
dt[, ticker_sharpe := sharpe_ratio(unlist(.SD)), .SDcols = paste0("ticker_ret_", 1:30, "d"), by = 1:nrow(dt)]
dt[, ticker_maxdrawdown := max_drawdown(unlist(.SD)), .SDcols = ticker_close_cols, by = 1:nrow(dt)]

# --- Calculate SPY returns columns ---
dt[, paste0("spy_ret_", 0:30, "d") := {
  ret <- calc_returns(unlist(.SD))
  as.list(ret)
}, by = 1:nrow(dt), .SDcols = spy_close_cols]

# Calculate SPY Sharpe and Max Drawdown
dt[, spy_sharpe := sharpe_ratio(unlist(.SD)), .SDcols = paste0("spy_ret_", 1:30, "d"), by = 1:nrow(dt)]
dt[, spy_maxdrawdown := max_drawdown(unlist(.SD)), .SDcols = spy_close_cols, by = 1:nrow(dt)]

# --- Calculate VIX returns columns ---
dt[, paste0("vix_ret_", 0:30, "d") := {
  ret <- calc_returns(unlist(.SD))
  as.list(ret)
}, by = 1:nrow(dt), .SDcols = vix_close_cols]

# Calculate VIX Sharpe and Max Drawdown
dt[, vix_sharpe := sharpe_ratio(unlist(.SD)), .SDcols = paste0("vix_ret_", 1:30, "d"), by = 1:nrow(dt)]
dt[, vix_maxdrawdown := max_drawdown(unlist(.SD)), .SDcols = vix_close_cols, by = 1:nrow(dt)]

# --- Compute alpha & beta for each trade using linear regression ---

dt[, c("alpha", "beta") := {
  # ticker + market returns
  y <- unlist(.SD[1, paste0("ticker_ret_", 1:30, "d"), with = FALSE])
  x <- unlist(.SD[1, paste0("spy_ret_", 1:30, "d"), with = FALSE])
  # remove NA or zero-variance
  ok <- complete.cases(y, x)
  y <- y[ok]
  x <- x[ok]
  if (length(y) < 2 || sd(x) == 0) {
    list(NA_real_, NA_real_)
  } else {
    fit <- lm(y ~ x)
    list(coef(fit)[1], coef(fit)[2])   # alpha, beta
  }
}, by = 1:nrow(dt)]



# Compute Jensen's Alpha, Treynor Ratio, Information Ratio
dt[, c("jensen_alpha", "treynor_ratio", "information_ratio") := {

  # returns
  rp <- unlist(.SD[1, paste0("ticker_ret_", 1:30, "d"), with = FALSE])
  rm <- unlist(.SD[1, paste0("spy_ret_", 1:30, "d"), with = FALSE])

  ok <- complete.cases(rp, rm)
  rp <- rp[ok]
  rm <- rm[ok]

  if (length(rp) < 2 || is.na(beta) || beta == 0) {
    list(NA_real_, NA_real_, NA_real_)
  } else {

    # Jensen's alpha
    j_alpha <- mean(rp) - beta * mean(rm)

    # Treynor ratio
    t_ratio <- mean(rp) / beta

    # Information ratio
    te <- sd(rp - rm)
    ir <- ifelse(te == 0, NA_real_, (mean(rp) - mean(rm)) / te)

    list(j_alpha, t_ratio, ir)
  }

}, by = 1:nrow(dt)]



# --- Prepare features for PCA ---
selected_features <- c(
  "Qty", "totalValue", "td_OHLC", "ticker_sharpe", "ticker_maxdrawdown", "DaysToFile",
  "spy_sharpe", "spy_maxdrawdown","vix_sharpe", "vix_maxdrawdown",
  "alpha", "beta", "jensen_alpha", "treynor_ratio", "information_ratio"
)
selected_features2 <- c(
  "Qty", "totalValue", "td_OHLC"
)

# Scale numeric features
num_features <- selected_features[selected_features %in% names(dt)]
dt[, (num_features) := lapply(.SD, scale), .SDcols = num_features]

# Run PCA
pca_res <- prcomp(dt[, ..num_features], center = FALSE, scale. = FALSE)

# Prepare PCA result data.table for plotting
pca_dt <- data.table(
  role_group = dt$role_group,
  Ticker = dt$Ticker,
  Insider = dt$Insider.Name,
  DaysToFile = dt$DaysToFile,
  Result = dt$result,
  PC1 = pca_res$x[, 1],
  PC2 = pca_res$x[, 2]
)

# --- Plotting example (using ggplot2) ---
ggOut <- ggplot(pca_dt, aes(x = PC1, y = PC2, color = role_group)) +
  geom_point(alpha = 0.7) +
  theme_classic(base_size = 18) +
  labs(title = "PCA plot with metrics",
       x = "PC1",
       y = "PC2")
ggsave(ggOut, height = 6, width = 8, filename = "/Users/limyiyang/Desktop/Home/projectAI/BEC/openInsider/output/graphs/PCAanalysis/PCA_group.png")

ggOut <- ggplot(pca_dt, aes(x = PC1, y = PC2, color = DaysToFile)) +
  geom_point(alpha = 0.7) +
  scale_color_distiller(palette = "RdBu") +
  theme_classic(base_size = 18) +
  labs(title = "PCA plot with metrics",
       x = "PC1",
       y = "PC2")
ggsave(ggOut, height = 6, width = 8, filename = "/Users/limyiyang/Desktop/Home/projectAI/BEC/openInsider/output/graphs/PCAanalysis/PCA_daysToFile.png")

ggOut <- ggplot(pca_dt, aes(x = PC1, y = PC2, color = Result)) +
  geom_point(alpha = 0.7) +
  theme_classic(base_size = 18) +
  labs(title = "PCA plot with metrics",
       x = "PC1",
       y = "PC2")
ggsave(ggOut, height = 6, width = 8, filename = "/Users/limyiyang/Desktop/Home/projectAI/BEC/openInsider/output/graphs/PCAanalysis/PCA_result.png")

ggOut <- ggplot(pca_dt, aes(x = PC1, y = PC2, color = DaysToFile)) +
  geom_point(alpha = 0.7) +
  scale_color_distiller(palette = "RdBu") +
  theme_classic(base_size = 18) +
  labs(title = "PCA plot with metrics",
       x = "PC1",
       y = "PC2")

insiderCount = masterlist[, .(count = .N), by = c("result", "Insider.Name")]
totalCounts = masterlist[, .(totalCount = .N), by = "Insider.Name"]
insiderCount = merge(insiderCount, totalCounts, by = "Insider.Name")
insiderCount$rate = insiderCount$count / insiderCount$totalCount *100
insiderCount = insiderCount[result == "win"]
insiderCount = insiderCount[totalCount != 1]
fwrite(insiderCount, sep = "\t", quote = F, file = "output/tradesTxt/04_topInsiderWinrate.txt")

masterlist[, .(count = .N), by = c("result")]
masterlist[, .(count = .N), by = c("result", "Insider.Name")]

sdev <- pca_res$sdev
variance <- sdev^2
prop_var <- variance / sum(variance)
percent_var <- prop_var * 100
percent_var

# Run PCA (center and scale recommended)
pca_res <- prcomp(dt[, ..num_features], center = TRUE, scale. = TRUE)
pca_coords <- as.data.table(pca_res$x)
masterlist_with_pca <- cbind(dt, pca_coords)


# calculate contribution
loadings <- pca_res$rotation   # matrix: variables x PCs
sq_load <- loadings^2
contrib <- sweep(sq_load, 2, colSums(sq_load), FUN = "/") * 100
contrib


# Win rate per group
winrate_by_group <- masterlist[, .(
  win_rate_pct = mean(result == "win", na.rm = TRUE) * 100
), by = role_group]


# Average win rate across groups
overall_win_rate_pct <- masterlist[, mean(result == "win", na.rm = TRUE) * 100]
winrate_by_group <- masterlist[, .(
  win_rate = mean(result == "win", na.rm = TRUE)
), by = role_group]
fwrite(winrate_by_group, sep = "\t", quote = F, file = "output/tradesTxt/04_topGroupWinrate.txt")

average_win_rate_across_groups_pct <- mean(winrate_by_group$win_rate) * 100
winrate_by_individual <- masterlist[, .(
  win_rate_pct = mean(result == "win", na.rm = TRUE) * 100,
  n_trades = .N
), by = c("Insider.Name", "role_group")]
fwrite(winrate_by_individual, sep = "\t", quote = F, file = "output/tradesTxt/04_topInsiderWinrate.txt")

# List of insiders to label
label_insiders <- c(
  "Monroe James III",
  "Berkshire Hathaway Inc",
  "Abdiel Capital Advisors, LP",
  "Cohn Charles K.",
  "Thrc Holdings, LP",
  "Control Empresarial De Capitales S.A. De C.V.",
  "Saba Capital Management, L.P.",
  "Global Gp LLC",
  "Barry John F"
)
label_insiders <- c(
  "Monroe James III",
  "Berkshire Hathaway Inc",
  "Abdiel Capital Advisors, LP"
)
# Add a flag column to label only these insiders
masterlist_with_pca[, Insider.Name := as.character(Insider.Name)]

# Then assign label_flag
masterlist_with_pca[, label_flag := ifelse(Insider.Name %in% label_insiders, Insider.Name, NA_character_)]

# Check the table again
table(masterlist_with_pca$label_flag)
ggOut = ggplot(masterlist_with_pca, aes(x = PC1, y = PC2, color = label_flag)) +
  geom_point(alpha = 0.6, size = 2) +
  theme_classic(base_size = 20) +
  labs(title = "PCA plot of insider trades",
       x = "PC1",
       y = "PC2",
       color = "Insider Name") +
  theme(legend.position = "bottom")
ggsave(ggOut, height = 6, width = 8, filename = "/Users/limyiyang/Desktop/Home/projectAI/BEC/openInsider/output/graphs/PCAanalysis/PCAinsider.png")


# Check how many points fall into this "cluster"
masterlist_with_pca[, cluster_flag := (PC1 > 0.2) & (PC2 < -2)]
table(masterlist_with_pca$cluster_flag)
ggplot(masterlist_with_pca, aes(x = PC1, y = PC2)) +
  geom_point(alpha = 0.5) +
  geom_point(data = masterlist_with_pca[cluster_flag == TRUE],
             aes(x = PC1, y = PC2), color = "red", size = 2) +
  theme_classic(base_size = 20) +
  labs(title = "Best Cluster",
       x = "PC1", y = "PC2")
tmp2 = masterlist_with_pca[cluster_flag == T]

