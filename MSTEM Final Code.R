# Get quantmod
if (!require("quantmod")) {
  install.packages("quantmod")
  library(quantmod)
}

start <- as.Date("2016-01-01")
end <- as.Date("2016-10-01")



getSymbols("AAPL", src = "yahoo", from = start, to = end)
plot(AAPL[, "AAPL.Close"], main = "AAPL")
candleChart(AAPL, up.col = "black", dn.col = "red", theme = "white")

getSymbols(c("MSFT", "GOOG"), src = "yahoo", from = start, to = end)

stocks <- as.xts(data.frame(AAPL = AAPL[, "AAPL.Close"], MSFT = MSFT[, "MSFT.Close"], GOOG = GOOG[, "GOOG.Close"]))
head(stocks)

plot(as.zoo(stocks), screens = 1, lty = 1:3, xlab = "Date", ylab = "Price")
legend("right", c("AAPL", "MSFT", "GOOG"), lty = 1:3, cex = 0.5)
plot(as.zoo(stocks[, c("AAPL.Close", "MSFT.Close")]), screens = 1, lty = 1:2, xlab = "Date", ylab = "Price")
par(new = TRUE)
plot(as.zoo(stocks[, "GOOG.Close"]), screens = 1, lty = 3, xaxt = "n", yaxt = "n",xlab = "", ylab = "")
axis(4)
mtext("Price", side = 4, line = 3)
legend("topleft", c("AAPL (left)", "MSFT (left)", "GOOG"), lty = 1:3, cex = 0.5)
if (!require("TTR")) {
  install.packages("TTR")
  library(TTR)
}
if (!require("quantstrat")) {
  install.packages("quantstrat", repos="http://R-Forge.R-project.org")
  library(quantstrat)
}
if (!require("IKTrading")) {
  if (!require("devtools")) {
    install.packages("devtools")
  }
  library(devtools)
  install_github("IKTrading", username = "IlyaKipnis")
  library(IKTrading)
}
library(quantmod)

start <- as.Date("2010-01-01")
end <- as.Date("2016-10-01")

# Let's get Apple stock data; Apple's ticker symbol is AAPL. We use the quantmod function getSymbols, and pass a string as a first argument to identify the desired ticker symbol, pass "yahoo" to src for Yahoo! Finance, and from and to specify date ranges

# The default behavior for getSymbols is to load data directly into the global environment, with the object being named after the loaded ticker symbol. This feature may become deprecated in the future, but we exploit it now.

getSymbols("AAPL", src="yahoo", from = start, to = end)
candleChart(AAPL, up.col = "black", dn.col = "red", theme = "white", subset = "2016-01-04/")

AAPL_sma_20 <- SMA(
  Cl(AAPL),  
  n = 20     
)

AAPL_sma_50 <- SMA(
  Cl(AAPL),
  n = 50
)

AAPL_sma_200 <- SMA(
  Cl(AAPL),
  n = 200
)

zoomChart("2016")  
addTA(AAPL_sma_20, on = 1, col = "red") 
addTA(AAPL_sma_50, on = 1, col = "blue")
addTA(AAPL_sma_200, on = 1, col = "green")
AAPL_trade <- AAPL
AAPL_trade$`20d` <- AAPL_sma_20
AAPL_trade$`50d` <- AAPL_sma_50

regime_val <- sigComparison("", data = AAPL_trade,
                            columns = c("20d", "50d"), relationship = "gt") -
  sigComparison("", data = AAPL_trade,
                columns = c("20d", "50d"), relationship = "lt")

plot(regime_val["2016"], main = "Regime", ylim = c(-2, 2))
candleChart(AAPL, up.col = "black", dn.col = "red", theme = "white", subset = "2016-01-04/")
addTA(regime_val, col = "blue", yrange = c(-2, 2))
addLines(h = 0, col = "black", on = 3)
addSMA(n = c(20, 50), on = 1, col = c("red", "blue"))
zoomChart("2016")
candleChart(AAPL, up.col = "black", dn.col = "red", theme = "white", subset = "2016-01-04/")
addTA(regime_val, col = "blue", yrange = c(-2, 2))
addLines(h = 0, col = "black", on = 3)
addSMA(n = c(20, 50), on = 1, col = c("red", "blue"))
sig <- diff(regime_val) / 2
plot(sig, main = "Signal", ylim = c(-2, 2))
candleChart(AAPL, up.col = "black", dn.col = "red", theme = "white")
addTA(regime_val, col = "blue", yrange = c(-2, 2))
addLines(h = 0, col = "black", on = 3)
addSMA(n = c(20, 50), on = 1, col = c("red", "blue"))
zoomChart("2014-05/2014-07")
candleChart(adjustOHLC(AAPL), up.col = "black", dn.col = "red", theme = "white")
addLines(h = 0, col = "black", on = 3)
addSMA(n = c(20, 50), on = 1, col = c("red", "blue"))
strategy_st <- portfolio_st <- account_st <- "SMAC-20-50"  # Names of objects
rm.strat(portfolio_st)  # Need to remove portfolio from blotter env
rm.strat(strategy_st)   # Ensure no strategy by this name exists either
initPortf(portfolio_st, symbols = "AAPL_adj",  # This is a simple portfolio
          # trading AAPL only
          initDate = initDate, currency = "USD")
initAcct(account_st, portfolios = portfolio_st,  # Uses only one portfolio
         initDate = initDate, currency = "USD",
         initEq = 1000000)  # Start with a million dollars
initOrders(portfolio_st, store = TRUE)  # Initialize the order container; will
strategy(strategy_st, store = TRUE)  # store = TRUE tells function to store in
# the .strategy environment


add.indicator(strategy = strategy_st, name = "SMA",     # SMA is a function
              arguments = list(x = quote(Cl(mktdata)),  # args of SMA
                               n = 20),
              label = "fastMA")

add.signal(strategy = strategy_st, name = "sigComparison",  # Remember me?
           arguments = list(columns = c("fastMA", "slowMA"),
                            relationship = "gt"),
           label = "bull")

add.rule(strategy = strategy_st, name = "ruleSignal",  # Almost always this one
         arguments = list(sigcol = "bull",  # Signal (see above) that triggers
                          sigval = TRUE,
                          ordertype = "market",
                          orderside = "long",
                          replace = FALSE,
                          prefer = "Open",
                          osFUN = osMaxDollar,

                          maxSize = quote(floor(getEndEq(account_st,
                                                         Date = timestamp) * .1)),
                          tradeSize = quote(floor(getEndEq(account_st,
                                                           Date = timestamp) * .1))),
         type = "enter", path.dep = TRUE, label = "buy")
add.rule(strategy = strategy_st, name = "ruleSignal",
         arguments = list(sigcol = "bear",
                          sigval = TRUE,
                          orderqty = "all",
                          ordertype = "market",
                          orderside = "long",
                          replace = FALSE,
                          prefer = "Open"),
         type = "exit", path.dep = TRUE, label = "sell")
# Get new symbols
symbols = c("AAPL", "MSFT", "GOOG", "FB", "TWTR", "NFLX", "AMZN", "YHOO",
            "SNY", "NTDOY", "IBM", "HPQ")
getSymbols(Symbols = symbols, from = start, to = end)
# Quickly define adjusted versions of each of these
`%s%` <- function(x, y) {paste(x, y)}
`%s0%` <- function(x, y) {paste0(x, y)}
for (s in symbols) {
  eval(parse(text = s %s0% "_adj <- adjustOHLC(" %s0% s %s0% ")"))
}
symbols_adj <- paste(symbols, "adj", sep = "_")

stock(symbols_adj, currency = "USD", multiplier = 1)

strategy_st_2 <- portfolio_st_2 <- account_st_2 <- "SMAC-20-50_v2"
rm.strat(portfolio_st_2)
rm.strat(strategy_st_2)
initPortf(portfolio_st_2, symbols = symbols_adj,
          initDate = initDate, currency = "USD")
initAcct(account_st_2, portfolios = portfolio_st_2,
         initDate = initDate, currency = "USD",
         initEq = 1000000)
initOrders(portfolio_st_2, store = TRUE)

strategy(strategy_st_2, store = TRUE)

add.indicator(strategy = strategy_st_2, name = "SMA",
              arguments = list(x = quote(Cl(mktdata)),
                               n = 20),
              label = "fastMA")
add.indicator(strategy = strategy_st_2, name = "SMA",
              arguments = list(x = quote(Cl(mktdata)),
                               n = 50),
              label = "slowMA")

# Next comes trading signals
add.signal(strategy = strategy_st_2, name = "sigComparison",  # Remember me?
           arguments = list(columns = c("fastMA", "slowMA"),
                            relationship = "gt"),
           label = "bull")
add.signal(strategy = strategy_st_2, name = "sigComparison",
           arguments = list(columns = c("fastMA", "slowMA"),
                            relationship = "lt"),
           label = "bear")

# Finally, rules that generate trades
add.rule(strategy = strategy_st_2, name = "ruleSignal",
         arguments = list(sigcol = "bull",
                          sigval = TRUE,
                          ordertype = "market",
                          orderside = "long",
                          replace = FALSE,
                          prefer = "Open",
                          osFUN = osMaxDollar,
                          maxSize = quote(floor(getEndEq(account_st_2,
                                                         Date = timestamp) * .1)),
                          tradeSize = quote(floor(getEndEq(account_st_2,
                                                           Date = timestamp) * .1))),
         type = "enter", path.dep = TRUE, label = "buy")
add.rule(strategy = strategy_st_2, name = "ruleSignal",
         arguments = list(sigcol = "bear",
                          sigval = TRUE,
                          orderqty = "all",
                          ordertype = "market",
                          orderside = "long",
                          replace = FALSE,
                          prefer = "Open"),
         type = "exit", path.dep = TRUE, label = "sell")

applyStrategy(strategy_st_2, portfolios = portfolio_st_2)

# Now for analytics
plot(final_acct2$summary$End.Eq["2010/2016"] / 1000000,
     main = "Portfolio Equity", ylim = c(0.8, 2.5))
lines(SPY$SPY.Adjusted / SPY$SPY.Adjusted[[1]], col = "blue")