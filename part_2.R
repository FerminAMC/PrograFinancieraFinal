
## Part 2, Portfolio strategy

# Instalation of quantstrat:
# install.packages("devtools")
# require(devtools)
# devtools::install_github("braverock/blotter") # dependency
# devtools::install_github("braverock/quantstrat")

# We add the needed libraries:
library(quantmod)
library(quantstrat)
library(dplyr)
library(quadprog)
library(PerformanceAnalytics)
library(FinancialInstrument)

# Cleaning the environment:
rm(list=ls())


# Declare the DVO function
DVO <- function(HLC, navg = 2, percentlookback = 126) {
  # Compute the ratio between closing prices to the average of high and low
  ratio <- Cl(HLC)/((Hi(HLC) + Lo(HLC))/2)
  # Smooth out the ratio outputs using a moving average
  avgratio <- SMA(ratio, n = navg)
  # Convert ratio into a 0-100 value using runPercentRank()
  out <- runPercentRank(avgratio, n = percentlookback, exact.multiplier = 1) * 100
  colnames(out) <- "DVO"
  return(out)
}

## Set the portafolio parameters
# set strategy, portfolio and account name
strategy.st <- portfolio.st <- account.st <- "teamstrat"
tradesize_initeq = 100000
symbol = "MSFT"
# SMA: Simple Moving Average
SMA_big = 200
SMA_small = 50
# RSI: Relative Strength Index
RSI_x = 3
# DVO: David Varadi Oscillator
DVO_x = 2
# An initialization date for the backtest
initdate <- "2005-01-01"
# The start of the data
from <- "2010-01-01"
# The end of the data
to <- "2018-11-20"

# Retrieve symbol from yahoo
getSymbols(symbol, from=from, to=to, src="yahoo", adjust=TRUE)

# We defined a portfolio strategy function to make it easier to adjust the strategy parameters and find better combinations for the indicators, signals and rules.  
BuildPortfolioStrategy <- function(tradesize_initeq, symbol, initdate, SMA_big, SMA_small, RSI_x, DVO_x) {
  # Define the trade size and initial equity
  # param: tradesize_initeq
  tradesize <- tradesize_initeq
  initeq <- tradesize_initeq
  
  # Define the names of the strategy, portfolio and account
  strategy.st <- portfolio.st <- account.st <- "teamstrat"
  
  # Remove the existing portfolio and strategy if it exists
  rm.strat(portfolio.st)
  rm.strat(strategy.st)
  
  Sys.setenv(TZ = "UTC")
  currency("MXN")
  
  # Use stock() to initialize symbol and set currency to MXN
  # param: symbol
  stock(symbol, currency = "MXN")
  
  # Initialize the portfolio
  initPortf(portfolio.st, symbols = symbol, initDate = initdate, currency = "MXN")
  
  # Initialize the account
  initAcct(account.st, portfolios = portfolio.st, initDate = initdate, currency = "MXN", initEq = initeq)
  
  # Initialize the orders
  initOrders(portfolio.st, initDate = initdate)
  
  strategy(strategy.st, store=TRUE)
  
  # -------------------- Add Indicators --------------------
  
  # Add a X-day SMA indicator to strategy.st (big)
  # param: SMA_big
  add.indicator(strategy = strategy.st, 
                # Add the SMA function
                name = "SMA", 
                # Create a lookback period
                arguments = list(x = quote(Cl(mktdata)), n = SMA_big),
                # Label the indicator SMA_big
                label = "SMA_big")
  
  # Add a Y-day SMA indicator to strategy.st (small, X > Y)
  # param: SMA_small
  add.indicator(strategy = strategy.st, 
                # Add the SMA function
                name = "SMA", 
                # Create a lookback period
                arguments = list(x = quote(Cl(mktdata)), n = SMA_small), 
                # Label the indicator SMA_small
                label = "SMA_small")
  
  
  # Add an RSI X indicator to strategy.st
  # param: RSI_x
  add.indicator(strategy = strategy.st, 
                # Add the RSI function
                name = "RSI", 
                # Create a lookback period
                arguments = list(price = quote(Cl(mktdata)), n = RSI_x), 
                # Label the indicator RSI_x
                label = "RSI_x")
  
  # Add the DVO indicator to the strategy
  # param: DVO_x
  add.indicator(strategy = strategy.st, name = "DVO", 
                arguments = list(HLC = quote(HLC(mktdata)), navg = DVO_x, percentlookback = 126),
                label = "DVO_x_126")
  
  
  # -------------------- Add Signals --------------------
  
  # Add a sigComparison which specifies that SMA50 must be greater than SMA200, call it longfilter
  add.signal(strategy.st, name = "sigComparison", 
             # We are interested in the relationship between the SMA50 and the SMA200
             arguments = list(columns = c("SMA_small", "SMA_big"), 
                              # Particularly, we are interested when the SMA50 is greater than the SMA200
                              relationship = "gt"),
             # Label this signal longfilter
             label = "longfilter")
  
  # Add a sigCrossover which specifies that the SMA50 is less than the SMA200 and label it filterexit
  add.signal(strategy.st, name = "sigCrossover",
             # We're interested in the relationship between the SMA50 and the SMA200
             arguments = list(columns = c("SMA_small", "SMA_big"),
                              # The relationship is that the SMA50 crosses under the SMA200
                              relationship = "lt"),
             # Label it filterexit
             label = "filterexit")
  
  # Implement a sigThreshold which specifies that DVO_x_126 must be less than 20, label it longthreshold
  add.signal(strategy.st, name = "sigThreshold", 
             # Use the DVO_x_126 column
             arguments = list(column = "DVO_x_126", 
                              # The threshold is 20
                              threshold = 20, 
                              # We want the oscillator to be under this value
                              relationship = "lt",
                              # We're interested in every instance that the oscillator is less than 20
                              cross = FALSE),
             # Label it longthreshold
             label = "longthreshold")
  
  # Add a sigThreshold signal to the strategy that specifies that DVO_x_126 must cross above 80 and label it thresholdexit
  add.signal(strategy.st, name = "sigThreshold",
             # Reference the column of DVO_x_126
             arguments = list(column = "DVO_x_126", 
                              # Set a threshold of 80
                              threshold = 80, 
                              # The oscillator must be greater than 80
                              relationship = "gt", 
                              # We are interested only in the cross
                              cross = TRUE), 
             # Label it thresholdexit
             label = "thresholdexit")
  
  # Create the dataset: test
  test_init <- applyIndicators(strategy.st, mktdata = OHLC(get(symbol)))
  test <- applySignals(strategy = strategy.st, mktdata = test_init)
  
  # Add a sigFormula signal to the code specifying that both longfilter and longthreshold must be TRUE, label it longentry
  add.signal(strategy.st, name = "sigFormula",
             # Specify that longfilter and longthreshold must be TRUE
             arguments = list(formula = "longfilter & longthreshold", 
                              # Specify that cross must be TRUE
                              cross = TRUE),
             # Label it longentry
             label = "longentry")
  
  
  # -------------------- Add Rules --------------------
  
  # Fill in the rule's type as exit
  add.rule(strategy.st, name = "ruleSignal", 
           arguments = list(sigcol = "filterexit", sigval = TRUE, orderqty = "all", 
                            ordertype = "market", orderside = "long", 
                            replace = FALSE, prefer = "Open"), 
           type = "exit")
  
  # Create an entry rule of 1 share when all conditions line up to enter into a position
  add.rule(strategy.st, name = "ruleSignal", 
           # Use the longentry column as the sigcol
           arguments=list(sigcol = "longentry", 
                          # Set sigval to TRUE
                          sigval = TRUE, 
                          # Set orderqty to 1
                          orderqty = 1,
                          # Use a market type of order
                          ordertype = "market",
                          # Take the long orderside
                          orderside = "long",
                          # Do not replace other signals
                          replace = FALSE, 
                          # Buy at the next day's opening price
                          prefer = "Open"),
           # This is an enter type rule, not an exit
           type = "enter")
}

### Build portafolio strategy with the defined parameters
BuildPortfolioStrategy(tradesize_initeq, symbol, initdate, SMA_big, SMA_small, RSI_x, DVO_x)

### Analyze Results
# Use applyStrategy() to apply the strategy. Save this to out
out <- applyStrategy(strategy = strategy.st, portfolios = portfolio.st)

# Update the portfolio (portfolio.st)
updatePortf(portfolio.st)
daterange <- time(getPortfolio(portfolio.st)$summary)[-1]

# Update the account (account.st)
updateAcct(account.st, daterange)
updateEndEq(account.st)

# Get the tradeStats for the portfolio
tstats <- tradeStats(Portfolios = portfolio.st)

# Print the profit factor
tstats$Profit.Factor

# Use chart.Posn to view the system's performance on the symbol
chart.Posn(Portfolio = portfolio.st, Symbol = symbol)

# Compute the SMA_small 
sma_s <- SMA(x = Cl(get(symbol)), n = SMA_small)

# Compute the SMA_big
sma_b <- SMA(x = Cl(get(symbol)), n = SMA_big)

# Compute the DVO_x_126 with an navg of DVO_x and a percentlookback of 126
dvo <- DVO(HLC = HLC(get(symbol)), navg = DVO_x, percentlookback = 126)

# Recreate the chart.Posn of the strategy from the previous exercise
chart.Posn(Portfolio = portfolio.st, Symbol = symbol)

# Overlay the sma_small on the plot as a blue line
add_TA(sma_s, on = 1, col = "blue")

# Overlay the SMA_big on the plot as a red line
add_TA(sma_b, on = 1, col = "red")

# Add the DVO_x_126 to the plot in a new window
add_TA(dvo)

# ---> Sharpe ratio

# Get instrument returns
instrets <- PortfReturns(portfolio.st)

# Compute Sharpe ratio from returns
SharpeRatio.annualized(instrets, geometric = FALSE)


### Compare with different parameters:
# SMA: Simple Moving Average
SMA_big = 150
SMA_small = 30
# RSI: Relative Strength Index
RSI_x = 4
# DVO: David Varadi Oscillator
DVO_x = 3

### Build portafolio strategy with the defined parameters
BuildPortfolioStrategy(tradesize_initeq, symbol, initdate, SMA_big, SMA_small, RSI_x, DVO_x)

### Analyze Results
# Use applyStrategy() to apply the strategy. Save this to out
out <- applyStrategy(strategy = strategy.st, portfolios = portfolio.st)

# Update the portfolio (portfolio.st)
updatePortf(portfolio.st)
daterange <- time(getPortfolio(portfolio.st)$summary)[-1]

# Update the account (account.st)
updateAcct(account.st, daterange)
updateEndEq(account.st)

# Get the tradeStats for the portfolio
tstats <- tradeStats(Portfolios = portfolio.st)

# Print the profit factor
tstats$Profit.Factor

# Use chart.Posn to view the system's performance on the symbol
chart.Posn(Portfolio = portfolio.st, Symbol = symbol)

# Compute the SMA_small 
sma_s <- SMA(x = Cl(get(symbol)), n = SMA_small)

# Compute the SMA_big
sma_b <- SMA(x = Cl(get(symbol)), n = SMA_big)

# Compute the DVO_x_126 with an navg of DVO_x and a percentlookback of 126
dvo <- DVO(HLC = HLC(get(symbol)), navg = DVO_x, percentlookback = 126)

# Recreate the chart.Posn of the strategy from the previous exercise
chart.Posn(Portfolio = portfolio.st, Symbol = symbol)

# Overlay the sma_small on the plot as a blue line
add_TA(sma_s, on = 1, col = "blue")

# Overlay the SMA_big on the plot as a red line
add_TA(sma_b, on = 1, col = "red")

# Add the DVO_x_126 to the plot in a new window
add_TA(dvo)

# ---> Sharpe ratio

# Get instrument returns
instrets <- PortfReturns(portfolio.st)

# Compute Sharpe ratio from returns
SharpeRatio.annualized(instrets, geometric = FALSE)