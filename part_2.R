# install libraries
# stackoverflow instalation of quantstrat:
#install.packages("devtools")
#require(devtools)
#devtools::install_github("braverock/blotter") # dependency
#devtools::install_github("braverock/quantstrat")

# import libraries
library(quantmod)
library(quantstrat)
library(dplyr)
library(quadprog)
library(PerformanceAnalytics)
library(FinancialInstrument)

# Cleaning the env
rm(list=ls())

# Define your trade size and initial equity
tradesize <- 100000
initeq <- 100000

# Define the names of your strategy, portfolio and account
strategy.st <- portfolio.st <- account.st <- "teamstrat"

# Remove the existing portfolio and strategy if it exists
rm.strat(portfolio.st)
rm.strat(strategy.st)

# An initialization date for the backtest
initdate <- "1999-01-01"
# The start of the data
from <- "2003-01-01"
# The end of the data
to <- "2015-12-31"

Sys.setenv(TZ = "UTC")
currency("USD")

# Retrieve SPY from yahoo
getSymbols("SPY", from=from, to=to, src="yahoo", adjust=TRUE)

# Use stock() to initialize SPY and set currency to USD
stock("SPY", currency = "USD")

# Initialize the portfolio
initPortf(portfolio.st, symbols = "SPY", initDate = initdate, currency = "USD")

# Initialize the account
initAcct(account.st, portfolios = portfolio.st, initDate = initdate, currency = "USD", initEq = initeq)

# Initialize the orders
initOrders(portfolio.st, initDate = initdate)

strategy(strategy.st, store=TRUE)

# -------------------- Add Indicators --------------------

# Create a 200-day SMA
spy_sma <- SMA(x = Cl(SPY), n = 200)

# Create an RSI with a 3-day lookback period
spy_rsi <- RSI(price = Cl(SPY), n = 3)

# Add a 200-day SMA indicator to strategy.st
add.indicator(strategy = strategy.st, 
              # Add the SMA function
              name = "SMA", 
              # Create a lookback period
              arguments = list(x = quote(Cl(mktdata)), n = 200),
              # Label your indicator SMA200
              label = "SMA200")

# Add a 50-day SMA indicator to strategy.st
add.indicator(strategy = strategy.st, 
              # Add the SMA function
              name = "SMA", 
              # Create a lookback period
              arguments = list(x = quote(Cl(mktdata)), n = 50), 
              # Label your indicator SMA50
              label = "SMA50")


# Add an RSI 3 indicator to strategy.st
add.indicator(strategy = strategy.st, 
              # Add the RSI 3 function
              name = "RSI", 
              # Create a lookback period
              arguments = list(price = quote(Cl(mktdata)), n = 3), 
              # Label your indicator RSI_3
              label = "RSI_3")

# Write the calc_RSI_avg function
calc_RSI_avg <- function(price, n1, n2) {
  
  # RSI 1 takes an input of the price and n1
  RSI_1 <- RSI(price = price, n = n1)
  
  # RSI 2 takes an input of the price and n2
  RSI_2 <- RSI(price = price, n = n2)
  
  # RSI_avg is the average of RSI_1 and RSI_2
  RSI_avg <- (RSI_1 + RSI_2)/2
  
  # Your output of RSI_avg needs a column name of RSI_avg
  colnames(RSI_avg) <- "RSI_avg"
  return(RSI_avg)
}

# Add this function as RSI_3_4 to your strategy with n1 = 3 and n2 = 4
add.indicator(strategy.st, name = "RSI", arguments = list(price = quote(Cl(mktdata)), n1 = 3, n2 = 4), label = "RSI_3_4")

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

# Add the DVO indicator to your strategy
add.indicator(strategy = strategy.st, name = "DVO", 
              arguments = list(HLC = quote(HLC(mktdata)), navg = 2, percentlookback = 126),
              label = "DVO_2_126")

# Use applyIndicators to test out your indicators
test <- applyIndicators(strategy = strategy.st, mktdata = OHLC(SPY))

# Subset your data between Sep. 1 and Sep. 5 of 2013
test_subset <- test["2013-09-01/2013-09-05"]


# -------------------- Add Signals --------------------

# Add a sigComparison which specifies that SMA50 must be greater than SMA200, call it longfilter
add.signal(strategy.st, name = "sigComparison", 
           # We are interested in the relationship between the SMA50 and the SMA200
           arguments = list(columns = c("SMA50", "SMA200"), 
                            # Particularly, we are interested when the SMA50 is greater than the SMA200
                            relationship = "gt"),
           # Label this signal longfilter
           label = "longfilter")

# Add a sigCrossover which specifies that the SMA50 is less than the SMA200 and label it filterexit
add.signal(strategy.st, name = "sigCrossover",
           # We're interested in the relationship between the SMA50 and the SMA200
           arguments = list(columns = c("SMA50", "SMA200"),
                            # The relationship is that the SMA50 crosses under the SMA200
                            relationship = "lt"),
           # Label it filterexit
           label = "filterexit")

# Implement a sigThreshold which specifies that DVO_2_126 must be less than 20, label it longthreshold
add.signal(strategy.st, name = "sigThreshold", 
           # Use the DVO_2_126 column
           arguments = list(column = "DVO_2_126", 
                            # The threshold is 20
                            threshold = 20, 
                            # We want the oscillator to be under this value
                            relationship = "lt",
                            # We're interested in every instance that the oscillator is less than 20
                            cross = FALSE),
           # Label it longthreshold
           label = "longthreshold")

# Add a sigThreshold signal to your strategy that specifies that DVO_2_126 must cross above 80 and label it thresholdexit
add.signal(strategy.st, name = "sigThreshold",
           # Reference the column of DVO_2_126
           arguments = list(column = "DVO_2_126", 
                            # Set a threshold of 80
                            threshold = 80, 
                            # The oscillator must be greater than 80
                            relationship = "gt", 
                            # We are interested only in the cross
                            cross = TRUE), 
           # Label it thresholdexit
           label = "thresholdexit")

# Create your dataset: test
test_init <- applyIndicators(strategy.st, mktdata = OHLC(SPY))
test <- applySignals(strategy = strategy.st, mktdata = test_init)

# Add a sigFormula signal to your code specifying that both longfilter and longthreshold must be TRUE, label it longentry
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

# Add a rule that uses an osFUN to size an entry position
'
add.rule(strategy = strategy.st, name = "ruleSignal",
         arguments = list(sigcol = "longentry", sigval = TRUE, ordertype = "market",
                          orderside = "long", replace = FALSE, prefer = "Open",
                          # Use the osFUN called osMaxDollar
                          osFUN = osMaxDollar,
                          # The tradeSize argument should be equal to tradesize (defined earlier)
                          tradeSize = tradesize,
                          # The maxSize argument should be equal to tradesize as well
                          maxSize = tradesize),
         type = "enter")
'

# -------------------- Analyze Results --------------------

# Use applyStrategy() to apply your strategy. Save this to out
out <- applyStrategy(strategy = strategy.st, portfolios = portfolio.st)

# Update your portfolio (portfolio.st)
updatePortf(portfolio.st)
daterange <- time(getPortfolio(portfolio.st)$summary)[-1]

# Update your account (account.st)
updateAcct(account.st, daterange)
updateEndEq(account.st)

# Get the tradeStats for your portfolio
tstats <- tradeStats(Portfolios = portfolio.st)

# Print the profit factor
tstats$Profit.Factor

# Use chart.Posn to view your system's performance on SPY
chart.Posn(Portfolio = portfolio.st, Symbol = "SPY")

# Compute the SMA50
sma50 <- SMA(x = Cl(SPY), n = 50)

# Compute the SMA200
sma200 <- SMA(x = Cl(SPY), n = 200)

# Compute the DVO_2_126 with an navg of 2 and a percentlookback of 126
dvo <- DVO(HLC = HLC(SPY), navg = 2, percentlookback = 126)

# Recreate the chart.Posn of the strategy from the previous exercise
chart.Posn(Portfolio = portfolio.st, Symbol = "SPY")

# Overlay the SMA50 on your plot as a blue line
add_TA(sma50, on = 1, col = "blue")

# Overlay the SMA200 on your plot as a red line
add_TA(sma200, on = 1, col = "red")

# Add the DVO_2_126 to the plot in a new window
add_TA(dvo)

# ---> Sharpe ratio

# Get instrument returns
instrets <- PortfReturns(portfolio.st)

# Compute Sharpe ratio from returns
SharpeRatio.annualized(instrets, geometric = FALSE)

