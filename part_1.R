# Autors: 
# Date: 07/11/2018

# Cleaning the env
rm(list=ls())
# Needed libraries
library(quantmod)
library(dplyr)
library(quadprog)
library(PerformanceAnalytics)
library(IntroCompFinR)

# ---------- Get and process data: ------------------

# Lets assume the initial day is 01/01/2017
tickers.list <- c("MSFT")
for (t in tickers.list){
  try(getSymbols(t,
                 from = "2017-11-07",
                 periodicity = "weekly",
                 src = "yahoo") )
}
objList <- lapply(tickers.list, get)
prices.zoo <- do.call(merge, objList)
returns.df <-as.data.frame(diff(log(Ad(prices.zoo))))
tickers.list <- as.list(tickers.list)
colnames(returns.df)<-tickers.list
# Continiously Compounded Returns for the stocks
returns.df
# Variance-covariance
COV <- var(returns.df, na.rm = TRUE)
COV
# Expected returns
ER <- exp(colMeans(returns.df, na.rm=TRUE)) - 1
ER
# Global min variance portfolio
gmvport <- globalMin.portfolio(ER, COV)
# Volatility = Standard deviation
volatility <- as.numeric(gmvport[3])
# Growth rate in case price goes up
u <- exp(volatility)
d <- 1/u
# Risk-free rate 
getSymbols("TB3MS",
           src = "FRED")
Rf <- as.numeric(tail(TB3MS, 1))/52/100
S0 <- as.numeric(tail(Ad(prices.zoo), 1))

# ---------- multiperiod binomial funciton definition: ------------------

# input: 
# call or put (option), initial stock price (S), growth factor (u), decline factor (d), 
# risk-free rate (Rf), number of simulations (n)
multiperiod_binomial <- function(option, S0, u, d, Rf, n) {
  if (option != "call" && option != "put"){
    return("error, option must be 'call' or 'put'")
  }
  cat("\n Parameters:","\n option:",option, ", S0:", S0, ", u:", u, 
      ", d:", d, ", Rf:", Rf, ", n:", n, "\n")
  # r: growth factor for risk-free investment.
  r = (1 + Rf)
  cat("\n r:",r)
  # q: f(u, d, r), prob of stock price going up
  q = (r - d) / (u - d)
  cat("\n q:", q)
  # K: strike price (any value K > S)
  K = S0 * 1.05
  cat("\n K:", K)
  acum_call_put = 0
  for (i in (1:n)) {
    S = S0
    # t: number of periods (weeks)
    t = 52
    for (j in (1:t)) {
      # state: random boolan variable to decide whether the price is going up or down.
      state = rbinom(1, 1, q)
      # calculate the stock price of the period
      # B0: risk-free bond, N: number of shares of the stock.
      if (state) {
        # Cu = (r * B0) + (N * u * S)
        S = S * u
      }
      else {
        # Cd = (r * B0) + (N * d * S)
        S = S * d
      } # end if state
    } # end for t
    if (option == "call"){
      # multiperiod binomial call
      call_put = max(0, (S-K))
    } else if (option == "put"){
      # multiperiod binomial put
      call_put = max(0, (K-S))
    }
    acum_call_put = acum_call_put + call_put
  } # end for n
  avrg_call_put = acum_call_put / n
  cat("\n average", option, ": ", avrg_call_put)
  # call_0: Present Value of avrg_call = avrg_call / (1 + Rf) ^ 52
  call_put_0 = avrg_call_put / (r ^ t)
  cat("\n present",option,"value:", call_put_0)
}

# ---------- multiperiod binomial funciton tests: ------------------

# test with incorrect option parameters
multiperiod_binomial("call error", S0, u, d, Rf, 1000)
# test call
multiperiod_binomial("call", S0, u, d, Rf, 1000)
# test put
multiperiod_binomial("put", S0, u, d, Rf, 1000)

