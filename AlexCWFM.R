
library(quantmod)
library(xts)
library(PerformanceAnalytics)
source("./getPortfolio.R")

# Input Date
begDate <- '2010-10' # 2020-10
endDate <- NULL # 2030-10

portAlex <- getPortfolio(c("BA", "DE", "HON", "LMT"), begDate= begDate, type='Ad')
portCWMF <- getPortfolio(c("AVB", "ESS", "EQR", "SUI"), begDate= begDate, type='Ad')

Return.annualized(dailyReturn(portAlex, type= 'log'), scale= 252)
Return.annualized(dailyReturn(portCWMF, type= 'log'), scale= 252)

# Sharpe ratio
SharpeRatio.annualized(dailyReturn(portAlex, type= 'log'), Rf= 0, scale= 252)
SharpeRatio.annualized(dailyReturn(portCWMF, type= 'log'), Rf= 0, scale= 252)

