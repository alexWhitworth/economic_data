
library(quantmod)
library(xts)
library(PerformanceAnalytics)

#' @param ticks A vector of Symbols to retrieve price information on.
#' @param begDate The beginning date for your portfolio data.
#' @param endDate The end date for your portfolio data. Defaults to NULL
#' @param type Specifies data you wish to extract from a OHLC object. Defaults to 'Ad'
#' C("Ad", "Op", "Hi", "Lo", "Cl", "Vo")
#' @return An \code{xts} list object with one vector-element for each series, where each series is
#' the extracted series specified by \code{type}.
#' @export
#' @example port1 <- getPortfolio(c("SPY", "BOND"), '2013-01/')
getPortfolio <- function(ticks, begDate, endDate= NULL, type= 'Ad') {
  require(quantmod)
  # get extraction function
  if (type == 'Ad') {
    method.fn <- Ad
  } else if (type == 'Op') {
    method.fn <- Op
  } else if (type == 'Hi') {
    method.fn <- Hi
  } else if (type == 'Lo') {
    method.fn <- Lo
  } else if (type == 'Cl') {
    method.fn <- Cl
  } else if (type == 'Vo') {
    method.fn <- Vo
  } else {
    stop("Please input appropriate extraction method.")
  }
  if(!(is.null(begDate)) || !(is.null(endDate))) {
    subCriteria <- paste(begDate, endDate, sep= "/")
  }
  getSymbols(Symbols= ticks) 
  port1 <- method.fn(get(ticks[1])[subCriteria])
  port <- matrix(nrow= length(port1), ncol= length(ticks),
                 dimnames= list(as.character(index(port1)), tolower(ticks)))
  port[,1] <- port1
  for (i in 2:length(ticks)) {
    port[,i] <- method.fn(get(ticks[i])[subCriteria])
  }
  return(as.xts(port))
}

# Input Date
begDate <- '2010-10' # 2020-10
endDate <- NULL # 2030-10

portAlex <- getPortfolio(c("BA", "DE", "HON", "LMT"), begDate= begDate, type='Ad')
portCWMF <- getPortfolio(c("AVB", "ESS", "EQR", "SUI"), begDate= begDate, type='Ad')

Return.annualized(dailyReturn(portAlex), scale= 252)
Return.annualized(dailyReturn(portCWMF), scale= 252)

# Sharpe ratio
SharpeRatio.annualized(returnsAlex, Rf= 0, scale= 252)
SharpeRatio.annualized(returnsCWFM, Rf= 0, scale= 252)

