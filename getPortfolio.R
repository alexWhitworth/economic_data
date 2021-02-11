
#' @title Download Portfolio Data
#' @description Wrapper to \code{\link[quantmod]{getSymbols}} to pull data on
#' a set of symbols
#' @param ticks A vector of Symbols to retrieve price information on.
#' @param begDate The beginning date for your portfolio data.
#' @param endDate The end date for your portfolio data. Defaults to NULL
#' @param type Specifies data you wish to extract from a OHLC object. Defaults to 'Ad'
#' C("Ad", "Op", "Hi", "Lo", "Cl", "Vo")
#' @return An \code{xts} list object with one vector-element for each series, where each series is
#' the extracted series specified by \code{type}.
#' @export
#' @example port1 <- getPortfolio(c("SPY", "IEF"), '2013-01/')
getPortfolio <- function(ticks, begDate, endDate= NULL, type= 'Ad') {
  # get extraction function
  if (type == 'Ad') {
    method.fn <- quantmod::Ad
  } else if (type == 'Op') {
    method.fn <- quantmod::Op
  } else if (type == 'Hi') {
    method.fn <- quantmod::Hi
  } else if (type == 'Lo') {
    method.fn <- quantmod::Lo
  } else if (type == 'Cl') {
    method.fn <- quantmod::Cl
  } else if (type == 'Vo') {
    method.fn <- quantmod::Vo
  } else {
    stop("Please input appropriate extraction method.")
  }
  if(!(is.null(begDate)) || !(is.null(endDate))) {
    subCriteria <- paste(begDate, endDate, sep= "/")
  }
  quantmod::getSymbols(Symbols= ticks) 
  port1 <- method.fn(get(ticks[1])[subCriteria])
  port <- matrix(nrow= length(port1), ncol= length(ticks),
                 dimnames= list(as.character(index(port1)), tolower(ticks)))
  port[,1] <- port1
  for (i in 2:length(ticks)) {
    port[,i] <- method.fn(get(ticks[i])[subCriteria])
  }
  return(xts::as.xts(port))
}

#' @title Calculation Option Expiry Dates
#' @description Calculate date of option expirys given beginning date and length of time.
#' @param begDate The beginning date for your portfolio data.
#' @param ndays An integer for the number of days to search along. Defaults to 90.
optionExpiryDt <- function(begDate, ndays= 90) {
  begDate <- as.POSIXlt(seq(as.Date(begDate), by = 1, length.out= ndays))
  d <- begDate[begDate$wday == 5]
  return(as.Date(d[ave(d$mon, list(d$mon, d$year), FUN= seq_along) == 3]))
}


#' @title Download Option Chain
#' @description Wrapper to \code{\link[quantmod]{getOptionChain}} to pull next \code{n} time periods
#' of option data. 
#' @param symbol Stock symbol
#' @param n Number of future dates to pull option data on.
#' @param which One of \code{c('calls', 'puts')}.
#' @param strikes Optional bounds for strike prices to pull data on. Input as \code{c(<min>, <max>)}
#' @return A \code{data.table} of pricing data.
optionChain <- function(symbol, n= 3, which= c('calls', 'puts'),
                        strikes= NULL) {
  require(data.table)
  x <- quantmod::getOptionChain(symbol, NULL)
  x <- x[1:n]
  out <- data.table::rbindlist(mapply(FUN= function(DT, nms) {
    return(
      data.table::data.table(
        dt_name= nms
        , strike= DT$Strike
        , last= DT$Last
        , bid= DT$Bid
        , ask= DT$Ask
        , vol= DT$Vol
        , oi= DT$OI
        
      )
    )
  }, DT= lapply(x, '[[', which), nms= as.list(names(x[1:n]))
  , SIMPLIFY = FALSE))
  
  # subset to strike prices
  if (!is.null(strikes) && length(strikes) == 2) {
    out <- out[strike >= strikes[1] & strike <= strikes[2],]
  }
  
  return(out)
}
