
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
