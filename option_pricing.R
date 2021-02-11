
#' @title Generalized Black Scholes-Merton
#' @description Calculate the price of a European call or put option via closed-form solution.
#' @param S stock price.
#' @param X strike price.
#' @param sigma volatility
#' @param r risk free rate of return (eg. 3-month treasury).
#' @param q Dividend yield
#' @param days Option time to maturity in days
#' @param type Option type - one of \code{c('call', 'put')}
#' @references \url{https://towardsdatascience.com/implied-volatility-in-r-assess-options-risk-correctly-70fe36843474}
gBSM <- function(S, X, sigma, r, q, days, type= c('call', 'put')) {
  b <- r - q
  yrs <- days / 365.25
  
  d1 <- (log(S / X) + (b + sigma^2 / 2) * yrs) / (sigma * sqrt(yrs))
  d2 <- d1 - sigma * sqrt(yrs)
  
  if (type == "call") {
    price <- S * exp((b - r) * yrs) * pnorm(d1) - X * exp(-r * yrs) * pnorm(d2)
  } else if (type == "put"){
    price <-  (X * exp(-r * yrs) * pnorm(-d2) - S * exp((b - r) * yrs) * pnorm(-d1))
  }
  
  return(price)
}



#' @title Inverse BSM pricing function.
#' @description Inverse generalized Black Scholes-Merton optimizer to calculate implied volatility.
#' @param sigma volatility
#' @param price stock price
#' @param S stock price.
#' @param K option strike price.
#' @param r risk free rate of return (eg. 3-month treasury).
#' @param q Dividend yield
#' @param days Option time to maturity in days
#' @param type Option type - one of \code{c('call', 'put')}
inv_gBSM <- function(sigma, price, S, K, r, q, days, type){
  abs(price - gBSM(S, K, sigma, r, q, days, type))
}

# wrapper for inv_gBSM so it can be used with apply
#' @param S stock price.
#' @param X option asking price.
#' @param sigma volatility
#' @param r risk free rate of return (eg. 3-month treasury).
#' @param q Dividend yield
#' @param days Option time to maturity in days
#' @param type Option type - one of \code{c('call', 'put')}
getIV <- function(S, X, r, q, days, type) {
  res <- optimize(inv_gBSM, interval = c(0, 2), 
                  S = S, X = X, r = r, q = q, days = days, type = type)
  return(res$minimum)
}
