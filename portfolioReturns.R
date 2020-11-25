
#' @title Calculate Periodic Returns for a set of assets
#' @description A wrapper to \code{\link[quantmod]{periodReturn}} for multiple securities.
#' @param x An \code{xts} object of price data for multiple securities.
#' @param period Character string indicating time period. Valid entries are ‘daily’, ‘weekly’, 
#' ‘monthly’, ‘quarterly’, ‘yearly’. 
#' @param ... Other arguments passed to \code{\link[quantmod]{periodReturn}}
#' @export
#' @examples 
#' \dontrun{
#'   port <- getPortfolio(c("SPY", "IEF"), '2013-01/')
#'   R <- calcReturns(x, period= 'monthly', type= 'arithmetic')
#' }
calcReturns <- function(x, period, ...) {
  # calculate first to determine pre-allocations; then pre-allocate
  R1 <- quantmod::periodReturn(x[,1], period= period, ... = ...)
  nc <- ncol(x)
  idx <- index(R1)
  R <- matrix(nrow= length(R1), ncol= nc,
              dimnames= list(as.character(idx), colnames(x)))
  R[,1] <- R1
  # loop
  for (i in 2:nc) {
    R[,i] <- quantmod::periodReturn(x[,i], period= period, ... = ...)
  }
  # return
  return(xts::as.xts(R))
}


randomWeights <- function(wt_min, wt_max) {
  wts <- mapply(function(mn, mx) {runif(1, mn, mx)}, mn= wt_min, mx= wt_max)
  return(wts / sum(wts))
}

portfolio_returns <- function(wts, mean_returns, periods= 12) {
  (sum(wts * mean_returns) + 1)^periods - 1
}

portfolio_sd <- function(wts, Sigma, periods= 12) {
  sqrt(t(wts) %*% (Sigma %*% wts)) * sqrt(periods)
}

sharpe <- function(wts, bar_R, Sigma, periods) {
  portfolio_returns(wts, bar_R, periods) / portfolio_sd(wts, Sigma, periods)
}

effFrontier <- function(mu, rcov, nports= 10e3, wmin= -0.1, wmax= 1) {
  N <- length(mu)
  if (length(wmin) != N) {wmin <- rep(wmin, N)}
  if (length(wmax) != N) {wmax <- rep(wmax, N)}
  
  p_sim <- matrix(nrow= nports, ncol= 3); colnames(p_sim) <- c("R", "SD", "Sharpe")
  wts <- matrix(nrow= nports, ncol= N); colnames(wts) <- colnames(rcov)
  for (k in 1:nports) {
    wts[k,] <- randomWeights(wmin, wmax)
    p_sim[k, 1] <- portfolio_returns(wts[k, ], mu, periods= 12)
    p_sim[k, 2] <- portfolio_sd(wts[k, ], rcov, periods= 12)
    p_sim[k, 3] <- p_sim[k, 1] / p_sim[k, 2]
  }
  return(list(metrics=p_sim, wts=wts))
}