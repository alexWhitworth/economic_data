
#' @title Shrinkage Covariance Estimators
#' @description Calculate the shrinkage estimator for the covariance matrix \eqn{\hat \Sigma}  to 
#' be used in portfolio optimization: \eqn{\hat \Sigma = \delta F + (1-\delta)S}. 
#' @param R An xts, matrix, data.frame, etc object of asset returns.
#' @param scalar Numeric scalar to modify the shrinkage.
#' @references Ledoit, Olivier, and Michael Wolf. "Honey, I shrunk the sample covariance matrix." 
#' The Journal of Portfolio Management 30.4 (2004): 110-119.
shrinkageCov <- function(R, scalar= 1/sqrt(ncol(R))) {
  # 00. initialize
  T <- nrow(R)
  N <- ncol(R)
  sigma <- cov(R)
  
  sii <- 0
  for (i in 1:(N-1)) {
    for (j in (i+1):N) {
      sii <- sii + sigma[i,j]
    }
  }
  
  bar_r <- 2 / (N * N-1) * sii
  bar_y <- colMeans(R)
  
  # 01. calc estimators: delta, phi, pi, F
  rho <- phi <- pi <- V_ii <- V_jj <- F <- matrix(nrow=N, ncol=N)
  f_ii <- diag(sigma); diag(F) <- f_ii
  
  for (i in 1:N) {
    for (j in 1:N) {
      pi[i,j] <- 1/T * sum( (t(R[,i] - bar_y[i]) %*% (R[,j] - bar_y[j]) - sigma[i,j])^2 )
      V_ii[i,j] <- 1/T *  
        t( (R[,i] - bar_y[i])^2 - f_ii[i]) %*% (R[,i] - bar_y[i] * (R[,j] - bar_y[j]) - sigma[i,j]) 
      V_jj[i,j] <- 1/T *  
        t( (R[,j] - bar_y[j])^2 - f_ii[j]) %*% (R[,j] - bar_y[j] * (R[,i] - bar_y[i]) - sigma[i,j]) 
      if (i != j) {
        F[i,j] <- bar_r * sqrt(f_ii[i] * f_ii[j])
        rho[i,j] <- bar_r / 2 * (
          sqrt(f_ii[j] / f_ii[i]) * V_ii[i,j] + sqrt(f_ii[i] / f_ii[j]) * V_jj[i,j]
        )
      }
      phi[i,j] <- (F[i,j] - sigma[i,j])^2
    }
  }
  
  hat_pi <- sum(pi)
  hat_phi <- sum(phi)
  hat_rho <- sum(diag(pi)) + sum(rho[upper.tri(rho)]) + sum(rho[lower.tri(rho)])
  
  # 02. calculate kappa
  kappa <- (hat_pi - hat_rho) / hat_phi
  delta <- max(0, min(kappa / T, 1)) * scalar
  
  # 03. return shrunk covariance
  return(delta * F + (1 - delta) * sigma)
}

