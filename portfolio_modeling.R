
library(quantmod)
library(xts)

# ?PerformanceAnalytics::StdDev
# ?PerformanceAnalytics::SharpeRatio
# ?tseries::portfolio.optim
# tseries:::portfolio.optim.default
# ?quadprog::solve.QP

setwd("/Users/alexwhitworth/github_projects/economic_data")
source("./getPortfolio.R")
source("./shrinkageCov.R")
source("./portfolioReturns.R")

#----------------------------------------------------------
# INPUTS
#----------------------------------------------------------
begin_dt <- '2015-06'
Rf <- 0.01/12 # risk-free return monthly
bench_wts <- c('VT'= 0.8, 'AGG'= 0.2)
max_lev <- 1

capm <- c('vt'= 0.041, 'vti'= 0.04, 'vwo'=0.051, 'iau'=0.01, 'gbtc'=0.1, 'ief'=0.0, 
          'tlt'=-0.005, 'vea'= 0.047) / 12
capm2 <- c('vt'= 0.061, 'vti'= 0.065, 'vwo'=0.07, 'iau'=0.01, 'gbtc'=0.1, 'ief'=0.0, 
          'tlt'=-0.005, 'vea'= 0.067) / 12
#----------------------------------------------------------

# Assets to model
ETF <- getPortfolio(c("VT", "AGG", "VTI", "VWO", "IAU", "GBTC", "IEF", "TLT", "VEA")
                     , begDate= begin_dt, type= "Ad")

R <- calcReturns(ETF, period= 'monthly')
R_excess <- R - Rf
Rbench <- R[,1] * bench_wts[1] + R[,2] * bench_wts[2]
cov_bench <- cov(R[,1:2])
cov_port <- cov(R[,-c(1:2)])

tseries::portfolio.optim(matrix(capm[-1], nrow=1),# R_excess[,-c(1,2)],
                         shorts= TRUE,
                         reslow= rep(-0.05, 7),
                         reshigh= rep(1, 7),
                         covmat= shrinkageCov(R[,-c(1:2)]))

tseries::portfolio.optim(R_excess[,-c(1,2)],
                         shorts= TRUE,
                         reslow= rep(-0.05, 7),
                         reshigh= rep(1, 7),
                         covmat= shrinkageCov(R[,-c(1:2)]))



set.seed(1234)
e1 <- effFrontier(colMeans(R_excess[,-c(1:2)]), rcov= shrinkageCov(R_excess[,-c(1:2)]), 
                  wmin= c(0.3, 0.05, 0.00, 0.01, rep(0,3)),
                  wmax= c(0.8, 0.2, 0.05, 0.05, 0.1, 0.1, 0.2))

e2 <- effFrontier(capm[-1], rcov= shrinkageCov(R_excess[,-c(1:2)]), 
                  wmin= c(0.3, 0.05, 0.00, 0.01, rep(0,3)),
                  wmax= c(0.8, 0.2, 0.05, 0.05, 0.1, 0.1, 0.2))


df1 <- data.frame(e1[[1]]); w1 <- e1[[2]]
df2 <- data.frame(e2[[1]]); w2 <- e2[[2]]


#----------------------------------------------------------
library(PortfolioAnalytics)
library(PerformanceAnalytics)
library(DEoptim)
library(ROI)

pspec <- portfolio.spec(colnames(R[,-c(1:2,9)]))
pspec <- add.constraint(pspec, type= "full_investment")
pspec <- add.constraint(pspec, type= 'box',
                        min= c(0.5, 0.05, 0, 0, 0, 0),
                        max= c(max_lev * 0.8, max_lev * 0.25, max_lev * 0.05, max_lev * 0.05,
                               max_lev * 0.2, max_lev * 0.2))

#pspec <- add.objective(pspec, type= 'risk', name='var')
#pspec <- add.objective(pspec, type= 'risk_budget', name= 'var', max_prisk= 0.3, arguments=list(p=0.95))
pspec <- add.objective(pspec, type= 'return', name= 'mean')
pspec <- add.objective(pspec, type= 'quadratic_utility', risk_aversion= 0.25)

m1 <- optimize.portfolio(R[,-c(1:2,9)], pspec, optimize_method= 'ROI',
                         trace=FALSE)

mx <- optimize.portfolio(R[,-c(1:2,9)], pspec, optimize_method= 'DEoptim',
                         search_size= 2000, traceDE= 10, trace=FALSE)

