
# https://towardsdatascience.com/implied-volatility-in-r-assess-options-risk-correctly-70fe36843474
library(quantmod)
library(data.table)
library(PerformanceAnalytics)
setwd("~/github_projects/economic_data/")
source("./getPortfolio.R")
source("./option_pricing.R")


# Input Date
symbols <- c("VTI", 'VWO', 'VEA')
strikes= list(
  'VTI'= c(190, 220)
  , 'VWO'= c(49, 53)
  , 'VEA'= c(45, 52)
)

x <- getPortfolio(symbols, begDate="2010-01", type= 'Ad')
r <- do.call('cbind', lapply(x, monthlyReturn)); colnames(r) <- colnames(x)
# https://www.treasury.gov/resource-center/data-chart-center/interest-rates/Pages/TextView.aspx?data=yield
t_3mo <- 0.09 # current 3-mo treasury yield; 
# current dividend yields -- needs to be looked up manually since yahooQF() doesn't work for ETFs
div= c('vti'= 0.0144, 'vea'= 0.0205, 'vwo'= 0.0194) 

# get option chain for next 3 expirations
opts <- vector('list') 
for (i in symbols) {
  opts[[i]] <- optionChain(i, n= 3, which= 'calls', strikes= strikes[[i]])
}
rm(i, strikes)
  
# historical return and volatility
lapply(r, function(l) round(quantile(l, c(0.05, 0.1, 0.5, 0.9, 0.95)), 3))
lapply(r, sd) # historical monthly sigma

# IV calcs is an annualized measure. Need to convert to monthly






#calculating IV
calls$iv <- apply(calls, 1, getIV, S = lastPrice, r = 0.0011, q = divYield, type = "call")
puts$iv <- apply(puts, 1, getIV, S = lastPrice, r = 0.0011, q = divYield, type = "put")

#create grids
ivGridCalls <- acast(calls, ttm ~ moneyness, value.var = "iv")
ivGridPuts <- acast(puts, ttm ~ moneyness, value.var = "iv")

#get coordinates of NAs in grid
toInterpolate <- which(is.na(ivGridCalls))
coords <- cbind(toInterpolate%%dim(ivGridCalls)[1], toInterpolate%/%dim(ivGridCalls)[1] + 1)
coords[coords[,1] == 0, 2] <- coords[coords[,1] == 0, 2] - 1 
coords[coords[,1] == 0, 1] <- dim(ivGridCalls)[1]

#loop through NAs and interpolate
for(i in 1:nrow(coords)){
  #get the coordinates of a 10x10 area around the missing value
  x1 <- max(coords[i,1] - 10, 1)
  x2 <- min(coords[i,1] + 10, dim(ivGridCalls)[1])
  y1 <- max(coords[i,2] - 10, 1)
  y2 <- min(coords[i,2] + 10, dim(ivGridCalls)[2])
  
  #get the moneyness/time to mat combination of the missing value
  x0 <- as.numeric(rownames(ivGridCalls)[coords[i,1]])
  y0 <- as.numeric(colnames(ivGridCalls)[coords[i,2]])
  
  #get the part of the grid that is used to interpolate and remove all missing values that are present
  interpGrid <- ivGridCalls[x1:x2,y1:y2]
  interpGrid <- melt(interpGrid)
  interpGrid <- na.omit(interpGrid)
  
  #interpolate linearly
  interpVal <- interp(x = interpGrid$Var1, y = interpGrid$Var2, z = interpGrid$value,
                      xo = x0, yo = y0,
                      linear = TRUE, extrap = TRUE)$z[1,1]
  
  #if linear interpolation doesnt yield a result, use spline interpolation
  if(is.na(interpVal)){
    interpVal <- interp(x = interpGrid$Var1, y = interpGrid$Var2, z = interpGrid$value,
                        xo = x0, yo = y0,
                        linear = FALSE, extrap = TRUE)$z[1,1]
  }
  
  #if the resulting value is clearly wrong, e.g. negative or way outside the values that are used to interpolate,
  #leave it as NA
  if(interpVal < 0 | interpVal > max(interpGrid$value * 1.5)){
    interpVal <- NA
  }
  
  #replace the value with the result of the interpolation
  ivGridCalls[coords[i,1],coords[i,2]] <- interpVal
}

#plot the resulting implied volatility surface
xaxx <- list(
  gridcolor='rgb(255, 255, 255)',
  zerolinecolor='rgb(255, 255, 255)',
  showbackground=TRUE,
  backgroundcolor='rgb(230, 230,230)',
  title = "Moneyness"
)

yaxx <- list(
  gridcolor='rgb(255, 255, 255)',
  zerolinecolor='rgb(255, 255, 255)',
  showbackground=TRUE,
  backgroundcolor='rgb(230, 230,230)',
  title = "Time To Maturity"
)

zaxx <- list(
  gridcolor='rgb(255, 255, 255)',
  zerolinecolor='rgb(255, 255, 255)',
  showbackground=TRUE,
  backgroundcolor='rgb(230, 230,230)',
  tickformat = "%",
  title = "Implied Volatility"
)

fig <- plot_ly(x = colnames(ivGridCalls), y =  rownames(ivGridCalls), z = ivGridCalls)
fig <- fig %>% add_surface()
fig <- fig %>% layout(scene = list(xaxis=xaxx, yaxis=yaxx, zaxis = zaxx))
fig <- fig %>% plotly::colorbar(title = "", x = 0.9, y = 0.75, tickformat = "%")
fig