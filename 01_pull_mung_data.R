
library(fredr)
library(data.table)
source("00_functions.R")

# 01. Pull data from FRED API
#----------------------------------------------------------
# FRED API key == "5080ca44da1ccf74fda114d6f9650645"
fredr::fredr_set_key("5080ca44da1ccf74fda114d6f9650645")

## MEASURES OF EMPLOYMENT AND INCOME
#----------------------------
# Units:  Thousands of Persons, Seasonally Adjusted
emp <- fredr(series_id = "PAYEMS", observation_start= as.Date("2003-01-01"), units= "lin")
# Units: Monthly  Rate, Seasonally Adjusted
quits <- fredr(series_id = "JTSQUR", observation_start= as.Date("2003-01-01"), units= "lin")
# Units: Dollars per Hour, Seasonally Adjusted
avg_earn <- fredr(series_id = "CES0500000003", observation_start= as.Date("2003-01-01"), units= "lin")
# Units:  Index 1982-1984=100, Seasonally Adjusted
CPI_u <- fredr(series_id = "CPIAUCSL", observation_start= as.Date("2003-01-01"), units= "lin")
# Units:  Billions of Chained 2012 Dollars, Seasonally Adjusted Annual Rate
real_inc <- fredr(series_id = "RPI", observation_start= as.Date("2003-01-01"), units= "lin")
# Units:  Chained 2012 Dollars, Seasonally Adjusted Annual Rate
real_inc_disposable <- fredr(series_id = "A229RX0", observation_start= as.Date("2003-01-01"), units= "lin")
# Units:  Billions of Dollars, Seasonally Adjusted Annual Rate
real_PCE <- fredr(series_id = "PCEC96", observation_start= as.Date("2003-01-01"), units= "lin")

# MEASURES OF INDUSTRIAL ACTIVITY
#----------------------------
# Units:  Millions of Dollars, Seasonally Adjusted
retail_sales <- fredr(series_id = "RSXFS", observation_start= as.Date("2003-01-01"), units= "lin")
# Units:  Index 2012=100, Seasonally Adjusted
indpro <- fredr(series_id = "INDPRO", observation_start= as.Date("2003-01-01"), units= "lin")

# MEASURES OF HOUSING
#----------------------------
# Units:  Thousands of Units, Seasonally Adjusted Annual Rate
housing_starts <- fredr(series_id = "HOUST", observation_start= as.Date("2003-01-01"), units= "lin")
# Units:  Thousands, Seasonally Adjusted Annual Rate
SFH_new <- fredr(series_id = "HSN1F", observation_start= as.Date("2003-01-01"), units= "lin")
# QUARTERLY. Percent, Seasonally Adjusted
#mort_pmt_pct_dispinc <- fredr(series_id = "TDSP", observation_start= as.Date("2003-01-01"), units= "lin")
# QUARTERLY. Percent, Seasonally Adjusted
permits <- fredr(series_id = "PERMIT", observation_start= as.Date("2003-01-01"), units= "lin")

# combine series into single dataset
dat <- list(
  data.table(date= emp$date, series= "nonfarm_payrolls", value= emp$value)
  , data.table(date= quits$date, series= "quit_rate", value= quits$value)
  , data.table(date= avg_earn$date, series= "hrly_earnings", value= avg_earn$value)
  , data.table(date= CPI_u$date, series= "cpi_u", value= CPI_u$value)
  , data.table(date= real_inc$date, series= "real_income", value= real_inc$value)
  , data.table(date= real_inc_disposable$date, series= "real_disposable_inc", value= real_inc_disposable$value)
  , data.table(date= real_PCE$date, series= "real_consumption", value= real_PCE$value)
  , data.table(date= retail_sales$date, series= "retail_sales", value= retail_sales$value)
  , data.table(date= indpro$date, series= "industrial_prod", value= indpro$value)
)

house_dat <- list(
  data.table(date= housing_starts$date, series= "housing_starts", value= housing_starts$value)
  , data.table(date= SFH_new$date, series= "SFH_sold", value= SFH_new$value)
  #, data.table(date= mort_pmt_pct_dispinc$date, series= "mortgage_pmt_pct", value= mort_pmt_pct_dispinc$value)
  , data.table(date= permits$date, series= "housing_permits", value= permits$value)
)

dat <- rbindlist(lapply(dat, function(l) {
  idx_base <- l[date == '2015-01-01', value]
  l[, `:=` (
    quarterly_yoy= quarterly_yoy(value)
    , index= value / idx_base
  )]
  return(l)
}))

house_dat <- rbindlist(lapply(house_dat, function(l) {
  idx_base <- l[date == '2015-01-01', value]
  l[, `:=` (
    quarterly_yoy= quarterly_yoy(value)
    , index= value / idx_base
  )]
  return(l)
}))

rm(avg_earn, CPI_u, emp, housing_starts, indpro, quarterly_yoy, quits, real_inc, real_inc_disposable, 
   real_PCE, retail_sales, SFH_new, mort_pmt_pct_dispinc)