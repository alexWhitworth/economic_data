
library(fredr)
library(data.table)
source("00_functions.R")

# 01. Pull data from FRED API
#----------------------------------------------------------
fredr::fredr_set_key("..redacted..")

## MEASURES OF EMPLOYMENT AND INCOME
#----------------------------
# Units:  Thousands of Persons, Seasonally Adjusted
emp <- fredr(series_id = "PAYEMS", observation_start= as.Date("2003-01-01"), units= "lin")
# Units: Millions of Units, Seasonally Adjusted Annual Rate
autos <- fredr(series_id = "LAUTOSA", observation_start= as.Date("2003-01-01"), units= "lin")
hw_trucks <- fredr(series_id = "HTRUCKSSAAR", observation_start= as.Date("2003-01-01"), units= "lin")
lw_trucks <- fredr(series_id = "LTRUCKSA", observation_start= as.Date("2003-01-01"), units= "lin")
lw_vehicles <- fredr(series_id = "ALTSALES", observation_start= as.Date("2003-01-01"), units= "lin")
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
# CPI_u deflated retail sales
rrs <- merge(CPI_u[, c(1,3)], retail_sales[, c(1,3)], by= 'date')
rrs$rrs <- rrs$value.y / rrs$value.x
# Units:  Index 2012=100, Seasonally Adjusted
# REAL output of (manufacturing + industrial sector, mining, and utilities industries)
indpro <- fredr(series_id = "INDPRO", observation_start= as.Date("2003-01-01"), units= "lin")
# Units: Billions of Dollars, Seasonally Adjusted Annual Rate
# measured QUARTERLY
# Corporate profits before tax (exc. IVA + CCAdj)
cp <- fredr(series_id = "A053RC1Q027SBEA", observation_start= as.Date("2003-01-01"), units= "lin")
cp$date <- lubridate::`%m+%`(cp$date, months(3))

# MEASURES OF HOUSING
#----------------------------
# Units:  Thousands of Units, Seasonally Adjusted Annual Rate
housing_starts <- fredr(series_id = "HOUST", observation_start= as.Date("2003-01-01"), units= "lin")
# Units:  Thousands, Seasonally Adjusted Annual Rate
SFH_new <- fredr(series_id = "HSN1F", observation_start= as.Date("2003-01-01"), units= "lin")
# QUARTERLY. Percent, Seasonally Adjusted
# mort_pmt_pct_dispinc <- fredr(series_id = "TDSP", observation_start= as.Date("2003-01-01"), units= "lin")
# QUARTERLY. Percent, Seasonally Adjusted
permits <- fredr(series_id = "PERMIT", observation_start= as.Date("2003-01-01"), units= "lin")

# combine series into single dataset
dat <- list(
  data.table(date= emp$date, series= "nonfarm_payrolls", value= emp$value)
  , data.table(date= avg_earn$date, series= "hrly_earnings", value= avg_earn$value)
  , data.table(date= CPI_u$date, series= "cpi_u", value= CPI_u$value)
  , data.table(date= real_inc$date, series= "real_income", value= real_inc$value)
  , data.table(date= real_inc_disposable$date, series= "real_disposable_inc", value= real_inc_disposable$value)
  , data.table(date= real_PCE$date, series= "real_consumption", value= real_PCE$value)
  # , data.table(date= retail_sales$date, series= "retail_sales", value= retail_sales$value)
  , data.table(date= indpro$date, series= "industrial_prod", value= indpro$value)
  , data.table(date= rrs$date, series= "real_retail_sales", value= rrs$rrs)
  , data.table(date= cp$date, series= "corp_profit", value= cp$value)
)

house_dat <- list(
  data.table(date= housing_starts$date, series= "housing_starts", value= housing_starts$value)
  , data.table(date= SFH_new$date, series= "SFH_sold", value= SFH_new$value)
  #, data.table(date= mort_pmt_pct_dispinc$date, series= "mortgage_pmt_pct", value= mort_pmt_pct_dispinc$value)
  , data.table(date= permits$date, series= "housing_permits", value= permits$value)
  , data.table(date= autos$date, series= "autos", value= autos$value)
  , data.table(date= lw_trucks$date, series= "lw_trucks", value= lw_trucks$value)
  , data.table(date= hw_trucks$date, series= "hw_trucks", value= hw_trucks$value)
  , data.table(date= lw_vehicles$date, series= "lw_vehicles", value= lw_vehicles$value)
)

dat <- rbindlist(lapply(dat, function(l) {
  idx_base <- l[date == '2018-01-01', value]
  l[, `:=` (
    quarterly_yoy= quarterly_yoy(value)
    , index= value / idx_base
  )][, `:=` (
    index_SMA3= ifelse(series == 'corp_profit' # quarterly, not monthly data
                       , index, zoo::rollmean(index, k= 3, fill= NA, align= "right"))
    , index_SMA6= ifelse(series == 'corp_profit'
                       , index, zoo::rollmean(index, k= 6, fill= NA, align= "right"))
  )]
  return(l)
}))

house_dat <- rbindlist(lapply(house_dat, function(l) {
  idx_base <- l[date == '2018-01-01', value]
  l[, `:=` (
    quarterly_yoy= quarterly_yoy(value)
    , index= value / idx_base
  )][, `:=` (
    index_SMA3= zoo::rollmean(index, k= 3, fill= NA, align= "right")
    , index_SMA6= zoo::rollmean(index, k= 6, fill= NA, align= "right")
  )]
  return(l)
}))

vehicle_dat <- house_dat[series %in% c('autos', 'lw_trucks', 'hw_trucks', 'lw_vehicles'), ]
house_dat <- house_dat[!(series %in% c('autos', 'lw_trucks', 'hw_trucks', 'lw_vehicles')), ]
corp_profit <- dat[series == 'corp_profit',]
dat <- dat[series != 'corp_profit',]

rm(avg_earn, CPI_u, emp, indpro, quarterly_yoy, autos, lw_trucks, hw_trucks, real_inc, real_PCE, 
   real_inc_disposable, retail_sales, housing_starts, SFH_new, permits, lw_vehicles) # mort_pmt_pct_dispinc
rm(cp, rrs)
