
library(fredr)
library(data.table)
library(ggplot2); library(scales); library(RColorBrewer)
setwd("~/github_projects/economic_data/")
source("00_functions.R")

# 01. Pull data from FRED API
#----------------------------------------------------------
source("01_pull_mung_data.R")

dt_range <- range(dat[!is.na(quarterly_yoy), date])
qtr_dts <- sort(seq.Date(dt_range[2], as.Date("2018-01-01"), "-1 quarter"))
yr_dts <- sort(seq.Date(dt_range[2], as.Date("2013-01-01"), "-1 year"))
#----------------------------------------------------------
# 02. PLOT: non housing values (ex. corp profit)
#----------------------------------------------------------
# quarterly rate of change YOY
y_lims <- range(dat[date >= as.Date('2018-01-01'), quarterly_yoy], na.rm= TRUE)
ggplot_series(dat, date_min= as.Date('2018-01-01'), date_seq= qtr_dts, use_index= FALSE, 
              y_range= c(max(y_lims[1], 0.85), min(y_lims[2], 1.3)),
              facet= "wrap") +
  labs(x= "Date", y= "Rate of Change", title= "Quarterly YoY growth in Key Economic Variables")

# indexed values
y_lims <- range(dat[date >= as.Date('2018-01-01'), index], na.rm= TRUE)
ggplot_series(dat, date_min= as.Date('2018-01-01'), date_seq= qtr_dts, use_index= TRUE, 
              y_range= c(max(y_lims[1], 0.8), min(y_lims[2], 1.3)),
              facet= "wrap") +
  labs(x= "Date", y= "Index (2018-01-01 == 1.00)", 
       title= "Indexed values of Key Economic Variables")

#----------------------------------------------------------
# 02b. PLOT: corp profit
#----------------------------------------------------------
y_lims <- range(corp_profit[date >= as.Date('2013-01-01'), quarterly_yoy], na.rm= TRUE)
ggplot_series(corp_profit, date_min= as.Date('2013-01-01'), date_seq= yr_dts, use_index= FALSE, 
              y_range= c(max(y_lims[1], 0.85), min(y_lims[2], 1.5)),
              facet= "wrap") +
    labs(x= "Date", y= "Rate of Change", title= "Quarterly YoY growth in Key Economic Variables")

# indexed values
y_lims <- range(corp_profit[date >= as.Date('2013-01-01'), index], na.rm= TRUE)
ggplot_series(corp_profit, date_min= as.Date('2013-01-01'), date_seq= yr_dts, use_index= TRUE, 
              y_range= c(max(y_lims[1], 0.9), min(y_lims[2], 1.7)),
              facet= "wrap") +
    labs(x= "Date", y= "Index (2018-01-01 == 1.00)", 
         title= "Indexed values of Key Economic Variables")

#----------------------------------------------------------
# 03. PLOT: housing values
#----------------------------------------------------------
# quarterly rate of change YOY
dt_range <- range(house_dat[!is.na(quarterly_yoy), date])
qtr_dts <- sort(seq.Date(dt_range[2], as.Date("2018-01-01"), "-1 quarter"))

y_lims <- range(house_dat[,quarterly_yoy], na.rm= TRUE)
ggplot_series(house_dat, date_min= as.Date('2018-01-01'), date_seq= qtr_dts, use_index= FALSE, 
              y_range= c(max(y_lims[1], 0.70), min(y_lims[2], 1.5)),
              facet= "grid") +
  labs(x= "Date", y= "Rate of Change", title= "Quarterly YoY growth in Key Housing Data")

# indexed values
y_lims <- range(house_dat[date >= as.Date('2018-01-01'), index], na.rm= TRUE)
ggplot_series(house_dat, date_min= as.Date('2018-01-01'), date_seq= qtr_dts, use_index= TRUE, 
              y_range= c(max(y_lims[1], 0.80), min(y_lims[2], 2)),
              facet= "grid") +
  labs(x= "Date", y= "Index (2018-01-01 == 1.00)", 
       title= "Indexed values of Key Housing Data")

#----------------------------------------------------------
# 03. PLOT: vehicle data
#----------------------------------------------------------
# quarterly rate of change YOY
dt_range <- range(vehicle_dat[!is.na(quarterly_yoy), date])
qtr_dts <- sort(seq.Date(dt_range[2], as.Date("2018-01-01"), "-1 quarter"))

y_lims <- range(vehicle_dat[date >= as.Date('2018-01-01'), quarterly_yoy], na.rm= TRUE)
ggplot_series(vehicle_dat[series %in% c('lw_vehicles', 'hw_trucks'), ], 
              date_min= as.Date('2018-01-01'), date_seq= qtr_dts, use_index= FALSE, 
              y_range= c(max(y_lims[1], 0.6), min(y_lims[2], 1.75)),
              facet= "grid") +
  labs(x= "Date", y= "Rate of Change", title= "Quarterly YoY growth in Vehicle Sales")

# indexed values
y_lims <- range(vehicle_dat[date >= as.Date('2018-01-01'), index], na.rm= TRUE)
ggplot_series(vehicle_dat[series %in% c('lw_vehicles', 'hw_trucks'), ], 
              date_min= as.Date('2018-01-01'), date_seq= qtr_dts, use_index= TRUE, 
              y_range= c(max(y_lims[1], 0.7), min(y_lims[2], 1.4)),
              facet= "grid") +
  labs(x= "Date", y= "Index (2018-01-01 == 1.00)", 
       title= "Indexed values of Vehicle Sales")
