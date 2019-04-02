
library(fredr)
library(data.table)
library(ggplot2); library(scales); library(RColorBrewer)
setwd("~/github_projects/economic_data/")
source("00_functions.R")

# 01. Pull data from FRED API
#----------------------------------------------------------
source("01_pull_mung_data.R")

#----------------------------------------------------------
# 02. PLOT: non housing values
#----------------------------------------------------------
# quarterly rate of change YOY
dt_range <- range(dat[!is.na(quarterly_yoy), date])
dts <- sort(seq.Date(dt_range[2], as.Date("2003-01-01"), "-1 year"))
dts2 <- sort(seq.Date(dt_range[2], as.Date("2015-01-01"), "-1 quarter"))
y_lims <- range(dat[,quarterly_yoy], na.rm= TRUE)

ggplot_series(dat, date_min= NULL, date_seq= dts, use_index= FALSE, 
              y_range= c(max(y_lims[1], 0.85), min(y_lims[2], 1.2)),
              facet= "wrap") +
  labs(x= "Date", y= "Rate of Change", title= "Quarterly YoY growth in Key Economic Variables")

ggplot_series(dat, date_min= as.Date('2015-01-01'), date_seq= dts2, use_index= FALSE, 
              y_range= c(max(y_lims[1], 0.95), min(y_lims[2], 1.1)),
              facet= "wrap") +
  labs(x= "Date", y= "Rate of Change", title= "Quarterly YoY growth in Key Economic Variables")

# indexed values
y_lims <- range(dat[,index], na.rm= TRUE)
ggplot_series(dat, date_min= NULL, date_seq= dts, use_index= TRUE, 
              y_range= c(max(y_lims[1], 0.85), min(y_lims[2], 1.2)),
              facet= "wrap") +
  labs(x= "Date", y= "Index (2015-01-01 == 1.00)", 
       title= "Indexed values of Key Economic Variables")

ggplot_series(dat, date_min= as.Date('2015-01-01'), date_seq= dts2, use_index= TRUE, 
              y_range= c(max(y_lims[1], 0.95), min(y_lims[2], 1.2)),
              facet= "wrap") +
  labs(x= "Date", y= "Index (2015-01-01 == 1.00)", 
       title= "Indexed values of Key Economic Variables")

#----------------------------------------------------------
# 03. PLOT: housing values
#----------------------------------------------------------
# quarterly rate of change YOY
dt_range <- range(house_dat[!is.na(quarterly_yoy), date])
dts <- sort(seq.Date(dt_range[2], as.Date("2003-01-01"), "-1 year"))
dts2 <- sort(seq.Date(dt_range[2], as.Date("2015-01-01"), "-1 quarter"))
y_lims <- range(house_dat[,quarterly_yoy], na.rm= TRUE)

ggplot_series(house_dat, date_min= NULL, date_seq= dts, use_index= FALSE, 
              y_range= c(max(y_lims[1], 0.6), min(y_lims[2], 1.4)),
              facet= "grid") +
  labs(x= "Date", y= "Rate of Change", title= "Quarterly YoY growth in Key Housing Data")

ggplot_series(house_dat, date_min= as.Date('2015-01-01'), date_seq= dts2, use_index= FALSE, 
              y_range= c(max(y_lims[1], 0.85), min(y_lims[2], 1.25)),
              facet= "grid") +
  labs(x= "Date", y= "Rate of Change", title= "Quarterly YoY growth in Key Housing Data")

# indexed values
y_lims <- range(house_dat[,index], na.rm= TRUE)
ggplot_series(house_dat, date_min= NULL, date_seq= dts, use_index= TRUE, 
              y_range= c(max(y_lims[1], 0.6), min(y_lims[2], 1.3)),
              facet= "grid") +
  labs(x= "Date", y= "Index (2015-01-01 == 1.00)", 
       title= "Indexed values of Key Housing Data")

ggplot_series(house_dat, date_min= as.Date('2015-01-01'), date_seq= dts2, use_index= TRUE, 
              y_range= c(max(y_lims[1], 0.8), min(y_lims[2], 1.3)),
              facet= "grid") +
  labs(x= "Date", y= "Index (2015-01-01 == 1.00)", 
       title= "Indexed values of Key Housing Data")

#----------------------------------------------------------
# 03. PLOT: vehicle data
#----------------------------------------------------------
# quarterly rate of change YOY
dt_range <- range(vehicle_dat[!is.na(quarterly_yoy), date])
dts2 <- sort(seq.Date(dt_range[2], as.Date("2015-01-01"), "-1 quarter"))
y_lims <- range(vehicle_dat[,quarterly_yoy], na.rm= TRUE)

ggplot_series(vehicle_dat, date_min= as.Date('2015-01-01'), date_seq= dts2, use_index= FALSE, 
              y_range= c(max(y_lims[1], 0.75), min(y_lims[2], 1.4)),
              facet= "grid") +
  labs(x= "Date", y= "Rate of Change", title= "Quarterly YoY growth in Vehicle Sales")

# indexed values
y_lims <- range(house_dat[,index], na.rm= TRUE)
ggplot_series(vehicle_dat, date_min= as.Date('2015-01-01'), date_seq= dts2, use_index= TRUE, 
              y_range= c(max(y_lims[1], 0.6), min(y_lims[2], 1.4)),
              facet= "grid") +
  labs(x= "Date", y= "Index (2015-01-01 == 1.00)", 
       title= "Indexed values of Vehicle Sales")
