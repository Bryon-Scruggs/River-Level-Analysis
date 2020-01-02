library(ggplot2)
library(readr)
library(magrittr)
library(dplyr)
library(lubridate)
library(data.table)

setwd('C:\\Users\\bryon\\OneDrive\\data\\usgs_gauges')

options(scipen=999) #supress scientific notation

skypath <- paste('https://waterdata.usgs.gov/nwis/uv?cb_00060=on&cb_00065=on&format=rdb&site_no=12134500&period=&begin_date=2019-11-01&end_date=2019-12-28')

sky <- fread(skypath, header = TRUE)

sky <- sky %>% rename('time_stamp' = 'datetime', 'cfs' = '151130_00060') %>% 
  select(time_stamp, agency_cd, site_no, tz_cd, cfs)

#Read from local text file. 
  # sky <- fread('12134500_SkyGoldbar.txt'
  #              ,select = c('site_no', 'datetime', '151130_00060')
  #              ,sep = '\t', skip = 31
  #              ,colClasses = 'character'
  #              ) %>% 
  #   rename('time_stamp' = 'datetime', 'cfs' = '151130_00060') %>% 
  #   select(time_stamp,everything())

#drop header row
  #site_no       time_stamp cfs
  #1:      15s              20d 14n
sky <- sky[-(cfs=='14n')]

#remove duplicates
sky <- sky[!duplicated(sky$time_stamp),]

#coerce data types
sky$cfs <- parse_number(sky$cfs)
sky$time_stamp <- parse_datetime(sky$time_stamp) #using readr::parse_date for performance issues. 


#calculate gaps between time_stamps, change in CFS and a row ID
sky$lag_mins <- difftime(sky$time_stamp,lag(sky$time_stamp,1),units = 'mins')
sky$lag_hours <- difftime(sky$time_stamp,lag(sky$time_stamp,1),units = 'hours')
sky$lag_days <- difftime(sky$time_stamp,lag(sky$time_stamp,1),units = 'days')

sky$delta_cfs <- sky$cfs - lag(sky$cfs,1)


#create a data.table of timestamps at 15 minute intervals. Start time is sky$min date, end time is sky$max date
date_time_vector <- seq.POSIXt(min(sky$time_stamp,na.rm = 1), max(sky$time_stamp,na.rm = 1), by = "15 min") %>% as.data.table() %>% 
  rename('time_stamp' = 'x') %>% setkey(time_stamp)

#left join date_time_vector with sky. This creates NA values wherever there are gaps in the dataset
sky <- sky[date_time_vector, on='time_stamp']

#create a column of true/valse values if cfs is missing. 
sky$cfs_missing <- is.na(sky$cfs)

#create a vector of unique missing days
days_missing <- round(as.double(sort(unique(sky$lag_days), decreasing = TRUE)),2)

#create a vector of unique missing hours
hours_missing <- round(as.double(sort(unique(sky$lag_hours), decreasing = TRUE)),2)

