# Flux Data 
rm(list=ls())

source("~/Dropbox (YSE)/Research/Flux Towers/TSPH7/Flux Functions.R")
# Import the data:
library(gtools)
library(beepr)

######################## Importing Flux Data #####################################

# 2016 Data:
setwd('/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/EDDYPRO_DATA_TOWERS/T7_FLUX_EDDY_PRO_DATA/2016_DATA/DECEMBER_2016_DATA')
files <- list.files(pattern ="full") # Creates a list of all .dat files in the directory
ts7.flux.2016 <- ts7.flux.import(files)
ts7.flux.2016$TIMESTAMP <- as.POSIXct(paste(ts7.flux.2016$date, ts7.flux.2016$time, sep=" "), tz = "EST", format= "%y/%m/%d %H:%M")
attr(ts7.flux.2016$TIMESTAMP, 'tzone') = 'EST' # There was an issue with the time

save(ts7.flux.2016, file="/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/Processed Data (Sparkle)/TS7_flux_annual_raw.RDATA")
append.Rda(ts7.flux.2016 , "/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/Processed Data (Sparkle)/TS7_flux_annual_raw.RDATA")#rm(ts7.flux.2016 )

# 2017 Data:
setwd('/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/EDDYPRO_DATA_TOWERS/T7_FLUX_EDDY_PRO_DATA/2017_DATA/JANUARY_2017_ALL_4-22-17')
files <- list.files(pattern="full") # Creates a list of all .dat files in the directory
ts7.flux.2017.1 <- ts7.flux.import(files)
ts7.flux.2017.1$TIMESTAMP <- as.POSIXct(paste(ts7.flux.2017.1$date, ts7.flux.2017.1$time, sep=" "), tz = "EST")
attr(ts7.flux.2017.1$TIMESTAMP, 'tzone') = 'EST' # There was an issue with the time


setwd('/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/EDDYPRO_DATA_TOWERS/T7_FLUX_EDDY_PRO_DATA/2017_DATA/FEBRUARY_2017_DATA')
files <- list.files(pattern="full") # Creates a list of all .dat files in the directory
ts7.flux.2017.2 <- ts7.flux.import(files)
ts7.flux.2017.2$TIMESTAMP <- as.POSIXct(paste(ts7.flux.2017.2$date, ts7.flux.2017.2$time, sep=" "), tz = "EST", format="%Y-%m-%d %H:%M")

setwd('/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/EDDYPRO_DATA_TOWERS/T7_FLUX_EDDY_PRO_DATA/2017_DATA/MARCH_2017_DATA')
files <- list.files(pattern="full") # Creates a list of all .dat files in the directory
ts7.flux.2017.3 <- ts7.flux.import(files)
ts7.flux.2017.3$TIMESTAMP <- as.POSIXct(paste(ts7.flux.2017.3$date, ts7.flux.2017.3$time, sep=" "), tz = "EST", format="%Y-%m-%d %H:%M")

setwd('/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/EDDYPRO_DATA_TOWERS/T7_FLUX_EDDY_PRO_DATA/2017_DATA/APRIL_2017_DATA')
files <- list.files(pattern="full") # Creates a list of all .dat files in the directory
ts7.flux.2017.4 <- ts7.flux.import(files)
ts7.flux.2017.4$TIMESTAMP <- as.POSIXct(paste(ts7.flux.2017.4$date, ts7.flux.2017.4$time, sep=" "), tz = "EST", format="%Y-%m-%d %H:%M")

setwd('/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/EDDYPRO_DATA_TOWERS/T7_FLUX_EDDY_PRO_DATA/2017_DATA/JUNE_2017_DATA')
files <- list.files(pattern="full") # Creates a list of all .dat files in the directory
ts7.flux.2017.6 <- ts7.flux.import(files)
ts7.flux.2017.6$TIMESTAMP <- as.POSIXct(paste(ts7.flux.2017.6$date, ts7.flux.2017.6$time, sep=" "), tz = "EST", format="%Y-%m-%d %H:%M")

setwd('/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/EDDYPRO_DATA_TOWERS/T7_FLUX_EDDY_PRO_DATA/2017_DATA/JULY_2017_DATA')
files <- list.files(pattern="full") # Creates a list of all .dat files in the directory
ts7.flux.2017.7 <- ts7.flux.import(files)
ts7.flux.2017.7$TIMESTAMP <- as.POSIXct(paste(ts7.flux.2017.7$date, ts7.flux.2017.7$time, sep=" "), tz = "EST", format="%Y-%m-%d %H:%M")

setwd('/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/EDDYPRO_DATA_TOWERS/T7_FLUX_EDDY_PRO_DATA/2017_DATA/AUGUST_2017_DATA')
files <- list.files(pattern ="full") # Creates a list of all .dat files in the directory
ts7.flux.2017.8 <- ts7.flux.import(files)
ts7.flux.2017.8$TIMESTAMP <- as.POSIXct(paste(ts7.flux.2017.8$date, ts7.flux.2017.8$time, sep=" "), tz = "EST", format="%Y-%m-%d %H:%M")

setwd('/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/EDDYPRO_DATA_TOWERS/T7_FLUX_EDDY_PRO_DATA/2017_DATA/SEPTEMBER_2017_DATA')
files <- list.files(pattern ="full") # Creates a list of all .dat files in the directory
ts7.flux.2017.9 <- ts7.flux.import(files)
ts7.flux.2017.9$TIMESTAMP <- as.POSIXct(paste(ts7.flux.2017.9$date, ts7.flux.2017.9$time, sep=" "), tz = "EST", format="%Y-%m-%d %H:%M")

setwd('/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/EDDYPRO_DATA_TOWERS/T7_FLUX_EDDY_PRO_DATA/2017_DATA/OCTOBER_2017_DATA')
files <- list.files(pattern ="full") # Creates a list of all .dat files in the directory
ts7.flux.2017.10 <- ts7.flux.import(files)
ts7.flux.2017.10$TIMESTAMP <- as.POSIXct(paste(ts7.flux.2017.10$date, ts7.flux.2017.10$time, sep=" "), tz = "EST", format="%Y-%m-%d %H:%M")

setwd('/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/EDDYPRO_DATA_TOWERS/T7_FLUX_EDDY_PRO_DATA/2017_DATA/NOVEMBER_2017_DATA')
files <- list.files(pattern ="full") # Creates a list of all .dat files in the directory
ts7.flux.2017.11 <- ts7.flux.import(files)
ts7.flux.2017.11$TIMESTAMP <- as.POSIXct(paste(ts7.flux.2017.11$date, ts7.flux.2017.11$time, sep=" "), tz = "EST", format="%Y-%m-%d %H:%M")

setwd('/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/EDDYPRO_DATA_TOWERS/T7_FLUX_EDDY_PRO_DATA/2017_DATA/DECEMBER_2017_DATA')
files <- list.files(pattern ="full") # Creates a list of all .dat files in the directory
ts7.flux.2017.12 <- ts7.flux.import(files)
ts7.flux.2017.12$TIMESTAMP <- as.POSIXct(paste(ts7.flux.2017.12$date, ts7.flux.2017.12$time, sep=" "), tz = "EST", format="%Y-%m-%d %H:%M")

ts7.flux.2017 <- smartbind( ts7.flux.2017.1, ts7.flux.2017.2, ts7.flux.2017.3, ts7.flux.2017.4, ts7.flux.2017.6,
                             ts7.flux.2017.7, ts7.flux.2017.8, ts7.flux.2017.9, ts7.flux.2017.10, ts7.flux.2017.11, ts7.flux.2017.12)

load("/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/Processed Data (Sparkle)/TS7_flux_annual_raw.RDATA" )
append.Rda(ts7.flux.2017 , "/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/Processed Data (Sparkle)/TS7_flux_annual_raw.RDATA")

rm( ts7.flux.2017.1, ts7.flux.2017.2, ts7.flux.2017.3, ts7.flux.2017.4, ts7.flux.2017.6,
    ts7.flux.2017.7, ts7.flux.2017.8, ts7.flux.2017.9, ts7.flux.2017.10, ts7.flux.2017.11, ts7.flux.2017.12)


# 2018
setwd('/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/EDDYPRO_DATA_TOWERS/T7_FLUX_EDDY_PRO_DATA/2018_DATA/JANUARY_2018_DATA')
files <- list.files(pattern ="full") # Creates a list of all .dat files in the directory
ts7.flux.2018.1 <- ts7.flux.import(files)
ts7.flux.2018.1$TIMESTAMP <- as.POSIXct(paste(ts7.flux.2018.1$date, ts7.flux.2018.1$time, sep=" "), tz = "EST")
attr(ts7.flux.2018.1$TIMESTAMP, 'tzone') = 'EST' # There was an issue with the time

setwd('/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/EDDYPRO_DATA_TOWERS/T7_FLUX_EDDY_PRO_DATA/2018_DATA/FEBRUARY_2018_DATA')
files <- list.files(pattern ="full") # Creates a list of all .dat files in the directory
ts7.flux.2018.2 <- ts7.flux.import(files)
ts7.flux.2018.2$TIMESTAMP <- as.POSIXct(paste(ts7.flux.2018.2$date, ts7.flux.2018.2$time, sep=" "), tz = "EST", format="%Y-%m-%d %H:%M")

setwd('/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/EDDYPRO_DATA_TOWERS/T7_FLUX_EDDY_PRO_DATA/2018_DATA/MARCH_2018_DATA')
files <- list.files(pattern ="full") # Creates a list of all .dat files in the directory
ts7.flux.2018.3 <- ts7.flux.import(files)
ts7.flux.2018.3$TIMESTAMP <- as.POSIXct(paste(ts7.flux.2018.3$date, ts7.flux.2018.3$time, sep=" "), tz = "EST", format="%Y-%m-%d %H:%M")

setwd('/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/EDDYPRO_DATA_TOWERS/T7_FLUX_EDDY_PRO_DATA/2018_DATA/APRIL_2018_DATA')
files <- list.files(pattern ="full") # Creates a list of all .dat files in the directory
ts7.flux.2018.4 <- ts7.flux.import(files)
ts7.flux.2018.4$TIMESTAMP <- as.POSIXct(paste(ts7.flux.2018.4$date, ts7.flux.2018.4$time, sep=" "), tz = "EST", format="%Y-%m-%d %H:%M")

setwd('/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/EDDYPRO_DATA_TOWERS/T7_FLUX_EDDY_PRO_DATA/2018_DATA/MAY_2018_DATA')
files <- list.files(pattern ="full") # Creates a list of all .dat files in the directory
ts7.flux.2018.5 <- ts7.flux.import(files)
ts7.flux.2018.5$TIMESTAMP <- as.POSIXct(paste(ts7.flux.2018.5$date, ts7.flux.2018.5$time, sep=" "), tz = "EST", format="%Y-%m-%d %H:%M")

setwd('/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/EDDYPRO_DATA_TOWERS/T7_FLUX_EDDY_PRO_DATA/2018_DATA/JUNE_2018_DATA')
files <- list.files(pattern ="full") # Creates a list of all .dat files in the directory
ts7.flux.2018.6 <- ts7.flux.import(files)
ts7.flux.2018.6$TIMESTAMP <- as.POSIXct(paste(ts7.flux.2018.6$date, ts7.flux.2018.6$time, sep=" "), tz = "EST", format="%Y-%m-%d %H:%M")

setwd('/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/EDDYPRO_DATA_TOWERS/T7_FLUX_EDDY_PRO_DATA/2018_DATA/JULY_2018_DATA')
files <- list.files(pattern ="full") # Creates a list of all .dat files in the directory
ts7.flux.2018.7 <- ts7.flux.import(files)
ts7.flux.2018.7$TIMESTAMP <- as.POSIXct(paste(ts7.flux.2018.7$date, ts7.flux.2018.7$time, sep=" "), tz = "EST", format="%Y-%m-%d %H:%M")

setwd('/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/EDDYPRO_DATA_TOWERS/T7_FLUX_EDDY_PRO_DATA/2018_DATA/AUGUST_2018_DATA')
files <- list.files(pattern ="full") # Creates a list of all .dat files in the directory
ts7.flux.2018.8 <- ts7.flux.import(files)
ts7.flux.2018.8$TIMESTAMP <- as.POSIXct(paste(ts7.flux.2018.8$date, ts7.flux.2018.8$time, sep=" "), tz = "EST", format="%Y-%m-%d %H:%M")

setwd('/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/EDDYPRO_DATA_TOWERS/T7_FLUX_EDDY_PRO_DATA/2018_DATA/SEPTEMBER_2018_DATA')
files <- list.files(pattern ="full") # Creates a list of all .dat files in the directory
ts7.flux.2018.9 <- ts7.flux.import(files)
ts7.flux.2018.9$TIMESTAMP <- as.POSIXct(paste(ts7.flux.2018.9$date, ts7.flux.2018.9$time, sep=" "), tz = "EST", format="%Y-%m-%d %H:%M")

setwd('/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/EDDYPRO_DATA_TOWERS/T7_FLUX_EDDY_PRO_DATA/2018_DATA/OCTOBER_2018_DATA')
files <- list.files(pattern ="full") # Creates a list of all .dat files in the directory
ts7.flux.2018.10 <- ts7.flux.import(files)
ts7.flux.2018.10$TIMESTAMP <- as.POSIXct(paste(ts7.flux.2018.10$date, ts7.flux.2018.10$time, sep=" "), tz = "EST", format="%Y-%m-%d %H:%M")

setwd('/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/EDDYPRO_DATA_TOWERS/T7_FLUX_EDDY_PRO_DATA/2018_DATA/NOVEMBER_2018_DATA')
files <- list.files(pattern ="full") # Creates a list of all .dat files in the directory
ts7.flux.2018.11 <- ts7.flux.import(files)
ts7.flux.2018.11$TIMESTAMP <- as.POSIXct(paste(ts7.flux.2018.11$date, ts7.flux.2018.11$time, sep=" "), tz = "EST", format="%Y-%m-%d %H:%M")

setwd('/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/EDDYPRO_DATA_TOWERS/T7_FLUX_EDDY_PRO_DATA/2018_DATA/DECEMBER_2018_DATA')
files <- list.files(pattern ="full") # Creates a list of all .dat files in the directory
ts7.flux.2018.12 <- ts7.flux.import(files)
ts7.flux.2018.12$TIMESTAMP <- as.POSIXct(paste(ts7.flux.2018.12$date, ts7.flux.2018.12$time, sep=" "), tz = "EST", format="%Y-%m-%d %H:%M")

ts7.flux.2018 <- smartbind( ts7.flux.2018.1, ts7.flux.2018.2, ts7.flux.2018.3, ts7.flux.2018.4, ts7.flux.2018.5, ts7.flux.2018.6,
                             ts7.flux.2018.7, ts7.flux.2018.8, ts7.flux.2018.9, ts7.flux.2018.10, ts7.flux.2018.11, ts7.flux.2018.12)
append.Rda(ts7.flux.2018 , "/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/Processed Data (Sparkle)/TS7_flux_annual_raw.RDATA")

rm( ts7.flux.2018.1, ts7.flux.2018.2, ts7.flux.2018.3, ts7.flux.2018.4, ts7.flux.2018.5, ts7.flux.2018.6,
    ts7.flux.2018.7, ts7.flux.2018.8, ts7.flux.2018.9, ts7.flux.2018.10, ts7.flux.2018.11, ts7.flux.2018.12)

# 2019
setwd('/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/EDDYPRO_DATA_TOWERS/T7_FLUX_EDDY_PRO_DATA/2019_DATA/JANUARY_2019_DATA')
files <- list.files(pattern ="full") # Creates a list of all .dat files in the directory
ts7.flux.2019.1 <- ts7.flux.import(files)
ts7.flux.2019.1$TIMESTAMP <- as.POSIXct(paste(ts7.flux.2019.1$date, ts7.flux.2019.1$time, sep=" "), tz = "EST")
attr(ts7.flux.2019.1$TIMESTAMP, 'tzone') = 'EST' # There was an issue with the time

setwd('/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/EDDYPRO_DATA_TOWERS/T7_FLUX_EDDY_PRO_DATA/2019_DATA/FEBRUARY_2019_DATA')
files <- list.files(pattern ="full") # Creates a list of all .dat files in the directory
ts7.flux.2019.2 <- ts7.flux.import(files)
ts7.flux.2019.2$TIMESTAMP <- as.POSIXct(paste(ts7.flux.2019.2$date, ts7.flux.2019.2$time, sep=" "), tz = "EST", format="%Y-%m-%d %H:%M")

setwd('/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/EDDYPRO_DATA_TOWERS/T7_FLUX_EDDY_PRO_DATA/2019_DATA/MARCH_2019_DATA')
files <- list.files(pattern ="full") # Creates a list of all .dat files in the directory
ts7.flux.2019.3 <- ts7.flux.import(files)
ts7.flux.2019.3$TIMESTAMP <- as.POSIXct(paste(ts7.flux.2019.3$date, ts7.flux.2019.3$time, sep=" "), tz = "EST", format="%Y-%m-%d %H:%M")

setwd('/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/EDDYPRO_DATA_TOWERS/T7_FLUX_EDDY_PRO_DATA/2019_DATA/APRIL_2019_DATA')
files <- list.files(pattern ="full") # Creates a list of all .dat files in the directory
ts7.flux.2019.4 <- ts7.flux.import(files)
ts7.flux.2019.4$TIMESTAMP <- as.POSIXct(paste(ts7.flux.2019.4$date, ts7.flux.2019.4$time, sep=" "), tz = "EST", format="%Y-%m-%d %H:%M")

setwd('/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/EDDYPRO_DATA_TOWERS/T7_FLUX_EDDY_PRO_DATA/2019_DATA/MAY_2019_DATA')
files <- list.files(pattern ="full") # Creates a list of all .dat files in the directory
ts7.flux.2019.5 <- ts7.flux.import(files)
ts7.flux.2019.5$TIMESTAMP <- as.POSIXct(paste(ts7.flux.2019.5$date, ts7.flux.2019.5$time, sep=" "), tz = "EST", format="%Y-%m-%d %H:%M")

setwd('/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/EDDYPRO_DATA_TOWERS/T7_FLUX_EDDY_PRO_DATA/2019_DATA/JUNE_2019_DATA')
files <- list.files(pattern ="full") # Creates a list of all .dat files in the directory
ts7.flux.2019.6 <- ts7.flux.import(files)
ts7.flux.2019.6$TIMESTAMP <- as.POSIXct(paste(ts7.flux.2019.6$date, ts7.flux.2019.6$time, sep=" "), tz = "EST", format="%Y-%m-%d %H:%M")

setwd('/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/EDDYPRO_DATA_TOWERS/T7_FLUX_EDDY_PRO_DATA/2019_DATA/JULY_2019_DATA')
files <- list.files(pattern ="full") # Creates a list of all .dat files in the directory
ts7.flux.2019.7 <- ts7.flux.import(files)
ts7.flux.2019.7$TIMESTAMP <- as.POSIXct(paste(ts7.flux.2019.7$date, ts7.flux.2019.7$time, sep=" "), tz = "EST", format="%Y-%m-%d %H:%M")

setwd('/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/EDDYPRO_DATA_TOWERS/T7_FLUX_EDDY_PRO_DATA/2019_DATA/AUGUST_2019_DATA')
files <- list.files(pattern ="full") # Creates a list of all .dat files in the directory
ts7.flux.2019.8 <- ts7.flux.import(files)
ts7.flux.2019.8$TIMESTAMP <- as.POSIXct(paste(ts7.flux.2019.8$date, ts7.flux.2019.8$time, sep=" "), tz = "EST", format="%Y-%m-%d %H:%M")

setwd('/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/EDDYPRO_DATA_TOWERS/T7_FLUX_EDDY_PRO_DATA/2019_DATA/SEPTEMBER_2019_DATA')
files <- list.files(pattern ="full") # Creates a list of all .dat files in the directory
ts7.flux.2019.9 <- ts7.flux.import(files)
ts7.flux.2019.9$TIMESTAMP <- as.POSIXct(paste(ts7.flux.2019.9$date, ts7.flux.2019.9$time, sep=" "), tz = "EST", format="%Y-%m-%d %H:%M")

setwd('/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/EDDYPRO_DATA_TOWERS/T7_FLUX_EDDY_PRO_DATA/2019_DATA/OCTOBER_2019_DATA')
files <- list.files(pattern ="full") # Creates a list of all .dat files in the directory
ts7.flux.2019.10 <- ts7.flux.import(files)
ts7.flux.2019.10$TIMESTAMP <- as.POSIXct(paste(ts7.flux.2019.10$date, ts7.flux.2019.10$time, sep=" "), tz = "EST", format="%Y-%m-%d %H:%M")

setwd('/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/EDDYPRO_DATA_TOWERS/T7_FLUX_EDDY_PRO_DATA/2019_DATA/NOVEMBER_2019_DATA')
files <- list.files(pattern ="full") # Creates a list of all .dat files in the directory
ts7.flux.2019.11 <- ts7.flux.import(files)
ts7.flux.2019.11$TIMESTAMP <- as.POSIXct(paste(ts7.flux.2019.11$date, ts7.flux.2019.11$time, sep=" "), tz = "EST", format="%Y-%m-%d %H:%M")

setwd('/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/EDDYPRO_DATA_TOWERS/T7_FLUX_EDDY_PRO_DATA/2019_DATA/DECEMBER_2019_DATA')
files <- list.files(pattern ="full") # Creates a list of all .dat files in the directory
ts7.flux.2019.12 <- ts7.flux.import(files)
ts7.flux.2019.12$TIMESTAMP <- as.POSIXct(paste(ts7.flux.2019.12$date, ts7.flux.2019.12$time, sep=" "), tz = "EST", format="%Y-%m-%d %H:%M")

ts7.flux.2019 <- smartbind( ts7.flux.2019.1, ts7.flux.2019.2, ts7.flux.2019.3, ts7.flux.2019.4, ts7.flux.2019.5,ts7.flux.2019.6,
                             ts7.flux.2019.7, ts7.flux.2019.8, ts7.flux.2019.9, ts7.flux.2019.10, ts7.flux.2019.11, ts7.flux.2019.12)
append.Rda(ts7.flux.2019 , "/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/Processed Data (Sparkle)/TS7_flux_annual_raw.RDATA")

rm( ts7.flux.2019.1, ts7.flux.2019.2, ts7.flux.2019.3, ts7.flux.2019.4, ts7.flux.2019.6,ts7.flux.2019.5,
    ts7.flux.2019.7, ts7.flux.2019.8, ts7.flux.2019.9, ts7.flux.2019.10, ts7.flux.2019.11, ts7.flux.2019.12)

# 2020
setwd('/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/EDDYPRO_DATA_TOWERS/T7_FLUX_EDDY_PRO_DATA/2020_DATA/JANUARY_2020_DATA')
files <- list.files(pattern ="full") # Creates a list of all .dat files in the directory
ts7.flux.2020.1 <- ts7.flux.import(files)
ts7.flux.2020.1$TIMESTAMP <- as.POSIXct(paste(ts7.flux.2020.1$date, ts7.flux.2020.1$time, sep=" "), tz = "EST")
attr(ts7.flux.2020.1$TIMESTAMP, 'tzone') = 'EST' # There was an issue with the time

setwd('/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/EDDYPRO_DATA_TOWERS/T7_FLUX_EDDY_PRO_DATA/2020_DATA/FEBRUARY_2020_DATA')
files <- list.files(pattern ="full") # Creates a list of all .dat files in the directory
ts7.flux.2020.2 <- ts7.flux.import(files)
ts7.flux.2020.2$TIMESTAMP <- as.POSIXct(paste(ts7.flux.2020.2$date, ts7.flux.2020.2$time, sep=" "), tz = "EST", format="%Y-%m-%d %H:%M")

setwd('/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/EDDYPRO_DATA_TOWERS/T7_FLUX_EDDY_PRO_DATA/2020_DATA/MARCH_2020_DATA')
files <- list.files(pattern ="full") # Creates a list of all .dat files in the directory
ts7.flux.2020.3 <- ts7.flux.import(files)
ts7.flux.2020.3$TIMESTAMP <- as.POSIXct(paste(ts7.flux.2020.3$date, ts7.flux.2020.3$time, sep=" "), tz = "EST", format="%Y-%m-%d %H:%M")

setwd('/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/EDDYPRO_DATA_TOWERS/T7_FLUX_EDDY_PRO_DATA/2020_DATA/APRIL_2020_DATA')
files <- list.files(pattern ="full") # Creates a list of all .dat files in the directory
ts7.flux.2020.4 <- ts7.flux.import(files)
ts7.flux.2020.4$TIMESTAMP <- as.POSIXct(paste(ts7.flux.2020.4$date, ts7.flux.2020.4$time, sep=" "), tz = "EST", format="%Y-%m-%d %H:%M")

setwd('/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/EDDYPRO_DATA_TOWERS/T7_FLUX_EDDY_PRO_DATA/2020_DATA/MAY_2020_DATA')
files <- list.files(pattern ="full") # Creates a list of all .dat files in the directory
ts7.flux.2020.5 <- ts7.flux.import(files)
ts7.flux.2020.5$TIMESTAMP <- as.POSIXct(paste(ts7.flux.2020.5$date, ts7.flux.2020.5$time, sep=" "), tz = "EST", format="%Y-%m-%d %H:%M")

setwd('/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/EDDYPRO_DATA_TOWERS/T7_FLUX_EDDY_PRO_DATA/2020_DATA/JUNE_2020_DATA')
files <- list.files(pattern ="full") # Creates a list of all .dat files in the directory
ts7.flux.2020.6 <- ts7.flux.import(files)
ts7.flux.2020.6$TIMESTAMP <- as.POSIXct(paste(ts7.flux.2020.6$date, ts7.flux.2020.6$time, sep=" "), tz = "EST", format="%Y-%m-%d %H:%M")

setwd('/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/EDDYPRO_DATA_TOWERS/T7_FLUX_EDDY_PRO_DATA/2020_DATA/JULY_2020_DATA')
files <- list.files(pattern ="full") # Creates a list of all .dat files in the directory
ts7.flux.2020.7 <- ts7.flux.import(files)
ts7.flux.2020.7$TIMESTAMP <- as.POSIXct(paste(ts7.flux.2020.7$date, ts7.flux.2020.7$time, sep=" "), tz = "EST", format="%Y-%m-%d %H:%M")

setwd('/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/EDDYPRO_DATA_TOWERS/T7_FLUX_EDDY_PRO_DATA/2020_DATA/AUGUST_2020_DATA')
files <- list.files(pattern ="full") # Creates a list of all .dat files in the directory
ts7.flux.2020.8 <- ts7.flux.import(files)
ts7.flux.2020.8$TIMESTAMP <- as.POSIXct(paste(ts7.flux.2020.8$date, ts7.flux.2020.8$time, sep=" "), tz = "EST", format="%Y-%m-%d %H:%M")

setwd('/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/EDDYPRO_DATA_TOWERS/T7_FLUX_EDDY_PRO_DATA/2020_DATA/SEPTEMBER_2020_DATA')
files <- list.files(pattern ="full") # Creates a list of all .dat files in the directory
ts7.flux.2020.9 <- ts7.flux.import(files)
ts7.flux.2020.9$TIMESTAMP <- as.POSIXct(paste(ts7.flux.2020.9$date, ts7.flux.2020.9$time, sep=" "), tz = "EST", format="%Y-%m-%d %H:%M")

setwd('/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/EDDYPRO_DATA_TOWERS/T7_FLUX_EDDY_PRO_DATA/2020_DATA/OCTOBER_2020_DATA')
files <- list.files(pattern ="full") # Creates a list of all .dat files in the directory
ts7.flux.2020.10 <- ts7.flux.import(files)
ts7.flux.2020.10$TIMESTAMP <- as.POSIXct(paste(ts7.flux.2020.10$date, ts7.flux.2020.10$time, sep=" "), tz = "EST", format="%Y-%m-%d %H:%M")

setwd('/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/EDDYPRO_DATA_TOWERS/T7_FLUX_EDDY_PRO_DATA/2020_DATA/NOVEMBER_2020_DATA')
files <- list.files(pattern ="full") # Creates a list of all .dat files in the directory
ts7.flux.2020.11 <- ts7.flux.import(files)
ts7.flux.2020.11$TIMESTAMP <- as.POSIXct(paste(ts7.flux.2020.11$date, ts7.flux.2020.11$time, sep=" "), tz = "EST", format="%Y-%m-%d %H:%M")

setwd('/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/EDDYPRO_DATA_TOWERS/T7_FLUX_EDDY_PRO_DATA/2020_DATA/DECEMBER_2020_DATA')
files <- list.files(pattern ="full") # Creates a list of all .dat files in the directory
ts7.flux.2020.12 <- ts7.flux.import(files)
ts7.flux.2020.12$TIMESTAMP <- as.POSIXct(paste(ts7.flux.2020.12$date, ts7.flux.2020.12$time, sep=" "), tz = "EST", format="%Y-%m-%d %H:%M")

ts7.flux.2020 <- smartbind( ts7.flux.2020.1, ts7.flux.2020.2, ts7.flux.2020.3, ts7.flux.2020.4, ts7.flux.2020.5,ts7.flux.2020.6,
                             ts7.flux.2020.7, ts7.flux.2020.8, ts7.flux.2020.9, ts7.flux.2020.10, ts7.flux.2020.11, ts7.flux.2020.12)
append.Rda(ts7.flux.2020 , "/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/Processed Data (Sparkle)/TS7_flux_annual_raw.RDATA")

rm( ts7.flux.2020.1, ts7.flux.2020.2, ts7.flux.2020.3, ts7.flux.2020.4, ts7.flux.2020.6,ts7.flux.2020.5,
    ts7.flux.2020.7, ts7.flux.2020.8, ts7.flux.2020.9, ts7.flux.2020.10, ts7.flux.2020.11, ts7.flux.2020.12)


#2021

setwd('/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/EDDYPRO_DATA_TOWERS/T7_FLUX_EDDY_PRO_DATA/2021_DATA/JANUARY_2021_DATA')
files <- list.files(pattern ="full") # Creates a list of all .dat files in the directory
ts7.flux.2021.1 <- ts7.flux.import(files)
ts7.flux.2021.1$TIMESTAMP <- as.POSIXct(paste(ts7.flux.2021.1$date, ts7.flux.2021.1$time, sep=" "), tz = "EST")
attr(ts7.flux.2021.1$TIMESTAMP, 'tzone') = 'EST' # There was an issue with the time

setwd('/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/EDDYPRO_DATA_TOWERS/T7_FLUX_EDDY_PRO_DATA/2021_DATA/FEBRUARY_2021_DATA')
files <- list.files(pattern ="full") # Creates a list of all .dat files in the directory
ts7.flux.2021.2 <- ts7.flux.import(files)
ts7.flux.2021.2$TIMESTAMP <- as.POSIXct(paste(ts7.flux.2021.2$date, ts7.flux.2021.2$time, sep=" "), tz = "EST", format="%Y-%m-%d %H:%M")

setwd('/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/EDDYPRO_DATA_TOWERS/T7_FLUX_EDDY_PRO_DATA/2021_DATA/MARCH_2021_DATA')
files <- list.files(pattern ="full") # Creates a list of all .dat files in the directory
ts7.flux.2021.3 <- ts7.flux.import(files)
ts7.flux.2021.3$TIMESTAMP <- as.POSIXct(paste(ts7.flux.2021.3$date, ts7.flux.2021.3$time, sep=" "), tz = "EST", format="%Y-%m-%d %H:%M")

setwd('/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/EDDYPRO_DATA_TOWERS/T7_FLUX_EDDY_PRO_DATA/2021_DATA/APRIL_2021_DATA')
files <- list.files(pattern ="full") # Creates a list of all .dat files in the directory
ts7.flux.2021.4 <- ts7.flux.import(files)
ts7.flux.2021.4$TIMESTAMP <- as.POSIXct(paste(ts7.flux.2021.4$date, ts7.flux.2021.4$time, sep=" "), tz = "EST", format="%Y-%m-%d %H:%M")

setwd('/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/EDDYPRO_DATA_TOWERS/T7_FLUX_EDDY_PRO_DATA/2021_DATA/MAY_2021_DATA')
files <- list.files(pattern ="full") # Creates a list of all .dat files in the directory
ts7.flux.2021.5 <- ts7.flux.import(files)
ts7.flux.2021.5$TIMESTAMP <- as.POSIXct(paste(ts7.flux.2021.5$date, ts7.flux.2021.5$time, sep=" "), tz = "EST", format="%Y-%m-%d %H:%M")

setwd('/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/EDDYPRO_DATA_TOWERS/T7_FLUX_EDDY_PRO_DATA/2021_DATA/JUNE_2021_DATA')
files <- list.files(pattern ="full") # Creates a list of all .dat files in the directory
ts7.flux.2021.6 <- ts7.flux.import(files)
ts7.flux.2021.6$TIMESTAMP <- as.POSIXct(paste(ts7.flux.2021.6$date, ts7.flux.2021.6$time, sep=" "), tz = "EST", format="%Y-%m-%d %H:%M")

setwd('/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/EDDYPRO_DATA_TOWERS/T7_FLUX_EDDY_PRO_DATA/2021_DATA/JULY_2021_DATA')
files <- list.files(pattern ="full") # Creates a list of all .dat files in the directory
ts7.flux.2021.7 <- ts7.flux.import(files)
ts7.flux.2021.7$TIMESTAMP <- as.POSIXct(paste(ts7.flux.2021.7$date, ts7.flux.2021.7$time, sep=" "), tz = "EST", format="%Y-%m-%d %H:%M")

setwd('/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/EDDYPRO_DATA_TOWERS/T7_FLUX_EDDY_PRO_DATA/2021_DATA/AUGUST_2021_DATA')
files <- list.files(pattern ="full") # Creates a list of all .dat files in the directory
ts7.flux.2021.8 <- ts7.flux.import(files)
ts7.flux.2021.8$TIMESTAMP <- as.POSIXct(paste(ts7.flux.2021.8$date, ts7.flux.2021.8$time, sep=" "), tz = "EST", format="%Y-%m-%d %H:%M")

setwd('/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/EDDYPRO_DATA_TOWERS/T7_FLUX_EDDY_PRO_DATA/2021_DATA/SEPTEMBER_2021_DATA')
files <- list.files(pattern ="full") # Creates a list of all .dat files in the directory
ts7.flux.2021.9 <- ts7.flux.import(files)
ts7.flux.2021.9$TIMESTAMP <- as.POSIXct(paste(ts7.flux.2021.9$date, ts7.flux.2021.9$time, sep=" "), tz = "EST", format="%Y-%m-%d %H:%M")

setwd('/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/EDDYPRO_DATA_TOWERS/T7_FLUX_EDDY_PRO_DATA/2021_DATA/OCTOBER_2021_DATA')
files <- list.files(pattern ="full") # Creates a list of all .dat files in the directory
ts7.flux.2021.10 <- ts7.flux.import(files)
ts7.flux.2021.10$TIMESTAMP <- as.POSIXct(paste(ts7.flux.2021.10$date, ts7.flux.2021.10$time, sep=" "), tz = "EST", format="%Y-%m-%d %H:%M")

setwd('/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/EDDYPRO_DATA_TOWERS/T7_FLUX_EDDY_PRO_DATA/2021_DATA/NOVEMBER_2021_DATA')
files <- list.files(pattern ="full") # Creates a list of all .dat files in the directory
ts7.flux.2021.11 <- ts7.flux.import(files)
ts7.flux.2021.11$TIMESTAMP <- as.POSIXct(paste(ts7.flux.2021.11$date, ts7.flux.2021.11$time, sep=" "), tz = "EST", format="%Y-%m-%d %H:%M")

setwd('/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/EDDYPRO_DATA_TOWERS/T7_FLUX_EDDY_PRO_DATA/2021_DATA/DECEMBER_2021_DATA')
files <- list.files(pattern ="full") # Creates a list of all .dat files in the directory
ts7.flux.2021.12 <- ts7.flux.import(files)
ts7.flux.2021.12$TIMESTAMP <- as.POSIXct(paste(ts7.flux.2021.12$date, ts7.flux.2021.12$time, sep=" "), tz = "EST", format="%Y-%m-%d %H:%M")

ts7.flux.2021 <- smartbind( ts7.flux.2021.1, ts7.flux.2021.2, ts7.flux.2021.3, ts7.flux.2021.4, ts7.flux.2021.5,ts7.flux.2021.6,
                             ts7.flux.2021.7, ts7.flux.2021.8, ts7.flux.2021.9, ts7.flux.2021.10, ts7.flux.2021.11, ts7.flux.2021.12)

append.Rda(ts7.flux.2021 , "/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/Processed Data (Sparkle)/TS7_flux_annual_raw.RDATA")

rm( ts7.flux.2021.1, ts7.flux.2021.2, ts7.flux.2021.3, ts7.flux.2021.4, ts7.flux.2021.6,ts7.flux.2021.5,
    ts7.flux.2021.7, ts7.flux.2021.8, ts7.flux.2021.9, ts7.flux.2021.10, ts7.flux.2021.11, ts7.flux.2021.12)

#2022

setwd('/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/EDDYPRO_DATA_TOWERS/T7_FLUX_EDDY_PRO_DATA/2022_DATA/JANUARY_2022_DATA')
files <- list.files(pattern ="full") # Creates a list of all .dat files in the directory
ts7.flux.2022.1 <- ts7.flux.import(files)
ts7.flux.2022.1$TIMESTAMP <- as.POSIXct(paste(ts7.flux.2022.1$date, ts7.flux.2022.1$time, sep=" "), tz = "EST", format="%Y-%m-%d %H:%M")

setwd('/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/EDDYPRO_DATA_TOWERS/T7_FLUX_EDDY_PRO_DATA/2022_DATA/FEBRUARY_2022_DATA')
files <- list.files(pattern ="full") # Creates a list of all .dat files in the directory
ts7.flux.2022.2 <- ts7.flux.import(files)
ts7.flux.2022.2$TIMESTAMP <- as.POSIXct(paste(ts7.flux.2022.2$date, ts7.flux.2022.2$time, sep=" "), tz = "EST", format="%Y-%m-%d %H:%M")


setwd('/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/EDDYPRO_DATA_TOWERS/T7_FLUX_EDDY_PRO_DATA/2022_DATA/MARCH_2022_DATA')
files <- list.files(pattern ="full") # Creates a list of all .dat files in the directory
ts7.flux.2022.3 <- ts7.flux.import(files)
ts7.flux.2022.3$TIMESTAMP <- as.POSIXct(paste(ts7.flux.2022.3$date, ts7.flux.2022.3$time, sep=" "), tz = "EST", format="%Y-%m-%d %H:%M")

setwd('/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/EDDYPRO_DATA_TOWERS/T7_FLUX_EDDY_PRO_DATA/2022_DATA/APRIL_2022_DATA')
files <- list.files(pattern ="full") # Creates a list of all .dat files in the directory
ts7.flux.2022.4 <- ts7.flux.import(files)
ts7.flux.2022.4$TIMESTAMP <- as.POSIXct(paste(ts7.flux.2022.4$date, ts7.flux.2022.4$time, sep=" "), tz = "EST", format="%Y-%m-%d %H:%M")

setwd('/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/EDDYPRO_DATA_TOWERS/T7_FLUX_EDDY_PRO_DATA/2022_DATA/MAY_2022_DATA')
files <- list.files(pattern ="full") # Creates a list of all .dat files in the directory
ts7.flux.2022.5 <- ts7.flux.import(files)
ts7.flux.2022.5$TIMESTAMP <- as.POSIXct(paste(ts7.flux.2022.5$date, ts7.flux.2022.5$time, sep=" "), tz = "EST", format="%Y-%m-%d %H:%M")

setwd('/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/EDDYPRO_DATA_TOWERS/T7_FLUX_EDDY_PRO_DATA/2022_DATA/JUNE_2022_DATA')
files <- list.files(pattern ="full") # Creates a list of all .dat files in the directory
ts7.flux.2022.6 <- ts7.flux.import(files)
ts7.flux.2022.6$TIMESTAMP <- as.POSIXct(paste(ts7.flux.2022.6$date, ts7.flux.2022.6$time, sep=" "), tz = "EST", format="%Y-%m-%d %H:%M")

setwd('/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/EDDYPRO_DATA_TOWERS/T7_FLUX_EDDY_PRO_DATA/2022_DATA/JULY_2022_DATA')
files <- list.files(pattern ="full") # Creates a list of all .dat files in the directory
ts7.flux.2022.7 <- ts7.flux.import(files)
ts7.flux.2022.7$TIMESTAMP <- as.POSIXct(paste(ts7.flux.2022.7$date, ts7.flux.2022.7$time, sep=" "), tz = "EST", format="%Y-%m-%d %H:%M")

setwd('/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/EDDYPRO_DATA_TOWERS/T7_FLUX_EDDY_PRO_DATA/2022_DATA/AUGUST_2022_DATA')
files <- list.files(pattern ="full") # Creates a list of all .dat files in the directory
ts7.flux.2022.8 <- ts7.flux.import(files)
ts7.flux.2022.8$TIMESTAMP <- as.POSIXct(paste(ts7.flux.2022.8$date, ts7.flux.2022.8$time, sep=" "), tz = "EST", format="%Y-%m-%d %H:%M")

setwd('/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/EDDYPRO_DATA_TOWERS/T7_FLUX_EDDY_PRO_DATA/2022_DATA/SEPTEMBER_2022_DATA')
files <- list.files(pattern ="full") # Creates a list of all .dat files in the directory
ts7.flux.2022.9 <- ts7.flux.import(files)
ts7.flux.2022.9$TIMESTAMP <- as.POSIXct(paste(ts7.flux.2022.9$date, ts7.flux.2022.9$time, sep=" "), tz = "EST", format="%Y-%m-%d %H:%M")

setwd('/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/EDDYPRO_DATA_TOWERS/T7_FLUX_EDDY_PRO_DATA/2022_DATA/OCTOBER_2022_DATA')
files <- list.files(pattern ="full") # Creates a list of all .dat files in the directory
ts7.flux.2022.10 <- ts7.flux.import(files)
ts7.flux.2022.10$TIMESTAMP <- as.POSIXct(paste(ts7.flux.2022.10$date, ts7.flux.2022.10$time, sep=" "), tz = "EST", format="%Y-%m-%d %H:%M")


setwd('/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/EDDYPRO_DATA_TOWERS/T7_FLUX_EDDY_PRO_DATA/2022_DATA/NOVEMBER_2022_DATA')
files <- list.files(pattern ="full") # Creates a list of all .dat files in the directory
ts7.flux.2022.11 <- ts7.flux.import(files)
ts7.flux.2022.11$TIMESTAMP <- as.POSIXct(paste(ts7.flux.2022.11$date, ts7.flux.2022.11$time, sep=" "), tz = "EST", format="%Y-%m-%d %H:%M")


ts7.flux.2022 <- smartbind( ts7.flux.2022.1, ts7.flux.2022.2, ts7.flux.2022.3, ts7.flux.2022.4, ts7.flux.2022.5,ts7.flux.2022.6,
                            ts7.flux.2022.7, ts7.flux.2022.8,  ts7.flux.2022.9,  ts7.flux.2022.10,  ts7.flux.2022.11)

load( "/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/Processed Data (Sparkle)/TS7_flux_annual_raw.RDATA")
append.Rda(ts7.flux.2022 , "/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/Processed Data (Sparkle)/TS7_flux_annual_raw.RDATA")


rm( ts7.flux.2022.1, ts7.flux.2022.2, ts7.flux.2022.3, ts7.flux.2022.4, ts7.flux.2022.5,ts7.flux.2022.6,
    ts7.flux.2022.7, ts7.flux.2022.8, ts7.flux.2022.9,  ts7.flux.2022.10,  ts7.flux.2022.11)

#### Current Year ####

#2023

setwd('/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/EDDYPRO_DATA_TOWERS/T7_FLUX_EDDY_PRO_DATA/2023_DATA/JANUARY_2023_DATA')
files <- list.files(pattern ="full") # Creates a list of all .dat files in the directory
ts7.flux.2023.1 <- ts7.flux.import(files)
ts7.flux.2023.1$TIMESTAMP <- as.POSIXct(paste(ts7.flux.2023.1$date, ts7.flux.2023.1$time, sep=" "), tz = "EST", format="%Y-%m-%d %H:%M")

setwd('/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/EDDYPRO_DATA_TOWERS/T7_FLUX_EDDY_PRO_DATA/2023_DATA/FEBRUARY_2023_DATA')
files <- list.files(pattern ="full") # Creates a list of all .dat files in the directory
ts7.flux.2023.2 <- ts7.flux.import(files)
ts7.flux.2023.2$TIMESTAMP <- as.POSIXct(paste(ts7.flux.2023.2$date, ts7.flux.2023.2$time, sep=" "), tz = "EST", format="%Y-%m-%d %H:%M")

setwd('/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/EDDYPRO_DATA_TOWERS/T7_FLUX_EDDY_PRO_DATA/2023_DATA/MARCH_2023_DATA')
files <- list.files(pattern ="full") # Creates a list of all .dat files in the directory
ts7.flux.2023.3 <- ts7.flux.import(files)
ts7.flux.2023.3$TIMESTAMP <- as.POSIXct(paste(ts7.flux.2023.3$date, ts7.flux.2023.3$time, sep=" "), tz = "EST", format="%Y-%m-%d %H:%M")

setwd('/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/EDDYPRO_DATA_TOWERS/T7_FLUX_EDDY_PRO_DATA/2023_DATA/MAY_2023_DATA')
files <- list.files(pattern ="full") # Creates a list of all .dat files in the directory
ts7.flux.2023.4 <- ts7.flux.import(files)
ts7.flux.2023.4$TIMESTAMP <- as.POSIXct(paste(ts7.flux.2023.4$date, ts7.flux.2023.4$time, sep=" "), tz = "EST", format="%Y-%m-%d %H:%M")


load( "/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/Processed Data (Sparkle)/TS7_flux_annual_raw.RDATA")
ts7.flux.2023 <- smartbind( ts7.flux.2023.1, ts7.flux.2023.2, ts7.flux.2023.3, ts7.flux.2023.4)

append.Rda(ts7.flux.2023 , "/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/Processed Data (Sparkle)/TS7_flux_annual_raw.RDATA")

rm( ts7.flux.2023.1, ts7.flux.2023.2, ts7.flux.2023.3, ts7.flux.2023.4)


######################## Processing  Flux Data #####################################

# Import and concatenate .dat files in R.
load("/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/Processed Data (Sparkle)/TS7_flux_annual_raw.RDATA")
library(gtools)

 # I am not using the 2016 data because it has the wrong time stamp...
# Merge all the annual flux files:
ts7.flux <- smartbind( ts7.flux.2017, ts7.flux.2018, ts7.flux.2019, ts7.flux.2020,ts7.flux.2021,ts7.flux.2022,ts7.flux.2023 )

ts7.flux$TIMESTAMP <- as.POSIXct(ts7.flux$TIMESTAMP , tz = "EST")
rm( ts7.flux.2016,ts7.flux.2017, ts7.flux.2018, ts7.flux.2019, ts7.flux.2020,ts7.flux.2021,ts7.flux.2022 ,ts7.flux.2023 )

# Make all corrections to .dat timestamp.
# Creat time stamp to Aggregate Data by half hour
ts7.flux$DATE <- format( ts7.flux$TIMESTAMP, format="%Y-%m-%d")
ts7.flux$Hour <- format( ts7.flux$TIMESTAMP, format= "%H")
ts7.flux$Min <- format( ts7.flux$TIMESTAMP, format= "%M")
ts7.flux$Minutes[ts7.flux$Min <= "29"]<- "00"
ts7.flux$Minutes[ts7.flux$Min > "29"]<-"30"

ts7.flux$TIME <- paste(ts7.flux$Hour, ts7.flux$Minutes, sep=":")
ts7.flux$TIMESTAMP.2 <- as.POSIXct(paste(ts7.flux$DATE, ts7.flux$TIME, sep=" " ),  tz = "EST", format="%Y-%m-%d %H:%M" )
ts7.flux$DATE <- ts7.flux$Hour <- ts7.flux$Min <- ts7.flux$Minutes <-ts7.flux$TIME <- NULL

# Create Halfhourly dataset:
ts7.flux1 <- aggregate(ts7.flux, by=list(ts7.flux$TIMESTAMP.2), FUN=mean, na.rm=T); rm(ts7.flux)
ts7.flux <-ts7.flux1
ts7.flux$TIMESTAMP <- ts7.flux$Group.1
ts7.flux$Group.1 <- ts7.flux$TIMESTAMP.2 <- ts7.flux1 <-NULL

# TimeStamp:
# Remove Duplicate Time Stamps:
ts7.flux <-  ts7.flux[!duplicated( ts7.flux$TIMESTAMP), ] # Remove duplicates 

ts <- data.frame(TIMESTAMP =seq.POSIXt(as.POSIXct("2016-01-01 00:00:00 EST"), as.POSIXct("2023-12-31 23:30:00 EST"), units = "sec", by = 1800))

ts7.flux <- merge( ts, ts7.flux, all=T, by="TIMESTAMP"); rm(ts) # Merge the TS file with the met file

# Merge Flux and met data:
load('/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/Processed Data (Sparkle)/TS7_MET_TOTAL_Clean.RDATA')

ts7.met$TIMESTAMP <- as.POSIXct(ts7.met$TIMESTAMP, tz = "EST", format="%Y-%m-%d %H:%M:%S")

names(ts7.met)  %in% names(ts7.flux) # Test for like columns
ts7.mf <- merge( ts7.flux, ts7.met, by="TIMESTAMP", all=T)

names(ts7.mf)

# #####  Need to bring in gapfilled met date ######: 
load('/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/Processed Data (Sparkle)/TS7_MET_TOTAL_Clean_Filled.RDATA')

summary(tsph7.filled$TIMESTAMP)
tsph7.filled <- tsph7.filled[which( tsph7.filled$TIMESTAMP > as.POSIXct("2016-12-31 24:30:00 EST" ) ),]
tsph7.filled  <- tsph7.filled [!duplicated( tsph7.filled $TIMESTAMP), ] 

names(ts7.mf) %in% names(tsph7.filled) # check for repeat names

ts7 <- merge( ts7.mf, tsph7.filled, by="TIMESTAMP", all=T)
summary(ts7$TIMESTAMP)
names(ts7)

# Filter thresholds:
par(mfrow=c(1,1))
plot(ts7.flux$TIMESTAMP, ts7.flux$co2_flux)

# Calculate the Storage Flux:
 ts7$nee.org <- ts7$co2_flux -  ts7$co2_strg # Needed for Report
 ts7$LE.org <- ts7$LE +  ts7$LE_strg # Needed for Report
 ts7$H.org <- ts7$H +  ts7$H_strg # Needed for Report
 
 ts7$nee <- ts7$co2_flux -  ts7$co2_strg
 ts7$LE <- ts7$LE +  ts7$LE_strg
 ts7$H <- ts7$H +  ts7$H_strg
 
 ## Filtering NEE:

plot(ts7$TIMESTAMP,ts7$nee)

# Out of Range
ts7$nee[ts7$nee > 200] <- NA
ts7$nee[ts7$nee < -50] <- NA
#ts7$nee[ts7$nee < -5 & ts7$PAR.f <10] <- NA # I am not filtering the opposite of this so I wont do this either 
ts7$nee[ts7$nee < -15 & ts7$PAR.f < 500] <- NA
ts7$nee[ts7$nee > 50 & ts7$PAR.f < 500] <- NA
ts7$nee[ts7$nee > 20 & ts7$PAR.f > 500] <- NA
ts7$nee[ts7$nee > 50] <- NA


plot(ts7$Rnet.f, ts7$H)
plot(ts7$Rnet.f, ts7$LE)

summary(lm(  ts7$H ~ ts7$Rnet.f))
summary(lm(  ts7$LE ~ ts7$Rnet.f))

ts7$H[ts7$H > 800] <- NA
ts7$H[ts7$H < -800] <- NA
ts7$LE[ts7$LE > 800] <- NA
ts7$LE[ts7$LE < -800] <- NA

# Remove all fluxes when any other flux is missing
ts7$nee[is.na(ts7$H)] <- NA
ts7$nee[is.na(ts7$LE)] <- NA
ts7$H[is.na(ts7$nee)] <- NA
ts7$LE[is.na(ts7$nee)] <- NA


# ### Gapfilling Energy and Fluxes: ####
le.parms <- Gapfill.parms.le(ts7)
h.parms <- Gapfill.parms.h(ts7)

ts7 <- Gapfill.energy.h( Gapfill.energy.le( ts7, le.parms), h.parms)

names(ts7)
# Gap filling fluxes:
######## code to apply functions:####

ts7.gaps <- gap.length(ts7)
ts7.MDV.5 <- MDV.5(ts7.gaps)
ts7.co2.lut <- lut.PAR.TA(ts7, flux='co2' ) # create a look up table for co2

ts7.co2.lut$model.lut <- ts7.co2.lut$model
ts7.co2.lut$model <- NULL

# Join the LUT with the dataframe to gapfill
## Add time into file:
ts7.MDV.5 <- ta.bins(ppfd.bins(ts7.MDV.5))
ts7.MDV.5$time <- format( ts7.MDV.5$TIMESTAMP,'%y-%m')
ts7.gapfilled <- ts7.MDV.5 %>% left_join(ts7.co2.lut, by=c("PAR.BINS","TA.BINS", "time"))

ts7.gapfilled$nee.filled <- ts7.gapfilled$nee
ts7.gapfilled$nee.filled[ is.na(ts7.gapfilled$nee.filled) & ts7.gapfilled$gap.length <= 5] <- ts7.gapfilled$nee.mdv[ is.na(ts7.gapfilled$nee.filled) & ts7.gapfilled$gap.length <= 5] 
ts7.gapfilled$nee.filled[ is.na(ts7.gapfilled$nee.filled )] <- ts7.gapfilled$co2.pred[ is.na(ts7.gapfilled$nee.filled )]


# Flux partitioning:
ts7.gapfilled.partitioned <- predict.respiration.parms(ts7.gapfilled)

# Calculate ET and BR
ts7.gapfilled.partitioned <- ET(ts7.gapfilled.partitioned)
ts7.gapfilled.partitioned$BR <- ts7.gapfilled.partitioned$H.filled/ ts7.gapfilled.partitioned$LE.filled


ts7.final <- ts7.gapfilled.partitioned[which( ts7.gapfilled.partitioned$TIMESTAMP >= as.POSIXct("2017-01-01 00:00:00 EST" )),]

write.csv( ts7.final, '/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/TOWERS_RAW_FIELD_DATA/TS7_data.csv')


save( ts7.final, le.parms, h.parms, ts7, ts7.co2.lut, file = "/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/Processed Data (Sparkle)/TS7.RData")

save(ts7.final, le.parms, h.parms, ts7, ts7.co2.lut, file =  "/Users/sm3466/Dropbox (YSE)/Research/Flux Towers/TSPH7/TS7.RData")

write.csv( ts7.flux.2023 , "/Users/sm3466/Dropbox (YSE)/Research/Flux Towers/BLUEFLUX_erin/FCE_LTER_TSPH7_test.csv")
