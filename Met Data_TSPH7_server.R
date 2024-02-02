# Sparkle L. Malone (smalone@fiu.edu)
rm(list=ls())

# TS 7 Met Data Script:
# Import the data:
library(gtools)

## FUNCTIONS: 
# Import metdata function:
ts7.met.import <- function(file_list){
  
  for (file in file_list){
    print(file)
    # if the merged dataset doesn't exist, create it
    if (!exists("met_raw")){
      
      header <- read.csv(file, nrows=1, skip=1)
      met_raw <- read.csv(file, header=FALSE, skip=4, stringsAsFactors=FALSE)
      colnames( met_raw ) <- colnames(header)
      rm(header)
      
    }else{
      #if the merged dataset does exist, append to it
      
      if (exists("met_raw")){
        temp_dataset <-read.csv(file, header=FALSE, skip=4, stringsAsFactors=FALSE)
        header <- read.csv(file, nrows=1, skip=1)
        colnames(temp_dataset) <- colnames(header)
        met_raw <-smartbind(met_raw, temp_dataset)
        rm(temp_dataset, header)
      }}}
  
  met <- met_raw
  rm(met_raw)
  return(met)
  rm(met)
}
met.na <- function(met.file){
  
  met.file[ met.file == -99999.000 ] <- NA
  met.file[ met.file == -99999] <- NA
  met.file[ met.file == -6999] <- NA
  met.file[ met.file == 99999.000 ] <- NA
  met.file[ met.file == 99999] <- NA
  met.file[ met.file == 6999] <- NA
  
  met.file <- met.na(met.file)
  return(met.file)
}
append.Rda <- function(x, file) {
  old.objects <- load(file, new.env())
  save(list = c(old.objects, deparse(substitute(x))), file = file)
}

#save(list = c("x", "y"), file = "temp.Rda")
#append.Rda(z, "temp.Rda")

#### IMPORTING MET .dat FILES ####
# Air Files:

#2017
return.2017 <-"/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/TOWERS_RAW_FIELD_DATA/T7_TOWER_DATA/TS7_2017_DATA/Data_downloaded_1-26-17/LGN_DATA/MetData"
setwd(return.2017)
file_list <- list.files(pattern="*.dat")
t7.2017.1 <- ts7.met.import(file_list)


return.2017 <-"/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/TOWERS_RAW_FIELD_DATA/T7_TOWER_DATA/TS7_2017_DATA/Data_downloaded_4-18-17/MET_DATA"
setwd(return.2017)
file_list <- list.files(pattern="*.dat")
t7.2017.4 <- ts7.met.import(file_list)

return.2017 <-"/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/TOWERS_RAW_FIELD_DATA/T7_TOWER_DATA/TS7_2017_DATA/Data_downloaded_5-25-17/MET_DATA"
setwd(return.2017)
file_list <- list.files(pattern="*.dat")
t7.2017.5 <- ts7.met.import(file_list)


return.2017 <-"/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/TOWERS_RAW_FIELD_DATA/T7_TOWER_DATA/TS7_2017_DATA/Data_downloaded_6-26-17/MET_DATA"
setwd(return.2017)
file_list <- list.files(pattern="*.dat")
t7.2017.6 <- ts7.met.import(file_list)


return.2017 <-"/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/TOWERS_RAW_FIELD_DATA/T7_TOWER_DATA/TS7_2017_DATA/Data_downloaded_8-1-17/MET_DATA"
setwd(return.2017)
file_list <- list.files(pattern="*.dat")
t7.2017.8a <- ts7.met.import(file_list)

return.2017 <-"/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/TOWERS_RAW_FIELD_DATA/T7_TOWER_DATA/TS7_2017_DATA/Data_downloaded_8-28-17/MET_DATA"
setwd(return.2017)
file_list <- list.files(pattern="*.dat")
t7.2017.8b <- ts7.met.import(file_list)

return.2017 <-"/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/TOWERS_RAW_FIELD_DATA/T7_TOWER_DATA/TS7_2017_DATA/Data_downloaded_9-26-17/MET_DATA"
setwd(return.2017)
file_list <- list.files(pattern="*.dat")
t7.2017.9 <- ts7.met.import(file_list)

return.2017 <-"/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/TOWERS_RAW_FIELD_DATA/T7_TOWER_DATA/TS7_2017_DATA/Data_downloaded_10-10-17/METDATA"
setwd(return.2017)
file_list <- list.files(pattern="*.dat")
t7.2017.10a <- ts7.met.import(file_list)

return.2017 <-"/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/TOWERS_RAW_FIELD_DATA/T7_TOWER_DATA/TS7_2017_DATA/Data_downloaded_10-26-17/METADATA"
setwd(return.2017)
file_list <- list.files(pattern="*.dat")
t7.2017.10b <- ts7.met.import(file_list)

return.2017 <-"/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/TOWERS_RAW_FIELD_DATA/T7_TOWER_DATA/TS7_2017_DATA/Data_downloaded_11-17-17/METDATA"
setwd(return.2017)
file_list <- list.files(pattern="*.dat")
t7.2017.11 <- ts7.met.import(file_list)

return.2017 <-"/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/TOWERS_RAW_FIELD_DATA/T7_TOWER_DATA/TS7_2017_DATA/Data_downloaded_12-17-17/METDATA"
setwd(return.2017)
file_list <- list.files(pattern="*.dat")
t7.2017.12 <- ts7.met.import(file_list)

ts7.2017 <- smartbind( t7.2017.1, t7.2017.4, t7.2017.5, t7.2017.6, t7.2017.5,t7.2017.8a, t7.2017.8b, t7.2017.9,t7.2017.10a,t7.2017.10b, t7.2017.11 , t7.2017.12)
save(ts7.2017, file = "/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/Processed Data (Sparkle)/TS7_MET.RDATA")
rm( t7.2017.1, t7.2017.4, t7.2017.5, t7.2017.6, t7.2017.5,t7.2017.8a, t7.2017.8b, t7.2017.10a,t7.2017.10b, t7.2017.11 , t7.2017.12)

# 2018

return.2018 <-"/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/TOWERS_RAW_FIELD_DATA/T7_TOWER_DATA/TS7_2018_DATA/Data_downloaded_2-7-18/METDATA"
setwd(return.2018)
file_list <- list.files(pattern="*.dat")
t7.2018.2 <- ts7.met.import(file_list)

return.2018 <-"/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/TOWERS_RAW_FIELD_DATA/T7_TOWER_DATA/TS7_2018_DATA/Data_downloaded_3-9-18/METDATA"
setwd(return.2018)
file_list <- list.files(pattern="*.dat")
t7.2018.3 <- ts7.met.import(file_list)

return.2018 <-"/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/TOWERS_RAW_FIELD_DATA/T7_TOWER_DATA/TS7_2018_DATA/Data_downloaded_4-3-18/METDATA"
setwd(return.2018)
file_list <- list.files(pattern="*.dat")
t7.2018.4 <- ts7.met.import(file_list)

return.2018 <-"/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/TOWERS_RAW_FIELD_DATA/T7_TOWER_DATA/TS7_2018_DATA/Data_downloaded_5-10-18/METDATA"
setwd(return.2018)
file_list <- list.files(pattern="*.dat")
t7.2018.5 <- ts7.met.import(file_list)

return.2018 <-"/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/TOWERS_RAW_FIELD_DATA/T7_TOWER_DATA/TS7_2018_DATA/Data_downloaded_6-13-18/METDATA"
setwd(return.2018)
file_list <- list.files(pattern="*.dat")
t7.2018.6 <- ts7.met.import(file_list)

return.2018 <-"/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/TOWERS_RAW_FIELD_DATA/T7_TOWER_DATA/TS7_2018_DATA/Data_downloaded_7-9-18/METDATA"
setwd(return.2018)
file_list <- list.files(pattern="*.dat")
t7.2018.7 <- ts7.met.import(file_list)

return.2018 <-"/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/TOWERS_RAW_FIELD_DATA/T7_TOWER_DATA/TS7_2018_DATA/Data_downloaded_8-9-18/METDATA"
setwd(return.2018)
file_list <- list.files(pattern="*.dat")
t7.2018.8 <- ts7.met.import(file_list)

return.2018 <-"/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/TOWERS_RAW_FIELD_DATA/T7_TOWER_DATA/TS7_2018_DATA/Data_downloaded_9-7-18/MET_DATA"
setwd(return.2018)
file_list <- list.files(pattern="*.dat")
t7.2018.9 <- ts7.met.import(file_list)

return.2018 <-"/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/TOWERS_RAW_FIELD_DATA/T7_TOWER_DATA/TS7_2018_DATA/Data_downloaded_10-12-18/METDATA"
setwd(return.2018)
file_list <- list.files(pattern="*.dat")
t7.2018.10 <- ts7.met.import(file_list)

return.2018 <-"/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/TOWERS_RAW_FIELD_DATA/T7_TOWER_DATA/TS7_2018_DATA/Data_downloaded_11-29-18/METDATA"
setwd(return.2018)
file_list <- list.files(pattern="*.dat")
t7.2018.11 <- ts7.met.import(file_list)

return.2018 <-"/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/TOWERS_RAW_FIELD_DATA/T7_TOWER_DATA/TS7_2018_DATA/Data_downloaded_12-17-18/METDATA"
setwd(return.2018)
file_list <- list.files(pattern="*.dat")
t7.2018.12 <- ts7.met.import(file_list)

ts7.2018 <- smartbind( t7.2018.2,t7.2018.3, t7.2018.4, t7.2018.5, t7.2018.6, t7.2018.7,t7.2018.8, t7.2018.9,t7.2018.10, t7.2018.11 , t7.2018.12)
append.Rda(ts7.2018, "/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/Processed Data (Sparkle)/TS7_MET.RDATA")
rm( t7.2018.2,t7.2018.3, t7.2018.4, t7.2018.5, t7.2018.6, t7.2018.7,t7.2018.8, t7.2018.9,t7.2018.10, t7.2018.11 , t7.2018.12)

# 2019

return.2019 <-"/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/TOWERS_RAW_FIELD_DATA/T7_TOWER_DATA/TS7_2019_DATA/Data_downloaded_1-18-19/METDATA"
setwd(return.2019)
file_list <- list.files(pattern="*.dat")
t7.2019.1 <- ts7.met.import(file_list)

return.2019 <-"/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/TOWERS_RAW_FIELD_DATA/T7_TOWER_DATA/TS7_2019_DATA/Data_downloaded_2-15-19/METDATA"
setwd(return.2019)
file_list <- list.files(pattern="*.dat")
t7.2019.2 <- ts7.met.import(file_list)

return.2019 <-"/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/TOWERS_RAW_FIELD_DATA/T7_TOWER_DATA/TS7_2019_DATA/Data_downloaded_3-15-19/METDATA"
setwd(return.2019)
file_list <- list.files(pattern="*.dat")
t7.2019.3 <- ts7.met.import(file_list)

return.2019 <-"/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/TOWERS_RAW_FIELD_DATA/T7_TOWER_DATA/TS7_2019_DATA/Data_downloaded_4-12-19/METDATA"
setwd(return.2019)
file_list <- list.files(pattern="*.dat")
t7.2019.4 <- ts7.met.import(file_list)

return.2019 <-"/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/TOWERS_RAW_FIELD_DATA/T7_TOWER_DATA/TS7_2019_DATA/Data_downloaded_5-20-19/METDATA"
setwd(return.2019)
file_list <- list.files(pattern="*.dat")
t7.2019.5 <- ts7.met.import(file_list)

return.2019 <-"/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/TOWERS_RAW_FIELD_DATA/T7_TOWER_DATA/TS7_2019_DATA/Data_downloaded_6-21-19/METDATA"
setwd(return.2019)
file_list <- list.files(pattern="*.dat")
t7.2019.6 <- ts7.met.import(file_list)

return.2019 <-"/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/TOWERS_RAW_FIELD_DATA/T7_TOWER_DATA/TS7_2019_DATA/Data_downloaded_7-19-19/METDATA"
setwd(return.2019)
file_list <- list.files(pattern="*.dat")
t7.2019.7 <- ts7.met.import(file_list)

return.2019 <-"/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/TOWERS_RAW_FIELD_DATA/T7_TOWER_DATA/TS7_2019_DATA/Data_downloaded_8-23-19/METDATA"
setwd(return.2019)
file_list <- list.files(pattern="*.dat")
t7.2019.8 <- ts7.met.import(file_list)

return.2019 <-"/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/TOWERS_RAW_FIELD_DATA/T7_TOWER_DATA/TS7_2019_DATA/Data_downloaded_10-18-19/METDATA"
setwd(return.2019)
file_list <- list.files(pattern="*.dat")
t7.2019.10 <- ts7.met.import(file_list)

return.2019 <-"/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/TOWERS_RAW_FIELD_DATA/T7_TOWER_DATA/TS7_2019_DATA/Data_downloaded_11-15-19/METDATA"
setwd(return.2019)
file_list <- list.files(pattern="*.dat")
t7.2019.11 <- ts7.met.import(file_list)

return.2019 <-"/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/TOWERS_RAW_FIELD_DATA/T7_TOWER_DATA/TS7_2019_DATA/Data_downloaded_12-31-19/METDATA"
setwd(return.2019)
file_list <- list.files(pattern="*.dat")
t7.2019.12 <- ts7.met.import(file_list)

ts7.2019 <- smartbind( t7.2019.1, t7.2019.2,t7.2019.3, t7.2019.4, t7.2019.5, t7.2019.6, t7.2019.7,t7.2019.8,t7.2019.10, t7.2019.11 , t7.2019.12)
append.Rda(ts7.2019, "/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/Processed Data (Sparkle)/TS7_MET.RDATA")
rm( t7.2019.1, t7.2019.2,t7.2019.3, t7.2019.4, t7.2019.5, t7.2019.6, t7.2019.7,t7.2019.8, t7.2019.9,t7.2019.10, t7.2019.11 , t7.2019.12)

#2020 COVID-19

return.2020 <-"/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/TOWERS_RAW_FIELD_DATA/T7_TOWER_DATA/TS7_2020_DATA/Data_downloaded_1-14-20/METDATA"
setwd(return.2020)
file_list <- list.files(pattern="*.dat")
t7.2020.1 <- ts7.met.import(file_list)

return.2020 <-"/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/TOWERS_RAW_FIELD_DATA/T7_TOWER_DATA/TS7_2020_DATA/Data_downloaded_2-19-20/METDATA"
setwd(return.2020)
file_list <- list.files(pattern="*.dat")
t7.2020.2 <- ts7.met.import(file_list)

return.2020 <-"/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/TOWERS_RAW_FIELD_DATA/T7_TOWER_DATA/TS7_2020_DATA/Data_downloaded_5-21-20/METDATA"
setwd(return.2020)
file_list <- list.files(pattern="*.dat")
t7.2020.5 <- ts7.met.import(file_list)

return.2020 <-"/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/TOWERS_RAW_FIELD_DATA/T7_TOWER_DATA/TS7_2020_DATA/Data_downloaded_6-25-20/METDATA"
setwd(return.2020)
file_list <- list.files(pattern="*.dat")
t7.2020.6 <- ts7.met.import(file_list)

return.2020 <-"/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/TOWERS_RAW_FIELD_DATA/T7_TOWER_DATA/TS7_2020_DATA/Data_downloaded_7-21-20/METDATA"
setwd(return.2020)
file_list <- list.files(pattern="*.dat")
t7.2020.7 <- ts7.met.import(file_list)

return.2020 <-"/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/TOWERS_RAW_FIELD_DATA/T7_TOWER_DATA/TS7_2020_DATA/Data_downloaded_9-25-20/METDATA"
setwd(return.2020)
file_list <- list.files(pattern="*.dat")
t7.2020.9 <- ts7.met.import(file_list)

return.2020 <-"/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/TOWERS_RAW_FIELD_DATA/T7_TOWER_DATA/TS7_2020_DATA/Data_downloaded_12-4-20/METDATA"
setwd(return.2020)
file_list <- list.files(pattern="*.dat")
t7.2020.12a <- ts7.met.import(file_list)

return.2020 <-"/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/TOWERS_RAW_FIELD_DATA/T7_TOWER_DATA/TS7_2020_DATA/Data_downloaded_12-31-20/METDATA"
setwd(return.2020)
file_list <- list.files(pattern="*.dat")
t7.2020.12b <- ts7.met.import(file_list)

ts7.2020 <- smartbind( t7.2020.1, t7.2020.2, t7.2020.5, t7.2020.6, t7.2020.7, t7.2020.9, t7.2020.12a , t7.2020.12b)
append.Rda(ts7.2020, "/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/Processed Data (Sparkle)/TS7_MET.RDATA")
rm( t7.2020.1, t7.2020.2, t7.2020.5, t7.2020.6, t7.2020.7, t7.2020.9, t7.2020.12a , t7.2020.12b)

# 2021

return.2021 <-"/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/TOWERS_RAW_FIELD_DATA/T7_TOWER_DATA/TS7_2021_DATA/Data_downloaded_1-22-21/METDATA"
setwd(return.2021)
file_list <- list.files(pattern="*.dat")
t7.2021.1 <- ts7.met.import(file_list)

return.2021 <-"/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/TOWERS_RAW_FIELD_DATA/T7_TOWER_DATA/TS7_2021_DATA/Data_downloaded_3-12-21/METDATA"
setwd(return.2021)
file_list <- list.files(pattern="*.dat")
t7.2021.3 <- ts7.met.import(file_list)

return.2021 <-"/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/TOWERS_RAW_FIELD_DATA/T7_TOWER_DATA/TS7_2021_DATA/Data_downloaded_5-25-21/METDATA"
setwd(return.2021)
file_list <- list.files(pattern="*.dat")
t7.2021.5 <- ts7.met.import(file_list)

return.2021 <-"/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/TOWERS_RAW_FIELD_DATA/T7_TOWER_DATA/TS7_2021_DATA/Data_downloaded_7-16-21/METDATA"
setwd(return.2021)
file_list <- list.files(pattern="*.dat")
t7.2021.7 <- ts7.met.import(file_list)

return.2021 <-"/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/TOWERS_RAW_FIELD_DATA/T7_TOWER_DATA/TS7_2021_DATA/Data_downloaded_9-21-21/METDATA"
setwd(return.2021)
file_list <- list.files(pattern="*.dat")
t7.2021.9 <- ts7.met.import(file_list)

return.2021 <-"/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/TOWERS_RAW_FIELD_DATA/T7_TOWER_DATA/TS7_2021_DATA/Data_downloaded_11-2-21/METDATA"
setwd(return.2021)
file_list <- list.files(pattern="*.dat")
t7.2021.11 <- ts7.met.import(file_list)

return.2021 <-"/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/TOWERS_RAW_FIELD_DATA/T7_TOWER_DATA/TS7_2021_DATA/Data_downloaded_12-31-21/METDATA"
setwd(return.2021)
file_list <- list.files(pattern="*.dat")
t7.2021.12 <- ts7.met.import(file_list)

ts7.2021 <- smartbind( t7.2021.1, t7.2021.3, t7.2021.5, t7.2021.7, t7.2021.9, t7.2021.11, t7.2021.12)
append.Rda(ts7.2021, "/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/Processed Data (Sparkle)/TS7_MET.RDATA")
rm( t7.2021.1, t7.2021.3, t7.2021.5, t7.2021.7, t7.2021.9, t7.2021.11, t7.2021.12)

#2022

return.2022 <-"/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/TOWERS_RAW_FIELD_DATA/T7_TOWER_DATA/TS7_2022_DATA/Data_downloaded_1-13-22/METDATA"
setwd(return.2022)
file_list <- list.files(pattern="*.dat")
t7.2022.1 <- ts7.met.import(file_list)


return.2022 <-"/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/TOWERS_RAW_FIELD_DATA/T7_TOWER_DATA/TS7_2022_DATA/Data_downloaded_3-25-22/METDATA"
setwd(return.2022)
file_list <- list.files(pattern="*.dat")
t7.2022.2 <- ts7.met.import(file_list)


return.2022 <-"/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/TOWERS_RAW_FIELD_DATA/T7_TOWER_DATA/TS7_2022_DATA/Data_downloaded_5-24-22/METDATA"
setwd(return.2022)
file_list <- list.files(pattern="*.dat")
t7.2022.3 <- ts7.met.import(file_list)


return.2022 <-"/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/TOWERS_RAW_FIELD_DATA/T7_TOWER_DATA/TS7_2022_DATA/Data_downloaded_6-24-22/METDATA"
setwd(return.2022)
file_list <- list.files(pattern="*.dat")
t7.2022.4 <- ts7.met.import(file_list)


return.2022 <-"/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/TOWERS_RAW_FIELD_DATA/T7_TOWER_DATA/TS7_2022_DATA/Data_downloaded_10-20-22/METDATA"
setwd(return.2022)
file_list <- list.files(pattern="*.dat")
t7.2022.5 <- ts7.met.import(file_list)

return.2022 <-"/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/TOWERS_RAW_FIELD_DATA/T7_TOWER_DATA/TS7_2022_DATA/Data_downloaded_12-22-22/METDATA"
setwd(return.2022)
file_list <- list.files(pattern="*.dat")
t7.2022.6 <- ts7.met.import(file_list)

load("/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/Processed Data (Sparkle)/TS7_MET.RDATA")

ts7.2022 <- smartbind( t7.2022.1, t7.2022.2, t7.2022.3, t7.2022.4, t7.2022.5, t7.2022.6 )

append.Rda(ts7.2022, "/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/Processed Data (Sparkle)/TS7_MET.RDATA")

rm( t7.2022.1, t7.2022.2, t7.2022.3, t7.2022.4, t7.2022.5, t7.2022.6)

#### Current Year: ####
#2023

return.2023 <-"/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/TOWERS_RAW_FIELD_DATA/T7_TOWER_DATA/TS7_2023_DATA"
setwd(return.2023)

setwd('Data_downloaded_3-9-23/METDATA')
file_list <- list.files(pattern="*.dat")
t7.2023.1 <- ts7.met.import(file_list)

setwd(return.2023)
setwd('Data_downloaded_4-27-23/METDATA')
file_list <- list.files(pattern="*.dat")
t7.2023.2 <- ts7.met.import(file_list)

setwd(return.2023)
setwd('Data_downloaded_7-7-23/METDATA')
file_list <- list.files(pattern="*.dat")
t7.2023.3 <- ts7.met.import(file_list)

load("/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/Processed Data (Sparkle)/TS7_MET.RDATA")

ts7.2023 <- smartbind( t7.2023.1, t7.2023.2 , t7.2023.3)
rm( t7.2023.1, t7.2023.2 , t7.2023.3)
append.Rda(ts7.2023, "/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/Processed Data (Sparkle)/TS7_MET.RDATA")


#............................................................................................................


##### Processing Met Files ####

load("/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/Processed Data (Sparkle)/TS7_MET.RDATA")
# Combine all files into a single file.

ts7 <- smartbind(ts7.2017, ts7.2018, ts7.2019, ts7.2020, ts7.2021, ts7.2022 , ts7.2023)

#TimeStamp:
 
ts7$TIMESTAMP <- as.POSIXct( ts7$TIMESTAMP, tz = "EST")

# Remove Duplicate Timestamps:
ts7 <-  ts7[!duplicated( ts7$TIMESTAMP), ] # Remove duplicates 

# Update the end date
ts <- data.frame(TIMESTAMP =seq.POSIXt(as.POSIXct("2016-01-01 00:00:00 EST"), as.POSIXct("2023-12-31 23:30:00 EST"), units = "sec", by = 1800))

ts7.met <- merge( ts, ts7, all.x=T, by="TIMESTAMP"); rm(ts) # Merge the TS file with the met file

# Format PAR:
ts7.met$PAR_Den1_Avg <- as.numeric( ts7.met$PAR_Den1_Avg)
ts7.met$PAR_Den2_Avg <- as.numeric( ts7.met$PAR_Den2_Avg)
ts7.met$PAR_Den3_Avg <- as.numeric( ts7.met$PAR_Den3_Avg)

plot(ts7.met$TIMESTAMP, ts7.met$PAR_Den1_Avg )

ts7.met$PAR_Tot1_Tot <- as.numeric( ts7.met$PAR_Tot1_Tot)
ts7.met$PAR_Tot2_Tot <- as.numeric( ts7.met$PAR_Tot2_Tot)
ts7.met$PAR_Tot3_Tot <- as.numeric( ts7.met$PAR_Tot3_Tot)


ts7.met$Albedo_Avg <- as.numeric( ts7.met$Albedo_Avg)


# Filter function:
ts.7.filter.met <- function(df){
  df$Albedo_Avg[df$Albedo_Avg < -100 | df$Albedo_Avg > 100] <- NA 
  
  df$temp3_Avg[ df$TIMESTAMP >= as.POSIXct("2019-11-14 00:00:00 EST")  & df$TIMESTAMP <= as.POSIXct("2020-01-05 23:30:00 EST" ) ] <- NA
  df$temp3_Avg[ df$TIMESTAMP >= as.POSIXct("2020-06-24 00:30:00 EST")  & df$TIMESTAMP <= as.POSIXct("2020-06-25 23:30:00 EST" ) ]
  df$temp3_Avg[df$temp3_Avg == -13.53 ] <- NA 
  df$temp3_Avg[ df$TIMESTAMP >= as.POSIXct("2020-05-17 08:30:00 EST")  & df$TIMESTAMP <= as.POSIXct("2020-05-17 12:30:00 EST" ) ] <- NA
  df$temp3_Avg[ df$TIMESTAMP >= as.POSIXct("2020-05-16 10:00:00 EST")  & df$TIMESTAMP <= as.POSIXct("2020-05-16 15:00:00 EST" ) ] <- NA
  
  df$cm10_Avg[df$TIMESTAMP >= as.POSIXct("2022-03-21 00:00:00") & df$TIMESTAMP <= as.POSIXct("2022-03-31 23:30:00") ] <- NA
  df$UpTot_Avg[df$UpTot_Avg < 0] <- NA
  df$NetRl_Avg[df$NetRl_Avg > 0] <- NA
  return(df)
}

ts7.met.f <- ts.7.filter.met(ts7.met)

summary(ts7.met.f)

attach(ts7.met.f)
# Look at the data:

par(mfrow=c(3, 2))
plot(TIMESTAMP, CM3Up_Avg) #
plot(TIMESTAMP, CM3Dn_Avg) # 
plot(TIMESTAMP, CG3Up_Avg) # 
plot(TIMESTAMP, CG3Dn_Avg) # 
plot(TIMESTAMP, CNR1TC_Avg) #
plot(TIMESTAMP, CNR1TC_Avg) #

par(mfrow=c(1, 3))
plot(TIMESTAMP, NetRs_Avg) 
plot(TIMESTAMP, NetRl_Avg) 
plot(TIMESTAMP, NetTot_Avg) # 

par(mfrow=c(1, 3))
plot(TIMESTAMP, Albedo_Avg) 
plot(TIMESTAMP, UpTot_Avg) # 
plot(TIMESTAMP, DnTot_Avg) #

par(mfrow=c(3, 2))
plot(TIMESTAMP, PAR_Den1_Avg ) # PAR Downwelling
plot(TIMESTAMP, PAR_Tot1_Tot) #
plot(TIMESTAMP, PAR_Den2_Avg) # PAR Upwelling
plot(TIMESTAMP, PAR_Tot2_Tot) #
plot(TIMESTAMP, PAR_Tot3_Tot ) # PAR under water?
plot(TIMESTAMP, PAR_Den3_Avg) #

ts7.met$PAR <- ts7.met$PAR_Den1_Avg

par(mfrow=c(1, 2))
plot(TIMESTAMP, CG3UpCo_Avg) # 
plot(TIMESTAMP, CG3DnCo_Avg) # 

par(mfrow=c(1, 3))
plot(TIMESTAMP, WS_ms_Avg) #
plot(TIMESTAMP, WS_ms_S_WVT) #
plot(TIMESTAMP, WindDir_D1_WVT) #

par(mfrow=c(1, 3))
plot(TIMESTAMP, temp1_Avg) #
plot(TIMESTAMP, temp2_Avg) #
plot(TIMESTAMP, temp3_Avg) #

par(mfrow=c(1, 1))
plot(TIMESTAMP, temp3_Avg) #

par(mfrow=c(1, 2))
plot(TIMESTAMP, RH1_Avg) #
plot(TIMESTAMP, RH2_Avg) #

par(mfrow=c(1, 2))
plot(TIMESTAMP, cm5_Avg) #
plot(TIMESTAMP, cm10_Avg) #


ts7.met <- ts7.met.f

write.csv( ts7.met, '/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/Processed Data (Sparkle)/tsph7_met_filtered.csv')
save(ts7.met, file='/Volumes/inwedata/WEEL/Tech/Data/EDDY_COVARIANCE_TOWERS_DATA/Processed Data (Sparkle)/TS7_MET_TOTAL_Clean.RDATA')
 
