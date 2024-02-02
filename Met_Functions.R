# Import metdata and concactanate:
library(gtools)
library("splitstackshape")

met.import <- function(filelist, headers){
  for (file in file_list){
    print(file)
    if (!exists("ts_met_raw")){
      ts_met_raw <- read.csv(file, header=FALSE, stringsAsFactors=FALSE, sep=",")
      names(ts_met_raw ) <- names(headers)
    }else{
      if (exists("ts_met_raw")){
        temp_dataset <-read.csv(file, header=FALSE, stringsAsFactors=FALSE, sep=",")
        names(temp_dataset) <- names(headers)
        ts_met_raw<-smartbind(ts_met_raw, temp_dataset)
        
        rm(temp_dataset)
      }}}
  return(ts_met_raw)
}

toa.import <- function(filelist){
  
  for (file in file_list){
    print(file)
    if (!exists("toa_raw")){
      toa_raw <- read.csv(file, header=FALSE, stringsAsFactors=FALSE, skip=4, sep=",")
      names(toa_raw ) <-read.csv(file, header=FALSE, skip=1, stringsAsFactors=FALSE, sep=",")[1,]
    }else{
      if (exists("toa_raw")){
        temp_dataset <-read.csv(file, header=FALSE, stringsAsFactors=FALSE, skip=4, sep=",")
        names(temp_dataset) <- read.csv(file, header=FALSE, skip=1, stringsAsFactors=FALSE, sep=",")[1,]
        toa_raw <- smartbind( toa_raw, temp_dataset)
      }}}
  return( toa_raw)
}

met.na <- function(met.file){
  
  met.file[ met.file == -99999.000 ] <- NA
  met.file[ met.file == -99999] <- NA
  met.file[ met.file == -6999] <- NA
  met.file[ met.file == 99999.000 ] <- NA
  met.file[ met.file == 99999] <- NA
  met.file[ met.file == 6999] <- NA
  return(met.file)
}# Remove -9999

timestamp <- function(start, stop){
  
  # Import leap year and non leap year files:
  nly <-read.csv("~/Dropbox (YSE)/Research/Flux Towers//TimeStamps/TimeStamp_NLY.csv", header=TRUE, stringsAsFactors=FALSE)
  ly <-read.csv("~/Dropbox (YSE)/Research/Flux Towers/TimeStamps/TimeStamp_LY.csv", header=TRUE, stringsAsFactors=FALSE)
  
  # create a list of years of interest:
  years <- seq(start, stop, 1)
  
  # leap year function:
  is.leapyear <- function(year){
    #http://en.wikipedia.org/wiki/Leap_year
    return(((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0))
  }
  
  time <- data.frame(Year=as.numeric(), Month=as.numeric(), Day=as.numeric(), Hour=as.numeric(), Doy=as.numeric(), Hour2=as.numeric())
  
  for (i in years) {
    if( is.leapyear(i) == "TRUE" ) {
      x <- ly
      x$Year <- i
      time <- rbind(time, x)
      rm(x)
    }else{
      x <- nly
      x$Year <- i
      time <- rbind(time, x)
      rm(x)}}
  
  # Create the timestamp:
  #separate time into hours and minutes
  time <-cSplit(time ,4, sep =".", drop = FALSE)
  time$Hours <- time$Hour_1 
  time$minutes <- (time$Hour_2)
  time$Hour_1 <- time$Hour_2 <- NULL
  
  #remove all NAs in the minutes field
  time$minutes[is.na(time$minutes)] <- 0
  
  #make all 5s equal to 30 (the half hour)
  Minutes<-function(x){
    if (x>0){
      y=30
    }else{y=0}
    return(y)
  }
  
  time$minutes2<-lapply(time$minutes, Minutes)
  
  #creates a new time feature (hour: miutes)
  time$minutes <-time$minutes2
  time$minutes<- sprintf("%02d", as.numeric(time$minutes))
  
  time$minutes2<-NULL
  
  time$time<-paste(time$Hours,time$minutes, sep=":")
  time$Hours <- NULL
  
  #creates the date as an date object
  time$date <-as.Date(ISOdate(time$Year, time$Month, time$Day ))
  class(time$date)
  
  time$minutes <- NULL
  time$datetime <-paste(time$date,time$time)
  time$time <- NULL
  return(time)
  rm(ly, nly, time)
}

met.timestamp <- function( met.file, startyear, stopyear){
  timestamp <- function(start, stop){
    
    # Import leap year and non leap year files:
    nly <-read.csv("~/Dropbox (YSE)/Research/Flux Towers/TimeStamps/TimeStamp_NLY.csv", header=TRUE, stringsAsFactors=FALSE)
    ly <-read.csv("~/Dropbox (YSE)/Research/Flux Towers/TimeStamps/TimeStamp_LY.csv", header=TRUE, stringsAsFactors=FALSE)
    
    # create a list of years of interest:
    years <- seq(start, stop, 1)
    
    
    # leap year function:
    is.leapyear <- function(year){
      #http://en.wikipedia.org/wiki/Leap_year
      return(((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0))
    }
    
    time <- data.frame(Year=as.numeric(), Month=as.numeric(), Day=as.numeric(), Hour=as.numeric(), Doy=as.numeric(), Hour2=as.numeric())
    
    for (i in years) {
      if( is.leapyear(i) == "TRUE" ) {
        x <- ly
        x$Year <- i
        time <- rbind(time, x)
        rm(x)
      }else{
        x <- nly
        x$Year <- i
        time <- rbind(time, x)
        rm(x)}}
    
    # Create the timestamp:
    #separate time into hours and minutes
    time <-cSplit(time ,4, sep =".", drop = FALSE)
    time$Hours <- time$Hour_1 
    time$minutes <- (time$Hour_2)
    time$Hour_1 <- time$Hour_2 <- NULL
    
    #remove all NAs in the minutes field
    time$minutes[is.na(time$minutes)] <- 0
    
    #make all 5s equal to 30 (the half hour)
    Minutes<-function(x){
      if (x>0){
        y=30
      }else{y=0}
      return(y)
    }
    
    time$minutes2<-lapply(time$minutes, Minutes)
    
    #creates a new time feature (hour: miutes)
    time$minutes <-time$minutes2
    time$minutes<- sprintf("%02d", as.numeric(time$minutes))
    
    time$minutes2<-NULL
    
    time$time<-paste(time$Hours,time$minutes, sep=":")
    time$Hours <- NULL
    
    #creates the date as an date object
    time$date <-as.Date(ISOdate(time$Year, time$Month, time$Day ))
    class(time$date)
    
    time$minutes <- NULL
    time$datetime <-paste(time$date,time$time)
    time$time <- NULL
    return(time)
    rm(ly, nly, time)
  }
  
  # Create timestamp file:
  timestamp<-timestamp (startyear, stopyear)
  timestamp$Hour2 <- as.numeric(timestamp$Hour2 ) # Format Hour 2
 
  #Get the Month and Day from DOY:
  met.file$date <- as.Date(strptime(paste(met.file$Year_RTM, met.file$Day_RTM), format="%Y %j"))
  met.file$Hour2 <- met.file$Hour_Minute_RTM
  
  # Merge timestamp with met file:
  met.file <- merge( timestamp, met.file, all=T, by= c('date', 'Hour2' ))

  #remove duplicate rows based on time stamp
  met.file <- met.file[!duplicated(met.file$datetime),]
}

toa.timestamp <- function( toa.file , startyear, stopyear){
  
  format.toa <- function(x){
    
    library(lubridate) 
    ymd <- ymd_hms(x$TIMESTAMP)
    mdy <- mdy_hm(x$TIMESTAMP)
    
    summary( ymd)
    summary( mdy)
    
   ymd[is.na(ymd)] <- mdy[is.na(ymd)] # some dates are ambiguous, here we give 
   
    x$DateTime <- ymd
    x$Date <- as.Date(x$DateTime )
    #rm(mdy, ymd, dmy)
    
    x <- x[!duplicated(x$DateTime),] # remove duplicates
    x <- x[order(x$DateTime),] # put sile in order by datetime
    
    return(x)
  }
  
  # formats the toa timestamp
  toa.file <- format.toa(toa.file)
 
  # Create timestamp:
  sample.timestamp <- function(start, stop){
    
    # Import leap year and non leap year files:
    nly <-read.csv("~/Dropbox (YSE)/Research/Flux Towers/TimeStamps/TimeStamp_NLY.csv", header=TRUE, stringsAsFactors=FALSE)
    ly <-read.csv("~/Dropbox (YSE)/Research/Flux Towers/TimeStamps/TimeStamp_LY.csv", header=TRUE, stringsAsFactors=FALSE)
    
    # create a list of years of interest:
    years <- seq(start, stop, 1)
    
    
    # leap year function:
    is.leapyear <- function(year){
      #http://en.wikipedia.org/wiki/Leap_year
      return(((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0))
    }
    
    time <- data.frame(Year=as.numeric(), Month=as.numeric(), Day=as.numeric(), Hour=as.numeric(), Doy=as.numeric(), Hour2=as.numeric())
    
    for (i in years) {
      if( is.leapyear(i) == "TRUE" ) {
        x <- ly
        x$Year <- i
        time <- rbind(time, x)
        rm(x)
      }else{
        x <- nly
        x$Year <- i
        time <- rbind(time, x)
        rm(x)}}
    
    # Create the timestamp:
    #separate time into hours and minutes
    time <-cSplit(time ,4, sep =".", drop = FALSE)
    time$Hours <- time$Hour_1 
    time$minutes <- (time$Hour_2)
    time$Hour_1 <- time$Hour_2 <- NULL
    
    #remove all NAs in the minutes field
    time$minutes[is.na(time$minutes)] <- 0
    
    #make all 5s equal to 30 (the half hour)
    Minutes<-function(x){
      if (x>0){
        y=30
      }else{y=0}
      return(y)
    }
    
    time$minutes2<-lapply(time$minutes, Minutes)
    
    #creates a new time feature (hour: miutes)
    time$minutes <-time$minutes2
    time$minutes<- sprintf("%02d", as.numeric(time$minutes))
    
    time$minutes2<-NULL
    
    time$time<-paste(time$Hours,time$minutes, sep=":")
    time$Hours <- NULL
    
    #creates the date as an date object
    time$date <-as.Date(ISOdate(time$Year, time$Month, time$Day ))
    class(time$date)
    
    time$minutes <- NULL
    time$datetime <-paste(time$date,time$time)
    time$time <- NULL
    return(time)
    rm(ly, nly, time)
  }
  
  
  timestamp <- sample.timestamp(startyear, stopyear)

  
  # create a timestamp that matches the TOA file:
  timestamp$Hour2 <- as.numeric(timestamp$Hour2 )
  timestamp$Hour3 <- timestamp$Hour2/100
  library(splitstackshape)
  timestamp <-cSplit(timestamp, 9, sep="." )
  timestamp$Hour3_2[is.na(timestamp$Hour3_2)] <- 0
  timestamp$Hour3_2 <-  timestamp$Hour3_2 * 10
  timestamp$time <- paste(timestamp$Hour3_1, timestamp$Hour3_2 , sep=":")
  timestamp$DateTime <- ymd_hm(paste(timestamp$date, timestamp$time, sep=" "))
  timestamp <- timestamp[ ,c("DateTime", "datetime")]
  
   # Merge timestamp with met file:
  toa.file <- merge(timestamp, toa.file, by.x= 'DateTime', by.y= 'DateTime', all.x=TRUE, all.y=FALSE)
  
  #remove duplicate rows based on time stamp
  toa.file <- toa.file[!duplicated(toa.file$DateTime),]
  
  return(toa.file)
}

met.data <- function(x) {
  x <- met.na(x)
  # SolarRAD
  x$SolarRad_AVG[x$SolarRad_AVG < 0 ]<- NA
  x$SolarRad_AVG[x$SolarRad_AVG > 1100 ]<- NA
  
  # PAR
  x$PAR_AVG[x$PAR_AVG < 0 ]<- NA
  
  # Clean up PAR:
  # Sunrise and set times:
  sun <- read.csv( "~/Dropbox (YSE)/Research/Flux Towers/SunRiseSet/SunRiseSet_Time_MIAMI.csv")
  
  sun$Day <- as.numeric(sun$Day)
  sun$Month <- as.numeric(sun$Month)
  sun$Rise2 <- format(as.POSIXct(sun$Rise, format="%H:%M",  tz = "EST") , format="%H:%M")
  sun$Set <- format(as.POSIXct(sun$Set, format="%H:%M",  tz = "EST"), format="%H:%M")
  
  x$Day <- as.numeric(format( x$DateTime, format="%d")) 
  x$Month <- as.numeric(format( x$DateTime, format="%m"))
  x <- merge( sun, x, all=T, by=c("Day", "Month"))
  
  # Correction to timestamp for 2017:'
  x <- x[which(!is.na(x$DateTime)),] 
  
  x$Rise <- format(as.POSIXct(x$Rise, format="%H:%M",  tz = "EST") , format="%H:%M")
  x$Set <- format(as.POSIXct(x$Set, format="%H:%M",  tz = "EST") , format="%H:%M")
  x$time <- format(as.POSIXct(x$DateTime, format="%H:%M",  tz = "EST") , format="%H:%M")
  
  # Filter out obviously erroneous par:
  x$err.rise <- as.POSIXct(as.POSIXct(x$Rise, format="%H:%M" ,  tz = "EST") - (3600*2), format="%H:%M",  tz = "EST")
  x$PAR_AVG[ as.POSIXct(x$time, format="%H:%M",  tz = "EST") < as.POSIXct(x$err.rise, format="%H:%M",  tz = "EST")] <-0
  x$err.set <- as.POSIXct(as.POSIXct(x$Set, format="%H:%M",  tz = "EST") + (3600*2), format="%H:%M",  tz = "EST")
  x$PAR_AVG[ as.POSIXct(x$time, format="%H:%M",  tz = "EST") > as.POSIXct(x$err.set, format="%H:%M",  tz = "EST")] <-0
  
  # Filter LOW PAR VALUES EARLY IN MORNING:
  x$PAR_AVG[x$time <  x$Rise & x$PAR_AVG  > 0] <- 0
  
  # Filter LOW PAR VALUES EARLY in Evening:
  x$PAR_AVG[x$time >  x$Set & x$PAR_AVG  > 0] <- 0
  
  # Filter Midday Par == 0:
  x$PAR_AVG[x$time >=  x$Rise & x$time <=  x$Set & x$PAR_AVG == 0] <- NA
  
  x$err.rise <- x$err.set <-x$Rise <- x$Set <- x$time <-  x$Day <-x$Month <- x$Rise2 <- NULL
  
  #Radiation
  try(lm.srad.x <- lm(x$SolarRad_AVG ~ x$PAR_AVG), silent=TRUE)
  try( x.rad <- as.data.frame(predict.lm(lm.srad.x, x, interval="confidence")), silent=TRUE)
  try(x$SolarRad <- x$SolarRad_AVG, silent=TRUE)
  try(x$SolarRad[x$SolarRad < (x.rad$lwr- x.rad$lwr*0.2)] <- NA, silent=TRUE)
  x$NETShort_AVG[x$NETShort_AVG < -10] <- NA
  x$NetLong_AVG[x$NetLong_AVG < -200] <- NA
  x$NetLong_AVG[x$NetLong_AVG > 0] <- NA
  x$Netall_AVG[x$Netall_AVG < -200] <- NA
  
  x$T_ref_AVG[x$T_ref_AVG < -10] <- NA
  x$Ts_mean[x$Ts_mean < -10]<- NA
  x$Ts_mean[is.na(x$Ts_mean)] <- x$T_ref_AVG[is.na(x$Ts_mean)] 
  x$pressure[x$pressure < 1000] <- NA
  x$press_mean[x$press_mean > 104] <- NA
  x$press_mean[is.na(x$press_mean)] <- x$pressure[is.na(x$press_mean)] /10
  x$TIMESTAMP<-x$Date <- x$date<- x$Hour2<- x$Year<- x$Month<- x$Day<- x$Hour<- x$Doy<- x$Year_RTM<- x$Day_RTM<- x$Hour_Minute_RTM <- NULL
  
  x <- x[which(!is.na(x$datetime)),]
  return(x)
}

rh_vpd <- function(x){
  
  x$h2o_Avg[x$h2o_Avg > 30]<- NA # Upper filter for H2O_Air
  x$h2o_Avg[x$h2o_Avg < 0 ]<- NA # Lower filter for H2O_Air
  
  x$SVD=6.335+(0.6718*x$Ts_mean)-(2.0887*10^-2*(x$Ts_mean^2))+(7.3095*10^-4*(x$Ts_mean^3))
  x$RH=(x$h2o_Avg/ x$SVD)*100
  x$SVP= 0.611*exp((17.502* x$Ts_mean)/(x$Ts_mean +240.97))
  x$AVP = (x$RH *x$SVP)/100
  x$VPD= (x$SVP - x$AVP)
  x$SVD <- x$SVP <- x$AVP <- NULL
  
  x$RH[x$RH > 99.99]<- 99.99
  x$RH[x$RH < 0]<- 0
  x$VPD[x$VPD < 0]<- 0
  return(x)
}

fill.met <- function (fill.data, site.data){
  
  original <- site.data 
  
  # rename fill data variables:
  fill.data <- fill.data[,c("TIMESTAMP", "PAR", "Rnet", "TA", "RH")]
  names(fill.data) <- c( 'TIMESTAMP' , "PAR.fill.data","Rnet.fill.data", "TA.fill.data", "RH.fill.data" )
  
  fill.data$TIMESTAMP <- as.character(  fill.data$TIMESTAMP)
  site.data$TIMESTAMP <- as.character(  site.data$TIMESTAMP)
  
  # Merge site data and fill data variables together:
  fill <- merge(fill.data, site.data, by='TIMESTAMP', all=T) # merge the two files
  
  
  # Use same site reflectance data to fill missing PAR and Netall:
  library(zoo)
  
  # Filling Rnet From the other site
  try( x.par.Rnet.lm <- lm( fill$Rnet ~ fill$Rnet.fill.data), silent=T)
  try(fill$x.Rnet.Pred <- predict(x.par.Rnet.lm, fill ), silent=T)
  try(fill$Rnet.f <- fill$Rnet, silent=T)
  try( fill$Rnet.f[is.na(fill$Rnet)] <- fill$x.Rnet.Pred[is.na(fill$Rnet)], silent=T)
  fill$Rnet.f[fill$Rnet.f < min( fill$Rnet, na.rm=T)] <- NA
  try(fill$Rnet.f <- na.approx(fill$Rnet.f, maxgap=8 , na.rm=F), silent=T)
  #print('Rnet:'); summary( fill$Rnet.f)

  
  # Filling PAR from the other site:
  try(fill$PAR.f <- fill$PAR , silent=T)
  try(fill$PAR.f[is.na(fill$PAR.f)] <- fill$PAR.fill.data[is.na(fill$PAR.f)]  , silent=T)
  try(fill$PAR.f <- na.approx(fill$PAR.f, maxgap=8 , na.rm=F), silent=T)
  fill$PAR.f[fill$PAR.f < 0] <- NA
 # print('PAR:'); summary(fill$PAR.f )
  
  # Filling PAR from the other again:
  try(fill$PAR.f[is.na(fill$PAR.f)] <- fill$PAR.fill.data[is.na(fill$PAR.f)]  , silent=T)
  try(fill$PAR.f <- na.approx(fill$PAR.f, maxgap=8 , na.rm=F), silent=T)
  #print('PAR:'); summary(fill$PAR.f )
  
  # Filling Rnet from PAR:
  try( x.par.Rnet.lm <- lm( fill$Rnet.f ~ fill$PAR.f), silent=T)
  try(fill$x.Rnet.Pred <- predict(x.par.Rnet.lm, fill ), silent=T)
  try(fill$Rnet.f[is.na(fill$Rnet.f)] <- x.Rnet.Pred[is.na(fill$Rnet.f)], silent=T)
  try(fill$Rnet.f <- na.approx(fill$Rnet.f, maxgap=8 , na.rm=F), silent=T)
  
  #Temperature:
  fill$TA.f <- fill$TA
  try(TA.lm <- lm( fill$TA.f ~ fill$TA.fill.data), silent=T)
  try(fill$TA.Pred <- predict(TA.lm , fill ) , silent=T)
  try(fill$TA.f[is.na(fill$TA.f)] <- fill$TA.Pred[is.na(fill$TA.f)], silent=T)

  # RH:
  fill$RH.f <- fill$RH
  try(RH.lm <- lm( fill$RH.f ~ fill$RH.fill.data), silent=T)
  try(fill$RH.Pred <- predict(RH.lm , fill ) , silent=T)
  try(fill$RH.f[is.na(fill$RH.f)] <- fill$RH.Pred[is.na(fill$RH.f)], silent=T)
  
  fill <- fill[,c('TIMESTAMP' , 'PAR.f', 'Rnet.f', 'TA.f', 'RH.f')]
  names(fill) <- c( 'TIMESTAMP' , 'PAR', 'Rnet', 'TA', "RH")
  
  return(fill)
}