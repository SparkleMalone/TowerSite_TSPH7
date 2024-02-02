# Flux Functions for TS7 (Eddypro)

library(plyr)
library(lubridate)
#library(FREddyPro)
library(zoo)
library(gtools)
library(dplyr)
library(stringr)

ts7.flux.import <- function(file_list){
  
  for (file in file_list){
    print(file)
    # if the merged dataset doesn't exist, create it
    if (!exists("flux_raw")){
      header <- read.csv(file, nrows=1, skip=1)
      flux_raw <- read.csv(file, header=FALSE, skip=3, stringsAsFactors=FALSE)
      colnames( flux_raw ) <- colnames(header)
      rm(header)
      
    }else{
      #if the merged dataset does exist, append to it
      
      if (exists("flux_raw")){
        temp_dataset <-read.csv(file, header=FALSE, skip=3, stringsAsFactors=FALSE)
        header <- read.csv(file, nrows=1, skip=1)
        colnames(temp_dataset) <- colnames(header)
        flux_raw <-smartbind(flux_raw, temp_dataset)
        rm(temp_dataset, header)
      }}}
  
  flux <- flux_raw
  rm(flux_raw)
  return(flux)
}

append.Rda <- function(x, file) {
  old.objects <- load(file, new.env())
  save(list = c(old.objects, deparse(substitute(x))), file = file)
}

Gapfill.parms <- function(x){

  # Sorts the file by TIMESTAMP:
  x <- x[ order(x$TIMESTAMP , decreasing = F ),]
  
  # Formats week, Month and Year:
  x$Month <- as.numeric(format(x$TIMESTAMP, format="%m"))
  x$Year <- as.numeric(format(x$TIMESTAMP, format="%Y"))
  x$Week <- week(x$TIMESTAMP)
  
  # Add seasonal indicator:
  x$Season <- NA
  x$Season[as.numeric(x$Month) < 05 | as.numeric(x$Month) > 10] <- 1
  x$Season[as.numeric(x$Month) >= 5 & as.numeric(x$Month) <= 10] <- 2
  
  # Creates Day and Night files:
  x$PAR <- x$PAR.f
  x$TA <- x$TA.f
  day <- x[which(x$PAR > 0 ),]
  night <-x[which(x$PAR <= 0),]
  
  #_________________________________________________________________________________
  #_________________________________________________________________________________
  #_________________________________________________________________________________
  
  # This sections creates the parms.NEE file:
  # First a time file is created that should contain all month-year combinations:
  
  # Creates a dataframe to store model parameters in:
  parms.NEE <- data.frame(
    Year=numeric(),
    Week=numeric(),
    Month=numeric(),
    a1=numeric(),
    ax=numeric(),
    r=numeric(),
    a1.pvalue=numeric(),
    ax.pvalue=numeric(),
    r.pvalue=numeric(),
    a=numeric(),
    b=numeric(), 
    a.pvalue=numeric(),
    b.pvalue=numeric(), stringsAsFactors=FALSE, row.names=NULL)
  
  # Creates time file to merge with parm file:
  time <-  x[,c("Year", "Week", "Month")]
  time <- time[!duplicated( time[ ,c( "Year", "Week", "Month")]),]
  
  time$Season <- NA # Adds Season to the file:
  time$Season[as.numeric(time$Month) < 05 | as.numeric(time$Month) > 10] <- 1
  time$Season[as.numeric(time$Month) >= 5 & as.numeric(time$Month) <= 10] <- 2
  
  parms.NEE <- merge(time, parms.NEE, all=T) # merge parms file with time file 
  
  #_________________________________________________________________________________
  #_________________________________________________________________________________
  #_________________________________________________________________________________
  
  # This section creates the day and night functions that will be used to gapfill the file:
  # This section can be adjusted by changing the starting values of both day and night functions.
  
  nee.day <- function(dataframe){ y = nls( nee ~ (a1 * PAR * ax)/(a1 * PAR + ax) + r, dataframe, 
                                           start=list(a1= -0.1 , ax= -7, r= 2),
                                           upper=list(a1= 0, ax= -1, r= 10),
                                           na.action=na.exclude, trace=F, control=nls.control(warnOnly=T))
  
  
  
  
  y.df <- as.data.frame(cbind(t(coef(summary(y)) [1:3, 1]), t(coef(summary(y)) [1:3, 4])))
  names(y.df) <-c("a1","ax", "r", "a1.pvalue", "ax.pvalue", "r.pvalue") 
  return (y.df )}
  
  nee.night <- function(dataframe){y.df = nls(nee ~ a * exp(b*TA), 
                                              dataframe, start=list(a= 1 , b=0.01 ),
                                              upper=list(a= 5 , b=1),
                                              na.action=na.exclude, trace=F,
                                              
                                              
                                              control=nls.control(warnOnly=T))
  
  y.df <- as.data.frame(cbind(t(coef(summary(y.df))[1:2, 1]), t(coef(summary(y.df)) [1:2, 4])))
  
  names(y.df) <- c("a", "b", "a.pvalue", "b.pvalue")                      
  return(y.df)}
  
  #_________________________________________________________________________________
  #_________________________________________________________________________________
  #_________________________________________________________________________________
  
  # Functions to filter parameter files by extreme parameter values:
  # These thresholds should be adjusted for each system:
  
  filter.parms.night <- function(parms){
    
    parms$a <- as.numeric(parms$a)
    parms$b <- as.numeric(parms$b)
    parms$a.pvalue <- as.numeric(parms$a.pvalue )
    parms$b.pvalue  <- as.numeric(parms$b.pvalue )
    
    # Filter parms file based on night model:
    parms$a[parms$b > 0.08 ] <- NA
    parms$a[parms$a <= 0.1 ] <- NA
    parms$a[ parms$b < 0] <- NA
    parms$a[parms$a.pvalue =="NaN"] <- NA
    parms$a[parms$b =="NaN"] <- NA
    parms$a[parms$b.pvalue =="NaN"] <- NA
    parms$a.pvalue[is.na(  parms$a)]<- NA
    parms$b[is.na(  parms$a)]<- NA
    parms$b.pvalue[is.na( parms$a)] <- NA
    try( parms$night.model[is.na( parms$a)] <- NA, silent=T)
    return(parms)
  }
  
  filter.parms.day <- function(parms){
    
    parms$a1 <- as.numeric(parms$a1)
    parms$ax <- as.numeric(parms$ax)
    parms$r <- as.numeric(parms$r)
    parms$a1.pvalue <- as.numeric(parms$a1.pvalue)
    parms$ax.pvalue <- as.numeric(parms$ax.pvalue)
    parms$r.pvalue <- as.numeric(parms$r.pvalue)
    
    # Filter parms file based on day model:
    parms$a1[  parms$a1 < -40 | parms$ax < -40 ] <- NA
    parms$a1[  parms$ax > -0.6 ] <- NA
    parms$a1[  parms$a1 > 0 ] <- NA
    parms$a1[  parms$r > 10 | parms$r < 0] <- NA
    parms$ax[is.na(parms$a1) ] <- NA
    parms$r[is.na(parms$a1) ] <- NA
    parms$a1.pvalue[is.na(parms$a1) ] <- NA
    parms$ax.pvalue[is.na(parms$a1) ] <- NA
    parms$r.pvalue[is.na(parms$a1) ] <- NA
    try( parms$day.model[is.na(parms$a1) ] <- NA, silent=T)
    return(parms)
  }
  
  #_________________________________________________________________________________
  #_________________________________________________________________________________
  #_________________________________________________________________________________
  
  # Fits weekely within a year equations and adds paramters to the parms.NEE file:
  
  w.parms <- parms.NEE # Creates a parms file for weekely equations:
  
  # This loop fits Annual-Monthly Models and adds the parms to the parms.NEE file:
  try(for ( i in unique(day$Year)){
    for(j in unique(day$Week)){
      
      y3 <- try(nee.day(day[which(day$Year == i & day$Week == j),]), silent=T) # Fit the day model
      
      y4 <- try(nee.night(night[which(night$Year == i & night$Week == j),]), silent=T) # Fit night model
      
      try(w.parms[c(w.parms$Year == i & w.parms$Week== j ), 5:14 ] <- cbind(y3,y4), silent=T)
      
      rm( y3, y4)
    }
  }, silent=T)
  
  # Filters annual-monthly params file:
  try(w.parms <- filter.parms.night(filter.parms.day(w.parms)), silent=T)
  
  try(w.parms$night.model[is.na(w.parms$a) == FALSE ] <- "w", silent=T) # Add indicator for the model
  try(w.parms$day.model[is.na(w.parms$a1) == FALSE ] <- "w" , silent=T)
  
  # Adds annual-monthly parms to parms.NEE:
  try(parms.NEE <- w.parms, silent=T)
  
  rm(w.parms)
  
  #_________________________________________________________________________________
  #_________________________________________________________________________________
  #_________________________________________________________________________________
  
  # Fits equations for missing Weeks using data from the week of, before, and after: 
  # Day:
  
  try(fill.parms.day <- parms.NEE[which(is.na(parms.NEE$a1)),][, c(1:10, 15)], silent=T) # subset to include only missing data:
  
  
  try(for( i in 1:length(fill.parms.day$Year)){
    
    a = fill.parms.day$Year[i]
    j= as.numeric(fill.parms.day$Week[i])
    j = sprintf( "%02d", j)
    j.1 <-sprintf( "%02d", (as.numeric(j) + 1))
    
    # Correct for the last month in a year:
    if(j.1 == 54 ){
      j.1 <- sprintf( "%02d",1)
      b = as.numeric(a) + 1
    }else{ j.1 <- j.1
    b=a}
    
    j.2 <- as.numeric(j) - 1
    
    y1 <- day[which(day$Year == a & day$Week == j),]
    y1 <- rbind(y1, day[which(day$Year == a & day$Week == j.2),])
    y1 <- rbind(y1, day[which(day$Year == b & day$Week == j.1),])
    
    y3 <- try(nee.day( y1 )) # Fit the day model
    
    try(fill.parms.day[c(fill.parms.day$Year == a & fill.parms.day$Week == j ), 5:10 ] <- cbind(y3), silent=T)
    
    rm( y3, y1, a,j,j.1, j.2, b)
    
  }, silent=T)
  
  #filters the fill.day file:
  
  try(fill.parms.day <- filter.parms.day(fill.parms.day), silent=T)
  
  try(fill.parms.day$day.model[is.na(fill.parms.day$a1) == FALSE ] <- "w2", silent=T) # Add indicator for the model
  try(fill.parms.day <- fill.parms.day[which(is.na(fill.parms.day$a1) == FALSE ),], silent=T) # removes NA
  
  # Adds fill.day data to the parms.NEE file:
  try(for ( i in unique(fill.parms.day$Year)){
    for( j in unique(fill.parms.day$Week)){
      
      parms.NEE[c(parms.NEE$Year == i & parms.NEE$Week == j) , c(5:10, 15) ] <- fill.parms.day[c(fill.parms.day$Year == i & fill.parms.day$Week == j) , c(5:10, 11)]
      
    }}, silent=T)
  
  try(rm(i,fill.parms.day), silent=T)
  
  #________________________________________________
  
  #Night:
  
  try(fill.parms.night <- parms.NEE[which(is.na(parms.NEE$a)),][, -c(5:10, 15)], silent=T) 
  
  try(for ( i in 1:length(fill.parms.night$Year)){
    
    a = fill.parms.night$Year[i]
    j= as.numeric(fill.parms.night$Week[i])
    j = sprintf( "%02d", j)
    j.1 <-sprintf( "%02d", (as.numeric(j) + 1))
    
    if(j.1 == 53 ){
      j.1 <- sprintf( "%02d",1)
      b = as.numeric(a) + 1
    }else{ j.1 <- j.1
    b=a}
    
    j.2 <- as.numeric(j) - 1
    
    y1 <- night[which(night$Year == a & night$Month == j),]
    y1 <- rbind(y1, night[which(night$Year == a & night$Month == j.2),])
    y1 <- rbind(y1, night[which(night$Year == b & night$Month == j.1),])
    
    y3 <- try(nee.night( y1 )) # Fit the day model
    
    try(fill.parms.night[c(fill.parms.night$Year == a & fill.parms.night$Week == j ), 5:8 ]<- cbind(y3), silent=T)
    
    rm( y3, y1, a,j,j.1, j.2, b)# rbind models to parms dataframe
    
  }, silent=T)
  
  #filters the fill.night file:
  try(fill.parms.night <- filter.parms.night(fill.parms.night), silent=T)
  
  try(fill.parms.night$night.model <- NA, silent=T)
  try(fill.parms.night$night.model[is.na(fill.parms.night$a) == FALSE ] <- "w2", silent=T)
  
  try(fill.parms.night <- fill.parms.night[which(is.na(fill.parms.night$a) == FALSE ),], silent=T) # removes NA
  
  # Adds fill.night data to the Parms file:
  try(for ( i in unique(fill.parms.night$Year)){
    for( j in unique(fill.parms.night$Week)){
      
      parms.NEE[c(parms.NEE$Year == i & parms.NEE$Week == j), c(11:14, 16)] <- fill.parms.night[c(fill.parms.night$Year == i & fill.parms.night$Week == j) , 5:9]
      
    }}, silent=T)
  
  try(rm(i,fill.parms.night), silent=T)
  
  #_________________________________________________________________________________
  #_________________________________________________________________________________
  #_________________________________________________________________________________
  #_________________________________________________________________________________
  #_________________________________________________________________________________
  #_________________________________________________________________________________
  
  # Fits monthly foreach year equations and adds paramters to the parms.NEE file:
  # Creates a parms file for annual equations:
  parms <- data.frame(
    Year=numeric(),
    Week=numeric(),
    Month=numeric(),
    Season=numeric(),
    a1=numeric(),
    ax=numeric(),
    r=numeric(),
    a1.pvalue=numeric(),
    ax.pvalue=numeric(),
    r.pvalue=numeric(),
    a=numeric(),
    b=numeric(), 
    a.pvalue=numeric(),
    b.pvalue=numeric(), 
    day.model= character(),
    night.model=character(),
    stringsAsFactors=FALSE, row.names=NULL)
  
  time <-  x[,c("Year", "Month")]
  time <- time[!duplicated( time[ ,c( "Year", "Month")]),]
  
  m.parms <- merge(time, parms, all=T)
  
  # This loop fits Annual-Monthly Models and adds the parms to the parms.NEE file:
  try(for ( i in unique(day$Year)){
    for(j in unique(day$Month)){
      
      y3 <- try(nee.day(day[which(day$Year == i & day$Month == j),]), silent=T) # Fit the day model
      
      y4 <- try(nee.night(night[which(night$Year == i & night$Month == j),]), silent=T) # Fit night model
      
      try(m.parms[c(m.parms$Year == i & m.parms$Month == j ), 5:14 ] <- cbind(y3,y4), silent=T)
      
      rm( y3, y4)
    }
  }, silent=T)
  
  # Filters annual-monthly params file:
  try(m.parms <- filter.parms.night(filter.parms.day(m.parms)), silent=T)
  
  try(m.parms$night.model[is.na(m.parms$a) == FALSE ] <- "m", silent=T) # Add indicator for the model
  try(m.parms$day.model[is.na(m.parms$a1) == FALSE ] <- "m" , silent=T)
  
  # Adds annual-monthly parms to parms.NEE:
  try(for ( i in 1:length(parms.NEE$Month)) {
    y = parms.NEE$Year[i]
    m = parms.NEE$Month[i]
    if(is.na(parms.NEE$a1[i]) == TRUE) {
      parms.NEE[ i, c(5:10, 15)]  <- m.parms[c(m.parms$Year == y & m.parms$Month == m), c(5:10, 15)] } 
  }, silent=T)
  
  try(for ( i in 1:length(parms.NEE$Month)) {
    y = parms.NEE$Year[i]
    m = parms.NEE$Month[i]
    if(is.na(parms.NEE$a[i]) == TRUE) {
      parms.NEE[ i, c(11:14, 16)]  <- m.parms[c(m.parms$Year == y & m.parms$Month == m), c(11:14, 16)] }
  }, silent=T)
  
  
  rm(m.parms)
  
  #_________________________________________________________________________________
  #_________________________________________________________________________________
  #_________________________________________________________________________________
  
  # Fits equations for missing Months using data from the month of, month before, and the month after: 
  # Day:
  
  parms <- data.frame(
    Year=numeric(),
    Week=numeric(),
    Month=numeric(),
    Season=numeric(),
    a1=numeric(),
    ax=numeric(),
    r=numeric(),
    a1.pvalue=numeric(),
    ax.pvalue=numeric(),
    r.pvalue=numeric(),
    a=numeric(),
    b=numeric(), 
    a.pvalue=numeric(),
    b.pvalue=numeric(), 
    day.model= character(),
    night.model=character(),
    stringsAsFactors=FALSE, row.names=NULL)
  
  class(parms.NEE$day.model)
  time <-  x[,c("Year", "Month")]
  time <- time[!duplicated( time[,c( "Year", "Month")]),]
  
  m.parms <- merge(time, parms, all=T)
  
  try(fill.parms.day <- m.parms[, c(1:10, 15)], silent=T)
  
  try(for( i in 1:length(fill.parms.day$Year)){
    
    a = fill.parms.day$Year[i]
    j= as.numeric(fill.parms.day$Month[i])
    j = sprintf( "%02d", j)
    j.1 <-sprintf( "%02d", (as.numeric(j) + 1))
    
    if(j.1 == 13 ){
      j.1 <- sprintf( "%02d",1)
      b = as.numeric(a) + 1
    }else{ j.1 <- j.1
    b=a}
    
    j.2 <- as.numeric(j) - 1
    
    y1 <- day[which(day$Year == a & day$Month == j),]
    y1 <- rbind(y1, day[which(day$Year == a & day$Month == j.2),])
    y1 <- rbind(y1, day[which(day$Year == b & day$Month == j.1),])
    
    y3 <- try(nee.day( y1 )) # Fit the day model
    
    try(fill.parms.day[c(fill.parms.day$Year == a & fill.parms.day$Month == j ), 5:10 ] <- cbind(y3), silent=T)
    
    rm( y3, y1, a,j,j.1, j.2, b)
    
  }, silent=T)
  
  #filters the fill.day file:
  try(fill.parms.day <- filter.parms.day(fill.parms.day), silent=T)
  
  try(fill.parms.day <- fill.parms.day[which(is.na(fill.parms.day$a1) == FALSE ),], silent=T) # removes NA
  try(fill.parms.day$day.model <- "m2", silent=T) # Add indicator for the model
  
  # Adds fill.day data to the parms.NEE file:
  try(for ( i in 1:length(fill.parms.day$Year)){
    parms.NEE[c(parms.NEE$Year == fill.parms.day$Year[i] & parms.NEE$Month == fill.parms.day$Month[i]) , 5:10] <- fill.parms.day[i , 5:10]
    parms.NEE$day.model[c(parms.NEE$Year == fill.parms.day$Year[i] & parms.NEE$Month == fill.parms.day$Month[i])] <- "m2"
    
  }, silent=T)
  
  try(rm(i,fill.parms.day), silent=T)
  
  #________________________________________________
  
  #Night:
  
  try(fill.parms.night <- m.parms[, -c(5:10, 15)], silent=T) 
  
  try(for ( i in 1:length(fill.parms.night$Year)){
    
    a = fill.parms.night$Year[i]
    j= as.numeric(fill.parms.night$Month[i])
    j = sprintf( "%02d", j)
    j.1 <-sprintf( "%02d", (as.numeric(j) + 1))
    
    if(j.1 == 13 ){
      j.1 <- sprintf( "%02d",1)
      b = as.numeric(a) + 1
    }else{ j.1 <- j.1
    b=a}
    
    j.2 <- as.numeric(j) - 1
    
    y1 <- night[which(night$Year == a & night$Month == j),]
    y1 <- rbind(y1, night[which(night$Year == a & night$Month == j.2),])
    y1 <- rbind(y1, night[which(night$Year == b & night$Month == j.1),])
    
    y3 <- try(nee.night( y1 )) # Fit the day model
    
    try(fill.parms.night[c(fill.parms.night$Year == a & fill.parms.night$Month == j ), 5:8 ]<- cbind(y3), silent=T)
    
    rm( y3, y1, a,j,j.1, j.2, b)# rbind models to parms dataframe
    
  }, silent=T)
  
  
  try(fill.parms.night <- filter.parms.night(fill.parms.night), silent=T) #filters the fill.night file:
  
  try(fill.parms.night$night.model <- NA, silent=T)
  try(fill.parms.night <- fill.parms.night[which(is.na(fill.parms.night$a) == FALSE ),], silent=T) # removes NA
  try(fill.parms.night$night.model <- "m2", silent=T)
  
  # Adds fill.night data to the Parms file:
  try(for ( i in 1:length(fill.parms.night$Year)){
    
    parms.NEE[c(parms.NEE$Year == fill.parms.night$Year[i] & parms.NEE$Month == fill.parms.night$Month[i]) , c(11:14, 16)] <- fill.parms.night[i , 5:9]
    parms.NEE$night.model[c(parms.NEE$Year == fill.parms.night$Year[i] & parms.NEE$Month == fill.parms.night$Month[i])] <- "m2"
    
  }, silent=T)
  
  try(rm(i,fill.parms.night), silent=T)
  
  #________________________________________________
  #________________________________________________
  #________________________________________________
  
  # Seasonal Models:
  # This loop fits annual models!
  seasonal.parms.NEE <- data.frame(
    Year=numeric(),
    Month=numeric(),
    a1=numeric(),
    ax=numeric(),
    r=numeric(),
    a1.pvalue=numeric(),
    ax.pvalue=numeric(),
    r.pvalue=numeric(),
    a=numeric(),
    b=numeric(), 
    a.pvalue=numeric(),
    b.pvalue=numeric(),
    Season=numeric(),
    stringsAsFactors=FALSE, row.names=NULL)
  
  try(for ( i in unique(day$Year)){
    for ( s in unique(day$Season)){
      
      print(i)
      print(s)
      
      try( y1 <- nee.day(day[which(day$Year == i & day$Season == s),]), silent=T) # Fit the day model
      
      try(print(y1), silent=T)
      
      if(exists("y1") ){ 
        
        try(y1$Year <- i , silent=T)# Add the year to the file
        try(y1$Season <- s, silent=T)# Add the month to the file
        
        try( y2 <- nee.night(night[which(night$Year == i & night$Season == s),]), silent=T) # Fit night model
        
        
        try(seasonal.parms.NEE <- rbind(seasonal.parms.NEE, cbind(y1,y2)), silent=T) # rbind models to parms dataframe
      }
      
    }}, silent=T) # Adds annual models to parm file
  
  try(rm( y1, y2, i, s), silent=T) 
  
  # Filter Seasonal Data:
  # Filter parms file based on night model:
  
  # Filter parms file based on day model:
  try(seasonal.parms.NEE <- filter.parms.day(seasonal.parms.NEE ), silent=T)
  try(seasonal.parms.NEE <- filter.parms.night(seasonal.parms.NEE ), silent=T)
  
  try(seasonal.parms.NEE$day.model <- NA, silent=T)
  try( seasonal.parms.NEE$day.model[is.na(seasonal.parms.NEE$a1) == FALSE ] <- "s", silent=T)
  
  try(seasonal.parms.NEE$night.model <- NA, silent=T)
  try(seasonal.parms.NEE$night.model[is.na(seasonal.parms.NEE$a) == FALSE ] <- "s", silent=T)
  
  
  # Adds seasonal models to the parm file:
  try(for( i in 1:length(parms.NEE$a1)){
    if(is.na(parms.NEE$a1[i]) == "TRUE"){
      try(parms.NEE[ i, 5:10] <- seasonal.parms.NEE[c(seasonal.parms.NEE$Year == parms.NEE$Year[i] & seasonal.parms.NEE$Season == parms.NEE$Season[i]) , 1:6], silent = T)
      parms.NEE$day.model[i] <- "s"
    }}, silent=T)
  
  try(for( i in 1:length(parms.NEE$a)){
    if(is.na(parms.NEE$a[i]) == "TRUE"){
      try(parms.NEE[ i, 11:14] <- seasonal.parms.NEE[c(seasonal.parms.NEE$Year == parms.NEE$Year[i] & seasonal.parms.NEE$Season == parms.NEE$Season[i]) , 9:12], silent = T)
      parms.NEE$night.model[i] <- "s"
    }}, silent=T)
  
  try(parms.NEE$day.model[is.na(parms.NEE$a1)] <- NA, silent=T)
  try(parms.NEE$night.model[is.na(parms.NEE$a)] <- NA, silent=T)
  
  try( rm(seasonal.parms.NEE ), silent=T)
  
  #________________________________________________
  #________________________________________________
  #________________________________________________
  
  # Annual Function:
  annual.parms.NEE <- data.frame(
    Year=numeric(),
    Month=numeric(),
    a1=numeric(),
    ax=numeric(),
    r=numeric(),
    a1.pvalue=numeric(),
    ax.pvalue=numeric(),
    r.pvalue=numeric(),
    a=numeric(),
    b=numeric(), 
    a.pvalue=numeric(),
    b.pvalue=numeric(), stringsAsFactors=FALSE, row.names=NULL)
  
  try(for ( i in unique(day$Year)){
    
    y3 <- try(nee.day(day[which(day$Year == i),]), silent=T) # Fit the day model
    
    try(y3$Year <- i, silent=T)
    
    y4 <- try(nee.night(night[which(night$Year == i ),]), silent=T) # Fit night model
    
    try(annual.parms.NEE <- rbind(annual.parms.NEE, cbind(y3,y4)), silent=T)
    
    try(rm( y3, y4, i) , silent=T)# rbind models to parms dataframe
  }, silent=T)
  
  # Add model indicator:
  try(annual.parms.NEE$day.model <- "a", silent=T) # Add indicator for th
  try(annual.parms.NEE$night.model <- "a", silent=T) # Add indicator for the model:
  
  # Filter parms file based on day and night model:
  
 try( annual.parms.NEE <- filter.parms.day(annual.parms.NEE ) , silent=T)
  try( annual.parms.NEE <- filter.parms.night(annual.parms.NEE ), silent = T)
  
  # Adds annual models to the parm file:
  try(for( i in 1:length(parms.NEE$a1)){
    if(is.na(parms.NEE$a1[i]) == "TRUE"){
      try(parms.NEE[ i, c(5:10, 15)] <- annual.parms.NEE[c(annual.parms.NEE$Year == parms.NEE$Year[i]) , c(1:6, 12)], silent = T)
    }}, silent=T)
  
  try(for( i in 1:length(parms.NEE$a)){
    if(is.na(parms.NEE$a[i]) == "TRUE"){
      try(parms.NEE[ i, c(11:14, 16)] <- annual.parms.NEE[c(annual.parms.NEE$Year == parms.NEE$Year[i] ) , c(8:11,13)], silent = T)
    }}, silent=T)
  
  try(rm(annual.parms.NEE), silent=T)
  #________________________________________________________________________________________________________
  #________________________________________________________________________________________________________
  #________________________________________________________________________________________________________
  
  # A global Seasonal Model!
  global.parms.NEE <- data.frame(
    Season=numeric(),
    a1=numeric(),
    ax=numeric(),
    r=numeric(),
    a1.pvalue=numeric(),
    ax.pvalue=numeric(),
    r.pvalue=numeric(),
    a=numeric(),
    b=numeric(), 
    a.pvalue=numeric(),
    b.pvalue=numeric(), stringsAsFactors=FALSE, row.names=NULL)
  
  # Day model:
  try (for ( s in unique(day$Season)){
    y1 <- try(nee.day(day[which(day$Season == s),]), silent=T) # Fit the day model
    global.parms.NEE[s, 2:7] <- y1 # rbind models to parms dataframe
    global.parms.NEE$Season[s] <- s #
    rm(y1)
  }, silent=T)
  
  try (for ( s in unique(day$Season)){
    y1 <- data.frame(try(nee.night(night[which(night$Season == s),]), silent=T)) # Fit night model
    global.parms.NEE[s, 8:11] <- y1 # rbind models to parms dataframe
    rm(y1)
  }, silent=T)
  
  try(global.parms.NEE$day.model <- "g", silent=T) # Add indicator for the model:  
  try(global.parms.NEE$night.model <- "g", silent=T) # Add indicator for the model:
  
  # Filter parms file based on day and night model:
  
  try(global.parms.NEE<- filter.parms.day(global.parms.NEE ), silent=T)
  try( global.parms.NEE <- filter.parms.night(global.parms.NEE ), silent=T)
  
  
  # Adds annual models to the parm file:
  
  try(for( i in 1:length(parms.NEE$a1)){
    
    if(is.na(parms.NEE$a1[i]) == "TRUE"){
      
      try(parms.NEE[ i, c(5:10, 15)] <- global.parms.NEE[c(2:7, 12)], silent = T)
    }}, silent=T)
  
  try(for( i in 1:length(parms.NEE$a)){
    if(is.na(parms.NEE$a[i]) == "TRUE"){
      try(parms.NEE[ i, c(11:14, 16)] <- global.parms.NEE[c(8:11, 13)], silent = T)
    }}, silent=T)
  
  try(rm(global.parms.NEE, time), silent=T)
  
  
  #________________________________________________________________________________________________________
  #________________________________________________________________________________________________________
  #________________________________________________________________________________________________________
  
  # A global Annual Model!
  
  global.parms.NEE <- data.frame(
    a1=numeric(),
    ax=numeric(),
    r=numeric(),
    a1.pvalue=numeric(),
    ax.pvalue=numeric(),
    r.pvalue=numeric(),
    a=numeric(),
    b=numeric(), 
    a.pvalue=numeric(),
    b.pvalue=numeric(), stringsAsFactors=FALSE, row.names=NULL)
  
  try( y1 <- nee.day(day), silent=T) # Fit the day model
  
  try(y2 <- nee.night(night), silent=T) # Fit night model
  
  try(global.parms.NEE <- rbind(global.parms.NEE, cbind(y1,y2)), silent=T)
  
  try(global.parms.NEE$day.model <- "g2", silent=T) # Add indicator for the model:  
  try(global.parms.NEE$night.model <- "g2", silent=T) # Add indicator for the model:
  
  
  # Adds annual models to the parm file:
  
  try(for( i in 1:length(parms.NEE$a1)){
    
    if(is.na(parms.NEE$a1[i]) == "TRUE"){
      
      try(parms.NEE[ i, c(5:10, 15)] <- global.parms.NEE[c(1:6, 11)], silent = T)
    }}, silent=T)
  
  try(for( i in 1:length(parms.NEE$a)){
    if(is.na(parms.NEE$a[i]) == "TRUE"){
      try(parms.NEE[ i, c(11:14, 16)] <- global.parms.NEE[c(7:10, 12)], silent = T)
    }}, silent=T)
  
  try(rm(global.parms.NEE), silent=T)
  #________________________________________________________________________________________________________
  #________________________________________________________________________________________________________
  #________________________________________________________________________________________________________
  
  return(parms.NEE)
} # Creates parameter file for NEE

Gapfill.NEE <- function(x, parms.NEE){
  
  # Formats Month and Year:
  x$Month <- as.numeric(format(x$TIMESTAMP, format="%m"))
  x$Year <- as.numeric(format(x$TIMESTAMP, format="%Y"))
  try(x$Week <- week(x$TIMESTAMP), silent=T)
  
  
  # Sorts the file by TIMESTAMP:
  x <- x[ order(x$TIMESTAMP , decreasing = F ),]
  
  # Add seasonal indicator:
  x$Season[as.numeric(x$Month) < 05 | as.numeric(x$Month) > 10] <- 1
  x$Season[as.numeric(x$Month) >= 5 & as.numeric(x$Month) <= 10] <- 2
  
  # Remove No PAR DATA:
  x$PAR <- x$PAR.f
  x$TA <- x$TA.f
  x$Netall <- x$Rnet.f
  x$NEE.filtered <- x$nee
  
  x.out <- x[which(is.na(x$PAR) | is.na(x$Netall) | is.na(x$TA) ),]
  x <- x[ which(!is.na(x$PAR) & !is.na(x$Netall) & !is.na(x$TA)),]
  
  # Creates Day and Night files:
  day <- x[which(x$PAR > 0 ),]
  night <-x[which(x$PAR <= 0),]

  # Write functions to estimate NEE from parameter values for day:
  nee.day.model <- function(PAR, a,b,c){
    
    if(is.na(PAR)){ y = NA
    }else{
      y = (a*PAR*b)/(a*PAR +b) + c 
    }
    return(y)}
  
  # Write function to estimate NEE from models for night:
  nee.night.model <- function(TA, a, b){
    if(is.na(TA)){ y = NA
    }else{
      y = a * exp(b*TA)
    }
    return(y)}
  
  for ( i in unique(x$Year)){
    for(j in unique(na.omit(x$Week))){
      
      try(day$modeled.nee[day$Year == i & day$Week == j ] <- mapply(nee.day.model, day$PAR[day$Year == i & day$Week == j],
                                                                    parms.NEE[which(parms.NEE$Year == i & parms.NEE$Week == j),][1,][,"a1"],
                                                                    parms.NEE[which(parms.NEE$Year == i & parms.NEE$Week == j),][1,][,"ax"],
                                                                    parms.NEE[which(parms.NEE$Year == i & parms.NEE$Week == j),][1,][,"r"]), silent=T)
      
    
      try(night$modeled.nee[night$Year == i & night$Week == j ] <- mapply(nee.night.model,
                                                                          night$TA[night$Year == i & night$Week == j],
                                                                          parms.NEE[which(parms.NEE$Year == i & parms.NEE$Week == j),][1,][,"a"],
                                                                          parms.NEE[which(parms.NEE$Year == i & parms.NEE$Week == j),][1,][,"b"]), silent=T)
      
      try(night$modeled.reco[night$Year == i & night$Week == j ] <- mapply(nee.night.model,
                                                                           night$TA[night$Year == i & night$Week == j],
                                                                           parms.NEE[which(parms.NEE$Year == i & parms.NEE$Week == j),][1,][,"a"],
                                                                           parms.NEE[which(parms.NEE$Year == i & parms.NEE$Week == j),][1,][,"b"]), silent=T)
      
      
      try(day$modeled.reco[day$Year == i & day$Week == j ] <- mapply(nee.night.model,
                                                                     day$TA[day$Year == i & day$Week == j],
                                                                     parms.NEE[which(parms.NEE$Year == i & parms.NEE$Week == j),][1,][,"a"],
                                                                     parms.NEE[which(parms.NEE$Year == i & parms.NEE$Week == j),][1,][,"b"]), silent=T)
    }}
  
  # Keep track of the model used:
  model <- parms.NEE[, c("Year", "Month", "day.model", "night.model")]
  night <- merge(night, model, by= c("Year", "Month"), allow.cartesian=TRUE)
  day <- merge(day, model, by= c("Year", "Month"), allow.cartesian=TRUE)
  
  # Remove Duplicates :
  night <- night[!duplicated(night$TIMESTAMP),]
  day <- day[!duplicated(day$TIMESTAMP),]
  
  # Merge Day and night dataframes:
  gapfilled <- smartbind(day, night)
  
  # Sorts the file by TIMESTAMP:
  gapfilled <- gapfilled[ order(gapfilled$TIMESTAMP , decreasing = F ),]
  gapfilled$nee.umol <- gapfilled$NEE.filtered
  gapfilled$nee.umol[is.na(gapfilled$nee.umol)] <- gapfilled$modeled.nee[is.na(gapfilled$nee.umol)]
  
  # Ecosystem Respiration:
  # reco night 
  
  gapfilled$reco.umol <- gapfilled$NEE.filtered # Add NEE to have unfilitered night data
  gapfilled$reco.umol[gapfilled$PAR > 0] <- NA # remove daytime unfiltered data
  gapfilled$reco.umol[gapfilled$PAR <= 0 & is.na(gapfilled$reco.umol)] <- gapfilled$modeled.reco[gapfilled$PAR <= 0 & is.na(gapfilled$reco.umol) ] # fill all missing night time data with modeled reco
  gapfilled$reco.umol[gapfilled$PAR > 0 ] <- gapfilled$modeled.reco[gapfilled$PAR > 0 ] # fill all daytime data with modeled reco
  gapfilled$reco.umol[!is.na(gapfilled$reco.umol) & gapfilled$nee.umol >  gapfilled$reco.umol] <-  gapfilled$nee.umol[!is.na(gapfilled$reco.umol) & gapfilled$nee.umol >  gapfilled$reco.umol] # use modeled nee.umol when ever nee is greater than reco * tends to be an issue early in the da
  
  # GEE
  gapfilled$gee.umol <- gapfilled$nee.umol - gapfilled$reco.umol
  
  class( gapfilled$TIMESTAMP)
  gapfilled$TIMESTAMP <- as.POSIXct(gapfilled$TIMESTAMP,  tz = "EST")
  gapfilled <- smartbind(gapfilled, x.out )
  return(gapfilled)
} # Gapfills NEE

Gapfill.parms.le <- function(x){
  try(rm(parms.energy), silent=T)
  x$Netall <- x$Rnet.f
  x$LE.filtered <- x$LE

  # libraries used:
  try(x$TIMESTAMP <- as.POSIXct(x$TIMESTAMP,  tz = "EST"), silent=T)
  try(x$Month <- as.numeric(format(x$TIMESTAMP, format="%m")), silent=T)
  try(x$Year <- as.numeric(format(x$TIMESTAMP, format="%Y")), silent=T)
  
  # Sorts the file by TIMESTAMP:
  x <- x[ order(x$TIMESTAMP , decreasing = F ),]
  
  # Add seasonal indicator:
  x$Season <- NA
  x$Season[as.numeric(x$Month) < 05 | as.numeric(x$Month) > 10] <- 1
  x$Season[as.numeric(x$Month) >= 5 & as.numeric(x$Month) <= 10] <- 2
  
  #_________________________________________________________________________________
  #_________________________________________________________________________________
  #_________________________________________________________________________________
  
  # This sections creates the parms.energy file:
  # First a time file is created that should contain all month-year combinations:
  
  
  # Create paramater file to store data:
  
  parms.energy <- data.frame(
    Year = as.numeric(),
    Month = as.numeric(),
    intercept = as.numeric(),
    slope = as.numeric(),
    r.squared = as.numeric(),
    stringsAsFactors=FALSE, row.names=NULL)
  
  time <- data.frame(Year=as.numeric(), Month=numeric(),stringsAsFactors=FALSE, row.names=NULL)
  
  for (i in unique(na.omit(x$Year)) ){
    print(i)
    time <- rbind(time, data.frame(Year=i, Month=unique(na.omit(x$Month))))
  }
  
  time$Season <- NA # Adds Season to the file:
  time$Season[as.numeric(time$Month) < 05 | as.numeric(time$Month) > 10] <- 1
  time$Season[as.numeric(time$Month) >= 5 & as.numeric(time$Month) <= 10] <- 2
  
  parms.energy<- merge( time, parms.energy, all=T) # merge parms file with time file 
  
  #_________________________________________________________________________________
  #_________________________________________________________________________________
  #_________________________________________________________________________________
  # Writes function to filter parm file:
  
  filter.parms <- function(parms){
    parms$intercept[parms$r.squared < 0.40]<- NA
    parms$slope[ parms$slope < 0]<- NA
    parms$intercept[is.na(parms$slope)]<- NA
    parms$slope[is.na(parms$intercept)]<- NA
    parms$r.squared[is.na(parms$intercept)]<- NA
    
    return(parms)
  }
  
  #_________________________________________________________________________________
  #_________________________________________________________________________________
  #_________________________________________________________________________________
  
  # Fits annual-monthly equations and adds paramters to the parms.energy file:
  

  for ( i in unique(parms.energy$Year)){
    for(j in unique(parms.energy$Month)){
      try(rm(y), silent=T)
        try(y <-as.data.frame(t(coef(summary(lm( LE.filtered ~ Netall,
                                                 data= x[which(x$Year == i & x$Month == j ),])))[1:2, c(1)])), silent=T) # Fit the day model
        
        try(names(y)<- c("intercept", "slope"), silent=T)
        try( y[,3] <- NULL, silent=T)
        
        try(y$r.squared <-summary(lm( LE.filtered ~ Netall, data= x[which(x$Year == i & x$Month == j ),]))$r.squared, silent=T)
        try(parms.energy[c(parms.energy$Year == i & parms.energy$Month == j ), 4:6 ] <- cbind(y), silent=T)
   
        }}
      
  try(parms.energy <- filter.parms(parms.energy), silent=T)
  
  try(parms.energy$le.model <- NA, silent=T)
  try(parms.energy$le.model[is.na(parms.energy$intercept) == FALSE] <- "m" , silent=T)
  #_________________________________________________________________________________
  #_________________________________________________________________________________
  #_________________________________________________________________________________
  
  # Fits monthly-plus equations and adds paramters to the parms.energy file:
  
  monthly.parms <- parms.energy[which(is.na(parms.energy$intercept)),]
  
  for ( i in 1:length(monthly.parms$Year)){
    
    a = monthly.parms$Year[i]
    j= as.numeric(monthly.parms$Month[i])
    j = sprintf( "%02d", j)
    j.1 <-sprintf( "%02d", (as.numeric(j) + 1))
    
    if(j.1 == 13 ){
      j.1 <- sprintf( "%02d",1)
      b = as.numeric(a) + 1
    }else{ j.1 <- j.1
    b=a}
    
    j.2 <- as.numeric(j) - 1
    
    try( y1 <- x[which(x$Year == a & x$Month == j),], silent=T)
    try(y1 <- rbind(y1, x[which(x$Year == a & x$Month == j.2),]), silent=T)
    try(y1 <- rbind(y1, x[which(x$Year == b & x$Month == j.1),]), silent=T)
    try(rm(y), silent=T)
    try(y <-as.data.frame(t(coef(summary(lm( LE.filtered ~ Netall, data= y1)))[1:2, c(1)])), silent=T) # Fit the day model
    try(y[,3] <- NULL, silent=T)
    try(names(y)<- c("intercept", "slope"), silent=T)
    try(y$r.squared <- summary(lm( LE.filtered ~ Netall, data= y1))$r.squared, silent=T)
    try(monthly.parms[ i, 4:6 ] <- cbind(y), silent=T)
  }
  
  try(monthly.parms <- filter.parms(monthly.parms), silent=T)
  try(monthly.parms <-monthly.parms[which(is.na(monthly.parms$intercept) ==FALSE),], silent=T)
  try(monthly.parms$le.model <- "m2", silent=T)
  
  for( i in 1:length(monthly.parms$Year)){
    
    try(parms.energy[c(parms.energy$Year == monthly.parms$Year[i] & parms.energy$Month == monthly.parms$Month[i]), 4:7] <- monthly.parms[ i , 4:7] , silent=T)
    try( rm(i), silent=T)
  }
  try(rm(monthly.parms), silent=T)
  
  #_________________________________________________________________________________
  #_________________________________________________________________________________
  #_________________________________________________________________________________
  
  # Fits seasonal equations and adds paramters to the parms.energy file:
  
  seasonal.parms <- parms.energy[which(is.na(parms.energy$intercept)),]
  
  for ( i in unique(x$Year)){
    for(j in unique(x$Season)){
      try(rm(y), silent=T)
      try(y <-as.data.frame(t(coef(summary(lm( LE.filtered ~ Netall,
                                               data= x[which(x$Year == i & x$Season == j ),])))[1:2, c(1)])), silent=T) # Fit the day model
      
      try(names(y)<- c("intercept", "slope"), silent=T)
      try(y[,3] <- NULL, silent=T)
      
      try(y$r.squared <- summary(lm( LE.filtered ~ Netall, data= x[which(x$Year == i & x$Season == j ),]))$r.squared, silent=T)
      try(seasonal.parms[c(seasonal.parms$Year == i & seasonal.parms$Season == j ), 4:6 ] <- cbind(y), silent=T)
    }}
  
  try(seasonal.parms<- filter.parms(seasonal.parms), silent=T)
  try(seasonal.parms <-seasonal.parms[which(is.na(seasonal.parms$intercept) ==FALSE),], silent=T)
  try(seasonal.parms$le.model <- "s", silent=T)
  
  try(for( i in 1:length(parms.energy$intercept)){
    
    if(is.na(parms.energy$intercept[i]) == "TRUE"){
      
      try(parms.energy[ i, 4:7] <- seasonal.parms[c(seasonal.parms$Year == parms.energy$Year[i] & seasonal.parms$Season == parms.energy$Season[i]) , 4:7], silent = T)
      
    }}, silent=T)
   try(rm(seasonal.parms) , silent=T)
  #_________________________________________________________________________________
  #_________________________________________________________________________________
  #_________________________________________________________________________________
  
  # Fits annual equations and adds paramters to the parms.energy file:
  
  annual.parms <- parms.energy[which(is.na(parms.energy$intercept)),]
  
  for ( i in unique(annual.parms$Year)){
    try(rm(y), silent=T)
    try(y <-as.data.frame(t(coef(summary(lm( LE.filtered ~ Netall,
                                             data= x[which(x$Year == i ),])))[1:2, c(1)])), silent=T) # Fit the day model
    
    try(names(y)<- c("intercept", "slope"), silent=T)
    try(y[,3] <- NULL, silent=T)
    
    try(y$r.squared <- summary(lm( LE.filtered ~ Netall, data= x[which(x$Year == i ),]))$r.squared, silent=T)
    try(annual.parms[c(annual.parms$Year == i), 4:6 ] <- cbind(y), silent=T)
  }
  
  #annual.parms <- filter.parms(annual.parms)
  try(annual.parms <-annual.parms[which(is.na(annual.parms$intercept) ==FALSE),], silent=T)
  try(annual.parms$le.model <- "a", silent=T)
  
  try(for( i in 1:length(parms.energy$intercept)){
    
    if(is.na(parms.energy$intercept[i]) == "TRUE"){
      
      try(parms.energy[ i, 4:7] <- annual.parms[c(annual.parms$Year == parms.energy$Year[i]) , 4:7], silent = T)
      
    }}, silent=T)
  
  try( return(parms.energy), silent=T)
  
} # Creates parameter file:

Gapfill.parms.h <- function(x){
  rm(parms.energy)
  x$Netall <- x$Rnet.f
  x$H.filtered <- x$H
  
  # libraries used:
  try(x$TIMESTAMP <- as.POSIXct(x$TIMESTAMP,  tz = "EST"), silent=T)
  try(x$Month <- as.numeric(format(x$TIMESTAMP, format="%m")), silent=T)
  try(x$Year <- as.numeric(format(x$TIMESTAMP, format="%Y")), silent=T)
  
  # Sorts the file by TIMESTAMP:
  x <- x[ order(x$TIMESTAMP , decreasing = F ),]
  
  # Add seasonal indicator:
  x$Season <- NA
  x$Season[as.numeric(x$Month) < 05 | as.numeric(x$Month) > 10] <- 1
  x$Season[as.numeric(x$Month) >= 5 & as.numeric(x$Month) <= 10] <- 2
  
  #_________________________________________________________________________________
  #_________________________________________________________________________________
  #_________________________________________________________________________________
  
  # This sections creates the parms.energy file:
  # First a time file is created that should contain all month-year combinations:
  
  
  # Create paramater file to store data:
  
  parms.energy <- data.frame(
    Year = as.numeric(),
    Month = as.numeric(),
    intercept = as.numeric(),
    slope = as.numeric(),
    r.squared = as.numeric(),
    stringsAsFactors=FALSE, row.names=NULL)
  
  time <- data.frame(Year=as.numeric(), Month=numeric(),stringsAsFactors=FALSE, row.names=NULL)
  
  for (i in unique(na.omit(x$Year)) ){
    print(i)
    time <- rbind(time, data.frame(Year=i, Month=unique(na.omit(x$Month))))
  }
  
  time$Season <- NA # Adds Season to the file:
  time$Season[as.numeric(time$Month) < 05 | as.numeric(time$Month) > 10] <- 1
  time$Season[as.numeric(time$Month) >= 5 & as.numeric(time$Month) <= 10] <- 2
  
  parms.energy <- merge( time, parms.energy, all=T) # merge parms file with time file 
  
  #_________________________________________________________________________________
  #_________________________________________________________________________________
  #_________________________________________________________________________________
  # Writes function to filter parm file:
  
  filter.parms <- function(parms){
    parms$intercept[parms$r.squared < 0.40]<- NA
    parms$slope[ parms$slope < 0]<- NA
    parms$intercept[is.na(parms$slope)]<- NA
    parms$slope[is.na(parms$intercept)]<- NA
    parms$r.squared[is.na(parms$intercept)]<- NA
    
    return(parms)
  }
  
  #_________________________________________________________________________________
  #_________________________________________________________________________________
  #_________________________________________________________________________________
  
  # Fits annual-monthly equations and adds paramters to the parms.energy file:
  
  for ( i in unique(parms.energy$Year)){
    for(j in unique(parms.energy$Month)){
      try(rm(y), silent=T)
      try(y <-as.data.frame(t(coef(summary(lm( H.filtered ~ Netall,
                                               data= x[which(x$Year == i & x$Month == j ),])))[1:2, c(1)])), silent=T) # Fit the day model
      
      try(names(y)<- c("intercept", "slope"), silent=T)
      try( y[,3] <- NULL, silent=T)
      
      try(y$r.squared <-summary(lm( H.filtered ~ Netall, data= x[which(x$Year == i & x$Month == j ),]))$r.squared, silent=T)
      try(parms.energy[c(parms.energy$Year == i & parms.energy$Month == j ), 4:6 ] <- cbind(y), silent=T)
    }}
  
  try(parms.energy <- filter.parms(parms.energy) , silent=T)
  
  try(parms.energy$h.model <- NA , silent=T)
  try(parms.energy$h.model[is.na(parms.energy$intercept) == FALSE] <- "m" , silent=T)
  #_________________________________________________________________________________
  #_________________________________________________________________________________
  #_________________________________________________________________________________
  
  # Fits monthly-plus equations and adds paramters to the parms.energy file:
  
  monthly.parms <- parms.energy[which(is.na(parms.energy$intercept)),]
  
  for ( i in 1:length(monthly.parms$Year)){
    
    a = monthly.parms$Year[i]
    j= as.numeric(monthly.parms$Month[i])
    j = sprintf( "%02d", j)
    j.1 <-sprintf( "%02d", (as.numeric(j) + 1))
    
    if(j.1 == 13 ){
      j.1 <- sprintf( "%02d",1)
      b = as.numeric(a) + 1
    }else{ j.1 <- j.1
    b=a}
    
    j.2 <- as.numeric(j) - 1
    
    y1 <- x[which(x$Year == a & x$Month == j),]
    y1 <- rbind(y1, x[which(x$Year == a & x$Month == j.2),])
    y1 <- rbind(y1, x[which(x$Year == b & x$Month == j.1),])
    try(rm(y), silent=T)
    try(y <-as.data.frame(t(coef(summary(lm( H.filtered ~ Netall, data= y1)))[1:2, c(1)])), silent=T) # Fit the day model
    try(y[,3] <- NULL, silent=T)
    try(names(y)<- c("intercept", "slope"), silent=T)
    try(y$r.squared <- summary(lm( H.filtered ~ Netall, data= y1))$r.squared, silent=T)
    try(monthly.parms[ i, 4:6 ] <- cbind(y), silent=T)
  }
  
  try(monthly.parms <- filter.parms(monthly.parms), silent=T)
  try(monthly.parms <-monthly.parms[which(is.na(monthly.parms$intercept) ==FALSE),], silent=T)
  try(monthly.parms$h.model <- "m2", silent=T)
  
  for( i in 1:length(monthly.parms$Year)){
    
    try(parms.energy[c(parms.energy$Year == monthly.parms$Year[i] & parms.energy$Month == monthly.parms$Month[i]), 4:7] <- monthly.parms[ i , 4:7], silent=T)
    rm(i)
  }
   try(rm(monthly.parms), silent=T)
  
  #_________________________________________________________________________________
  #_________________________________________________________________________________
  #_________________________________________________________________________________
  
  # Fits seasonal equations and adds paramters to the parms.energy file:
  
  seasonal.parms <- parms.energy[which(is.na(parms.energy$intercept)),]
  
  for ( i in unique(x$Year)){
    for(j in unique(x$Season)){
      try(rm(y), silent=T)
      try(y <-as.data.frame(t(coef(summary(lm( H.filtered ~ Netall,
                                               data= x[which(x$Year == i & x$Season == j ),])))[1:2, c(1)])), silent=T) # Fit the day model
      
      try(names(y)<- c("intercept", "slope"), silent=T)
      try(y[,3] <- NULL, silent=T)
      
      try(y$r.squared <- summary(lm( H.filtered ~ Netall, data= x[which(x$Year == i & x$Season == j ),]))$r.squared, silent=T)
      try(seasonal.parms[c(seasonal.parms$Year == i & seasonal.parms$Season == j ), 4:6 ] <- cbind(y), silent=T)
    }}
  
  try(seasonal.parms<- filter.parms(seasonal.parms), silent=T)
  try(seasonal.parms <-seasonal.parms[which(is.na(seasonal.parms$intercept) ==FALSE),], silent=T)
  try(seasonal.parms$h.model <- "s", silent=T)
  
  try(for( i in 1:length(parms.energy$intercept)){
    
    if(is.na(parms.energy$intercept[i]) == "TRUE"){
      
      try(parms.energy[ i, 4:7] <- seasonal.parms[c(seasonal.parms$Year == parms.energy$Year[i] & seasonal.parms$Season == parms.energy$Season[i]) , 4:7], silent = T)
      
    }}, silent=T)
  try(rm(seasonal.parms), silent=T)
  #_________________________________________________________________________________
  #_________________________________________________________________________________
  #_________________________________________________________________________________
  
  # Fits annual equations and adds paramters to the parms.energy file:
  
  annual.parms <- parms.energy[which(is.na(parms.energy$intercept)),]
  
  for ( i in unique(annual.parms$Year)){
    try(rm(y), silent=T)
    try(y <-as.data.frame(t(coef(summary(lm( H.filtered ~ Netall,
                                             data= x[which(x$Year == i ),])))[1:2, c(1)])), silent=T) # Fit the day model
    
    try(names(y)<- c("intercept", "slope"), silent=T)
    try(y[,3] <- NULL, silent=T)
    
    try(y$r.squared <- summary(lm( H.filtered ~ Netall, data= x[which(x$Year == i ),]))$r.squared, silent=T)
    try(annual.parms[c(annual.parms$Year == i), 4:6 ] <- cbind(y), silent=T)
  }
  
  #annual.parms <- filter.parms(annual.parms)
  try(annual.parms <-annual.parms[which(is.na(annual.parms$intercept) ==FALSE),], silent=T)
  try(annual.parms$h.model <- "a", silent=T)
  
  try(for( i in 1:length(parms.energy$intercept)){
    
    if(is.na(parms.energy$intercept[i]) == "TRUE"){
      
      try(parms.energy[ i, 4:7] <- annual.parms[c(annual.parms$Year == parms.energy$Year[i]) , 4:7], silent = T)
      
    }}, silent=T)
  
  return(parms.energy)
  
} # Creates parameter file:

Gapfill.energy.le <- function(x, parms){
  
  x$TIMESTAMP <- as.POSIXct(x$TIMESTAMP,  tz = "EST")
  x$Month <- as.numeric(format(x$TIMESTAMP, format="%m"))
  x$Year <- as.numeric(format(x$TIMESTAMP, format="%Y"))
  
  x$Netall <- x$Rnet.f
  x$LE.filtered <- x$LE
  
  
  # Write functions to estimate energy from parameter values for day:
  
  model <- function(Netall, slope, intercept){
    
    if(is.na(Netall)){ y = NA
    }else{
      y = intercept + slope*Netall
    }
    return(y)}
  
  # Loop to calculate LE:
  for ( i in unique(x$Year)){
    for(j in unique(x$Month)){
      
      try(x$modeled.le[x$Year == i & x$Month == j ] <- mapply(model,x$Netall[x$Year == i & x$Month == j],
                                                          parms[which(parms$Year == i & parms$Month == j),][1,]["slope"],
                                                          parms[which(parms$Year == i & parms$Month == j),][1,]["intercept"]), silent=T)
    }}
  
  # Keep tack of the model used:
  model <- parms[, c("Year", "Month", "le.model")]
  
  x <- merge(x, model)
  
  # Sorts the file by TIMESTAMP:
  x <- x[ order(x$TIMESTAMP , decreasing = F ),]
  
  # Fill le
  x$LE.filled <- x$LE.filtered
  x$LE.filled[is.na(x$LE.filled)] <- x$modeled.le[is.na(x$LE.filled)]
  return(x)
} # Gapfills LE

Gapfill.energy.h <- function(x, parms){
  
  x$TIMESTAMP <- as.POSIXct(x$TIMESTAMP,  tz = "EST")
  x$Month <- as.numeric(format(x$TIMESTAMP, format="%m"))
  x$Year <- as.numeric(format(x$TIMESTAMP, format="%Y"))
  
  x$Netall <- x$Rnet.f
  x$H.filtered <- x$H
  
  
  # Write functions to estimate energy from parameter values for day:
  
  model <- function(Netall, slope, intercept){
    
    if(is.na(Netall)){ y = NA
    }else{
      y = intercept + slope*Netall
    }
    return(y)}
  
  # Loop to calculate LE:
  for ( i in unique(x$Year)){
    for(j in unique(x$Month)){
      
      try(x$modeled.h[x$Year == i & x$Month == j ] <- mapply(model,x$Netall[x$Year == i & x$Month == j],
                                                         parms[which(parms$Year == i & parms$Month == j),][1,]["slope"],
                                                         parms[which(parms$Year == i & parms$Month == j),][1,]["intercept"]), silent=T)
    }}
  
  # Keep tack of the model used:
  model <- parms[, c("Year", "Month", "h.model")]
  
  x <- merge(x, model)
  
  # Sorts the file by TIMESTAMP:
  x <- x[ order(x$TIMESTAMP , decreasing = F ),]
  
  # Fill h
  x$H.filled <- x$H.filtered
  x$H.filled[is.na(x$H.filled)] <- x$modeled.h[is.na(x$H.filled)]
  return(x)
} # Gapfills H

carbon.budget <- function(x){
  
  x$nee.gC <-  12.0107 * x$nee.umol / 1000000 * 1800
  x$gee.gC <-  12.0107 * x$gee.umol / 1000000 * 1800
  x$reco.gC <-  12.0107 * x$reco.umol / 1000000 * 1800
  return(x)
  
}

ET <- function(x){
  
  x$pw <- 1000*(1-(x$TA + 288.6)/(521862*(x$TA + 65.2851))*(x$TA-3.8911)^2)
  
  x$ET <- (((x$LE.filled*0.0000010)*1800)/(x$pw*2.45))*10^3
  
  x$ET <- round(x$ET, 8)
  
  x$pw <- NULL
  return(x)
}

#### Working on new gap filling functions ####

#### Measure the size of the gap####

gap.length <- function(x) {
  
  res = rle(is.na(x$nee))
  x$gap.length = rep(res$values*res$lengths,res$lengths)
  return(x)
}

respiration.parms <- function(x){
  
  # Sorts the file by TIMESTAMP:
  x <- x[ order(x$TIMESTAMP , decreasing = F ),]
  
  # Formats week, Month and Year:
  x$Month <- as.numeric(format(x$TIMESTAMP, format="%m"))
  x$Year <- as.numeric(format(x$TIMESTAMP, format="%Y"))
  x$Week <- week(x$TIMESTAMP)
  
  # Add seasonal indicator:
  x$Season <- NA
  x$Season[as.numeric(x$Month) < 05 | as.numeric(x$Month) > 10] <- 1
  x$Season[as.numeric(x$Month) >= 5 & as.numeric(x$Month) <= 10] <- 2
  
  # Creates Day and Night files:
  x$PAR <- x$PAR.f
  x$TA <- x$TA.f
  night <-x[which(x$PAR <= 0),]
  
  #_________________________________________________________________________________
  #_________________________________________________________________________________
  #_________________________________________________________________________________
  
  # This sections creates the parms.NEE file:
  # First a time file is created that should contain all month-year combinations:
  
  # Creates a dataframe to store model parameters in:
  parms.NEE <- data.frame(
    Year=numeric(),
    Week=numeric(),
    Month=numeric(),
    a=numeric(),
    b=numeric(), 
    a.pvalue=numeric(),
    b.pvalue=numeric(), stringsAsFactors=FALSE, row.names=NULL)
  
  # Creates time file to merge with parm file:
  time <-  x[,c("Year", "Week", "Month")]
  time <- time[!duplicated( time[ ,c( "Year", "Week", "Month")]),]
  
  time$Season <- NA # Adds Season to the file:
  time$Season[as.numeric(time$Month) < 05 | as.numeric(time$Month) > 10] <- 1
  time$Season[as.numeric(time$Month) >= 5 & as.numeric(time$Month) <= 10] <- 2
  
  parms.NEE <- merge(time, parms.NEE, all=T) # merge parms file with time file 
  
  #_________________________________________________________________________________
  #_________________________________________________________________________________
  #_________________________________________________________________________________
  
  # This section creates the day and night functions that will be used to gapfill the file:
  # This section can be adjusted by changing the starting values of both day and night functions.
  
  
  nee.night <- function(dataframe){y.df = nls(nee ~ a * exp(b*TA), 
                                              dataframe, start=list(a= 1 , b=0.01 ),
                                              upper=list(a= 5 , b=1),
                                              na.action=na.exclude, trace=F,
                                              
                                              
                                              control=nls.control(warnOnly=T))
  
  y.df <- as.data.frame(cbind(t(coef(summary(y.df))[1:2, 1]), t(coef(summary(y.df)) [1:2, 4])))
  
  names(y.df) <- c("a", "b", "a.pvalue", "b.pvalue")                      
  return(y.df)}
  
  #_________________________________________________________________________________
  #_________________________________________________________________________________
  #_________________________________________________________________________________
  
  # Functions to filter parameter files by extreme parameter values:
  # These thresholds should be adjusted for each system:
  
  filter.parms.night <- function(parms){
    
    parms$a <- as.numeric(parms$a)
    parms$b <- as.numeric(parms$b)
    parms$a.pvalue <- as.numeric(parms$a.pvalue )
    parms$b.pvalue  <- as.numeric(parms$b.pvalue )
    
    # Filter parms file based on night model:
    parms$a[parms$b > 0.08 ] <- NA
    parms$a[parms$a <= 0.1 ] <- NA
    parms$a[ parms$b < 0] <- NA
    parms$a[parms$a.pvalue =="NaN"] <- NA
    parms$a[parms$b =="NaN"] <- NA
    parms$a[parms$b.pvalue =="NaN"] <- NA
    parms$a.pvalue[is.na(  parms$a)]<- NA
    parms$b[is.na(  parms$a)]<- NA
    parms$b.pvalue[is.na( parms$a)] <- NA
    try( parms$night.model[is.na( parms$a)] <- NA, silent=T)
    return(parms)
  }
  
  #_________________________________________________________________________________
  #_________________________________________________________________________________
  #_________________________________________________________________________________
  
  # Fits weekely within a year equations and adds paramters to the parms.NEE file:
  
  w.parms <- parms.NEE # Creates a parms file for weekely equations:
  
  # This loop fits Annual-Monthly Models and adds the parms to the parms.NEE file:
  try(for ( i in unique(night$Year)){
    for(j in unique(night$Week)){
      
      print(i)
      print(j)
      
      y4 <- try(nee.night(night[which(night$Year == i & night$Week == j),]), silent=T) # Fit night model
      
      try(w.parms[c(w.parms$Year == i & w.parms$Week== j ), 5:8 ] <- cbind(y4), silent=T)
      
      rm( y3, y4)
    }
  }, silent=T)
  
  # Filters annual-monthly params file:
  try(w.parms <- filter.parms.night(w.parms), silent=T)
  
  try(w.parms$night.model[is.na(w.parms$a) == FALSE ] <- "w", silent=T) # Add indicator for the model
  
  
  # Adds annual-monthly parms to parms.NEE:
  try(parms.NEE <- w.parms, silent=T)
  
  rm(w.parms)
  
  #_________________________________________________________________________________
  #_________________________________________________________________________________
  #_________________________________________________________________________________
  
  # Fits equations for missing Weeks using data from the week of, before, and after: 
  
  #Night:
  
  try(fill.parms.night <- parms.NEE[which(is.na(parms.NEE$a)),][, -c(5:9)], silent=T) 
  
  try(for ( i in 1:length(fill.parms.night$Year)){
    
    a = fill.parms.night$Year[i]
    j= as.numeric(fill.parms.night$Week[i])
    j = sprintf( "%02d", j)
    j.1 <-sprintf( "%02d", (as.numeric(j) + 1))
    
    if(j.1 == 53 ){
      j.1 <- sprintf( "%02d",1)
      b = as.numeric(a) + 1
    }else{ j.1 <- j.1
    b=a}
    
    j.2 <- as.numeric(j) - 1
    
    y1 <- night[which(night$Year == a & night$Month == j),]
    y1 <- rbind(y1, night[which(night$Year == a & night$Month == j.2),])
    y1 <- rbind(y1, night[which(night$Year == b & night$Month == j.1),])
    
    y3 <- try(nee.night( y1 )) # Fit the day model
    
    try(fill.parms.night[c(fill.parms.night$Year == a & fill.parms.night$Week == j ), 5:8 ]<- cbind(y3), silent=T)
    
    rm( y3, y1, a,j,j.1, j.2, b)# rbind models to parms dataframe
    
  }, silent=T)
  
  #filters the fill.night file:
  try(fill.parms.night <- filter.parms.night(fill.parms.night), silent=T)
  
  try(fill.parms.night$night.model <- NA, silent=T)
  try(fill.parms.night$night.model[is.na(fill.parms.night$a) == FALSE ] <- "w2", silent=T)
  
  try(fill.parms.night <- fill.parms.night[which(is.na(fill.parms.night$a) == FALSE ),], silent=T) # removes NA
  
  # Adds fill.night data to the Parms file:
  try(for ( i in unique(fill.parms.night$Year)){
    for( j in unique(fill.parms.night$Week)){
      
      parms.NEE[c(parms.NEE$Year == i & parms.NEE$Week == j), c(5:9)] <- fill.parms.night[c(fill.parms.night$Year == i & fill.parms.night$Week == j) , 5:9]
      
    }}, silent=T)
  
  try(rm(i,fill.parms.night), silent=T)
  
  #_________________________________________________________________________________
  #_________________________________________________________________________________
  #_________________________________________________________________________________
  #_________________________________________________________________________________
  #_________________________________________________________________________________
  #_________________________________________________________________________________
  
  # Fits monthly foreach year equations and adds paramters to the parms.NEE file:
  # Creates a parms file for annual equations:
  parms <- data.frame(
    Year=numeric(),
    Week=numeric(),
    Month=numeric(),
    Season=numeric(),
    a=numeric(),
    b=numeric(), 
    a.pvalue=numeric(),
    b.pvalue=numeric(), 
    night.model=character(),
    stringsAsFactors=FALSE, row.names=NULL)
  
  time <-  x[,c("Year", "Month")]
  time <- time[!duplicated( time[ ,c( "Year", "Month")]),]
  
  m.parms <- merge(time, parms, all=T)
  
  # This loop fits Annual-Monthly Models and adds the parms to the parms.NEE file:
  try(for ( i in unique(night$Year)){
    for(j in unique(night$Month)){
      
      y4 <- try(nee.night(night[which(night$Year == i & night$Month == j),]), silent=T) # Fit night model
      
      try(m.parms[c(m.parms$Year == i & m.parms$Month == j ), 5:9 ] <- cbind(y4), silent=T)
      
      rm( y4)
    }
  }, silent=T)
  
  try(m.parms <- filter.parms.night(m.parms), silent=T)
  # Filters annual-monthly params file:
  try(m.parms$night.model[is.na(m.parms$a) == FALSE ] <- "m", silent=T) # Add indicator for the model
  
  # Adds annual-monthly parms to parms.NEE:
  try(for ( i in 1:length(parms.NEE$Month)) {
    y = parms.NEE$Year[i]
    m = parms.NEE$Month[i]
    if(is.na(parms.NEE$a[i]) == TRUE) {
      parms.NEE[ i, c(5:9)]  <- m.parms[c(m.parms$Year == y & m.parms$Month == m), c(5:9)] }
  }, silent=T)
  
  
  rm(m.parms)
  
  #_________________________________________________________________________________
  #_________________________________________________________________________________
  #_________________________________________________________________________________
  
  # Fits equations for missing Months using data from the month of, month before, and the month after: 
  # Day:
  
  parms <- data.frame(
    Year=numeric(),
    Week=numeric(),
    Month=numeric(),
    Season=numeric(),
    a=numeric(),
    b=numeric(), 
    a.pvalue=numeric(),
    b.pvalue=numeric(), 
    night.model=character(),
    stringsAsFactors=FALSE, row.names=NULL)
  
  class(parms.NEE$day.model)
  time <-  x[,c("Year", "Month")]
  time <- time[!duplicated( time[,c( "Year", "Month")]),]
  
  m.parms <- merge(time, parms, all=T)
  
  
  try(fill.parms.night <- m.parms[, -c(5:9)], silent=T) 
  
  try(for ( i in 1:length(fill.parms.night$Year)){
    
    a = fill.parms.night$Year[i]
    j= as.numeric(fill.parms.night$Month[i])
    j = sprintf( "%02d", j)
    j.1 <-sprintf( "%02d", (as.numeric(j) + 1))
    
    if(j.1 == 13 ){
      j.1 <- sprintf( "%02d",1)
      b = as.numeric(a) + 1
    }else{ j.1 <- j.1
    b=a}
    
    j.2 <- as.numeric(j) - 1
    
    y1 <- night[which(night$Year == a & night$Month == j),]
    y1 <- rbind(y1, night[which(night$Year == a & night$Month == j.2),])
    y1 <- rbind(y1, night[which(night$Year == b & night$Month == j.1),])
    
    y3 <- try(nee.night( y1 )) # Fit the day model
    
    try(fill.parms.night[c(fill.parms.night$Year == a & fill.parms.night$Month == j ), 5:8 ]<- cbind(y3), silent=T)
    
    rm( y3, y1, a,j,j.1, j.2, b)# rbind models to parms dataframe
    
  }, silent=T)
  
  
  try(fill.parms.night <- filter.parms.night(fill.parms.night), silent=T) #filters the fill.night file:
  
  try(fill.parms.night$night.model <- NA, silent=T)
  try(fill.parms.night <- fill.parms.night[which(is.na(fill.parms.night$a) == FALSE ),], silent=T) # removes NA
  try(fill.parms.night$night.model <- "m2", silent=T)
  
  # Adds fill.night data to the Parms file:
  try(for ( i in 1:length(fill.parms.night$Year)){
    
    parms.NEE[c(parms.NEE$Year == fill.parms.night$Year[i] & parms.NEE$Month == fill.parms.night$Month[i]) , c(5:9)] <- fill.parms.night[i , 5:9]
    parms.NEE$night.model[c(parms.NEE$Year == fill.parms.night$Year[i] & parms.NEE$Month == fill.parms.night$Month[i])] <- "m2"
    
  }, silent=T)
  
  try(rm(i,fill.parms.night), silent=T)
  
  #________________________________________________
  #________________________________________________
  #________________________________________________
  
  # Seasonal Models:
  # This loop fits annual models!
  seasonal.parms.NEE <- data.frame(
    Year=numeric(),
    Season=numeric(),
    a=numeric(),
    b=numeric(), 
    a.pvalue=numeric(),
    b.pvalue=numeric(),
    stringsAsFactors=FALSE, row.names=NULL)
  
  
  # This is where I stopped
  try(for ( Year in unique(night$Year)){
    for ( Season in unique(night$Season)){
      
      print(Year)
      print(Season)
      
      try( y2 <- nee.night(night[which(night$Year == Year & night$Season == Season),]), silent=T) # Fit night model
      
      
      try(seasonal.parms.NEE <- smartbind(seasonal.parms.NEE, cbind(Year, Season, y2)), silent=T) # rbind models to parms dataframe
      
    }}, silent=T) # Adds annual models to parm file
  
  try(rm( y2, Year, Season), silent=T) 
  
  # Filter Seasonal Data:
  # Filter parms file based on night model:
  
  # Filter parms file based on day model:
  try(seasonal.parms.NEE <- filter.parms.night(seasonal.parms.NEE ), silent=T)
  
  try(seasonal.parms.NEE$night.model <- NA, silent=T)
  try(seasonal.parms.NEE$night.model[is.na(seasonal.parms.NEE$a) == FALSE ] <- "s", silent=T)
  
  
  # Adds seasonal models to the parm file:
  
  try(for( i in 1:length(parms.NEE$a)){
    if(is.na(parms.NEE$a[i]) == "TRUE"){
      try(parms.NEE[ i, 5:9] <- seasonal.parms.NEE[c(seasonal.parms.NEE$Year == parms.NEE$Year[i] & seasonal.parms.NEE$Season == parms.NEE$Season[i]) , 3:7], silent = T)
      parms.NEE$night.model[i] <- "s"
    }}, silent=T)
  
  try(parms.NEE$night.model[is.na(parms.NEE$a)] <- NA, silent=T)
  try( rm(seasonal.parms.NEE ), silent=T)
  
  #________________________________________________
  #________________________________________________
  #________________________________________________
  
  # Annual Function:
  annual.parms.NEE <- data.frame(
    Year=numeric(),
    a=numeric(),
    b=numeric(), 
    a.pvalue=numeric(),
    b.pvalue=numeric(), stringsAsFactors=FALSE, row.names=NULL)
  
  try(for ( Year in unique(night$Year)){
    
    y4 <- try(nee.night(night[which(night$Year == Year ),]), silent=T) # Fit night model
    
    try(annual.parms.NEE <- smartbind(annual.parms.NEE, cbind(Year,y4)), silent=T)
    
    try(rm( Year, y4) , silent=T)# rbind models to parms dataframe
  }, silent=T)
  
  # Add model indicator:
  try(annual.parms.NEE$night.model <- "a", silent=T) # Add indicator for the model:
  annual.parms.NEE$y4 <- NULL
  
  # Filter parms file based on day and night model:
  try( annual.parms.NEE <- filter.parms.night(annual.parms.NEE ), silent = T)
  
  # Adds annual models to the parm file:
  
  try(for( i in 1:length(parms.NEE$a)){
    if(is.na(parms.NEE$a[i]) == "TRUE"){
      try(parms.NEE[ i, c(5:9)] <- annual.parms.NEE[c(annual.parms.NEE$Year == parms.NEE$Year[i] ) , c(2:6)], silent = T)
    }}, silent=T)
  
  try(rm(annual.parms.NEE), silent=T)
  #________________________________________________________________________________________________________
  #________________________________________________________________________________________________________
  #________________________________________________________________________________________________________
  
  # A global Seasonal Model!
  global.parms.NEE <- data.frame(
    Season=numeric(),
    a=numeric(),
    b=numeric(), 
    a.pvalue=numeric(),
    b.pvalue=numeric(), stringsAsFactors=FALSE, row.names=NULL)
  
  try (for ( Season in unique(night$Season)){
    print(Season)
    y1 <- data.frame(try(nee.night(night[which(night$Season == Season),]), silent=T)) # Fit night model
    global.parms.NEE <- rbind( global.parms.NEE , cbind(Season, y1)) # rbind models to parms dataframe
    rm(y1)
  }, silent=T)
  
  try(global.parms.NEE$night.model <- "g", silent=T) # Add indicator for the model:
  try( global.parms.NEE <- filter.parms.night(global.parms.NEE ), silent=T)
  
  
  # Adds models to the parm file:
  
  try(for( i in 1:length(parms.NEE$a)){
    if(is.na(parms.NEE$a[i]) == "TRUE"){
      try(parms.NEE[ i, c(5:9)] <- global.parms.NEE[c(2:6)], silent = T)
    }}, silent=T)
  
  try(rm(global.parms.NEE, time), silent=T)
  
  
  #________________________________________________________________________________________________________
  #________________________________________________________________________________________________________
  #________________________________________________________________________________________________________
  
  # A global Annual Model!
  
  global.parms.NEE <- data.frame(
    a=numeric(),
    b=numeric(), 
    a.pvalue=numeric(),
    b.pvalue=numeric(), stringsAsFactors=FALSE, row.names=NULL)
  
  try(y2 <- nee.night(night), silent=T) # Fit night model
  
  try(global.parms.NEE <- rbind(global.parms.NEE, cbind(y2)), silent=T)
  try(global.parms.NEE$night.model <- "g2", silent=T) # Add indicator for the model:
  
  
  # Adds models to the parm file:
  try(for( i in 1:length(parms.NEE$a)){
    if(is.na(parms.NEE$a[i]) == "TRUE"){
      try(parms.NEE[ i, c(5:9)] <- global.parms.NEE[c(1:5)], silent = T)
    }}, silent=T)
  
  try(rm(global.parms.NEE), silent=T)
  #________________________________________________________________________________________________________
  #________________________________________________________________________________________________________
  #________________________________________________________________________________________________________
  
  return(parms.NEE)
} 
predict.respiration.parms <- function(x){
  print("Fiting parms for Reco")
  try(respiration.parms <- respiration.parms(x), silent=T)
  print("done")
  
  print("formatiing time")
  # Formats times needed
  x$Month <- as.numeric(format(x$TIMESTAMP, format="%m"))
  x$Year <- as.numeric(format(x$TIMESTAMP, format="%Y"))
  x$Week <- week(x$TIMESTAMP)
  x <- x[ order(x$TIMESTAMP , decreasing = F ),] # Sorts the file by TIMESTAMP:
  
  # Add seasonal indicator:
  x$Season[as.numeric(x$Month) < 05 | as.numeric(x$Month) > 10] <- 1
  x$Season[as.numeric(x$Month) >= 5 & as.numeric(x$Month) <= 10] <- 2
  print("done")
  
  
  print("merging parms with the data")
  # Remove No PAR DATA:
  x$TA <- x$TA.f
  x$NEE.filtered <- x$nee
  # Merge subset with the PARMS FILE
  x1 <- x %>% left_join(respiration.parms, by=c('Year', 'Week'))
  x1 <-x1[!duplicated(x1$TIMESTAMP),]
  
  # CALCULATE RECO.pred AND MERge this with the model used into the data file:
  x1$Reco.pred <- x1$a * exp(x1$b*x1$TA)
  
  # Sorts the file by TIMESTAMP:
  x1 <- x1[ order(x1$TIMESTAMP , decreasing = F ),]
  
  # Reco:
  x1$reco <- x1$nee.filled
  x1$reco[ x1$PAR.f > 0 & !is.na(x1$PAR.f)] <- x1$Reco.pred[ x1$PAR.f > 0 & !is.na(x1$PAR.f)]
  
  x1$reco[x1$PAR <= 0 & is.na(x1$reco)] <- x1$modeled.reco[x1$PAR <= 0 & is.na(x1$reco) ] # fill all missing night time data with modeled reco
  x1$reco[x1$PAR > 0 ] <- x1$modeled.reco[x1$PAR > 0 ] # fill all daytime data with modeled reco
  x1$reco[!is.na(x1$reco) & x1$nee.umol >  x1$reco] <-  x1$nee.umol[!is.na(x1$reco) & x1$nee.umol >  x1$reco] # use modeled nee.umol when ever nee is greater than reco * tends to be an issue early in the da
  x1$reco[x1$nee.filled > 0 & !is.na(x1$nee.filled) & x1$PAR.f > 0 & !is.na(x1$PAR.f)] <- x1$nee.filled[x1$nee.filled > 0 & !is.na(x1$nee.filled) & x1$PAR.f > 0 & !is.na(x1$PAR.f) ] * 2
  x1$gee.umol[x1$nee.filled > 0 & !is.na(x1$nee.filled) & x1$PAR.f > 0 & !is.na(x1$PAR.f)] <- x1$nee.filled[x1$nee.filled > 0 & !is.na(x1$nee.filled) & x1$PAR.f > 0 & !is.na(x1$PAR.f) ] *-1
  
  # GEE
  x1$gee.umol <- x1$nee.filled -  x1$reco
  
  # NEE
  x1$nee.umol <- x1$nee.filled
  x1$reco.umol <- x1$reco
  x1$reco<- NULL
  #add data in g C
  x1$nee.gC <-  12.0107 * x1$nee.filled / 1000000 * 1800
  x1$gee.gC <-  12.0107 * x1$gee.umol/ 1000000 * 1800
  x1$reco.gC <-  12.0107 * x1$reco.umol / 1000000 * 1800
  print("done")
  
  return(x1)
}  # Gapfills NEE

#### Mean Diurnal Variation ####
# This approach is used for caps <= 5 hours:
MDV.5 <- function(x){
  
  x$Date <- as.Date(format(x$TIMESTAMP, format="%Y-%m-%d"))
  x$Hour <- format(x$TIMESTAMP, format="%H")
  
  mdv.lut.date  <- x[c( 'Date', 'Hour') ]
  # remove duplicates
  mdv.lut.date <- distinct(mdv.lut.date, Date,Hour, .keep_all= TRUE)
  
  mdv.lut <- data.frame()
  

  for ( i in unique(as.character(mdv.lut.date$Date))){
    
    print(i)
    
    try(i <- as.Date(i, '%Y-%m-%d'), silent=T)
    
    try(i.p <- i+ 14, silent=T)
    try(i.m <- i- 14, silent=T)
    
    try(data.subset <- x[ which( x$Date <= i.p & x$Date >= i.m), ], silent=T)
    try(data.subset <- data.subset[ c( 'Hour', 'nee', 'LE', 'H')], silent=T)
    
    # Create LUT
    try(lut.srs <- aggregate( . ~  Hour, data.subset, mean, na.rm=T, all=T), silent=T)
    
    try(mdv.lut.a <- merge( mdv.lut.date[which( mdv.lut.date$Date == i) ,] , lut.srs, by="Hour", all=T), silent=T)
    try(mdv.lut <- smartbind(mdv.lut, mdv.lut.a ), silent=T)
    
  }
  
  names(mdv.lut) <- c("Hour", "Date", "nee.mdv",  "LE.mdv",   "H.mdv"  ) # rename fluxes
  mdv.lut$Date <- as.character(mdv.lut$Date)
  mdv.lut$Hour <- as.character(mdv.lut$Hour)
  
  x$Date <- as.character(x$Date)
  x$Hour <- as.character(x$Hour)
  
  x.mdv <- x %>% left_join(mdv.lut, by=c("Date","Hour"))
  
  return(x.mdv)
}

# Look up table functions: 

# Create look up tables for the next round of gap-filling:
# Functions to Bin data
ppfd.bins <- function( dataframe){
  dataframe$PAR.BINS <- NA
  dataframe$PAR.round <- round( dataframe$PAR.f,0)
  dataframe$PAR.BINS[ dataframe$PAR.round >= 0 ] <- 0
  dataframe$PAR.BINS[ dataframe$PAR.round >= 1 & dataframe$PAR.round <= 100] <- 100
  dataframe$PAR.BINS[ dataframe$PAR.round >= 101 & dataframe$PAR.round <= 200] <- 200
  dataframe$PAR.BINS[ dataframe$PAR.round >= 201 & dataframe$PAR.round <= 300] <- 300
  dataframe$PAR.BINS[ dataframe$PAR.round >= 301 & dataframe$PAR.round <= 400] <- 400
  dataframe$PAR.BINS[ dataframe$PAR.round >= 401 & dataframe$PAR.round <= 500] <- 500
  dataframe$PAR.BINS[ dataframe$PAR.round >= 501 & dataframe$PAR.round <= 600] <- 600
  dataframe$PAR.BINS[ dataframe$PAR.round >= 601 & dataframe$PAR.round <= 700] <- 700
  dataframe$PAR.BINS[ dataframe$PAR.round >= 701 & dataframe$PAR.round <= 800] <- 800
  dataframe$PAR.BINS[ dataframe$PAR.round >= 801 & dataframe$PAR.round <= 900] <- 900
  dataframe$PAR.BINS[ dataframe$PAR.round >= 901 & dataframe$PAR.round <= 1000] <- 1000
  dataframe$PAR.BINS[ dataframe$PAR.round >= 1001 & dataframe$PAR.round <= 1100] <- 1100
  dataframe$PAR.BINS[ dataframe$PAR.round >= 1101 & dataframe$PAR.round <= 1200] <- 1200
  dataframe$PAR.BINS[ dataframe$PAR.round >= 1201 & dataframe$PAR.round <= 1300] <- 1300
  dataframe$PAR.BINS[ dataframe$PAR.round >= 1301 & dataframe$PAR.round <= 1400] <- 1400
  dataframe$PAR.BINS[ dataframe$PAR.round >= 1401 & dataframe$PAR.round <= 1500] <- 1500
  dataframe$PAR.BINS[ dataframe$PAR.round >= 1501 & dataframe$PAR.round <= 1600] <- 1600
  dataframe$PAR.BINS[ dataframe$PAR.round >= 1601 & dataframe$PAR.round <= 1700] <- 1700
  dataframe$PAR.BINS[ dataframe$PAR.round >= 1701 & dataframe$PAR.round <= 1800] <- 1800
  dataframe$PAR.BINS[ dataframe$PAR.round >= 1801 & dataframe$PAR.round <= 1900] <- 1900
  dataframe$PAR.BINS[ dataframe$PAR.round >= 1901 & dataframe$PAR.round <= 2000] <- 2000
  dataframe$PAR.BINS[ dataframe$PAR.round >= 2001 & dataframe$PAR.round <= 2100] <- 2100
  dataframe$PAR.BINS[ dataframe$PAR.round >= 2101 & dataframe$PAR.round <= 2200] <- 2200
  dataframe$PAR.BINS[ dataframe$PAR.round >= 2201 & dataframe$PAR.round <= 2300] <- 2300
  dataframe$PAR.BINS[ dataframe$PAR.round >= 2301 & dataframe$PAR.round <= 2400] <- 2400
  dataframe$PAR.BINS[ dataframe$PAR.round >= 2401 & dataframe$PAR.round <= 2500] <- 2500
  dataframe$PAR.BINS[ dataframe$PAR.round >= 2501 & dataframe$PAR.round <= 2600] <- 2600
  
  return(dataframe)
}
ta.bins <- function(dataframe){
  
  dataframe$TA.BINS <- NA
  dataframe$TA.round <- round( dataframe$TA.f , 0)
  
  dataframe$TA.BINS[ dataframe$TA.round  >= -16 & dataframe$TA.round  <= -14 ] <- -14
  dataframe$TA.BINS[ dataframe$TA.round  >= -13 & dataframe$TA.round  <=  -11 ] <- -11
  dataframe$TA.BINS[ dataframe$TA.round  >=-10 & dataframe$TA.round  <=  -8 ] <- -8
  dataframe$TA.BINS[ dataframe$TA.round  >= -7 & dataframe$TA.round  <=  -5 ] <- -5
  dataframe$TA.BINS[ dataframe$TA.round  >= -4 & dataframe$TA.round  <=  -2 ] <- -2
  dataframe$TA.BINS[ dataframe$TA.round  >= -1 & dataframe$TA.round  <=  1 ] <- 1
  dataframe$TA.BINS[ dataframe$TA.round  >= 2 & dataframe$TA.round  <=  4 ] <- 4
  dataframe$TA.BINS[ dataframe$TA.round  >= 5 & dataframe$TA.round  <=  7 ] <- 7
  dataframe$TA.BINS[ dataframe$TA.round  >= 8 & dataframe$TA.round  <=  10 ] <- 10
  dataframe$TA.BINS[ dataframe$TA.round  >= 11 & dataframe$TA.round  <=  13 ] <- 13
  dataframe$TA.BINS[ dataframe$TA.round  >= 14 & dataframe$TA.round  <=  16 ] <- 16
  dataframe$TA.BINS[ dataframe$TA.round  >= 17 & dataframe$TA.round  <=  19 ] <- 19
  dataframe$TA.BINS[ dataframe$TA.round  >= 20 & dataframe$TA.round  <=  22 ] <- 22
  dataframe$TA.BINS[ dataframe$TA.round  >= 23 & dataframe$TA.round  <=  25 ] <- 25
  dataframe$TA.BINS[ dataframe$TA.round  >= 26 & dataframe$TA.round  <=  28 ] <- 28
  dataframe$TA.BINS[ dataframe$TA.round  >= 29 & dataframe$TA.round  <=  31 ] <- 31
  dataframe$TA.BINS[ dataframe$TA.round  >= 32 & dataframe$TA.round  <=  34 ] <- 34
  dataframe$TA.BINS[ dataframe$TA.round  >= 35 & dataframe$TA.round  <=  37 ] <- 37
  dataframe$TA.BINS[ dataframe$TA.round  >= 38 & dataframe$TA.round  <=  40 ] <- 40
  dataframe$TA.BINS[ dataframe$TA.round  >= 41 & dataframe$TA.round  <=  43 ] <- 43
  dataframe$TA.BINS[ dataframe$TA.round  >= 44 & dataframe$TA.round  <=  46 ] <- 46
  dataframe$TA.BINS[ dataframe$TA.round  >= 47 & dataframe$TA.round  <=  49 ] <- 49
  
  #dataframe$TA.round  <- NULL
  return(dataframe)
}

TA.BINS <- seq(-14, 49, 3 )
PAR.BINS <- seq(0, 2600, 100 )

conditions <- merge(TA.BINS, PAR.BINS )
names(conditions) <- c("TA.BINS", "PAR.BINS")

# Should only be used for CO2 and CH4. (yearmonth, year season, month is the fill order for the table)
lut.PAR.TA <- function(dataframe, flux) { 
  time <- 'yearmonth'
  dataframe2 <- ppfd.bins (ta.bins(dataframe))
  dataframe2$time <- dataframe2$YearMonth <- format( dataframe2$TIMESTAMP,'%y-%m')

  # Create Look up table:
  # 2nd look up table based on hour
  
  lut1 <- data.frame(
    PAR.BINS = as.numeric(),
    TA.BINS = as.numeric(),
    flux = as.numeric(),
    time = as.character(),
    stringsAsFactors = FALSE)
  
  for ( i in unique( dataframe2$time) ){
    print(i)
    
    if ( flux == "co2" ) {
      try( data.subset <- dataframe2[which( dataframe2$time == i),] , silent = T)
      # Create LUT
      data.subset$flux <- data.subset$nee
      try(lut.flux <-aggregate(flux ~  PAR.BINS + TA.BINS, data.subset, mean) , silent=T)
      try( flux.lm <- lm( flux ~ PAR.BINS * TA.BINS, data = lut.flux), silent = T)
      # Fill missing values for flux in LUT
      try( lut.flux$flux.pred <- predict(flux.lm, lut.flux), silent = T)
      try( lut.flux$flux.f <- lut.flux$flux, silent = T)
      try( lut.flux$flux.f[is.na(lut.flux$flux.f)] <- lut.flux$flux.pred[is.na(lut.flux$flux.f)], silent = T)
      try( lut.flux$flux.pred <- lut.flux$flux <- NULL, silent = T)
      try( lut.flux$time <- i, silent = T)
      try( lut.flux <- as.data.frame(lut.flux) , silent = T)
      try( lut1  <- smartbind(lut1, lut.flux) , silent = T)
    } else if (flux == "H" ){
      try( data.subset <- dataframe2[which( dataframe2$time == i),] , silent = T)
      data.subset$flux <- data.subset$H
      try(lut.flux <-aggregate(flux ~  PAR.BINS + TA.BINS, data.subset, mean) , silent=T)
      try( flux.lm <- lm( flux ~ PAR.BINS * TA.BINS, data = lut.flux), silent = T)
      # Fill missing values for flux in LUT
      try( lut.flux$flux.pred <- predict(flux.lm, lut.flux), silent = T)
      try( lut.flux$flux.f <- lut.flux$flux, silent = T)
      try( lut.flux$flux.f[is.na(lut.flux$flux.f)] <- lut.flux$flux.pred[is.na(lut.flux$flux.f)], silent = T)
      try( lut.flux$flux.pred <- lut.flux$flux <- NULL, silent = T)
      try( lut.flux$time <- i, silent = T)
      try( lut.flux <- as.data.frame(lut.flux) , silent = T)
      try( lut1  <- smartbind(lut1, lut.flux) , silent = T)
    }else if (flux == "LE" ){
      try( data.subset <- dataframe2[which( dataframe2$time == i),] , silent = T)
      data.subset$flux <- data.subset$LE
      
      try(lut.flux <-aggregate(flux ~  PAR.BINS + TA.BINS, data.subset, mean) , silent=T)
      try( flux.lm <- lm( flux ~ PAR.BINS * TA.BINS, data = lut.flux), silent = T)
      # Fill missing values for flux in LUT
      try( lut.flux$flux.pred <- predict(flux.lm, lut.flux), silent = T)
      try( lut.flux$flux.f <- lut.flux$flux, silent = T)
      try( lut.flux$flux.f[is.na(lut.flux$flux.f)] <- lut.flux$flux.pred[is.na(lut.flux$flux.f)], silent = T)
      try( lut.flux$flux.pred <- lut.flux$flux <- NULL, silent = T)
      try( lut.flux$time <- i, silent = T)
      try( lut.flux <- as.data.frame(lut.flux) , silent = T)
      try( lut1  <- smartbind(lut1, lut.flux) , silent = T)
    }else if (flux == "ch4" ){
      try( data.subset <- dataframe2[which( dataframe2$time == i),] , silent = T)
      data.subset$flux <- data.subset$Fch4
      
      try(lut.flux <-aggregate(flux ~  PAR.BINS + TA.BINS, data.subset, mean) , silent=T)
      try( flux.lm <- lm( flux ~ PAR.BINS * TA.BINS, data = lut.flux), silent = T)
      # Fill missing values for flux in LUT
      try( lut.flux$flux.pred <- predict(flux.lm, lut.flux), silent = T)
      try( lut.flux$flux.f <- lut.flux$flux, silent = T)
      try( lut.flux$flux.f[is.na(lut.flux$flux.f)] <- lut.flux$flux.pred[is.na(lut.flux$flux.f)], silent = T)
      try( lut.flux$flux.pred <- lut.flux$flux <- NULL, silent = T)
      try( lut.flux$time <- i, silent = T)
      try( lut.flux <- as.data.frame(lut.flux) , silent = T)
      try( lut1  <- smartbind(lut1, lut.flux) , silent = T)
    } else{ print(" No LUT for You")}
    
  }
  
  
  # make a full conditions df:
  
  dataframe2$year <-format( dataframe2$TIMESTAMP, '%y')
  dataframe2$month <-format( dataframe2$TIMESTAMP, '%m')
  year <-  seq(min(dataframe2$year), max(dataframe2$year), by=1)
  month <-  str_pad(seq(min(dataframe2$month), max(dataframe2$month), by=1), 2, pad = "0")
  time2 <- merge(year, month, all=T )
  time2$time <- paste(time2$x, time2$y, sep="-")
  names(time2) <- c('year', 'month', 'time')
  conditions2 <- merge(time2, conditions, all=T)
  
  # FINAL LUT
  conditions.M <- merge(conditions2, lut1, by=c( 'time', 'TA.BINS', 'PAR.BINS'), all=T)
  
  summary(conditions.M)
  conditions.M$flux.pred.y <- NA
  conditions.M$flux.pred.m <- NA
  conditions.M$flux.pred.ys <- NA
  
  #  create a year model to fill gaps in the table
  for( i in na.omit(unique( conditions.M$year))){
    print(i)
    try(flux.lm <- lm( flux.f ~ PAR.BINS * TA.BINS, data = conditions.M[which( conditions.M$year == i),] ), silent=T)
    try(conditions.M[which( conditions.M$year == i),][,7]  <- predict(flux.lm,  conditions.M[which( conditions.M$year == i),], na.action = na.omit ), silent=T)
  }
  
  for( i in na.omit(unique( conditions.M$month))){
    print(i)
    try(flux.lm <- lm( flux.f ~ PAR.BINS * TA.BINS, data = conditions.M[which( conditions.M$month == i),] ), silent=T)
    try(conditions.M[which( conditions.M$month == i),][,8]  <- predict(flux.lm,  conditions.M[which( conditions.M$month == i),], na.action = na.omit ), silent=T)
  }
  
  # Create a season filled model
  conditions.M$month <- as.numeric(conditions.M$month)
  conditions.M$Season <- NA
  conditions.M$Season[as.numeric(conditions.M$month) < 05 | as.numeric(conditions.M$month) > 10] <- 1
  conditions.M$Season[as.numeric(conditions.M$month) >= 05 & as.numeric(conditions.M$month) <= 10] <- 2
  conditions.M$yearseason <- paste(conditions.M$year, conditions.M$Season, sep='-')

  for( i in na.omit(unique( conditions.M$yearseason))){
    print(i)
    try(flux.lm <- lm( flux.f ~ PAR.BINS * TA.BINS, data = conditions.M[which( conditions.M$month == i),] ), silent=T)
    try(conditions.M[which( conditions.M$yearseason == i),][,9]  <- predict(flux.lm,  conditions.M[which( conditions.M$yearseason == i),], na.action = na.omit ), silent=T)
  }
  
  # fill the gaps in the flux.f column
  conditions.M$model <-"yearmonth"
  conditions.M$flux.gf <- conditions.M$flux.f
  conditions.M$model[is.na(conditions.M$flux.gf)] <-"yearseason"
  conditions.M$flux.gf[is.na(conditions.M$flux.gf)] <- conditions.M$flux.pred.ys[is.na(conditions.M$flux.gf)]
  conditions.M$model[is.na(conditions.M$flux.gf)] <-"month"
  conditions.M$flux.gf[is.na(conditions.M$flux.gf)] <- conditions.M$flux.pred.m[is.na(conditions.M$flux.gf)]
  conditions.M$model[is.na(conditions.M$flux.gf)] <-"year"
  conditions.M$flux.gf[is.na(conditions.M$flux.gf)] <- conditions.M$flux.pred.y[is.na(conditions.M$flux.gf)]
  

  # Change flux to the name of the flux
  if ( flux == "co2" ) {   
    conditions.M$co2.pred <- conditions.M$flux.gf
    conditions.M$flux.pred <- conditions.M$flux.f <- conditions.M$flux.gf  <- conditions.M$flux.pred.y  <-conditions.M$flux.pred.m <-NULL
  }else if (flux == "H" ){
    conditions.M$H.pred <- conditions.M$flux.gf
    conditions.M$flux.pred <- conditions.M$flux.f <- conditions.M$flux.gf  <- conditions.M$flux.pred.y  <-conditions.M$flux.pred.m <-NULL
  }else if(flux == "LE" ){
    conditions.M$LE.pred <- conditions.M$flux.gf
    conditions.M$flux.pred <- conditions.M$flux.f <- conditions.M$flux.gf  <- conditions.M$flux.pred.y  <-conditions.M$flux.pred.m <-NULL
  }else if (flux == "ch4" ){
    conditions.M$ch4.pred <- conditions.M$flux.gf
    conditions.M$flux.pred <- conditions.M$flux.f <- conditions.M$flux.gf  <- conditions.M$flux.pred.y  <-conditions.M$flux.pred.m <-NULL
  }else{ print(" No LUT for You")}
  
  
  return( conditions.M)
}
