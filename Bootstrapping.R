# Bootstrapping parameters:
rm(list=ls())

library(plyr)
library(lubridate)
library(FREddyPro)
library(zoo)
library(gtools)
library(nlstools)


## Office Computer
load( "~/OneDrive - Florida International University/Research/Flux Towers/TSPH7/TS7.RData")
## Laptop computer:
##load( "~/Desktop/OneDrive - Florida International University/Research/Flux Towers/TSPH7/TS7.RData")

# Use Parms file to determine bootstrapping map:
x <- ts7
parms <- nee.parms


  # Create file to store parms and se
  boot.NEE <- parms[, c("Year", "Month", "Week", "Season", "day.model", "night.model")]
  boot.NEE$a1.est <- 0
  boot.NEE$ax.est<- 0
  boot.NEE$r.est<- 0
  boot.NEE$a1.se<- 0
  boot.NEE$ax.se<- 0
  boot.NEE$r.se<- 0
  boot.NEE$a.est<- 0
  boot.NEE$b.est<- 0
  boot.NEE$a.se<- 0
  boot.NEE$b.se<- 0
  
  #_________________________________________________________________________________
  ## Creates file to store data:
  
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
  x$T_sonic <- x$Temp.f
  day <- x[which(x$PAR > 0 ),]
  night <-x[which(x$PAR <= 0),]
  
  #_________________________________________________________________________________
  
  # Fits weekely within a year equations and adds paramters to the parms.NEE file:
  # This loop fits Annual-Monthly Models and adds the parms to the parms.NEE file:
  
  week.boot.nee.day <- boot.NEE[which(boot.NEE$day.model == "w"),]
  week.boot.nee.night <- boot.NEE[which(boot.NEE$night.model == "w"),]
  
  for ( i in unique(week.boot.nee.day$Year)){
    for(j in unique(week.boot.nee.day$Week)){
      print('Weekly Model Fit')
      print(i); print(j)
      rm(day.fit, a, b, c, results)
      try(day.fit <- nls( nee ~ (a1 * PAR * ax)/(a1 * PAR + ax) + r, data=day[which(day$Year == i & day$Week == j),], 
                          start=list(a1= -0.1 , ax= -7, r= 2),
                          upper=list(a1= 0, ax= -1, r= 10),
                          na.action=na.exclude, trace=F, control=nls.control(warnOnly=T)), silent=T)
      
      try(results <- nlsBoot(day.fit, niter=10 ), silent=T)
      try( a <- t(results$estiboot)[1, 1:3], silent=T)
      try(  names(a) <- c('a1.est', 'ax.est', 'r.est'), silent=T)
      try(b <- t(results$estiboot)[2, 1:3], silent=T)
      try(names(b) <- c('a1.se', 'ax.se', 'r.se'), silent=T)
      try(c <- t(data.frame(c(a,b))), silent=T)
      try( week.boot.nee.day[c(week.boot.nee.day$Year == i & week.boot.nee.day$Week == j), 7:12] <- c[1, 1:6], silent=T)
      try( boot.NEE[c(boot.NEE$Year == i & boot.NEE$Week == j), 7:12] <- c[1, 1:6], silent=T)
      rm(day.fit, a, b, c, results)
    }
  }
  
  ## Night Model
  for ( i in unique(week.boot.nee.night$Year)){
    for(j in unique(week.boot.nee.night$Week)){
      
      print('Weekly Model Fit')
      print(i); print(j)
      rm(day.fit, a, b, c, results)
      ## Night Model
      try(nee.night <- nls(nee ~ a * exp(b*T_sonic),data=night[which(night$Year == i & night$Week == j),]
                           , start=list(a= 1 , b=0.01 ),upper=list(a= 5 , b=1),
                           na.action=na.exclude, trace=F, control=nls.control(warnOnly=T)), silent=T)
      try(results <- nlsBoot( nee.night, niter=1000 ), silent=T)
      try( a <- t(results$estiboot)[1, 1:2], silent=T)
      try(  names(a) <- c('a.est', 'b.est'), silent=T)
      try(b <- t(results$estiboot)[2, 1:2], silent=T)
      try(names(b) <- c('a.se', 'b.se'), silent=T)
      try(c <- t(data.frame(c(a,b))), silent=T)
      try( week.boot.nee.night[c(week.boot.nee.night$Year == i & week.boot.nee.night$Week == j), 13:16] <- c[1, 1:4], silent=T)
      try( boot.NEE[c(boot.NEE$Year == i & boot.NEE$Week == j), 13:16] <- c[1, 1:4], silent=T)
      rm(day.fit, a, b, c, results)
    }}
  
  #_________________________________________________________________________________
  
  # fill data with the week before...
  
  week2.boot.nee.day <- boot.NEE[which(boot.NEE$day.model == "w2"),]
  week2.boot.nee.night <- boot.NEE[which(boot.NEE$night.model == "w2"),]
  
  
  try(for( i in 1:length(week2.boot.nee.day$Year)){
    
    print("2-Week Window model")
    print(i)
    
    a = week2.boot.nee.day$Year[i]
    j= as.numeric(week2.boot.nee.day$Week[i])
    #j = sprintf( "%02d", j)
    #j.1 <-sprintf( "%02d", (as.numeric(j) + 1))
    j.1 <- j + 1
    # Correct for the last month in a year:
    if(j.1 == 54 ){
      #j.1 <- sprintf( "%02d",1)
      j.1 <- 1
      b = as.numeric(a) + 1
    }else{ j.1 <- j.1
    b=a}
    
    j.2 <- as.numeric(j) - 1
    
    y1 <- day[which(day$Year == a & day$Week == j),]
    y1 <- rbind(y1, day[which(day$Year == a & day$Week == j.2),])
    y1 <- rbind(y1, day[which(day$Year == b & day$Week == j.1),])
    
    rm(a,b,j.1, j.2)
    
    try(day.fit <- nls( nee ~ (a1 * PAR * ax)/(a1 * PAR + ax) + r, data=y1, 
                        start=list(a1= -0.1 , ax= -7, r= 2),
                        upper=list(a1= 0, ax= -1, r= 10),
                        na.action=na.exclude, trace=F, control=nls.control(warnOnly=T)), silent=T)
    
    try(results <- nlsBoot(day.fit, niter=10 ), silent=T)
    try( a <- t(results$estiboot)[1, 1:3], silent=T)
    try(  names(a) <- c('a1.est', 'ax.est', 'r.est'), silent=T)
    try(b <- t(results$estiboot)[2, 1:3], silent=T)
    try(names(b) <- c('a1.se', 'ax.se', 'r.se'), silent=T)
    try(c <- t(data.frame(c(a,b))), silent=T)
    try( week.boot.nee.day[c(week.boot.nee.day$Year == i & week.boot.nee.day$Week == j), 7:12] <- c[1, 1:6], silent=T)
    try( boot.NEE[c(boot.NEE$Year == i & boot.NEE$Week == j), 7:12] <- c[1, 1:6], silent=T)
    rm(day.fit, a, b, c, results, j, y1)
  }, silent=T)
  
  #________________________________________________
  
  #Night:
  try(for ( i in 1:length(week2.boot.nee.night$Year)){
    
    print("2-Week Window model")
    print(i)
    
    a = week2.boot.nee.nightYear[i]
    j= as.numeric(week2.boot.nee.night$Week[i])
    #j = sprintf( "%02d", j)
    #j.1 <-sprintf( "%02d", (as.numeric(j) + 1))
    j.1 <- j + 1
    # Correct for the last month in a year:
    if(j.1 == 54 ){
      #j.1 <- sprintf( "%02d",1)
      j.1 <- 1
      b = as.numeric(a) + 1
    }else{ j.1 <- j.1
    b=a}
    
    j.2 <- as.numeric(j) - 1
    
    y1 <- night[which(night$Year == a & night$Month == j),]
    y1 <- rbind(y1, night[which(night$Year == a & night$Month == j.2),])
    y1 <- rbind(y1, night[which(night$Year == b & night$Month == j.1),])
    
    
    rm(a,b,j.1, j.2)
    
    try(nee.night <- nls(nee ~ a * exp(b*T_sonic),data=y1
                         , start=list(a= 1 , b=0.01 ),upper=list(a= 5 , b=1),
                         na.action=na.exclude, trace=F, control=nls.control(warnOnly=T)), silent=T)
    try(results <- nlsBoot( nee.night, niter=1000 ), silent=T)
    try( a <- t(results$estiboot)[1, 1:2], silent=T)
    try(  names(a) <- c('a.est', 'b.est'), silent=T)
    try(b <- t(results$estiboot)[2, 1:2], silent=T)
    try(names(b) <- c('a.se', 'b.se'), silent=T)
    try(c <- t(data.frame(c(a,b))), silent=T)
    try( week2.boot.nee.night[c(week2.boot.nee.night$Year == i & week2.boot.nee.night$Week == j), 13:16] <- c[1, 1:4], silent=T)
    try( boot.NEE[c(boot.NEE$Year == i & boot.NEE$Week == j), 13:16] <- c[1, 1:4], silent=T)
    
    rm(day.fit, a, b, c, results, j, y1)# rbind models to parms dataframe
    
  }, silent=T) 
  
  #_________________________________________________________________________________
  
  # Monthly files!!!!
  
  #### Need to remove repetative months-Years from file
  month.boot.nee.day <- boot.NEE[which(boot.NEE$day.model == "m"),]
  month.boot.nee.night <- boot.NEE[which(boot.NEE$night.model == "m"),]
  
  # Remove duplicates of year month combinations:
  month.boot.nee.day <-month.boot.nee.day[!duplicated(month.boot.nee.day[c(1,2)]),]
  month.boot.nee.night <-month.boot.nee.night[!duplicated(month.boot.nee.night[c(1,2)]),]
  
  # Day Model:
  try(for ( i in unique(month.boot.nee.day$Year)){
    for(j in unique(month.boot.nee.day$Month)){
      print("Monthly Model")
      print(i)
      print(j)
      
      y1 <-day[which(day$Year == i & day$Month == j),]
      
      try(day.fit <- nls( nee ~ (a1 * PAR * ax)/(a1 * PAR + ax) + r, data=y1, 
                          start=list(a1= -0.1 , ax= -7, r= 2),
                          upper=list(a1= 0, ax= -1, r= 10),
                          na.action=na.exclude, trace=F, control=nls.control(warnOnly=T)), silent=T)
      
      try(results <- nlsBoot(day.fit, niter=10 ), silent=T)
      try( a <- t(results$estiboot)[1, 1:3], silent=T)
      try(  names(a) <- c('a1.est', 'ax.est', 'r.est'), silent=T)
      try(b <- t(results$estiboot)[2, 1:3], silent=T)
      try(names(b) <- c('a1.se', 'ax.se', 'r.se'), silent=T)
      try(c <- t(data.frame(c(a,b))), silent=T)
      try( month.boot.nee.day[c(month.boot.nee.day$Year == i & month.boot.nee.day$Month == j), 7:12] <- c[1, 1:6], silent=T)
      
      for (l in 1:length(boot.NEE$day.model[c(boot.NEE$Year == i & boot.NEE$Month == j & boot.NEE$day.model == "m")])){
        print(l)
        try( boot.NEE[c(boot.NEE$Year == i & boot.NEE$Month == j & boot.NEE$day.model == "m"),][ l, 7:12] <- c[1, 1:6], silent=T)
      }
      
      rm(day.fit, a, b, c, results, y1)
      
    }
  }, silent=T)
  
  # Night Models
  try(for ( i in unique(month.boot.nee.night$Year)){
    for(j in unique(month.boot.nee.night$Month)){
      print("Monthly Model Night")
      print(i)
      print(j)
      
      y1 <-night[which(night$Year == i & night$Month == j),]
      
      try(nee.night <- nls(nee ~ a * exp(b*T_sonic),data=y1
                           , start=list(a= 1 , b=0.01 ),upper=list(a= 5 , b=1),
                           na.action=na.exclude, trace=F, control=nls.control(warnOnly=T)), silent=T)
      
      try(results <- nlsBoot( nee.night, niter=10 ), silent=T)
      try( a <- t(results$estiboot)[1, 1:2], silent=T)
      try(  names(a) <- c('a.est', 'b.est'), silent=T)
      try(b <- t(results$estiboot)[2, 1:2], silent=T)
      try(names(b) <- c('a.se', 'b.se'), silent=T)
      try(c <- t(data.frame(c(a,b))), silent=T)
      try( month.boot.nee.night[c(month.boot.nee.night$Year == i & month.boot.nee.night$Month == j), 13:16] <- c[1, 1:4], silent=T)
      
      for (l in 1:length(boot.NEE$night.model[c(boot.NEE$Year == i & boot.NEE$Month == j & boot.NEE$night.model == "m")])){
        print(l)
        try( boot.NEE[c(boot.NEE$Year == i & boot.NEE$Month == j & boot.NEE$night.model == "m"),][ l, 13:16] <- c[1, 1:4], silent=T)
      }
      
      rm(day.fit, a, b, c, results, y1)
      
    }
  }, silent=T)
  
  #_________________________________________________________________________________
  #_________________________________________________________________________________
  
  # Monthly2 files!!!!
  
  #### Need to remove repetative months-Years from file
  month.boot.nee.day <- boot.NEE[which(boot.NEE$day.model == "m2"),]
  month.boot.nee.night <- boot.NEE[which(boot.NEE$night.model == "m2"),]
  
  # Remove duplicates of year month combinations:
  month.boot.nee.day <-month.boot.nee.day[!duplicated(month.boot.nee.day[c(1,2)]),]
  month.boot.nee.night <-month.boot.nee.night[!duplicated(month.boot.nee.night[c(1,2)]),]
  
  # Day Model:
  try(for ( i in unique(month.boot.nee.day$Year)){
    for(j in unique(month.boot.nee.day$Month)){
      
      print("Monthly Model 2")
      print(i)
      print(j)
      
      a = i
      
      j.1 <- j + 1
      # Correct for the last month in a year:
      if(j.1 == 13){
        j.1 <- 1
        b = as.numeric(a) + 1
      }else{ j.1 <- j.1
      b=a}
      
      
      j.2 <- as.numeric(j) - 1
      
      y1 <- day[which(day$Year == a & day$Month == j),]
      y1 <- rbind(y1, day[which(day$Year == a & day$Month == j.2),])
      y1 <- rbind(y1, day[which(day$Year == b & day$Month == j.1),])
      
      rm(a,b,j.1, j.2)
      
      try(day.fit <- nls( nee ~ (a1 * PAR * ax)/(a1 * PAR + ax) + r, data=y1, 
                          start=list(a1= -0.1 , ax= -7, r= 2),
                          upper=list(a1= 0, ax= -1, r= 10),
                          na.action=na.exclude, trace=F, control=nls.control(warnOnly=T)), silent=T)
      
      try(results <- nlsBoot(day.fit, niter=10 ), silent=T)
      try( a <- t(results$estiboot)[1, 1:3], silent=T)
      try(  names(a) <- c('a1.est', 'ax.est', 'r.est'), silent=T)
      try(b <- t(results$estiboot)[2, 1:3], silent=T)
      try(names(b) <- c('a1.se', 'ax.se', 'r.se'), silent=T)
      try(c <- t(data.frame(c(a,b))), silent=T)
      try( month.boot.nee.day[c(month.boot.nee.day$Year == i & month.boot.nee.day$Month == j), 7:12] <- c[1, 1:6], silent=T)
      
      for (l in 1:length(boot.NEE$day.model[c(boot.NEE$Year == i & boot.NEE$Month == j & boot.NEE$day.model == "m2")])){
        print(l)
        try( boot.NEE[c(boot.NEE$Year == i & boot.NEE$Month == j & boot.NEE$day.model == "m2"),][ l, 7:12] <- c[1, 1:6], silent=T)
      }
      
      rm(day.fit, a, b, c, results, y1)
      
    }
  }, silent=T)
  
  # Night Models
  
  try(for ( i in unique(month.boot.nee.night$Year)){
    for(j in unique(month.boot.nee.night$Month)){
      
      print("Monthly 2 Model Night")
      print(i)
      print(j)
      
      a = i
      j= i
      j.1 <- j + 1
      
      # Correct for the last month in a year:
      if(j.1 == 13 ){
        j.1 <- 1
        b = as.numeric(a) + 1
      }else{ j.1 <- j.1
      b=a}
      
      j.2 <- as.numeric(j) - 1
      
      y1 <- night[which(night$Year == a & night$Month == j),]
      y1 <- rbind(y1, night[which(night$Year == a & night$Month == j.2),])
      y1 <- rbind(y1, night[which(night$Year == b & night$Month == j.1),])
      
      rm(a,b,j.1, j.2)
      
      
      try(nee.night <- nls(nee ~ a * exp(b*T_sonic),data=y1
                           , start=list(a= 1 , b=0.01 ),upper=list(a= 5 , b=1),
                           na.action=na.exclude, trace=F, control=nls.control(warnOnly=T)), silent=T)
      
      try(results <- nlsBoot( nee.night, niter=10 ), silent=T)
      try( a <- t(results$estiboot)[1, 1:2], silent=T)
      try(  names(a) <- c('a.est', 'b.est'), silent=T)
      try(b <- t(results$estiboot)[2, 1:2], silent=T)
      try(names(b) <- c('a.se', 'b.se'), silent=T)
      try(c <- t(data.frame(c(a,b))), silent=T)
      try( month.boot.nee.night[c(month.boot.nee.night$Year == i & month.boot.nee.night$Month == j), 13:16] <- c[1, 1:4], silent=T)
      
      for (l in 1:length(boot.NEE$night.model[c(boot.NEE$Year == i & boot.NEE$Month == j & boot.NEE$night.model == "m2")])){
        print(l)
        try( boot.NEE[c(boot.NEE$Year == i & boot.NEE$Month == j & boot.NEE$night.model == "m2"),][ l, 13:16] <- c[1, 1:4], silent=T)
      }
      
      rm(day.fit, a, b, c, results, y1)
      
    }
  }, silent=T)
  
  #_________________________________________________________________________________
  #_________________________________________________________________________________
  #_________________________________________________________________________________
  #_________________________________________________________________________________
  
  # Seasonal models:
  
  #### Need to remove repetative season-Years from file
  season.boot.nee.day <- boot.NEE[which(boot.NEE$day.model == "s"),]
  season.boot.nee.night <- boot.NEE[which(boot.NEE$night.model == "s"),]
  
  # Remove duplicates of year season combinations:
  season.boot.nee.day <- season.boot.nee.day[!duplicated(season.boot.nee.day[c(1,4)]),]
  season.boot.nee.night <-season.boot.nee.night[!duplicated(season.boot.nee.night[c(1,4)]),]
  
  # Day Model:
  try(for ( i in unique(season.boot.nee.day$Year)){
    for(j in unique(season.boot.nee.day$Season)){
      print("Seasonal Model")
      print(i)
      print(j)
      
      y1 <-day[which(day$Year == i & day$Season == j),]
      
      try(day.fit <- nls( nee ~ (a1 * PAR * ax)/(a1 * PAR + ax) + r, data=y1, 
                          start=list(a1= -0.1 , ax= -7, r= 2),
                          upper=list(a1= 0, ax= -1, r= 10),
                          na.action=na.exclude, trace=F, control=nls.control(warnOnly=T)), silent=T)
      
      try(results <- nlsBoot(day.fit, niter=10 ), silent=T)
      try( a <- t(results$estiboot)[1, 1:3], silent=T)
      try(  names(a) <- c('a1.est', 'ax.est', 'r.est'), silent=T)
      try(b <- t(results$estiboot)[2, 1:3], silent=T)
      try(names(b) <- c('a1.se', 'ax.se', 'r.se'), silent=T)
      try(c <- t(data.frame(c(a,b))), silent=T)
      try( season.boot.nee.day[c(season.boot.nee.day$Year == i & season.boot.nee.day$Season == j), 7:12] <- c[1, 1:6], silent=T)
      
      for (l in 1:length(boot.NEE$day.model[c(boot.NEE$Year == i & boot.NEE$Season == j & boot.NEE$day.model == "s")])){
        print(l)
        try( boot.NEE[c(boot.NEE$Year == i & boot.NEE$Season == j & boot.NEE$day.model == "s"),][ l, 7:12] <- c[1, 1:6], silent=T)
      }
      
      rm(day.fit, a, b, c, results, y1)
      
    }}, silent=T)
  
  # Night Models
  try(for ( i in unique(season.boot.nee.night$Year)){
    for(j in unique(season.boot.nee.night$Season)){
      print("Seasonal Model Night")
      
      print(i)
      print(j)
      
      y1 <-night[which(night$Year == i & night$Season == j),]
      
      try(nee.night <- nls(nee ~ a * exp(b*T_sonic),data=y1
                           , start=list(a= 1 , b=0.01 ),upper=list(a= 5 , b=1),
                           na.action=na.exclude, trace=F, control=nls.control(warnOnly=T)), silent=T)
      
      try(results <- nlsBoot( nee.night, niter=10 ), silent=T)
      try( a <- t(results$estiboot)[1, 1:2], silent=T)
      try(  names(a) <- c('a.est', 'b.est'), silent=T)
      try(b <- t(results$estiboot)[2, 1:2], silent=T)
      try(names(b) <- c('a.se', 'b.se'), silent=T)
      try(c <- t(data.frame(c(a,b))), silent=T)
      
      try( month.boot.nee.night[c(month.boot.nee.night$Year == i & month.boot.nee.night$Season == j), 13:16] <- c[1, 1:4], silent=T)
      
      for (l in 1:length(boot.NEE$night.model[c(boot.NEE$Year == i & boot.NEE$Season == j & boot.NEE$night.model == "s")])){
        print(l)
        try( boot.NEE[c(boot.NEE$Year == i & boot.NEE$Season == j & boot.NEE$night.model == "s"),][ l, 13:16] <- c[1, 1:4], silent=T)
      }
      
      rm(day.fit, a, b, c, results, y1)
      
    }}, silent=T)
  
  #_________________________________________________________________________________
  #_________________________________________________________________________________
  
  # Annual models:
  
  #### Need to remove repetative annual from file
  annual.boot.nee.day <- boot.NEE[which(boot.NEE$day.model == "s"),]
  annual.boot.nee.night <- boot.NEE[which(boot.NEE$night.model == "s"),]
  
  # Remove duplicates of annual combinations:
  annual.boot.nee.day <- annual.boot.nee.day[!duplicated(annual.boot.nee.day[c(1)]),]
  annual.boot.nee.night <-annual.boot.nee.night[!duplicated(annual.boot.nee.night[c(1)]),]
  
  # Day Model:
  try(for ( i in unique(annual.boot.nee.day$Year)){
    print("Annual Model")
    print(i)
    
    y1 <-day[which(day$Year == i),]
    
    try(day.fit <- nls( nee ~ (a1 * PAR * ax)/(a1 * PAR + ax) + r, data=y1, 
                        start=list(a1= -0.1 , ax= -7, r= 2),
                        upper=list(a1= 0, ax= -1, r= 10),
                        na.action=na.exclude, trace=F, control=nls.control(warnOnly=T)), silent=T)
    
    try(results <- nlsBoot(day.fit, niter=10 ), silent=T)
    try( a <- t(results$estiboot)[1, 1:3], silent=T)
    try(  names(a) <- c('a1.est', 'ax.est', 'r.est'), silent=T)
    try(b <- t(results$estiboot)[2, 1:3], silent=T)
    try(names(b) <- c('a1.se', 'ax.se', 'r.se'), silent=T)
    try(c <- t(data.frame(c(a,b))), silent=T)
    try( season.boot.nee.day[c(season.boot.nee.day$Year == i), 7:12] <- c[1, 1:6], silent=T)
    
    for (l in 1:length(boot.NEE$day.model[c(boot.NEE$Year == i & boot.NEE$day.model == "s")])){
      print(l)
      try( boot.NEE[c(boot.NEE$Year == i & boot.NEE$day.model == "s"),][ l, 7:12] <- c[1, 1:6], silent=T)
    }
    
    rm(day.fit, a, b, c, results, y1)
  }, silent=T)
  
  # Night Models
  try(for ( i in unique(annual.boot.nee.night$Year)){
    
    print("Annual Model Night")
    
    print(i)
    
    y1 <-night[which(night$Year == i),]
    
    try(nee.night <- nls(nee ~ a * exp(b*T_sonic),data=y1
                         , start=list(a= 1 , b=0.01 ),upper=list(a= 5 , b=1),
                         na.action=na.exclude, trace=F, control=nls.control(warnOnly=T)), silent=T)
    
    try(results <- nlsBoot( nee.night, niter=10 ), silent=T)
    try( a <- t(results$estiboot)[1, 1:2], silent=T)
    try(  names(a) <- c('a.est', 'b.est'), silent=T)
    try(b <- t(results$estiboot)[2, 1:2], silent=T)
    try(names(b) <- c('a.se', 'b.se'), silent=T)
    try(c <- t(data.frame(c(a,b))), silent=T)
    
    try( month.boot.nee.night[c(month.boot.nee.night$Year == i), 13:16] <- c[1, 1:4], silent=T)
    
    for (l in 1:length(boot.NEE$night.model[c(boot.NEE$Year == i & boot.NEE$night.model == "s")])){
      print(l)
      try( boot.NEE[c(boot.NEE$Year == i & boot.NEE$night.model == "s"),][ l, 13:16] <- c[1, 1:4], silent=T)
    }
    
    rm(day.fit, a, b, c, results, y1)
  }, silent=T)
  
  #_________________________________________________________________________________
  #_________________________________________________________________________________
  #_________________________________________________________________________________
  #_________________________________________________________________________________
  
  # Seasonal Global models files!!!!
  
  #### Need to remove repetative months-Years from file
  season2.boot.nee.day <- boot.NEE[which(boot.NEE$day.model == "g"),]
  season2.boot.nee.night <- boot.NEE[which(boot.NEE$night.model == "g"),]
  
  # Remove duplicates of year month combinations:
  season2.boot.nee.day <- season2.boot.nee.day[!duplicated(season2.boot.nee.day[c(4)]),]
  season2.boot.nee.night <-season2.boot.nee.night[!duplicated(season2.boot.nee.night[c(4)]),]
  
  # Day Model:
  try(for(j in unique(season2.boot.nee.day$Season)){
    print("Seasonal Model")
    print(j)
    
    y1 <-day[which(day$Season == j),]
    
    try(day.fit <- nls( nee ~ (a1 * PAR * ax)/(a1 * PAR + ax) + r, data=y1, 
                        start=list(a1= -0.1 , ax= -7, r= 2),
                        upper=list(a1= 0, ax= -1, r= 10),
                        na.action=na.exclude, trace=F, control=nls.control(warnOnly=T)), silent=T)
    
    try(results <- nlsBoot(day.fit, niter=10 ), silent=T)
    try( a <- t(results$estiboot)[1, 1:3], silent=T)
    try(  names(a) <- c('a1.est', 'ax.est', 'r.est'), silent=T)
    try(b <- t(results$estiboot)[2, 1:3], silent=T)
    try(names(b) <- c('a1.se', 'ax.se', 'r.se'), silent=T)
    try(c <- t(data.frame(c(a,b))), silent=T)
    try( season2.boot.nee.day[c( season2.boot.nee.day$Season == j), 7:12] <- c[1, 1:6], silent=T)
    
    for (l in 1:length(boot.NEE$day.model[c(boot.NEE$Season == j & boot.NEE$day.model == "g")])){
      print(l)
      try( boot.NEE[c(boot.NEE$Season == j & boot.NEE$day.model == "g"),][ l, 7:12] <- c[1, 1:6], silent=T)
    }
    
    rm(day.fit, a, b, c, results, y1)
  }, silent=T)
  
  # Night Models
  try(for ( j in unique(season2.boot.nee.night$Season)){
    print("Seasonal Model Night")
    print(j)
    
    y1 <-night[which(night$Season == j),]
    
    try(nee.night <- nls(nee ~ a * exp(b*T_sonic),data=y1
                         , start=list(a= 1 , b=0.01 ),upper=list(a= 5 , b=1),
                         na.action=na.exclude, trace=F, control=nls.control(warnOnly=T)), silent=T)
    
    try(results <- nlsBoot( nee.night, niter=10 ), silent=T)
    try( a <- t(results$estiboot)[1, 1:2], silent=T)
    try(  names(a) <- c('a.est', 'b.est'), silent=T)
    try(b <- t(results$estiboot)[2, 1:2], silent=T)
    try(names(b) <- c('a.se', 'b.se'), silent=T)
    try(c <- t(data.frame(c(a,b))), silent=T)
    
    try( season2.boot.nee.night[c( season2.boot.nee.night$Season == j), 13:16] <- c[1, 1:4], silent=T)
    
    for (l in 1:length(boot.NEE$night.model[c( boot.NEE$Season == j & boot.NEE$night.model == "g")])){
      print(l)
      try( boot.NEE[c( boot.NEE$Season == j & boot.NEE$night.model == "g"),][ l, 13:16] <- c[1, 1:4], silent=T)
    }
    
    rm(day.fit, a, b, c, results, y1)
  }, silent=T)
  
  rm(annual.boot.nee.day, annual.boot.nee.night, month.boot.nee.day, month.boot.nee.night,
      season.boot.nee.day, season.boot.nee.night, season2.boot.nee.day, season2.boot.nee.night,
      week.boot.nee.day, week2.boot.nee.night, week.boot.nee.night, week2.boot.nee.night)
 

  #return(boot.NEE)
  rm(x, parms)
  
  save(ts7.flux,ts7.met, ts7, nee.parms,le.parms, h.parms,boot.NEE, file = "~/OneDrive - Florida International University/Research/Flux Towers/TSPH7/TS7.RData")
  
