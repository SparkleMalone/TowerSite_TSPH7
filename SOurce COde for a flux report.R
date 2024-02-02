# Un organized code for thereport:

#Looking at gap filled LUT
library(ggplot2)

load( "/Users/sm3466/Dropbox (YSE)/Research/Flux Towers/TSPH7/TS7.RData")


ggplot(data = ts7.co2.lut[which(ts7.co2.lut$model == 'yearmonth'),], aes(x = TA.BINS, y = PAR.BINS, z = co2.pred)) + geom_contour_filled(breaks=seq(-30, 10, 1))
ggplot(data = ts7.co2.lut[which(ts7.co2.lut$model == 'yearseason'),], aes(x = TA.BINS, y = PAR.BINS, z = co2.pred)) + geom_contour_filled(breaks=seq(-30, 10, 1))
ggplot(data = ts7.co2.lut[which(ts7.co2.lut$model == 'month'),], aes(x = TA.BINS, y = PAR.BINS, z = co2.pred)) + geom_contour_filled(breaks=seq(-30, 20, 1))

par(mfrow=c(3,1))
plot(ts7.gapfilled$TIMESTAMP, ts7.gapfilled$nee)
plot(ts7.gapfilled$TIMESTAMP, ts7.gapfilled$nee.mdv, col="green")
plot(ts7.gapfilled$TIMESTAMP, ts7.gapfilled$co2.pred, col="green")

par(mfrow=c(2,1))
plot(ts7.gapfilled$TIMESTAMP, ts7.gapfilled$nee.filled, col="red")
points(ts7.gapfilled$TIMESTAMP, ts7.gapfilled$nee)
plot(ts7.gapfilled$TIMESTAMP, ts7.gapfilled$nee.filled, col="red")






# Daytime summaries
ts7.gapfilled.day <- ts7.gapfilled[ , c("TIMESTAMP","PAR.f", "nee.filled"),]
ts7.gapfilled.day$Date <-format(ts7.gapfilled.day$TIMESTAMP, "%Y-%m-%d")
ts7.gapfilled.day <- ts7.gapfilled.day[which(ts7.gapfilled.day$PAR.f > 10 ),]

ts7.gapfilled.day.day <-ts7.gapfilled.day %>% group_by(Date) %>% summarise( nee.day.mean= mean(nee.filled),
                                                                            nee.day.min= min(nee.filled),
                                                                            nee.day.max= max(nee.filled),
                                                                            nee.day.total= sum(nee.filled))


ts7.fp <- ts7.gapfilled[ , c("TIMESTAMP", "year", 'month',  "nee", "PAR.f", "TA.f", "LE.filled", "H.filled"),]
ts7.fp$Date <-format(ts7.fp$TIMESTAMP, "%Y-%m-%d")
ts7.fp <- ts7.fp %>% left_join(ts7.gapfilled.day.day , by='Date' )



ts7.fp$time <- format(ts7.fp$TIMESTAMP,'%y-%m')
ts7.fp <- ts7.fp[which(ts7.fp$PAR.f <= 0), ]
ts7.fp <-na.omit( ts7.fp)
sample <- sample(c(TRUE, FALSE), nrow(ts7.fp), replace=TRUE, prob=c(0.8,0.2))
train  <- ts7.fp [sample, ]
test   <- ts7.fp [!sample, ]

library(randomForest)

rf.m.reco <- randomForest(data= train, nee ~ TA.f + LE.filled + H.filled + year + month + time +  nee.day.mean + nee.day.min +  nee.day.max + nee.day.total , do.trace=TRUE)
rf.m.reco
varImpPlot(rf.m.reco)

plot(train$nee)

# Evaluate Reco approaches by just looking at nighttime data:
ts7.gapfilled.partitioned <- ts7.gapfilled.partitioned %>% left_join(ts7.gapfilled.day.day , by='Date' )
ts7.gapfilled.partitioned$Reco.pred.rf <- predict( rf.m.reco ,ts7.gapfilled.partitioned )

# Create files and summaries needed for reporting:
summary(lm( ts7.gapfilled.partitioned$Reco.pred ~ts7.gapfilled.partitioned$Reco.pred.rf ))

plot( ts7.gapfilled.partitioned$TIMESTAMP, ts7.gapfilled.partitioned$Reco.pred.rf)
points( ts7.gapfilled.partitioned$TIMESTAMP, ts7.gapfilled.partitioned$Reco.pred, col='red')

plot( ts7.gapfilled.partitioned$TIMESTAMP, ts7.gapfilled.partitioned$gee.umol)


summary(ts7.gapfilled.partitioned$Reco.pred)
summary( ts7.gapfilled.partitioned$gee.umol)

summary( ts7.gapfilled.partitioned$reco)

# add ET to file:





# Time stamps needed for aggregation:
ts7.gapfilled$year <- format(ts7.gapfilled$TIMESTAMP, '%Y')
ts7.gapfilled$year.mon <- format(ts7.gapfilled$TIMESTAMP, '%Y-%m')
ts7.gapfilled$Date <- format(ts7.gapfilled$TIMESTAMP, '%Y-%m-%d')

# create yearmon summary
library(zoo)
ts7.ym <- aggregate(ts7.gapfilled[c('nee.gC')], by=list(ts7.gapfilled$year.mon), FUN="sum", na.rm=TRUE)
ts7.ym$Date <- as.yearmon(ts7.ym$Group.1)

# Daily Summary:
ts7.daily <- aggregate(ts7.gapfilled[c('nee.gC')], by=list(ts7.gapfilled$Date), FUN="sum", na.rm=TRUE)
ts7.daily$Date <- as.Date(ts7.daily$Group.1)

summary(ts7$TIMESTAMP)
plot( ts7.daily$Date, ts7.daily$nee.gC)

# this is where to star
# day night monthly summaries
ts7.day <- ts7[which( ts7$PAR.f > 0),]
ts7.night <- ts7[which( ts7$PAR.f == 0),]

ts7.day.ym <- aggregate(ts7.day[c('nee.gC', 'gee.gC', 'reco.gC')], by=list(ts7.day$year.mon), FUN="sum", na.rm=TRUE)
ts7.day.ym$Date <- as.yearmon(ts7.day.ym$Group.1)

ts7.night.ym <- aggregate(ts7.night[c('nee.gC', 'gee.gC', 'reco.gC')], by=list(ts7.night$year.mon), FUN="sum", na.rm=TRUE)
ts7.night.ym$Date <- as.yearmon(ts7.night.ym$Group.1)

# Subset of data for report:
ts7.report <- ts7[,c("TIMESTAMP", 'NetTot_Avg', 'Albedo_Avg', 'PAR', 'T_sonic', "RH1_Avg", "RH2_Avg", "cm5_Avg", "cm10_Avg", "WS_ms_Avg",
                     "WindDir_D1_WVT", "nee.gC", "LE.filled", "H.filled", "VPD", "ET", "BR")]

ts7.report$BR <- ts7.report$H.filled/ ts7.report$LE.filled

# missiling data summary:
filtering.report <- function(x){
  
  x$date <- as.Date(format(x$TIMESTAMP, format="%m/%d/%y"), format="%m/%d/%y")
  x$year <- format(x$TIMESTAMP, '%Y')
  
  x$gaps <- 0
  x$gaps[is.na(x$nee) | is.na(x$H ) |is.na(x$LE)  ]<- 1
  
  # CO2 Out of Range
  x$Filter.OutRange <- 0 # Create a Filter Index
  x$Filter.OutRange[x$nee.org > 200] <- 1
  x$Filter.OutRange[x$nee.org < -50] <- 1
  x$Filter.OutRange[x$nee.org < -5 & x$PAR.f <10] <- 1
  x$Filter.OutRange[x$nee.org < -15 & x$PAR.f < 500] <- 1
  x$Filter.OutRange[x$nee.org > 50 & x$PAR.f < 500] <- 1
  x$Filter.OutRange[x$nee.org > 20 & x$PAR.f > 500] <- 1
  
  x$Filter.OutRange[x$H.org > 800] <- 1
  x$Filter.OutRange[x$LE.org > 800] <- 1
  x$Filter.OutRange[x$LE.org < -800] <- 1
  x$Filter.OutRange[x$LE.org > 200 & x$Rnet.f < 200] <- 1
  x$Filter.OutRange[x$LE.org< 20 & x$Rnet.f > 400] <- 1
  x$Filter.OutRange[x$LE.org > 300 & x$Rnet.f > 600] <- 1
  x$Filter.OutRange[x$LE.org > 300 & x$Rnet.f < 600] <- 1
  x$Filter.OutRange[x$LE.org < -10 & x$Rnet.f < 400] <- 1
  
  y <- x[c( 'date', 'year', 'gaps','Filter.OutRange')]
  
  
  y$date <-NULL
  y$count <- 1
  y.agg <- aggregate( y[c( 'count','gaps', 'Filter.OutRange')], by=list(y$year), FUN="sum")
  
  return(y.agg)
}

report.gaps <-filtering.report(ts7) 


plot( ts7.daily$Date, ts7.daily$gee.gC, typ="l", col="darkgreen", ylim=c(-5,5),
      ylab= expression(paste("Carbon Flux ( g C m" ^"-2", "day" ^"-1", ")" )) , xlab="")
lines( ts7.daily$Date, ts7.daily$nee.gC, typ="l", col="darkgrey")
lines( ts7.daily$Date, ts7.daily$reco.gC, typ="l", col="black")

legend('topright', legend=c('NEE','GEE', expression("R"[eco])), col=c("darkgrey","darkgreen", "black" ) , lwd=2, bty = "n")


ts7$WUE <- ts7$nee.gC / ts7$ET

# Evergy:
plot( ts7$TIMESTAMP [ts7$Year == '2020'], ts7$WUE[ts7$Year == '2020'], typ="l", col="blue", ylim=c(-400,400),
      ylab= expression(paste("LE ( W m" ^"-2", "s" ^"-1", ")" )) , xlab="")


plot( ts7$TIMESTAMP, ts7$LE.filled, typ="l", col="blue", ylim=c(-400,400),
      ylab= expression(paste("LE ( W m" ^"-2", "s" ^"-1", ")" )) , xlab="")

plot( ts7$TIMESTAMP[ts7$Year == '2020'], ts7$ET[ts7$Year == '2020'], col="blue", typ="l", 
      ylab= expression(paste("ET ( mm m" ^"-2",  ")" )) , xlab="")

plot( ts7$TIMESTAMP, ts7$H.filled, col="black", ylim=c(-400,400), typ="l", 
      ylab= expression(paste("H ( W m" ^"-2", "s" ^"-1", ")" )) , xlab="")

plot( ts7$TIMESTAMP, ts7$BR, col="black", typ="l", ylim=c(-500,500),
      ylab= expression(paste("BR ( W m" ^"-2", "s" ^"-1", ")" )) , xlab="")

write.csv( ts7, '~/OneDrive - Florida International University/Research/Flux Towers/TSPH7/TS7_Data2016-2022.csv')

ts7.daily$year <- format( ts7.daily$Date, "%Y")

library(dplyr)
summary <- group_by(ts7.daily,year) %>% summarise(nee.gC  = sum(nee.gC ),
                                                  gee.gC  = sum(gee.gC ), 
                                                  reco.gC  = sum(reco.gC ))



# check night time respiratopn:

plot(ts7.gapfilled$nee[ ts7.gapfilled$PAR.f < 10])
summary(ts7.gapfilled$nee[ ts7.gapfilled$PAR.f < 10])

summary(ts7.gapfilled$nee[ ts7.gapfilled$PAR.f < 10 & ts7.gapfilled$nee < 0])
length(ts7.gapfilled$nee[ ts7.gapfilled$PAR.f < 10 & ts7.gapfilled$nee < 0])


summary(ts7.gapfilled.partitioned$gee.umol[ ts7.gapfilled.partitioned$PAR.f < 10 & ts7.gapfilled.partitioned$nee < 0])

length(ts7.gapfilled$nee[ ts7.gapfilled$PAR.f < 10 & ts7.gapfilled$nee < 0])

summary(ts7.gapfilled.partitioned$nee.filled[ ts7.gapfilled.partitioned$gee.umol >0])
summary(ts7.gapfilled.partitioned$reco[ ts7.gapfilled.partitioned$gee.umol >0])

summary(ts7.gapfilled.partitioned$PAR.f[ ts7.gapfilled.partitioned$gee.umol >0])

hist(ts7.gapfilled.partitioned$PAR.f[ ts7.gapfilled.partitioned$gee.umol >0], n=2000)
hist(ts7.gapfilled.partitioned$nee.filled[ ts7.gapfilled.partitioned$gee.umol >0], n=2000)
summary(ts7.gapfilled.partitioned$nee.filled[ ts7.gapfilled.partitioned$gee.umol >0])

ts7.gapfilled.partitioned$model.y <- as.factor(ts7.gapfilled.partitioned$model.y)
summary(ts7.gapfilled.partitioned$model.y[ ts7.gapfilled.partitioned$gee.umol >0])




# New stuff:


ts7.co2.test <- lut.PAR.TA(ts7, flux='co2' )
hist(ts7$nee, n=500, xlim=c(-50, 50))

# Three dimensional plots of the look up tables R
library(ggplot2)

ggplot(data = ts7.co2.test , aes(x = TA.BINS, y = PAR.BINS, z = co2.pred)) + geom_contour_filled(bins = 50)

######## code to apply functions:####

ts7.gaps <- gap.length(ts7)
hist(ts7.gaps$gap.length[ts7.gaps$gap.length > 0],)
names(ts7.gaps)
ts7.MDV.5 <- MDV.5(ts7.gaps)

# Joint the LUT with the dataframe to gapfill
srs6.nee <- srs6 %>% left_join(srs6.lut, by=c("PAR.BINS","TA.BINS", "yearmonth"))
srs6.nee$nee.filled <- srs6.nee$nee
srs6.nee$nee.filled[ is.na(srs6.nee$nee.filled)] <- srs6.nee$nee.pred[is.na(srs6.nee$nee.filled)]


plot(srs6.nee$TIMESTAMP, srs6.nee$nee.f, col="red")
points(  srs6.nee$TIMESTAMP, srs6.nee$nee)

plot(srs6.nee$TIMESTAMP, srs6.nee$nee.filled, typ="l")
plot(srs6.nee$TIMESTAMP, srs6.nee$nee.pred, typ="l", col="red")



# Fill all nee data gaps less than 3.5.

plot( srs6.mdv$nee.mdv[srs6.mdv$gap.length <= 5] )

srs6.mdv$nee[ is.na(srs6.mdv$nee) & srs6.mdv$gap.length <= 5] <- srs6.mdv$nee.mdv[ is.na(srs6.mdv$nee) & srs6.mdv$gap.length <= 5] 
srs6.mdv$LE[ is.na(srs6.mdv$LE) & srs6.mdv$gap.length <= 5] <- srs6.mdv$LE.mdv[ is.na(srs6.mdv$LE) & srs6.mdv$gap.length <= 5] 
srs6.mdv$H[ is.na(srs6.mdv$H) & srs6.mdv$gap.length <= 5] <- srs6.mdv$H.mdv[ is.na(srs6.mdv$H) & srs6.mdv$gap.length <= 5] 


srs6.mdv$nee.mdv.y <- srs6.mdv$nee.mdv
srs6 <- srs6.mdv

