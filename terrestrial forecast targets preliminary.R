

###### Forecasting terrestrial soil moisture, Co2 flux, and H20 flux

## This script makes 4 target dataframes 
## for soil moisture: half.hour.moisture.target  ; daily.flux.target
## for Co2 and H2o flux: half.hour.flux.target ; daily.flux.target


library(neonUtilities)
library(lubridate)
library(ggplot2)
options(stringsAsFactors=F)

###### Soil moisture target

## sd has Sensor Depths
sd<-read.csv("C:\\Users\\Dropcopter2\\Downloads\\SWC_depths.csv")
# site-plot-meas. Used for matching up sensor depths
sd$spd<-paste(sd$site, sd$plot, sd$measurementLevel)

## Load soil moisture data to R environment
sm <- loadByProduct(dpID="DP1.00094.001",
                    site=c("BART","SRER","KONZ","OSBS"),
                    startdate="2019-01",
                    enddate="2019-12")
# press 'y' into the console, to download file into R.


# Use the half hourly data
sm30<-sm$SWS_30_minute
# turn into numeric for later matching with sensor depth measurements
sm30$horizontalPosition<-as.numeric(sm30$horizontalPosition)
sm30$verticalPosition<-as.numeric(sm30$verticalPosition)
# subtract 500 to get rid of the leading '50_' in the vertical position codes.
sm30$verticalPosition<-sm30$verticalPosition-500

## subset to only use closest sensor (sensor 1 is typically the closest)
sensor1<-sm30[sm30$horizontalPosition=="1",]
# make a 'spd' site, plot, depth code to match in the sensor depths for any NEON site
sensor1$spd<-paste(sensor1$siteID, sensor1$horizontalPosition, sensor1$verticalPosition)

# graph data to view if you'd like
#ggplot(sensor1, aes(x=startDateTime, y=VSWCMean, col=as.factor(verticalPosition)))+geom_point()+facet_wrap(~siteID)

# match the sensor depths from sd into sensor 1 by 'spd'
sensor1$depth<-sd$sensorDepths[match(sensor1$spd, sd$spd)]

head(sd)

# graph with sensor depth on the y axis
ggplot(sensor1, aes(y=depth, x=VSWCMean))+geom_point()+facet_wrap(~siteID)+ylab("Soil moisture measurent depth")+xlab("soil moisture")

## Weight the importance of each sensor by depth (sensors deeper in the profile are less important)
# sensor depth is max of 2 meters, sensor depths have meters for units.
sensor1$prop<-(1 - (-sensor1$depth/2))
# multiply the sensor water content and uncertainty times the weighting factor
sensor1$soil.moist<-sensor1$prop*sensor1$VSWCMean
sensor1$uncert<-sensor1$prop*sensor1$VSWCExpUncert
sensor1$day<-yday(sensor1$startDateTime)

## take the average of the depth-weighted soil moisture and uncertainty for each sensor 
half.hour.moisture.target<-aggregate(list(moisture=sensor1$soil.moist, ExpUncert=sensor1$uncert), by=
                        list(siteID=sensor1$siteID, startDateTime=sensor1$startDateTime, day=sensor1$day), FUN="mean", na.rm=T)

## take the average of the depth-weighted soil moisture and uncertainty for each day
daily.moisture.target<-aggregate(list(moisture=sensor1$soil.moist, ExpUncert=sensor1$uncert), by=
                           list(siteID=sensor1$siteID, day=sensor1$day), FUN="mean", na.rm=T)



########## Co2 and H20 flux targets

## download bundled data product from NEON API
zipsByProduct(dpID= c("DP4.00200.001"), package="basic", 
              site=c("BART","KONZ","SRER","OSBS"), 
              startdate="2019-06", enddate="2019-07",
              savepath="C:\\Users\\Dropcopter2\\Documents\\GitHub\\EFI_terrestrial", 
              check.size=F)


# set your working directory to the download location. 
#setwd("C:\\Users\\aryoung\\Desktop\\EFI\\Downloads")
setwd("C:\\Users\\Dropcopter2\\Documents\\GitHub\\EFI_terrestrial")

## read in the zipped files
flux <- stackEddy(filepath="filesToStack00200",level="dp04")
 
# the object flux has each site in a slot. Add siteID then rbind them into one df called 'fl'
OSBS<-flux$OSBS
OSBS$siteID<-"OSBS"
BART<-flux$BART
BART$siteID<-"BART"
KONZ<-flux$KONZ
KONZ$siteID<-"KONZ"
SRER<-flux$SRER
SRER$siteID<-"SRER"

# arrange the dataframe with all sites
fl<-rbind(OSBS, KONZ, SRER,BART)
names(fl)

## add Day Of Year and Year
fl$DOY<-yday(fl$timeBgn)
fl$Year<-year(fl$timeBgn)

## this is the half hour target ** without uncertainty
names(fl)

half.hour.flux.target<-fl[ ,c("siteID","Year","DOY","timeBgn","data.fluxCo2.nsae.flux","data.fluxH2o.nsae.flux")]

## calculate daily sum of Co2 and H2o flux

# Take the average half hour flux per day, then multiply by 48 half hour measurements for daily sum
avg.daily<-aggregate(list(nsae.Co2=fl$data.fluxCo2.nsae.flux,nsae.H2o=fl$data.fluxH2o.nsae.flux),
                  by=list( siteID=fl$siteID, Year=fl$Year, DOY=fl$DOY), FUN="mean", na.rm=T)
# create a site-year-date unique ID
avg.daily$syd<-paste(avg.daily$siteID, avg.daily$Year, avg.daily$DOY)

# But we need to take out days with fewer than 44 usable observations (a day must have 22 hours of data to be included)
fl$goodc<-as.numeric(is.na(fl$data.fluxCo2.nsae.flux))
fl$goodH2o<-as.numeric(is.na(fl$data.fluxH2o.nsae.flux))

# here I count the number of NA in each day
t.c<-aggregate(fl$goodc, by=list(siteID=fl$siteID,Year=fl$Year,DOY=fl$DOY), FUN="sum")
t.c$syd<-paste(t.c$siteID, t.c$DOY, t.c$DOY)
use.c<-subset(t.c, x>=44)
table(use.c$x) #confirm there are values for 44, 45:48 half hour obs only.

t.H2o<-aggregate(fl$goodH2o, by=list(siteID=fl$siteID,Year=fl$Year,DOY=fl$DOY), FUN="sum")
t.H2o$syd<-paste(t.H2o$siteID, t.H2o$DOY, t.H2o$DOY)
use.H2o<-subset(t.H2o, x>=44)
table(use.H2o$x) #confirm there are values for 44, 45:48 half hour obs only.


use.c$nsae.Co2<-avg.daily$nsae.Co2[match(use.c$syd, avg.daily$syd)]
use.H2o$nsae.H2o<-avg.daily$nsae.H2o[match(use.H2o$syd, avg.daily$syd)]

# multiple avg 30 min flux times 48
use.c$daily.nsae.Co2<-use.c$nsae.Co2*48
use.H2o$daily.nsae.H2o<-use.H2o$nsae.H2o*48


## create daily flux targets for Co2 and H2o
daily.Co2.flux.target<-use.c[,c("siteID","Year","DOY","daily.nsae.Co2")]
daily.H2o.flux.target<-use.H2o[,c("siteID","Year","DOY","daily.nsae.H2o")]


