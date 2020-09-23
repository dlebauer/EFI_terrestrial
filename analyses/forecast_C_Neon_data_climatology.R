
# install neonUtilities - can skip if already installed
# load neonUtilities
library(neonUtilities)
library(rhdf5)
library(BiocManager)
library(lubridate)
library(ggpubr)

options(stringsAsFactors=F)

# download data
# Eddy tower data: "DP4.00200.001"
# precip data:  DP1.00006.001
# Tower PAR data DP1.00024.001
# bio temp: DP2.0004.001

# download zip file
zipsByProduct(dpID= c("DP1.00007.001"), package="basic", 
              site=c("BART","KONZ","SRER"), 
              startdate="2015-01", enddate="2019-12",
              savepath="C:\\Users\\aryoung\\Desktop\\EFI\\Downloads", 
              check.size=F)


# set your working directory to the download location. 
setwd("C:\\Users\\aryoung\\Desktop\\EFI\\Downloads")


#### Co2 and h20 flux
flux <- stackEddy(filepath="filesToStack00200",level="dp04")

names(flux)
WREF<-flux$WREF
WREF$siteID<-"WREF"
BART<-flux$BART
BART$siteID<-"BART"
KONZ<-flux$KONZ
KONZ$siteID<-"KONZ"
SRER<-flux$SRER
SRER$siteID<-"SRER"

# arrange the dataframe
fl<-rbind(SJER, KONZ, WREF,BART)
dim(WREF)

names(fl)
fl$timeBgn <- as.POSIXct(fl$timeBgn, format="%Y-%m-%dT%H:%M:%S", tz="GMT")
fl$DOY<-yday(fl$timeBgn)
fl$Year<-year(fl$timeBgn)

# aggregate to daily sum
co2h2o<-aggregate(list(nsae.Co2=fl$data.fluxCo2.nsae.flux,nsae.H2o=fl$data.fluxH2o.nsae.flux),
                  by=list( siteID=fl$siteID, Year=fl$Year, DOY=fl$DOY), FUN="sum", na.rm=T)

# take out flux > 5000?
co2h2o<-co2h2o[co2h2o$nsae.Co2<5000,]
ggplot(co2h2o, aes(x=DOY, y=nsae.Co2, shape=siteID,col=siteID, alpha=Year))+geom_point()+theme_bw()

# take out flux > 5000?
co2h2o<-co2h2o[co2h2o$nsae.H2o>-5000,]
ggplot(co2h2o, aes(x=DOY, y=nsae.H2o, shape=siteID,col=siteID, alpha=Year))+geom_point()+theme_bw()

write.csv(co2h2o, file="co2h2o_flux_data.csv")


### this is for par
tower.par <- loadByProduct(dpID="DP1.00024.001",
                               site=c("BART","SJER","KONZ"),
                               startdate="2015-01",
                               enddate="2019-12")

names(tower.par)
tp30<-tower.par$PARPAR_30min

tp30$DOY<-yday(tp30$startDateTime)
tp30$Year<-year(tp30$startDateTime)
table(tp30$verticalPosition)
# there are vertical positions on the towers-  I summed the total daily PAR below with aggregate

# aggregate to daily sum
par<-aggregate(list(PARMean=tp30$PARMean,PARExpUncert=tp30$PARExpUncert),
               by=list( siteID=tp30$siteID, Year=tp30$Year, DOY=tp30$DOY), FUN="sum", na.rm=T)
ggplot(par, aes(x=DOY, y=PARMean, col=siteID, alpha=Year))+geom_point()+theme_bw()

write.csv(par, file="par_data.csv")


##############################################
##  this is for precip  
## read in precipitation dataset

sensor.precip <- loadByProduct(dpID="DP1.00006.001",
                    site=c("BART","SJER","KONZ"),
                    startdate="2015-01",
                    enddate="2019-12")
names(sensor.precip)
var<-sensor.precip$variables_00006
var[, c("description","table")]
var$description
var[var$table=="THRPRE_30min", ]

sp30<-sensor.precip$THRPRE_30min
names(sp30)
sp30$DOY<-yday(sp30$startDateTime)
sp30$Year<-year(sp30$startDateTime)
table(sp30$verticalPosition)
# Just one sensor- The top of the tower I believe?

# aggregate to daily sum
precip<-aggregate(list(TFPrecipBulk=sp30$TFPrecipBulk,TFPrecipExpUncert=sp30$TFPrecipExpUncert),
          by=list( siteID=sp30$siteID, Year=sp30$Year, DOY=sp30$DOY), FUN="sum", na.rm=T)
ggplot(precip, aes(x=DOY, y=TFPrecipBulk, col=siteID, alpha=Year))+geom_point()+theme_bw()+scale_y_log10()


write.csv(precip, file="precip_data.csv")
#############################################


### this is for soil moisture
soil.moisture <- loadByProduct(dpID="DP1.00094.001",
                    site=c("BART","SJER","KONZ"),
                    startdate="2015-01",
                    enddate="2019-12")

names(soil.moisture)
sm30<-soil.moisture$SWS_30_minute
names(sm30)  
  sm30$DOY<-yday(sm30$startDateTime)
sm30$Year<-year(sm30$startDateTime)
table(sm30$verticalPosition)
# there are vertical positions on the towers-  I summed the total daily PAR below with aggregate

head(sm30)
# aggregate to daily sum
somo<-aggregate(list(    VSWCMean=sm30$VSWCMean  ,   VSWCExpUncert=sm30$VSWCExpUncert   ),
               by=list( siteID=sm30$siteID, Year=sm30$Year, DOY=sm30$DOY), FUN="sum", na.rm=T)

ggplot(somo, aes(x=DOY, y=VSWCMean, col=siteID, alpha=Year))+geom_point()+theme_bw()
write.csv(somo, file="soil_moisture_data.csv")

##################################################################################

### this is for biological temp
ir.temp <- loadByProduct(dpID="DP1.00005.001",
                               site=c("BART","SJER","KONZ"),
                               startdate="2015-01",
                               enddate="2019-12")
bt30<-ir.temp$IRBT_30_minute

bt30$DOY<-yday(bt30$startDateTime)
bt30$Year<-year(bt30$startDateTime)
table(bt30$verticalPosition)
# there are vertical positions on the towers-  I summed the total daily PAR below with aggregate

# aggregate to daily sum
temp<-aggregate(list(    bioTempMean=bt30$bioTempMean  ,   bioTempExpUncert=bt30$bioTempExpUncert   ),
                by=list( siteID=bt30$siteID, Year=bt30$Year, DOY=bt30$DOY), FUN="mean", na.rm=T)

ggplot(temp, aes(x=DOY, y=bioTempMean, col=siteID, alpha=Year))+geom_point()+theme_bw()

write.csv(temp, file="temp_data.csv")

#3
### this is for humidity
tower.humidity <- loadByProduct(dpID="DP1.00098.001",
                               site=c("BART","SJER","KONZ"),
                               startdate="2015-01",
                               enddate="2019-12")

rh30<-tower.humidity$RH_30min

names(rh30)  
rh30$DOY<-yday(rh30$startDateTime)
rh30$Year<-year(rh30$startDateTime)
table(rh30$verticalPosition)
# there are vertical positions on the towers-  I summed the total daily PAR below with aggregate

head(rh30)
# aggregate to daily sum
humid<-aggregate(list(    RHMean =rh30$RHMean   ,   RHExpUncert =rh30$RHExpUncert    ),
                by=list( siteID=rh30$siteID, Year=rh30$Year, DOY=rh30$DOY), FUN="mean", na.rm=T)

ggplot(humid, aes(x=DOY, y=RHMean, col=siteID, alpha=Year))+geom_point()+theme_bw()+geom_smooth()

write.csv(humid, file="humidity_data.csv")



#33  below is for nice net CO2 and H2- flux graphs
g1<-ggplot(fl, aes(x=timeB, y=data.fluxCo2.nsae.flux))+geom_point(size=.2, col="forest green")+
  facet_wrap(~siteID, nrow=3,scales="free_y")+theme_bw()+theme(text=element_text(size=18))+
  xlab("")+ylim(-40,40)+ggtitle("Net surface atmosphere exchange CO2")+ylab("Co2 flux (umolCo2 m-2 s-1)")+
  theme(legend.position="bottom")+guides(colour = guide_legend(override.aes = list(size=10)))
g1


w1<-ggplot(fl, aes(x=timeB, y=data.fluxH2o.nsae.flux))+geom_point(size=.2, col="blue")+
  facet_wrap(~siteID, nrow=3,scales="free_y")+theme_bw()+theme(text=element_text(size=18))+
  xlab("")+ylim(-200,900)+ggtitle("Net surface atmosphere exchange H2o")+ylab("H2o flux (W m-2)")+
  theme(legend.position="bottom")+guides(colour = guide_legend(override.aes = list(size=10)))
w1


week26<-fl[fl$week=="26",]

head(week26)
g2<-ggplot(week26, aes(x=timeB, y=data.fluxCo2.nsae.flux))+geom_point(col="forest green")+
  facet_wrap(~siteID, nrow=3,scales="free_y")+theme_bw()+theme(text=element_text(size=18))+
  xlab("")+ylim(-40,40)+geom_line()+ggtitle("1 week in June 2019")+ylab("Co2 flux (umolCo2 m-2 s-1)")+
  theme(legend.position="bottom")+guides(colour = guide_legend(override.aes = list(size=10)))
g2

w2<-ggplot(week26, aes(x=timeB, y=data.fluxH2o.nsae.flux))+geom_point(col="blue")+
  facet_wrap(~siteID, nrow=3,scales="free_y")+theme_bw()+theme(text=element_text(size=18))+
  xlab("")+ylim(-200,900)+geom_line()+ggtitle("1 week in June 2019")+ylab("H2o flux (W m-2)")+
  theme(legend.position="bottom")+guides(colour = guide_legend(override.aes = list(size=10)))
w2



ggarrange(g1, g2, w1, w2)






## this downloads soil moisture but I didn not get to it.
zipsByProduct(dpID= "DP1.00094.001", package="basic", 
              site=c("BART","KONZ","SRER"), 
              startdate="2019-05", enddate="2019-05",
              savepath="C:\\Users\\Dropcopter2\\Documents\\R\\EFI", 
              check.size=F)


soil.moisture<-stackByTable(filepath = "C:\\Users\\Dropcopter2\\Documents\\R\\EFI\\filesToStack00094")

BART<-soil.moisture$BART
BART$siteID<-"BART"
KONZ<-soil.moisture$KONZ
KONZ$siteID<-"KONZ"
SRER<-soil.moisture$SRER
SRER$siteID<-"SRER"


# bring all sites together
sm<-rbind(SRER, KONZ, BART)
names(sm)


