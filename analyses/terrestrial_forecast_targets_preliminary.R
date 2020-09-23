

###### Forecasting terrestrial soil moisture, Co2 flux, and H20 flux

## This script makes 4 target dataframes
## for soil moisture: target_30m_moisture  ; target_daily_moisture
## for Co2 and H2o flux: half.hour.flux.target ; daily.flux.target


library(neonUtilities)
library(lubridate)
library(ggplot2)
options(stringsAsFactors = FALSE)

###### Soil moisture target


## sd has Sensor Depths
sensor_depth <-
  read.csv("inputs/SWC_depths.csv")

# multiply negative depth values by -1 for easier math
sensor_depth$sensorDepths <- sensor_depth$sensorDepths * -1
# only use the first sensor per site (typically the closest to each tower)
sensor_depth <- sensor_depth[sensor_depth$plot == 1, ]
# site-plot-meas. Used for matching up sensor depths
sensor_depth$spd <-
  paste(
    sensor_depth$site,
    sensor_depth$plot,
    sensor_depth$measurementLevel
  )

### calculated sensor widths for calculating their weight for weighted average
# BART
bart_sd <- sensor_depth[sensor_depth$site == "BART", ]
bart_sd$width <- 0 # made a new column, a numerical placeholderto be filled with sensor widths
bart_sd[1, 9] <- ((bart_sd[2, 5] - bart_sd[1, 5]) / 2) + bart_sd[1, 5]
bart_sd[2, 9] <- ((bart_sd[2, 5] - bart_sd[1, 5]) / 2) + ((bart_sd[3, 5] - bart_sd[2, 5]) / 2)
bart_sd[3, 9] <- ((bart_sd[3, 5] - bart_sd[2, 5]) / 2) + ((bart_sd[4, 5] - bart_sd[3, 5]) / 2)
bart_sd[4, 9] <- ((bart_sd[4, 5] - bart_sd[3, 5]) / 2) + ((bart_sd[5, 5] - bart_sd[4, 5]) / 2)
bart_sd[5, 9] <- ((bart_sd[5, 5] - bart_sd[4, 5]) / 2) + ((bart_sd[6, 5] - bart_sd[5, 5]) / 2)
bart_sd[6, 9] <- ((bart_sd[6, 5] - bart_sd[5, 5]) / 2) + ((bart_sd[7, 5] - bart_sd[6, 5]) / 2)
bart_sd[7, 9] <- ((bart_sd[7, 5] - bart_sd[6, 5]) / 2) + ((bart_sd[8, 5] - bart_sd[7, 5]) / 2)
bart_sd[8, 9] <- ((bart_sd[8, 5] - bart_sd[7, 5]) / 2)
bart_sd$weight <- bart_sd$width / bart_sd[8, 5] # divide by the lowest sensor depth

# OSBS
osbs_sd <- sensor_depth[sensor_depth$site == "OSBS", ]
osbs_sd$width <- 0 # made a new column, a numerical placeholderto be filled with sensor widths
osbs_sd[1, 9] <- ((osbs_sd[2, 5] - osbs_sd[1, 5]) / 2) + osbs_sd[1, 5]
osbs_sd[2, 9] <- ((osbs_sd[2, 5] - osbs_sd[1, 5]) / 2) + ((osbs_sd[3, 5] - osbs_sd[2, 5]) / 2)
osbs_sd[3, 9] <- ((osbs_sd[3, 5] - osbs_sd[2, 5]) / 2) + ((osbs_sd[4, 5] - osbs_sd[3, 5]) / 2)
osbs_sd[4, 9] <- ((osbs_sd[4, 5] - osbs_sd[3, 5]) / 2) + ((osbs_sd[5, 5] - osbs_sd[4, 5]) / 2)
osbs_sd[5, 9] <- ((osbs_sd[5, 5] - osbs_sd[4, 5]) / 2) + ((osbs_sd[6, 5] - osbs_sd[5, 5]) / 2)
osbs_sd[6, 9] <- ((osbs_sd[6, 5] - osbs_sd[5, 5]) / 2) + ((osbs_sd[7, 5] - osbs_sd[6, 5]) / 2)
osbs_sd[7, 9] <- ((osbs_sd[7, 5] - osbs_sd[6, 5]) / 2) + ((osbs_sd[8, 5] - osbs_sd[7, 5]) / 2)
osbs_sd[8, 9] <- ((osbs_sd[8, 5] - osbs_sd[7, 5]) / 2)
osbs_sd$weight <- osbs_sd$width / osbs_sd[8, 5] # divide by the lowest sensor depth

# SRER
srer_sd <- sensor_depth[sensor_depth$site == "SRER", ]
srer_sd$width <- 0 # made a new column, a numerical placeholderto be filled with sensor widths
srer_sd[1, 9] <- ((srer_sd[2, 5] - srer_sd[1, 5]) / 2) + srer_sd[1, 5]
srer_sd[2, 9] <- ((srer_sd[2, 5] - srer_sd[1, 5]) / 2) + ((srer_sd[3, 5] - srer_sd[2, 5]) / 2)
srer_sd[3, 9] <- ((srer_sd[3, 5] - srer_sd[2, 5]) / 2) + ((srer_sd[4, 5] - srer_sd[3, 5]) / 2)
srer_sd[4, 9] <- ((srer_sd[4, 5] - srer_sd[3, 5]) / 2) + ((srer_sd[5, 5] - srer_sd[4, 5]) / 2)
srer_sd[5, 9] <- ((srer_sd[5, 5] - srer_sd[4, 5]) / 2) + ((srer_sd[6, 5] - srer_sd[5, 5]) / 2)
srer_sd[6, 9] <- ((srer_sd[6, 5] - srer_sd[5, 5]) / 2) + ((srer_sd[7, 5] - srer_sd[6, 5]) / 2)
srer_sd[7, 9] <- ((srer_sd[7, 5] - srer_sd[6, 5]) / 2) + ((srer_sd[8, 5] - srer_sd[7, 5]) / 2)
srer_sd[8, 9] <- ((srer_sd[8, 5] - srer_sd[7, 5]) / 2)
srer_sd$weight <- srer_sd$width / srer_sd[8, 5] # divide by the lowest sensor depth

# KONZ
konz_sd <- sensor_depth[sensor_depth$site == "KONZ", ]
konz_sd$width <- 0 # made a new column, a numerical placeholderto be filled with sensor widths
konz_sd[1, 9] <- ((konz_sd[2, 5] - konz_sd[1, 5]) / 2) + konz_sd[1, 5]
konz_sd[2, 9] <- ((konz_sd[2, 5] - konz_sd[1, 5]) / 2) + ((konz_sd[3, 5] - konz_sd[2, 5]) / 2)
konz_sd[3, 9] <- ((konz_sd[3, 5] - konz_sd[2, 5]) / 2) + ((konz_sd[4, 5] - konz_sd[3, 5]) / 2)
konz_sd[4, 9] <- ((konz_sd[4, 5] - konz_sd[3, 5]) / 2) + ((konz_sd[5, 5] - konz_sd[4, 5]) / 2)
konz_sd[5, 9] <- ((konz_sd[5, 5] - konz_sd[4, 5]) / 2) + ((konz_sd[6, 5] - konz_sd[5, 5]) / 2)
konz_sd[6, 9] <- ((konz_sd[6, 5] - konz_sd[5, 5]) / 2) + ((konz_sd[7, 5] - konz_sd[6, 5]) / 2)
konz_sd[7, 9] <- ((konz_sd[7, 5] - konz_sd[6, 5]) / 2) + ((konz_sd[8, 5] - konz_sd[7, 5]) / 2)
konz_sd[8, 9] <- ((konz_sd[8, 5] - konz_sd[7, 5]) / 2)
konz_sd$weight <- konz_sd$width / konz_sd[8, 5] # divide by the lowest sensor depth

weighted_sensors <- rbind(bart_sd, osbs_sd, srer_sd, konz_sd)
weighted_sensors


## Load soil moisture data to R environment
sm <- loadByProduct(
  dpID = "DP1.00094.001",
  site = c("BART", "SRER", "KONZ", "OSBS"),
  startdate = "2019-07",
  enddate = "2019-08", check.size = FALSE
)


# Use the half hourly data
sm30 <- sm$SWS_30_minute
# turn into numeric for later matching with sensor depth measurements
sm30$horizontalPosition <- as.numeric(sm30$horizontalPosition)
sm30$verticalPosition <- as.numeric(sm30$verticalPosition)
# subtract 500 to get rid of the leading '50_' in the vertical position codes.
sm30$verticalPosition <- sm30$verticalPosition - 500
# add day of year
sm30$day <- yday(sm30$startDateTime)


## subset to only use closest sensor (sensor 1 is typically the closest)
sensor1 <- sm30[sm30$horizontalPosition == "1", ]
# make a 'spd' site, plot, depth code to match in the sensor depths for any NEON site
sensor1$spd <-
  paste(
    sensor1$siteID,
    sensor1$horizontalPosition,
    sensor1$verticalPosition
  )

# graph data to view if you'd like
# ggplot(sensor1, aes(x=startDateTime, y=VSWCMean, col=as.factor(verticalPosition)))+geom_point()+facet_wrap(~siteID)

# match the sensor weights from sd into sensor 1 by 'spd'
sensor1$weight <-
  weighted_sensors$weight[match(sensor1$spd, weighted_sensors$spd)]

## Weight each sensor by the proportion of the depth profile it represents
sensor1$weighted_moisture <- sensor1$VSWCMean * sensor1$weight

## take the sum of the weighted-average soil moisture for each sensor station
wt_avg_soil_moisture <-
  aggregate(
    list(moisture = sensor1$weighted_moisture),
    by =
      list(
        siteID = sensor1$siteID,
        startDateTime = sensor1$startDateTime,
        day = sensor1$day,
        spd = sensor1$spd
      ),
    FUN = "sum",
    na.rm = TRUE
  )

#### calculate the weighted uncertainty? ----------------------------------------------
#** # Check to see if this is right!
#### Divide each sensor by 2 to undo NEON processing. Does this produce the standard deviation?
sensor1$uncert_std_dev <- sensor1$VSWCExpUncert / 2
# weight the standard deviation by sensor width proportion (weight)
sensor1$weighted_std_dev <- sensor1$uncert_std_dev
# square the values
sensor1$sq_weighted_std_dev <- (sensor1$weighted_std_dev)^2

# sum(?) the 8 sensor uncertainties
sensor_moisture_uncert <-
  aggregate(
    list(sq_uncert = sensor1$sq_weighted_std_dev),
    by =
      list(
        siteID = sensor1$siteID,
        startDateTime = sensor1$startDateTime,
        day = sensor1$day,
        spd = sensor1$spd
      ),
    FUN = "sum",
    na.rm = TRUE
  )

# take the square root of the sum of squared, weighted uncertainty to produce the standard deviation
sensor_moisture_uncert$std_dev_uncert <-
  sqrt(sensor_moisture_uncert$sq_uncert)

### target for half hour soil moisture
wt_avg_soil_moisture$std_dev_uncert <-
  sensor_moisture_uncert$std_dev_uncert[match(wt_avg_soil_moisture$spd, sensor_moisture_uncert$spd)]

# make a well labeled object for the half hour target
target_30m_moisture <- wt_avg_soil_moisture

## take the average of the depth-weighted soil moisture and uncertainty for each day
target_daily_moisture <- aggregate(
  list(
    moisture = target_30m_moisture$moisture,
    uncertainty = target_30m_moisture$std_dev_uncert
  ),
  by = list(siteID = sensor1$siteID, day = sensor1$day),
  FUN = "mean",
  na.rm = TRUE
)



########## Co2 and H20 flux targets

## download bundled data product from NEON API

zipsByProduct(
  dpID = c("DP4.00200.001"),
  package = "basic",
  site = c("BART", "KONZ", "SRER", "OSBS"),
  startdate = "2019-06",
  enddate = "2019-07",
  savepath = "outputs/",
  check.size = FALSE
)

## read in the zipped files
flux <- stackEddy(filepath = "outputs/filesToStack00200", level = "dp04")

# the object flux has each site in a slot. Add siteID then rbind them into one df called 'fl'
OSBS <- flux$OSBS
OSBS$siteID <- "OSBS"
BART <- flux$BART
BART$siteID <- "BART"
KONZ <- flux$KONZ
KONZ$siteID <- "KONZ"
SRER <- flux$SRER
SRER$siteID <- "SRER"

# arrange the dataframe with all sites
fl <- rbind(OSBS, KONZ, SRER, BART)
names(fl)

## add Day Of Year and Year
fl$DOY <- yday(fl$timeBgn)
fl$Year <- year(fl$timeBgn)

## this is the half hour target ** without uncertainty
names(fl)

half.hour.flux.target <-
  fl[, c(
    "siteID",
    "Year",
    "DOY",
    "timeBgn",
    "data.fluxCo2.nsae.flux",
    "data.fluxH2o.nsae.flux"
  )]

## calculate daily sum of Co2 and H2o flux

# Take the average half hour flux per day, then multiply by 48 half hour measurements for daily sum
avg.daily <-
  aggregate(
    list(
      nsae.Co2 = fl$data.fluxCo2.nsae.flux,
      nsae.H2o = fl$data.fluxH2o.nsae.flux
    ),
    by = list(
      siteID = fl$siteID,
      Year = fl$Year,
      DOY = fl$DOY
    ),
    FUN = "mean",
    na.rm = TRUE
  )
# create a site-year-date unique ID
avg.daily$syd <-
  paste(avg.daily$siteID, avg.daily$Year, avg.daily$DOY)

# But we need to take out days with fewer than 44 usable observations (a day must have 22 hours of data to be included)
fl$goodc <- as.numeric(is.na(fl$data.fluxCo2.nsae.flux))
fl$goodH2o <- as.numeric(is.na(fl$data.fluxH2o.nsae.flux))

# here I count the number of NA in each day to be able to subset by days with >= 44 obs
t.c <-
  aggregate(
    fl$goodc,
    by = list(
      siteID = fl$siteID,
      Year = fl$Year,
      DOY = fl$DOY
    ),
    FUN = "sum"
  )
t.c$syd <- paste(t.c$siteID, t.c$DOY, t.c$DOY)
use.c <- subset(t.c, x >= 44)
table(use.c$x) # confirm there are values for 44, 45:48 half hour obs only.

t.H2o <-
  aggregate(
    fl$goodH2o,
    by = list(
      siteID = fl$siteID,
      Year = fl$Year,
      DOY = fl$DOY
    ),
    FUN = "sum"
  )
t.H2o$syd <- paste(t.H2o$siteID, t.H2o$DOY, t.H2o$DOY)
use.H2o <- subset(t.H2o, x >= 44)
table(use.H2o$x) # confirm there are values for 44, 45:48 half hour obs only.

# To avoid using days with fewer than 44 obs, match data from avg. daily into the use.c dataframe that only has days with >=44
use.c$nsae.Co2 <- avg.daily$nsae.Co2[match(use.c$syd, avg.daily$syd)]
use.H2o$nsae.H2o <-
  avg.daily$nsae.H2o[match(use.H2o$syd, avg.daily$syd)]

# multiple avg 30 min flux times 48 for cumulative daily flux
use.c$daily.nsae.Co2 <- use.c$nsae.Co2 * 48
use.H2o$daily.nsae.H2o <- use.H2o$nsae.H2o * 48

## create daily flux targets for Co2 and H2o
daily.Co2.flux.target <-
  use.c[, c("siteID", "Year", "DOY", "daily.nsae.Co2")]
daily.H2o.flux.target <-
  use.H2o[, c("siteID", "Year", "DOY", "daily.nsae.H2o")]

## calculate uncertainty in the daily flux values using the half hour observations?
#- take the variance of the half hour obs, then take the square root?
