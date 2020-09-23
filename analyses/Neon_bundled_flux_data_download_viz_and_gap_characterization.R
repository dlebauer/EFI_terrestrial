## download view and inspect Neon eddy covariance data bundle

# load neonUtilities
library(neonUtilities)
library(rhdf5)
library(BiocManager)
library(lubridate)
library(ggpubr)


options(stringsAsFactors = FALSE)

# download data
# Eddy tower data: "DP4.00200.001"

# download zip file
# specify the savepath someplace Easy to find!
zipsByProduct(
  dpID = c("DP4.00200.001"), package = "basic",
  site = c("SRER", "KONZ", "BART"),
  startdate = "2014-01", enddate = "2020-08",
  savepath = "C:\\Users\\aryoung\\Desktop\\EFI\\Downloads",
  check.size = FALSE
)


# set your working directory to the download location.
setwd("C:\\Users\\aryoung\\Desktop\\EFI\\Downloads")


#### Co2 and h20 flux
flux <- stackEddy(filepath = "filesToStack00200", level = "dp04")

names(flux)

BART <- flux$BART
BART$siteID <- "BART"
KONZ <- flux$KONZ
KONZ$siteID <- "KONZ"
SRER <- flux$SRER
SRER$siteID <- "SRER"

# combine the 3 sites into 1 dataframe
fl <- rbind(SRER, KONZ, BART)

# formatting for easier date manipulation
fl$timeBgn <- as.POSIXct(fl$timeBgn, format = "%Y-%m-%dT%H:%M:%S", tz = "GMT")
fl$DOY <- yday(fl$timeBgn)
fl$Year <- year(fl$timeBgn)
fl$Month <- month(fl$timeBgn)
fl$time <- as.Date(fl$timeBgn)
fl$YD <- paste(fl$Year, fl$DOY, fl$siteID)

# this makes a subset of the variables of interest. and flagging info
efi <- fl[, c(1, 35, 36, 37, 38, 40, 3, 6, 32, 33)]
efi$time <- as.Date(efi$timeBgn) # for graphing in ggplot
efi <- efi[efi$Year > 2016, ] # only bartlett has data in 2016
##
names(efi)


# graph the fluxes for each site
ggplot(efi, aes(x = DOY, y = data.fluxCo2.nsae.flux, col = siteID)) +
  geom_point() +
  theme_bw() +
  facet_grid(siteID ~ Year, scales = "free_x") +
  ggtitle("Net Co2 flux availability for three Neon sites")


# inspect the NA values for Co2 flux
efi$goodc <- is.na(efi$data.fluxCo2.nsae.flux)
table(efi$goodc)
efi$gud[efi$goodc == "TRUE"] <- 0
efi$gud[efi$goodc == "FALSE"] <- 1
efi$gud <- as.numeric(efi$gud)

# here I count the number of NA in each day
t <- aggregate(efi$gud, by = list(YD = efi$YD, Year = efi$Year, DOY = efi$DOY, siteID = efi$siteID), FUN = "sum")
head(t)

# this is a graph with the number of half hour observations per day
ggplot(t, aes(x = DOY, y = x, col = siteID)) +
  geom_point() +
  theme_bw() +
  facet_grid(siteID ~ Year, scales = "free_x") +
  ylab("Number of half hour observations per day ")

v <- t[t$x > 1, ] # take out obs with all missing data

quantile(t$x, .31)

quantile(v$x, .31)

summary(v$x)


ggplot(t, aes(x = x, fill = siteID)) +
  geom_histogram() +
  theme_bw() +
  facet_grid(siteID ~ Year, scales = "free_x") +
  ylab("Number of days") +
  xlab("Number of half hour observations per day")
