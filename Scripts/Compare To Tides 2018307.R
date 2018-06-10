require(dplyr)
require(plotly)
require(lubridate)
require(tidyr)

#### STEP 0: READ CSVS ####

## read tides data 
# see "tides data web scraper 20180307.R" for details
Tides <- read.csv('Belize City Tide Data 2016.csv')
Tides$X <- NULL
Tides$Datetime <- lubridate::ymd_hms(Tides$Datetime)

## read meter data
# see "Windrose With Tilt 20180307.R"
CavePearl <- read.csv('ProcessedAccelMag (Windrose with Tilt 20180307).csv')
CavePearl$X <- NULL
CavePearl$datetime <- lubridate::ymd_hms(CavePearl$datetime)

Angles <- dplyr::select(CavePearl, datetime, tiltAngle, headingDegrees)

#### STEP 1: REGRESSION! ####
# I am NOT emotionally up for this rn sry bud

