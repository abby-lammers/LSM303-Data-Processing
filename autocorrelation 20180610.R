## AUTOCORRELATION
require('lubridate')
require('dplyr')

# read and parse csv
PuertoTides <- read.csv('App/Data/puerto-morales-all.csv', encoding = 'UTF-8')
PuertoTides$datetime <- mdy_hm(PuertoTides$datetime)

nExpectedObs <- as.period(max(PuertoTides$datetime) - min(PuertoTides$datetime)) / minutes(15)

fullDateVec <- as_datetime(as.POSIXct(seq(from = as.numeric(min(PuertoTides$datetime)), to = as.numeric(max(PuertoTides$datetime)), by = as.numeric(minutes(15))),origin = "1970-01-01"))

word