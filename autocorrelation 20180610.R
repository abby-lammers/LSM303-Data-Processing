## AUTOCORRELATION
require('lubridate')

# read and parse csv
PuertoTides <- read.csv('App/Data/puerto-morales-all.csv', encoding = 'UTF-8')
PuertoTides$datetime <- mdy_hm(PuertoTides$datetime)

nExpectedObs <- as.period(max(PuertoTides$datetime) - min(PuertoTides$datetime)) / minutes(15)
