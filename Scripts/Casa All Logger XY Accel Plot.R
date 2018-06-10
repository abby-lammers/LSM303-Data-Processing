require(lubridate)  # for date cleaning and parsing from strings
require(plotly)     # for interactive plotting

allCSVs <- c('LOG00000.CSV','LOG00001.CSV','LOG00002.CSV','LOG00003.CSV','LOG00004.CSV','LOG00005.CSV','LOG00006.CSV')

ReadRawCavePearlLog <- function(CSVName) {
  RawLog <- read.csv(CSVName, skip = 22, na.strings = FALSE, # skip 22 to trim header
    col.names = c('Datetime','Bat_mV','raw_T','AccX','AccY','AccZ','RawTilt','CompX','CompY','CompZ','CoinCell_mV')) 
  RawLog <- RawLog[1:(nrow(RawLog) - 8),] # trim footer
  RawLog <- RawLog[RawLog$AccX != "" & !is.na(RawLog$AccX) & !is.null(RawLog$AccX), ] # remove inexplicable blank rows
  
  # change column types from factor to date or numeric
  RawLog$Datetime <- lubridate::ymd_hm(RawLog$Datetime) 
  RawLog[,2:ncol(RawLog)] <- lapply(2:ncol(RawLog), function(x) as.numeric(RawLog[[x]]))
  
  RawLog$FileName <- CSVName
  
  return(RawLog)
}

AllRawLogs <- allCSVs %>% lapply(FUN = ReadRawCavePearlLog) %>% bind_rows()

# Examining where the accel data has non-negative signs
CreateOrientationTable <- function(Log) {
  
  OrientationMatrix <- data.frame(
    Xsign = c("pos","pos","neg","neg"),
    Ysign = c("pos","neg","pos","neg"),
    NObs = c(0,0,0,0)
  )
  
  for (ii in 1:nrow(OrientationMatrix)) {
    OrientationRow <- OrientationMatrix[ii,]
    xIndex <- if (OrientationRow$Xsign == "pos") {AllRawLogs$AccX > 0} else {AllRawLogs$AccX < 0}
    yIndex <- if (OrientationRow$Ysign == "pos") {AllRawLogs$AccY > 0} else {AllRawLogs$AccY < 0}
    OrientationMatrix$NObs[ii] <- sum(xIndex & yIndex)
  }
  
  return(OrientationMatrix)
}

OrientationMatrix <- CreateOrientationTable(AllRawLogs)
OrientationMatrix # print 

## plot raw AccX and AccY just to understand orientation (hopefully)
##  takes a very long time to load
AccXY_Plot <- plot_ly(
  data = AllRawLogs,
  x = ~AccX,
  y = ~AccY,
  type = 'scatter',
  mode = 'markers',
  color = ~FileName,
  hoverinfo = 'none'
) 

