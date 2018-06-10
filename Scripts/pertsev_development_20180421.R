######
# from server.R
require(shiny)
require(plotly)
require(lubridate)
require(dplyr) # imports magrittr (pipe operators "%>%")
require(DT)
require(shinyjs)
require(oce)
require(shinyWidgets)

# include function files
source('LSM303-file-helpers.R')
#####

TSAccelMag <- Read_LSM303_csv(
  fileName = 'OddessyCalibratedWithDateTime.csv',
  crop = 3000
) %>% 
  Normalize_Accel(cal = TRUE) %>% 
  Get_Accel_Angles() %>% 
  Normalize_Mag(cal = TRUE) %>% 
  Compensate_Mag_Field() %>% 
  Get_Heading() %>% 
  arrange(datetime) %>% 
  Round_TSAccelMag()

## pertsev
## TODO: some mathematical mismatch
TSAccelMag$Pertsev <- lead(TSAccelMag$tiltAngle, 18) +
  lead(TSAccelMag$tiltAngle, 13) +
  lead(TSAccelMag$tiltAngle, 10) +
  lead(TSAccelMag$tiltAngle, 5) +
  lead(TSAccelMag$tiltAngle, 3) +
  lead(TSAccelMag$tiltAngle, 2) +
  TSAccelMag$tiltAngle +
  lag(TSAccelMag$tiltAngle, 18) +
  lag(TSAccelMag$tiltAngle, 13) +
  lag(TSAccelMag$tiltAngle, 10) +
  lag(TSAccelMag$tiltAngle, 5) +
  lag(TSAccelMag$tiltAngle, 3) +
  lag(TSAccelMag$tiltAngle, 2)
TSAccelMag$Pertsev <- TSAccelMag$Pertsev / 15
  

p <- plot_ly(
  data = TSAccelMag,
  x = ~datetime,
  y = ~tiltAngle,
  type = 'scatter',
  mode = 'lines',
  line = list(color = 'blue')
) %>% add_trace(
  y = ~Pertsev,
  line = list(color = 'green')
)

# Hourly Pertsev
HourData <- TSAccelMag %>% filter(minute(datetime) == 0)
HourData$Pertsev <- lead(HourData$tiltAngle, 18) +
  lead(HourData$tiltAngle, 13) +
  lead(HourData$tiltAngle, 10) +
  lead(HourData$tiltAngle, 8) +
  lead(HourData$tiltAngle, 5) +
  lead(HourData$tiltAngle, 3) +
  lead(HourData$tiltAngle, 2) +
  HourData$tiltAngle +
  lag(HourData$tiltAngle, 18) +
  lag(HourData$tiltAngle, 13) +
  lag(HourData$tiltAngle, 10) +
  lag(HourData$tiltAngle, 8) +
  lag(HourData$tiltAngle, 5) +
  lag(HourData$tiltAngle, 3) +
  lag(HourData$tiltAngle, 2)
HourData$Pertsev <- HourData$Pertsev / 15

plot_ly(
  data = HourData,
  x = ~datetime,
  y = ~tiltAngle,
  type = 'scatter',
  mode = 'lines',
  name = 'Tilt Angle',
  line = list(color = 'blue')
) %>% add_trace(
  y = ~Pertsev,
  name = 'Pertsev Tilt',
  line = list(color = 'green')
) %>% layout(
  title = 'Pertsev Filter on Tilt Angle (Casa Cenote)'
)

cutoffTilt <- quantile(HourData$tiltAngle, 0.1)

HourData$RejectTilt <- HourData$tiltAngle
HourData$RejectTilt[HourData$RejectTilt < cutoffTilt] <- 0
HourData$P2 <- lead(HourData$RejectTilt, 18) +
  lead(HourData$RejectTilt, 13) +
  lead(HourData$RejectTilt, 10) +
  lead(HourData$RejectTilt, 8) +
  lead(HourData$RejectTilt, 5) +
  lead(HourData$RejectTilt, 3) +
  lead(HourData$RejectTilt, 2) +
  HourData$RejectTilt +
  lag(HourData$RejectTilt, 18) +
  lag(HourData$RejectTilt, 13) +
  lag(HourData$RejectTilt, 10) +
  lag(HourData$RejectTilt, 8) +
  lag(HourData$RejectTilt, 5) +
  lag(HourData$RejectTilt, 3) +
  lag(HourData$RejectTilt, 2)
HourData$P2 <- HourData$P2 / 15

plot_ly(
  data = HourData,
  x = ~datetime,
  y = ~tiltAngle,
  type = 'scatter',
  mode = 'lines',
  name = 'Tilt Angle',
  line = list(color = 'blue')
) %>% add_trace(
  y = ~Pertsev,
  name = 'Pertsev Tilt',
  line = list(color = 'green')
) %>% add_trace(
  y = ~P2,
  name = 'Removed Low Tilt',
  line = list(color = 'red')
) %>% layout(
  title = 'Pertsev Filter on Tilt Angle (Casa Cenote)'
) %>% config(displayModeBar = FALSE)
