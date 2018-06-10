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

## moving average
TSAccelMag$AvgTiltAngle <- zoo::rollapply(TSAccelMag$tiltAngle, 7, function(x) {
    (0.3*x[4] + 0.2*(x[3]+x[5]) + 0.1*(x[2]+x[6]) + 0.05*(x[1]+x[7]))
}, fill = NA)

## pertsev
## TODO: some mathematical mismatch
TSAccelMag$Pertsev <- lead(TSAccelMag$tiltAngle, 18*4) +
  lead(TSAccelMag$tiltAngle, 13*4) +
  lead(TSAccelMag$tiltAngle, 10*4) +
  lead(TSAccelMag$tiltAngle, 8*4) +
  lead(TSAccelMag$tiltAngle, 5*4) +
  lead(TSAccelMag$tiltAngle, 3*4) +
  lead(TSAccelMag$tiltAngle, 2*4) +
  TSAccelMag$tiltAngle +
  lag(TSAccelMag$tiltAngle, 18*4) +
  lag(TSAccelMag$tiltAngle, 13*4) +
  lag(TSAccelMag$tiltAngle, 10*4) +
  lag(TSAccelMag$tiltAngle, 8*4) +
  lag(TSAccelMag$tiltAngle, 5*4) +
  lag(TSAccelMag$tiltAngle, 3*4) +
  lag(TSAccelMag$tiltAngle, 2*4)
TSAccelMag$Pertsev <- TSAccelMag$Pertsev / 15

TSAccelMag$WMAPertsev <- lead(TSAccelMag$AvgTiltAngle, 18*4) +
  lead(TSAccelMag$AvgTiltAngle, 13*4) +
  lead(TSAccelMag$AvgTiltAngle, 10*4) +
  lead(TSAccelMag$AvgTiltAngle, 8*4) +
  lead(TSAccelMag$AvgTiltAngle, 5*4) +
  lead(TSAccelMag$AvgTiltAngle, 3*4) +
  lead(TSAccelMag$AvgTiltAngle, 2*4) +
  TSAccelMag$AvgTiltAngle +
  lag(TSAccelMag$AvgTiltAngle, 18*4) +
  lag(TSAccelMag$AvgTiltAngle, 13*4) +
  lag(TSAccelMag$AvgTiltAngle, 10*4) +
  lag(TSAccelMag$AvgTiltAngle, 8*4) +
  lag(TSAccelMag$AvgTiltAngle, 5*4) +
  lag(TSAccelMag$AvgTiltAngle, 3*4) +
  lag(TSAccelMag$AvgTiltAngle, 2*4)
TSAccelMag$WMAPertsev <- TSAccelMag$WMAPertsev / 15


plot_ly(
  data = TSAccelMag,
  x = ~datetime,
  y = ~tiltAngle,
  type = 'scatter',
  mode = 'lines',
  name = 'Tilt Angle',
  line = list(color = 'lightblue')
) %>% add_trace (
  y = ~Pertsev,
  name = 'Pertsev',
  line = list(color = 'blue')
) %>% add_trace(
  y = ~AvgTiltAngle,
  name = 'Weighted Moving Average',
  line = list(color = 'lightgreen')
) %>% add_trace(
  y = ~WMAPertsev,
  name = 'WMA Pertsev',
  line = list(color = 'green')
) %>% layout(
  title = 'Odyssey Pertsev Filter on Tilt Angle'
)
