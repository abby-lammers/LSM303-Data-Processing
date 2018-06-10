require(dplyr)
require(plotly)
require(lubridate)
require(tidyr)
require(oce)

#### STEP 0: READ CSVS ####
## read meter data
# see "Windrose With Tilt 20180307.R"
CavePearl <- read.csv('ProcessedAccelMag (Windrose with Tilt 20180307).csv')
CavePearl$X <- NULL
CavePearl$datetime <- lubridate::ymd_hms(CavePearl$datetime)

CartesianAngles <- dplyr::select(CavePearl, datetime, tiltAngle, headingDegrees) %>% head(500)

# have to rotate the heading degrees from bearing coordinates
CartesianAngles$rotatedHeadingDegrees <- 90 - CartesianAngles$headingDegrees

# x = r cos(t)
# y = r sin(t)
# t = rotatedHeadingDegrees, r = tiltAngle
CartesianAngles$x_coord <- CartesianAngles$tiltAngle * cos(CartesianAngles$rotatedHeadingDegrees * pi / 180)
CartesianAngles$y_coord <- CartesianAngles$tiltAngle * sin(CartesianAngles$rotatedHeadingDegrees * pi / 180)

middley <- (range(CartesianAngles$tiltAngle)[1] + range(CartesianAngles$tiltAngle)[2])/2

oce.plot.ts(x = CartesianAngles$datetime, y = CartesianAngles$tiltAngle, type = 'l')
plotSticks(
  x = CartesianAngles$datetime, 
  u = CartesianAngles$x_coord, 
  v = CartesianAngles$y_coord, 
  y = middley, 
  add = TRUE, 
  yscale = 5, col = 'darkblue'
)
