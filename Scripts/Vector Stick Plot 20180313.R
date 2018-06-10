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

Angles <- dplyr::select(CavePearl, datetime, tiltAngle, headingDegrees)

## convert from polar to cartesian-- from "Windrose with Tilt 20180307.R" section 6.2
# x = r cos(t)
# y = r sin(t)
# t = rotatedHeadingDegrees, r = tiltAngle
Angles$rotatedHeadingDegrees <- 90 - Angles$headingDegrees

Angles$x_coord <- Angles$tiltAngle * cos(Angles$rotatedHeadingDegrees * pi / 180)
Angles$y_coord <- Angles$tiltAngle * sin(Angles$rotatedHeadingDegrees * pi / 180)

## filter by date
date <- ymd('2016-08-15')

# e.g. from plotly, slightly modified to prove a point
plot_ly(
  data = filter(Angles, floor_date(datetime,'day') == date),
  type = 'scatter', 
  mode = 'none', 
  x = ~datetime, 
  y = ~0
) %>% add_annotations(
  x = ~datetime,
  y = ~0, 
  text = "",
  xref = "x",
  yref = "y",
  arrowhead = 0,
  ax = ~x_coord*8, # 8 is an arbibrary scaling factor to convert to pixels (ideally react to viewport size idk)
  ay = ~-y_coord*8 
)


