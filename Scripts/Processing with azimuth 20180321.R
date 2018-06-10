require(dplyr)
require(plotly)
require(lubridate)

# read csv of data
# HEADERS: c('datetime','xa','ya','za','xm','ym','zm')
RawAccelMag <- read.csv('AKsouth_012_20160403-0714.csv') 

# crop data set to 3000 rows (for ease of plotting)
#RawAccelMag <- head(RawAccelMag, 3000)

# parse date
RawAccelMag$datetime <- mdy_hm(RawAccelMag$datetime)


#### STEP 0: NORMALIZE ACCELEROMETER DATA ####
# Let Ax, Ay, Az be the calibrated accelerometer values
# Ax1, Ay1, and Az1 are the "normalized" readings.
# Goal: "the root sum of Ax1, Ay1, and Az1 should be equal to one when the accelerometer is still"
#     - LSM303 Vignette


## normalize with modulus
RawAccelMag$a_modulus <- sqrt(RawAccelMag$xa^2 + RawAccelMag$ya^2 + RawAccelMag$za^2)
RawAccelMag$xa_norm <- RawAccelMag$xa / RawAccelMag$a_modulus
RawAccelMag$ya_norm <- RawAccelMag$ya / RawAccelMag$a_modulus
RawAccelMag$za_norm <- RawAccelMag$za / RawAccelMag$a_modulus


# from LSM303 vignette Appendix A
# https://www.sparkfun.com/datasheets/Sensors/Magneto/Tilt%20Compensated%20Compass.pdf
#### STEP 1: TILT, ROLL, PITCH, AZIMUTH ####

# magnitude of the projection of acceleration vector in the xy plane 
RawAccelMag$Gproj_XY <- sqrt(RawAccelMag$xa_norm^2 + RawAccelMag$ya_norm^2)

# calculate tilt angle in degrees 
RawAccelMag$tiltAngle <- atan2(RawAccelMag$Gproj_XY, RawAccelMag$za_norm) * 180 / pi

# pitch and tilt angles in radians
# xa_norm must be less than 1 for this to work 
RawAccelMag$pitchRadians <- asin(-1*RawAccelMag$xa_norm) # should stay bn +/- 45 degrees 
RawAccelMag$rollRadians <- asin(RawAccelMag$ya_norm) #different from LSM303

## Azimuth
AssignQuadrant <- function(Xvec, Yvec) {
  ReturnVec <- rep(0, length(Xvec))
  ReturnVec[Xvec >= 0 & Yvec >= 0] <- 1
  ReturnVec[Xvec < 0 & Yvec >= 0] <- 2
  ReturnVec[Xvec < 0 & Yvec < 0] <- 3
  ReturnVec[Xvec >= 0 & Yvec < 0] <- 4
  # if any of the observations are exactly equal to zero, a zero is returned
  return(ReturnVec)
}

GetAzimuth <- function(Xvec, Yvec) {
  Quadrant <- AssignQuadrant(Xvec, Yvec)
  
  ReturnVec <- atan(Yvec/Xvec) * 180 / pi
  ReturnVec[Quadrant == 2 | Quadrant == 3] <- 180 + ReturnVec[Quadrant == 2 | Quadrant == 3]
  ReturnVec[Quadrant == 4] <- 360 + ReturnVec[Quadrant == 4]
  
  return(ReturnVec)
}

RawAccelMag$azimuthDegrees <- GetAzimuth(RawAccelMag$xa_norm, RawAccelMag$ya_norm)

#### STEP 2: NORMALIZE MAGNETOMETER DATA ####
# note: magnetometer data was calibrated in Oddessy spreadsheet
# data in nanoteslas

# Let Mx, My, Mz be the calibrated magnetometer values (nanoteslas)
# Mx1, My1, and Mz1 are the "normalized" readings.
# Goal: "the square root of the sum squared values should be equal to 1
#   when there is no external interference magnetic field"
#     - LSM303 Vignette

# 2.1: calculate the magnitude of the magnetic field vector for each observation
RawAccelMag$m_modulus <- sqrt(RawAccelMag$xm^2 + RawAccelMag$ym^2 + RawAccelMag$zm^2)

# 2.2: normalize Mx, My, Mz by dividing by modulus
RawAccelMag$xm_norm <- RawAccelMag$xm / RawAccelMag$m_modulus
RawAccelMag$ym_norm <- RawAccelMag$ym / RawAccelMag$m_modulus 
RawAccelMag$zm_norm <- RawAccelMag$zm / RawAccelMag$m_modulus

#### STEP 3: TILT-COMPENSATED MAG FIELD ####
# calculations based on eqn 12 in the LSM303 Vignette
RawAccelMag$xm_comp <- RawAccelMag$xm_norm * cos(RawAccelMag$pitchRadians) +
  RawAccelMag$zm_norm * sin(RawAccelMag$pitchRadians)

RawAccelMag$ym_comp <- RawAccelMag$xm_norm * sin(RawAccelMag$rollRadians) * sin(RawAccelMag$pitchRadians) +
  RawAccelMag$ym_norm * cos(RawAccelMag$rollRadians) -
  RawAccelMag$zm_norm * sin(RawAccelMag$rollRadians) * cos(RawAccelMag$pitchRadians)

RawAccelMag$zm_comp <- -1 * RawAccelMag$xm_norm * cos(RawAccelMag$rollRadians) * sin(RawAccelMag$pitchRadians) +
  RawAccelMag$ym_norm * sin(RawAccelMag$rollRadians) +
  RawAccelMag$zm_norm * cos(RawAccelMag$rollRadians) * cos(RawAccelMag$pitchRadians)

# summary(sqrt(RawAccelMag$xm_comp^2 + RawAccelMag$ym_comp^2 + RawAccelMag$zm_comp^2))


#### STEP 4: HEADING CALCULATION ####
# see equation 13 in LSM303 vignette
RawAccelMag$headingDegrees <- GetAzimuth(RawAccelMag$xm_comp, RawAccelMag$ym_comp)

# add azimuth and heading degrees
# then take remainder when divided by 360 to get an angle between 0 and 360
RawAccelMag$azimuthDegrees_adjusted <- (RawAccelMag$headingDegrees + RawAccelMag$azimuthDegrees) %% 360

#### STEP 5: WINDROSE (FREQUENCY) ####

# 5.1 Make frequency table for tilt angle histograms #
tempFreqTable <- table(round(RawAccelMag$azimuthDegrees_adjusted,1))
AngleFrequencyTable <- data.frame(
  Angle = as.numeric(names(tempFreqTable)),
  Frequency = as.vector(tempFreqTable)
)

# 5.2 Windrose A: heading angle vs frequency (binned by 0.1) (bar) (0-360 degrees)
plot_ly(
  data = AngleFrequencyTable,
  type = 'area',
  t = ~Angle,
  r = ~Frequency,
  opacity = 1
) %>% layout(
  radialaxis = list(
    visible = T
  ),
  angularaxis = list(
    visible = T,
    type = 'linear',
    thetaunit = 'degrees',
    range = c(0,360)
  ),
  orientation = 270,
  showlegend = F
) %>% config(
  displayModeBar = TRUE
)


# 5.2 Windrose B: heading angle vs frequency (binned by 0.1) (bar) (autoranged degrees)
plot_ly(
  data = AngleFrequencyTable,
  type = 'area',
  t = ~Angle,
  r = ~Frequency,
  opacity = 0.5
) %>% layout(
  radialaxis = list(
    visible = T
  ),
  angularaxis = list(
    visible = T,
    type = 'linear',
    thetaunit = 'degrees',
    range = range(AngleFrequencyTable$Angle)
  ),
  orientation = 270,
  showlegend = F
) %>% config(
  displayModeBar = TRUE
)


# 5.3 Frequency vs Angle Bar Plot 
plot_ly(
  data = AngleFrequencyTable,
  type = 'bar',
  x = ~Angle,
  y = ~Frequency
) %>% layout(
  showlegend = F,
  title = "Oddessy Heading Angle Histogram",
  xaxis = list(title = "Angle (clockwise degrees from magnetic north)"),
  yaxis = list(title = "Frequency")
) %>% config(
  displayModeBar = FALSE
)


# 5.4 Raw Acceleration Coords Scatter Plot (xa_norm vs ya_norm) 
# plot_ly(
#   data = RawAccelMag,
#   x = ~xa_norm,
#   y = ~ya_norm,
#   type = 'scatter',
#   mode = 'markers',
#   hoverinfo = 'none'
# ) %>% layout(
#   xaxis = list(range = c(-0.5,0.5)),
#   yaxis = list(range = c(-0.5,0.5))
# )

#### STEP 6: WINDROSE (TILT AND/OR VELOCITY) ####

# 6.1 Heading vs Tilt (polar scatter)
plot_ly(
  data = RawAccelMag,
  type = 'scatter',
  mode = 'markers',
  t = ~azimuthDegrees_adjusted,
  r = ~tiltAngle,
  opacity = 0.3,
  marker = list(size = 4)
) %>% layout(
  radialaxis = list(
    visible = T
  ),
  angularaxis = list(
    visible = T,
    type = 'linear',
    thetaunit = 'degrees',
    range = c(0,360)
  ),
  orientation = 270,
  showlegend = F,
  title = "Heading (angular axis) vs Tilt (radial axis)"
) %>% config(
  displayModeBar = TRUE
)

# 6.2 Heading vs Tilt (Cartesian scatter in first quadrant to allow zoom)

# new data.frame with fewer columns
CartesianAngles <- select(RawAccelMag, tiltAngle, azimuthDegrees_adjusted)

# have to rotate the heading degrees from bearing coordinates
CartesianAngles$rotatedazimuthDegrees_adjusted <- 90 - CartesianAngles$azimuthDegrees_adjusted


# x = r cos(t)
# y = r sin(t)
# t = rotatedazimuthDegrees_adjusted, r = tiltAngle
CartesianAngles$x_coord <- CartesianAngles$tiltAngle * cos(CartesianAngles$rotatedazimuthDegrees_adjusted * pi / 180)
CartesianAngles$y_coord <- CartesianAngles$tiltAngle * sin(CartesianAngles$rotatedazimuthDegrees_adjusted * pi / 180)

# make sure that axes are equal 
axisMax = max(max(CartesianAngles$x_coord), max(CartesianAngles$y_coord))

plot_ly(
  data = CartesianAngles,
  x = ~x_coord,
  y = ~y_coord,
  text = ~paste0('Heading Angle: ',round(azimuthDegrees_adjusted,2), ' deg<br>Tilt: ', round(tiltAngle,2), ' deg'),
  hoverinfo = 'text',
  type = 'scattergl',
  mode = 'markers',
  marker = list(
    size = 5, 
    color = 'rgba(0,0,0,0.1)',
    line = list(color = 'rgba(0,0,0,0.5)', width = 1)
  )
)  %>% layout (
  title = "Heading (angular axis) vs Tilt (radial axis), \n Cartesian Coords for zoomability",
  xaxis = list(autorange = FALSE, range = c(-axisMax, axisMax)),
  yaxis = list(autorange = FALSE, range = c(-axisMax, axisMax))
)%>% config(displayModeBar = 'hover')

#### STEP 7: TIME-SERIES VECTOR PLOTS ####

# 7.1 Angle vs Time (scattergl, color by tilt ~velocity)
plot_ly(
  data = RawAccelMag,
  x = ~datetime,
  y = ~tiltAngle,
  color = ~azimuthDegrees_adjusted,
  type = 'scattergl',
  mode = 'markers',
  marker = list(size = 5)
) %>% layout(
  title = 'bearing vs time',
  xaxis = list(range = as.numeric(range(RawAccelMag$datetime))*1000)
) %>% config(displayModeBar = TRUE)


#### STEP 8: ANGLE PLOTS ####
plot_ly(
  data = RawAccelMag,
  x = ~xa_norm,
  y = ~ya_norm,
  z = ~rollRadians *180 / pi,
  type = 'scatter3d',
  mode = 'markers',
  marker = list(color = 'red'),
  name = 'roll'
) %>% add_markers(
  z = ~pitchRadians * 180 / pi,
  marker = list(color = 'purple'),
  name = 'pitch'
) %>% add_markers(
  z = 0,
  marker = list(color = 'black'),
  name = 'zero'
) %>% add_markers(
  z = ~azimuthDegrees,
  marker = list(color = 'blue'),
  name = 'azimuth'
)
