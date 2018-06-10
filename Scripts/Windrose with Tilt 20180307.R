require(dplyr)
require(plotly)
require(lubridate)

AssignQuadrant <- function(Xvec, Yvec) {
  ReturnVec <- rep(0, length(Xvec))
  ReturnVec[Xvec > 0 & Yvec > 0] <- 1
  ReturnVec[Xvec < 0 & Yvec > 0] <- 2
  ReturnVec[Xvec < 0 & Yvec < 0] <- 3
  ReturnVec[Xvec > 0 & Yvec < 0] <- 4
  # if any of the observations are exactly equal to zero, a zero is returned
  return(ReturnVec)
}

RawAccelMag <- read.csv('OddessyCalibratedWithDateTime.csv') 

# crop data set to 3000 rows (for ease of plotting)
#RawAccelMag <- head(RawAccelMag, 3000)

# parse date
RawAccelMag$datetime <- mdy_hm(RawAccelMag$datetime)

# Calculate Gproj_XY, the magnitude of the projection of the direction vector,
#   H, in the XY plane. 
RawAccelMag$Gproj_XY <- sqrt(RawAccelMag$xa^2 + RawAccelMag$ya^2)

# Calculate tilt angle
RawAccelMag$tiltAngle <- atan2(RawAccelMag$Gproj_XY, RawAccelMag$za) * 180 / pi

# Examining where the accel data has non-negative signs
# > sum(RawAccelMag$xa > 0 | RawAccelMag$ya > 0)
# [1] 95 (out of 25,442 data points)


# Assign a quadrant to everything
RawAccelMag$Quadrant <- AssignQuadrant(RawAccelMag$xa, RawAccelMag$ya)
# easier orientation table
# table(RawAccelMag$Quadrant)



#### STEP 0: NORMALIZE ACCELEROMETER DATA ####
# Let Ax, Ay, Az be the calibrated accelerometer values
# Ax1, Ay1, and Az1 are the "normalized" readings.
# Goal: "the root sum of Ax1, Ay1, and Az1 should be equal to one when the accelerometer is still"
#     - LSM303 Vignette

# 0.1: find the place where the accelerometer is CLOSEST to still 
#     which will be where x and y are both closest to 0.
RawAccelMag$DistanceFromUpright <- abs(RawAccelMag$xa) + abs(RawAccelMag$ya)
MostVerticalObservations <- RawAccelMag[order(RawAccelMag$DistanceFromUpright)[1:10], ] # 10 most vertical observations

# another way to do this would be to calculate the tilt angle. 
# the resulting set of observations is almost exactly the same, 
#   but this way you get to avoid using arctan


# 0.2 get a normalization factor based on the most vertical observations
AverageVerticalZ <- mean(MostVerticalObservations$za)

# 0.3 normalize Ay and Ax
RawAccelMag$xa_norm <- RawAccelMag$xa / AverageVerticalZ
RawAccelMag$ya_norm <- RawAccelMag$ya / AverageVerticalZ


# from LSM303 vignette Appendix A
# https://www.sparkfun.com/datasheets/Sensors/Magneto/Tilt%20Compensated%20Compass.pdf
#### STEP 1: ROLL AND PITCH ####

RawAccelMag$pitchRadians <- asin(-1*RawAccelMag$xa_norm) # should stay bn +/- 45 degrees 
RawAccelMag$rollRadians <- RawAccelMag$ya_norm / cos(RawAccelMag$pitchRadians)

#### STEP 2: NORMALIZE MAGNETOMETER DATA ####
# note: magnetometer data was calibrated in Oddessy spreadsheet
# data in nanoteslas

# Let Mx, My, Mz be the calibrated magnetometer values (nanoteslas)
# Mx1, My1, and Mz1 are the "normalized" readings.
# Goal: "the square root of the sum squared values should be equal to 1
#   when there is no external interference magnetic field"
#     - LSM303 Vignette

# 2.1: calculate the magnitude of the magnetic field vector for each observation
RawAccelMag$mMagnitude <- sqrt(RawAccelMag$xm^2 + RawAccelMag$ym^2 + RawAccelMag$zm^2)

# 2.2: get normalization factor by finding the mean (without outliers)

# get range of quartiles from R's boxplot()
MMRange <- boxplot(RawAccelMag$mMagnitude)$stats[c(1,5),1]

# remove outliers, the points outside of that range
TrimmedAccelMag <- RawAccelMag %>% filter(mMagnitude > MMRange[1] & mMagnitude < MMRange[2])

# get mean of resulting data set
MMMean <- mean(TrimmedAccelMag$mMagnitude)

## some quick stats for reference
# sd(TrimmedAccelMag$mMagnitude) # standard deviation
# hist(TrimmedAccelMag$mMagnitude) # histogram
# qqnorm(TrimmedAccelMag$mMagnitude) # Displays Q-Q plot showing how normal data is 

# 2.3 normalize Mx, My, Mz
TrimmedAccelMag$xm_norm <- TrimmedAccelMag$xm / MMMean
TrimmedAccelMag$ym_norm <- TrimmedAccelMag$ym / MMMean
TrimmedAccelMag$zm_norm <- TrimmedAccelMag$zm / MMMean

# check to make sure that values are around 1
# hist(sqrt(TrimmedAccelMag$xm_norm^2 + TrimmedAccelMag$ym_norm^2 + TrimmedAccelMag$zm_norm^2), breaks = 50)
cat('\n>>> Distribution of Magnitudes of Normalized Magnetometer Data: <<<') # print for debugging
summary(sqrt(TrimmedAccelMag$xm_norm^2 + TrimmedAccelMag$ym_norm^2 + TrimmedAccelMag$zm_norm^2))

#### STEP 3: TILT-COMPENSATED MAG FIELD ####
# calculations based on eqn 12 in the LSM303 Vignette
TrimmedAccelMag$xm_comp <- TrimmedAccelMag$xm_norm * cos(TrimmedAccelMag$pitchRadians) +
  TrimmedAccelMag$zm_norm * sin(TrimmedAccelMag$pitchRadians)

TrimmedAccelMag$ym_comp <- TrimmedAccelMag$xm_norm * sin(TrimmedAccelMag$rollRadians) * sin(TrimmedAccelMag$pitchRadians) +
  TrimmedAccelMag$ym_norm * cos(TrimmedAccelMag$rollRadians) -
  TrimmedAccelMag$zm_norm * sin(TrimmedAccelMag$rollRadians) * cos(TrimmedAccelMag$pitchRadians)

TrimmedAccelMag$zm_comp <- -1 * TrimmedAccelMag$xm_norm * cos(TrimmedAccelMag$rollRadians) * sin(TrimmedAccelMag$pitchRadians) +
  TrimmedAccelMag$ym_norm * sin(TrimmedAccelMag$rollRadians) +
  TrimmedAccelMag$zm_norm * cos(TrimmedAccelMag$rollRadians) * cos(TrimmedAccelMag$pitchRadians)

# summary(sqrt(TrimmedAccelMag$xm_comp^2 + TrimmedAccelMag$ym_comp^2 + TrimmedAccelMag$zm_comp^2))

## TODO: replace current trimming method (which is very heavy-handed) with smoothing function?


#### STEP 4: HEADING CALCULATION ####
# see equation 13 in LSM303 vignette

# to be applied over rows
getDegreeHeading <- function(observationRow) {
  mx <- observationRow[which(names(observationRow) == 'xm_comp')][[1]] %>% as.numeric
  my <- observationRow[which(names(observationRow) == 'ym_comp')][[1]] %>% as.numeric
  
  if (mx > 0 && my >= 0) {
    heading <- atan(my/mx) * 180 / pi
  } else if (mx < 0) {
    heading <- 180 + atan(my/mx) * 180 / pi
  } else if (mx > 0 && my <= 0) {
    heading <- 360 + atan(my/mx) * 180 / pi
  } else if (mx == 0 && my < 0) {
    heading <- 90
  } else if (mx == 0 && my > 0) {
    heading <- 270
  } else {
    heading <- NA
  }
  
  return(heading)
}

TrimmedAccelMag$headingDegrees <- apply(TrimmedAccelMag, MARGIN = 1, getDegreeHeading)

#### STEP 5: WINDROSE (FREQUENCY) ####

# 5.1 Make frequency table for tilt angle histograms #
tempFreqTable <- table(round(TrimmedAccelMag$headingDegrees,1))
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
  opacity = 0.1
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
  data = TrimmedAccelMag,
  type = 'scatter',
  mode = 'markers',
  t = ~headingDegrees,
  r = ~tiltAngle,
  opacity = 0.1,
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
CartesianAngles <- select(TrimmedAccelMag, tiltAngle, headingDegrees)

# have to rotate the heading degrees from bearing coordinates
CartesianAngles$rotatedHeadingDegrees <- 90 - CartesianAngles$headingDegrees


# x = r cos(t)
# y = r sin(t)
# t = rotatedHeadingDegrees, r = tiltAngle
CartesianAngles$x_coord <- CartesianAngles$tiltAngle * cos(CartesianAngles$rotatedHeadingDegrees * pi / 180)
CartesianAngles$y_coord <- CartesianAngles$tiltAngle * sin(CartesianAngles$rotatedHeadingDegrees * pi / 180)

# make sure that axes are equal 
axisMax = max(max(CartesianAngles$x_coord), max(CartesianAngles$y_coord))

plot_ly(
  data = CartesianAngles,
  x = ~x_coord,
  y = ~y_coord,
  text = ~paste0('Heading Angle: ',round(headingDegrees,2), ' deg<br>Tilt: ', round(tiltAngle,2), ' deg'),
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
  xaxis = list(autorange = FALSE, range = c(0, axisMax)),
  yaxis = list(autorange = FALSE, range = c(0, axisMax))
)%>% config(displayModeBar = TRUE)

#### STEP 7: TIME-SERIES VECTOR PLOTS ####

# 7.1 Angle vs Time (scattergl, color by tilt ~velocity)
plot_ly(
  data = TrimmedAccelMag,
  x = ~datetime,
  y = ~tiltAngle,
  color = ~headingDegrees,
  type = 'scattergl',
  mode = 'markers',
  marker = list(size = 5)
) %>% layout(
  title = 'bearing vs time',
  xaxis = list(range = as.numeric(range(TrimmedAccelMag$datetime))*1000)
) %>% config(displayModeBar = TRUE)


DataToExplore <- filter(TrimmedAccelMag, datetime > ymd('2016-08-12') & datetime < ymd('2016-08-19'))

plot_ly(
  data = DataToExplore,
  x = ~datetime,
  y = ~tiltAngle,
  color = ~headingDegrees,
  type = 'scattergl',
  mode = 'markers',
  marker = list(size = 5)
) %>% layout(
  title = 'bearing vs time',
  xaxis = list(range = as.numeric(range(DataToExplore$datetime))*1000)
) %>% config(displayModeBar = TRUE)
