# see Feb 28 - Mar 02 Log Notes
require(dplyr)

# Examining where the accel data has non-negative signs
CreateOrientationTable <- function(LogXCol, LogYCol) {
  
  OrientationMatrix <- data.frame(
    Xsign = c("pos","pos","neg","neg"),
    Ysign = c("pos","neg","pos","neg"),
    NObs = c(0,0,0,0)
  )
  
  for (ii in 1:nrow(OrientationMatrix)) {
    OrientationRow <- OrientationMatrix[ii,]
    xIndex <- if (OrientationRow$Xsign == "pos") {LogXCol > 0} else {LogXCol < 0}
    yIndex <- if (OrientationRow$Ysign == "pos") {LogYCol > 0} else {LogYCol < 0}
    OrientationMatrix$NObs[ii] <- sum(xIndex & yIndex)
  }
  
  return(OrientationMatrix)
}

AssignQuadrant <- function(Xvec, Yvec) {
  ReturnVec <- rep(0, length(Xvec))
  ReturnVec[Xvec > 0 & Yvec > 0] <- 1
  ReturnVec[Xvec < 0 & Yvec > 0] <- 2
  ReturnVec[Xvec < 0 & Yvec < 0] <- 3
  ReturnVec[Xvec > 0 & Yvec < 0] <- 4
  # if any of the observations are exactly equal to zero, a zero is returned
  return(ReturnVec)
}


RawAccelMag <- read.csv('CalibratedAllData.csv')

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


# make a quick orientation table
OrientationTable_ForNormalization <- CreateOrientationTable(
  LogXCol = MostVerticalObservations$xa, 
  LogYCol = MostVerticalObservations$ya
)
OrientationTable_ForNormalization


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

# remove outliers, the points outside of that range, then get mean of resulting data set
MMMean <- mean(RawAccelMag$mMagnitude[RawAccelMag$mMagnitude > MMRange[1] & RawAccelMag$mMagnitude < MMRange[2]])

## some quick stats for reference
# sd(RawAccelMag$mMagnitude) # standard deviation
# hist(RawAccelMag$mMagnitude) # histogram
# qqnorm(RawAccelMag$mMagnitude) # Displays Q-Q plot showing how normal data is 

# 2.3 normalize Mx, My, Mz
RawAccelMag$xm_norm <- RawAccelMag$xm / MMMean
RawAccelMag$ym_norm <- RawAccelMag$ym / MMMean
RawAccelMag$zm_norm <- RawAccelMag$zm / MMMean

# check to make sure that values are around 1
# hist(sqrt(RawAccelMag$xm_norm^2 + RawAccelMag$ym_norm^2 + RawAccelMag$zm_norm^2), breaks = 50)
cat('\n>>> Distribution of Magnitudes of Normalized Magnetometer Data: <<<') # print for debugging
summary(sqrt(RawAccelMag$xm_norm^2 + RawAccelMag$ym_norm^2 + RawAccelMag$zm_norm^2))

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

## TODO: remove "bad" data where the magnitude of the tilt-compensated mag vector
##    (i.e. the root squared sum of the m__comp terms) is not close enough to 1


#### STEP 4: HEADING CALCULATION ####
# see equation 13 in LSM303 vignette

# to be applied over rows
getDegreeHeading <- function(observationRow) {
  mx <- observationRow[which(names(observationRow) == 'xm_comp')][[1]]
  my <- observationRow[which(names(observationRow) == 'ym_comp')][[1]]
  
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

RawAccelMag$headingDegrees <- apply(RawAccelMag, MARGIN = 1, getDegreeHeading)
