# see Feb. 28 Log Notes

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


RawAccel <- read.csv('OnlyCalibratedAccelFlowData.csv')

# Calculate Gproj_XY, the magnitude of the projection of the direction vector,
#   H, in the XY plane. 
RawAccel$Gproj_XY <- sqrt(RawAccel$Xcal^2 + RawAccel$Ycal^2)

# Calculate tilt angle
RawAccel$tiltAngle <- atan2(RawAccel$Gproj_XY, RawAccel$Zcal) * 180 / pi

# Examining where the accel data has non-negative signs
# > sum(RawAccel$Xcal > 0 | RawAccel$Ycal > 0)
# [1] 95 (out of 25,442 data points)


# Assign a quadrant to everything
RawAccel$Quadrant <- AssignQuadrant(RawAccel$Xcal, RawAccel$Ycal)
# easier orientation table
# table(RawAccel$Quadrant)



### STEP 0: NORMALIZE ###
# Let Ax, Ay, Az be the calibrated accelerometer values
# Ax1, Ay1, and Az1 are the "normalized" readings.
# Goal: "the root sum of Ax1, Ay1, and Az1 should be equal to one when the accelerometer is still"
#     - LSM303 Vignette

# 0.1: find the place where the accelerometer is CLOSEST to still
#     which will be where x and y are both closest to 0.
RawAccel$DistanceFromUpright <- abs(RawAccel$Xcal) + abs(RawAccel$Ycal)
MostVerticalObservations <- RawAccel[order(RawAccel$DistanceFromUpright)[1:10], ] # 10 most vertical observations

# another way to do this would be to calculate the tilt angle. 
# the resulting set of observations is almost exactly the same, 
#   but this way you get to avoid using arctan


# make a quick orientation table
OrientationTable_ForNormalization <- CreateOrientationTable(
  LogXCol = MostVerticalObservations$Xcal, 
  LogYCol = MostVerticalObservations$Ycal
)
OrientationTable_ForNormalization


# 0.2 get a normalization factor based on the most vertical observations
AverageVerticalZ <- mean(MostVerticalObservations$Zcal)

# 0.3 normalize Ay and Ax
RawAccel$Xnorm <- RawAccel$Xcal / AverageVerticalZ
RawAccel$Ynorm <- RawAccel$Ycal / AverageVerticalZ


# from LSM303 vignette Appendix A
# https://www.sparkfun.com/datasheets/Sensors/Magneto/Tilt%20Compensated%20Compass.pdf
### STEP 1: CALCULATE ROLL AND PITCH ###

RawAccel$pitchRadians <- asin(-1*RawAccel$Xnorm) # should stay bn +/- 45 degrees 
RawAccel$rollRadians <- RawAccel$Ynorm / cos(RawAccel$pitchRadians)

