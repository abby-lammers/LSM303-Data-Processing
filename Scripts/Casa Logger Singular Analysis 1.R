RawAccel <- read.csv('log000000_RawAccAndComp.csv')

# Calculate Gproj_XY, the magnitude of the projection of the direction vector,
#   H, in the XY plane. 
RawAccel$Gproj_XY <- sqrt(RawAccel$AccX^2 + RawAccel$AccY^2)

# Calculate tilt angle
RawAccel$TiltR <- atan2(RawAccel$Gproj_XY, RawAccel$AccZ) * 180 / pi

# compare tilt angles from R and raw calcs (simple, probably ineffective method)
mean(abs(RawAccel$TiltR - RawAccel$RawTilt))

# Examining where the accel data has non-negative signs
OrientationMatrix <- data.frame(
  Xsign = c("pos","pos","neg","neg"),
  Ysign = c("pos","neg","pos","neg"),
  NObs = c(0,0,0,0)
)

for (ii in 1:nrow(OrientationMatrix)) {
  OrientationRow <- OrientationMatrix[ii,]
  xIndex <- if (OrientationRow$Xsign == "pos") {RawAccel$AccX > 0} else {RawAccel$AccX < 0}
  yIndex <- if (OrientationRow$Ysign == "pos") {RawAccel$AccY > 0} else {RawAccel$AccY < 0}
  OrientationMatrix$NObs[ii] <- sum(xIndex & yIndex)
}


sum(RawAccel$AccX > 0 & RawAccel$AccY < 0)

# > sum(RawAccel$Xcal > 0 | RawAccel$Ycal > 0)
# [1] 95 (out of 25,442 data points)
