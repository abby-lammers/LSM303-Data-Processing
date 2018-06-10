RawAccel <- read.csv('OnlyCalibratedAccelFlowData.csv')

# Calculate Gproj_XY, the magnitude of the projection of the direction vector,
#   H, in the XY plane. 
RawAccel$Gproj_XY <- sqrt(RawAccel$Xcal^2 + RawAccel$Ycal^2)

# Calculate tilt angle
RawAccel$tiltAngle <- atan2(RawAccel$Gproj_XY, RawAccel$Zcal) * 180 / pi

# Examining where the accel data has non-negative signs
# > sum(RawAccel$Xcal > 0 | RawAccel$Ycal > 0)
# [1] 95 (out of 25,442 data points)
