require(dplyr)
require(plotly)
require(lubridate)

RawAccelMag <- read.csv('AKsouth_012_20160403-0714.csv')  %>% sample_n(100)

# parse date
RawAccelMag$datetime <- mdy_hm(RawAccelMag$datetime)


# Calculate Gproj_XY, the magnitude of the projection of the direction vector,
#   H, in the XY plane. 
RawAccelMag$Gproj_XY <- sqrt(RawAccelMag$xa^2 + RawAccelMag$ya^2)
# Calculate tilt angle
RawAccelMag$tiltAngle <- atan2(RawAccelMag$Gproj_XY, RawAccelMag$za) * 180 / pi


## normalize with modulus
RawAccelMag$modulus <- sqrt(RawAccelMag$xa^2 + RawAccelMag$ya^2 + RawAccelMag$za^2)
RawAccelMag$xa_norm <- RawAccelMag$xa / RawAccelMag$modulus
RawAccelMag$ya_norm <- RawAccelMag$ya / RawAccelMag$modulus
RawAccelMag$za_norm <- RawAccelMag$za / RawAccelMag$modulus

RawAccelMag$Gproj_XY_norm <- sqrt(RawAccelMag$xa_norm^2 + RawAccelMag$ya_norm^2)
RawAccelMag$tiltAngle_norm <- atan2(RawAccelMag$Gproj_XY_norm, RawAccelMag$za_norm) * 180 / pi

# RawAccelMag$tiltAngle - RawAccelMag$tiltAngle_norm is negligible 

