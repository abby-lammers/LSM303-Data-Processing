require(dplyr)
require(plotly)
require(lubridate)

TSAccelMag <- Read_LSM303_csv('B4_20141222-150317_AKnorth.csv', sample = 0) %>% 
  Normalize_Accel() %>% 
  Get_Accel_Angles() %>% 
  Normalize_Mag() %>% 
  Compensate_Mag_Field() %>% 
  Get_Heading()

# read csv of data
# COLUMN NAMES: c('datetime','xa','ya','za','xm','ym','zm')
Read_LSM303_csv <- function(fileName, sample = 0, crop = 0) {
  TSAccelMag <- read.csv(fileName) #time series accelerometer and magnetometer data
  TSAccelMag$datetime <- lubridate::mdy_hm(TSAccelMag$datetime) # convert date string to date object
  
  TSAccelMag <- dplyr::select(TSAccelMag, datetime, xa, ya, za, xm, ym, zm) # select only necessary columns
  
  
  # crop and sample methods for ease of *development*
  # work with fewer points to speed things up
  if (sample > 0) {
    # sample n random rows
    TSAccelMag <- dplyr::sample_n(TSAccelMag, sample)
  } 
  
  if (crop > 0) {
    # take first n rows of data
    TSAccelMag <- head(TSAccelMag, crop) 
  }
  
  return(TSAccelMag)
}


#### STEP 0: NORMALIZE ACCELEROMETER DATA ####
# Let Ax, Ay, Az be the calibrated accelerometer values
# Ax1, Ay1, and Az1 are the "normalized" readings.
# Goal: "the root sum of Ax1, Ay1, and Az1 should be equal to one when the accelerometer is still"
#     - LSM303 Vignette
Normalize_Accel <- function(TSAccelMag) {
  # get length (modulus) of acceleration vector
  TSAccelMag$a_modulus <- sqrt(TSAccelMag$xa^2 + TSAccelMag$ya^2 + TSAccelMag$za^2)
  
  # normalize each component so that it becomes a unit vector (i.e. magnitude of 1)
  # especially important for trig functions
  TSAccelMag$xa_norm <- TSAccelMag$xa / TSAccelMag$a_modulus
  TSAccelMag$ya_norm <- TSAccelMag$ya / TSAccelMag$a_modulus
  TSAccelMag$za_norm <- TSAccelMag$za / TSAccelMag$a_modulus
  
  return(TSAccelMag)
}



# from LSM303 vignette Appendix A
# https://www.sparkfun.com/datasheets/Sensors/Magneto/Tilt%20Compensated%20Compass.pdf
#### STEP 1: TILT, ROLL, PITCH ####

# returns tilt (degrees), roll (radians), and pitch (radians) angles. 
# tilt angle is proportional to the velocity of the water and is used as a velocity proxy
Get_Accel_Angles <- function(TSAccelMag) {
  # magnitude of the projection of acceleration vector in the xy plane 
  TSAccelMag$Gproj_XY <- sqrt(TSAccelMag$xa_norm^2 + TSAccelMag$ya_norm^2)
  
  # calculate tilt angle in degrees 
  TSAccelMag$tiltAngle <- atan2(TSAccelMag$Gproj_XY, TSAccelMag$za_norm) * 180 / pi
  
  # pitch and tilt angles in radians
  # xa_norm must be less than 1 for this to work 
  TSAccelMag$pitchRadians <- asin(-1*TSAccelMag$xa_norm) # should stay bn +/- 45 degrees for linearity (see LSM303 vignette for details) 
  TSAccelMag$rollRadians <- TSAccelMag$ya_norm / cos(TSAccelMag$pitchRadians)
  
  return(TSAccelMag)
}


#### STEP 2: NORMALIZE MAGNETOMETER DATA ####
# note: magnetometer data was calibrated in Oddessy spreadsheet

# data in nanoteslas

# Let Mx, My, Mz be the calibrated magnetometer values (nanoteslas)
# Mx1, My1, and Mz1 are the "normalized" readings.
# Goal: "the square root of the sum squared values should be equal to 1
#   when there is no external interference magnetic field"
#     - LSM303 Vignette

Normalize_Mag <- function(TSAccelMag) {
  # get length (modulus) of magnetometer vector for each observation
  TSAccelMag$m_modulus <- sqrt(TSAccelMag$xm^2 + TSAccelMag$ym^2 + TSAccelMag$zm^2)
  
  # normalize Mx, My, Mz by dividing by modulus
  TSAccelMag$xm_norm <- TSAccelMag$xm / TSAccelMag$m_modulus
  TSAccelMag$ym_norm <- TSAccelMag$ym / TSAccelMag$m_modulus 
  TSAccelMag$zm_norm <- TSAccelMag$zm / TSAccelMag$m_modulus
  
  return(TSAccelMag)
}


#### STEP 3: TILT-COMPENSATED MAG FIELD ####
Compensate_Mag_Field <- function(TSAccelMag) {
  
  # calculations based on eqn 12 in the LSM303 Vignette
  TSAccelMag$xm_comp <- TSAccelMag$xm_norm * cos(TSAccelMag$pitchRadians) +
    TSAccelMag$zm_norm * sin(TSAccelMag$pitchRadians)
  
  TSAccelMag$ym_comp <- TSAccelMag$xm_norm * sin(TSAccelMag$rollRadians) * sin(TSAccelMag$pitchRadians) +
    TSAccelMag$ym_norm * cos(TSAccelMag$rollRadians) -
    TSAccelMag$zm_norm * sin(TSAccelMag$rollRadians) * cos(TSAccelMag$pitchRadians)
  
  TSAccelMag$zm_comp <- -1 * TSAccelMag$xm_norm * cos(TSAccelMag$rollRadians) * sin(TSAccelMag$pitchRadians) +
    TSAccelMag$ym_norm * sin(TSAccelMag$rollRadians) +
    TSAccelMag$zm_norm * cos(TSAccelMag$rollRadians) * cos(TSAccelMag$pitchRadians)
  
  return(TSAccelMag)
}


#### STEP 4: HEADING CALCULATION ####
# see equation 13 in LSM303 vignette

Get_Heading <- function(TSAccelMag) {
  
  # to be applied over rows
  # VERY time-intensive (need to find faster method)
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
  
  # apply getDegreeHeading function to each row (MARGIN = 1)
  # similar to for loop but faster
  TSAccelMag$headingDegrees <- apply(TSAccelMag, MARGIN = 1, getDegreeHeading)
  
  return(TSAccelMag)
}
