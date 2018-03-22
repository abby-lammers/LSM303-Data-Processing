require(dplyr)
require(plotly)
require(lubridate)

# Helper to assign quadrant based on X and Y position
AssignQuadrant <- function(Xvec, Yvec) {
  ReturnVec <- rep(0, length(Xvec))
  ReturnVec[Xvec >= 0 & Yvec >= 0] <- 1
  ReturnVec[Xvec < 0 & Yvec >= 0] <- 2
  ReturnVec[Xvec < 0 & Yvec < 0] <- 3
  ReturnVec[Xvec >= 0 & Yvec < 0] <- 4
  # if any of the observations are exactly equal to zero, a zero is returned
  return(ReturnVec)
}

# returns azimuth/bearing based on X and Y vectors
GetAzimuth <- function(Xvec, Yvec) {
  Quadrant <- AssignQuadrant(Xvec, Yvec)
  
  ReturnVec <- atan(Yvec/Xvec) * 180 / pi
  ReturnVec[Quadrant == 2 | Quadrant == 3] <- 180 + ReturnVec[Quadrant == 2 | Quadrant == 3]
  ReturnVec[Quadrant == 4] <- 360 + ReturnVec[Quadrant == 4]
  
  return(ReturnVec)
}


#### STEP 0: READ CSV ####
# read csv of data
# COLUMN NAMES: c('datetime','xa','ya','za','xm','ym','zm')
Read_LSM303_csv <- function(fileName, sample = 0, crop = 0) {
  TSAccelMag <- read.csv(fileName) #time series accelerometer and magnetometer data
  TSAccelMag$datetime <- lubridate::mdy_hm(TSAccelMag$datetime) # convert date string to date object
  
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


#### STEP 1: NORMALIZE ACCELEROMETER DATA ####
# Let Ax, Ay, Az be the calibrated accelerometer values
# Ax1, Ay1, and Az1 are the "normalized" readings.
# Goal: "the root sum of Ax1, Ay1, and Az1 should be equal to one when the accelerometer is still"
#     - LSM303 Vignette
Normalize_Accel <- function(TSAccelMag, cal = TRUE) {
  
  if (cal) { # use calibrated data
    
    # get length (modulus) of acceleration vector
    TSAccelMag$a_modulus <- sqrt(TSAccelMag$xa_cal^2 + TSAccelMag$ya_cal^2 + TSAccelMag$za_cal^2)
    
    # normalize each component so that it becomes a unit vector (i.e. magnitude of 1)
    # especially important for trig functions
    TSAccelMag$xa_norm <- TSAccelMag$xa_cal / TSAccelMag$a_modulus
    TSAccelMag$ya_norm <- TSAccelMag$ya_cal / TSAccelMag$a_modulus
    TSAccelMag$za_norm <- TSAccelMag$za_cal / TSAccelMag$a_modulus
    
  } else { # use uncalibrated data
    
    # get length (modulus) of acceleration vector
    TSAccelMag$a_modulus <- sqrt(TSAccelMag$xa_raw^2 + TSAccelMag$ya_raw^2 + TSAccelMag$za_raw^2)
    
    # normalize each component so that it becomes a unit vector (i.e. magnitude of 1)
    # especially important for trig functions
    TSAccelMag$xa_norm <- TSAccelMag$xa_raw / TSAccelMag$a_modulus
    TSAccelMag$ya_norm <- TSAccelMag$ya_raw / TSAccelMag$a_modulus
    TSAccelMag$za_norm <- TSAccelMag$za_raw / TSAccelMag$a_modulus
  }
  
  return(TSAccelMag)
}


# from LSM303 vignette Appendix A
# https://www.sparkfun.com/datasheets/Sensors/Magneto/Tilt%20Compensated%20Compass.pdf
#### STEP 2: TILT, ROLL, PITCH ####

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
  
  
  # roll calculation modified slightly from LSM303 vignette
  # cos(pitchRadians) reduces to 1
  # old:
  # TSAccelMag$rollRadians <- asin(TSAccelMag$ya_norm / cos(TSAccelMag$pitchRadians))
  TSAccelMag$rollRadians <- asin(TSAccelMag$ya_norm)
  
  # Use GetAzimuth function to get Azimuth relative to body coordinates
  # Not yet relative to true N (need magnetometer heading first)
  TSAccelMag$azimuthDegrees <- GetAzimuth(TSAccelMag$xa_norm, TSAccelMag$ya_norm)
  
  return(TSAccelMag)
}


#### STEP 3: NORMALIZE MAGNETOMETER DATA ####
# note: magnetometer data was calibrated in Oddessy spreadsheet

# data in nanoteslas

# Let Mx, My, Mz be the calibrated magnetometer values (nanoteslas)
# Mx1, My1, and Mz1 are the "normalized" readings.
# Goal: "the square root of the sum squared values should be equal to 1
#   when there is no external interference magnetic field"
#     - LSM303 Vignette

Normalize_Mag <- function(TSAccelMag, cal = TRUE) {
  
  if(cal) { # use calibrated data
    
    # get length (modulus) of magnetometer vector for each observation
    TSAccelMag$m_modulus <- sqrt(TSAccelMag$xm_cal^2 + TSAccelMag$ym_cal^2 + TSAccelMag$zm_cal^2)
    
    # normalize Mx, My, Mz by dividing by modulus
    TSAccelMag$xm_norm <- TSAccelMag$xm_cal / TSAccelMag$m_modulus
    TSAccelMag$ym_norm <- TSAccelMag$ym_cal / TSAccelMag$m_modulus 
    TSAccelMag$zm_norm <- TSAccelMag$zm_cal / TSAccelMag$m_modulus
    
  } else { # use uncalibrated data
    
    # get length (modulus) of magnetometer vector for each observation
    TSAccelMag$m_modulus <- sqrt(TSAccelMag$xm_raw^2 + TSAccelMag$ym_raw^2 + TSAccelMag$zm_raw^2)
    
    # normalize Mx, My, Mz by dividing by modulus
    TSAccelMag$xm_norm <- TSAccelMag$xm_raw / TSAccelMag$m_modulus
    TSAccelMag$ym_norm <- TSAccelMag$ym_raw / TSAccelMag$m_modulus 
    TSAccelMag$zm_norm <- TSAccelMag$zm_raw / TSAccelMag$m_modulus
  }
  
  
  return(TSAccelMag)
}


#### STEP 4: TILT-COMPENSATED MAG FIELD ####
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


#### STEP 5: HEADING CALCULATION ####
# see equation 13 in LSM303 vignette

Get_Heading <- function(TSAccelMag) {
  # see equation 13 in LSM303 vignette
  # use GetAzimuth function to get magnetometer heading
  #   (The angle between the XY projection of the magnetometer vector
  #     relative to the true magnetic north)
  TSAccelMag$headingDegrees <- GetAzimuth(TSAccelMag$xm_comp, TSAccelMag$ym_comp)
  
  # add azimuth and heading degrees
  # then take remainder when divided by 360 to get angle between 0 and 360
  TSAccelMag$azimuthDegrees_adjusted <- (TSAccelMag$headingDegrees + TSAccelMag$azimuthDegrees) %% 360
  
  return(TSAccelMag)
}

#### STEP 6: ROUNDING ####
Round_TSAccelMag <- function(TSAccelMag) {

  # round to 1 decimal
  roundToOneDecimal <-  c('a_modulus', 'tiltAngle','azimuthDegrees','m_modulus','headingDegrees','azimuthDegrees_adjusted')
  TSAccelMag[ , roundToOneDecimal] <- round(TSAccelMag[, roundToOneDecimal], 1)
  
  # round to 4 decimal places (normalized measurements and angles in radians)
  roundToFourDecimals <- c('xa_norm','ya_norm','za_norm','Gproj_XY','pitchRadians','rollRadians','xm_norm','ym_norm','zm_norm','xm_comp','ym_comp','zm_comp')
  TSAccelMag[ , roundToFourDecimals] <- round(TSAccelMag[, roundToFourDecimals], 4)
  
  return(TSAccelMag)
}
