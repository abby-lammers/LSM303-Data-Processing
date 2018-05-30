require(shiny)
require(plotly)
require(lubridate)
require(dplyr) # imports magrittr (pipe operators "%>%")
require(DT)
require(shinyjs)
require(oce)
require(shinyWidgets)

# include function files
source('LSM303-file-helpers.R')
source('plot-helpers.R')

SiteFileList <- list(
  'Akumal South' = 'AKsouth_012_20160403-0714_cleaned.csv',
  'Akumal North' = 'B4_20141222-150317_AKnorth.csv',
  'Casa Cenote' = 'casa_cenote_all_raw_accel_mag.csv',
  'Odyssey' = 'OddessyCalibratedWithDateTime.csv',
  'Calibration 1 (XY circle)' = 'calibration 1 clockwise circle true N.csv',
  'Calibration 2 (Rotation around Z axis)' = 'calibration 2 ccw mag rotation no tilt.csv',
  'Calibration 3 (N/S/E/W Tilt)' = 'calibration 3 plus drill.csv'
)
