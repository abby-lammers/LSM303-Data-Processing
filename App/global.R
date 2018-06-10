require(shiny)
require(plotly)
require(lubridate)
require(dplyr) # imports magrittr (pipe operators "%>%")
require(DT)
require(shinyjs)
require(oce)
require(shinyWidgets)
require(shinydashboard)

# include function files
source('LSM303-file-helpers.R')
source('plot-helpers.R')

SiteFileList <- list(
  'Akumal South' = 'Data/AKsouth_012_20160403-0714_cleaned.csv',
  'Akumal North' = 'Data/B4_20141222-150317_AKnorth.csv',
  'Casa Cenote' = 'Data/casa_cenote_all_raw_accel_mag.csv',
  'Odyssey' = 'Data/OddessyCalibratedWithDateTime.csv'
)
