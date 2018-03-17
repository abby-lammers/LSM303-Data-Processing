require(shiny)
require(plotly)
require(lubridate)
require(dplyr) # imports magrittr (pipe operators "%>%")
require(DT)
require(shinyjs)

# include function files
source('LSM303-file-helpers.R')
source('plot-helpers.R')

shinyServer(function(input, output, session) {
  
  #### TSAccelMag_Cal ####
  TSAccelMag_Cal <- reactive({
    
    # for reference, %>% is R's pipe operator
    # from the magrittr package (imported by dplyr)
    # for more information: http://magrittr.tidyverse.org/
    TSAccelMag <- Read_LSM303_csv('AKsouth_012_20160403-0714.csv', sample = 300) %>% 
      Normalize_Accel() %>% 
      Get_Accel_Angles() %>% 
      Normalize_Mag() %>% 
      Compensate_Mag_Field() %>% 
      Get_Heading()
    
    return(TSAccelMag)
  })
  
  #### > rawdata ####
  output$rawdata <- DT::renderDataTable(TSAccelMag_Cal(), 
    options = list(
      pageLength = 100
      #pageLength = nrow(TSAccelMag_Cal()) # view all data on one page without having to click through
    )
  )
  
  #### > polarWindrose_tilt ####
  output$polarWindrose_tilt <- renderPlotly({
    windrose_heading_tilt(TSAccelMag_Cal())
  })
  
  #### > cartesianWindrose_tilt ####
  output$cartesianWindrose_tilt <- renderPlotly({
    cartesian_heading_tilt(TSAccelMag_Cal())
  })
  
  output$headingHistogram <- renderPlotly({
    histogram_heading_frequency(TSAccelMag_Cal())
  })
})