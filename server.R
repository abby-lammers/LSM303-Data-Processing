require(shiny)
require(plotly)
require(lubridate)
require(dplyr) # imports magrittr (pipe operators "%>%")
require(DT)
require(shinyjs)
require(oce)

# include function files
source('LSM303-file-helpers.R')
source('plot-helpers.R')

CSV_NAME <- 'OddessyCalibratedWithDateTime.csv'

shinyServer(function(input, output, session) {
  
  #### TSAccelMag_Cal ####
  TSAccelMag_Cal <- reactive({
    
    # for reference, %>% is R's pipe operator
    # from the magrittr package (imported by dplyr)
    # for more information: http://magrittr.tidyverse.org/
    TSAccelMag <- Read_LSM303_csv(CSV_NAME, sample = 2000) %>% 
    # TSAccelMag <- Read_LSM303_csv('AKsouth_012_20160403-0714.csv', crop = 3000) %>% 
      Normalize_Accel(cal = TRUE) %>% 
      Get_Accel_Angles() %>% 
      Normalize_Mag(cal = TRUE) %>% 
      Compensate_Mag_Field() %>% 
      Get_Heading() %>% 
      arrange(datetime)
    
    return(TSAccelMag)
  })
  
  #### TSAccelMag_Raw ####
  TSAccelMag_Raw <- reactive({
    # Parallel to computation for TSAccelMag_Cal but starting with uncalibrated data
    TSAccelMag <- Read_LSM303_csv(CSV_NAME, sample = 2000) %>% 
      # TSAccelMag <- Read_LSM303_csv('AKsouth_012_20160403-0714.csv', crop = 3000) %>% 
      Normalize_Accel(cal = FALSE) %>% 
      Get_Accel_Angles() %>% 
      Normalize_Mag(cal = FALSE) %>% 
      Compensate_Mag_Field() %>% 
      Get_Heading() %>% 
      arrange(datetime)
    
    return(TSAccelMag)
  })
  
  #### ____ WINDROSE ____ ####
  #### > rawdata ####
  output$rawdata <- DT::renderDataTable(TSAccelMag_Cal(), 
    options = list(
      pageLength = 100
      #pageLength = nrow(TSAccelMag_Cal()) # view all data on one page without having to click through
    )
  )
  
  #### > polarWindrose_tilt_plot ####
  output$polarWindrose_tilt_plot <- renderPlotly({
    windrose_heading_tilt(TSAccelMag_Cal())
  })
  
  #### > cartesianWindrose_tilt_plot ####
  output$cartesianWindrose_tilt_plot <- renderPlotly({
    cartesian_heading_tilt(TSAccelMag_Cal())
  })
  
  #### > headingHistogram_plot ####
  output$headingHistogram_plot <- renderPlotly({
    histogram_heading_frequency(TSAccelMag_Cal())
  })
  
  #### ____ TIME-SERIES ____ ####
  
  #### > tilt_ts_plot ####
  output$tilt_ts_plot <- renderPlotly({
    line_ts_tiltangle(TSAccelMag_Cal())
  })
  
  #### > heading_ts_plot ####
  output$heading_ts_plot <- renderPlotly({
    scatter_ts_heading(TSAccelMag_Cal())
  })
  
  #### > stickplot ####
  output$stickplot <- renderPlot({
    firstdate <- TSAccelMag_Cal() %>% select(datetime) %>% head(1)[1] %>% floor_date('day')
    lastdate <- firstdate + weeks(1)
    
    heading_stickplot(TSAccelMag_Cal(), firstdate, lastdate)
  })
  
  #### ____ CALIBRATION  ____ ####
  #### > calibrated_windrose ####
  output$calibrated_windrose <- renderPlotly({
    windrose_heading_tilt(TSAccelMag_Cal())
  })
  
  #### > uncalibrated_windrose ####
  output$uncalibrated_windrose <- renderPlotly({
    windrose_heading_tilt(TSAccelMag_Raw())
  })
  
  #### > compare_calibration_histogram ####
  output$compare_calibration_histogram <- renderPlot({
    compare_angle_histogram_gg(TSAccelMag_Cal(), TSAccelMag_Raw())
  })
  
  #### > compare_calibration_tilt ####
  output$compare_calibration_tilt <- renderPlot({
    compare_tilt_line_gg(TSAccelMag_Cal(), TSAccelMag_Raw())
  })
})