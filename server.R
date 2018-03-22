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

shinyServer(function(input, output, session) {
  
  #### TSAccelMag_Cal ####
  TSAccelMag_Cal <- reactive({
    
    # for reference, %>% is R's pipe operator
    # from the magrittr package (imported by dplyr)
    # for more information: http://magrittr.tidyverse.org/
    TSAccelMag <- Read_LSM303_csv('B4_20141222-150317_AKnorth.csv') %>% 
    # TSAccelMag <- Read_LSM303_csv('AKsouth_012_20160403-0714.csv', crop = 3000) %>% 
      Normalize_Accel() %>% 
      Get_Accel_Angles() %>% 
      Normalize_Mag() %>% 
      Compensate_Mag_Field() %>% 
      Get_Heading() %>% 
      arrange(datetime)
    
    return(TSAccelMag)
  })
  
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
  
  #### > tilt_ts_plot ####
  output$tilt_ts_plot <- renderPlotly({
    line_ts_tiltangle(TSAccelMag_Cal())
  })
  
  #### > heading_ts_plot ####
  output$heading_ts_plot <- renderPlotly({
    scatter_ts_heading(TSAccelMag_Cal())
  })
  
  output$stickplot <- renderPlot({
    firstdate <- TSAccelMag_Cal() %>% select(datetime) %>% head(1)[1] %>% floor_date('day')
    lastdate <- firstdate + weeks(1)
    
    heading_stickplot(TSAccelMag_Cal(), firstdate, lastdate)
  })
})