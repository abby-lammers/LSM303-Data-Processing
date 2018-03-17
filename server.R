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
  
  TSAccelMag_CSV <- reactive({
    
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
  
  output$rawdata <- DT::renderDataTable(TSAccelMag_CSV(), options = list(
    pageLength = nrow(TSAccelMag_CSV),
    initComplete = JS('function(setting, json) { alert("done"); }')
  ))
  
})