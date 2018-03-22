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

CSV_NAME <- 'casa_cenote_all_raw_accel_mag.csv'

shinyServer(function(input, output, session) {
  
  output$welcomePanel <- renderUI({
    if (length(paste(input$tabs)) == 0) {
      ui <- list(
        h1('Welcome', align= 'center'),
        h4('Click a tab to the left to get started.', align = 'center')
      )
    } else {
      ui <- list()
    }
    
    return(ui)
  })
  
  #### TSAccelMag_Cal ####
  TSAccelMag_Cal <- reactive({
    
    # for reference, %>% is R's pipe operator
    # from the magrittr package (imported by dplyr)
    # for more information: http://magrittr.tidyverse.org/
    TSAccelMag <- Read_LSM303_csv(CSV_NAME)
    
    # if calibrated columns are not present, return NULL
    if (sum(colnames(TSAccelMag) == 'xa_cal') < 1) {
      return(NULL)
    }
    
    TSAccelMag <- TSAccelMag %>% 
    # TSAccelMag <- Read_LSM303_csv('AKsouth_012_20160403-0714.csv', crop = 3000) %>% 
      Normalize_Accel(cal = TRUE) %>% 
      Get_Accel_Angles() %>% 
      Normalize_Mag(cal = TRUE) %>% 
      Compensate_Mag_Field() %>% 
      Get_Heading() %>% 
      arrange(datetime) %>% 
      Round_TSAccelMag()
    
    return(TSAccelMag)
  })
  
  #### TSAccelMag_Raw ####
  TSAccelMag_Raw <- reactive({
    # Parallel to computation for TSAccelMag_Cal but starting with uncalibrated data
    TSAccelMag <- Read_LSM303_csv(CSV_NAME, crop = 2000) %>% 
      # TSAccelMag <- Read_LSM303_csv('AKsouth_012_20160403-0714.csv', crop = 3000) %>% 
      Normalize_Accel(cal = FALSE) %>% 
      Get_Accel_Angles() %>% 
      Normalize_Mag(cal = FALSE) %>% 
      Compensate_Mag_Field() %>% 
      Get_Heading() %>% 
      arrange(datetime) %>% 
      Round_TSAccelMag()
    
    return(TSAccelMag)
  })
  
  #### TSAccelMag_Main ####
  TSAccelMag_Main <- reactive({
    # if calibrated data exists, use that.
    # if not, use raw data. 
    if (!is.null(TSAccelMag_Cal())) {
      return(TSAccelMag_Cal())
    } else {
      return(TSAccelMag_Raw())
    }
  })
  
  #### > sidebarmenu ####
  output$sidebarmenu <- renderUI({
    req(TSAccelMag_Main())
    if (!is.null(TSAccelMag_Cal())) {
      ui <- sidebarMenu(id = 'tabs',
        menuItem(text = 'Data Import',tabName = 'import'),
        menuItem(text = 'Windrose', tabName = 'windrose'),
        menuItem(text = 'Time-Series Plots', tabName = 'timeseries'),
        menuItem(text = 'Compare to Uncalibrated Data', tabName = 'compareData')
      )
    } else {
      ui <- sidebarMenu(id = 'tabs',
        menuItem(text = 'Data Import',tabName = 'import'),
        menuItem(text = 'Windrose', tabName = 'windrose'),
        menuItem(text = 'Time-Series Plots', tabName = 'timeseries')
      )
    }
    
    return(ui)
  })
  
  #### :: stickplot dates ####
  observe({
    req(TSAccelMag_Main())
    
    firstdate <- TSAccelMag_Main()$datetime[1][1] %>% floor_date('day')
    oneweek <- firstdate + weeks(1)
    lastdate <- TSAccelMag_Main()$datetime[nrow(TSAccelMag_Main())][1] %>% floor_date('day')
    
    updateDateRangeInput(
      session = session,
      inputId = 'stickplot_dates', 
      start = firstdate,
      end = oneweek,
      min = firstdate,
      max = lastdate
    )
  })
  
  #### > rawdata ####
  ToDisplay <- reactive({
    req(TSAccelMag_Main())
    
    # TODO: make date display on one line
    Table <- TSAccelMag_Main()
    Table$datetime <- paste(Table$datetime)
    
    return(Table)
    
  })
  
  output$rawdata <- DT::renderDataTable(ToDisplay(), 
    options = list(
      pageLength = 100
      #pageLength = nrow(TSAccelMag_Cal()) # view all data on one page without having to click through
    )
  )
  
  #### :: update csv name ####
  observe(
    updateTextInput(
      session = session,
      inputId = 'csvName',
      value = paste0("Processed_",year(now()),"_", month(now()),"_",day(now()),"_", CSV_NAME)
    )
  )
  
  #### > writeCSV ####
  output$writeCSV <- downloadHandler(
    filename = function(){ paste(input$csvName) },
    content = function(file) {
      write.csv(TSAccelMag_Main(), file)
    },
    contentType = "text/csv"
  )
  
  #### ____ WINDROSE ____ ####
  
  #### > polarWindrose_tilt_plot ####
  output$polarWindrose_tilt_plot <- renderPlotly({
    windrose_heading_tilt(TSAccelMag_Main())
  })
  
  #### > cartesianWindrose_tilt_plot ####
  output$cartesianWindrose_tilt_plot <- renderPlotly({
    cartesian_heading_tilt(TSAccelMag_Main())
  })
  
  #### > headingHistogram_plot ####
  output$headingHistogram_plot <- renderPlotly({
    histogram_heading_frequency(TSAccelMag_Main())
  })
  
  #### ____ TIME-SERIES ____ ####
  
  #### > tilt_ts_plot ####
  output$tilt_ts_plot <- renderPlotly({
    line_ts_tiltangle(TSAccelMag_Main())
  })
  
  #### > heading_ts_plot ####
  output$heading_ts_plot <- renderPlotly({
    scatter_ts_heading(TSAccelMag_Main())
  })
  
  #### > stickplot ####
  output$stickplot <- renderPlot({
    # get dates from stickplot_dates selector on ui side
    firstdate <- ymd(input$stickplot_dates[1])
    lastdate <- ymd(input$stickplot_dates[2])
    
    validate(need(firstdate + days(1) < lastdate, 'Please select a date range greater than one day.'))

    heading_stickplot(TSAccelMag_Main(), firstdate, lastdate)
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