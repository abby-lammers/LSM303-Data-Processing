shinyServer(function(input, output, session) {
  
  #### $$$ vals ####
  vals <- reactiveValues(
    timeSeries_xRange = NULL
  )
  
  #### sample_crop_index ####
  # ensures that when rows are sampled, 
  # the same rows are sampled from the calibrated and uncalibrated data. 
  # TODO: just get the raw data.frame (read_lsm303_csv() in a separate reactive())
  #   to avoid all of this mess
  sample_crop_index <- eventReactive(input$selectDatafileButton, {
    rownum <- nrow(read.csv(input$datafile))
    
    # crop should be less than the number of rows of the data set,
    # otherwise it should be 0
    if(input$crop_num_bool && input$crop_num < rownum) {
      index <- 1:input$crop_num
    } else {
      index <- 1:rownum
    }
    
    # if sample is selected AND greater than ZERO AND
    # sample is less than the length of index,
    # sample sample_num from index
    
    if (input$sample_num_bool && input$sample_num > 0 && input$sample_num < length(index)) {
      index <- sort(sample(index, input$sample_num))
    }
    
    return(index)
  })
  
  #### ____ TSACCELMAG ____ ####
  #### TSAccelMag_Raw ####
  TSAccelMag_Raw <- eventReactive(input$selectDatafileButton, {
    # Parallel to computation for TSAccelMag_Cal but starting with uncalibrated data
    TSAccelMag <- Read_LSM303_csv(
      fileName = input$datafile, 
      index = sample_crop_index()
    ) %>% 
      Normalize_Accel(cal = FALSE) %>% 
      Get_Accel_Angles() %>% 
      Normalize_Mag(cal = FALSE) %>% 
      Compensate_Mag_Field() %>% 
      Get_Heading() %>% 
      arrange(datetime) %>% 
      Round_TSAccelMag()
    
    return(TSAccelMag)
  })
  
  
  #### TSAccelMag_Cal ####
  TSAccelMag_Cal <- eventReactive(input$selectDatafileButton, {
    
    # for reference, %>% is R's pipe operator
    # from the magrittr package (imported by dplyr)
    # for more information: http://magrittr.tidyverse.org/
    TSAccelMag <- Read_LSM303_csv(
      fileName = input$datafile, 
      index = sample_crop_index()
    ) 
    
    # if calibrated columns are not present, return NULL
    if (sum(colnames(TSAccelMag) == 'xa_cal') < 1) {
      return(NULL)
    }
    
    TSAccelMag <- TSAccelMag %>% 
      Normalize_Accel(cal = TRUE) %>% 
      Get_Accel_Angles() %>% 
      Normalize_Mag(cal = TRUE) %>% 
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
    req(sample_crop_index())
    
    if (!is.null(TSAccelMag_Cal())) {
      ui <- sidebarMenu(
        menuItem(text = 'Data Export',tabName = 'export'),
        menuItem(text = 'Windrose', tabName = 'windrose'),
        menuItem(text = 'Time-Series Plots', tabName = 'timeseries'),
        menuItem(text = 'Compare to Uncalibrated Data', tabName = 'compareData')
      )
    } else {
      ui <- sidebarMenu(
        menuItem(text = 'Data Export',tabName = 'export'),
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
  
  
  #### ____ DOWNLOAD ____ ####
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
      value = paste0("Processed_",year(now()),"_", month(now()),"_",day(now()),"_", input$datafile)
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
  
  #### :: plotly_relayout ####
  ## event_data(plotly_relayout)
  # $xaxis.range[0] = min
  # $xaxis.range[1] = max
  # $yaxis.range[0]... you get the picture
  observeEvent(eventExpr = event_data("plotly_relayout"), ignoreInit = TRUE, {
    if (!is.null(event_data("plotly_relayout"))) {
      vals$timeSeries_xRange <- c(event_data("plotly_relayout")$`xaxis.range[0]`, event_data("plotly_relayout")$`xaxis.range[1]`)
    }
  })
  
  #### :: reset range ####
  # reset range on time series plots when new data set is uploaded
  observeEvent(TSAccelMag_Raw(), {
    vals$timeSeries_xRange <- NULL
  })
  
  #### > tilt_ts_plot ####
  output$tilt_ts_plot <- renderPlotly({
    line_ts_tiltangle(TSAccelMag_Main(), xrange = vals$timeSeries_xRange)
  })
  
  #### > heading_ts_plot ####
  output$heading_ts_plot <- renderPlotly({
    scatter_ts_heading(TSAccelMag_Main(), xrange = vals$timeSeries_xRange, rotateReference = input$rotateHeadingPlotSelect)
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