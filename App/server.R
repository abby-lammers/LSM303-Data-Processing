shinyServer(function(input, output, session) {
  
  #### $$$ vals ####
  vals <- reactiveValues(
    timeSeries_xRange = NULL
  )
  
  #### > fileUploadSelector ####
  output$fileUploadSelector <- renderUI({
    validate(need(input$ExistingOrCSV, "Loading..."))
    
    if(input$ExistingOrCSV == 'csv') {
      ui <- fileInput('csv_datafile', 
        label = '', 
        accept = c("text/csv", "text/comma-separated-values,text/plain",".csv"),
        buttonLabel = "Browse...", 
        placeholder = "No file selected", 
        width = '100%'
      )
    } else {
      ui <- selectInput('existing_datafile', label = 'Choose a site:', choices = SiteFileList, width = '100%')
    }
    
    return(ui)
  })
  
  #### < csvUploadInfoModalButton ####
  observeEvent(input$csvUploadInfoModalButton, {
    showModal(modalDialog(
      title = "CSV Upload Requirements",
      h2("Required Columns"),
      imageOutput('csvColumnInfoTable'), # see below
      size = 'm',
      easyClose = TRUE
    ))
  })
  # don't ask me why you have to wrap the image in a separate renderimage statement,
  #   I can't figure it out either.
  # I think it may have to do with relative vs absolute file addresses (?)
  output$csvColumnInfoTable <- renderImage({
    list(src = 'www/CSV column description table.png',
      contentType = 'image/png',
      width = '100%'
    )
  }, deleteFile = FALSE)
  
  #### MasterRawCSV ####
  MasterRawCSV <- eventReactive(input$selectDatafileButton, {
    
    ## read the csv (depends on file source)
    if (input$ExistingOrCSV == 'existing') {
      TSAccelMag <- read.csv(input$existing_datafile)
    } else {
      TSAccelMag <- read.csv(input$csv_datafile$datapath) # $datapath gets to actual file
    }
    
    ## parse datetime
    TSAccelMag$datetime <- lubridate::mdy_hm(TSAccelMag$datetime) # convert date string to date object
    
    ## crop and sample for ease of development
    ## speeds up plotting
    rownum <- nrow(TSAccelMag)
    
    ## 1. optional crop
    if (input$crop_num_bool && input$crop_num < rownum && input$crop_num > 0) {
      TSAccelMag <- head(TSAccelMag, input$crop_num)
      rownum <- input$crop_num
    }
    
    ## 2. optional sample
    if (input$sample_num_bool && input$sample_num < rownum && input$sample_num > 0) {
      TSAccelMag <- TSAccelMag[sort(sample(1:rownum, input$sample_num)), ]
    }
    
    return(TSAccelMag)
  })
  
  #### ____ TSACCELMAG ____ ####
  #### TSAccelMag_Raw ####
  TSAccelMag_Raw <- eventReactive(input$selectDatafileButton, {
    # Parallel to computation for TSAccelMag_Cal but starting with uncalibrated data
    TSAccelMag <- MasterRawCSV() %>% 
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
    TSAccelMag <- MasterRawCSV()
    
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
    req(MasterRawCSV())
    
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