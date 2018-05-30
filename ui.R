shinyUI(dashboardPage(
  
  dashboardHeader(title = 'LSM303 Data Processing and Visualization', titleWidth = '100%'),
  
  dashboardSidebar(
    sidebarMenu(id = 'tabs',
      menuItem(text = 'Data Import', tabName = 'import'),
      uiOutput('sidebarmenu')
    )
  ),
  
  dashboardBody(
    # include the CSS script for additional custom formatting
    tags$head(includeCSS('www/style.css')),
    
    uiOutput('welcomePanel'),
    
    ### BEGIN REAL CONTENT ###
    tabItems(
      tabItem(tabName = 'import',
        h1('Welcome', align= 'center'),
        h4('Select a site below and click "Open File" to get started.', align = 'center'),
        fluidRow(
          column(width = 4,
            selectInput('datafile', label = 'Choose a site:', choices = SiteFileList, width = '100%')
          ),
          column(width = 8, style = "margin-top: 25px;",
            actionButton('selectDatafileButton', label = 'Open File')
          )
        ),
        
        br(),
        h4('Crop/Sample Data to reduce load time (optional)'),
        p('Recommended to crop or sample to 2000 observations or less (until faster plotting algorithms implemented)'),
        p('Note: if both "crop" and "sample" are selected, data will first be cropped, then sampled.'),
        p('Date range selection coming soon.'),
        
        fluidRow(
          column(width = 3,
            numericInput('crop_num', label = 'Crop data to first n observations', value = 0, width = '100%')
          ),
          column(width = 2,
            awesomeCheckbox('crop_num_bool', label = "", value = FALSE)
          )
        ),
        
        fluidRow(
          column(width = 3,
            numericInput('sample_num', label = 'Randomly sample n observations', value = 0, width = '100%')
            
          ),
          column(width = 2,
            awesomeCheckbox('sample_num_bool', label = "", value = FALSE)
          )
        )
      ),
      
      tabItem(tabName = 'export',
        
        # button align bottom from r4ndomw4lk https://stackoverflow.com/questions/28960189/bottom-align-a-button-in-r-shiny
        fluidRow(
          column(width = 6,
            textInput("csvName", width = "100%",
              label = "Enter CSV title (ending in .csv)"
            )
          ),
          column(width = 3, style = "margin-top: 25px;",
            downloadButton('writeCSV',label = 'Download CSV')
          )
        ),
        
        br(),
        
        fluidRow(
          box(width = 12,
            div(class = 'overflowXY',
              DT::dataTableOutput('rawdata')
            )    
          )
        )
      ),
      
      ## TODO: resize windrose 
      tabItem(tabName = 'windrose',
        fluidRow(
          box(width = 6,
            plotlyOutput('polarWindrose_tilt_plot', width = "25%")
          ),
          box(width = 6,
            plotlyOutput('cartesianWindrose_tilt_plot')
          )
        ),
        
        fluidRow(
          box(width = 12,
            plotlyOutput('headingHistogram_plot')
          )
        )
      ),
      
      tabItem(tabName = 'timeseries',
        fluidRow(
          box(width = 12,
            plotlyOutput('tilt_ts_plot')
          )
        ),
        fluidRow(
          box(width = 12,
            selectInput('rotateHeadingPlotSelect', 
              label = 'Orient heading relative to: ',
              choices = c('North','West','South','East')
            ),
            plotlyOutput('heading_ts_plot')
          )
        ),
        fluidRow(
          box(width = 12,
            dateRangeInput('stickplot_dates', 
              label = 'Select Dates for Vector Plot (Recommended time span: one week)', 
              start = '2018-01-01', # will be overridden in server.R as soon as data is processed
              end = '2018-01-07'
            ),
            hr(),
            h4('Flow Velocity Vector over Time', align = 'center'),
            p('Light grey line represents tilt angle. Magnitude of each vector is tilt angle (proportional to velocity) and direction is azimuth angle.', align = 'center'),
            plotOutput('stickplot')
          )
        )
      ),
      
      # will be hidden from menu if no calibrated data
      tabItem(tabName = 'compareData',
        fluidRow(
          box(width = 12, height = "500px", title = 'Compare Windrose',
            column(width = 6,
              h4('Calibrated'),
              plotlyOutput('calibrated_windrose')
            ),
            column(width = 6,
              h4('Uncalibrated'),
              plotlyOutput('uncalibrated_windrose')
            )
          )
        ),
        
        fluidRow(
          box(width = 12,
            plotOutput('compare_calibration_histogram')
          )
        ),
        
        fluidRow(
          box(width = 12,
            plotOutput('compare_calibration_tilt')
          )
        )
      )
    )
  )
))