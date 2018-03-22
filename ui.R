require(shiny)
require(shinydashboard)
require(plotly)
require(DT)
require(shinyjs)

shinyUI(dashboardPage(
  
  dashboardHeader(title = 'LSM303 Data Processing and Visualization'),
  
  dashboardSidebar(
    uiOutput('sidebarmenu')
  ),
  
  dashboardBody(
    # include the CSS script for additional custom formatting
    tags$head(includeCSS('www/style.css')),
    
    uiOutput('welcomePanel'),
    
    ### BEGIN REAL CONTENT ###
    tabItems(
      tabItem(tabName = 'import',
        
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