require(shiny)
require(shinydashboard)
require(plotly)
require(DT)
require(shinyjs)

shinyUI(dashboardPage(
  
  dashboardHeader(title = 'LSM303 Data Processing and Visualization'),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem(text = 'Data Import',tabName = 'import'),
      menuItem(text = 'Windrose', tabName = 'windrose'),
      menuItem(text = 'Time-Series Plots', tabName = 'timeseries'),
      menuItem(text = 'Compare to Uncalibrated Data', tabName = 'compareData')
    )
  ),
  
  dashboardBody(
    # include the CSS script for additional custom formatting
    tags$head(includeCSS('www/style.css')),
    
    ### BEGIN REAL CONTENT ###
    tabItems(
      tabItem(tabName = 'import',
        
        box(width = 12,
          div(class = 'overflowXY',
            DT::dataTableOutput('rawdata')
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
            plotOutput('stickplot')
          )
        )
      ),
      
      tabItem(tabName = 'compareData',
        fluidRow(
          box(width = 12, height = 500, title = 'Compare Windrose',
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