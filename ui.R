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
      menuItem(text = 'Windrose', tabname = 'windrose')
    )
  ),
  
  dashboardBody(
    # include the CSS script for additional custom formatting
    tags$head(includeCSS('www/style.css')),
    
    # include bindings to use custom javascript
    useShinyjs(),
    
    ### BEGIN REAL CONTENT ###
    tabItems(
      tabItem(tabName = 'import',
        
        box(width = 12,
          div(class = 'overflowXY',
            DT::dataTableOutput('rawdata')
          )    
        )
      ),
      
      tabItem(tabName = 'windrose',
        plotlyOutput('polarWindrose_tilt'),
        plotlyOutput('cartesianWindrose_tilt'),
        plotlyOutput('headingHistogram')
      )
    )
    
    
  )
))