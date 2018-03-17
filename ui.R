require(shiny)
require(shinydashboard)
require(plotly)
require(DT)
require(shinyjs)

shinyUI(dashboardPage(
  
  dashboardHeader(title = 'LSM303 Data Processing and Visualization'),
  
  dashboardSidebar(),
  
  dashboardBody(
    # include the CSS script for additional custom formatting
    tags$head(includeCSS('www/style.css')),
    
    # include bindings to use custom javascript
    useShinyjs(),
    
    box(width = 12,
      div(class = 'overflowXY',
        DT::dataTableOutput('rawdata')
      )    
    )
  )
))