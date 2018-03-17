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
      menuItem(text = 'Windrose', tabName = 'windrose')
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
            plotlyOutput('polarWindrose_tilt', width = "25%")
          ),
          box(width = 6,
            plotlyOutput('cartesianWindrose_tilt')
          )
        ),
        
        fluidRow(
          box(width = 12,
            plotlyOutput('headingHistogram')
          )
        )
      )
    )
    
    
  )
))