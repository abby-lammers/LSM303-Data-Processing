library(plotly)
library(shiny)

source('plot-helpers.R')

ui <- fluidPage(
  
  fluidRow(plotlyOutput('tilt_ts_plot')),
  fluidRow(plotlyOutput("heading_ts_plot")),
  verbatimTextOutput('textt')
)

server <- function(input, output, session) {
  
  vals <- reactiveValues(
    timeSeries_xRange = NULL
  )
  
  ## event_data(plotly_relayout)
  # $xaxis.range[0] = min
  # $xaxis.range[1] = max
  # $yaxis.range[0]... you get the picture
  observeEvent(eventExpr = event_data("plotly_relayout"), ignoreInit = TRUE, {
    if (!is.null(event_data("plotly_relayout"))) {
      vals$timeSeries_xRange <- c(event_data("plotly_relayout")$`xaxis.range[0]`, event_data("plotly_relayout")$`xaxis.range[1]`)
    }
  })
  
  #### > tilt_ts_plot ####
  output$tilt_ts_plot <- renderPlotly({
    line_ts_tiltangle(TSAccelMag, xrange = vals$timeSeries_xRange)
  })
  
  #### > heading_ts_plot ####
  output$heading_ts_plot <- renderPlotly({
    scatter_ts_heading(TSAccelMag, xrange = vals$timeSeries_xRange)
  })
  
  output$textt <- renderPrint(event_data("plotly_relayout"))
}

shinyApp(ui, server)