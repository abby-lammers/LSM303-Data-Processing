######
# from server.R
require(shiny)
require(plotly)
require(lubridate)
require(dplyr) # imports magrittr (pipe operators "%>%")
require(DT)
require(shinyjs)
require(oce)
require(shinyWidgets)

# include function files
source('LSM303-file-helpers.R')
#####

thing <- read.csv('headingtestfile_frompololu_3.csv') %>% 
  Normalize_Accel(cal = FALSE) %>% 
  Get_Accel_Angles() %>% 
  Normalize_Mag(cal = FALSE) %>% 
  Compensate_Mag_Field() %>% 
  Get_Heading() 

thing$index <- 1:nrow(thing)

thing$H_vignette <- thing$H_vignette %% 360

plot_ly(
  type = 'scatter',
  mode = 'markers+lines',
  data = thing,
  x = ~index,
  y = ~H_pololu,
  name = 'pololu'
) %>% add_trace(
  y = ~H_vignette,
  name = 'vignette'
) %>% add_trace(
  y = ~azimuthDegrees_adjusted,
  name = 'reggie'
)

plot_ly(
  type = 'scatter',
  mode = 'markers+lines',
  data = thing,
  x = ~index,
  y = ~azimuthDegrees_adjusted - H_pololu,
  name = 'vs pololu'
) %>% add_trace(
  y = ~azimuthDegrees_adjusted - H_vignette,
  name = 'vs vignette'
) %>% add_trace(
  y = ~H_pololu - H_vignette,
  name = 'compare'
)

plot_ly(
  type = 'scatter',
  mode = 'markers+lines',
  data = thing,
  x = ~index,
  y = ~pitch,
  name = 'arduino'
) %>% add_trace(
  y = ~pitchRadians,
  name = 'R'
) %>% add_trace(
  y = ~pitchRadians - pitch,
  name = 'difference'
) %>% layout(
  title = 'Compare pitch radians'
) %>% config(displayModeBar = FALSE)

plot_ly(
  type = 'scatter',
  mode = 'markers+lines',
  data = thing,
  x = ~index,
  y = ~Xnorm,
  name = 'arduino'
) %>% add_trace(
  y = ~xa_norm,
  name = 'R'
) %>% add_trace(
  y = ~Xnorm - xa_norm,
  name = 'difference'
) %>% layout(
  title = 'Compare pitch radians'
) %>% config(displayModeBar = FALSE)
