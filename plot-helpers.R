require(plotly)
require(lubridate)

TITLEFONT <- list(
  family = c('Open Sans','Arial'),
  size = 18,
  color = 'black'
)

MAINFONT <- list(
  family = c('Open Sans','Arial'),
  size = 12,
  color = 'black'
)


#### STEP 5: WINDROSE (FREQUENCY) ####
Get_Heading_Frequency_Table <- function(TSAccelMag) {
  tempFreqTable <- table(round(TSAccelMag$azimuthDegrees_adjusted,1))
  AngleFrequencyTable <- data.frame(
    Angle = as.numeric(names(tempFreqTable)),
    Frequency = as.vector(tempFreqTable)
  )
  
  return(AngleFrequencyTable)
}

windrose_heading_frequency <- function(TSAccelMag, autoAxes = FALSE) {
  HeadingTable <- Get_Heading_Frequency_Table(TSAccelMag)
  
  p <- plot_ly(
    data = HeadingTable,
    type = 'area',
    t = ~Angle,
    r = ~Frequency,
    opacity = 0.2
  ) %>% layout(
    title = "Heading Angle (&deg; from North) vs Frequency \n Polar Histogram",
    font = TITLEFONT, 
    margin = list(t = 40, r = 20, l = 20),
    radialaxis = list(
      visible = T,
      font = MAINFONT
    ),
    angularaxis = list(
      visible = T,
      type = 'linear',
      thetaunit = 'degrees',
      range = ifelse(autoAxes, range(HeadingTable$Angle), c(0,360)),
      tickfont = MAINFONT
    ),
    orientation = 270,
    showlegend = F
  ) %>% config(
    displayModeBar = 'hover'
  )
  
  return(p)
}




# 5.3 Frequency vs Angle Bar Plot 
histogram_heading_frequency <- function(TSAccelMag) {
  HeadingTable <- Get_Heading_Frequency_Table(TSAccelMag)
  # TODO: relative frequencies
  # TODO: adjust bin size
  
  p <- plot_ly(
    data = HeadingTable,
    type = 'bar',
    x = ~Angle,
    y = ~Frequency,
    hovertext = ~paste0("Angle: ", Angle, "&deg;", "<br>", "Freq:",Frequency),
    hoverinfo = 'text'
  ) %>% layout(
    showlegend = F,
    title = "Heading Angle Histogram",
    titlefont = TITLEFONT,
    font = MAINFONT,
    xaxis = list(title = "Angle (clockwise degrees from magnetic north)"),
    yaxis = list(title = "Frequency"),
    margin = list(t = 70, r = 60, l = 60, b = 60)
  ) %>% config(displayModeBar = 'hover')
  
  return(p)
}

#### STEP 6: WINDROSE (TILT AND/OR VELOCITY) ####

windrose_heading_tilt <- function(TSAccelMag) {
  p <- plot_ly(
    data = TSAccelMag,
    type = 'area',
    t = ~azimuthDegrees_adjusted,
    r = ~tiltAngle,
    opacity = 0.3,
    marker = list(size = 4)
  ) %>% layout(
    title = "Heading (angular axis) vs Tilt (radial axis)",
    font = TITLEFONT, 
    margin = list(t = 40, r = 20, l = 20),
    radialaxis = list(
      visible = T,
      tickfont = MAINFONT
    ),
    angularaxis = list(
      visible = T,
      type = 'linear',
      thetaunit = 'degrees',
      range = c(0,360),
      tickfont = MAINFONT
    ),
    orientation = 270,
    showlegend = F
  ) %>% config(
    displayModeBar = 'hover'
  )
  
  return(p)
}



cartesian_heading_tilt <- function(TSAccelMag) {
  # new data.frame with fewer columns
  CartesianAngles <- select(TSAccelMag, tiltAngle, azimuthDegrees_adjusted)
  
  # have to rotate the heading degrees from bearing coordinates
  CartesianAngles$rotatedazimuthDegrees_adjusted <- 90 - CartesianAngles$azimuthDegrees_adjusted
  
  # x = r cos(t)
  # y = r sin(t)
  # t = rotatedazimuthDegrees_adjusted, r = tiltAngle
  CartesianAngles$x_coord <- CartesianAngles$tiltAngle * cos(CartesianAngles$rotatedazimuthDegrees_adjusted * pi / 180)
  CartesianAngles$y_coord <- CartesianAngles$tiltAngle * sin(CartesianAngles$rotatedazimuthDegrees_adjusted * pi / 180)
  
  # make sure that axes are equal 
  axisMax = max(max(CartesianAngles$x_coord), max(CartesianAngles$y_coord))
  
  p <- plot_ly(
    data = CartesianAngles,
    x = ~x_coord,
    y = ~y_coord,
    text = ~paste0('Heading Angle: ',round(azimuthDegrees_adjusted,2), '&deg;<br>Tilt: ', round(tiltAngle,2), '&deg;'),
    hoverinfo = 'text',
    type = 'scatter',
    mode = 'markers',
    marker = list(
      size = 5, 
      color = 'rgba(0,0,0,0.1)',
      line = list(color = 'rgba(0,0,0,0.5)', width = 1)
    )
  )  %>% layout (
    title = "Heading (angular axis) vs Tilt (radial axis), \n Cartesian Coords for zoomability",
    titlefont = TITLEFONT,
    font = MAINFONT,
    margin = list(t = 70, r = 60, l = 60, b = 60),
    xaxis = list(autorange = FALSE, range = c(-axisMax, axisMax), title = ""),
    yaxis = list(autorange = FALSE, range = c(-axisMax, axisMax), title = "")
  ) %>% config(displayModeBar = 'hover')
  
  return(p)
}

#### STEP 7: TIME-SERIES VECTOR PLOTS ####
line_ts_tiltangle <- function(TSAccelMag) {
  p <- plot_ly(
    data = arrange(TSAccelMag, datetime),
    x = ~datetime,
    y = ~tiltAngle,
    type = 'scatter',
    mode = 'lines',
    text = ~paste0(datetime, "<br>", "Tilt: ", tiltAngle, "&deg;"),
    hoverinfo = 'text'
  ) %>% layout(
    title = 'Tilt Angle',
    titlefont = TITLEFONT,
    font = MAINFONT,
    xaxis = list(
      range = as.numeric(range(TSAccelMag$datetime))*1000,
      title = 'Date/time'
    ),
    yaxis = list(title = 'Tilt Angle (degrees from vertical)')
  ) %>% config(displayModeBar = 'hover')
  
  return(p)
}

scatter_ts_heading <- function(TSAccelMag) {
  p <- plot_ly(
    data = TSAccelMag,
    x = ~datetime,
    y = ~azimuthDegrees_adjusted,
    color = ~tiltAngle,
    type = 'scatter',
    mode = 'markers',
    marker = list(size = 4),
    text = ~paste0(datetime, "<br>", "Heading: ", azimuthDegrees_adjusted, "&deg;", '<br>', "Tilt: ", tiltAngle, "&deg;"),
    hoverinfo = 'text'
  ) %>% layout(
    title = 'Heading Angle',
    titlefont = TITLEFONT,
    font = MAINFONT,
    xaxis = list(
      range = as.numeric(range(TSAccelMag$datetime))*1000,
      title = 'Date/time'
    ),
    yaxis = list(title = 'Heading Angle (degrees from North)')
  ) %>% config(displayModeBar = 'hover')
  
  return(p)
}

#### STEP 8: STICKPLOT ####
heading_stickplot <- function(TSAccelMag, firstdate, lastdate) {
  CartesianAngles <- dplyr::select(TSAccelMag, datetime, tiltAngle, azimuthDegrees_adjusted) %>% 
    filter(floor_date(datetime, 'day') >= ymd(firstdate) & floor_date(datetime,'day') <= ymd(lastdate))
  
  # have to rotate the heading degrees from bearing coordinates
  CartesianAngles$rotatedazimuthDegrees_adjusted <- 90 - CartesianAngles$azimuthDegrees_adjusted
  
  # x = r cos(t)
  # y = r sin(t)
  # t = rotatedazimuthDegrees_adjusted, r = tiltAngle
  CartesianAngles$x_coord <- CartesianAngles$tiltAngle * cos(CartesianAngles$rotatedazimuthDegrees_adjusted * pi / 180)
  CartesianAngles$y_coord <- CartesianAngles$tiltAngle * sin(CartesianAngles$rotatedazimuthDegrees_adjusted * pi / 180)
  
  middley <- (range(CartesianAngles$tiltAngle)[1] + range(CartesianAngles$tiltAngle)[2])/2
  
  par(col = 'grey')
  oce.plot.ts(x = CartesianAngles$datetime, y = CartesianAngles$tiltAngle, type = 'l')
  plotSticks(
    x = CartesianAngles$datetime, 
    u = CartesianAngles$x_coord, 
    v = CartesianAngles$y_coord, 
    y = middley, 
    add = TRUE, 
    yscale = 3, col = 'black'
  )
}


#### COMPARE UNCALIBRATED ####
compare_angle_histogram <- function(TSAccelMag_Cal, TSAccelMag_Raw) {
  CalTable <- Get_Heading_Frequency_Table(TSAccelMag_Cal)
  RawTable <- Get_Heading_Frequency_Table(TSAccelMag_Raw)
  # TODO: relative frequencies
  # TODO: adjust bin size
  
  HeadingTable <- full_join(CalTable, RawTable, by = 'Angle')
  colnames(HeadingTable) <- c('Angle', 'CalFrequency','RawFrequency')
  HeadingTable[is.na(HeadingTable)] <- 0
  
  p <- plot_ly(
    data = HeadingTable,
    type = 'bar',
    x = ~Angle,
    y = ~CalFrequency,
    hovertext = ~paste0("Angle: ", Angle, "&deg;", "<br>", "Freq:",CalFrequency),
    hoverinfo = 'text',
    opacity = 0.5,
    marker = list(color = 'blue'),
    name = 'Calibrated'
  ) %>% add_bars(
    y = ~RawFrequency,
    hovertext = ~paste0("Angle: ", Angle, "&deg;", "<br>", "Freq:",RawFrequency),
    hoverinfo = 'text',
    marker = list(color = 'red'),
    name = 'Uncalibrated'
  ) %>% layout(
    showlegend = T,
    hovermode = 'x',
    title = "Heading Angle Histogram",
    titlefont = TITLEFONT,
    font = MAINFONT,
    xaxis = list(title = "Angle (clockwise degrees from magnetic north)"),
    yaxis = list(title = "Frequency"),
    margin = list(t = 70, r = 60, l = 60, b = 60)
  ) %>% config(displayModeBar = 'hover')
  
  return(p)
}



compare_tilt_line <- function(TSAccelMag_Cal, TSAccelMag_Raw) {
  
  ## ABSURDLY HARD TO LOAD
  # requires way too much computational power at this time
  # p <- plot_ly(
  #   data = arrange(TSAccelMag_Cal, datetime),
  #   x = ~datetime,
  #   y = ~tiltAngle,
  #   type = 'scatter',
  #   mode = 'lines',
  #   line = list(color = 'blue'),
  #   opacity = 0.5,
  #   text = ~paste0(datetime, "<br>", "Tilt: ", tiltAngle, "&deg;"),
  #   hoverinfo = 'text',
  #   name = 'Calibrated'
  # ) %>% add_lines(
  #   data = arrange(TSAccelMag_Raw, datetime),
  #   x = ~datetime,
  #   y = ~tiltAngle,
  #   type = 'scatter',
  #   mode = 'lines',
  #   line = list(color = 'red'),
  #   opacity = 0.5,
  #   text = ~paste0(datetime, "<br>", "Tilt: ", tiltAngle, "&deg;"),
  #   hoverinfo = 'text',
  #   name = 'Uncalibrated'
  # ) %>% layout(
  #   title = 'Tilt Angle',
  #   titlefont = TITLEFONT,
  #   font = MAINFONT,
  #   xaxis = list(
  #     range = as.numeric(range(TSAccelMag_Cal$datetime))*1000,
  #     title = 'Date/time'
  #   ),
  #   yaxis = list(title = 'Tilt Angle (degrees from vertical)')
  # ) %>% config(displayModeBar = 'hover')
  
}
