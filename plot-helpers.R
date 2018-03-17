require(plotly)
require(lubridate)


#### STEP 5: WINDROSE (FREQUENCY) ####

# 5.1 Make frequency table for tilt angle histograms #
tempFreqTable <- table(round(TSAccelMag$headingDegrees,1))
AngleFrequencyTable <- data.frame(
  Angle = as.numeric(names(tempFreqTable)),
  Frequency = as.vector(tempFreqTable)
)

# 5.2 Windrose A: heading angle vs frequency (binned by 0.1) (bar) (0-360 degrees)
plot_ly(
  data = AngleFrequencyTable,
  type = 'area',
  t = ~Angle,
  r = ~Frequency,
  opacity = 0.2
) %>% layout(
  radialaxis = list(
    visible = T
  ),
  angularaxis = list(
    visible = T,
    type = 'linear',
    thetaunit = 'degrees',
    range = c(0,360)
  ),
  orientation = 270,
  showlegend = F
) %>% config(
  displayModeBar = TRUE
)


# 5.2 Windrose B: heading angle vs frequency (binned by 0.1) (bar) (autoranged degrees)
plot_ly(
  data = AngleFrequencyTable,
  type = 'area',
  t = ~Angle,
  r = ~Frequency,
  opacity = 0.5
) %>% layout(
  radialaxis = list(
    visible = T
  ),
  angularaxis = list(
    visible = T,
    type = 'linear',
    thetaunit = 'degrees',
    range = range(AngleFrequencyTable$Angle)
  ),
  orientation = 270,
  showlegend = F
) %>% config(
  displayModeBar = TRUE
)


# 5.3 Frequency vs Angle Bar Plot 
plot_ly(
  data = AngleFrequencyTable,
  type = 'bar',
  x = ~Angle,
  y = ~Frequency
) %>% layout(
  showlegend = F,
  title = "Oddessy Heading Angle Histogram",
  xaxis = list(title = "Angle (clockwise degrees from magnetic north)"),
  yaxis = list(title = "Frequency")
) %>% config(
  displayModeBar = FALSE
)


# 5.4 Raw Acceleration Coords Scatter Plot (xa_norm vs ya_norm) 
# plot_ly(
#   data = TSAccelMag,
#   x = ~xa_norm,
#   y = ~ya_norm,
#   type = 'scatter',
#   mode = 'markers',
#   hoverinfo = 'none'
# ) %>% layout(
#   xaxis = list(range = c(-0.5,0.5)),
#   yaxis = list(range = c(-0.5,0.5))
# )

#### STEP 6: WINDROSE (TILT AND/OR VELOCITY) ####

# 6.1 Heading vs Tilt (polar scatter)
plot_ly(
  data = TSAccelMag,
  type = 'scatter',
  mode = 'markers',
  t = ~headingDegrees,
  r = ~tiltAngle,
  opacity = 0.3,
  marker = list(size = 4)
) %>% layout(
  radialaxis = list(
    visible = T
  ),
  angularaxis = list(
    visible = T,
    type = 'linear',
    thetaunit = 'degrees',
    range = c(0,360)
  ),
  orientation = 270,
  showlegend = F,
  title = "Heading (angular axis) vs Tilt (radial axis)"
) %>% config(
  displayModeBar = TRUE
)

# 6.2 Heading vs Tilt (Cartesian scatter in first quadrant to allow zoom)

# new data.frame with fewer columns
CartesianAngles <- select(TSAccelMag, tiltAngle, headingDegrees)

# have to rotate the heading degrees from bearing coordinates
CartesianAngles$rotatedHeadingDegrees <- 90 - CartesianAngles$headingDegrees


# x = r cos(t)
# y = r sin(t)
# t = rotatedHeadingDegrees, r = tiltAngle
CartesianAngles$x_coord <- CartesianAngles$tiltAngle * cos(CartesianAngles$rotatedHeadingDegrees * pi / 180)
CartesianAngles$y_coord <- CartesianAngles$tiltAngle * sin(CartesianAngles$rotatedHeadingDegrees * pi / 180)

# make sure that axes are equal 
axisMax = max(max(CartesianAngles$x_coord), max(CartesianAngles$y_coord))

plot_ly(
  data = CartesianAngles,
  x = ~x_coord,
  y = ~y_coord,
  text = ~paste0('Heading Angle: ',round(headingDegrees,2), ' deg<br>Tilt: ', round(tiltAngle,2), ' deg'),
  hoverinfo = 'text',
  type = 'scattergl',
  mode = 'markers',
  marker = list(
    size = 5, 
    color = 'rgba(0,0,0,0.1)',
    line = list(color = 'rgba(0,0,0,0.5)', width = 1)
  )
)  %>% layout (
  title = "Heading (angular axis) vs Tilt (radial axis), \n Cartesian Coords for zoomability",
  xaxis = list(autorange = FALSE, range = c(-axisMax, axisMax)),
  yaxis = list(autorange = FALSE, range = c(-axisMax, axisMax))
)%>% config(displayModeBar = TRUE)

#### STEP 7: TIME-SERIES VECTOR PLOTS ####

# 7.1 Angle vs Time (scattergl, color by tilt ~velocity)
plot_ly(
  data = TSAccelMag,
  x = ~datetime,
  y = ~tiltAngle,
  color = ~headingDegrees,
  type = 'scattergl',
  mode = 'markers',
  marker = list(size = 5)
) %>% layout(
  title = 'bearing vs time',
  xaxis = list(range = as.numeric(range(TSAccelMag$datetime))*1000)
) %>% config(displayModeBar = TRUE)
