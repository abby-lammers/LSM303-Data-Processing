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
source('App/LSM303-file-helpers.R')
#####

TSAccelMag <- Read_LSM303_csv(
  fileName = 'App/Data/AKsouth_012_20160403-0714_cleaned.csv',
  crop = 3000
) %>% 
  Normalize_Accel(cal = TRUE) %>% 
  Get_Accel_Angles() %>% 
  Normalize_Mag(cal = TRUE) %>% 
  Compensate_Mag_Field() %>% 
  Get_Heading() %>% 
  arrange(datetime) %>% 
  Round_TSAccelMag()


## Add axes to stickplot
firstdate <- '2016-04-01'
lastdate <- '2016-04-10'

CartesianAngles <- dplyr::select(TSAccelMag, datetime, tiltAngle, azimuthDegrees_adjusted) %>% 
  filter(floor_date(datetime, 'day') >= ymd(firstdate) & floor_date(datetime,'day') <= ymd(lastdate))

# have to rotate the heading degrees from bearing coordinates
CartesianAngles$rotatedazimuthDegrees_adjusted <- 90 - CartesianAngles$azimuthDegrees_adjusted

# x = r cos(t)
# y = r sin(t)
# t = rotatedazimuthDegrees_adjusted, r = tiltAngle
CartesianAngles$x_coord <- CartesianAngles$tiltAngle * cos(CartesianAngles$rotatedazimuthDegrees_adjusted * pi / 180)
CartesianAngles$y_coord <- CartesianAngles$tiltAngle * sin(CartesianAngles$rotatedazimuthDegrees_adjusted * pi / 180)

#middley <- (range(CartesianAngles$tiltAngle)[1] + range(CartesianAngles$tiltAngle)[2])/2
max_y <- max(CartesianAngles$tiltAngle)
# zero point
zero_y <- max_y*1.33
y_scale = 2

par(col = 'grey')

# usr = xmin, xmax, ymin, ymax
oce.plot.ts(
  x = CartesianAngles$datetime, 
  y = CartesianAngles$tiltAngle, 
  type = 'l', 
  ylim = c(-0.03*max_y, 1.67*max_y),
  xlab = 'Date',
  ylab = 'Tilt Angle Magnitude (Gray Time Series)',
  mar = c(3.5,3.5,3.5,3.5)
)
plotSticks(
  x = CartesianAngles$datetime, 
  u = CartesianAngles$x_coord, 
  v = CartesianAngles$y_coord, 
  y = zero_y, 
  add = TRUE, 
  yscale = y_scale, col = 'black'
)
### get axis parameters from left axis, make an 1/3-scaled y axis but shifted to reflect centerline
# parameters from left axis:
yAxisRange <- (par('yaxp')[1:2] - zero_y) * y_scale
nticks <- par('yaxp')[3]
tickInterval <- (par('yaxp')[2] / (nticks))

# label_vec gives labels corresponding to at_vec
label_vec <- seq(floor(yAxisRange[1]/tickInterval) * tickInterval, floor(yAxisRange[2]/tickInterval) * tickInterval, tickInterval*y_scale)
# at_vec gives positions of labels
at_vec <- label_vec / y_scale + zero_y

axis(side = 4, col = 'white', line = -1.5, labels = FALSE)
axis(side=4, col="black", col.axis="black", at = at_vec, labels = abs(label_vec), line = -1.5)     # additional y-axis
mtext("Tilt Angle Magnitude (Black Vector Plot)", side = 4, line = 0.5, col = 'black')

