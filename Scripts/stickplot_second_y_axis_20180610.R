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

par(col = 'grey')

# usr = xmin, xmax, ymin, ymax
oce.plot.ts(
  x = CartesianAngles$datetime, 
  y = CartesianAngles$tiltAngle, 
  type = 'l', 
  ylim = c(-0.03*max_y, 1.67*max_y),
  xlab = 'Tilt Angle',
  ylab = 'Date',
  mar = c(3.5,3.5,3.5,3.5)
)
plotSticks(
  x = CartesianAngles$datetime, 
  u = CartesianAngles$x_coord, 
  v = CartesianAngles$y_coord, 
  y = max_y*1.33, 
  add = TRUE, 
  yscale = 3, col = 'black'
)
### TODO: get axis parameters from left axis, make an 1/3-scaled y axis but shifted to reflect centerline
axis(side=4, at=seq(0,100,by=10), col="yellow", col.axis="yellow")     # additional y-axis
mtext("line0", side = 4, line = 0, col = 'black')

