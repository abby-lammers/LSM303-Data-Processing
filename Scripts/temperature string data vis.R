## CONTOUR PLOT OF ABACO TEMPERATURE STRING DATA

require(plotly) # plotting
require(lubridate) # dates
require(zoo) # misc data handling functions
require(tidyr) # for reshaping data sets
require(RColorBrewer)

# read data, a few minor processing bits
RawTemperatureData <- read.csv('abaco_temp_data.csv')
RawTemperatureData <- na.trim(RawTemperatureData)
RawTemperatureData$datetime <- lubridate::mdy_hm(RawTemperatureData$datetime)

# reshape data into three columns (x,y,z)
ReshapedTempData <- gather(RawTemperatureData, key = Depth, value = Temperature, -datetime)

# convert temperature column names into numbers
ReshapedTempData$Depth <- as.numeric(substr(ReshapedTempData$Depth, 2, 6))
TestData <- ReshapedTempData %>% filter(datetime < ymd('2016-08-01'))

## ggplot with gradient color scale
gg <- ggplot(TestData, aes(datetime, Depth)) +
  geom_raster(aes(fill = Temperature)) +
  scale_y_reverse() +
  scale_fill_gradientn(colours = colorRamps::matlab.like(10))
ggplotly(gg)

# improved color gradient, removed background
gg <- ggplot(TestData, aes(datetime, Depth)) +
  geom_raster(aes(fill = Temperature)) +
  scale_y_reverse() +
  scale_fill_gradientn(colours = rev(brewer.pal(11, 'Spectral'))) +
  theme_bw() +
  theme(panel.grid.minor=element_blank(),
    panel.grid.major=element_blank())
p <- ggplotly(gg)


## generate color palette and scale dynamically
## from https://stackoverflow.com/questions/16922988/interpolating-a-sequential-brewer-palette-as-a-legend-for-ggplot2
tempRange <- range(TestData$Temperature)
ncolors <- (ceiling(tempRange[2]) - floor(tempRange[1])) * 4
palette <- colorRampPalette(rev(brewer.pal(11,"Spectral")))(ncolors + 1)
colorIndex <- (0:ncolors)/ncolors

colorscale <- data.frame(index = colorIndex, palette = palette)

#The colorscale must be an array containing arrays mapping a normalized value to an 
# rgb, rgba, hex, hsl, hsv, or named color string. At minimum, a mapping for the 
# lowest (0) and highest (1) values are required. For example, 
# `[[0, 'rgb(0,0,255)', [1, 'rgb(255,0,0)']]`. 
# To control the bounds of the colorscale in z space, use zmin and zmax



## contour using just plotly
p <- plot_ly(
  type = 'contour',
  data = TestData,
  x = ~datetime,
  y = ~Depth,
  z = ~Temperature,
  contours = list(
    coloring = 'fill',
    showlines = FALSE
  ),
  colorscale = colorscale,
  colorbar = list(title = "Temperature (°C)")
) %>% layout(
  yaxis = list(
    autorange = "reversed",
    title = 'Depth (m)'
  ),
  xaxis = list(
    title = "Date/Time"
  ),
  title = "Abaco Temperature Profile"
) 

#### add rangeslider
firstDate <- min(TestData$datetime)
firstWeek <- firstDate + lubridate::weeks(1)

p <- plot_ly(
  type = 'contour',
  data = TestData,
  x = ~datetime,
  y = ~Depth,
  z = ~Temperature,
  contours = list(
    coloring = 'fill',
    showlines = FALSE
  ),
  colorscale = colorscale,
  colorbar = list(title = "Temperature (°C)")
) %>% layout(
  yaxis = list(
    autorange = "reversed",
    title = 'Depth (m)'
  ),
  xaxis = list(
    title = "Date/Time",
    range = c(firstDate,firstWeek),
    rangeslider = list(
      type = "date",
      range = range(TestData$datetime)
    )
  ),
  title = "Abaco Temperature Profile"
) 


#### attempt with full data; crash computer
tempRange <- range(ReshapedTempData$Temperature)
ncolors <- (ceiling(tempRange[2]) - floor(tempRange[1])) * 4
palette <- colorRampPalette(rev(brewer.pal(11,"Spectral")))(ncolors + 1)
colorIndex <- (0:ncolors)/ncolors

colorscale <- data.frame(index = colorIndex, palette = palette)

firstDate <- min(ReshapedTempData$datetime)
firstWeek <- firstDate + lubridate::weeks(1)

p <- plot_ly(
  type = 'contour',
  data = ReshapedTempData,
  x = ~datetime,
  y = ~Depth,
  z = ~Temperature,
  hoverinfo = 'none',
  contours = list(
    coloring = 'fill',
    showlines = FALSE
  ),
  colorscale = colorscale,
  colorbar = list(title = "Temperature (°C)")
) %>% layout(
  yaxis = list(
    autorange = "reversed",
    title = 'Depth (m)'
  ),
  xaxis = list(
    title = "Date/Time",
    range = c(firstDate,firstWeek),
    rangeslider = list(
      type = "date",
      range = range(ReshapedTempData$datetime)
    )
  ),
  title = "Abaco Temperature Profile"
) 

#### just a month of data
OneMonthTempData <- ReshapedTempData %>% filter(datetime < ymd('2016-08-08'))

tempRange <- range(OneMonthTempData$Temperature)
ncolors <- (ceiling(tempRange[2]) - floor(tempRange[1])) * 4
palette <- colorRampPalette(rev(brewer.pal(11,"Spectral")))(ncolors + 1)
colorIndex <- (0:ncolors)/ncolors

colorscale <- data.frame(index = colorIndex, palette = palette)

firstDate <- min(OneMonthTempData$datetime)
firstWeek <- firstDate + lubridate::weeks(1)

p <- plot_ly(
  type = 'contour',
  data = OneMonthTempData,
  x = ~datetime,
  y = ~Depth,
  z = ~Temperature,
  hoverinfo = 'none',
  contours = list(
    coloring = 'fill',
    showlines = FALSE
  ),
  colorscale = colorscale,
  colorbar = list(title = "Temperature (°C)")
) %>% layout(
  yaxis = list(
    autorange = "reversed",
    title = 'Depth (m)'
  ),
  xaxis = list(
    title = "Date/Time",
    range = c(firstDate,firstWeek),
    rangeslider = list(
      type = "date",
      range = range(OneMonthTempData$datetime)
    )
  ),
  title = "Abaco Temperature Profile"
) 


## TODO: Convert from contour to square scatter?