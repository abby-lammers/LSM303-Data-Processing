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
## TODO: investigate bioinformatics heatmap method (see trello.cave_pearl_processing)