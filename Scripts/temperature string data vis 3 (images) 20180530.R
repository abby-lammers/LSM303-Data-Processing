## Temperature String Heatmap
## visualize using image() command by manually mapping data to colors.

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

Palette <- data.frame(temperature = sort(unique(OneMonthTempData$Temperature)))
Palette$Hex <- colorRampPalette(rev(brewer.pal(11,"Spectral")))(nrow(Palette))

OneMonthTempData


tempRange <- range(OneMonthTempData$Temperature)
ncolors <- (ceiling(tempRange[2]) - floor(tempRange[1])) * 4
palette <- colorRampPalette(rev(brewer.pal(11,"Spectral")))(ncolors + 1)
colorIndex <- (0:ncolors)/ncolors

colorscale <- data.frame(index = colorIndex, palette = palette)

image(as.matrix(select(RawTemperatureData, -datetime)), col = Palette$Hex)

newData <- RawTemperatureData[,5:24]
image(as.matrix(newData), col = Palette$Hex)
