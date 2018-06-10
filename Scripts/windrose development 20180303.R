library(plotly) # viz
library(tidyr)  # data munge
library(dplyr)  # data munge

# list of countries
Countries <- c("SG", "UK", "CAD", "USA", "AU")

# data 
set.seed(1)
data.frame(replicate(5, sample(0:10, 5, rep=TRUE))) %>%
  cbind(Countries) %>%
  gather(key = Variable
    , value = value
    , -Countries) ->
  df

# single country
SG <- df[df$Countries %in% c("SG"),]

# plot
plot_ly(
  type = 'area',
  t = c(0,45,90,135,180,225,270,360),
  r = c(1,2,3,4,5,6,7,8)
  ) %>% layout(
    radialaxis = list(
      visible = T
    ),
 
  showlegend = F
)
