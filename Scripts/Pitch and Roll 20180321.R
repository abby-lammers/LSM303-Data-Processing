# Let's see what PITCH and ROLL look like for a given TILT. 

require(plotly)


GetAzimuth <- function(Xvec, Yvec, Quadrant) {
  ReturnVec <- atan(Yvec/Xvec) * 180 / pi
  ReturnVec[Quadrant == 2 | Quadrant == 3] <- 180 + ReturnVec[Quadrant == 2 | Quadrant == 3]
  ReturnVec[Quadrant == 4] <- 360 + ReturnVec[Quadrant == 4]
  
  return(ReturnVec)
}

AssignQuadrant <- function(Xvec, Yvec) {
  ReturnVec <- rep(0, length(Xvec))
  ReturnVec[Xvec >= 0 & Yvec >= 0] <- 1
  ReturnVec[Xvec < 0 & Yvec >= 0] <- 2
  ReturnVec[Xvec < 0 & Yvec < 0] <- 3
  ReturnVec[Xvec >= 0 & Yvec < 0] <- 4
  # if any of the observations are exactly equal to zero, a zero is returned
  return(ReturnVec)
}

#### 1. Get tau, the constant "radius" of the circle formed by Ax1 and Ay1 ####

tilt <- 5 # degrees
tiltRadians <- tilt * pi / 180

# tau <- tan(tiltRadians) ^ 2 / ( tan(tiltRadians) ^ 2 + 1 ) ## good news this equals sin^2
tau <- sin(tiltRadians)^2

#### 2. Get Ax1 and Ay1, the normalized acceleration components ####

# sin^2(tiltRadians) = Ax1^2 + Ay1^2    (circle equation)

# at Ay1 = 0, Ax1 = sqrt(tau) = min/max X
# get a vector of X from min to max, then get y from circle equation

x_max <- sqrt(tau)
x_vec <- c(-50:50, 49:-49) / 50 * x_max

# get y_vec from circle equation
y_vec <- c(sqrt(tau - x_vec[1:101]^2), - sqrt(tau - x_vec[102:200]^2))

#### 3. Get Az1 ####
Az1 <- 1 - sqrt(x_vec[1]^2 + y_vec[1]^2)

#### 4. Get roll and pitch angles ####
pitch_vec <- asin(-x_vec) * 180 / pi
roll_vec <- asin(y_vec) * 180 / pi # this is DIFFERENT FROM LSM303! I didn't derive it, but it makes sense?

#### 5. Get Azimuth ####
quad_vec <- AssignQuadrant(x_vec, y_vec)

azimuth_vec <- GetAzimuth(x_vec, y_vec, quad_vec) * pi / 180

#### 6. Plot roll, pitch, and xy on one plane ####
# expect to see circles

p <- plot_ly(
  x = x_vec,
  y = y_vec,
  z = roll_vec,
  type = 'scatter3d',
  mode = 'markers',
  marker = list(color = 'red'),
  name = 'roll'
) %>% add_markers(
  x = x_vec,
  y = y_vec,
  z = pitch_vec,
  marker = list(color = 'purple'),
  name = 'pitch'
) %>% add_markers(
  x = x_vec,
  y = y_vec,
  z = 0,
  marker = list(color = 'black'),
  name = 'zero'
) %>% add_markers(
  x = x_vec, 
  y = y_vec,
  z = azimuth_vec,
  marker = list(color = 'blue'),
  name = 'azimuth'
) %>% layout(
  title = 'Pitch, Roll, and Azimuth at Constant Tilt'
)


# 
# plot_ly(
#   x = x_vec,
#   y = roll_vec,
#   type = 'scatter',
#   mode = 'markers',
#   marker = list(color = 'red'),
#   name = 'roll'
# ) %>% add_markers(
#   y = pitch_vec,
#   marker = list(color = 'purple'),
#   name = 'pitch'
# ) %>% add_markers(
#   y = y_vec,
#   marker = list(color = 'yellow'),
#   name = 'y'
# ) %>% add_markers(
#   y = x_vec,
#   marker = list(color = 'green'),
#   name = 'x'
# )
# 
