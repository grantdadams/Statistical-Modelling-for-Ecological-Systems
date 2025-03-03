# Generate data
size<-c(1.42,1.58,1.78,1.99,1.99,1.99,2.13,2.13,2.13, 2.32,2.32,2.32,2.32,2.32,2.43,2.43,2.78,2.98,2.98)
wear<-c(4.0,4.2,2.5,2.6,2.8,2.4,3.2,2.4,2.6,4.8,2.9, 3.8,3.0,2.7,3.1,3.3,3.0,2.8,1.7)
x <- size - min(size)
x <- x/max(x)

# Plot
plot(x, wear, xlab = "Scaled engine size", ylab = "Wear index")


# Fit polynomial basis model ----
mod <- lm(wear ~ x + I(x^2) + I(x^3))

# - Get beta
b1 = mod$coefficients[1]; b2 = mod$coefficients[2]; b3 = mod$coefficients[3]; b4 = mod$coefficients[4];

# - Plot
plot(x, wear, xlab = "Scaled engine size", ylab = "Wear index")
curve(b1 + x*b2 + x^2*b3 + x^3*b4, from = 0, to = 1, add = TRUE)


# Cubic regression spline ----
rk<-function(x,z){ # R(x,x*) for cubic spline on [0,1]
  ((z-0.5)^2-1/12)*((x-0.5)^2-1/12)/4- ((abs(x-z)-0.5)^4-(abs(x-z)-0.5)^2/2+7/240)/24
}

spl.X <-function(x, xk){      # setup model matrix forcubic penalized regression spline
  q <- length(xk)+2           # number of parameters
  n <- length(x)              # number of data
  X <- matrix(1, n, q)        # initialized model matrix
  X[, 2] <-x                  # set second column to x
  X[,3:q]<-outer(x,xk,FUN=rk) # and remaining to R(x,xk)
  X
}

xk <- 1:4/5               # choose location of 4 knots
Xmat <- spl.X(x, xk)         # generate model matrix

# - Fit
mod.1 <- lm(wear ~ Xmat - 1) # fit model
xp <- 0:100 / 100         # x values for prediction
Xp <- spl.X(xp, xk)       # prediction matrix

# - Plot
plot(x, wear, xlab = "Scaled engine size", ylab = "Wear index")
curve(b1 + x*b2 + x^2*b3 + x^3*b4, from = 0, to = 1, add = TRUE)
lines(xp, Xp %*% coef(mod.1), col = 2) # plot fitted spline


# Fit GAM cubic spline in 'mgcv' ----
library(mgcv)
mod.2 <- gam(wear ~ s(x, k = 4, fx = F)) # fit model with 4 knots
predy <- predict(mod.2, newdata = data.frame(x = xp), type = "response")

# - Plot
plot(x, wear, xlab = "Scaled engine size", ylab = "Wear index")
curve(b1 + x*b2 + x^2*b3 + x^3*b4, from = 0, to = 1, add = TRUE)
lines(xp, Xp %*% coef(mod.1), col = 2) # plot fitted spline
lines(xp, predy, col = 3)


# - Plot basis functions
library(gratia)
library(dplyr)
basis(mod.2) %>%
  draw()


# Fit to simulated survey data ----
get_density = function(depth,
                       best_depth = -1000,
                       DepthSigma = 1.5){
  exp(-1 * ( log(depth/best_depth) )^2 / DepthSigma ^2)
}
curve(get_density(x), from = -5000, to = 0, xlab = "Depth", ylab = "Habitat suitability")


# - Simulate data and fit
source("Day 5/Simulate_Spatial_Data_No_Plots.R")
library(mgcv)
gm.1 <- gam(CPUE ~ s(Depth, k = 10) + as.factor(Year), data = haul_data, family = poisson, select = TRUE)

# - Plot
plot(gm.1)

# Spatial distribution ----
gm.2 <- bam(CPUE ~ ti(X, Y, k = c(15, 15)), data = haul_data_random, family = poisson, select = TRUE)

# Predict distribution and plot
# -- Predict distribution
pred <- data.frame(X = polygony$X,
                   Y = polygony$Y)
pred$pred <- predict(gm.2, pred)

# -- Plot
polygony <- polygony %>%
  dplyr::mutate(Pred = pred$pred)
plot(polygony[c("depth", "habitat_suitability", "Pred")])


# Fit to observed data (mackerel egg survey) ----
library(gamair)
data("mack")

mack$log.net.area <- log(mack$net.area)

gm <- gam(egg.count ~ s(lon, lat) + offset(log.net.area), data = mack, family = nb, control = gam.control(maxit=100))

# Plot
# - Coastline
data("coast")

# - Predict distribution
lon = seq(-15, -1, by = 0.25) # X
lat = seq(44, 58, by = 0.25) # Y
pred <- expand.grid(lon = lon,
                    lat = lat)
pred$log.net.area <- 0
pred$pred <- predict(gm, pred)

# - Plot
par(mar = c(3,3.5,1,0.5), mgp = c(2,1,0))
image(lon, lat, matrix(pred$pred, 57, 57), cex.lab=1.5, cex.axis=1.4)
contour(lon, lat, matrix(pred$pred, 57, 57), add = TRUE)
lines(coast$lon, coast$lat, col=1)

