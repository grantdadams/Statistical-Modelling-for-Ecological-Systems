# Load data
library(gamair)
data(mack)


head(mack)
hist
hist(mack$egg.count)

# Poisson distribution
pois_fun <- function(theta, y){
  llpois <- log(theta^y*exp(-theta)/factorial(y)) # mismo que "dpois"
  return(-sum(llpois))
}

# Profile estimation
thetas <- seq(from = 1, to = 60, by = 0.001)
ll_thetas <- sapply(thetas, function(x) pois_fun(theta = x, y = mack$egg.count))

plot(y= ll_thetas, x = thetas, type = "l")
thetas[which.min(ll_thetas)]

# Maximum likelihood estimation
optim(par = 5,
      fn = pois_fun,
      y = mack$egg.count,
      method = "Brent", lower = 0, upper = 100)


# Analytical solution
# theta^y*exp(-theta)/!y
# log(theta^y*exp(-theta)/!y)
# log(theta)*y-theta)-log(!y)
# dTheta/dy = 1/theta*y-1
sum(mack$egg.count)/length(mack$egg.count)
