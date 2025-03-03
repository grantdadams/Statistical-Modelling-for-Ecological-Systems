# Load data
library(gamair)
data(mack)


# Data exploration
head(mack)
hist(mack$egg.count)


# Poisson negative log likelihood
pois_nll <- function(theta, y){
  # theta^y*exp(-theta)/factorial(y) # mismo que "dpois"
  nll_pois <- -log(theta)*y + theta + log(factorial(y)) # negative log-likelihood
  return(sum(nll_pois))
}

# -- First derivative
pois_d1 <- function(theta, y){
  # -log(theta)*y + theta + log(factorial(y)) - negative loglike
  nll_pois_d1 <-   1 - y/theta
  return(sum(nll_pois_d1))
}

# - Second derivative
pois_d2 <- function(theta, y){
  # -log(theta)*y + theta + log(factorial(y)) - negative loglike
  nll_pois_d2 <-   y/theta^2
  return(sum(nll_pois_d2))
}




# Profile estimation
thetas <- seq(from = 10, to = 20, by = 0.001)
nll_thetas <- sapply(thetas, function(x) pois_nll(theta = x, y = mack$egg.count))
minima <- thetas[which.min(nll_thetas)]
plot(y= nll_thetas, x = thetas, type = "l"); abline(v = minima, col = 2)



# - Echemos un vistazo a la primera y segunda derivada
d1_thetas <- sapply(thetas, function(x) pois_d1(theta = x, y = mack$egg.count))
d2_thetas <- sapply(thetas, function(x) pois_d2(theta = x, y = mack$egg.count))

par(mfrow = c(1,3))
plot(y= nll_thetas, x = thetas, type = "l"); abline(v = minima, col = 2)
plot(y= abs(d1_thetas), x = thetas, type = "l"); abline(v = minima, col = 2)
plot(y= d2_thetas, x = thetas, type = "l"); abline(v = minima, col = 2)
plot(y= d1_thetas/d2_thetas, x = thetas, type = "l"); abline(v = minima, col = 2)


# Maximum likelihood estimation 1
opt <- optim(par = 5,
             fn = pois_nll,
             y = mack$egg.count,
             method = "Brent", lower = 0, upper = 100)
opt$par


# Maximum likelihood estimation 2
mod <- glm(egg.count~1, mack, family = "poisson") # Assumes log-link
theta_hat <- exp(mod$coefficients)[1]
res <- abs(resid(mod, type = "deviance")) # Standardized residual
std_res <- res/theta_hat
plot(sort(res), x = exp(qnorm(ppoints(length(res)))))
abline(1, 1, col = "blue")

# Analytical solution
# - theta^y*exp(-theta)/!y
# - \sum log(theta^y*exp(-theta)/!y)
# - \sum log(theta)*y-theta)-log(!y)
# - \sum dTheta/dy = 1/theta*y-n
# - 0 = 1/theta*\sum y-n
# - \sum y/n = theta
sum(mack$egg.count)/length(mack$egg.count)


# Extra Credit: maximum likelihood estimation
# -  Run Nelder-Mead
step = 1000
theta_vec = 2
ind = 1
threshold = 0.000001

while(abs(step) > threshold){
  step = pois_d1(theta_vec[ind], mack$egg.count)/pois_d2(theta_vec[ind], mack$egg.count)
  theta_vec[ind+1] = theta_vec[ind] - step
  print(theta_vec[ind+1])
  ind = ind+1
}
theta_vec

# Profile estimation
thetas <- seq(from = 2, to = 20, by = 0.001)
nll_thetas <- sapply(thetas, function(x) pois_nll(theta = x, y = mack$egg.count))
minima <- thetas[which.min(nll_thetas)]
plot(y= nll_thetas, x = thetas, type = "l"); abline(v = minima, col = 2)
points(x = theta_vec, y = sapply(theta_vec, function(x) pois_nll(theta = x, y = mack$egg.count)), pch = 16, col = 2)


