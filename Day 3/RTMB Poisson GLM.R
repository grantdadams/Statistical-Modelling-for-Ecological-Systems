# https://github.com/kaskr/RTMB/blob/master/tmb_examples/linreg.R
library(RTMB)

# Simulate data
set.seed(123)
n <- 40
x <- rnorm(n, 0 , 1)
x2 <- rnorm(n, 0 , 1)
y = rpois(n, exp(2))
plot(x, y)


# Write model
f <- function(parms) {
  # Datos
  Y <- data$y
  x <- data$x

  # Parametros
  a <- parms$a
  b <- parms$b

  # Model
  link_pred = a+b*x
  y_pred = exp(a+b*x)

  # Likelihood
  nll = -sum(dpois(Y, y_pred, log = TRUE))
  nll = nll + 0.2 * b^2

  # Report
  RTMB::REPORT(link_pred)
  RTMB::REPORT(y_pred)
  RTMB::ADREPORT(y_pred)

  # Return nll
  nll
}

# Make object
data <- list(x = x, y = y)
parameters <- list(a=0, b=0) # Initial parameters
obj <- MakeADFun(f, parameters, data = data, silent = TRUE)

# Fit model
opt <- do.call("optim", obj)
opt <- nlminb(start = obj$par, objective = obj$fn, gradient = obj$gr)
opt$par

rep <- sdreport(obj)
summary(rep)

# GLM
mod.1 <- glm(y ~ x, family = poisson(link = "log"))
mod.2 <- gam(y ~ x2, family = poisson(link = "log"))
summary(mod.1)
summary(mod.2)


# Plot
alpha <- rep$par.fixed[1]
beta <- rep$par.fixed[2]

mod.2 <- lm(y ~ x)

curve(exp(alpha+beta*x), from = -3, to = 3, add = TRUE)
# abline(mod.2, col = 2)
curve(exp(2.039644+1.006822*x), from = -3, to = 3, add = TRUE, col = 4)



