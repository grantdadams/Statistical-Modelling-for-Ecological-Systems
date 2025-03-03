# https://github.com/kaskr/RTMB/blob/master/tmb_examples/linreg.R
library(RTMB)

# Simulate data
set.seed(123)
n <- 60
x <- rnorm(n, 0 , 1)
p <- 1/(1+exp(-(1+5*x))) # Logistic function
y = rbinom(n, 1, p)
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
  y_pred = 1/(1+exp(-(a+b*x)))

  # Likelihood
  nll = -sum(dbinom(y, 1, y_pred, log = TRUE))

  # Report
  RTMB::REPORT(link_pred)
  RTMB::REPORT(y_pred)

  # Return nll
  nll
}

# Make object
data <- list(x = x, y = y)
parameters <- list(a=0, b=0) # Initial parameters
obj <- MakeADFun(f, parameters, data = data)

# Fit model
opt <- do.call("optim", obj)
opt

rep <- sdreport(obj)
summary(rep)


# GLM
mod <- glm(y ~ x, family = binomial(link = "logit"))
summary(mod)

pred <- data.frame(x = c(-2, -1, 0, 1, 2))
predict(mod, newdata = pred, type = "response")

# Plot
alpha <- rep$par.fixed[1]
beta <- rep$par.fixed[2]

curve(1/(1+exp(-(alpha+beta*x))), from = -3, to = 3, add = TRUE)
