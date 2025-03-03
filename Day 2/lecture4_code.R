# Linear model ----
n <- 40
x <- runif(n, 0, 20)
y <- 0.2 + 0.3 * x + rnorm(n, mean = 0, sd = 0.5)

mod1 <- lm(y ~ x)

par_vec <- c()
nboot <- 1000

for( i in 1:nboot){

  rows_temp <- sample(1:length(y), length(y), replace = TRUE)
  mod <- lm(y ~ x, data = data.frame(y = y, x = x)[rows_temp, ])
  par_vec[i] <- mod$coefficients[1]
}


hist(par_vec)
summary(mod1)


vcov(mod)

# Summary
summary(mod)

# Plot
plot(x, y)
abline(mod)


# A mano ----
lm_fun <- function(pars, y, x, sigma_fijada){
  alpha <- pars[1]
  beta <- pars[2]
  sigma <- sigma_fijada # Exp to keep positive

  nll = -sum(dnorm(x = y - alpha + beta * x, mean = 0, sd = sigma, log = TRUE))
  return(nll)
}

fit <- optim(par = c(0,0,0), lm_fun, y = y, x = x, hessian = TRUE)
fit$par


# A mano mejor ----
lm_fun <- function(pars, y, x){
  beta <- pars[1:ncol(x)]
  sigma <- exp(pars[ncol(x) + 1]) # Exp to keep positive

  nll = -sum(dnorm(y, x %*% beta, sigma, log = TRUE))
  return(nll)
}

x_mat <- model.matrix(formula(~ 1 + x), data = data.frame(x = x))

pars <- c(1:ncol(x_mat), 0)
pars

fit <- optim(par = pars, lm_fun, y = y, x = x_mat, hessian = TRUE)
fit$par

# Error de parametros ----
H <- fit$hessian
vcov <- solve(fit$hessian)
se <- sqrt(diag(vcov))
se

summary(lm(y ~ x))

# Plot
plot(x, y)
curve(fit$par[1] + x * fit$par[1], from = 0, to = 20, add = TRUE)


# RTMB ----
library(RTMB)
data <- list(Y = rnorm(10) + 1:10, x=1:10)
parameters <- list(a=0, b=0, logSigma=0)

f <- function(parms) {
  Y <- data$Y
  x <- data$x
  a <- parms$a
  b <- parms$b
  logSigma <- parms$logSigma
  ADREPORT(exp(2*logSigma));
  nll = -sum(dnorm(Y, a+b*x, exp(logSigma), TRUE))
  nll
}
obj <- MakeADFun(f, parameters)

obj$hessian <- TRUE
opt <- optim(par = obj$par, fn = obj$fn) #do.call("optim", obj)
opt
opt$hessian ## <-- FD hessian from optim
obj$he()    ## <-- Analytical hessian
rep <- sdreport(obj)
summary(rep)


# Comparar dos grupos ----
group <- rep(c(0, 1), each = n/2)
y <- 0.2 + 2 * group + rnorm(n, mean = 0, sd = 0.5)

mod <- lm(y ~ as.factor(group))
summary(mod)


# Dos predictores ----
y <- 0.2 + 0.3 * x + 2 * group + rnorm(n, mean = 0, sd = 0.5)
mod <- lm(y ~ x + as.factor(group))
summary(mod)
plot(x, y, col = 1 + group, pch = 16)

# Plot fitted model
abline(a = mod$coefficients[1], b = mod$coefficients[2])
abline(a = mod$coefficients[1] + mod$coefficients[3], b = mod$coefficients[2], col = 2)


# Interaction ----
y <- 0.2 + 0.3 * x + x * group + rnorm(n, mean = 0, sd = 0.5)

mod <- lm(y ~ x * as.factor(group))
mod <- lm(y ~ x + as.factor(group) + x * as.factor(group))

plot(x, y, col = 1 + group, pch = 16)

# Plot fitted model
abline(a = mod$coefficients[1], b = mod$coefficients[2])
abline(a = mod$coefficients[1]+mod$coefficients[3], b = mod$coefficients[2] + +mod$coefficients[4], col = 2)


# Dos predictores continuous ----
x1 <- runif(n, 0, 20)
x2 <- runif(n, 5, 10)
y <- 0.2 + 0.3 * x1 + 2 * x2 + rnorm(n, mean = 0, sd = 0.5)

mod <- lm(y ~ x1 + x2)

# Mal plot
par(mfrow = c(2, 1), mar = c(4, 4, 0.5, 0.5))
plot(x1, y)
abline(a = mod$coefficients[1], b = mod$coefficients[2])

plot(x2, y)
abline(a = mod$coefficients[1], b = mod$coefficients[3])


# Bien plot
par(mfrow = c(2, 1), mar = c(4, 4, 0.5, 0.5))
plot(x1, y)
pred1 <- data.frame(x1 = x1, x2 = mean(x2))
pred1$y = predict(mod, pred1)
lines(pred1$x1, pred1$y)

plot(x2, y)
pred2 <- data.frame(x1 = mean(x1), x2 = x2)
pred2$y = predict(mod, pred2)
lines(pred2$x2, pred2$y)


# Modelo fallando ----
x1 <- runif(n, 0, 20)
rho = 1
x2 <- 2 + rho * x1 + (1-rho) * runif(n, 0, 20)
y <- 0.2 + 0.3 * x1 + 2 * x2 + rnorm(n, mean = 0, sd = 0.5)

mod <- lm(y ~ x1 + x2)


# Experimento de collinearity ----
rho_vec <- seq(0, 0.99, by = 0.01)
se_vec <- c()
for(i in 1:length(rho_vec)){
  set.seed(123)
  x2 <- 2 + rho_vec[i] * x1 + (1-rho_vec[i]) * runif(n, 0, 20)
  y <- 0.2 + 0.3 * x1 + 2 * x2 + rnorm(n, mean = 0, sd = 0.5)
  mod <- lm(y ~ x1 + x2)
  se_vec[i] <- summary(mod)$coefficients[2,2]
}

plot(x = rho_vec, y = (se_vec), type = "l")
