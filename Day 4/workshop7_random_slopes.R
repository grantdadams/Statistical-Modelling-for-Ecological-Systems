library(lme4)
library(glmmTMB)

## Random slopes
# * Simulate data for random slopes ----
# Simulate predictors
group_i = rep( 1:10, 1:10)
x_i <- rnorm( length(group_i), mean=0, sd=1)
beta0 = 0
beta1 = 1
beta1_g = rnorm(length(unique(group_i)), mean=0, sd=0.5)

# Simulate response
y_i =  beta0 + (beta1 + beta1_g[group_i]) * x_i + rnorm( length(group_i), mean=0, sd=1)



# Plot
plot(x = x_i, y = y_i, col = group_i, pch = 16)
for(i in 1:10){
  abline(a = 0, b = beta1 + beta1_g[i], col = i)
}



# * Fit model in R ----
lme.1 = lmer( y_i ~ 1 + x_i + (x_i - 1|factor(group_i)), REML = FALSE) # lme4
summ.1 <- summary(lme.1)
ranef(lme.1)

lme.2 = glmmTMB( y_i ~ 1 + x_i + (x_i - 1|factor(group_i)))            # glmmTMB
summ.2 <- summary(lme.2)


# * Fit model in RTMB ----
library(RTMB)

# Write model
lmm <- function(parameters){
  # - Data
  y_i <- data$y_i
  x_i <- data$x_i
  group_i <- data$group_i

  # - Parameters
  beta0 <- parameters$beta0
  beta1 <- parameters$beta1
  log_SD0 <- parameters$log_SD0
  log_SDZ <- parameters$log_SDZ
  beta1_g <- parameters$beta1_g

  # - Parameter transform
  SD0 = exp(log_SD0)
  SDZ = exp(log_SDZ)

  # - Likelihood
  # Random effects
  nll = -sum(dnorm(beta1_g, 0, SDZ, log = TRUE))

  # Data
  nll = nll -sum(dnorm(y_i, beta0 + (beta1 + beta1_g[group_i]) * x_i , SD0, log = TRUE))

  # - Report
  RTMB::REPORT(SD0)
  RTMB::REPORT(SDZ)

  return(nll)
}

# Build inputs
data = list(  group_i = group_i, y_i = y_i, x_i = x_i, n_groups = length(unique(group_i)))
parameters = list( "beta0"=-10, "beta1"=1, "log_SD0"=2, "log_SDZ"=2, "beta1_g"=rep(0, data$n_groups))
random = c("beta1_g")

# Build object
obj <- RTMB::MakeADFun(lmm, parameters, random = random)

# Optimize
obj$hessian <- TRUE
opt <- do.call("optim", obj)
opt

# Report output
rep <- sdreport(obj)


## Compare estimates ----
# - Global intercept and slope
cbind( fixef(lme.1), summ.2$coefficients$cond[1:2], opt$par[1:2])

# - Random effects
cbind( "True"=beta1_g, "lme4"=ranef(lme.1)[['factor(group_i)']], "glmmTMB"= ranef(lme.2)[[1]][[1]], "RTMB" = obj$env$parList()$beta1_g )


