library(lme4)
library(glmmTMB)

## Random intercepts
# * Simulate data for random intercept ----
# Simulate predictors
group_i = rep( 1:10, 1:10)
set.seed(123)
z_g = rnorm( length(unique(group_i)), mean=0, sd=1)
beta0 = 1


# Simulate response
y_i = z_g[group_i] + beta0 + rnorm( length(group_i), mean=0, sd=1)


# Plot
tbl <- table(group_i)
barplot(tbl, beside = TRUE, legend = TRUE)

plot(x = as.factor(group_i), y = y_i)



# * Fit model in R ----
lme.1 = lmer( Abundance ~ 1 + (1|Speces) + Habitat + (1|Year) + (1|Community), REML = FALSE) # lme4
summ.1 <- summary(lme.1)
tmp <- ranef(lme.1)
tmp$`factor(group_i)` + summ.1$coefficients[1]
summ.1

lme.2 = glmmTMB( y_i ~ 1|factor(group_i))            # glmmTMB
summ.2 <- summary(lme.2)
summ.2

# * Fit model in RTMB ----
library(RTMB)

# Write model
lmm <- function(parameters){
  # - Data
  y_i <- data$y_i
  group_i <- data$group_i

  # - Parameters
  beta0 <- parameters$beta0
  log_SD0 <- parameters$log_SD0
  log_SDZ <- parameters$log_SDZ
  z_g <- parameters$z_g

  # - Parameter transform
  SD0 = exp(log_SD0)
  SDZ = exp(log_SDZ)

  # - Likelihood
  # Random effects
  nll = -sum(dnorm(z_g, 0, exp(log_SDZ), log = TRUE))

  # Data
  nll = nll -sum(dnorm(y_i, beta0 + z_g[group_i], exp(log_SD0), log = TRUE))

  # - Report
  RTMB::REPORT(SD0)
  RTMB::REPORT(SDZ)

  return(nll)
}

# Build inputs
data = list(  group_i = group_i, y_i = y_i, n_groups = length(unique(group_i)))
parameters = list( "beta0"=-10, "log_SD0"=2, "log_SDZ"=2, "z_g"=rep(0, data$n_groups))
random = c("z_g")

# Build object
obj <- RTMB::MakeADFun(lmm, parameters, random = random)

# Optimize
obj$hessian <- TRUE
opt <- do.call("optim", obj)
opt

# Report output
rep <- sdreport(obj)


## Compare estimates ----
# - Global intercept
c( fixef(lme.1), summ.2$coefficients$cond[1], opt$par[1])

# - Random effects
cbind( "True"=z_g, "lme4"=ranef(lme.1)[['factor(group_i)']], "glmmTMB"= ranef(lme.2)[[1]][[1]], "RTMB" = obj$env$parList()$z_g )

# - Variances
summ.1
unlist( Report[c("SDZ","SD0")] )

