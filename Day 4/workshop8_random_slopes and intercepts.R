library(lme4)
library(glmmTMB)

## Random intercepts and slopes
# * Simulate data for random slopes and intercepts ----
# Simulate predictors
n_group <- 5
n_data <- 20
group_i = rep( 1:n_group, each = n_data)
x_i <- rnorm( length(group_i), mean=0, sd = 1)

# - Slopes
beta1 = 1
beta1_g = rnorm(n_group, mean=0, sd=0.5)

# - Intercepts
beta0 = 0
beta0_g = rnorm( n_group, mean=2, sd=6)

# Simulate response
y_i =  (beta0 + beta0_g[group_i]) + (beta1 + beta1_g[group_i]) * x_i + rnorm( length(group_i), mean=0, sd=1)



# Plot
plot(x = x_i, y = y_i, col = group_i, pch = 16)
for(i in 1:n_group){
  abline(a = (beta0 + beta0_g[i]), b = beta1 + beta1_g[i], col = i)
}



# * Fit model in R ----
lme.1 = lmer( y_i ~ 1 + x_i + (x_i|factor(group_i)), REML = FALSE) # lme4
summ.1 <- summary(lme.1)

lme.2 = glmmTMB( y_i ~ 1 + x_i + (x_i|factor(group_i)))            # glmmTMB
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
  # - Intercepts
  beta0 <- parameters$beta0
  beta0_g <- parameters$beta0_g

  # - Slopes
  beta1 <- parameters$beta1
  beta1_g <- parameters$beta1_g

  # - Variance
  log_SDobs <- parameters$log_SDobs
  log_SDb0 <- parameters$log_SDb0
  log_SDb1 <- parameters$log_SDb1

  # - Parameter transform
  SDobs = exp(log_SDobs)
  SDb0 = exp(log_SDb0)
  SDb1 = exp(log_SDb1)

  # - Likelihood
  # Random effects
  nll = -sum(dnorm(beta0_g, 0, SDb0, log = TRUE))
  nll = nll -sum(dnorm(beta1_g, 0, SDb1, log = TRUE))

  # Data
  nll = nll -sum(dnorm(y_i, (beta0 + beta0_g[group_i]) + (beta1 + beta1_g[group_i]) * x_i , SDobs, log = TRUE))

  # - Report
  RTMB::REPORT(SDobs)
  RTMB::REPORT(SDb0)
  RTMB::REPORT(SDb1)

  return(nll)
}

# Build inputs
data = list(  group_i = group_i, y_i = y_i, x_i = x_i, n_groups = length(unique(group_i)))
parameters = list( "beta0"=-10, "beta0_g"=rep(0, data$n_groups), # Intercepts
                   "beta1"=1,  "beta1_g"=rep(0, data$n_groups),  # Slopes
                   "log_SDobs"=2, "log_SDb0"=2, "log_SDb1"=2)   # Variances
random = c("beta1_g", "beta0_g") # What parameters are random effects

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



# Fit to real data ----
library(palmerpenguins)
library(ggplot2)
data(penguins)
head(penguins)
penguins$species <- as.factor(penguins$species)

# * Fit model in R ----
penguino = glmmTMB( bill_length_mm ~ 1 + flipper_length_mm + (flipper_length_mm|species), data = penguins)            # glmmTMB
summ <- summary(penguino)
summ

plot(x = penguins$flipper_length_mm, y = penguins$bill_length_mm, col = as.numeric(penguins$species) + 1, pch = 16, ylab = "Bill length", xlab = "Flipper length")

# Plot fitted lines
abline(summ$coefficients$cond[,1], col = 1, lwd = 4) # Global

abline(as.numeric(ranef(penguino)[[1]][[1]][1,] + summ$coefficients$cond[,1]), col = 2, lwd = 4)
abline(as.numeric(ranef(penguino)[[1]][[1]][2,] + summ$coefficients$cond[,1]), col = 3, lwd = 4)
abline(as.numeric(ranef(penguino)[[1]][[1]][3,] + summ$coefficients$cond[,1]), col = 4, lwd = 4)



# - Pocos datos
# - Subsample chinstrap
chin <- penguins %>%
  dplyr::filter(species == "Chinstrap")
losdemas <- penguins %>%
  dplyr::filter(species != "Chinstrap")

data_pocos <- rbind(losdemas, chin[sample(1:nrow(chin), 8),])
data_pocos$species <- as.factor(data_pocos$species)


penguino_pocos = glmmTMB( bill_length_mm ~ 1 + flipper_length_mm + (flipper_length_mm|species), data = data_pocos)            # glmmTMB

plot(x = data_pocos$flipper_length_mm, y = data_pocos$bill_length_mm, col = as.numeric(data_pocos$species) + 1, pch = 16, ylab = "Bill length", xlab = "Flipper length")

# Plot fitted lines
abline(summ$coefficients$cond[,1], col = 1, lwd = 4) # Global

abline(as.numeric(ranef(penguino)[[1]][[1]][1,] + summ$coefficients$cond[,1]), col = 2, lwd = 4)
abline(as.numeric(ranef(penguino)[[1]][[1]][2,] + summ$coefficients$cond[,1]), col = 3, lwd = 4)
abline(as.numeric(ranef(penguino)[[1]][[1]][3,] + summ$coefficients$cond[,1]), col = 4, lwd = 4)


chinmod = lm( bill_length_mm ~ 1 + flipper_length_mm, data = chin)
abline(chinmod, lty =)
