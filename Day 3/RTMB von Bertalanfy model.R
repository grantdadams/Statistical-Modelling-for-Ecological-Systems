# https://grantdadams.wordpress.com/von-bertalanffy-in-template-model-builder/
library(RTMB)

###### Simulate data
N_obs = 1000 # Number of observations
Min_age = 0 # Minimum age in the population
Max_age = 10 # Maximum age in the population

# Parameters
Obs_sd = 20 # Standard deviation
Linf = 350 # Asymptoptic length/mean length at maximum age (mm)
k = .3 # Growth rate (yr^-1)
t0 = -1.5 # Estimated age at length 0 (yr)

# Simulation
Age_i = runif(N_obs, Min_age, Max_age)
Length_i = rnorm(N_obs, Linf * (1 - exp(-k * (Age_i - t0))), Obs_sd) # Error is normal

# Plot length-at-age
plot(Age_i, Length_i , xlab = "Age (yr)", ylab = "Length (mm)")


###### Write model
VBGM <- function(parms, dataList) {
  # - Data
  Age_i <- dataList$Age_i
  Length_i <- dataList$Length_i

  # - Parameters
  Linf <- exp(parms$logLinf) # To keep > 0
  k <- exp(parms$logk)
  t0 <- parms$t0
  SD <- exp(parms$logSD)

  # - Likelihood
  nll = -sum(dnorm(Length_i, Linf * (1 - exp(-k * (Age_i - t0))), SD, TRUE))

  # - Report
  ADREPORT(Linf);
  ADREPORT(k);
  ADREPORT(t0);
  ADREPORT(SD);

  nll
}


###### Fit model
# Build model objects
parameters = list(logLinf = 8, logk = log(0.3), t0 = 2, logSD = 5) # CAREFUL WITH STARTING VALUES
data = list(Length_i = Length_i, Age_i = Age_i)

cmb <- function(f, d) function(p) f(p, d) ## Helper to make data and explicit input
obj <- MakeADFun(cmb(VBGM, data), parameters)

# Estimate
obj$hessian <- TRUE
opt <- do.call("optim", obj)
opt

# Get uncertainty via Delta method
rep <- sdreport(obj)
summ <- summary(rep)
summ

# Plot
plot(Age_i, Length_i , xlab = "Age (yr)", ylab = "Length (mm)")
curve(summ[5,1] * (1 - exp(-summ[6,1] * (x - summ[7,1]))), from = 0, to = 10, col = 2, lwd = 4, add = TRUE)
