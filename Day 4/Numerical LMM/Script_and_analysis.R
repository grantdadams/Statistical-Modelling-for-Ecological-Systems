# Fits a random intercepts model
# Part 1 - Fit using nlme

library(nlme)
library(lme4)
dat <- read.table("Day 4/Numerical LMM/Data1.TXT", header = FALSE)
colnames(dat) <- c("stream", "ignore", "response")
mod1 <- lme(response ~ 1, random = ~1|stream, data = dat, method = "ML")
mod2 <- lmer(response ~ 1+(1|stream), data = dat, REML = FALSE)
print(summary(mod1))



# Part 2 - Fit using numerical integration
log_like <- function(response, intercept, intercept_re, sigma_intercept, sigma_obs){

  ll_obs <- dnorm(respons, intercept + intercept_re, sigma_obs, log = TRUE)
}

mixed_mod <-function(beta_mu, sigma_beta, sigma){
  # Build nll
  library(Rcpp)
  sourceCpp("lmm.cpp")

  # Caclulate nll
  dat <- read.table("Data1.TXT", header = FALSE)
  colnames(dat) <- c("stream", "blah", "response")
  beta_re <- seq( from = -5, to = 5, by = .01)
  nll <- mixed_nll( dat$response, dat$stream, beta_mu, beta_re, sigma_beta, sigma)
  return(nll)
}

# Fit model
fit_r <- function(){
  library(stats4)
  mle_calc <- mle(mixed_mod, start=list(beta_mu = 50, sigma_beta = log(4), sigma = log(4)))
  parameters <- c(mle_calc@coef[1], exp(mle_calc@coef[2:3]))
  print(parameters)
}

# Profile likelihood
like_profile <- function(){
  # Calculate MLEs
  mle_calc <- mle(mixed_mod, start=list(beta_mu = 50, sigma_beta = log(4), sigma = log(4)))
  parameters <- c(mle_calc@coef[1], exp(mle_calc@coef[2:3]))
  parameters
  nll <- mle_calc@min

  # Build nll
  library(Rcpp)
  sourceCpp("lmm.cpp")

  # Load data
  dat <- read.table("Data1.TXT", header = FALSE)
  colnames(dat) <- c("stream", "blah", "response")
  beta_re <- seq( from = -5, to = 5, by = .2)

  # Calculate vector of likelihood
  beta_vec <- seq(from = parameters[1] * 0.7, to = parameters[1] * 1.3, by = 0.1)

  nll_vec <- c()
  for(i in 1:length(beta_vec)){
    nll_vec[i] <-mixed_nll( dat$response, dat$stream, beta_vec[i], beta_re, log(parameters[2]), log(parameters[3]))
  }

  # Convert to likelihood
  like_vec <- nll_vec - min(nll_vec)

  # Calculate where it is 1.96
  critical <- 1.96
  upper_ci <- uniroot( function(x) mixed_nll( dat$response, dat$stream, x, beta_re, log(parameters[2]), log(parameters[3])) - nll - critical, upper = max(beta_vec), lower = parameters[1], tol = 1e-5)

  lower_ci <- uniroot( function(x) mixed_nll( dat$response, dat$stream, x, beta_re, log(parameters[2]), log(parameters[3])) - nll - critical, lower = min(beta_vec), upper = parameters[1], tol = 1e-5)

  # Plot likelihood profile
  plot(x = beta_vec,  y = like_vec, xlab = "Mean stream density", ylab = "Scaled nll", type = "l", lwd = 2)
  abline(v = upper_ci, col = 2, lwd = 2)
  abline(v = lower_ci, col = 2, lwd = 2)
  abline(v = parameters[1], col = 1, lwd = 2, lty = 1)
  legend("bottomright", legend = c("MLE", "CI", "R", "NLME"), col = c(1,2,1,1), lty = c(1,1,1,2), bty = "n")

  # Compar with NLME
  library(nlme)
  dat <- read.table("Data1.TXT", header = FALSE)
  colnames(dat) <- c("stream", "ignore", "response")
  mod1 <- lme(response ~ 1, random = ~1|stream, data = dat, method = "ML")
  sum <- intervals(mod1)
  upper_nlme <- sum$fixed[1]
  lower_nlme <- sum$fixed[3]
  abline(v = sum$fixed[2], col = 1, lwd = 2, lty = 2)
  abline(v = upper_nlme, col = 2, lwd = 2, lty = 2)
  abline(v = lower_nlme, col = 2, lwd = 2, lty = 2)
}

