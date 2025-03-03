# https://github.com/kaskr/RTMB/blob/master/tmb_examples/linreg.R
library(RTMB)

# Simulate data
nyrs = 20 # - Number of years
years <- 1:nyrs
n_hauls <- 50

# - Simulate CPUE
trend <- c(1:(nyrs/2), rev(1:(nyrs/2)))
mean_CPUE = 2 + trend
mean_P = 1/(1+exp(-(scale(trend))))
CPUE = rpois(n_hauls * nyrs, (rep(mean_CPUE, each = n_hauls))) * rbinom(n_hauls * nyrs, 1, rep(mean_P, each = n_hauls))
mydata <- data.frame(CPUE = CPUE, year = rep(years, each = n_hauls))

# - Mean plot
plot(x = years, y = mean_CPUE)

# - Observed plot
plot(x = mydata$year, y = mydata$CPUE)


# Write model
delta_glmm <- function(parms, dataList) {
  # Datos
  Y <- dataList$y
  x <- dataList$x
  x_pred <- dataList$x_pred

  # Parametros
  b_bi <- parms$b_bi # - Binomial
  b_po <- parms$b_po # - Poisson

  # Model
  # - Binomial
  lin_pred_presence = x %*% b_bi
  pred_presence = 1/(1+exp(-lin_pred_presence))

  # - Poisson
  lin_pred_po = x %*% b_po
  pred_positive_catch = exp(lin_pred_po)

  # - Combined
  pred = pred_positive_catch * pred_presence

  # Likelihood
  nll_bi = -sum(dbinom(Y > 0, 1, pred_presence, log = TRUE))

  # nll_bi = -(sum(dbinom(Y > 0, 1, pred_presence, log = TRUE)) + sum(dpois(0, pred_positive_catch[Y==0], log = TRUE))) # Zero-inflated model

  nll_po = -sum(dpois(Y[Y>0], pred_positive_catch[Y>0], log = TRUE))
  nll = nll_bi + nll_po

  # Prediction
  y_pred = exp(x_pred %*% b_po)/(1+exp(-x_pred %*% b_bi))

  # Report
  RTMB::REPORT(lin_pred_presence)
  RTMB::REPORT(pred_presence)
  RTMB::REPORT(lin_pred_po)
  RTMB::REPORT(pred_positive_catch)
  RTMB::REPORT(pred)

  RTMB::ADREPORT(y_pred)

  # Return nll
  nll
}

# Make object
# - Data
x_mat <- model.matrix(formula(~ as.factor(year)), data = mydata)

# - Data para el prediction
x_mat_pred <- model.matrix(formula(~ as.factor(sort(unique(mydata$year)))))

data_list <- list(x = x_mat, x_pred = x_mat_pred, y = mydata$CPUE)

# - Parameters
parameters <- list(b_bi = rep(0, ncol(x_mat)), # Binomial coefficients
                   b_po = rep(0, ncol(x_mat))  # Poisson
                   ) # Initial parameters

# - Build model
cmb <- function(f, d) function(p) f(p, d) ## Helper to make data and explicit input
obj <- RTMB::MakeADFun(cmb(delta_glmm, data_list), parameters, silent = TRUE)

# Fit model
opt <- do.call("optim", obj)
opt <- nlminb(start = obj$par, objective = obj$fn, gradient = obj$gr)
opt$par

# GLM version
mod.1 <- glm(I(CPUE > 0) ~ as.factor(year), data = mydata, family = binomial(link = "logit"))
mod.2 <- glm(CPUE ~ as.factor(year), data = mydata, family = poisson(link = "log"))
mod.1$coefficients
mod.2$coefficients

# - Lo mismo que RTMB!


# Hacemos el indice
rep <- sdreport(obj) # Get SE over predicted data


# Plot
pred <- data.frame(year = as.factor(sort(unique(mydata$year))),
                   Index = rep$value,
                   IndexUpper = rep$value + 1.96 * rep$sd,
                   IndexLower = rep$value - 1.96 * rep$sd)

require(ggplot2)
ggplot(pred, aes(x = year, y = Index)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = IndexUpper, ymin = IndexLower)) +
  theme_classic()


plot(x = mean_CPUE, y = pred$Index)




