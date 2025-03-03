library(glmmTMB)

# Read data
data <- read.csv("Day 3/EBS_YellowFin_CPUE.csv")

# Clean data ----
data <- data[, c("YEAR", "CPUE_KGKM2", "BOTTOM_TEMPERATURE_C", "ColdPool_Extent")]
data <- data[complete.cases(data),] # Elimina rows with NA
data$presence <- data$CPUE_KGKM2 > 0 # Presence/absence
data$SCALED_BOTTOM_TEMPERATURE_C <- scale(data$BOTTOM_TEMPERATURE_C)


# Part 1 ----
# * Create delta-model ----
# - Prueba 1
binom_model <- glm(presence ~ as.factor(YEAR) + ColdPool_Extent, data = data, family = binomial(link = "logit")) # Presence-absence

catch_model <- glm(CPUE_KGKM2 ~ as.factor(YEAR) + ColdPool_Extent, data = data[data$CPUE_KGKM2 > 0,], family = Gamma(link = "log")) # Non-zero catch

summary(binom_model)
summary(catch_model)
# - Coldpool solo tiene un valor por año y nos da NA
# - usamos 'BOTTOM_TEMPERATURE_C'


# - Prueba 2
binom_model <- glm(presence ~ as.factor(YEAR) + BOTTOM_TEMPERATURE_C + I(BOTTOM_TEMPERATURE_C^2), data = data, family = binomial(link = "logit")) # Presence-absence

catch_model <- glm(CPUE_KGKM2 ~ as.factor(YEAR) + BOTTOM_TEMPERATURE_C + I(BOTTOM_TEMPERATURE_C^2), data = data[data$CPUE_KGKM2 > 0,], family = Gamma(link = "log")) # Non-zero catch

summary(binom_model)
summary(catch_model)
# Bueno


# * Create an index ----
pred_data <- data.frame(YEAR = sort(unique(data$YEAR)), BOTTOM_TEMPERATURE_C = mean(data$BOTTOM_TEMPERATURE_C)) # Use mean temperature (arbitrary)
pred_data$prob_presence <- predict(binom_model, newdata = pred_data, type = "response") # Predicted probability of presence
pred_data$est_abundance <- predict(catch_model, newdata = pred_data, type = "response") # Predicted CPUE given presence

pred_data$pred_index <- pred_data$prob_presence  *  pred_data$est_abundance # Multiply probability of presence * abundance|present

# * Plot index ----
plot(pred_data$YEAR, pred_data$pred_index, type = "l", xlab = "Year", ylab = "Index")



# Part 2 ----
# Compare root mean squared error (RMSE) between a model with and without bottom temperature
# K-fold cross validation
K = 10 # Number of groups
data$group <- sample(1:K, nrow(data), replace = TRUE)
data$group <- rep(1:K, ceiling(nrow(data)/K))[1:nrow(data)] # Tambien
data$PRED_CPUE.1 <- NA
data$PRED_CPUE.2 <- NA


for(k in 1:K){
  # Fit model to data (except group K)
  ind <- data$group != k

  # * Fit model to training data ----
  # - Delta with temperature
  binom_model.1 <- glm(presence ~ as.factor(YEAR) + BOTTOM_TEMPERATURE_C + I(BOTTOM_TEMPERATURE_C^2), data = data[ind,], family = binomial(link = "logit")) # Presence-absence
  catch_model.1 <- glm(CPUE_KGKM2 ~ as.factor(YEAR) + BOTTOM_TEMPERATURE_C + I(BOTTOM_TEMPERATURE_C^2), data = data[data$CPUE_KGKM2 > 0 & ind,], family = Gamma(link = "log")) # Non-zero catch

  # - Delta without temperature
  binom_model.2 <- glm(presence ~ as.factor(YEAR), data = data[ind,], family = binomial(link = "logit")) # Presence-absence
  catch_model.2 <- glm(CPUE_KGKM2 ~ as.factor(YEAR), data = data[data$CPUE_KGKM2 > 0 & ind,], family = Gamma(link = "log")) # Non-zero catch


  # * Predict on 'test' out of sample data ----
  # - Delta with temperature
  prob_presence.1 <- predict(binom_model.1, newdata = data[!ind,], type = "response") # Predicted probability of presence
  est_abundance.1 <- predict(catch_model.1, newdata = data[!ind,], type = "response") # Predicted CPUE given presence
  data$PRED_CPUE.1[!ind] <- prob_presence.1 * est_abundance.1

  # - Delta without temperature
  prob_presence.2 <- predict(binom_model.2, newdata = data[!ind,], type = "response") # Predicted probability of presence
  est_abundance.2 <- predict(catch_model.2, newdata = data[!ind,], type = "response") # Predicted CPUE given presence
  data$PRED_CPUE.2[!ind] <- prob_presence.2 * est_abundance.2
}

# Calculate RMSE
# - Delta with temperature
sqrt(mean((data$PRED_CPUE.1 - data$CPUE_KGKM2)^2))

# - Delta without temperature
sqrt(mean((data$PRED_CPUE.2 - data$CPUE_KGKM2)^2))
# El delta without temperature es Mejor!



# Extra credit ----
# Fit a delta model in RTMB and derive the CI of the index using the delta-method. Compare it with the bootstrap estimates.

# * Write RTMB model ----
delta_glmm <- function(parms, dataList) {
  # Datos
  Y <- dataList$y
  x <- dataList$x
  x_pred <- dataList$x_pred

  # Parametros
  b_bi <- parms$b_bi # - Binomial
  b_po <- parms$b_po # - Poisson
  sigmaGamma = exp( parms$logSigmaGamma ) # Dispersion for gamma


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
  nll_po = -sum(dgamma(Y[Y>0], shape = 1/sigmaGamma^2, scale =  sigmaGamma^2*pred_positive_catch[Y>0], log = TRUE))
  # Mean = shape * scale
  # Variance = shape * scale^2
  # shape / Variance = scale^2

  nll = nll_bi + nll_po

  # Prediction
  # - Binomial
  lin_pred_presence_pred = x_pred %*% b_bi
  pred_presence_pred = 1/(1+exp(-lin_pred_presence_pred))

  # - Poisson
  lin_pred_po_pred = x_pred %*% b_po
  pred_positive_catch_pred = exp(lin_pred_po_pred)

  pred_index <- pred_presence_pred * pred_positive_catch_pred

  # Report
  RTMB::REPORT(lin_pred_presence)
  RTMB::REPORT(pred_presence)
  RTMB::REPORT(lin_pred_po)
  RTMB::REPORT(pred_positive_catch)
  RTMB::REPORT(pred)

  RTMB::ADREPORT(pred_index) # Delta method for Index

  # Return nll
  nll
}

# * Make object ----
# - Data
x_mat <- model.matrix(formula(~ as.factor(YEAR)), data = data)
x_mat_pred <- model.matrix(formula(~ as.factor(sort(unique(data$YEAR))))) # - Data para el prediction

data_list <- list(x = x_mat,
                  x_pred = x_mat_pred,
                  y = data$CPUE_KGKM2)

# - Parameters
parameters <- list(b_bi = rep(0, ncol(x_mat)), # Binomial coefficients
                   b_po = rep(0, ncol(x_mat)),  # Poisson
                   logSigmaGamma = 0 # Dispersion for Gamma
) # Initial parameters

# - Build model
cmb <- function(f, d) function(p) f(p, d) ## Helper to make data and explicit input
obj <- RTMB::MakeADFun(cmb(delta_glmm, data_list), parameters, silent = TRUE, hessian = TRUE)

# * Fit model ----
opt <- nlminb(start = obj$par, objective = obj$fn, gradient = obj$gr)
opt$par

# * Compare to GLM ----
binom_model <- glm(presence ~ as.factor(YEAR), data = data, family = binomial(link = "logit")) # Presence-absence

catch_model <- glm(CPUE_KGKM2 ~ as.factor(YEAR), data = data[data$CPUE_KGKM2 > 0,], family = Gamma(link = "log")) # Non-zero catch

binom_model$coefficients
catch_model$coefficients
opt$par
# - Casi lo mismo que RTMB!


# * Create RTMB index ----
rep <- sdreport(obj) # Get SE over predicted data using delta Method


# Bootstrap GLM ----
# Non-parametric bootstrap
nboot <- 1000
index_mat <- matrix(NA, nrow = nrow(x_mat_pred), ncol = nboot)

for(i in 1:nboot){
  # * Resample data ----
  samp <- sample(1:nrow(data), nrow(data), replace = TRUE)
  data_boot <- data[samp,]

  # * Fit model ----
  # - Binomial model
  binom_model_boot <- glm(presence ~ as.factor(YEAR), data = data_boot, family = binomial(link = "logit")) # Presence-absence

  # - Gamma model
  catch_model_boot <- glm(CPUE_KGKM2 ~ as.factor(YEAR), data = data_boot[data_boot$CPUE_KGKM2 > 0,], family = Gamma(link = "log")) # Non-zero catch


  # * Create an index ----
  pred_data <- data.frame(YEAR = sort(unique(data_boot$YEAR)))
  prob_presence <- predict(binom_model_boot, newdata = pred_data, type = "response") # Predicted probability of presence
  est_abundance <- predict(catch_model_boot, newdata = pred_data, type = "response") # Predicted CPUE given presence

  index_mat[,i] <- prob_presence  *  est_abundance
}



# Plot ----
# * Base R w/ bootstrap ----
pred_boot <- data.frame(
  Method = "Bootstrap",
  year = (sort(unique(data$YEAR))) - 0.2,
                        Index = rowMeans(index_mat),
                        IndexUpper = apply(index_mat, 1, function(x) quantile(x, probs = 0.975)),
                        IndexLower = apply(index_mat, 1, function(x) quantile(x, probs = 0.025)))


# * RTMB ----
pred_rtmb <- data.frame(
  Method = "RTMB delta method",
  year = (sort(unique(data$YEAR))) + 0.2,
                   Index = rep$value,
                   IndexUpper = rep$value + 1.96 * rep$sd,
                   IndexLower = rep$value - 1.96 * rep$sd)

# * Plot ----
require(ggplot2)
ggplot(rbind(pred_rtmb, pred_boot), aes(x = year, y = Index, colour = Method)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymax = IndexUpper, ymin = IndexLower)) +
  theme_classic() +
  theme(legend.position="bottom") + scale_color_manual(values=c("#999999", "#56B4E9"))




