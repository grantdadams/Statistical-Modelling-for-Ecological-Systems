# Simulate data
# - Number of years
nyrs = 20 # - Number of years
years <- 1:nyrs
n_hauls <- 50

# - Simulate CPUE
trend <- c(1:(nyrs/2), rev(1:(nyrs/2)))
rev_trend <- c((nyrs/2):1, rev((nyrs/2):1))
mean_CPUE = 2 + trend  # Tendencia de CPUE
mean_P = 1/(1+exp(-(scale(rev_trend)))) # Probability de capture
CPUE = rpois(n_hauls * nyrs, (rep(mean_CPUE, each = n_hauls))) * rbinom(n_hauls * nyrs, 1, rep(mean_P, each = n_hauls))
mydata <- data.frame(CPUE = CPUE, year = rep(years, each = n_hauls))

# - Mean plot
plot(x = years, y = mean_CPUE)
plot(x = years, y = mean_P)

# - Observed plot
plot(x = mydata$year, y = mydata$CPUE)

# Model presence absence
mydata$presence <- as.numeric(mydata$CPUE > 0)
mod1 <- glm(presence ~ as.factor(year), family = binomial, data = mydata)

# Model non-zero catch
mod2 <- glm(CPUE ~ as.factor(year), data = mydata[mydata$CPUE > 0,], family = poisson)

# Create an index
pred <- data.frame(year = sort(unique(mydata$year)))
pred$prob_presence <- predict(mod1, newdata = pred, type = "response")
pred$est_abundance <- predict(mod2, newdata = pred, type = "response")

pred$pred_index <- pred$prob_presence  *  pred$est_abundance # Multiply probability of presence * abundance|present

# Plot
plot(x = mean_CPUE, y = pred$est_abundance, xlab = "True index", ylab = "Estimated index")

