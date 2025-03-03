# Load data
data <- read.table("Day 4/Lab 4.TXT", header = TRUE)
data$response <- log(data$REC) - log(data$SSB) + log(data$SBPR)
data$negSSB <- - data$SSB
data$SP <- as.factor(data$SP)

# Fit model
library(lme4)
library(nlme)
ricker_mod <- lmer(response ~negSSB:SP + (1|SP), data = data, REML = FALSE)
summ_ricker <- summary(ricker_mod)
print(summ_ricker)


# get residuals and fitted values
data$pred <- (predict(ricker_mod))
data$resid <- resid(ricker_mod)
groups <- unique(data$SP)

# Residual vs predicted
plot(ricker_mod,pch=16,csi=0.3, xlab = "Fitted values", ylab = "Residual") # Total
plot(ricker_mod, form = resid(.) ~ fitted(.) | SP, abline = 0,pch=16, xlab = "Fitted values", ylab = "Residual") # By group
boxplot(data$resid~data$SP, xlab = "Species", ylab = "Residual", main = "Residuals by species")
# No huge trends between groups, maybe some differences in residual variance we might be interested in accounting for or that could be acounted for by rescaling the data (although, may look biological significance)

# QQ plot
par(mfrow = c(1,1))
qqnorm(resid(ricker_mod), main = "Residual QQ plot")
qqline(resid(ricker_mod))
# This looks OK, not very good on the tails, but good enough


# QQplot Random effects, normal
ranefs <- ranef(ricker_mod)$SP$`(Intercept)`
qqnorm(ranefs, main = "Random effects QQ plot")
qqline(ranefs)
# This looks OK, tails a bit off, but good enough.

# Fitted vs observed
plot(data$response, data$pred, xlab = "Observed", ylab = "Predicted", main = "Fitted vs observed")
abline(b = 1, a =0)
# Again, observed vs predicted is OK


# Plot
alpha <- exp(ranefs + fixef(ricker_mod)[1]) # Un alpha (random effect) por cada especie
# Divide by SSB-per-recruit
library(dplyr)
ssbpr <- data %>%
  dplyr::group_by(SP) %>%
  slice(1) %>%
  pull(SBPR)

beta <- fixef(ricker_mod)[2:11] # Un beta (fixed effect) por cada especie

# Plot 1 (one looks bad!)
plot(x = data$SSB, y = data$REC, col = as.numeric(data$SP), pch = 16, xlab = "SSB", ylab = "R")
for(i in 1: length(alpha)){
  curve(alpha[i]/ssbpr[i] * x * exp(-beta[i] * x), from = 0, to = 500, col = i, add = TRUE)
}

