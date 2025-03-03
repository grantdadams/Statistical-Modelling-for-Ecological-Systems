library(lme4)
library(glmmTMB)

# Small number of observations for some groups ----
# * Simulate data for random intercept ----
# Simulate predictors
group_i = rep(1:10, 1:10)
z_g = rnorm( length(unique(group_i)), mean=0, sd=1)
beta0 = 0

# Simulate response
y_i = z_g[group_i] + beta0 + rnorm( length(group_i), mean=0, sd=1)

# Plot
tbl <- table(group_i)
barplot(tbl, beside = TRUE, legend = TRUE)

plot(x = as.factor(group_i), y = y_i)


## Fit lm
mod.1 <- lm(y_i ~ as.factor(group_i))
as.numeric(mod.1$coefficients)[1]
y_i[1]
z_g[1]

## Fit lmm
mod.2 <- glmmTMB(y_i ~ (1|as.factor(group_i)))
mod.2$fit$par[1] + as.numeric(ranef(mod.2)[[1]][[1]][[1]])[1]
# - Mas cerca el promedio de la poblacion
y_i[1]

# Group level covariates ----
# Read data
data <- read.csv("Day 3/EBS_YellowFin_CPUE.csv")
data$YEAR <- as.factor(data$YEAR)
# GLM
binom_model <- glm(CPUE_KGKM2 > 1 ~ YEAR + ColdPool_Extent, data = data, family = binomial(link = "logit")) # Presence-absence
binom_model # ColdPool is NA

# Random effects
binom_model <- glmmTMB(CPUE_KGKM2 > 1 ~ (1|YEAR) + ColdPool_Extent, data = data, family = binomial(link = "logit")) # Presence-absence
binom_model # ColdPool is 0.003727
