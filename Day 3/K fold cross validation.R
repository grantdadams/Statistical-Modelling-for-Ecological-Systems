# Simulate data
set.seed(123)
n <- 40
x <- rnorm(n, 0 , 1)
y = rpois(n, exp(2 + x))
data <- data.frame(y = y, x = x, predPois = NA, predNorm = NA)
plot(x, y)

# K-fold cross validation
K = 10 # Number of groups
data$group <- sample(1:K, n, replace = TRUE)
data$group <- rep(1:K, each = n/K) # Tambien


for(k in 1:K){
  # Fit model to data (except group K)
  ind <- data$group != k
  mod.pois <- glm(y ~ x, family = poisson(link = "log"), data = data[ind,])
  mod.norm <- lm(y ~ x, data = data[ind,])

  # Predict data for group K
  ind <- data$group == k
  data$predPois[ind] <- predict(mod.pois, newdata = data, type = "response")[ind]
  data$predNorm[ind] <- predict(mod.norm, newdata = data, type = "response")[ind]
}

# Calculate RMSE
# - Pois
sqrt(mean((data$predPois - data$y)^2))

# - Norm
sqrt(mean((data$predNorm - data$y)^2))

