# Installing packages ----
install.packages("mgcv")



# Loading data ----
# - From csv
mydata <- read.csv("Day 1/mack.csv")

# - From excel
library(readxl)
mydata <- readxl::read_xlsx("Day 1/mack.xlsx")



# Normal distribution ----
# -  Simulating data
data <- rnorm(n = 100, mean = 6, sd = 1)
hist(data)

# - Likelihood or probability density
dnorm(0, mean = 6, sd = 1)
dnorm(6, mean = 6, sd = 1)

# - Plotting
curve(dnorm(x, mean = 6, sd = 1), from = 0, to = 12)
abline(v = 6, col = 2)

# ~ 68% of probability
abline(v = 5, col = 3)
abline(v = 7, col = 3)

# ~95% of probability
abline(v = 4, col = 4)
abline(v = 8, col = 4)



# Poisson distribution ----
data <- rpois(100, lambda = 10)
hist(data)

dpois(1, 10)
dpois(14, 10)



# Binomial distribution ----
data <- rbinom(10, size = 1, prob = 0.5)
hist(data)

dbinom(1, size = 10, prob = 0.5)
dbinom(7, size = 10, prob = 0.5)



# Probability of capture ----
# - Simulate data
set.seed(123)
prob_captura <- 0.2
ncasts = 20

trip1 <- rbinom(ncasts, size = 1, prob = prob_captura)
trip1

trip2 <- rbinom(1, size = 1, prob = prob_captura)
trip2

# * Analytical maximum likelihood ----
sum(trip1)/ncasts # P from trip1
trip2/ncasts # P from trip2
(sum(trip1)+trip2)/(ncasts * 2) # P from both trips


# * Repeated sampling ----
data <- data.frame(rep = 1:10000, Tuna = NA, P = NA)

for(rep in 1:nrow(data)){
  data$Tuna[rep] = rbinom(1, size = (ncasts*2), prob = prob_captura)
  data$P[rep] = data$Tuna[rep]/(ncasts*2)
}

hist(data$P, breaks = 15, xlab = "p", main = "Rep de pesca")
abline(v = mean(data$P), col = 2, lwd = 4)


# * Repeated but better sampling ----
data <- data.frame(rep = 1:10000, Tuna = NA, P = NA)
ncasts_mas = 200

for(rep in 1:nrow(data)){
  data$Tuna[rep] = rbinom(1, size = (ncasts_mas*2), prob = prob_captura)
  data$P[rep] = data$Tuna[rep]/(ncasts_mas*2)
}

hist(data$P, breaks = 15, xlab = "p", main = "Rep de pesca")
abline(v = mean(data$P), col = 2, lwd = 4)


# * Numerical estimation ----
# - Define negative log-likelihood function
nll_fun <- function(par, k, n){
  nll = -sum(dbinom(k, n, par, log = TRUE))
  return(nll)
}

# - Plot nll across different values of P
prob_vec <- seq(0.1, 0.4, by = 0.001)
nll_vec <- c()

for(i in 1:length(prob_vec)){
  nll_vec[i] <- nll_fun(prob_vec[i],
                        k = (sum(trip1)+trip2),
                        n = ncasts * 2)
}

# nll_vec <- sapply(prob_vec, function(x) nll_fun(x, k = (sum(trip1)+trip2), n = ncasts * 2)))

plot(x = prob_vec, y = nll_vec, ylab = "NLL", xlab = "P", type = "l")


# - Find minima
opt.1 <- optim(par = 0.1,
             fn = nll_fun,
             k = (sum(trip1)+trip2),
             n = (ncasts * 2),
             method = "Brent", lower = 0, upper = 1)

opt.2 <- nlminb(start = 0.1,
             objective = nll_fun,
             k = (sum(trip1)+trip2),
             n = (ncasts * 2), lower = 0, upper = 1)


opt.1$par
abline(v = opt.1$par, col = 2, lwd = 2)
