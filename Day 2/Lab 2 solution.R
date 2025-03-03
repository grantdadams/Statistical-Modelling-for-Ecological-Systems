# Read data
spawn_recruit <- read.csv("Day 2/DeFilippo_data.csv")

# Calculate R/S
spawn_recruit$RS <- spawn_recruit$Return/spawn_recruit$Spawners


# Estimate ricker
mod <- lm(log(RS) ~ 1 + Spawners, data = spawn_recruit)
summ <- summary(mod)
summ
alpha <- exp(mod$coefficients[1])
beta <- -(mod$coefficients[2])
smax = 1/beta
Smax_se <- summ$coefficients[2,2] * 1/beta^2 # Delta method with lm: SE(x) * F'(X) = SE(B) * 1/B^2 porque f(x) = -1/B

# Plot on transformed scale
par(mfrow = c(1, 2))
plot(y = log(spawn_recruit$RS), x = spawn_recruit$Spawners)
abline(mod)

# Plot on natural scale
plot(y = spawn_recruit$Return, x = spawn_recruit$Spawners)
curve(alpha*x*exp(-beta*x), from = 0, to = 2e6, add = TRUE, col = 2)
abline(v = 1/beta, col = 3, lwd = 2)
abline(h = alpha/(beta * exp(1)), col = 3, lwd = 2)


# Non-parametric bootstrap
nboot <- 1000
smax_vec <- c()

for(i in 1:nboot){
  samp <- sample(1:nrow(spawn_recruit), nrow(spawn_recruit), replace = TRUE)
  mod <- lm(log(RS) ~ 1 + Spawners, data = spawn_recruit[samp, ])
  smax_vec[i] <- 1/-(mod$coefficients[2])
}


# Plot
hist(smax_vec)


# RTMB
spawn_recruit_clean <- spawn_recruit[complete.cases(spawn_recruit[,"RS"]),] # Filter NAs
data <- list(Y = log(spawn_recruit_clean$RS), x = spawn_recruit_clean$Spawners)
parameters <- list(a=0, b=0, logSigma=0)

f <- function(parms) {
  Y <- data$Y
  x <- data$x
  a <- parms$a
  b <- parms$b
  logSigma <- parms$logSigma

  nll = -sum(dnorm(Y, a+b*x, exp(logSigma), TRUE))

  # Smax
  Smax = -1/b
  ADREPORT(Smax);

  nll
}
obj <- MakeADFun(f, parameters)

obj$hessian <- TRUE
opt <- do.call("optim", obj)
opt
rep <- sdreport(obj)
summary(rep)


# Profile likelihood
map <- list(b = factor(NA)) # Fija "b"
smax_profile <- seq(from = 1e5, to = 3e6, by = 1000)
nll_profile = c()

for(i in 1:length(smax_profile)){
  parameters <- list(a=0, b=-1/smax_profile[i], logSigma=0)
  obj <- MakeADFun(f, parameters, map = map, silent = TRUE)
  opt <- do.call("optim", obj)
  nll_profile[i] <- opt$value
}


# Plot
par(mfrow = c(1,2))
hist(smax_vec, breaks = 40, main = "Smax", xlab = "Smax") # Bootstrap
curve(dnorm(x, rep$value, rep$sd)/dnorm(rep$value, rep$value, rep$sd) * 400, from = 0, to = 2e6, add = TRUE, col = 2, lwd = 2) # Delta method in RTMB
curve(dnorm(x, smax, Smax_se)/dnorm(smax, smax, Smax_se) * 400, from = 0, to = 2e6, add = TRUE, col = 3, lwd = 2) # Delta method in "lm"

legend("topright", c("Bootstrap", "Delta method RTMB", "Delta method 'lm'"), lty = 1, lwd = 4, col = c(1,2,3), bty = "n")

plot(x = smax_profile, y = nll_profile - min(nll_profile), xlab = "Smax", ylab = "NLL - min(NLL)", type = "l", xlim = range(smax_vec), main = "Profile")
abline(h = 1.92, lty = 2, col = 4)





