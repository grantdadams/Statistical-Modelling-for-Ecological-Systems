# https://github.com/grantdadams/Growth-Models/tree/master
library(RTMB)

###### Simulate data ----
set.seed(1234)

# Number of Groups
Groups=6

# True VBGM hyperparameters c(mean, sd)
true.Linf = c(500,20)
true.k = c(.3,.05)
true.t0 = c(1.5,.4)

sigma <- 15

# Age range
ages = seq(from=1,to=15, by = .05)

# Empty matrix and vectors to fill with parameters and data, respectively
param.mat = matrix(NA,Groups,3,byrow = T)
colnames(param.mat) <- c("Linf", "k", "t0")
ctr = 0
age = c()
length = c()
group = c()

# Simulate group level parameters
for(i in 1:Groups){
  # This should be a multi-variate normal ideally
  param.mat[i,1] = rnorm(1,true.Linf[1],true.Linf[2]) # Assign group level Linf
  param.mat[i,2] = rnorm(1,true.k[1],true.k[2]) # Assign group level k
  param.mat[i,3] = rnorm(1,true.t0[1],true.t0[2]) # Assign group level t0
  n.samples = sample(200:1000, 1) # Number of samples per group s

  # Simulate data
  for(j in 1:n.samples) {
    ctr = ctr + 1 # Indexing variable
    age[ctr] = sample(ages, 1) # Sample randon age from age range
    length[ctr] = (param.mat[i,1] * (1 - exp(-param.mat[i,2]*(age[ctr]-param.mat[i,3])))) + rnorm(1,0,sigma)
    group[ctr] = i
  }
}

# Assign data to data frame
dat = data.frame(age = age, length = length, group = group, N = length(age), G = length(unique(group)))
dat <- dat[which(dat$length > 0),]

# Plot the data
plot(NA, NA, xlim = c(0, 15), ylim = c(0, max(dat$length)), xlab = "Age (yr)", ylab = "Length (mm)")
points(dat$age, dat$length , col = dat$group)




###### Write model ----
VBGM <- function(parms, dataList) {
  # - Data
  Age_i <- dataList$Age_i
  Length_i <- dataList$Length_i
  Group_i <- dataList$Group_i

  # - Parameters
  Linf <- exp(parms$logLinf) # To keep > 0
  k <- exp(parms$logk)
  t0 <- parms$t0
  SD <- exp(parms$logSD)

  # random effects params
  log_Linf_d <- parms$coef_Linf
  Linf_d <- exp(log_Linf_d)
  Linf_d_sigma <- exp(parms$log_Linf_sigma)

  log_k_d <- parms$coef_k
  k_d <- exp(log_k_d)
  k_d_sigma <- exp(parms$log_k_sigma)

  t0_d <- parms$coef_t0
  t0_d_sigma <- exp(parms$log_t0_sigma)

  # - Model
  Linf_g = Linf * Linf_d
  k_g = k_d * k
  t0_g = t0 + t0_d

  Pred_Length_i = Linf_g[Group_i] * (1 - exp(-k_g[Group_i]* (Age_i - t0_g[Group_i])))

  # - Likelihood
  nll = 0
  nll = nll - sum(dnorm(x = log_Linf_d, mean = 0, sd = Linf_d_sigma, log = TRUE))
  nll = nll - sum(dnorm(x = log_k_d, mean = 0, sd = k_d_sigma, log = TRUE))
  nll = nll - sum(dnorm(x = t0_d, mean = 0, sd = t0_d_sigma, log = TRUE))

  nll = nll - sum(dnorm(x = Length_i, mean = Pred_Length_i, sd = SD, log = TRUE))

  # - Report
  REPORT(Linf_g)
  REPORT(k_g)
  REPORT(t0_g)

  ADREPORT(Linf);
  ADREPORT(k);
  ADREPORT(t0);
  ADREPORT(SD);


  ADREPORT(Linf_g);
  ADREPORT(k_g);
  ADREPORT(t0_g);
  # ADREPORT(Linf_g[Group_i] * (1 - exp(-k_g[Group_i]* (Age_i - t0_g[Group_i]))));

  nll
}


###### Fit model ----
# Build model objects
# - Parameters
parameters = list(logLinf = 5,
                  logk = log(0.3),
                  t0 = 2,
                  logSD = 0,
                  coef_Linf = rep(0, Groups),
                  log_Linf_sigma = 0,
                  coef_k = rep(0, Groups),
                  log_k_sigma = 0,
                  coef_t0 = rep(0, Groups),
                  log_t0_sigma = 0
) # CAREFUL WITH STARTING VALUES


# - Map (turn off parameters using NA)
map <- list(
  coef_Linf = as.factor(rep(NA, Groups)),
  coef_k = as.factor(rep(NA, Groups)),
  coef_t0 = as.factor(rep(NA, Groups)),
  log_Linf_sigma = as.factor(NA),
  log_k_sigma = as.factor(NA),
  log_t0_sigma = as.factor(NA)
)

# - Data
data = list(Length_i = dat$length, Age_i = dat$age, Group_i = dat$group)
random = c('coef_Linf', 'coef_k', 'coef_t0')

# - Model object
cmb <- function(f, d) function(p) f(p, d) ## Helper to make data and explicit input
obj <- MakeADFun(cmb(VBGM, data), parameters, random = random)

# Estimate
obj$hessian <- TRUE
opt <- nlminb(obj$par, obj$fn, obj$gr)
opt

# Get uncertainty via Delta method
rep <- sdreport(obj)
summ <- summary(rep)


###### Plot ----
plot(NA, NA, xlim = c(0, 15), ylim = c(0, max(dat$length)), xlab = "Age (yr)", ylab = "Length (mm)")
points(dat$age, dat$length , col = dat$group)

# - Rearrange parameters
library(dplyr)
library(stringr)
summ <- as.data.frame(summ)
summ$Parm <- rownames(summ)
Linf_g <- summ %>%
  filter(str_detect(Parm, regex("Linf_g", ignore_case = TRUE)))
k_g <- summ %>%
  filter(str_detect(Parm, regex("k_g", ignore_case = TRUE)))
t0_g <- summ %>%
  filter(str_detect(Parm, regex("t0_g", ignore_case = TRUE)))


# - Plot curves
for(i in 1:Groups){
  curve(Linf_g[i,1] * (1 - exp(-k_g[i,1] * (x - t0_g[i,1]))), from = 0, to = 15, col = i, lwd = 3, add = TRUE)
}


###### Likelihood profile ----
# Profile over global Linf
# - Map (turn off logLinf using NA)
map <- list(logLinf = as.factor(NA))

# - Vector of logLinf
logLinf_vec <- log(seq(from = 450, to = 550, by = 1))
nll_vec <- c()

for(i in 1:length(logLinf_vec)){
  # - Update logLinf fixed value
  parameters$logLinf <- logLinf_vec[i]

  # - Fit model object and save NLL
  obj <- MakeADFun(cmb(VBGM, data), parameters, random = random, map = map, silent = TRUE)

  opt <- nlminb(obj$par, obj$fn, obj$gr)
  nll_vec[i] <- opt$objective
}


plot(x = exp(logLinf_vec), y = nll_vec - min(nll_vec), ylab = "NLL - min(NLL)", xlab = "Linf", type = "l")
abline(h = 2, col = 3)



