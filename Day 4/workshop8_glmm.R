Use_REML = TRUE


############
# Generalized linear mixed model
############
library(lme4)

###### Simulate data
# Parameters
Nsite = 10
Nobs_per_site = 10
Site_logMean = log(10)
Site_logSd = 1

# Bookkeeping
s_i = rep( 1:Nsite, each=Nobs_per_site)
s_i = factor(s_i, levels = 1:10)

# Simulation
z_s = rnorm( Nsite, mean=0, sd=Site_logSd )
Mean_s = exp( Site_logMean + z_s )
y_i = rpois( Nsite*Nobs_per_site, lambda=Mean_s[s_i] )

# Plot data
library(lattice)
histogram( ~ y_i | s_i, breaks=seq( min(y_i), max(y_i), length=10), type="density", panel=function(x,...){ panel.histogram(x, ...); panel.mathdensity(dmath=dnorm, col="black", args = list(mean=mean(x),sd=sd(x))) } )      #

###### Fit using R
# No site level (Not recommended)
GLM = glm( y_i ~ 1, family="poisson")
print( summary(GLM) )

# Using fixed effects (Not recommended)
GLM = glm( y_i ~ 0 + s_i, family="poisson" )
print( summary(GLM) )

# Using mixed effects (Recommended)
library(lme4)
GLMM = glmer( y_i ~ 1 + (1 | s_i), family="poisson" )
print( summary(GLMM) )

####################
# Fit using RTMB
####################
