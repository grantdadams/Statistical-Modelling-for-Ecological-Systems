## Simulate age-structured model with no fishing ----
# - Specifications
R0 <- 1000
M <- 0.2
Msigma <- 0.1
Rsigma <- 0.8
nages <- 10
nyrs <- 100
beta = 0.001 # Ricker beta
alpha = 5 # Ricker alpha
Nmat <- matrix(0, nyrs, nages)

# Time varying natural mortality
Mvec <- M * exp(rnorm(nyrs, 0, Msigma))
Maturity <- rep(c(0, 1), each = nages/2)

# Fill out numbers at age
# - Year 1
Nmat[1,] <- R0 * exp(-Mvec[1] * ((1:nages) - 1))
Nmat[1,nages] <- Nmat[1,nages] / (1 - exp(-Mvec[1] * (nages - 1))) # Geometric series

# - Year 2:nyrs
for(yr in 2:nyrs){
  # Recruitment
  Spawners <- sum(Maturity * Nmat[yr - 1, ])
  Nmat[yr, 1] <- alpha * Spawners * exp(-beta * Spawners) * exp(dnorm(1, 0, Rsigma)) # Ricker

  # Ages 2+
  for(age in 2:nages){
    Nmat[yr, age] <- Nmat[yr - 1, age - 1] * exp(-Mvec[yr])
  }
}
