library(TMB)
library(gap)
library(DHARMa)

set.seed(888)
Ndata <- 20
Beta2 <- c(1,2)
X <- cbind(rnorm(Ndata,1,1),rnorm(Ndata,2,0.5))
Y <- rnorm(Ndata,X%*%Beta2,0.4)

Data <- list(Y=Y, X=X)
Par <- list(Beta=rep(0,2), ln_sd= 0)
compile("LectD1.cpp", flags="-Wno-ignored-attributes")
dyn.load("LectD1")
obj <- MakeADFun(Data,Par,DLL="LectD1",silent=T)
opt <- nlminb(obj$par,obj$fn,obj$gr);
print(sdreport(obj))

# simulate data
y.sim <- replicate(500,{obj$simulate()$Y})

n.obs <- length(Y)
sim.resid <- rep(NA, n.obs)
for (i in 1:n.obs)
 sim.resid[i] <- ecdf(y.sim[i,])(Y[i])
print(sim.resid)
 
p.val <- ks.test(sim.resid,'punif')$p.value

gap::qqunif(sim.resid,logscale=FALSE)

print(p.val)

sim.resid <- createDHARMa(y.sim, Y, integerResponse=F)

plotQQunif(sim.resid)