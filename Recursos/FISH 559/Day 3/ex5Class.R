
################################################################################

setwd("D:\\courses\\FISH 559_20\\TMB Workshop\\In Class Assignments\\Ex5")
require(TMB)

################################################################################

m <- scan("ex5.dat",skip=1,n=1,quiet=T)
TT <- scan("ex5.dat",skip=3,n=m,quiet=T)
Tmax <- scan("ex5.dat",skip=5,n=1,quiet=T)
B <- matrix(scan("ex5.dat",skip=7,n=m*Tmax,quiet=T),ncol=Tmax,byrow=T)
R <- matrix(scan("ex5.dat",skip=58,n=m*Tmax,quiet=T),ncol=Tmax,byrow=T)
Phi0 <- scan("ex5.dat",skip=109,n=m,quiet=T)

data <- list(m=m,TT=TT,Tmax=Tmax,B=B,R=R,Phi0=Phi0)

################################################################################
# Hint Nproj = 0 for basic estimation
# Basic estimation
################################################################################

# Set the parameters to initial values
??
parameters <- list(dummy=0,mu=mu,log_tau=log_tau,B0=B0,log_sigR=log_sigR,eta=eta)

#print(data)
#print(parameters)

compile("Ex5.cpp", flags="-Wno-ignored-attributes")
dyn.load(dynlib("Ex5"))

# test code - for checking for minimization
map<-list(mu=factor(NA),log_tau=factor(NA),B0=rep(factor(NA),m),log_sigR=rep(factor(NA),m),eta=rep(factor(NA),m))
model <- MakeADFun(data, parameters, DLL="Ex5",silent=T,map=map)
xx <- model$fn(model$env$last.par)
cat(xx,model$report()$obj_fun,"\n")
#AAA

# Now estimate everything
map<-list(dummy=factor(NA))
model <- MakeADFun(data, parameters, random="??", DLL="Ex5",silent=T,map=map)

# Bounds on the parameters
lowbnd=??
uppbnd=??
fit <- nlminb(model$par, model$fn, model$gr, control=list(rel.tol=1e-12,eval.max=100000,iter.max=1000),
              lower=lowbnd,upper=uppbnd)
best <- model$env$last.par.best
print(best)
rep <- sdreport(model)
print(summary(rep))


