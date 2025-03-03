setwd("D:\\courses\\FISH 559_20\\TMB Workshop\\Lecture Examples\\")




#==========================================================================================

require(TMB)
compile("LectE.cpp", flags="-Wno-ignored-attributes")
dyn.load(dynlib("LectE"))

################################################################################

Nyear <- scan("LectE.dat",skip=1,n=1,quiet=T)
Nage <- scan("LectE.dat",skip=3,n=1,quiet=T)+1
M <- scan("LectE.dat",skip=5,n=1,quiet=T)
Wght <- scan("LectE.dat",skip=7,n=Nage,quiet=T)
SigCatch <- scan("LectE.dat",skip=9,n=1,quiet=T)
SigCPUE <-scan("LectE.dat",skip=11,n=1,quiet=T)  
Omega <-scan("LectE.dat",skip=13,n=1,quiet=T)
Catch <- matrix(scan("LectE.dat",skip=15,n=Nyear*3,quiet=T),ncol=3,byrow=T)[,2]
CPUE <- matrix(scan("LectE.dat",skip=15,n=Nyear*3,quiet=T),ncol=3,byrow=T)[,3]
Propn <- matrix(scan("LectE.dat",skip=37,n=Nyear*(Nage+1),quiet=T),ncol=Nage+1,byrow=T)[,-1]

################################################################################

LogN <- scan("LectD3.pin",skip=2,n=Nyear+Nage-1,quiet=T)
Sel50 <- scan("LectD3.pin",skip=4,n=1,quiet=T)
Sel95 <- scan("LectD3.pin",skip=6,n=1,quiet=T)
LogFish <- scan("LectD3.pin",skip=8,n=Nyear,quiet=T)
logq <- scan("LectD3.pin",skip=10,n=1,quiet=T)

################################################################################

# Hint Project is set to 1 for projections

# Data vector
data <- list(Nyear=Nyear,Nage=Nage,M=M,Wght=Wght,SigCatch=SigCatch,SigCPUE=SigCPUE,Omega=Omega,
             Catch=Catch,CPUE=CPUE,Propn=Propn)
             
             
parameters <- list(dummy=0,LogN=LogN,Sel50=Sel50,Sel95=Sel95,LogFish=LogFish,logq=logq)

# When I was testing the code
#map<-list(LogN=rep(factor(NA),length(LogN)),Sel50=factor(NA),Sel95=factor(NA),
#                   LogFish=rep(factor(NA),length(LogFish)),logq=factor(NA))
# Estimate everything
map<-list(dummy=factor(NA))

#print(data)
#print(parameters)

################################################################################

model <- MakeADFun(data, parameters, DLL="LectE",silent=T,map=map,hessian=T)

# test code - for checking for minimization
xx <- model$fn(model$env$last.par)
#print(model$report())
#cat(model$report()$obj_fun,model$report()$Like1,model$report()$Like2,model$report()$Like3,"\n")

# Actual minimzation (with some "Bonus" parameters from nlminb)
fit <- nlminb(model$par, model$fn, model$gr, control=list(eval.max=100000,iter.max=1000))
best <- model$env$last.par.best
print(best)

# Asymptotic variances
rep <- sdreport(model)
print(summary(rep))

# Extract the variance-covariance matrix (and report that)
VarCo <- solve(model$he())
Var <- diag(VarCo)
Corrn <- matrix(0,nrow=length(best),ncol=length(best))
for (II in 1:length(best))
 for (JJ in 1:length(best))  
  Corrn[II,JJ] <- VarCo[II,JJ]/sqrt(Var[II]*Var[JJ])  
print(round(Corrn,3))

# Likelihood profile for 
prof <- tmbprofile(model,"Sel50",trace=F)
plot(prof)
print(confint(prof, level = 0.8))
print(confint(prof, level = 0.9))
print(confint(prof, level = 0.95))

