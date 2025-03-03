
setwd("D:\\courses\\FISH 559_20\\TMB Workshop\\In Class Assignments\\Ex4")

#devtools::install_github('colemonnahan/adnuts', build_vignettes=TRUE)

# =================================================================================================================

MCMC <- function(model,covar,xxx,mult,Nreps=22000,Nburns=2000,Nevery=10)
{
  library(mvtnorm)
  
  Npars <- length(xxx)
  Nout <- Nreps
  Outputs <- matrix(0,nrow=Nout,ncol=Npars)
  ObjFn <- rep(NA,length=Nout)
  
  # Counters
  ichk <- 0
  jchk <- 0
  
  # Initialize
  Theta <- xxx
  DeltaInit <- 0.1
  Delta <- DeltaInit * xxx
  print(Theta)
  gold <- exp(-1*model$fn(Theta))
  print("gold")
  print(log(gold))
  
  Iout <- 0
  for (Irep in 1:Nreps)
  {
    
    # do for each parameter
    asave <- Theta
    Theta <- rmvnorm(1, mean=asave, sigma=covar*mult)
    g1 <- exp(-1*model$fn(Theta))
    #print(log(g1))
    
    RND <-runif(1,0,1)
    if (g1/gold > RND)
    { gold <- g1; ichk <- ichk+1 }
    else
    { Theta <- asave; jchk <- jchk + 1 } 
    
    if (Irep <= Nburns) 
      if (Irep %% Nevery == 0)
      {
        if (ichk > jchk)
          mult <- mult * 1.05
        else 
          mult <- mult * 0.95
        ichk <- 0
        jchk <- 0
      }  
    
    # end of current replicate
    if (Irep %% Nevery == 0)
      if (Irep > Nburns)
      { 
        Iout <- Iout + 1 
        Outputs[Iout,] <- Theta 
        ObjFn[Iout] <- log(gold)
      }
    if (Irep %% 1000 == 0) print(Irep)  
    
  }
  
  Outs <- list()
  Outs$Outs <- Outputs
  Outs$Outs <- Outs$Outs[1:Iout,]
  Outs$ObjFn <- ObjFn[1:Iout]
  return(Outs)
}

################################################################################

DoMCMCAll <- function(model,VarCo)
 {  
  # Now consider mcmc sampling
  post1 <- MCMC(model,VarCo,model$env$last.par.best,mult=1.0,Nreps=1000000,Nburns=100000,Nevery=1000)
  post1a <- post1$Outs
  save(post1a,file="post1.RData")

  # Now consider mcmc sampling (stan)
  #mcmcout <- tmbstan(obj=model,iter=20000, chains=1)
  #post2 <- extract(mcmcout)
  #save(post2,file="F:\\post2.RData")
}  

################################################################################

MCMCSumm <- function(file,best,Nyear,data,map,parameters)
{
  # Load the parameter file
  load(file=file)

  # Set up for graphs
  par(mfrow=c(2,2))

  Nsim <- length(post1a[,1])
  Npar <- length(post1a[1,])
 
  # Good to check (compare with the sdreport)
  for (II in 1:Npar)  
   cat(II,best[II],mean(post1a[,II]),sd(post1a[,II]),"\n") 

  # Extract biomass and plot
  Biomass <- matrix(0,nrow=Nsim,ncol=Nyear+1)
  for (Isim in 1:Nsim)
   { xx <- model$fn(post1a[Isim,]); Biomass[Isim,] <- model$report()$BioPred[1:(Nyear+1)]; }  
  quant <- matrix(0,nrow=5,ncol=Nyear+1)
  for (Iyear in 1:(Nyear+1))
   quant[,Iyear] <- quantile(Biomass[,Iyear],probs=c(0.05,0.25,0.5,0.75,0.95))  
  Years <- 1:(Nyear+1)
  
  # Plot of biomass posterior
  ymax <- max(quant)
  plot(Years,quant[3,],xlab="Year",ylab="Biomass",ylim=c(0,ymax),type='l')
  xx <- c(Years,rev(Years))
  yy <- c(quant[1,],rev(quant[5,]))
  polygon(xx,yy,col="gray10")
  xx <- c(Years,rev(Years))
  yy <- c(quant[2,],rev(quant[4,]))
  polygon(xx,yy,col="gray90")
  lines(Years,quant[3,],lwd=3,lty=3)
  
  # Projection    
  # ==========
  
  # hahaha - this is for you!
  
}  

################################################################################

setwd("D:\\courses\\FISH 559_18\\TMB Workshop\\In Class Assignments\\Ex4\\")

require(TMB)

################################################################################

Nyear <- scan("ex4.dat",skip=1,n=1,quiet=T)
Nclass <- scan("ex4.dat",skip=3,n=1,quiet=T)
Length <- scan("ex4.dat",skip=5,n=Nclass,quiet=T)
Weight <- scan("ex4.dat",skip=7,n=Nclass,quiet=T)
X <- matrix(scan("ex4.dat",skip=9,n=Nclass*Nclass,quiet=T),ncol=Nclass,byrow=T)
M <- scan("ex4.dat",skip=19,n=1,quiet=T)
CWObs <- scan("ex4.dat",skip=22,n=Nyear,quiet=T)
CALObs <- matrix(scan("ex4.dat",skip=49,n=Nyear*Nclass,quiet=T),ncol=Nclass,byrow=T)
Neff <- scan("ex4.dat",skip=75,n=1,quiet=T)
BioIndex <- scan("ex4.dat",skip=78,n=Nyear,quiet=T)
BioSig <- scan("ex4.dat",skip=104,n=1,quiet=T)

# Selectivity parameter (convert to selectivity)
S50 <- scan("ex4.dat",skip=15,n=2,quiet=T)[1]
S95 <- scan("ex4.dat",skip=15,n=2,quiet=T)[2]
SS50 <- scan("ex4.dat",skip=17,n=2,quiet=T)[1]
SS95 <- scan("ex4.dat",skip=17,n=2,quiet=T)[2]
S <- rep(0,Nclass); SurveyS <- rep(0,Nclass)
for(Iclass in 1:Nclass)
 {
  S[Iclass] <- 1.0/(1+exp(-log(19.0)*(Length[Iclass]-S50)/(S95-S50)))
  SurveyS[Iclass] <- 1.0/(1+exp(-log(19.0)*(Length[Iclass]-SS50)/(SS95-SS50)))
 }  

# Normalize the catcha-at-length data
for (Iyear in 1:Nyear)
 {
  Total <- 0
  for (Iclass in 1:Nclass) Total <- Total + CALObs[Iyear,Iclass]
  for (Iclass in 1:Nclass) CALObs[Iyear,Iclass] = CALObs[Iyear,Iclass]/Total
 }  

################################################################################
# Hint Nproj = 0 for basic estimation
# Basic estimation
################################################################################

# Set the parameters to initial values
LogRbar <- scan("ex4.pin",skip=1,n=1,quiet=T)
LogNinit <- scan("ex4.pin",skip=3,n=Nclass,quiet=T)
LogFullF <- scan("ex4.pin",skip=5,n=Nyear,quiet=T)
Eps <- scan("ex4.pin",skip=9,n=Nyear,quiet=T)
Fproj <- 0

# Data vector
data <- list(Nyear=Nyear,Nclass=Nclass,Length=Length,Weight=Weight,X=X,S=S,SurveyS=SurveyS,M=M,
             CWObs=CWObs,CALObs=CALObs,Neff=Neff,BioIndex=BioIndex,BioSig=BioSig,Nproj=0,Fproj=0)
parameters <- list(dummy=0,LogRbar=LogRbar,LogNinit=LogNinit,LogFullF=LogFullF,Eps=Eps)
# When I was testing the code
map<-list(LogRbar=factor(NA),LogNinit=rep(factor(NA),Nclass),LogFullF=rep(factor(NA),Nyear),
          Eps=rep(factor(NA),Nyear))
# Estimate everything
map<-list(dummy=factor(NA))

#print(data)
#print(parameters)

compile("Ex4.cpp", flags="-Wno-ignored-attributes")
dyn.load(dynlib("Ex4"))

model <- MakeADFun(data, parameters, DLL="Ex4",silent=T,map=map,hessian=T)

# test code - for checking for minimization
xx <- model$fn(model$env$last.par)
#print(model$par)
#print(model$report())
#cat(model$report()$obj_fun,model$report()$LikeCatch,model$report()$LikeCAL,model$report()$LikeBio,model$report()$Penal,"\n")
fit <- nlminb(model$par, model$fn, model$gr, control=list(eval.max=100000,iter.max=1000))
best <- model$env$last.par.best
print(best)
rep <- sdreport(model)
print(rep)
cat(model$report()$obj_fun,model$report()$LikeCatch,model$report()$LikeCAL,model$report()$LikeBio,model$report()$Penal,"\n")

################################################################################
# Hint Nproj = 20 for projections
# Basic estimation
################################################################################

Nproj = 20
compile("Ex4.cpp", flags="-Wno-ignored-attributes")
dyn.load(dynlib("Ex4"))

# Set the parameters to initial values
LogRbar <- scan("ex4.pin",skip=1,n=1,quiet=T)
LogNinit <- scan("ex4.pin",skip=3,n=Nclass,quiet=T)
LogFullF <- scan("ex4.pin",skip=5,n=Nyear,quiet=T)
Eps <- scan("ex4.pin",skip=9,n=Nyear,quiet=T)
Eps <- c(Eps,rep(0,Nproj))

# Data vector
data <- list(Nyear=Nyear,Nclass=Nclass,Length=Length,Weight=Weight,X=X,S=S,SurveyS=SurveyS,M=M,
             CWObs=CWObs,CALObs=CALObs,Neff=Neff,BioIndex=BioIndex,BioSig=BioSig,Nproj=Nproj,Fproj=0)
parameters <- list(dummy=0,LogRbar=LogRbar,LogNinit=LogNinit,LogFullF=LogFullF,Eps=Eps)
# When I was testing the code
map<-list(LogRbar=factor(NA),LogNinit=rep(factor(NA),Nclass),LogFullF=rep(factor(NA),Nyear),
          Eps=rep(factor(NA),Nyear))
# Estimate everything
map<-list(dummy=factor(NA))

#print(data)
#print(parameters)

# Note hessian=T so we get the Hessian matrix
model <- MakeADFun(data, parameters, DLL="Ex4",silent=T,map=map,hessian=T)

# test code - for checking for minimization
xx <- model$fn(model$env$last.par)
#print(model$par)
#print(model$report())
#cat(model$report()$obj_fun,model$report()$LikeCatch,model$report()$LikeCAL,model$report()$LikeBio,model$report()$Penal,"\n")
fit <- nlminb(model$par, model$fn, model$gr, control=list(eval.max=100000,iter.max=1000))
best <- model$env$last.par.best
print(best)
cat(model$report()$obj_fun,model$report()$LikeCatch,model$report()$LikeCAL,model$report()$LikeBio,model$report()$Penal,"\n")
VarCo <- solve(model$he())
rep <- sdreport(model)
print(rep)
print(model$report()$obj_fun)
# Check for Hessian
print(sqrt(diag(VarCo)))


# Now consider mcmc sampling and provide a posterio for By
# ========================================================
DoMCMCAll(model,VarCo)

# Now do the projections
# ==========================
MCMCSumm("post1.RData",model$env$last.par.best,Nyear,data,map,parameters)

  