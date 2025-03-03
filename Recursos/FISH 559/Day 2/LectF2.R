#devtools::install_github('colemonnahan/adnuts', build_vignettes=TRUE)

library(rstan)
library(StanHeaders)
library(tmbstan)
library(ggplot2)
library(TMB)


# =================================================================================================================

MCMCNaive <- function(model,covar,xxx,mult,Nreps=22000,Nburns=2000,Nevery=10)
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
  post1 <- MCMCNaive(model,VarCo,model$env$last.par.best,mult=1.0,Nreps=10000,Nburns=1,Nevery=5)
  post1a <- post1$Outs
  print(head(post1a))
  Nrow <- length(post1a[,1])  
  xx <- seq(1,Nrow)
  par(mfrow=c(2,2))
  plot(xx,exp(post1a[,1]),xlab="Cycle number",ylab="Parameter 1",pch=16)
  plot(xx,exp(post1a[,2]),xlab="Cycle number",ylab="Parameter 2",pch=16)
  plot(xx,exp(post1a[,3]),xlab="Cycle number",ylab="Parameter 3",pch=16)
  plot(xx,-1*post1$ObjFn,xlab="Cycle number",ylab="Post Density",pch=16)
  dataout<-matrix(c(exp(post1a[,1]),exp(post1a[,2]),exp(post1a[,3]),-1*post1$ObjFn),nrow=Nrow)
  pairs(dataout,labels=c("A50","A95","Sigma","Post Density"),pch=16)
  

  # Now consider mcmc sampling 
  plot(0,0)
  mcmcout <- tmbstan(obj=model,iter=2000,chains=1,init="random")
  post2 <- extract(mcmcout)
  print(str(post2))
  Nrow <- length(post2$LogA50)  
  xx <- seq(1,Nrow)
  par(mfrow=c(2,2))
  plot(xx,exp(post2$LogA50),xlab="Cycle number",ylab="Parameter 1",pch=16)
  plot(xx,exp(post2$LogA95),xlab="Cycle number",ylab="Parameter 2",pch=16)
  plot(xx,exp(post2$LogSigma),xlab="Cycle number",ylab="Parameter 3",pch=16)
  dataout<-matrix(c(exp(post2$LogA50),exp(post2$LogA95),exp(post2$LogSigma)),nrow=Nrow)
  pairs(dataout,labels=c("A50","A95","Sigma"),pch=16)
}  

################################################################################

MCMCSumm <- function(file,best,Nyear,data,map,parameters)
{
}  

################################################################################

setwd("D:\\courses\\FISH 559_18\\TMB Workshop\\Lecture Examples\\")

require(TMB)

################################################################################

TheData<-scan(file="lectF.txt",what=list(Length=0,Prob=0))
data <- list(Length=TheData$Length,Prob=TheData$Prob,Prob2=log(TheData$Prob/(1-TheData$Prob)))
parameters = list(LogA50=log(40),LogA95=log(80),LogSigma=log(5))

print(data)
print(parameters)

compile("LectF2.cpp")
dyn.load(dynlib("LectF2"))

model <- MakeADFun(data, parameters, DLL="LectF2",silent=T,hessian=T)
fit <- nlminb(model$par, model$fn, model$gr, control=list(eval.max=100000,iter.max=1000))
best <- model$env$last.par.best
print(best)
rep <- sdreport(model)
print(rep)
VarCo <- solve(model$he())
# Check for Hessian
print(sqrt(diag(VarCo)))

# Now consider mcmc sampling 
plot(0,0)
mcmcout <- tmbstan(obj=model,iter=2000,chains=1,init=list(model$env$parameters))
post2 <- extract(mcmcout)
print(str(post2))
Nrow <- length(post2$LogA50)  
xx <- seq(1,Nrow)
par(mfrow=c(2,2))
plot(xx,exp(post2$LogA50),xlab="Cycle number",ylab="Parameter 1",pch=16)
plot(xx,exp(post2$LogA95),xlab="Cycle number",ylab="Parameter 2",pch=16)
plot(xx,exp(post2$LogSigma),xlab="Cycle number",ylab="Parameter 3",pch=16)
dataout<-matrix(c(exp(post2$LogA50),exp(post2$LogA95),exp(post2$LogSigma)),nrow=Nrow)
pairs(dataout,labels=c("A50","A95","Sigma"),pch=16)
AA



# Now consider mcmc sampling and provide a posterio for By
# ========================================================
DoMCMCAll(model,VarCo)

# Now do the projections
# ==========================
#MCMCSumm("post1.RData",model$env$last.par.best,Nyear,data,map,parameters)

  