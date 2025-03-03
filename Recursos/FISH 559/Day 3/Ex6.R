setwd("D:\\courses\\FISH 559_20\\TMB Workshop\\In Class Assignments\\Ex6")

# ========================================================================================================

DoPlots <- function(hake)
 {   
  hake$B <- model$report()$B[1:Nyear]
  hake$Ihat1 <- model$report()$Ihat1
  hake$Ihat2 <- model$report()$Ihat2
  plot(hake$t, hake$B, type="l", xlab="Year", ylab="Biomass and Catch (kt)",col="red",lty=2,ylim=c(0,max(hake$B)*1.1))
  lines(hake$t, hake$C)
  plot(I1~t, hake, ylim=c(0,1.1*max(hake$I1)), yaxs="i")
  lines(Ihat1~t, hake)
  plot(I2~t, hake, ylim=c(0,1.1*max(hake$I2)), yaxs="i")
  lines(Ihat2~t, hake)
 } 


# ========================================================================================================

require(TMB)
compile("Ex6Class.cpp", flags="-Wno-ignored-attributes")
dyn.load(dynlib("Ex6Class"))
compile("Ex6.cpp", flags="-Wno-ignored-attributes")
dyn.load(dynlib("Ex6"))

# Don't plot stuff
Plot <- F
par(mfrow=c(3,4))

# specifications of the operating model
r <- 0.37; k <- 2820; q1 <- 1; q2 <- 1; SigmaI <- 0.2; SigmaR <- 0.3
Nyear <- 24*3
Expl <- scan("ExploitRate.dat",n=Nyear)
Expl <- c(Expl,Expl,Expl)
cat(r,k,q1,q2,SigmaR,"\n")

Nsim <- 50  # 50
set.seed(19101)

# Declare objects
hake <- NULL
TrueVals <- matrix(nrow=Nsim,ncol=4)
Estimates <- array(0,dim=c(3,Nsim,4))

# Conduct the simulation stuff
for (Isim in 1:Nsim)
 {
  cat("Starting simulation",Isim,"\n")
  
  # Generate a true population   
  TrueB <- rep(0,Nyear+1); C <- rep(0,Nyear); I1 <- rep(0,Nyear); I2 <- rep(0,Nyear)
  TrueEps <- rnorm(Nyear,0,SigmaR)
  TrueB[1] <- k;
  for (Iyear in 1:Nyear)
   {
    C[Iyear] <- Expl[Iyear]*TrueB[Iyear] 
    I1[Iyear] <- q1*TrueB[Iyear]*exp(rnorm(1,0,SigmaI)) 
    I2[Iyear] <- q2*TrueB[Iyear]*exp(rnorm(1,0,SigmaI)) 
    TrueB[Iyear+1] <- (TrueB[Iyear] + r*TrueB[Iyear]*(1.0-TrueB[Iyear]/k) - C[Iyear])*exp(TrueEps[Iyear])
   }  
   hake$t <- 1:Nyear; hake$C <- C; hake$I1 <- I1; hake$I2 <- I2
   if (Plot==T) plot(hake$t,TrueB[1:Nyear],xlab="Year",ylab="TrueB",type="l",ylim=c(0,max(TrueB)*1.1))
   TrueVals[Isim,1] <- r*k/4
   TrueVals[Isim,2] <- TrueB[Nyear+1]
   TrueVals[Isim,3] <- TrueB[Nyear+1]/k
   TrueVals[Isim,4] <- SigmaR
   

   # Fit the observation error estimator (class version)
   parameters <- list(logR=-1.1, logK=10.0, logQ1=-7.9, logQ2=-7.9, logSigma=log(SigmaI),FF=rep(-2,Nyear),Eps=rep(0,Nyear),LogSigmaR=-1)
   map <- list(Eps=rep(factor(NA),Nyear),LogSigmaR=factor(NA),logSigma=factor(NA),logQ2=factor(NA))
   model <- MakeADFun(hake, parameters,DLL="Ex6Class",map=map,control=list(eval.max=10000,iter.max=1000,rel.tol=1e-15),silent=T)
   fit <- nlminb(model$par, model$fn, model$gr)
   for (i in 1:3) fit <- nlminb(model$env$last.par.best, model$fn, model$gr)
   rep <- sdreport(model)
   #print(summary(rep))

   # Fit the observation error estimator (final version)
   map <- list(Eps=rep(factor(NA),Nyear),LogSigmaR=factor(NA),logSigma=factor(NA))
   model <- MakeADFun(hake, parameters,DLL="Ex6",map=map,control=list(eval.max=10000,iter.max=1000,rel.tol=1e-15),silent=T)
   fit <- nlminb(model$par, model$fn, model$gr)
   for (i in 1:3) fit <- nlminb(model$env$last.par.best, model$fn, model$gr)
   rep <- sdreport(model)
   #print(summary(rep))
   Report <- model$report()
   Estimates[1,Isim,1] <- Report$r*Report$k/4.0
   Estimates[1,Isim,2] <- Report$B[Nyear+1]
   Estimates[1,Isim,3] <- Report$B[Nyear+1]/Report$k
   if (Plot==T) DoPlots(hake)

   # Fit the state space model (errors in variables)
   map <- list(LogSigmaR=factor(NA),logSigma=factor(NA))
   parameters <- list(logR=-1.1, logK=10.0, logQ1=-7.9, logQ2=-7.9, logSigma=log(SigmaI),FF=rep(-2,Nyear),Eps=rep(0,Nyear),LogSigmaR=-1)
   model <- MakeADFun(hake, parameters,DLL="Ex6",map=map,control=list(eval.max=10000,iter.max=1000,rel.tol=1e-15),silent=T)
   fit <- nlminb(model$par, model$fn, model$gr)
   for (i in 1:3) fit <- nlminb(model$env$last.par.best, model$fn, model$gr)
   rep <- sdreport(model)
   #print(summary(rep))
   Report <- model$report()
   Estimates[2,Isim,1] <- Report$r*Report$k/4.0
   Estimates[2,Isim,2] <- Report$B[Nyear+1]
   Estimates[2,Isim,3] <- Report$B[Nyear+1]/Report$k
   if (Plot==T) DoPlots(hake)

   # Fit the state space model (Random effects)
   map <- list(logSigma=factor(NA))
   parameters <- list(logR=-1.1, logK=10.0, logQ1=-7.9, logQ2=-7.9, logSigma=log(SigmaI),FF=rep(-2,Nyear),Eps=rep(0,Nyear),LogSigmaR=-1)
   model <- MakeADFun(hake, parameters,DLL="Ex6",map=map,random="Eps",control=list(eval.max=10000,iter.max=1000,rel.tol=1e-15),silent=T)
   fit <- nlminb(model$par, model$fn, model$gr)
   Index <- which(names(model$env$last.par.best)!="Eps")
   for (i in 1:3) fit <- nlminb(model$env$last.par.best[Index], model$fn, model$gr)
   rep <- sdreport(model)
   #print(summary(rep))
   Report <- model$report()
   Estimates[3,Isim,1] <- Report$r*Report$k/4.0
   Estimates[3,Isim,2] <- Report$B[Nyear+1]
   Estimates[3,Isim,3] <- Report$B[Nyear+1]/Report$k
   Estimates[3,Isim,4] <- Report$SigmaR
   if (Plot==T) DoPlots(hake)
   
      
 }  

 # Hisorgram of results
 par(mfrow=c(3,4))
 Titles <- c("MSY","Current biomss","Current depletion","SigmaR")
 for (Iest in 1:3)
  {
   for (II in 1:4)
    {   
     dist <- (Estimates[Iest,,II] - TrueVals[,II])/TrueVals[,II]
     cat(Iest,II,median(abs(dist)),"\n")
     #print(dist)
     if (II < 4 || Iest==3)
      { 
       hist(dist,xlab=Titles[II],main="",xlim=c(-1,1),yaxs="i",xaxs="i",breaks=10)
       abline(v=0,lwd=3,lty=1,col="red")
      } 
     else 
      plot(0,0,type="n",axes=F,xlab="",ylab="")  
    }
  }    
 
 
################################################################################

parameters <- list(logR=model$env$last.par.best["logR"], 
                   logK=model$env$last.par.best["logK"], 
                   logQ1=model$env$last.par.best["logQ1"],
                   logQ2=model$env$last.par.best["logQ2"], 
                   logSigma=model$env$last.par.best["logSigma"],
                   FF=model$env$last.par.best[names(model$env$last.par.best)=="FF"],
                   Eps=model$env$last.par.best[names(model$env$last.par.best)=="Eps"], 
                   LogSigmaR=-1)
