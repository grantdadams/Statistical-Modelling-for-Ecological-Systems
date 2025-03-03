setwd("D:\\COURSES\\FISH 559_20\\LECTURES\\")

Lecture4<-function()                    
{
  #Case1()
  Case2()
}

Case1<-function()
{
# Case1Gen()
 FileName <- "LECT4A.TXT" 
 TheData <- read.table(file=FileName)
 
 library(nlme) 
 lm1 <- lme(Profit~Lats*Longs,data=TheData,random = ~1|Boats,weights=varIdent(form=~1|Boats),method="ML")
 lm2 <- lme(Profit~Lats,data=TheData,random = ~1|Boats,weights=varIdent(form=~1|Boats),method="ML")
 lm3 <- lme(Profit~Lats*Longs,data=TheData,random = ~1|Boats,method="ML")
 print(summary(lm2))
 print(anova(lm1,lm2,lm3))

 NULL
}

Case1Gen <- function()
{
 set.seed(18010)
 Nboat <- 20
 NdataPerBoat <- 100
 Ntotal <-NdataPerBoat*Nboat
 Lats <- runif(Ntotal,0,60)
 Longs <-runif(Ntotal,100,140)
 BoatFact <- rnorm(Nboat,0,5)
 
 BoatFact <- rep(BoatFact,each=NdataPerBoat)
 Boats <- rep(seq(from=1,to=Nboat,by=1),each=NdataPerBoat)
 BoatVar <- runif(Nboat,1,5)
 print(BoatVar)
 print(BoatVar/BoatVar[1])
 BoatVar <-rep(BoatVar,each=NdataPerBoat)
 
 Pred <- 5+BoatFact*2+Lats*0.1+Longs*0.001
 Obs <- rnorm(Ntotal,Pred,BoatVar)
 
 par(mfrow=c(2,2))
 plot(Boats,Pred)
 plot(Lats,Pred)
 plot(Longs,Pred)

 par(mfrow=c(2,2))
 plot(Boats,Obs)
 plot(Lats,Obs)
 plot(Longs,Obs)
 
 TheData <- data.frame(Profit=Obs,Boats=factor(Boats),Lats=Lats,Longs=Longs)
 FileName <- "C:\\COURSES\\FISH 559_20\\LECTURES\\LECT4A.TXT" 
 write.table(TheData,file=FileName)
 
}

Case2<-function()
{
 # Note paste the results
  
 library(nlme)
 library(graphics)

 FileName <- "LECT4B.TXT" 
 TheData <- scan(FileName,what=list(Subject=0,Age=0,Length=0,NULL),skip=1,n=4*100)
 xx <- as.data.frame(cbind(Subject=TheData$Subject,Age=TheData$Age,Length=TheData$Length))
 AgeLen <- groupedData(Length~Age|Subject,data=as.data.frame(xx),labels=list(x="Age",y="Length"),units=list(x="(yr)",y="(cm)"))
 print(AgeLen)
 par(pch=16)
 plot(AgeLen)
 plot(AgeLen,outer=~1)

# Non-linear model 
 lm1 <- nls(formula=Length~Linf*(1-exp(-1*Kappa*(Age-Tzero))),data=AgeLen,start=c(Linf=100,Kappa=0.2,Tzero=0))
 print(summary(lm1))
 print(AIC(lm1))
 plot(lm1,pch=16,csi=0.3)
 boxplot(split(residuals(lm1),AgeLen$Subject),ylab="Residual",xlab="Subject",csi=0.2)
 abline(0,0,lty=1,lwd=4)

# Non-linear mixed model
 print("non linear model")
 lm2 <- nlme(model=Length~Linf*(1-exp(-1*Kappa*(Age-Tzero))),data=AgeLen,random=Linf~1,fixed=Linf+Kappa+Tzero~1,start=c(Linf=100,Kappa=0.2,Tzero=0))
 #print(summary(lm2))
 LinfEst <- lm2$coefficients$fixed[1]
 #print(LinfEst+lm2$coefficients$random$Subject)
 plot(lm2,pch=16,csi=0.3)
 plot(lm2,Subject~resid(.),abline=0)
 plot(LinfEst+lm2$coefficients$random$Subject,pch=16,cex=1.5)
 plot(lm2, form = resid(., type = "p") ~ fitted(.) | Subject, abline = 0,pch=16)
 plot(augPred(lm2),csi=0.3,pch=16,lwd=2)
 
}

Lecture4()
