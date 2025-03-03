library(nlme)
setwd("D:\\COURSES\\FISH 559_20\\LECTURES\\")

Lecture1<-function()                    
{
 library(nlme)
 Case1()
 #Case2()
}

Case1<-function()
{
 par(mfrow=c(2,2))

# Read in the data and convert to a groupedData object
 TheData <- scan("LECT1A.TXT",what=list(Stream=0,Tst=0,Density=0),n=3*18)
 Streams <- data.frame(TheData)
 print(Streams)
    
 print("Linear Fixed Effects Model")
 lm1 <- lm(Density~1,data=Streams)  
 boxplot(split(lm1$residuals,Streams$Stream),ylab="Residual",xlab="Stream",csi=0.2)
 abline(0,0,lwd=3)
 print(summary(lm1))
 print(paste("AIC ",AIC(lm1)))

 print("Linear Fixed Effects Model")
 lm2 <- lm(Density~factor(Stream)-1,data=Streams)   
 boxplot(split(lm2$residuals,Streams$Stream),ylab="Residual",xlab="Stream",csi=0.2)
 abline(0,0,lwd=3)
 print(summary(lm2))
 print(paste("AIC ",AIC(lm2)))

 print("Linear Mixed Effects Model")
 lm3 <- lme(fixed = Density ~ 1,data=Streams,random = ~ 1 | Stream) 
 boxplot(split(lm3$residuals[,2],Streams$Stream),ylab="Residual",xlab="Stream",csi=0.2)
 abline(0,0,lwd=3)
 print(summary(lm3))

 NULL

}

Case2 <- function()
{
    
 library(lattice)
# Read in the data and convert to a groupedData object
 TheData <- scan("LECT1B.TXT",what=list(Subject=0,TST1=0,TST2=0,Length=0,Weight=0,TST3=0),n=6*100)
 LenW <- data.frame(TheData)
 LenW2 <- groupedData(Weight~Length|Subject,data=LenW)
 print(attributes(LenW2))
 lm1 <- lme(Weight~Length,data=LenW2,random = ~1 |Subject,method="REML") 
 print(summary(lm1))
 plot(lm1,Weight~fitted(.)|Subject, abline = c(0,1))
 par(mfrow=c(2,2))
 print("Model 1")
 print(summary(lm1))

 
 lmeControl(maxIter=10000)
 lm1 <- lme(Weight~Length*Subject,data=LenW,random = ~1 |Subject,method="ML")  
 lm2 <- lme(Weight~Length,data=LenW,random = ~1 |Subject,method="ML")  
 print("# Models 1 and 2")
 print(anova(lm1,lm2))
 print(summary(lm1))
 print(coef(lm1))
 print(intervals(lm1,0.95))
}

Lecture1()
