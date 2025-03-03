#library(nlme)
library(lme4)

Owls <- read.table("D:\\courses\\FISH 559_20\\Lectures\\Owls.txt",header=T)
print(head(Owls))
Owls$NCalls<-Owls$SiblingNegotiation
Owls$LBroodSize<-log(Owls$BroodSize)
Owls$fNest<-factor(Owls$Nest)

# Poisson GLM
Case1 <- glm(NCalls ~ offset(LBroodSize)+FoodTreatment+ArrivalTime,data=Owls,family=poisson)
print(summary(Case1))
par(mfrow=c(2,2))
plot(Case1)

# Poisson GLMM
Case2 <- glmer(NCalls ~ offset(LBroodSize)+FoodTreatment+ArrivalTime + (1|fNest),data=Owls,family=poisson)
print(summary(Case2))
par(mfrow=c(2,2))
plot(Case2)


library(mgcv)
Case3<-gamm(NCalls~offset(LBroodSize)+FoodTreatment+s(ArrivalTime),random=list(fNest=~1),data=Owls,family=poisson)

summary(Case3$gam)
anova(Case3$gam)
plot(Case3$gam)

summary(Case3$lme)

E4<-resid(Case3$lme,type="normalized")

