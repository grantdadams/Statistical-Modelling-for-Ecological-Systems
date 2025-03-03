library(TMB)

setwd("D:\\Research\\csiro\\Species20\\TMB Class\\Assignments\\Ex6\\")

Data <- read.csv("Ex6.csv")[,-1]
Nbatch <- max(Data$Batches)
Nunit <- length(Data[,1])

# Compile
require(TMB)
compile("Ex7.cpp", flags="-Wno-ignored-attributes")
dyn.load(dynlib("Ex7"))

# Create the data and parameters
data <- NULL
data$Nbatch=Nbatch; data$Nunit=Nunit;data$IsRandom=0
data$Batches <- Data$Batches; data$Units <- Data$Units; data$Treat <- Data$Treat 
data$Original <- Data$Original; data$Final <- Data$Final
#print(str(data))
parameters <- list(Control=0,Treatment=0,EpsB=rep(0,Nbatch),EpsU=rep(0,Nunit),logSigmaB=-1,logSigmaU=-1)
#print(str(parameters))


print("Model 1: No random effects")
map <- list(EpsB=rep(factor(NA),Nbatch),EpsU=rep(factor(NA),Nunit),logSigmaB=factor(NA),logSigmaU=factor(NA))
model <- MakeADFun(data, parameters,DLL="Ex7",map=map,control=list(eval.max=10000,iter.max=1000,rel.tol=1e-15),silent=T)
fit <- nlminb(model$par, model$fn, model$gr)
print(model$fn())
rep <- sdreport(model)
print(summary(rep))
Report1 <- model$report()
#print(Report)

print("Model 2: With random effects")
map <- NULL
data$IsRandom <- 1
model <- MakeADFun()
fit <- nlminb(model$par, model$fn, model$gr)
print(model$fn())
rep <- sdreport(model)
rep2 <- summary(rep)
print(rep2[row.names(rep2)!="EpsU" & row.names(rep2)!="EpsB",])
Report2 <- model$report()
#print(Report)

