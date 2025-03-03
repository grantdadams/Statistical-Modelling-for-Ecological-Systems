setwd("D:\\courses\\FISH 559_20\\TMB Workshop\\Lecture Examples\\")
data <- list(x=rivers)
parameters <- list(mu=0,logSigma=0)

require(TMB)
compile('LectB1.cpp', flags="-Wno-ignored-attributes")
dyn.load(dynlib('LectB1'))

##################
model <- MakeADFun(data,parameters,silent=T)
fit   <- nlminb(model$par, model$fn, model$gr)
rep   <- sdreport(model)
print(summary(rep))