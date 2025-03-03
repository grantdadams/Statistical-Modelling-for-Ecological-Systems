setwd("D:\\courses\\FISH 559_20\\TMB Workshop\\Lecture Examples\\")

hake <- read.table("LectC2.dat", header=TRUE)
names(hake) <- c("t", "C", "I")
Nyear <- length(hake$C)
parameters <- list(logR=-1.1, logK=8.0, logQ=-7.9, logSigma=-2.3,FF=rep(-2,Nyear))
print(parameters)

require(TMB)
compile("LectC2.cpp", flags="-Wno-ignored-attributes")
dyn.load(dynlib("LectC2"))

################################################################################

model <- MakeADFun(hake, parameters,DLL="LectC2",control=list(eval.max=10000,iter.max=1000,rel.tol=1e-15),silent=T)
print(attributes(model))

fit <- nlminb(model$par, model$fn, model$gr)
for (i in 1:3)
 fit <- nlminb(model$env$last.par.best, model$fn, model$gr)

rep <- sdreport(model)

# Sumamrize ALL
print(summary(rep,p.value=T))
# Restrict what comes out to the fixed parameters only
print(summary(rep,select="fixed",p.value=F))

################################################################################

hake$B <- model$report()$B[1:Nyear]
hake$Ihat <- model$report()$Ihat

par(mfrow=c(2,2))
matplot(hake$t, hake[c("C","B")], type="l",
        xlab="Year", ylab="Biomass and Catch (kt)")
plot(I~t, hake, ylim=c(0,1.1*max(hake$I)), yaxs="i")
lines(Ihat~t, hake)

################################################################################

# Do a likelihood profile
prof <- tmbprofile(model,"logR",trace=F)
plot(prof)
print(confint(prof,leve=0.1))
