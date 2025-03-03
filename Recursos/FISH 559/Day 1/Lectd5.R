setwd("D:\\courses\\FISH 559_20\\TMB Workshop\\Lecture Examples\\")


library(TMB)
compile("LectD5.cpp",libinit=FALSE, flags="-Wno-ignored-attributes")                    ## notice flag
dyn.load(dynlib("LectD5"))
set.seed(123)
data <- list(Y = rnorm(10) + 1:10, x=1:10)
parameters <- list(a=0, b=0, logSigma=0)

#obj <- MakeADFun(data, parameters, DLL="LectD42")


## setup call from R
myline<-function(x,a,b).Call("call_myline",as.double(x), as.double(a), as.double(b),PACKAGE="LectD5")

## now you can call

print(myline(1:10, 1, 1.0001))

