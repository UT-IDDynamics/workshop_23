#### estimate R0 from case data

## load 'R0' package
require(R0)

## create the case data time series
mydata<-c(1,2,5,9,7,14,22,20,27,30,45,41,72,83)

## Ase a maximum likelihood method to simultaneously estimate R0 
## and the generation time distribution from case data
R0est<-estimate.R(epid=mydata, 
                  unknown.GT=TRUE, 
                  GT= generation.time("gamma",c(4,1)), 
                  method="ML")
## note: in the above step, we're estimating the generation time from case data,
## starting with a prior (the gamma distribution with mean 4 and sd 1). 
## If you had better information from case data (e.g., symptom onset dates for 
## case pairs), you could inform the generation time distribution from that.
## For more info, see ?estimate.R and the links to papers therein.

R0est ## this object contains your estimate and a bunch of extra info
R0est$estimates$ML$R ## ML estimate
R0est$estimates$ML$conf.int ## 95% confidence interval

try(dev.off(),silent=T) ## clears your recent plot
## the black dots are the number of cases over time, the red line
##is the best-fit prediction model
plotfit(R0est) 
#### end R0 estimation from case data

#####################################################
############# Discussion Questions ##################
#####################################################
## 1) What do you think about this method? Useful or not?
## 2) What factors might influence your estimate?
## 3) How might you improve your estimate?
#####################################################
#####################################################


