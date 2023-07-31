##### Fit an SIR model to data

##first, clean up by filling missing data with 0
require(padr)
require(deSolve)

timepoint<-5## update this value to move forward in time!
## 1 = end of December
## 2 = end of January
## 3 = end of February
## 4 = end of March
## 5 = present

d<-read.csv(paste0("https://raw.githubusercontent.com/jaywl/workshop_23/main/",
                   timepoint),sep="\t")
d$Week.Ending.Date<-as.Date(d$Week.Ending.Date,"%m/%d/%y")
d<-d[order(d$Week.Ending.Date),]
dat3<-pad(d,interval="week")
dat3$Count[is.na(dat3$Count)]<-0

## now, this is our function to calculate the sum of squares. (predicted-observed)^2
ssq<-function(observed,predicted){
  sum((predicted$I -
         observed$Count/1726765)^2) ## that big number is the population size!
}

## we'll use a new SIR model that uses whole numbers, rather than proportions
SIR.model.2<-function(t, I1, Beta, Gamma){
  init<-c(S=1726765,I=I1,R=0)/1726765 ## initial values for compartments, as % population
  ## note: above is normalzed to population of county!
  parameters<-c(Beta=Beta,Gamma=Gamma) ## beta and gamma parameters
  time<-seq(0,t,by=7) ## time over which to analyze
  
  eqn<-function(time,state,parameters){
    with(as.list(c(state,parameters)),{
      dS<- -Beta*S*I ## dS/dt diff eq
      dI<-Beta*S*I-Gamma*I ## dI/dt diff eq
      dR<-Gamma*I ## dR/dt diff eq
      return(list(c(dS,dI,dR)))})}
  
  out.df<-as.data.frame(ode(y=init,times=time,func=eqn,parms=parameters))
  return(out.df)
}## end function

## we want to minimize the sum of squares. Let's let the computer do the work!
## we'll iterate across a bunch of combinations of parameter values until we 
## find the set of values with the lowest ssq

gams<-seq(0.001,.5,by=.001) ## the gamma values to try. 
bets<-seq(0.001,.5,by=.001) ## the beta values to try

##make an empty matrix to record results
mymat<-matrix(0,nrow=length(gams),ncol=length(bets))

## find out when the current outbreak started, and how long it has been running
# start.date<-dat3$Week.Ending.Date[max(which(dat3$Count==0))]
start.date<-min(dat3$Week.Ending.Date)
epi.length<-length(dat3$Week.Ending.Date[dat3$Week.Ending.Date>=(start.date+7)])*7

## we loop through every combination of gamma and beta 

for(i in 1:length(bets)){
  for(e in 1:length(gams)){
    bet<-gams[e]*bets[i]
    so<-SIR.model.2(t=epi.length,
                    I1=dat3$Count[dat3$Week.Ending.Date==start.date], 
                    ## note: initial infected value in the model is the initial value for the data
                    Beta=bets[i],
                    Gamma=gams[e])
    
    mymat[e,i]<-ssq(observed=dat3[dat3$Week.Ending.Date>=start.date,],
                    predicted=so)
    
  }
}## Impatient? Try using a different range or step size for gams and bets.

## and find the lowest ssq
best.fit<-which(mymat==min(mymat),arr.ind=T)

##record our best-fit values
gamma.fit<-gams[best.fit[1]]
beta.fit<-bets[best.fit[2]]

## specify forecast period, in weeks
nweeks<-0

## let's plot! green dots are observed weekly cases, black dots are model output
plot(y=dat3$Count[dat3$Week.Ending.Date>=start.date]/1726765,
     x=dat3$Week.Ending.Date[dat3$Week.Ending.Date>=start.date],col="dark green",
     ylab="infected proportion of population",
     xlim=c(min(start.date),max(dat3$Week.Ending.Date)+7*nweeks))
points(y=SIR.model.2(t=epi.length+7*nweeks,
                     I1=dat3$Count[dat3$Week.Ending.Date==start.date],
                     Beta=beta.fit,Gamma=gamma.fit)$I,
       x=seq.Date(from=start.date,
                  to=max(dat3$Week.Ending.Date)+7*nweeks,
                  by=7),
         col="black",
       pch=16)

beta.fit ## best fit beta parameter value
gamma.fit ## best fit gamma parameter value
beta.fit/gamma.fit ## best fit R0

#####################################################
############# Discussion Questions ##################
#####################################################
## 1) What do you think about these parameter estimates? 
## Are they reasonable? Why or why not?
## 2) What would you change to improve your estimates?
## 3) Can you forecast with this model? (HINT: Try it
## by simulating the model over a longer period of time,
## by changing the "nweeks" value.
## 4) How reliable do you think that forecast is? why?
## 5) What scenarios could you investigate using this 
## model (or modifications thereof)? How would you go
## about doing so?
## 6) If you wanted to forecast using this model, how would
## you go about doing that? What caveats should you include
## alongside any forecast made with this model?
##
##### !!!!!!!!!!!!!!!! ALERT !!!!!!!!!!!!!!!!!! #####
##
## UPDATE! Some intrepid epidemiologists report that 
## the recovery period for this disease is about 7 days.
## That means you have some information about gamma! 
## Awesome. Now, how can you modify the model fitting 
## approach above to improve your estimates of beta, 
## and therefor R0? Try it, if you have time!
## How does this influence your forecast?
#####################################################
#####################################################
