#### SIR model

## first, load 'deSolve' package
require(deSolve) ## this package helps solve systems of differential equations!

## let's code up a basic SIR model
SIR.model<-function(t, Beta, Gamma){
  init<-c(S=0.999,I=0.001,R=0.0) ## initial values for compartments, % of pop
  parameters<-c(Beta=Beta,Gamma=Gamma) ## beta and gamma parameters
  time<-seq(0,t,by=1) ## time over which to analyze
  
  eqn<-function(time,state,parameters){
    with(as.list(c(state,parameters)),{
      dS<- -Beta*S*I ## dS/dt diff eq
      dI<-Beta*S*I-Gamma*I ## dI/dt diff eq
      dR<-Gamma*I ## dR/dt diff eq
      return(list(c(dS,dI,dR)))})}
  
  out.df<-as.data.frame(ode(y=init,times=time,func=eqn,parms=parameters))
  return(out.df)
}## end function

## plot the output!
sirout<-SIR.model(t=100,Beta=.5,Gamma=.3) ## you can adjust the parameter values here.
plot(S~time,sirout,type="l",lwd=3,col="blue",ylim=c(0,1),
     ylab="proportion of population")
lines(I~time,sirout,lwd=3,col="red")
lines(R~time,sirout,lwd=3,col="forest green")
legend("right",legend=c("S","I","R"),
       col=c("blue","red","forest green"),
       lty=1,lwd=3,bty="n")
#### end SIR model

#####################################################
############# Discussion Questions ##################
#####################################################
## 1) Change around the values of t, beta, and gamma
## in the call of SIR.model, a few lines above, then
## plot again.
## 2) What happens when you increase beta? gamma?
## 3) What happens when beta is greater than gamma?
## 4) What happens when gamma is greater than beta?
##
#####################################################
#####################################################
