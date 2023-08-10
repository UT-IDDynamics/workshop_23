#### SIRV model
require(deSolve)
## We'll code another compartmental model, but this time, we add a vaccinated 
## compartment. The approach is the "mass vaccination during an epidemic of a 
## rare, nonendemic pathogen". Vaccination is with a perfect vaccine giving 
## instant, lifelong immunity to all vaccinated. For other variants, please 
## see Keeling and Rohani, chapter 8.

SIRV.model<-function(t, V1, Beta, Gamma, Mu){
  init<-c(S=0.999-V1,I=0.001,R=0.0,V=V1) ## initial values for compartments
  parameters<-c(Beta=Beta,Gamma=Gamma,Mu=Mu) ## beta, gamma, and mu parameters
  time<-seq(0,t,by=1) ## time over which to analyze
  
  eqn<-function(time,state,parameters){
    with(as.list(c(state,parameters)),{
      dS<- -Beta*S*I-Mu ## dS/dt diff eq. 
      ## Note individuals in S compartment are removed via vaccination rate mu.
      dI<-Beta*S*I-Gamma*I ## dI/dt diff eq
      dR<-Gamma*I ## dR/dt diff eq
      dV<-Mu ## DV/dt diff eq. mu is the rate of vaccine delivery per unit time.
      return(list(c(dS,dI,dR,dV)))})}
  
  out.df<-as.data.frame(ode(y=init,times=time,func=eqn,parms=parameters))
  return(out.df)
}## end function

##plot!
sirvout<-SIRV.model(t=100,V1=0,Beta=.5,Gamma=.3,Mu=.001) 
## note: can specify both initial vaccinated and mu.
plot(S~time,sirvout,type="l",lwd=3,col="blue",ylim=c(0,1),
     ylab="proportion of population")
lines(I~time,sirvout,lwd=3,col="red")
lines(R~time,sirvout,lwd=3,col="forest green")
lines(V~time,sirvout,lwd=3,col="aquamarine") 
legend("right",legend=c("S","I","R","V"),
       col=c("blue","red","forest green","aquamarine"),
       lty=1,lwd=3,bty="n")

#####################################################
############# Discussion Questions ##################
#####################################################
## 1) Note how V grows linearly with time. Do you think
## that's reasonable?
## 2) Can you think of other ways to model vaccination?
## 3) How would you change the model to do so?
## 4) With the same beta and gamma parameter values, 
## the peak of the outbreak was different with and without
## vaccination... if you run the SIR model with the same parameter values...
wov<-max(sirout$I) ## without vaccination (make sure parameter values are equal)
wv<-max(sirvout$I) ## with vaccination (make sure parameter values are equal)
wov
wv
(wov-wv)/wov ## vaccination results in an estimated __ 
## reduction in peak infections
## 5) What happens if you increase the rate of vaccination 
## (mu) in the model?
## 6) Can you think of other ways to evaluate the effect 
## of vaccination?
#####################################################
#####################################################
