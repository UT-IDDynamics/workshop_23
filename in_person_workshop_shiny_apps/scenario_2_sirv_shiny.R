##dependencies
require(deSolve)
require(shiny)
## sirv model
SIRV.model<-function(t, V1, I1, b, g, m){
  init<-c(S=1726765-(V1*1726765),I=I1,R=0.0,V=V1*1726765)/1726765 ## initial values for compartments
  parameters<-c(ba=b,ga=g,mu=m) ## beta, gamma, and mu parameters
  time<-seq(0,t,by=1) ## time over which to analyze
  
  eqn<-function(time,state,parameters){
    with(as.list(c(state,parameters)),{
      dS<- -ba*S*I-mu ## dS/dt diff eq. 
      ## Note individuals in S compartment are removed via vaccination rate mu.
      dI<-ba*S*I-ga*I ## dI/dt diff eq
      dR<-ga*I ## dR/dt diff eq
      dV<-mu ## DV/dt diff eq. mu is the rate of vaccine delivery per unit time.
      return(list(c(dS,dI,dR,dV)))})}
  
  out.df<-as.data.frame(ode(y=init,times=time,func=eqn,parms=parameters))
  return(out.df)
}## end function

##shiny app
ui <- fluidPage(
  sliderInput("beta", label = "If beta is", min = 0, max = 1, step=.01, value = .5),
  sliderInput("gamma", label = "If gamma is", min = 0, max = 1, step=.01, value = .5),
  textInput("Iinit",label = "Initial infections = ", value = 7,placeholder="number of infections"),
  sliderInput("time", label = "Evaluated over this many days", min = 0, max = 210, step=7, value = 21),
  sliderInput("V1",label = "Initial proportion of population vaccinated is", min = 0, max = 1, step=0.01, value = 0.0),
  sliderInput("mu",label = "If mu (per-time vaccination rate) is", min = 0, max = .001, step=0.000001, value = 0.0),
  "then R0 = ", textOutput("r0",inline=T),
  ", total people infected = ", textOutput("infs",inline=T), 
  ", and vaccine doses given = ", textOutput("vaxs",inline=T),
  
  plotOutput("plot", click = "plot_click", )
)
server <- function(input, output, session) {
  
  
  output$infs<-renderText(round(SIRV.model(t=input$time,V1=input$V1,I1=as.numeric(input$Iinit),b=input$beta,g=input$gamma,m=input$mu)$R[
    length(SIRV.model(t=input$time,V1=input$V1,I1=as.numeric(input$Iinit),b=input$beta,g=input$gamma,m=input$mu)$R)]
    * 1726765)) ## the big number is the population size
  output$r0<-renderText(as.character(round(as.numeric(input$beta)/as.numeric(input$gamma),2)))
  output$vaxs<-renderText(as.character(round(round(max(SIRV.model(t=input$time,V1=input$V1,I1=as.numeric(input$Iinit),b=input$beta,g=input$gamma,m=input$mu)$V)* 1726765 )-
                                               round(min(SIRV.model(t=input$time,V1=input$V1,I1=as.numeric(input$Iinit),b=input$beta,g=input$gamma,m=input$mu)$V)* 1726765))
                                         ))
  output$plot <- renderPlot({
    sirvout<-SIRV.model(t=input$time,V1=input$V1,I1=as.numeric(input$Iinit),b=input$beta,g=input$gamma,m=input$mu)
    plot(S~time,sirvout,type="l",lwd=3,col="blue",ylim=c(0,1),
         ylab="proportion of population")
    lines(I~time,sirvout,lwd=3,col="red")
    lines(R~time,sirvout,lwd=3,col="forest green")
    lines(V~time,sirvout,lwd=3,col="aquamarine") 
    legend("right",legend=c("S","I","R","V"),
           col=c("blue","red","forest green","aquamarine"),
           lty=1,lwd=3,bty="n")
    
  }, res = 96)
}

shinyApp(ui, server)


