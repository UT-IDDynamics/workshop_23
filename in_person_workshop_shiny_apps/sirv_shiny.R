##dependencies
require(deSolve)
require(shiny)
## sirv model
SIRV.model<-function(t, V1, b, g, m){
  init<-c(S=1-0.001-V1,I=0.001,R=0.0,V=V1) ## initial values for compartments
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
  sliderInput("time", label = "Evaluated over this much time", min = 0, max = 150, step=1, value = 20),
  sliderInput("V1",label = "Initial proportion of population vaccinated is", min = 0, max = 1, step=0.01, value = 0.0),
  sliderInput("mu",label = "If mu (per-time vaccination rate) is", min = 0, max = .01, step=0.0001, value = 0.0),
  "then R0 = ", textOutput("r0",inline=T),
  "and total infected = ", textOutput("infs",inline=T), "% of population",
  
  plotOutput("plot", click = "plot_click", )
)
server <- function(input, output, session) {
  
  
  observeEvent(input$plot_click,
               dist(nearPoints(df, input$plot_click, allRows = TRUE, addDist = TRUE)$dist_)  
  )
  # output$R0<-renderText(round(input$beta/input$gamma,3))
  output$infs<-renderText(round(SIRV.model(t=input$time,V1=input$V1,b=input$beta,g=input$gamma,m=input$mu)$R[
    length(SIRV.model(t=input$time,V1=input$V1,b=input$beta,g=input$gamma,m=input$mu)$R)],3)
    * 100)
  output$r0<-renderText(as.character(round(as.numeric(input$beta)/as.numeric(input$gamma),2)))
  output$plot <- renderPlot({
    sirvout<-SIRV.model(t=input$time,V1=input$V1,b=input$beta,g=input$gamma,m=input$mu)
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


