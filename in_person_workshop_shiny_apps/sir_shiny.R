##dependencies
require(deSolve)
require(shiny)
##basic SIR model
SIR.model<-function(t, b, g){
  init<-c(S=.999,I=.001,R=0)
  parameters<-c(ba=b,ga=g)
  time<-seq(0,t,by=1)#t/(2*length(1:t)))
  
  # eqn<-function(time,state,parameters){
  #   with(as.list(c(state,parameters)),{
  #     dS<- -ba*S*I
  #     dI<-ba*S*I-ga*I
  #     dR<-ga*I
  #     return(list(c(dS,dI,dR)))})}
  
  eqn<-function(time,state,parameters){
    with(as.list(c(state,parameters)),{
      dS<- -ba*S*I
      dI<-ba*S*I-ga*I
      dR<-ga*I
      return(list(c(dS,dI,dR)))})}
  
  out.df<-as.data.frame(ode(y=init,times=time,func=eqn,parms=parameters))
  return(out.df)
}## end function

##shiny app
ui <- fluidPage(
  sliderInput("beta", label = "If beta is", min = 0, max = 1, step=.01, value = .5),
  sliderInput("gamma", label = "If gamma is", min = 0, max = 1, step=.01, value = .5),
  sliderInput("time", label = "Evaluated over this much time", min = 0, max = 150, step=1, value = 20),
  "then R0 = ", textOutput("R0",inline=T), "and total infected = ", textOutput("infs",inline=T), "% of population",

  plotOutput("plot", click = "plot_click", )
)
server <- function(input, output, session) {
  
  
  observeEvent(input$plot_click,
               dist(nearPoints(df, input$plot_click, allRows = TRUE, addDist = TRUE)$dist_)  
  )
  output$R0<-renderText(round(input$beta/input$gamma,3))
  output$infs<-renderText(round(SIR.model(t=input$time,b=input$beta,g=input$gamma)$R[
                          length(SIR.model(t=input$time,b=input$beta,g=input$gamma)$R)],3)
                          * 100 )
  output$plot <- renderPlot({
    sirout<-SIR.model(t=input$time,b=input$beta,g=input$gamma)
    plot(S~time,sirout,type="l",lwd=3,col="blue",ylim=c(0,1),
         ylab="proportion of population")
    lines(I~time,sirout,lwd=3,col="red")
    lines(R~time,sirout,lwd=3,col="forest green")
    legend("right",legend=c("S","I","R"),
           col=c("blue","red","forest green"),
           lty=1,lwd=3,bty="n")
 
     }, res = 96)
}

shinyApp(ui, server)


