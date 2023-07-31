##dependencies
require(deSolve)
require(shiny)
require(padr)

## read in data from flu in new york
# flu2<-read.csv("https://health.data.ny.gov/api/views/jr8b-6gh6/rows.csv?accessType=DOWNLOAD")
# ##subset the data that we want
# flu<-flu2[flu2$Disease=="INFLUENZA_B" &
#             flu2$County=="NEW YORK" &
#             (flu2$Season=="2022-2023" |
#                flu2$Season=="2021-2022" |
#                flu2$Season=="2020-2021"),]
timepoint<-5
d<-read.csv(paste0("https://raw.githubusercontent.com/jaywl/workshop_23/main/",
                   timepoint),sep="\t")
d$Week.Ending.Date<-as.Date(d$Week.Ending.Date,"%m/%d/%y")
d<-d[order(d$Week.Ending.Date),]
##first, clean up by filling missing data with 0
dat3<-pad(d,interval="week")
dat3$Count[is.na(dat3$Count)]<-0# ##do some cleaning



# dat3<-read.csv("https://raw.githubusercontent.com/jaywl/workshop_23/main/2022topresent_clean.csv")
# dat3$Week.Ending.Date<-as.Date(dat3$Week.Ending.Date,"%m/%d/%Y")
## now, this is our function to calculate the sum of squares. (predicted-observed)^2
ssq<-function(observed,predicted){
  sum((head(predicted$I,length(observed$Count)) - ### note that the function must account for the different lengths in observed and predicted (in sample times + forecast period)
         observed$Count/1726765)^2) ## that big number is the population size!
}

## we'll use a new SIR model that uses whole numbers, rather than proportions
SIR.model.2<-function(t, I1, b, g){
  init<-c(S=1726765,I=I1,R=0)/1726765 ## initial values for compartments, as % population
  parameters<-c(ba=b,ga=g) ## beta and gamma parameters
  time<-seq(1,t,by=7) ## time over which to analyze
  
  eqn<-function(time,state,parameters){
    with(as.list(c(state,parameters)),{
      dS<- -ba*S*I ## dS/dt diff eq
      dI<-ba*S*I-ga*I ## dI/dt diff eq
      dR<-ga*I ## dR/dt diff eq
      return(list(c(dS,dI,dR)))})}
  
  out.df<-as.data.frame(ode(y=init,times=time,func=eqn,parms=parameters))
  return(out.df)
}## end function


## we want to minimize the sum of squares. Let's let the computer do the work!
## we'll iterate across a bunch of combinations of parameter values until we 
## find the set of values with the lowest ssq





##shiny app
ui <- fluidPage(
  radioButtons("cutoff", label = "Data observed through end of (no cheating!)",
               choices=list("December","January","February","March","Present"),
               selected="December",inline=T),
  textInput("beta", label = "If beta is", value = "0", placeholder=paste0("number or ?")),
  textInput("gamma", label = "If gamma is", value = "0", placeholder=paste0("number or ?")),
  textInput("time", label = "Forecast for how many weeks beyond data", value = "0", placeholder=paste0("number of weeks ahead to forecast")),
  "then estimates are: beta = ", textOutput("betout",inline=T), ", gamma = ", textOutput("gamout",inline=T),
  ", R0 = ",textOutput("r0",inline=T),
  ", and ", textOutput("infs",inline=T), "cases on ", textOutput("infdate",inline=T),
  ", and sum of squares = ", textOutput("ssq",inline=T),
  plotOutput("plot", click = "plot_click", )
)
server <- function(input, output, session) {
  
  
  output$plot <- renderPlot({
    if(input$gamma=="?"){
      if(input$beta!="?"){
        gams<-seq(0.001,0.99,by=.001)
      } else {
      gams<-seq(0.001,0.1,by=.001) ## the gamma values to try
      }
    }else{
      gams<-as.numeric(input$gamma)
    }
    if(input$beta=="?"){
      if(input$gamma!="?"){
        bets<-seq(0.001,0.99,by=.001)
        } else {
      bets<-seq(0.001,0.1,by=.001) ## the beta values to try
        }
    }else{
      bets<-as.numeric(input$beta)
    }
    
    if(input$cutoff=="December"){
      dat3<-dat3[dat3$Week.Ending.Date<as.Date("2023-01-01"),]
    }
    if(input$cutoff=="January"){
      dat3<-dat3[dat3$Week.Ending.Date<as.Date("2023-02-01"),]
    }
    if(input$cutoff=="February"){
      dat3<-dat3[dat3$Week.Ending.Date<as.Date("2023-03-01"),]
    }
    if(input$cutoff=="March"){
      dat3<-dat3[dat3$Week.Ending.Date<as.Date("2023-04-01"),]
    }
    if(input$cutoff=="Present"){
      dat3<-dat3
    }
    
    ## find out when the current outbreak started, and how long it has been running
    # start.date<-max(dat3$Week.Ending.Date[dat3$Count==0])
    start.date<-min(dat3$Week.Ending.Date)
    epi.length<-length(dat3$Week.Ending.Date[dat3$Week.Ending.Date>=(start.date)])*7 ## removed start.date+7
    epi.dates<-dat3$Week.Ending.Date[dat3$Week.Ending.Date>=(start.date)] ## same here
    
    ##make an empty matrix to record results
    mymat<-matrix(Inf,nrow=length(gams),ncol=length(bets))
    ## we loop through every combination of gamma and beta 
    for(i in 1:length(bets)){
      for(e in 1:length(gams)){
        if(gams[e]<bets[i]){
        so<-SIR.model.2(t=epi.length+as.numeric(input$time)*7,
                        I1=dat3$Count[dat3$Week.Ending.Date==start.date], ## removed start.date+7 ## initial infected is 7
                        b=bets[i],
                        g=gams[e])
        mymat[e,i]<-ssq(observed=dat3[dat3$Week.Ending.Date>=start.date,], ## equal to start.date
                        predicted=so)
        } ## throwing an error under certain conditions... ?
        
      }
    }
    
    ## and find the lowest ssq
    best.fit<-which(mymat==min(mymat),arr.ind=T)
    best.ssq<-mymat[best.fit]
    ##record our best-fit values
    gam.fit<-gams[best.fit[1]]
    bet.fit<-bets[best.fit[2]]
    output$gamout<-renderText({gam.fit})
    output$betout<-renderText({bet.fit})
    output$r0<-renderText({round(bet.fit/gam.fit,2)})
    sirout<-SIR.model.2(t=epi.length+as.numeric(input$time)*7,
                I1=dat3$Count[dat3$Week.Ending.Date==start.date], ## removed start.date+7 here
                b=bet.fit,g=gam.fit)
    # output$totinfs<-renderText({round(max(
    #   sirout$R*1726765))})
    output$infs<-renderText({round(tail(sirout$I*1726765,1))})
    output$infdate<-renderText({as.character(as.Date(
      max(epi.dates)+7*as.numeric(input$time)))})
    output$ssq<-renderText({best.ssq})
    
    plot(y=dat3$Count[dat3$Week.Ending.Date>=start.date], ## ewual to start date
         x=dat3$Week.Ending.Date[dat3$Week.Ending.Date>=start.date],## ewual to start date
         col="black",pch=16,las=1,
         ylab="infections",
         ylim=c(min(c(0,
                      sirout$I*1726765)),
                max(c(dat3$Count[dat3$Week.Ending.Date>=start.date],## ewual to start date
                      sirout$I*1726765))),
         xlim=c(start.date,max(epi.dates)+7*as.numeric(input$time)),
         xlab="",
         xaxt="n"
    )
    points(y=sirout$I*1726765,
           x=seq.Date(start.date,max(epi.dates+7*as.numeric(input$time)),by=7), ## removed start.date+7
           col="dark green")
    lines(y=sirout$I*1726765,
          x=seq.Date(start.date,max(epi.dates+7*as.numeric(input$time)),by=7),## removed start.date+7
          col="dark green")
    rect(xleft=as.Date(max(epi.dates)),
         ybottom=-10,
         ytop=max(c(sirout$I*1726765,dat3$Count[dat3$Week.Ending.Date>=start.date]))+10,
         xright=as.Date(max(epi.dates+7*as.numeric(input$time))),
         col = rgb(0.5,0.5,0.5,1/8),border=rgb(0.5,0.5,0.5,1/8))
    axis.Date(1, at = seq.Date(start.date,max(epi.dates)+7*as.numeric(input$time),by=14),## removed start.date+7
              las=2,format="%m/%d/%y")
    legend("topleft",legend=c("observed","simulated","forecast region"),
           col=c("black","forest green",rgb(0.5,0.5,0.5,1/8)),
           pch=c(16,1,15),bty="n")

  }, res = 96)
}

shinyApp(ui, server)


