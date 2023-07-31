##dependencies
require(mgcv)
require(shiny)
require(padr)
## gam model

##shiny app
ui <- fluidPage(
  radioButtons("cutoff", label = "Data observed through end of (no cheating!)",
               choices=list("December","January","February","March","Present"),
               selected="December",inline=T),
  sliderInput("nweeks", label = "If forecast is __ weeks ahead", min = 1, max = 20, step=1, value = 1),
  checkboxInput("CIs",label = "Check to use 95% confidence intervals",value=F ),
  textOutput("infs",inline=T), "cases on ", textOutput("infdate",inline=T),
  plotOutput("plot", click = "plot_click", )
)
server <- function(input, output, session) {
  
  
  # observeEvent(input$plot_click,
  #              dist(nearPoints(df, input$plot_click, allRows = TRUE, addDist = TRUE)$dist_)  
  # )

  output$plot <- renderPlot({
    nweeks<-input$nweeks
    if(input$cutoff=="December"){
      d<-read.csv(paste0("https://raw.githubusercontent.com/jaywl/workshop_23/main/",999),sep="\t")
      d$Week.Ending.Date<-as.Date(d$Week.Ending.Date,"%m/%d/%y")
      d<-d[d$Week.Ending.Date<as.Date("2023-01-01") & d$Season=="2022-2023",]
      d<-d[order(d$Week.Ending.Date),]
      dat3<-pad(d,interval="week")
      dat3$Count[is.na(dat3$Count)]<-0
    }
    if(input$cutoff=="January"){
      d<-read.csv(paste0("https://raw.githubusercontent.com/jaywl/workshop_23/main/",999),sep="\t")
      d$Week.Ending.Date<-as.Date(d$Week.Ending.Date,"%m/%d/%y")
      d<-d[d$Week.Ending.Date<as.Date("2023-02-01")&d$Season=="2022-2023",]
      d<-d[order(d$Week.Ending.Date),]
      dat3<-pad(d,interval="week")
      dat3$Count[is.na(dat3$Count)]<-0
      }
    if(input$cutoff=="February"){
      d<-read.csv(paste0("https://raw.githubusercontent.com/jaywl/workshop_23/main/",999),sep="\t")
      d$Week.Ending.Date<-as.Date(d$Week.Ending.Date,"%m/%d/%y")
      d<-d[d$Week.Ending.Date<as.Date("2023-03-01")&d$Season=="2022-2023",]
      d<-d[order(d$Week.Ending.Date),]
      dat3<-pad(d,interval="week")
      dat3$Count[is.na(dat3$Count)]<-0
      }
    if(input$cutoff=="March"){
      d<-read.csv(paste0("https://raw.githubusercontent.com/jaywl/workshop_23/main/",999),sep="\t")
      d$Week.Ending.Date<-as.Date(d$Week.Ending.Date,"%m/%d/%y")
      d<-d[d$Week.Ending.Date<as.Date("2023-04-01")&d$Season=="2022-2023",]
      d<-d[order(d$Week.Ending.Date),]
      dat3<-pad(d,interval="week")
      dat3$Count[is.na(dat3$Count)]<-0
      }
    if(input$cutoff=="Present"){
      d<-read.csv(paste0("https://raw.githubusercontent.com/jaywl/workshop_23/main/",999),sep="\t")
      d$Week.Ending.Date<-as.Date(d$Week.Ending.Date,"%m/%d/%y")
      d<-d[d$Week.Ending.Date<as.Date("2023-08-01")&d$Season=="2022-2023",]
      d<-d[order(d$Week.Ending.Date),]
      dat3<-pad(d,interval="week")
      dat3$Count[is.na(dat3$Count)]<-0
      }
    ## fit a GAM model to the timeseries data
    gfit<-gam(Count~s(CDC.Week),data=dat3[dat3$Season=="2022-2023",])
    nd<-c(dat3$CDC.Week[dat3$Season=="2022-2023"],
          seq(from=tail(dat3$CDC.Week[dat3$Season=="2022-2023"],1)+1,
              tail(dat3$CDC.Week[dat3$Season=="2022-2023"],1)+(nweeks)))
    ndd<-data.frame(CDC.Week=nd,Count=NA)
    pred.gam<-predict(gfit,newdata=ndd, se.fit = TRUE)
    
    # plot(dat3$Count~dat3$Week.Ending.Date,
    #      xlim=c(min(dat3$Week.Ending.Date[dat3$Season=="2022-2023"]),max(dat3$Week.Ending.Date)+nweeks*7),
    #      ylim=c(0,max(c(as.vector(pred.gam$fit),dat3$Count))),
    #      ylab="cases",xlab="Week",
    #      col="black",pch=16)
    plot(dat3$Count~dat3$Week.Ending.Date,
         xlim=c(min(dat3$Week.Ending.Date[dat3$Season=="2022-2023"]),max(dat3$Week.Ending.Date)+nweeks*7),
         ylim=c(0,max(c(as.vector(pred.gam$fit),dat3$Count))),
         ylab="cases",xlab="",xaxt="n",
         col="black",pch=16)
    lines(y=as.vector(pred.gam$fit),
           x=seq.Date(min(dat3$Week.Ending.Date[dat3$Season=="2022-2023"]),max(dat3$Week.Ending.Date)+nweeks*7,by=7),
           col="dark red",lwd=2)
    points(y=as.vector(pred.gam$fit),
          x=seq.Date(min(dat3$Week.Ending.Date[dat3$Season=="2022-2023"]),max(dat3$Week.Ending.Date)+nweeks*7,by=7),
          col="dark red")
    if(input$CIs==T){
      lines(y=(pred.gam$fit - 2*pred.gam$se.fit),
          x=seq.Date(min(dat3$Week.Ending.Date[dat3$Season=="2022-2023"]),max(dat3$Week.Ending.Date)+nweeks*7,by=7),
          col="red",lwd=2,lty=2)
      lines(y=as.vector(pred.gam$fit + 2*pred.gam$se.fit),
          x=seq.Date(min(dat3$Week.Ending.Date[dat3$Season=="2022-2023"]),max(dat3$Week.Ending.Date)+nweeks*7,by=7),
          col="red",lwd=2,lty=2)
    }
    rect(xleft=as.Date(max(dat3$Week.Ending.Date[dat3$Season=="2022-2023"])),
         ybottom=-10,
         ytop=max(as.vector(pred.gam$fit))+10,
         xright=as.Date(max(dat3$Week.Ending.Date[dat3$Season=="2022-2023"])+nweeks*7),
         col = rgb(0.5,0.5,0.5,1/8),border=rgb(0.5,0.5,0.5,1/8))
    
    axis.Date(1, at = seq.Date(min(dat3$Week.Ending.Date[dat3$Season=="2022-2023"]),max(dat3$Week.Ending.Date)+nweeks*7,by=14),## removed start.date+7
              las=2,format="%m/%d/%y")
    
    legend("left",legend=c("data","model", "95% CI", "forecast region"),
           col=c("black","dark red", "red",rgb(0.5,0.5,0.5,1/8)),
           lwd=3,lty=c(1,1,2,1),bty="n")

    output$infs<-renderText({round(as.numeric(as.vector(tail(pred.gam$fit,1))))})
    output$infdate<-renderText({as.character(as.Date(max(dat3$Week.Ending.Date)+(nweeks)*7))})
  }, res = 96)
}

shinyApp(ui, server)


