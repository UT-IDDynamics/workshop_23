
#### GAM model
require(padr)
require(mgcv)

## read in data from flu in new york
timepoint<-999## update this value to move forward in time!
## 1 = end of December
## 2 = end of January
## 3 = end of February
## 4 = end of March
## 5 = present
## 999 for last 3 seasons
d<-read.csv(paste0("https://raw.githubusercontent.com/jaywl/workshop_23/main/",timepoint),sep="\t")
## do some cleaning
d$Week.Ending.Date<-as.Date(d$Week.Ending.Date,"%m/%d/%y")
d<-d[order(d$Week.Ending.Date),]
d<-d[d$Season=="2022-2023",]
dat3<-pad(d,interval="week")
dat3$Count[is.na(dat3$Count)]<-0


##take a look
plot(Count~Week.Ending.Date,dat3)

## fit a GAM model to the timeseries data
gfit<-gam(Count~s(CDC.Week),data=dat3[dat3$Season=="2022-2023",])
## gfit object contains lots of info on how the model was fit
summary(gfit)
plot(gfit$residuals) ## etc...

## This checks the fit visually
gam.check(gfit)


## and make a forecast for the next __ weeks
nweeks<-20 ## (must be >0 for the below plot to work)
nd<-c(dat3$CDC.Week[dat3$Season=="2022-2023"],
      seq(from=tail(dat3$CDC.Week[dat3$Season=="2022-2023"],1)+1,
          tail(dat3$CDC.Week[dat3$Season=="2022-2023"],1)+(nweeks)))
ndd<-data.frame(CDC.Week=nd,Count=NA)
pred.gam<-predict(gfit,newdata=ndd, se.fit = TRUE)
## take a look. Black points are the real data, red are forecast
plot(dat3$Count~dat3$Week.Ending.Date,
     xlim=c(min(dat3$Week.Ending.Date[dat3$Season=="2022-2023"]),max(dat3$Week.Ending.Date)+nweeks*7),
     ylim=c(0,max(c(pred.gam$fit,dat3$Count))),
     ylab="cases",xlab="Week",
     col="black",pch=16,lwd=2)
lines(y=as.vector(pred.gam$fit),
      x=seq.Date(min(dat3$Week.Ending.Date[dat3$Season=="2022-2023"]),max(dat3$Week.Ending.Date)+nweeks*7,by=7),
      col="dark red",lwd=2)
lines(y=as.vector(pred.gam$fit),
      x=seq.Date(min(dat3$Week.Ending.Date[dat3$Season=="2022-2023"]),max(dat3$Week.Ending.Date)+nweeks*7,by=7),
      col="dark red")
lines(y=(pred.gam$fit - 2*pred.gam$se.fit),
      x=seq.Date(min(dat3$Week.Ending.Date[dat3$Season=="2022-2023"]),max(dat3$Week.Ending.Date)+nweeks*7,by=7),
      col="red",lwd=2,lty=2)
lines(y=as.vector(pred.gam$fit + 2*pred.gam$se.fit),
      x=seq.Date(min(dat3$Week.Ending.Date[dat3$Season=="2022-2023"]),max(dat3$Week.Ending.Date)+nweeks*7,by=7),
      col="red",lwd=2,lty=2)
rect(xleft=as.Date(max(dat3$Week.Ending.Date[dat3$Season=="2022-2023"])),
     ybottom=-10,
     ytop=max(as.vector(pred.gam$fit))+10,
     xright=as.Date(max(dat3$Week.Ending.Date[dat3$Season=="2022-2023"])+nweeks*7),
     col = rgb(0.5,0.5,0.5,1/8),border=rgb(0.5,0.5,0.5,1/8))
legend("left",legend=c("data","model", "95% CI", "forecast region"),
       col=c("black","dark red", "red",rgb(0.5,0.5,0.5,1/8)),
       lwd=3,lty=c(1,1,2,1),bty="n")


#####################################################
############# Discussion Questions ##################
#####################################################
## 1) What do you think about the GAM forecast? is it
## reasonable? Why or why not?
## 2) Change the forecast length (nweeks) to a higher 
## number. Say... 20 weeks. Run the model again.
## 3) do you think the forecast is reasonable now? 
## Why or why not?
## 
#####################################################
#####################################################
