rm(list=ls())
library(forecast)
library(corrplot)
library(lattice)
library(zoo)
library(stats)
library(leaps)
library(httr)
library(pls)
library(HH)
library(scales)


# LoadForecast<-function(cityName){
#   city<-cityName
 
  city<-"Berlin-MD"
  ct<-chartr(" ", ".", city)
  ct<-chartr("-", ".", ct)
  
  #Get the historical load data(monthly)
  data<-read.csv("historical load.csv",header=TRUE,sep=",")
  ncol<-which(colnames(data)==ct)
  load<-as.character(data[,ncol])
  for (i in 1:length(load)){
    load[i]<-gsub('\\,', '', load[i])
  }
  load<-as.numeric(load)
  isna<-which(is.na(load))
  
  if (length(isna)!=0){
    start<-as.numeric(which.max(as.numeric(as.character(isna))))
    WhichYear.start<-2007+round(start/12,0)
    load<-load[(start+1):length(load)]
    WhichYear.end<-WhichYear.start+round(length(load)/12,0)-1
    yearsY<-seq(WhichYear.start,WhichYear.end,by=1)
    yearsM<-seq(WhichYear.start,WhichYear.end+1,length=12*length(yearsY))
    ts<-ts(load[1:length(yearsM)],start=c(WhichYear.start,1),end=c(WhichYear.end,12),frequency=12)
    
    load.bar<-c()
    
    for(i in yearsY){
      load.bar<-c(load.bar,mean(window(ts, start=c(i,1), end=c(i+1,0))))
    }
    
    load.dot<-c()
    
    for(i in yearsY){
        load.dot<-c(load.dot,window(ts, start=c(i,1), end=c(i+1,0))/load.bar[i-WhichYear.start+1])
        }
    
    } else{
    yearsY<-seq(2007,2016,by=1)
    yearsM<-seq(2007,2017,length=120)
    ts<-ts(as.numeric(load),start=c(2007,1),end=c(2016,12),frequency=12)
    # Yearly mean load
    load.bar<-c(mean(window(ts, start=c(2007,1), end=c(2008,0))),mean(window(ts, start=c(2008,1), end=c(2009,0))),mean(window(ts, start=c(2009,1), end=c(2010,0))),
                mean(window(ts, start=c(2010,1), end=c(2011,0))),mean(window(ts, start=c(2011,1), end=c(2012,0))),mean(window(ts, start=c(2012,1), end=c(2013,0))),
                mean(window(ts, start=c(2013,1), end=c(2014,0))),mean(window(ts, start=c(2014,1), end=c(2015,0))),mean(window(ts, start=c(2015,1), end=c(2016,0))),
                mean(window(ts, start=c(2016,1), end=c(2017,0))))
    
    # Monthly normalized load
    load.dot<-c(window(ts, start=c(2007,1), end=c(2008,0))/load.bar[1],window(ts, start=c(2008,1), end=c(2009,0))/load.bar[2],window(ts, start=c(2009,1), end=c(2010,0))/load.bar[3],
                window(ts, start=c(2010,1), end=c(2011,0))/load.bar[4],window(ts, start=c(2011,1), end=c(2012,0))/load.bar[5],window(ts, start=c(2012,1), end=c(2013,0))/load.bar[6],
                window(ts, start=c(2013,1), end=c(2014,0))/load.bar[7],window(ts, start=c(2014,1), end=c(2015,0))/load.bar[8],window(ts, start=c(2015,1), end=c(2016,0))/load.bar[9],
                window(ts, start=c(2016,1), end=c(2017,0))/load.bar[10])
  }
  
  #Do a normalization of monthly load data

  load.dot<-log(load.dot)
  load.bar<-log(load.bar)

  par(mfrow=c(3,1))
  plot(log(ts),main="Monthly Historical load",ylab="Load")
  plot(yearsM,load.dot,type="b",main="Monthly normalized load",ylab="Load",xlab="Time")
  plot(yearsY,load.bar,type="b",main="Yearly average load",ylab="Load",xlab="Time")
  par(mfrow=c(1,1))
  
  #Get historical weather data(monthly)
  AMP.Member<-read.csv("city info.csv",header=TRUE,sep=",")
  rowindex<-which(AMP.Member["City"]==city)
  airport<-AMP.Member[rowindex,3]
  
  year<-yearsY
  month<-seq(1,12,by=1)
  
  maxTemp<-matrix(0,nrow=length(year),ncol=12)
  meanTemp<-matrix(0,nrow=length(year),ncol=12)
  minTemp<-matrix(0,nrow=length(year),ncol=12)
  meanHumi<-matrix(0,nrow=length(year),ncol=12)
  monCDD<-matrix(0,nrow=length(year),ncol=12)
  monHDD<-matrix(0,nrow=length(year),ncol=12)
  
  for(i in 1:length(year)){
    for(j in 1:length(month)){
      
      url<-paste0("https://www.wunderground.com/history/airport/",airport,"/",year[i],"/",month[j],"/1/MonthlyHistory.html?")
      
      req<-GET(url)
      dat<-content(req, "text")
      
      myStr = strsplit(dat,"Daily Weather History &amp; Observations")[[1]][2]
      myStr = strsplit(myStr,"background-color: #FFF")[[1]][2]
      myStr = strsplit(myStr,"script type=")[[1]][1]
      myStr = gsub("\n|\r|\t|<tr>|</tr>|</td>|</tbody>|<tbody>","",myStr)
      myStr = gsub("Rain,|Snow,|Fog,|Thunderstorm,|Hail,|Tornado,","",myStr)
      myStr = gsub("Rain|Snow|Fog|Thunderstorm|Hail|Tornado|&nbsp;","",myStr)
      myStr = gsub('<span class="wx-value">|</span>',"",myStr)
      myStr = gsub("<.+?>", ",", myStr)
      myStr = substring(myStr,6,nchar(myStr)-6) 
      myStr = gsub(",,,","\n",myStr)
    
      weather<-read.csv(textConnection(myStr), header = F)
      colnames(weather)<-c("ForecastDay","Empty","TempHigh","TempAvg","TempLow","DewHigh","DewAvg","DewLow","HumidHigh","HumidAvg","HumidLow","SeaLevelHigh","SeaLevelAvg","SeaLevelLow","VisibilityHigh","VisibilityAvg","VisibilityLow","WindHigh","WindAvg","WindLow","Precip")  

      maxTemp[i,j]<-mean(na.omit(as.numeric(as.character(as.matrix(weather["TempHigh"])))))   #Monthly Maximum Temperature
      meanTemp[i,j]<-mean(na.omit(as.numeric(as.character(as.matrix(weather["TempAvg"]))))) #Monthly mean Temperature
      minTemp[i,j]<-mean(na.omit(as.numeric(as.character(as.matrix(weather["TempLow"])))))   #Monthly Minimum TemperatureF
      meanHumi[i,j]<-mean(na.omit(as.numeric(as.character(as.matrix(weather["HumidAvg"])))))     #Monthly Mean Humidity
      incdd<-as.numeric(as.character(as.matrix(weather["TempAvg"])))-65
      inhdd<-55-as.numeric(as.character(as.matrix(weather["TempAvg"])))
      monCDD[i,j]<-sum(incdd[which(incdd > 0)])
      monHDD[i,j]<-sum(inhdd[which(inhdd > 0)])
      
    }
  }
  
  maxTemp<-as.vector(t(maxTemp))
  meanTemp<-as.vector(t(meanTemp))
  minTemp<-as.vector(t(minTemp))
  meanHumi<-as.vector(t(meanHumi))
  monCDD<-as.vector(t(monCDD))
  monHDD<-as.vector(t(monHDD))
  load.dot<-as.vector(t(load.dot))
  weather.Model<-data.frame(load.dot,maxTemp,meanTemp,minTemp,meanHumi,monCDD,monHDD)
  
  plot(yearsM,maxTemp,type="l",col="red",main="Historical weather data",xlab="Time")
  lines(yearsM,meanTemp,type="l")
  lines(yearsM,minTemp,type="l",col="blue")
  legend("topright",c("Maximum Temperature","Mean Temperature","Minimum Temperature"),col=c("red","black","blue"),lty=c(1,1,1),bty="n")

  par(mfrow=c(2,1))
  plot(yearsM,monHDD,type="l",col="red",main="Historical HDD",xlab="Time")
  plot(yearsM,monCDD,type="l",col="blue",main="Historical CDD",xlab="Time")
  par(mfrow=c(1,1))

  # Fit weather-demand sub-model
  
  tryfit.weather<-plsr(load.dot~.,data=weather.Model,validation="CV")
  ncomp0<-which.min(tryfit.weather$validation$adj)
  fit.weather<-tryfit.weather$fitted.values[,,ncomp0]

   plot(yearsM,load.dot,type="l",xlab="Year",ylab="Yearly load")
   lines(yearsM,fit.weather,type="b",col="red")
   legend("topright",c("original data","fitted data"),col=c("black","red"),lty=c(1,1),bty="n")
  
  temp<-matrix(0,nrow=12,ncol=6)
  
  for(m in 1:12){
    for(n in 1:length(yearsY)){
      temp[m,]<-temp[m,]+as.matrix(weather.Model)[,-1][12*(n-1)+m,]
    }
  }
  
  temp<-temp/length(yearsY)
  
  new.weather<-rbind(temp,temp,temp,temp,temp,temp,temp,temp,temp,temp)
  colnames(new.weather)<-c("maxTemp","meanTemp","minTemp","meanHumi","monCDD","monHDD")
  
  par(mfrow=c(4,1))

  plot(meanTemp,type="l",main="Historical meanTemp",xlab="Time")
  lines(new.weather[,"meanTemp"],col="red")
  legend("topright",c("Average meanTemp","hitorical meanTemp"),col=c("red","black"),lty=c(1,1),bty="n")

  plot(meanHumi,type="l",main="Historical meanHumi",xlab="Time")
  lines(new.weather[,"meanHumi"],col="red")
  legend("topright",c("Average meanHumi","hitorical meanHumi"),col=c("red","black"),lty=c(1,1),bty="n")

  plot(monHDD,type="l",main="Historical HDD",xlab="Time")
  lines(new.weather[,"monHDD"],col="red")
  legend("topright",c("Average monHDD","hitorical monHDD"),col=c("red","black"),lty=c(1,1),bty="n")

  plot(monCDD,type="l",main="Historical CDD",xlab="Time")
  lines(new.weather[,"monCDD"],col="red")
  legend("topright",c("Average monCDD","hitorical monCDD"),col=c("red","black"),lty=c(1,1),bty="n")

  par(mfrow=c(1,1))

  # Predict future weather
  predict.Weather<-predict(tryfit.weather,new.weather,ncomp=ncomp0)
  wholeTime<-seq(WhichYear.start,WhichYear.end+10,length=length(yearsM)+120)
  
  plot(wholeTime,c(load.dot,predict.Weather),xlab="Time",ylab="Monthly Load",main="monthly load forecast",type="l")
  lines(yearsM,fit.weather,type="b",col="red")
  abline(v=2017,col="red",lty=2)

  #Get historical eco data(yearly)
  
  file.Name<-as.character(AMP.Member["W.P.Lookup"][rowindex,])
  file.Name<-gsub("[[:space:]]", "",paste("/",substr(file.Name,3,4),"/",substr(file.Name,6,nchar(file.Name)-1)))
  file.Name<-paste("S:/Load Forecasting/Econometric/WoodsPoole2017",file.Name,".csv",sep="")
  
  ecoData<-read.csv(file.Name,header=TRUE,sep=",")#Search the targeting file  MUST BE CSV FORMAT
  
  totalPopulation<-as.numeric(t(ecoData[3,-1]))
  totalEmployment<-as.numeric(t(ecoData[34,-1]))
  totalHouseholds<-as.numeric(t(ecoData[99,-1]))
  totalRetail<-as.numeric(t(ecoData[111,-1]))
  time<-as.character(t(ecoData[2,-1]))
  eco.Data<-data.frame(time,totalPopulation,totalEmployment,totalHouseholds,totalRetail)
  
  start<-which(time=="2007")
  end<-which(time=="2016")
  hiseco.Data<-eco.Data[start:end,]
  
  par(mfrow=c(2,2))
  plot(seq(2007,2016,by=1),hiseco.Data[,2],type="b",main="Total Population",ylab="Population",xlab="Time")
  plot(seq(2007,2016,by=1),hiseco.Data[,3],type="b",main="Total Employment",ylab="Employment",xlab="Time")
  plot(seq(2007,2016,by=1),hiseco.Data[,4],type="b",main="Total Households",ylab="Housholds",xlab="Time")
  plot(seq(2007,2016,by=1),hiseco.Data[,5],type="b",main="Total Retail sales",ylab="Retail sales",xlab="Time")
  par(mfrow=c(1,1))

  corrplot.mixed(cor(hiseco.Data[,2:5]),upper="ellipse")
  
  # Fit Yealy eco model (multivariate linear model)
  eco.model<-data.frame(load.bar,hiseco.Data[,2:5])
  colnames(eco.model)<-c("yearlyLoad","totalPopulation","totalEmployment","totalHouseholds","totalRetail")
  
  tryfit.eco<-plsr(yearlyLoad~.,data=eco.model,validation="CV")
  ncomp<-which.min(tryfit.eco$validation$adj)
  fit.eco<-tryfit.eco$fitted.values[,,ncomp]
  
  plot(seq(2007,2016,length=10),load.bar,type="b",xlab="Year",ylab="Yearly load")
  lines(seq(2007,2016,length=10),fit.eco,type="b",col="red")
  legend("topright",c("original data","fitted data"),col=c("black","red"),lty=c(1,1),bty="n")

  # Get future eco data
  start.whole<-which(time=="2007")
  end.whole<-which(time=="2026")
  Wholeco.Data<-eco.Data[start.whole:end.whole,]
  
  par(mfrow=c(2,2))
  plot(seq(2007,2026,by=1),Wholeco.Data[,2],xaxt = "n",type="b",main="Future Total Population",ylab="Population",xlab="Time")
  axis(side = 1, seq(2007,2026,by=1))
  abline(v=2016,col="red",lty=2)
  plot(seq(2007,2026,by=1),Wholeco.Data[,3],xaxt = "n",type="b",main="Future Total Employment",ylab="Employment",xlab="Time")
  axis(side = 1, seq(2007,2026,by=1))
  abline(v=2016,col="red",lty=2)
  plot(seq(2007,2026,by=1),Wholeco.Data[,4],xaxt = "n",type="b",main="Future Total Households",ylab="Housholds",xlab="Time")
  axis(side = 1, seq(2007,2026,by=1))
  abline(v=2016,col="red",lty=2)
  plot(seq(2007,2026,by=1),Wholeco.Data[,5],xaxt = "n",type="b",main="Future Total Retail sales",ylab="Retail sales",xlab="Time")
  axis(side = 1, seq(2007,2026,by=1))
  abline(v=2016,col="red",lty=2)
  par(mfrow=c(1,1))
  
  start<-which(time=="2017")
  end<-which(time=="2026")
  new.Data<-eco.Data[start:end,]
  
  predictedLoad<-predict(tryfit.eco,new.Data,ncomp=ncomp)
  
  # Predict future load trend]
  
  normalize<-fit.eco[10]-load.bar[10]
  predictedLoad<-predictedLoad-normalize
  
  plot(seq(2007,2026,by=1),c(load.bar,predictedLoad),xlab="Time",ylab="Yearly Load",main="Yearly load forecast")
  lines(seq(2007,2016,by=1),fit.eco,type="b",col="red")
  abline(v=2016,col="red",lty=2)

  # Fit monthly weather model
  
  pre.historical<-c()
  for(i in 1:10){
    pre.historical<-c(pre.historical,fit.weather[(12*(i-1)+1):(12*i)]+fit.eco[i])
  }
  
  pre.historical<-exp(pre.historical)
  
  plot(ts,type="l")
  lines(seq(2007,2017,length=120),pre.historical,col="red",type="l")
  legend("topright",c("original data","fitted data"),col=c("black","red"),lty=c(1,1),bty="n")
  
  pre<-c()
  for(i in 1:10){
    pre<-c(pre,predict.Weather[(12*(i-1)+1):(12*i)]+predictedLoad[i])
  }
  
  pre<-exp(pre)

  plot(seq(2017,2027,length=120),pre,type="l",col="red",xlab="Time",ylab="Load KWH",main="Load Forecast from 2017 to 2026")

  wholedata<-ts(c(ts,pre),start=c(2007,1),end=c(2026,12),frequency=12)
  
  plot(seq(2007,2027,length=240),wholedata,ylab="Load",type="l",xlab="Time",main="Historical & Future load (Monthly)")
  abline(v=2017,col="red",lty=2)
  
  yearlyload<-c(sum(window(wholedata, start=c(2007,1), end=c(2008,0))),sum(window(wholedata, start=c(2008,1), end=c(2009,0))),sum(window(wholedata, start=c(2009,1), end=c(2010,0))),
                sum(window(wholedata, start=c(2010,1), end=c(2011,0))),sum(window(wholedata, start=c(2011,1), end=c(2012,0))),sum(window(wholedata, start=c(2012,1), end=c(2013,0))),
                sum(window(wholedata, start=c(2013,1), end=c(2014,0))),sum(window(wholedata, start=c(2014,1), end=c(2015,0))),sum(window(wholedata, start=c(2015,1), end=c(2016,0))),
                sum(window(wholedata, start=c(2016,1), end=c(2017,0))),sum(window(wholedata, start=c(2017,1), end=c(2018,0))),sum(window(wholedata, start=c(2018,1), end=c(2019,0))),
                sum(window(wholedata, start=c(2019,1), end=c(2020,0))),sum(window(wholedata, start=c(2020,1), end=c(2021,0))),sum(window(wholedata, start=c(2021,1), end=c(2022,0))),
                sum(window(wholedata, start=c(2022,1), end=c(2023,0))),sum(window(wholedata, start=c(2023,1), end=c(2024,0))),sum(window(wholedata, start=c(2024,1), end=c(2025,0))),
                sum(window(wholedata, start=c(2025,1), end=c(2026,0))),sum(window(wholedata, start=c(2026,1), end=c(2026,12))))
  
  plot(seq(2007,2026,length=20),yearlyload,xaxt = "n",type="b",xlab="Time",main="Historical & Future load (Yearly)")
  axis(side = 1, seq(2007,2026,by=1))
  abline(v=2016,col="red",lty=2)
  
  yearlyload<-ts(yearlyload,start=2007,end=2026,frequency=1)
  percentGrowth<-as.numeric(diff(yearlyload)/lag(yearlyload,-1))
  percentGrowth<-c(NA,percent(percentGrowth))
  write.table(data.frame("Time"=seq(2007,2026,by=1),"Yearly Load"=yearlyload,"Percent Growth"=percentGrowth),paste("prediction for",city,".csv"),sep=",",row.names=FALSE,col.names=TRUE)
  
}

