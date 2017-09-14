rm(list=ls())
library(forecast)
library(corrplot)
library(lattice)
library(zoo)
library(stats)
library(leaps)
library(boot)
library(leaps)
library(httr)
library(HH)
library(plsdepot)
library(pls)

ct<-"Berlin"
city<-"Berlin"
zipCode<-15530

#Get the historical load data(monthly)
data<-read.csv("historical load.csv",header=TRUE,sep=",")
ncol<-which(colnames(data)==ct)
load<-as.character(data[,ncol])
for (i in 1:length(load)){
  load[i]<-gsub('\\,', '', load[i])
}
ts<-ts(na.omit(as.numeric(load)),start=c(2007,1),end=c(2017,0),frequency=12)

#Do a normalization of monthly load data

# Yearly mean load
load.bar<-c(mean(window(ts, start=c(2007,1), end=c(2008,0))),mean(window(ts, start=c(2008,1), end=c(2009,0))),mean(window(ts, start=c(2009,1), end=c(2010,0))),
            mean(window(ts, start=c(2010,1), end=c(2011,0))),mean(window(ts, start=c(2011,1), end=c(2012,0))),mean(window(ts, start=c(2012,1), end=c(2013,0))),
            mean(window(ts, start=c(2013,1), end=c(2014,0))),mean(window(ts, start=c(2014,1), end=c(2015,0))),mean(window(ts, start=c(2015,1), end=c(2016,0))),
            mean(window(ts, start=c(2016,1), end=c(2017,0))))

# Monthly normalized load
load.dot<-rbind(window(ts, start=c(2007,1), end=c(2008,0))/load.bar[1],window(ts, start=c(2008,1), end=c(2009,0))/load.bar[2],window(ts, start=c(2009,1), end=c(2010,0))/load.bar[3],
                window(ts, start=c(2010,1), end=c(2011,0))/load.bar[4],window(ts, start=c(2011,1), end=c(2012,0))/load.bar[5],window(ts, start=c(2012,1), end=c(2013,0))/load.bar[6],
                window(ts, start=c(2013,1), end=c(2014,0))/load.bar[7],window(ts, start=c(2014,1), end=c(2015,0))/load.bar[8],window(ts, start=c(2015,1), end=c(2016,0))/load.bar[9],
                window(ts, start=c(2016,1), end=c(2017,0))/load.bar[10])

load.dot<-log(load.dot)
load.bar<-log(load.bar)

par(mfrow=c(3,1))
plot(log(ts),main="Monthly Historical load",ylab="Load")
plot(seq(2007,2017,length=120),load.dot,type="b",main="Monthly normalized load",ylab="Load",xlab="Time")
plot(seq(2007,2016,by=1),load.bar,type="b",main="Yearly average load",ylab="Load",xlab="Time")

#Get historical weather data(monthly)
AMP.Member<-read.csv("city info.csv",header=TRUE,sep=",")
rowindex<-which(AMP.Member["Zipcode"]==zipCode&AMP.Member["City"]==city)
airport<-AMP.Member[rowindex,4]
year<-seq(2007,2016,by=1)
month<-seq(1,12,by=1)

maxTemp<-matrix(0,nrow=10,ncol=12)
meanTemp<-matrix(0,nrow=10,ncol=12)
minTemp<-matrix(0,nrow=10,ncol=12)
meanHumi<-matrix(0,nrow=10,ncol=12)
monCDD<-matrix(0,nrow=10,ncol=12)
monHDD<-matrix(0,nrow=10,ncol=12)

for(i in 1:length(year)){
  for(j in 1:length(month)){
    
    url<-paste0("https://www.wunderground.com/history/airport/",airport,"/",year[i],"/",month[j],"/1/MonthlyHistory.html?")
    
    req<-GET(url)
    dat<-content(req, "text")
    
    myStr<-strsplit(dat,"Daily Weather History &amp; Observations")[[1]][2]
    myStr<-strsplit(myStr,"background-color: #FFF")[[1]][2]
    myStr<-strsplit(myStr,"script type=")[[1]][1]
    myStr<-gsub("\n|\r|\t|<tr>|</tr>|</td>|</tbody>|<tbody>","",myStr)
    myStr<-gsub("Rain,|Snow,|Fog,|Thunderstorm,","",myStr)
    myStr<-gsub("Rain|Snow|Fog|Thunderstorm|&nbsp;","",myStr)
    myStr<-gsub('<span class="wx-value">|</span>',"",myStr)
    myStr<-gsub("<.+?>", ",", myStr)
    myStr<-substring(myStr,6,nchar(myStr)-6) 
    myStr<-gsub(",,,","\n",myStr)
    
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

plot(maxTemp,type="l",col="red",main="Historical weather data",xlab="Time")
lines(meanTemp,type="l")
lines(minTemp,type="l",col="blue")
legend("topright",c("Maximum Temperature","Mean Temperature","Minimum Temperature"),col=c("red","black","blue"),lty=c(1,1,1),bty="n")


par(mfrow=c(2,1))
plot(monHDD,type="l",col="red",main="Historical HDD",xlab="Time")
plot(monCDD,type="l",col="blue",main="Historical CDD",xlab="Time")
par(mfrow=c(1,1))

# Fit weather-demand sub-model

tryfit.weather<-plsr(load.dot~.,data=weather.Model,validation="CV")
ncomp0<-which.min(tryfit.weather$validation$adj)
fit.weather<-tryfit.weather$fitted.values[,,ncomp0]

plot(seq(2007,2017,length=120),load.dot,type="l",xlab="Year",ylab="Yearly load")
lines(seq(2007,2017,length=120),fit.weather,type="b",col="red")
legend("topright",c("original data","fitted data"),col=c("black","red"),lty=c(1,1),bty="n")

temp<-matrix(0,nrow=12,ncol=6)

for(m in 1:12){
  for(n in 1:10){
      temp[m,]<-temp[m,]+as.matrix(weather.Model)[,-1][12*(n-1)+m,]
  } 
}

temp<-temp/10
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

plot(seq(2007,2027,length=240),c(load.dot,predict.Weather),xlab="Time",ylab="Monthly Load",main="monthly load forecast",type="l")
#lines(seq(2007,2017,length=120),fit.weather,type="b",col="red")
abline(v=2017,col="red",lty=2)
abline(h=predict.Weather[1],col="red",lty=2)

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
plot(hiseco.Data[,2],type="b",main="Total Population",ylab="Population")
plot(hiseco.Data[,3],type="b",main="Total Employment",ylab="Employment")
plot(hiseco.Data[,4],type="b",main="Total Households",ylab="Housholds")
plot(hiseco.Data[,5],type="b",main="Total Retail sales",ylab="Retail sales")
par(mfrow=c(1,1))

plot(corrplot.mixed(cor(hiseco.Data[,2:5]),upper="ellipse"))

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
start<-which(time=="2017")
end<-which(time=="2026")
new.Data<-eco.Data[start:end,]

par(mfrow=c(2,2))
plot(new.Data[,2],type="b",main="Future Total Population",ylab="Population")
plot(new.Data[,3],type="b",main="Future Total Employment",ylab="Employment")
plot(new.Data[,4],type="b",main="Future Total Households",ylab="Housholds")
plot(new.Data[,5],type="b",main="Future Total Retail sales",ylab="Retail sales")
par(mfrow=c(1,1))

predictedLoad<-predict(tryfit.eco,new.Data,ncomp=ncomp)

# Predict future load trend]
plot(seq(2007,2026,by=1),c(load.bar,predictedLoad),xlab="Time",ylab="Yearly Load",main="Yearly load forecast")
lines(seq(2007,2016,by=1),fit.eco,type="b",col="red")
abline(v=2016,col="red",lty=2)

# Fit monthly weather model

pre<-c()
for(i in 1:10){
  pre<-c(pre,exp(predict.Weather[(12*(i-1)+1):(12*i)])*exp(predictedLoad[i]))
}

plot(seq(2017,2027,length=120),pre,type="l",col="red",xlab="Time",ylab="Load KWH",main="Load Forecast from 2017 to 2026")

wholedata<-ts(c(ts,pre),start=c(2007,1),end=c(2026,12),frequency=12)

plot(seq(2007,2027,length=240),wholedata,ylab="Load",type="l",xlab="Time",main="Historical & Future load (Monthly)")
abline(v=2017,col="red",lty=2)

#write.table(as.data.frame(prediction),paste("prediction for",city,".csv"),sep=",",row.names=as.Date(prediction),col.names=FALSE)

yearlyload<-c(sum(window(wholedata, start=c(2007,1), end=c(2008,0))),sum(window(wholedata, start=c(2008,1), end=c(2009,0))),sum(window(wholedata, start=c(2009,1), end=c(2010,0))),
              sum(window(wholedata, start=c(2010,1), end=c(2011,0))),sum(window(wholedata, start=c(2011,1), end=c(2012,0))),sum(window(wholedata, start=c(2012,1), end=c(2013,0))),
              sum(window(wholedata, start=c(2013,1), end=c(2014,0))),sum(window(wholedata, start=c(2014,1), end=c(2015,0))),sum(window(wholedata, start=c(2015,1), end=c(2016,0))),
              sum(window(wholedata, start=c(2016,1), end=c(2017,0))),sum(window(wholedata, start=c(2017,1), end=c(2018,0))),sum(window(wholedata, start=c(2018,1), end=c(2019,0))),
              sum(window(wholedata, start=c(2019,1), end=c(2020,0))),sum(window(wholedata, start=c(2020,1), end=c(2021,0))),sum(window(wholedata, start=c(2021,1), end=c(2022,0))),
              sum(window(wholedata, start=c(2022,1), end=c(2023,0))),sum(window(wholedata, start=c(2023,1), end=c(2024,0))),sum(window(wholedata, start=c(2024,1), end=c(2025,0))),
              sum(window(wholedata, start=c(2025,1), end=c(2026,0))),sum(window(wholedata, start=c(2026,1), end=c(2026,12))))

plot(seq(2007,2026,length=20),yearlyload,type="b",xlab="Time",main="Historical & Future load (Yearly)")
abline(v=2016,col="red",lty=2)

yearlyload<-ts(yearlyload,start=2007,end=2026,frequency=1)
write.table(as.data.frame(yearlyload),paste("prediction for",city,".csv"),sep=",",row.names=as.character(seq(2007,2026,length=20)),col.names=FALSE)



