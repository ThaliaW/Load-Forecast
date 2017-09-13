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

ct<-"Blakely"
city<-"Blakely"
zipCode<-18452

#Get the historical load data(monthly)
data<-read.csv("historical load.csv",header=TRUE,sep=",")
ncol<-which(colnames(data)==ct)
load<-as.character(data[,ncol])
for (i in 1:length(load)){
  load[i]<- paste0(substr(load[i],1,1),substr(load[i],3,nchar(load[i])))
}
ts<-ts(na.omit(as.numeric(load)),start=c(2007,1),end=c(2017,0),frequency=12)

#Do a normalization of monthly load data

# Yearly mean load
load.bar<-c(mean(window(ts, start=c(2007,1), end=c(2008,0))),mean(window(ts, start=c(2008,1), end=c(2009,0))),mean(window(ts, start=c(2009,1), end=c(2010,0))),
            mean(window(ts, start=c(2010,1), end=c(2011,0))),mean(window(ts, start=c(2011,1), end=c(2012,0))),mean(window(ts, start=c(2012,1), end=c(2013,0))),
            mean(window(ts, start=c(2013,1), end=c(2014,0))),mean(window(ts, start=c(2014,1), end=c(2015,0))),mean(window(ts, start=c(2015,1), end=c(2016,0))),
            mean(window(ts, start=c(2016,1), end=c(2017,0))))

# Monthly normalized load
load.dot<-rbind(window(ts, start=c(2007,1), end=c(2008,0))/load.bar[1],window(ts, start=c(2008,1), end=c(2009,0))/load.bar[1],window(ts, start=c(2009,1), end=c(2010,0))/load.bar[1],
                window(ts, start=c(2010,1), end=c(2011,0))/load.bar[1],window(ts, start=c(2011,1), end=c(2012,0))/load.bar[1],window(ts, start=c(2012,1), end=c(2013,0))/load.bar[1],
                window(ts, start=c(2013,1), end=c(2014,0))/load.bar[1],window(ts, start=c(2014,1), end=c(2015,0))/load.bar[1],window(ts, start=c(2015,1), end=c(2016,0))/load.bar[1],
                window(ts, start=c(2016,1), end=c(2017,0))/load.bar[1])

histload<-data.frame(ts,load.bar,load.dot)

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

write.table(weather.Model,paste(ct,"Weather Data.csv"),row.names=FALSE,sep=",")#Get the Econometric data

plot(maxTemp,type="l",col="red",main="Historical weather data",xlab="Time")
lines(meanTemp,type="l")
lines(minTemp,type="l",col="blue")
legend("topright",c("Maximum Temperature","Mean Temperature","Minimum Temperature"),col=c("red","black","blue"),lty=c(1,1,1),bty="n")

par(mfrow=c(2,1))
plot(monHDD,type="l",col="red",main="Historical HDD",xlab="Time")
plot(monCDD,type="l",col="blue",main="Historical CDD",xlab="Time")
par(mfrow=c(1,1))

tryfit.weather<-regsubsets(load.dot~.,data=weather.Model)
selection<-which.min(summary(tryfit.weather)$bic)
fit.weather<-lm.regsubsets(tryfit.weather,selection)

plot(load.dot,type="l",xlab="Year",ylab="Monthlty load")
lines(fit.weather$fitted.values,type="b",col="red")
legend("topright",c("original data","fitted data"),col=c("black","red"),lty=c(1,1),bty="n")
#We have monthly maxTemp, meanTemp, minTemp, meanHumi, monHDD, monCDD

# Get future weather data
d<-12
N<-length(ts)
n<-round(N/d,0)
b<-2
k<-round(n/b,0)
l<-k*b*d
i<-sample(1:k)
new.weather<-data.frame("maxTemp"=c(),"meanTemp"=c(),"minTemp"=c(),"meanHumi"=c(),"monCDD"=c(),"monHDD"=c())

for(m in 1:k){
  for(j in 1:(b*d)){
    new.weather[(m-1)*b*d+j,"maxTemp"]<-weather.Model[i[m]*d+j-1,"maxTemp"]
    new.weather[(m-1)*b*d+j,"meanTemp"]<-weather.Model[i[m]*d+j-1,"meanTemp"]
    new.weather[(m-1)*b*d+j,"minTemp"]<-weather.Model[i[m]*d+j-1,"minTemp"]
    new.weather[(m-1)*b*d+j,"meanHumi"]<-weather.Model[i[m]*d+j-1,"meanHumi"]
    new.weather[(m-1)*b*d+j,"monCDD"]<-weather.Model[i[m]*d+j-1,"monCDD"]
    new.weather[(m-1)*b*d+j,"monHDD"]<-weather.Model[i[m]*d+j-1,"monHDD"]
  }
}

predict.Weather<-predict(fit.weather,new.weather)

par(mfrow=c(2,1))
plot(meanTemp,type="l",col="red",main="Historical meanTemp",xlab="Time")
plot.ts(ts(new.weather["meanTemp"]),main="Future meanTemp")
par(mfrow=c(1,1))

par(mfrow=c(2,1))
plot(meanHumi,type="l",col="red",main="Historical meanHumi",xlab="Time")
plot.ts(ts(new.weather["meanHumi"]),main="Future meanHumi ")
par(mfrow=c(1,1))

par(mfrow=c(2,1))
plot(monHDD,type="l",col="red",main="Historical HDD",xlab="Time")
plot.ts(ts(new.weather["monHDD"]),main="Future HDD ")
par(mfrow=c(1,1))

par(mfrow=c(2,1))
plot(monCDD,type="l",col="red",main="Historical CDD",xlab="Time")
plot.ts(ts(new.weather["monCDD"]),main="Future CDD ")
par(mfrow=c(1,1))

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

corrplot.mixed(cor(hiseco.Data[,2:5]),upper="ellipse")

# Fit Yealy eco model (multivariate linear model)
eco.model<-data.frame(load.bar,hiseco.Data[,2:5])
colnames(eco.model)<-c("yearlyLoad","totalPopulation","totalEmployment","totalHouseholds","totalRetail")

tryfit.eco<-plsr(yearlyLoad~.,data=eco.model,validation="CV")
ncomp<-which.min(tryfit.eco$validation$adj)
fit.eco<-tryfit.eco$fitted.values[,,ncomp]


# tryfit.eco<-regsubsets(yearlyLoad~.,data=eco.model)
# selection2<-which.min(summary(tryfit.eco)$bic)
# fit.eco<-lm.regsubsets(tryfit.eco,selection2)

plot(seq(2007,2016,by=1),load.bar,type="b",xlab="Year",ylab="Yearly load")
lines(seq(2007,2016,by=1),fit.eco,type="b",col="red")
legend("topright",c("original data","fitted data"),col=c("black","red"),lty=c(1,1),bty="n")

# lines(seq(2007,2016,by=1),fit.eco$fitted.values,type="b",col="red")
# 

# Get future eco data
start<-which(time=="2017")
end<-which(time=="2026")
new.Data<-eco.Data[start:end,]

predictedLoad<-predict(tryfit.eco,new.Data,ncomp=ncomp)


# Predict future load trend
# predictedLoad<-predict(fit.eco,new.Data)
#plot(seq(2007,2026,by=1),c(load.bar,predict(fit.eco,new.Data)),xlab="Time",ylab="Yearly Load",main="Yearly load forecast")

plot(seq(2007,2026,by=1),c(load.bar,predictedLoad),xlab="Time",ylab="Yearly Load",main="Yearly load forecast")
lines(seq(2007,2016,by=1),fit.eco,type="b",col="red")
#lines(seq(2007,2016,by=1),fit.eco$fitted.values,type="b",col="red")
abline(v=2016,col="red",lty=2)

# Fit monthly weather model

pre<-c()
for(i in 1:10){
  pre<-c(pre,predict.Weather[(12*(i-1)+1):(12*i)]*predictedLoad[i])
}

plot(seq(2017,2026,length=120),pre,type="l",col="red",xlab="Time",ylab="Load KWH",main="Load Forecast from 2017 to 2026")

prediction<-ts(pre,start=c(2017,1),end=c(2026,12),frequency=12)

wholedata<-ts(c(ts,pre))
plot.ts(seq(2007,2026,length=240),wholedata,ylab="Load",type="l",xlab="Time",main="Historical & Future load")
abline(v=2016,col="red",lty=2)
write.table(as.data.frame(prediction),"predicted.csv",sep=",",row.names=as.Date(prediction),col.names=FALSE)

hiseco.Data
new.Data