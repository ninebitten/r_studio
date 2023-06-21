#Q1: How to decomposed time series? And using ARIMA for modeling and forecasting.
#Data
data("AirPassengers")
View(AirPassengers)
AirPassengers
#preparing data
AP<-ts(AirPassengers, frequency =12, start =c(1949,1960)  )
# Understanding data
plot(AP) 
#log-Transform to fix-up variation
AP2<-log(AP)
plot(AP2)
#Decompose
DAP<- decompose(AP2)
DAP$figure
plot(DAP$figure,
     type='b', 
     xlab="month",	
     ylab="seasonality Index",
     col="red",
     las=2)
plot(DAP)  
#Auto rigrative moving average model(ARIMA)
#ARIMA(p,d,q) model
install.packages("forecast")
library(forecast)
fitmodel<-auto.arima(AP2)
fitmodel
#check Residual pot
hist(fitmodel$residuals,
     main = "Residual plot",
     xlab = "error",
     col = "green",
     freq = F)
lines(density(fitmodel$residuals),
      col="red",
      lw=2)
#Forecast for the next 4 years
pred<-forecast(fitmodel,4*12)
install.packages("ggplot2")
library(ggplot2)
autoplot(pred)	
accuracy(pred)



#Q2:How to select model and using selected model how to forecast by the fitted model?
  
#Data
data("AirPassengers")
AirPassengers
#understanding and preparing data
boxplot(AirPassengers-cycle(AirPassengers))
plot(AirPassengers)
abline(lm(AirPassengers~time(AirPassengers)))
#make it stationary
plot(diff(log(AirPassengers))) 
#Modelling: ARIMA(p,d,q)
install.packages("tseries")
library(tseries)
# selecting the value of q
acf(AirPassengers) 
acf(diff(log(AirPassengers)))
# selecting the value of P
pacf(diff(log(AirPassengers)))
# fit ARIMA(0,1,1)model
myfit<-arima(log(AirPassengers), 
             order =c(0,1,1), 
             seasonal=list(order=c(0,1,1),
                           period=12))
myfit
#forecast for next 10 year
pred<-predict(myfit,n.ahead = 10*12)
finalpred<-exp(pred$pred)
ts.plot(AirPassengers,finalpred, log='y',lty=c(1,3))

#cheaking accuracy of the prediction
datat<-ts(AirPassengers,frequency=12,start=c(1949,1),end=c(1959,12))
myfit2<-arima(log(datat), 
              order=c(0,1,1), 
              seasonal=list(order=c(0,1,1),
                            period=12))
pred2<-predict(myfit2,n.ahead = 1*12)
finalpred2<-exp(pred2$pred)
finalpred3<-exp(pred2$pred)
pred_1960<-round(finalpred2,0)
true_1960<-tail(AirPassengers,12)
data.frame(pred_1960,true_1960)




#Q3: How to time series analysis nicely and fancy style visualizing by using ggplot?
# Declaration Library Function
library(forecast)
install.packages("fpp")
install.packages("fpp2")
library(fpp)
library(fpp2)
library(ggplot2)
#time series plot
data(a10)
View(a10)
a10
autoplot(a10)+
  ggtitle("Antidiabeteic drug sales")+
  ylab("$ million")+
  xlab("Year")
#seasonal plot
ggseasonplot(a10,year.labels = T,year.labels.left = T)+
  ggtitle("seasonal plot: Antidiabeti drug sales")+
  ylab("$ million")
# polar seasonal plot
ggseasonplot(a10,polar = T)+	
  ggtitle("polar seasonal plot: Antidiabeti drug sales")+
  ylab("$ million")
#seasonal sub series plot
ggsubseriesplot(a10)+	
  ggtitle("sub-series plot: Antidiabeti drug sales")+
  ylab("$ million")

# visualizing ausbeer data
data(ausbeer)
beer2<-window(ausbeer,start=1992, end=c(2006,4))
autoplot(beer2)	
beerfit1<-meanf(beer2,h=12)
beerfit2<-rwf(beer2,h=12)
beerfit3<-snaive(beer2,h=12)
autoplot(window(ausbeer,start=1992))+
  autolayer(beerfit1,series="mean", PI=F)+	
  autolayer(beerfit2,series="Naive", PI=F)+
  autolayer(beerfit3,series="seasibak Naive", PI=F)+
  xlab("year")+ ylab("Meagalitres")+
  ggtitle("Forecasted quartely beer produuction")+
  guides(colour=guide_legend(title = "Forecast"))

# Accuracy
beer3<-window(ausbeer,start=2008)
accuracy(beerfit1,beer3)
accuracy(beerfit2,beer3)
accuracy(beerfit3,beer3)
# final selection
autoplot(beerfit3)	
#another data
goog200
autoplot(goog200)	
googf1<-meanf(goog200, h=40)
googf2<-rwf(goog200, h=40)
googf3<-rwf(goog200,drift=T, h=40)

autoplot(subset(goog,end=204))+
  autolayer(googf1,series="mean",PI=F)+	
  autolayer(googf2,series="NAive",PI=F)+
  autolayer(googf3,series="Drift",PI=F)+
  xlab("Day")+ ylab("closing Price(US$)")+
  ggtitle("google stock price")+
  guides(colour=guide_legend(title = "Forecast"))
autoplot(googf3)  










#Q4: How to generating Data and detection of trend and Get a stationary Time Series?
  #Time series analysis
  #Generating data
  x<-rnorm(100)
view(x)
x
monthly_ts<-ts(data = x,start = 1995,frequency = 12)
plot(monthly_ts)
#Detection of Trend and Get A Stationary Time Series
install.packages("fpp")
library(fpp)
library(forecast)
data("ausbeer")
View(ausbeer) 
Ausbeer
beer.ts<-window(ausbeer,start=1995,end=2005)
plot(as.ts(beer.ts))


#Creating Moving Average That Will Be Close To Trend
beer.trend <- ma(beer.ts,order=4,centre = T)
#Plot Trend and MA Together
plot(as.ts(beer.ts)) 
lines(beer.trend)

#Remove the trend from the time series
beer.detrend=beer.ts-beer.trend
plot(as.ts(beer.detrend))




#Q5: How to Analyzing and forecasting  from Wikipedia data?
  #Time series analysis
  #Getting wikipedia trend data
  install.packages("wikipediatrend")
library(wikipediatrend)
data<-wp_trend(page = "Sakib_al_hasan",
               from = "2015-01-01",
               to="2021-08-20")
View(data)
#plot
library(ggplot2)
qplot(date,views,data = data) 
summary(data)
#manupulation data
data$views[data$views==0]<-NA
qplot(date,views,data =data)
      ds<-data$date
      y<-log(data$views)
df<-data.frame(ds,y)
View(df)
qplot(ds,y,data = df)
#forcusting with facebook prophet
install.packages("prophet")
library(prophet)
mf<-prophet(df)
#prediction
prdict<-make_future_dataframe(mf,365)
tail(predict)
forecast<-predict(mf,predict)
tail(forecast(c['ds','yhat','yhat_lower','yhat_upper']))
     plot(mf,forecast)
     plot(mf,forecast)
#Q6: How to analyzing and forecasting COVID-19 data of  Bangladesh ?
#Time series analysis
#Getting lattest data of Bangladesh
data<-read.csv(choose.files(),header=T,sep=",")
View(data)
data

#packages
install.packages("ggplot2")
install.packages("dplyr")
library(ggplot2)
library(dplyr)


#screaning Data for analysis
data1<-filter(data,location--"Bangladesh")
View(data1)
data1<-filter(data1,date>="2020-03-15")
data2<-select(data1,date,new_cases)
View(data2)
str(data2)
data2$date<-as.date(data2$date)

#plot
qplot(date,new_cases,data = data2,
      main="covid-19 new cases in Bangladesh")
df<-data2$date
y<-data.frame(ds,y)
View(df)

#Forecasting
install.packages("prophet")
library(prophet)
mcc<-prophet(df)

#prediction
predict<-make_future_dataframe(mcc,periods = 130)
forecast<-predict(mcc,predict)
plot(mcc,forecast,xlab="data",ylab="newcases")







#Q7: How to analyzing and forecasting latest COVID-19 data of Bangladesh?
  #Time series analysis
  #Getting lattest data of Bangladesh
  data<-read.csv(choose.files(),header=T,sep=",")
View(data)

#packages
install.packages("ggplot2")
install.packages("dplyr")
library(ggplot2)
library(dplyr)


#screaning Data for analysis
data1<-filter(data,location=="Bangladesh")
View(data1)
data1<-filter(data1,date>="2020-03-15")
cc<-select(data1,date,total_cases)
View(cc)
str(cc)
cc$total_cases<-as.numeric(cc$total_cases)
cc$date<-as.date(cc$data)

#plot
qplot(date,total_cases,data =cc,)
main="covid-19 confirmed cases in Banglade"

df<-cc $date
y<-data.frame(ds,y)
View(df)

#Forecasting
install.packages("prophet")
library(prophet)
mcc<-prophet(df)

#prediction
predict<-make_future_dataframe(mcc,periods = 180)
tail(predict)
forecast<-predict(mcc,predict)
tail(forecast[c('ds','yhat','yhat_lower','yhat_upper')])
plot(mcc,forecast)
dyplot.prophet(mcc,forecast)
prophet_plot_components(mcc,forecast)

#model 1 performance
pred<-forecast$yhat[1:332]
actual<-mcc$history$y
plot(actul,pred)
abline(lm(pred~actual),col="red" )


     
 
      













