# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

hello <- function() {
  print("Hello, world!")
}
getwd()
dir()
rm(list=Is(all=TRUE))
rm(list=is(all=TRUE))
data=read.csv('C:/Users/USER/OneDrive/Desktop/BYOP-WALMART/Walmart.csv',header=T)
(data=readdata=read.csv('C:/Users/USER/OneDrive/Desktop/BYOP-WALMART/Walmart.csv',header=T))
is()
data=data=read.csv('C:/Users/USER/OneDrive/Desktop/BYOP-WALMART/Walmart.csv',header=T)
str(data)
data$X5.1
length(data)
mean(data)
a.matrix<-matrix(vector,nrow=r.ncol=c,byrow=FALSE,dimnames=list(char-vector-rownames,char-vector-col-names))
dim(data)
nrow(data)
ncol(data)
a.matrix<-matrix(vector,nrow,ncol,byrow=TRUE,dimnames=list(char-vector-rownames,char-vector-col-names))
y<-matrix(1:50,nrow=5,ncol=5)
A=matrix(C(1,2,3,4,5),nrow=2,byrow=T)
A
A=matrix(data(1,2,3,4,5),ncol=2)
print(data)
mean(unemployment)
cc=data$cc_employment
mean(cc)
mean(temperature)
store=data$store
dept=data$dept
date=data$date
sales=data$weekly_sales
holiday=data$isholiday
type=data$type
size=data$size
temp=data$temperature
fuel_price=data$fuel_price
m1=data$markdown1
m2=data$markdown2
m3=data$markdown3
m4=data$markdown4
m5=data$markdown5
cpi=data$cpi
unemployment=data$unemployment
mean(sales)
mean(temp)
mean(fuel_price)
mean(holiday)
sd(cpi)
sd(dept)
sd(fuel_price)
hist(sales)
hist(fuel_price)
boxplot(fuel_price,col="dark green")
library(ggplot2)
hist(type)
install.packages("corrgram")
library(corrgram)
corrgram(train,panel=panel.shade,text.panel=panel.txt,main="correlogram")
ggplot(train,aes(Outlet_Identifier, item_type))
train<- read.csv('C:/Users/USER/OneDrive/Desktop/BYOP-WALMART/Walmart.csv')
view(train)
library(ggplot2)
ggplot2(train,aes(sales,store)+geom_point()+scale_x_continuous('sales',breaks= seq(0,0.35,0.05))+scale_y_continuous('store',breaks=seq(0,270,by=30))+theme_bw())
ggplot(train,aes(store, sales))+geom_abline(aes(color=date))+scale_x_continuous('store')+scale_y_continuous('sales')+theme_bw()+labs(title='lineplot')+facet_wrap(~date)
ggplot(train,aes(store))+geom_histogram(binwidth =2)+ scale_x_continuous("store",breaks=seq(0,270,by=30))+scale_y_continuous("count",breaks=seq(0,200,by=20))+labs(title="HISTOGRAM")
ggplot(train,aes(store,fill= sales))+geom_bar()+labs(title="Stacked bar chart",x="store",y="sales")
summary(data)
library(psych)
hist(cpi)
hist(m1)
hist(date)
hist(dept)
hist(fuel_price)
hist(holiday)
hist(m2)
hist(m3)
hist(m4)
hist(m5)
hist(sales)
hist(size)
hist(unemployment)
install.packages("ggplot.multistats")
library('ggplot.multistats')
ggplot.multistats(train,aes(store,sales))+geom_point(aes(color=sales))+scale_x_continuous('store',breaks= seq(0,0.035,0.05))+ scale_y_continuous('sales',breaks= seq(0,270,by=30))+theme_dark()+labs(title= 'scatterplot')+facet_wrap(~date)
install.packages(corrgram)
library(corrgram)
corrgram(train,order=TRUE,panel=panel.shade,text.panel=panel.txt,main="correlogram")
newdata=mean(data,na.rm=TRUE)
m1=mean(m1,na.rm=TRUE)
m2=mean(m2,na.rm=TRUE)
m3=mean(m3,na.rm=TRUE)
m4=mean(m4,na.rm=TRUE)
m5=mean(m5,na.rm=TRUE)
mynewdata=cbind(m1,m2,m3,m4,m5)
write.csv(mynewdata,'C:/Users/USER/OneDrive/Desktop/BYOP-WALMART/Walmart.csv')
summary(mynewdata)
sales=ts(mynewdata$sales, start=c(2012,4),end=c(2013,10),frequency=12)
sales
plot(sales, type='b')
fuel_price=ts(mynewdata$fuel_price, start=c(2012,4),end=c(2013,10),frequency=12)
fuel_price
plot(fuel_price)
install.packages("tseries")
library("tseries")
adf.test(sales)
adf.test(fuel_price)
adf.test(date)
adf.test(store)
adf.test(size)
install.packages("forecast")
library(forecast)
kpss.test(sales)
kpss.test(store)
kpss.test(temp)
kpss.test(type)
kpss.test(unemployment)
mynewdata=ts(sales<-as.data.frame(store))
plot(mynewdata)
install.packages("forecast")
library(forecast)
ndiffs(store)
ndiffs(dept)
ndiffs(cpi)
ndiffs(fuel_price)
ndiffs(holiday)
ndiffs(temp)
ndiffs(type)
mydiffdata=diff(fuel_price,difference=1)
plot(mydiffdata)
adf.test(mydiff)
kpss.test(mydiffdata)
mydiffdata=diff(cpi,difference=1)
plot(mydiffdata)
adf.test(mydiff)
kpss.test(mydiffdata)
mydiffdata=diff(dept,difference=1)
plot(mydiffdata)
adf.test(mydiff)
kpss.test(mydiffdata)
mydiffdata=diff(holiday,difference=1)
plot(mydiffdata)
adf.test(mydiff)
kpss.test(mydiffdata)
mydiffdata=diff(temp,difference=1)
plot(mydiffdata)
adf.test(mydiff)
kpss.test(mydiffdata)
mydiffdata=diff(type,difference=1)
plot(mydiffdata)
adf.test(mydiff)
kpss.test(mydiffdata)
mydiffdata=diff(store,difference=1)
plot(mydiffdata)
adf.test(mydiff)
kpss.test(mydiffdata)
amount=ts(store<-as.data.frame(sales))
plot(amount)
mymodel=HoltWinters(sales,beta=FALSE,gamma=FALSE)
mymodel
mymodel=HoltWinters(sales,beta=TRUE,gamma=TRUE)
mymodel
plot(mymodel)
pred=fitted(mymodel)
res=residuals(mymodel)
outputdata=cbind(sales)
write.csv(outputdata)
abs_res=abs(res)
res_sq=res^2
pae=abs_res/sales
outputdata=cbind(cpi)
write.csv(outputdata)
abs_res=abs(res)
res_sq=res^2
pae= abs_res/cpi
mean(res)
mean(abs_res)
mse=mean(res_sq)
sqrt(mse)
mean(PAE)*100
qqnorm(res)
qqline(res)
shapiro.test(res)
mean(res)
library(forecast)
forecast=forecast(mymodel,1)
forecast
plot(forecast)
sales<-ts(mymodel$sales,start=2015, end=2022)
acf(sales,7)
acf(sales)
sales<-ts(mymodel$sales,start=2015, end=2022)
acf(sales,3)
acf(sales)
summary(mydiffdata)
library(forecast)
mymodel=auto.arima(mydiffdata)
mymodel
arima(mydiffdata,c(0,0,1))
arima(mydiffdata,c(0,1,0))
arima(mydiffdata,c(1,0,0))
arima(mydiffdata,c(1,0,1))
summary(mydiffdata)
pred=fitted(mydiffdata)
res=residuals(mydiffdata)
plot(res)
qqnorm(res)
qqline(res)
shapiro.test(res)
hist(res,col='pink')
forecast=forecast(mydiffdata,h=5)
forecast
