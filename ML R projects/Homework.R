#--------------------------1---------------------------
#Чтение файла данных
setwd("C:/Users/karak/Documents")
data_google <- read.csv("Google_Stock.csv", header=T, sep=",", encoding = "UTF-8")
data_google$Date<-as.Date(data_google$X.U.FEFF.Date)
plot(data_google$Date, data_google$Open, type= "l", col="black", panel.first=grid(),  xlab = "Дата", ylab = "Стоимость", ylim=c(-100,1100))

library(forecast)

# Выделение трендовой составляющей
trend = ma(data_google$Open, order = 10, centre = T)
lines(data_google$Date, trend, type = "l", col="blue")

rest<-data_google$Open-trend
lines(data_google$Date, rest, type = "l", col="blue")

#Разбиваем данные на тестовую и  обучающую выборки
n<-length(data_google$Date)
t<-round(n*0.8)

data_train<-data.frame(date=data_google$Date[2:t], trend=trend[2:t], rest=rest[2:t], trend2=trend[1:(t-1)], rest2=rest[1:(t-1)], open=data_google$Open[2:t], open2=data_google$Open[1:(t-1)])
data_train<-na.omit(data_train)

data_test<-data.frame(date=data_google$Date[(t+1):n], trend=trend[(t+1):n], rest=rest[(t+1):n], trend2=trend[t:(n-1)], rest2=rest[t:(n-1)], open=data_google$Open[(t+1):n], open2=data_google$Open[t:(n-1)])
data_train<-na.omit(data_test)

library(caret)

# ---===[ SVM ]===---
mod_svm_trend<-train(trend~trend2+date, data=data_train, method = "knn")
#res_svm_trend<-predict(mod_svm_trend, data_test)

mod_svm_rest<-train(rest~rest2+date, data=data_train, method = "knn")
#res_svm_rest<-predict(mod_svm_rest, data_test)

mod_svm_open<-train(open~open2+date, data=data_train, method = "knn")

res_svm_rest<-NA
res_svm_trend<-NA
res_svm_open<-NA

for (i in 1:length(data_test$trend)) {
  res_trend<-predict(mod_svm_trend, data_test[i,])
  res_rest<-predict(mod_svm_rest, data_test[i,])
  res_open<-predict(mod_svm_open, data_test[i,])
  if (i<length(data_test$trend)) {
    data_test[i+1,]$trend2 <-res_trend 
    data_test[i+1,]$rest2 <-res_rest
    data_test[i+1,]$open2 <-res_open
  }
  res_svm_rest[i]<-res_rest
  res_svm_trend[i]<-res_trend
  res_svm_open[i]<-res_open
}

lines(data_test$date[1:length(res_svm_trend)], res_svm_trend, type = "l", col="green")
lines(data_test$date[1:length(res_svm_rest)], res_svm_rest, type = "l", col="green")

lines(data_test$date[1:length(res_svm_rest)], res_svm_open, type = "l", col="violet")

fcast<-res_svm_trend+res_svm_rest

lines(data_test$date[1:length(res_svm_rest)], fcast, type = "l", col="red")

# Проверка адеватности
boxplot(data_google$Open[(t+1):n], res_svm_open, fcast)

#Критерий Фишера
var.test(data_google$Open[(t+1):n],res_svm_open)
var.test(data_google$Open[(t+1):n],fcast)

#--------------------------2---------------------------
data<-read.csv("bike.csv", header = T, sep=",", encoding = "UTF-8")

res<-nls(count~a+b*holiday+c*humidity+d*summer+e*temp+i*windspeed+j*workingday, data=data, start=list(a=0.01, b=0.01, c=0.01, d=0.01, e=0.01, i=0.01, j=0.01))
coef_func<-coef(res)

plot(data$count, type= "l", col="blue", panel.first=grid(),  xlab = "Номер измерения", ylab = "Количество")

fcast2<-coef_func[1]+coef_func[2]*data$holiday+coef_func[3]*data$humidity+coef_func[4]*data$summer+coef_func[5]*data$temp+coef_func[6]*data$windspeed+coef_func[7]*data$workingday
lines(fcast2)

#Сортируем
data2<-data[with(data, order(count)), ]
plot(data2$count, type= "l", col="blue", panel.first=grid(),  xlab = "Номер измерения", ylab = "Количество")

fcast3<-coef_func[1]+coef_func[2]*data2$holiday+coef_func[3]*data2$humidity+coef_func[4]*data2$summer+coef_func[5]*data2$temp+coef_func[6]*data2$windspeed+coef_func[7]*data2$workingday
lines(fcast3)

res1<-NA
res2<-NA
res1[which(data$count>100)]<-1
res1[which(data$count<101)]<-0

res2[which(fcast2>100)]<-1
res2[which(fcast2<101)]<-0

precision<-precision(data = factor(res2), reference = factor(res1))
recall<-recall(data = factor(res2), reference = factor(res1))
F_means<-F_meas(data = factor(res2), reference = factor(res1))

library("MLmetrics")
accuracy<-Accuracy(res2, res1)

library(pROC)
SVMl.roc<-roc(res2,res1)
plot(SVMl.roc)

