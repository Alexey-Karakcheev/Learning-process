#Чтение файла данных
data <- read.csv("C:/Users/karak/Documents/covid-daily-vs-total-cases.csv")

data$Day<-as.Date(data$Day)
#Код страны
CD<-"RUS"


data_1county<-data.frame(Day=data$Day[data$Code==CD],
                         Day_Value=data$Daily.new.confirmed.cases.due.to.COVID.19..rolling.7.day.average..right.aligned.[data$Code==CD],
                         Total_Value=data$Total.confirmed.cases.of.COVID.19[data$Code==CD])
#графики
plot(data_1county$Day, data_1county$Total_Value, type="l", col="blue", panel.first = grid(), xlab="Дата", ylab = "Количество заболеваний")
lines(data_1county$Day, data_1county$Day_Value, type = "l", col="red")
hist(data_1county$Day_Value, xlab = "Значение", ylab = "Частота", main = "Статистика по числу заболевших")


#Lession 3
#Удаляем строки с пустыми данными
data2<-na.omit(data)
code_uniq<-unique(data2$Code)

data3<-data.frame(land1=NA, land2=NA, cor_daily=NA, cor_val=NA, dtw_daily=NA, dtw_val=NA)
library(dtw)

for (i in c(CD)) {
  code_uniq<-code_uniq[-which(code_uniq==i)]
  for (j in code_uniq) {
    
    a1<-data.frame(c1=data2$Daily.new.confirmed.cases.due.to.COVID.19..rolling.7.day.average..right.aligned.[data2$Code==i],
                   c2=data2$Total.confirmed.cases.of.COVID.19[data2$Code==i],
                   d=data2$Day[data2$Code==i])
    a2<-data.frame(c1=data2$Daily.new.confirmed.cases.due.to.COVID.19..rolling.7.day.average..right.aligned.[data2$Code==j],
                   c2=data2$Total.confirmed.cases.of.COVID.19[data2$Code==j],
                   d=data2$Day[data2$Code==j])
    l1<-length(a1$c1)
    l2<-length(a2$c1)
    
    if (l1>=l2) {
      c_daily<-cor(a1$c1[(l1-l2+1):l1],a2$c1)
      c_val<-cor(a1$c2[(l1-l2+1):l1],a2$c2)
      D_daily <- dtw(a1$c1[(l1-l2+1):l1],a2$c1, keep=TRUE)
      D_val <- dtw(a1$c2[(l1-l2+1):l1],a2$c2, keep=TRUE)
    } else {
      c_daily<-cor(a1$c1,a2$c1[(l2-l1+1):l2])
      c_val<-cor(a1$c2,a2$c2[(l2-l1+1):l2])
      D_daily <- dtw(a1$c1,a2$c1[(l2-l1+1):l2], keep=TRUE)
      D_val <- dtw(a1$c2,a2$c2[(l2-l1+1):l2], keep=TRUE)
    }
    
    cat("Корреляция между",i," и " ,j,": ", c_daily, " и ", c_val, "\n")
    cat("Расстояние по DTW ", i, "и ", j, ": ", D_daily$distance, " и ", D_val$distance, "\n")
    
    data3<-rbind(data3,c(i,j,c_daily,c_val,D_daily$distance,D_val$distance))
    
  }
}

data3<-na.omit(data3)
rm(a1,a2,c_daily, c_val, D_daily, D_val, l1, l2, i, j, code_uniq)

#Многопараметрические регрессионные модели

data_RUS<-data.frame(Day=data2$Day[data2$Code=="RUS"],RUS_Value=data2$Daily.new.confirmed.cases.due.to.COVID.19..rolling.7.day.average..right.aligned.[data2$Code=="RUS"],
                     RUS_Sum=data2$Total.confirmed.cases.of.COVID.19[data2$Code=="RUS"])
data_LVA<-data.frame(Day=data2$Day[data2$Code=="LVA"],LVA_Value=data2$Daily.new.confirmed.cases.due.to.COVID.19..rolling.7.day.average..right.aligned.[data2$Code=="LVA"],
                     LVA_Sum=data2$Total.confirmed.cases.of.COVID.19[data2$Code=="LVA"])
data_NCL<-data.frame(Day=data2$Day[data2$Code=="NCL"],NCL_Value=data2$Daily.new.confirmed.cases.due.to.COVID.19..rolling.7.day.average..right.aligned.[data2$Code=="NCL"],
                     NCL_Sum=data2$Total.confirmed.cases.of.COVID.19[data2$Code=="NCL"])
data_LTU<-data.frame(Day=data2$Day[data2$Code=="LTU"],LTU_Value=data2$Daily.new.confirmed.cases.due.to.COVID.19..rolling.7.day.average..right.aligned.[data2$Code=="LTU"],
                     LTU_Sum=data2$Total.confirmed.cases.of.COVID.19[data2$Code=="LTU"])
data_GEO<-data.frame(Day=data2$Day[data2$Code=="GEO"],GEO_Value=data2$Daily.new.confirmed.cases.due.to.COVID.19..rolling.7.day.average..right.aligned.[data2$Code=="GEO"],
                     GEO_Sum=data2$Total.confirmed.cases.of.COVID.19[data2$Code=="GEO"])


data_SUM<-merge(data_RUS,data_LVA, by="Day")
data_SUM<-merge(data_SUM,data_NCL, by="Day")


data_SUM<-data.frame(Day=data_SUM$Day,  RUS_Value=data_SUM$RUS_Value, RUS_Sum=data_SUM$RUS_Sum,
                                        LVA_Value=data_SUM$LVA_Value.x, LVA_Sum=data_SUM$LVA_Sum.x,
                                        NCL_Value=data_SUM$NCL_Value.y,NCL_Sum=data_SUM$NCL_Sum.y)

data_SUM<-merge(data_SUM, data_LTU, by="Day")
data_SUM<-data.frame(Day=data_SUM$Day, RUS_Value=data_SUM$RUS_Value, RUS_Sum=data_SUM$RUS_Sum,
                                       LVA_Value = data_SUM$LVA_Value.x, LVA_Sum=data_SUM$LVA_Sum.x,
                                       NCL_Value = data_SUM$NCL_Value, NCL_Sum=data_SUM$NCL_Sum,
                                       LTU_Value = data_SUM$LTU_Value.y, LTU_Sum=data_SUM$LTU_Sum.y)

data_SUM<-merge(data_SUM, data_GEO, by="Day")
data_SUM<-data.frame(Day=data_SUM$Day, RUS_Value = data_SUM$RUS_Value, RUS_Sum=data_SUM$RUS_Sum,
                                       LVA_Value = data_SUM$LVA_Value.x, LVA_Sum=data_SUM$LVA_Sum.x,
                                       NCL_Value = data_SUM$NCL_Value, NCL_Sum=data_SUM$NCL_Sum,
                                       LTU_Value = data_SUM$LTU_Value, LTU_Sum=data_SUM$LTU_Sum,
                                       GEO_Value = data_SUM$GEO_Value.y, GEO_Sum=data_SUM$GEO_Sum.y)


n<-length(data_SUM$Day)
t<-round(n*0.8)
n<-1 #сдвиг по времени

data_train<-data.frame(Day = data_SUM$Day[1:(t-n)],
                       RUS_Value = data_SUM$RUS_Value[1:(t-n)],
                       RUS_Value2 = data_SUM$RUS_Value[(n+1):(t)],
                       RUS_Sum = data_SUM$RUS_Sum[1:(t-n)],
                       RUS_Sum2 = data_SUM$RUS_Sum[(n+1):(t)],
                       LVA_Value = data_SUM$LVA_Sum[1:(t-n)],
                       LVA_Sum = data_SUM$LVA_Value[1:(t-n)],
                       NCL_Value = data_SUM$NCL_Sum[1:(t-n)],
                       NCL_Sum = data_SUM$NCL_Value[1:(t-n)],
                       LTU_Value = data_SUM$LTU_Sum[1:(t-n)],
                       LTU_Sum = data_SUM$LTU_Value[1:(t-n)],
                       GEO_Value = data_SUM$GEO_Sum[1:(t-n)],
                       GEO_Sum = data_SUM$GEO_Value[1:(t-n)]
)

data_test<-data.frame(Day = data_SUM$Day[t:(nrow(data_SUM)-1)],
                      RUS_Value = data_SUM$RUS_Value[t:(nrow(data_SUM)-1)],
                      RUS_Value2 = data_SUM$RUS_Value[(t+1):(nrow(data_SUM))],
                      RUS_Sum = data_SUM$RUS_Sum[t:(nrow(data_SUM)-1)],
                      RUS_Sum2 = data_SUM$RUS_Sum[(t+1):(nrow(data_SUM))],
                      LVA_Value = data_SUM$LVA_Sum[t:(nrow(data_SUM)-1)],
                      LVA_Sum = data_SUM$LVA_Value[t:(nrow(data_SUM)-1)],
                      NCL_Value = data_SUM$NCL_Sum[t:(nrow(data_SUM)-1)],
                      NCL_Sum = data_SUM$NCL_Value[t:(nrow(data_SUM)-1)],
                      LTU_Value = data_SUM$LTU_Sum[t:(nrow(data_SUM)-1)],
                      LTU_Sum = data_SUM$LTU_Value[t:(nrow(data_SUM)-1)],
                      GEO_Value = data_SUM$GEO_Sum[t:(nrow(data_SUM)-1)],
                      GEO_Sum = data_SUM$GEO_Value[t:(nrow(data_SUM)-1)]
)
#Метод авторегрессии VAR
library(vars)

e_arima_m<-data.frame(y=data_train$RUS_Value2, x1=data_train$LVA_Value, x2=data_train$NCL_Value, x3=data_train$LTU_Value, x4=data_train$GEO_Value)
e_arima_m2<-data.frame(y=data_train$RUS_Sum2, x1=data_train$LVA_Sum, x2=data_train$NCL_Sum, x3=data_train$LTU_Sum, x4=data_train$GEO_Sum)
#прогноз по дням
mod_var<-VAR(e_arima_m, p = 2, type= "const", season = NULL, exog = NULL)
res_var <- predict(mod_var, n.ahead = length(data_test$Day), ci = 0.95)

plot(data_SUM$Day, data_SUM$RUS_Value, type = "l", col = "black", panel.first=grid(), xlab = "Data", ylab= "Zabolevshie")
lines(data_test$Day, res_var$fcst$y[,1], col = "green", type = "l")
lines(data_test$Day, res_var$fcst$y[,2], col = "gray", type = "l")
lines(data_test$Day, res_var$fcst$y[,3], col = "gray", type = "l")

#прогноз накопительным итогом

mod_var2<-VAR(e_arima_m2, p = 2, type= "const", season = NULL, exog = NULL)
res_var2 <- predict(mod_var2, n.ahead = length(data_test$Day), ci = 0.95)

plot(data_SUM$Day, data_SUM$RUS_Value, type = "l", col = "black", panel.first=grid(), xlab = "Data", ylab= "Zabolevshie")
lines(data_test$Day, res_var$fcst$y[,1], col = "green", type = "l")
lines(data_test$Day, res_var$fcst$y[,2], col = "gray", type = "l")
lines(data_test$Day, res_var$fcst$y[,3], col = "gray", type = "l")

library(caret)

#лассо

mod_lasso<-train(RUS_Value2~Day+RUS_Value+LVA_Value+LTU_Value+NCL_Value+GEO_Value, data=data_train, method = "lasso")
res_lasso<-predict(mod_lasso, data_test)

#PLS
mod_pls<-train(RUS_Value2~Day+RUS_Value+LVA_Value+LTU_Value+NCL_Value+GEO_Value, data=data_train, method = "pls")
res_pls<-predict(mod_pls, data_test)
#KNN
mod_knn<-train(RUS_Value2~Day+RUS_Value+LVA_Value+LTU_Value+NCL_Value+GEO_Value, data=data_train, method = "knn")
res_knn<-predict(mod_knn, data_test)

#SVM
mod_svm<-train(RUS_Value2~Day+RUS_Value+LVA_Value+LTU_Value+NCL_Value+GEO_Value, data=data_train, method = "svmLinear2")
res_svm<-predict(mod_svm, data_test)

mod_nnet<-train(RUS_Value2~Day+RUS_Value+LVA_Value+LTU_Value+NCL_Value+GEO_Value, data=data_train, method = "nnet")
res_nnet<-predict(mod_nnet, data_test)

#mod_Rborist<-train(RUS_Value2~Day+RUS_Value+LVA_Value+LTU_Value+NCL_Value+GEO_Value, data=data_train, method = "Rborist")
#res_Rborist<-predict(mod_Rborist, data_test)


plot(data_SUM$Day, data_SUM$RUS_Value, type = "l", col = "black", panel.first=grid(), xlab = "Data", ylab= "Zabolevshie")
lines(data_test$Day, res_lasso, col = "red", type = "l")
lines(data_test$Day, res_pls, col = "blue", type = "l")
lines(data_test$Day, res_knn, col = "violet", type = "l")
lines(data_test$Day, res_svm, col = "green", type = "l")
lines(data_test$Day, res_nnet, col = "gray", type = "l")
#lines(data_test$Day, res_Rborist, col = "yellow", type = "l")

#Значения накопительным итогом
#лассо

mod_lasso2<-train(RUS_Sum2~Day+RUS_Sum+LVA_Sum+LTU_Sum+NCL_Sum+GEO_Sum, data=data_train, method = "lasso")
res_lasso2<-predict(mod_lasso2, data_test)

#PLS
mod_pls2<-train(RUS_Sum2~Day+RUS_Sum+LVA_Sum+LTU_Sum+NCL_Sum+GEO_Sum, data=data_train, method = "pls")
res_pls2<-predict(mod_pls2, data_test)
#KNN
mod_knn2<-train(RUS_Sum2~Day+RUS_Sum+LVA_Sum+LTU_Sum+NCL_Sum+GEO_Sum, data=data_train, method = "knn")
res_knn2<-predict(mod_knn2, data_test)

#SVM
mod_svm2<-train(RUS_Sum2~Day+RUS_Sum+LVA_Sum+LTU_Sum+NCL_Sum+GEO_Sum, data=data_train, method = "svmLinear2")
res_svm2<-predict(mod_svm2, data_test)

mod_nnet2<-train(RUS_Sum2~Day+RUS_Sum+LVA_Sum+LTU_Sum+NCL_Sum+GEO_Sum, data=data_train, method = "nnet")
res_nnet2<-predict(mod_nnet2, data_test)

#mod_Rborist2<-train(RUS_Sum2~Day+RUS_Sum+LVA_Sum+LTU_Sum+NCL_Sum+GEO_Sum, data=data_train, method = "Rborist")
#res_Rborist2<-predict(mod_Rborist2, data_test)

plot(data_SUM$Day, data_SUM$RUS_Sum, type = "l", col = "black", panel.first=grid(), xlab = "Data", ylab= "Zabolevshie")
lines(data_test$Day, res_lasso2, col = "red", type = "l")
lines(data_test$Day, res_pls2, col = "blue", type = "l")
lines(data_test$Day, res_knn2, col = "violet", type = "l")
lines(data_test$Day, res_svm2, col = "green", type = "l")
lines(data_test$Day, res_nnet2, col = "gray", type = "l")
#lines(data_test$Day, res_Rborist2, col = "yellow", type = "l")

#--------------------------------------Занятие 4------------------------------------------------------------#
 
library(caret)

data_train2<-data_train
data_test2<-data_test
res_lasso_frcst<-NA
mod_lasso<-train(RUS_Value2~Day+RUS_Value+LVA_Value+LTU_Value+NCL_Value+GEO_Value, data=data_train, method = "lasso")

for (i in 1:length(data_test$RUS_Value2)){
  res_lasso<-predict(mod_lasso, data_test[i,])
  if (i<length(data_test$RUS_Value2)){
    data_test[i+1,]$RUS_Value<-res_lasso
  }
  res_lasso_frcst[i]<-res_lasso
}

plot(data_SUM$Day, data_SUM$RUS_Value, type = "l", col = "black", panel.first=grid(), xlab = "Дата", ylab= "Количество заболеваний")
lines(data_test$Day, res_lasso_frcst, col = "red", type = "l")


#STL Разложение

stl(data_train$RUS_Sum2)

library(forecast)

#Выделение трендовой составляющей

#выделение трендовой сотавляющей
trend_train = ma(data_train$RUS_Value, order = 10, centre = T)
trend_test = ma(data_test$RUS_Value, order = 10, centre = T)
trend_train2 = ma(data_train$RUS_Value2, order = 10, centre = T)
trend_test2 = ma(data_test$RUS_Value2, order = 10, centre = T)

plot.ts(data_train$RUS_Value2)
lines(trend, col="red")

rest_train=data_train$RUS_Value-trend_train
rest_test=data_test$RUS_Value-trend_test
rest_train2=data_train$RUS_Value2-trend_train2
rest_test2=data_test$RUS_Value2-trend_test2

#plot.ts(rest)

data_train_trend<-data.frame(Day = data_SUM$Day[1:(t-n)],
                             RUS_Value = trend_train,
                             RUS_Value2 = trend_train2,
                             RUS_Sum = data_SUM$RUS_Sum[1:(t-n)],
                             RUS_Sum2 = data_SUM$RUS_Sum[(n+1):(t)],
                             LVA_Value = data_SUM$LVA_Sum[1:(t-n)],
                             LVA_Sum = data_SUM$LVA_Value[1:(t-n)],
                             NCL_Value = data_SUM$NCL_Sum[1:(t-n)],
                             NCL_Sum = data_SUM$NCL_Value[1:(t-n)],
                             LTU_Value = data_SUM$LTU_Sum[1:(t-n)],
                             LTU_Sum = data_SUM$LTU_Value[1:(t-n)],
                             GEO_Value = data_SUM$GEO_Sum[1:(t-n)],
                             GEO_Sum = data_SUM$GEO_Value[1:(t-n)]
)

data_test_trend<-data.frame(Day = data_SUM$Day[t:(nrow(data_SUM)-1)],
                            RUS_Value = trend_test,
                            RUS_Value2 = trend_test2,
                            RUS_Sum = data_SUM$RUS_Sum[t:(nrow(data_SUM)-1)],
                            RUS_Sum2 = data_SUM$RUS_Sum[(t+1):(nrow(data_SUM))],
                            LVA_Value = data_SUM$LVA_Sum[t:(nrow(data_SUM)-1)],
                            LVA_Sum = data_SUM$LVA_Value[t:(nrow(data_SUM)-1)],
                            NCL_Value = data_SUM$NCL_Sum[t:(nrow(data_SUM)-1)],
                            NCL_Sum = data_SUM$NCL_Value[t:(nrow(data_SUM)-1)],
                            LTU_Value = data_SUM$LTU_Sum[t:(nrow(data_SUM)-1)],
                            LTU_Sum = data_SUM$LTU_Value[t:(nrow(data_SUM)-1)],
                            GEO_Value = data_SUM$GEO_Sum[t:(nrow(data_SUM)-1)],
                            GEO_Sum = data_SUM$GEO_Value[t:(nrow(data_SUM)-1)]
)
data_train_rest<-data.frame(Day = data_SUM$Day[1:(t-n)],
                            RUS_Value = rest_train,
                            RUS_Value2 = rest_train2,
                            RUS_Sum = data_SUM$RUS_Sum[1:(t-n)],
                            RUS_Sum2 = data_SUM$RUS_Sum[(n+1):(t)],
                            LVA_Value = data_SUM$LVA_Sum[1:(t-n)],
                            LVA_Sum = data_SUM$LVA_Value[1:(t-n)],
                            NCL_Value = data_SUM$NCL_Sum[1:(t-n)],
                            NCL_Sum = data_SUM$NCL_Value[1:(t-n)],
                            LTU_Value = data_SUM$LTU_Sum[1:(t-n)],
                            LTU_Sum = data_SUM$LTU_Value[1:(t-n)],
                            GEO_Value = data_SUM$GEO_Sum[1:(t-n)],
                            GEO_Sum = data_SUM$GEO_Value[1:(t-n)]
)

data_test_rest<-data.frame(Day = data_SUM$Day[t:(nrow(data_SUM)-1)],
                           RUS_Value = rest_test,
                           RUS_Value2 = rest_test2,
                           RUS_Sum = data_SUM$RUS_Sum[t:(nrow(data_SUM)-1)],
                           RUS_Sum2 = data_SUM$RUS_Sum[(t+1):(nrow(data_SUM))],
                           LVA_Value = data_SUM$LVA_Sum[t:(nrow(data_SUM)-1)],
                           LVA_Sum = data_SUM$LVA_Value[t:(nrow(data_SUM)-1)],
                           NCL_Value = data_SUM$NCL_Sum[t:(nrow(data_SUM)-1)],
                           NCL_Sum = data_SUM$NCL_Value[t:(nrow(data_SUM)-1)],
                           LTU_Value = data_SUM$LTU_Sum[t:(nrow(data_SUM)-1)],
                           LTU_Sum = data_SUM$LTU_Value[t:(nrow(data_SUM)-1)],
                           GEO_Value = data_SUM$GEO_Sum[t:(nrow(data_SUM)-1)],
                           GEO_Sum = data_SUM$GEO_Value[t:(nrow(data_SUM)-1)]
)

data_train_trend<-na.omit(data_train_trend)
mod_lasso<-train(RUS_Value2~Day+RUS_Value+LVA_Value+LTU_Value+NCL_Value+GEO_Value, data=data_train_trend, method = "lasso")

data_test_trend<-na.omit(data_test_trend)

res_lasso_frcst_trend<-NA
for (i in 1:length(data_test_trend$RUS_Value2)) {
  res_lasso<-predict(mod_lasso, data_test_trend[i,])
  if (i<length(data_test_trend$RUS_Value2)) {
    data_test_trend[i+1,]$RUS_Value<-res_lasso
  }
  res_lasso_frcst_trend[i]<-res_lasso
}


plot(data_test_trend$Day, data_test_trend$RUS_Value, type = "l", col = "black", panel.first=grid(), xlab = "Дата", ylab= "Количество заболеваний")
lines(data_test_trend$Day, res_lasso_frcst_trend, col = "red", type = "l")



data_train_rest<-na.omit(data_train_rest)
mod_lasso<-train(RUS_Value2~Day+RUS_Value+LVA_Value+LTU_Value+NCL_Value+GEO_Value, data=data_train_rest, method = "lasso")

data_test_rest<-na.omit(data_test_rest)

res_lasso_frcst_rest<-NA
for (i in 1:length(data_test_rest$RUS_Value2)) {
  res_lasso<-predict(mod_lasso, data_test_rest[i,])
  if (i<length(data_test_rest$RUS_Value2)) {
    data_test_rest[i+1,]$RUS_Value<-res_lasso
  }
  res_lasso_frcst_rest[i]<-res_lasso
}

plot(data_test_rest$Day, data_test_rest$RUS_Value, type = "l", col = "black", panel.first=grid(), xlab = "Дата", ylab= "Количество заболеваний")
lines(data_test_rest$Day, res_lasso_frcst_rest, col = "red", type = "l")


plot(data_test_rest$Day, data_test_rest$RUS_Value+data_test_trend$RUS_Value, type = "l", col = "black", panel.first=grid(), xlab = "Дата", ylab= "Количество заболеваний")
lines(data_test_rest$Day, res_lasso_frcst_rest+res_lasso_frcst_trend, col = "red", type = "l")


#перидограмма
spec.pgram(data_train$RUS_Value2, col="blue")


#Метод Вейвлет анализа
library(WaveletComp)
library(biwavelet)

e<-data.frame(y=data_train$RUS_Value,x=data_train$Day)
modWV<-analyze.wavelet(e, my.series = 1,
                       loess.span = 0,
                       dt=1, dj=1/298,
                       lowerPeriod = 1/4,
                       upperPeriod = 256,
                       make.pval = F, n.sim=10)
wt.image(modWV, n.levels = 337,
         legend.params = list(lab="Степень влияния"))

modWV_1 <- reconstruct(modWV, sel.period=3, show.legend = F) 
modWV_2 <- reconstruct(modWV, sel.period=6, show.legend = F) 
modWV_3 <- reconstruct(modWV, sel.period=16, show.legend = F) 
modWV_4 <- reconstruct(modWV, sel.period=64, show.legend = F) 
modWV_5 <- reconstruct(modWV, sel.period=256, show.legend = F)

plot(data_SUM$Day, data_SUM$RUS_Value, type = "l", col = "black", panel.first=grid(), xlab = "Дата", ylab= "Количество заболеваний")

lines(data_train$Day, modWV_5$series$y.r, type = "l", col = "red")

#прогноз по каждой гармонике отдельно
modWV_rec1<-auto.arima(modWV_1$series$y.r)
modWV_rec1<-forecast(modWV_rec1, h=length(data_test$RUS_Value2))
modWV_rec2<-auto.arima(modWV_2$series$y.r)
modWV_rec2<-forecast(modWV_rec2, h=length(data_test$RUS_Value2))
modWV_rec3<-auto.arima(modWV_3$series$y.r)
modWV_rec3<-forecast(modWV_rec3, h=length(data_test$RUS_Value2))
modWV_rec4<-auto.arima(modWV_4$series$y.r)
modWV_rec4<-forecast(modWV_rec4, h=length(data_test$RUS_Value2))
modWV_rec5<-auto.arima(modWV_5$series$y.r)
modWV_rec5<-forecast(modWV_rec5, h=length(data_test$RUS_Value2))
lines(data_test$Day,modWV_rec5$mean, type = "l", col = "green")

modWV_rec<-(modWV_rec1$mean+modWV_rec2$mean+modWV_rec3$mean+modWV_rec4$mean+modWV_rec5$mean)/5
lines(data_test$Day,modWV_rec, type = "l", col = "blue")


#Прогнозирование фрактальным методом

library(fractaldim)

prediction_frac <- function(test_data, train_data, test_time, train_time, values, plot_indicator) 
{
  endingIndex <- length(train_data)
  total_error <- 0
  error_per_prediction <- c()
  method <- "rodogram"
  random_sample_count <- values
  
  Sm1 <- as.data.frame(test_data, row.names = NULL) #test
  Sm2 <- as.data.frame(train_data, row.names = NULL)  #train
  
  for(i in 1:values){
    delta <- c()
    for(j in 2:length(train_data)){
      delta <- rbind(delta, (train_data[j]-train_data[j-1]))
    }
    # calculate standard deviation of delta
    Std_delta <- apply(delta, 2, sd)
    
    #update fractal dimension used as reference
    V_Reference <- fd.estimate(train_data, method=method, trim=TRUE)$fd
    
    # create N guesses drawing from the normal distribution
    # use the last value of Sm as mean and the standard deviation
    # of delta as the deviation
    Sm_guesses <- rnorm(random_sample_count , mean=train_data[length(train_data)], sd =Std_delta)
    
    minDifference = 1000000
    
    # check the fractal dimension of Sm plus each different guess and
    # choose the value with the least difference with the reference
    for(j in 1:length(Sm_guesses)){
      new_Sm <- rbind(Sm2, Sm_guesses[j])
      new_V_Reference <- fd.estimate(new_Sm$train_data, method=method, trim=TRUE)$fd
      
      if (abs(new_V_Reference - V_Reference) < minDifference ){      
        Sm_prediction <- Sm_guesses[j]
        minDifference = abs(new_V_Reference - V_Reference)
      }
    }
    Sm2 <- rbind(Sm2, Sm_prediction)
  }
  s<-Sm2[,1]
  ss<-s[(endingIndex+1):(endingIndex+values)]
  if (plot_indicator) {
    add_data2<-matrix(NA, nrow = length(train_data), ncol = 1, byrow = TRUE)
    plot(c(train_time,test_time), c(train_data,test_data), type='l',xlab="Время", ylab="Значение параметра",ylim=range(0:max(c(train_data,test_data))))
    lines(c(train_time,test_time), c(add_data2,test_data), col= 'green') # рисуем тестовые данные
    lines(c(train_time,test_time), c(add_data2, ss), col= 'red') # рисуем предсказанные данные
  }
  return(ss)
}


pred_frac_values<-prediction_frac(data_test$RUS_value2, data_train$RUS_Value2, (length(data_train$Day)+1):(length(data_train$Day)+length(data_test$Day)), 1:length(data_train$Day), 152, TRUE)











