data<-read.csv("bike.csv", header = T, sep=",", encoding = "UTF-8")

clss<-NA
clss[which(data$count>100)]<-1
clss[which(data$count<101)]<-0

data<-cbind(data, clss)

library(caret)

mod_svm<-train(factor(clss)~holiday+humidity+summer+temp+windspeed+workingday, data=data, method="knn")

clss2<-predict(mod_svm, data)

precision<-precision(data = factor(clss2), reference = factor(clss))
recall<-recall(data = factor(clss2), reference = factor(clss))
F_means<-F_meas(data = factor(clss2), reference = factor(clss))

library("MLmetrics")
accuracy<-Accuracy(clss2, clss)

library(pROC)
SVMl.roc<-roc(clss2,clss)
plot(SVMl.roc)

#Задача с мошенничесвтом в интернет магазинах
library(caret)
library(caretEnsemble)
library(MLmetrics)
library(pROC)



# Описание данных
#trustLevel - Индивидуальный уровень доверия клиента. 6: Высочайшая надежность
#totalScanTimeInSeconds - Общее время в секундах между первым и последним отсканированным продуктом
#grandTotal - Общая стоимость отсканированных продуктов	
#lineItemVoids - Количество аннулированных сканирований	
#scansWithoutRegistration - Количество попыток сканирования без какого-либо сканирования (неудачное сканирование)	
#quantityModification - Число изменений количества товаров для одного из сканируемых продуктов	
#scannedLineItemsPerSecond - Среднее количество отсканированных продуктов в секунду	
#valuePerSecond - Средняя общая стоимость отсканированных продуктов в секунду	
#lineItemVoidsPerPosition - Отношение числа аннулированных сканирований к общему числу аннулированных и не аннулированных сканирований	
#fraud - Классификатор как мошенничество (1) или не мошенничество (0)


#Читаем файл данных 
trainData<- read.csv("fraud_train.csv", header =T, sep = "|")
testData<- read.csv("fraud_test.csv", header =T, sep = "|") 
realData <- read.csv("fraud_realclass.csv", header =T, sep = "|") 

# Введенные вычислимые показатели

#ne_otm	Количество неотменённых заказов	= totalScanTimeInSeconds * scannedLineItemsPerSecond
#otm_i_ne_otm	Отношение количества аннулированных сканирований к не аннулированным	= lineItemVoids + ne_otm
#sec_na_1_udach_scan	Отношение общего времени к количеству удачных сканирований	= totalScanTimeInSeconds / otm_i_ne_otm
#udach_i_neudach_scan	Количество удачных и неудачных сканирований	= otm_i_ne_otm + scansWithoutRegistration
#dolya_neudach_scan	Отношение количества удачных сканирований к неудачным	= scansWithoutRegistration / udach_i_neudach_scan
#sec_na_1_scan	Отношение общего времени в магазине к общему количеству сканирований	= totalScanTimeInSeconds / udach_i_neudach_scan


#trustLevel
trainData[,"ne_otm"]<-trainData$totalScanTimeInSeconds * trainData$scannedLineItemsPerSecond
trainData[,"otm_i_ne_otm"]<-trainData$lineItemVoids + trainData$ne_otm
trainData[,"sec_na_1_udach_scan"]<-trainData$totalScanTimeInSeconds / trainData$otm_i_ne_otm
trainData[,"udach_i_neudach_scan"]<-trainData$otm_i_ne_otm + trainData$scansWithoutRegistration
trainData[,"dolya_neudach_scan"]<-trainData$scansWithoutRegistration / trainData$udach_i_neudach_scan
trainData[,"sec_na_1_scan"]<-trainData$totalScanTimeInSeconds / trainData$udach_i_neudach_scan

testData[,"ne_otm"]<-testData$totalScanTimeInSeconds * testData$scannedLineItemsPerSecond
testData[,"otm_i_ne_otm"]<-testData$lineItemVoids + testData$ne_otm
testData[,"sec_na_1_udach_scan"]<-testData$totalScanTimeInSeconds / testData$otm_i_ne_otm
testData[,"udach_i_neudach_scan"]<-testData$otm_i_ne_otm + testData$scansWithoutRegistration
testData[,"dolya_neudach_scan"]<-testData$scansWithoutRegistration / testData$udach_i_neudach_scan
testData[,"sec_na_1_scan"]<-testData$totalScanTimeInSeconds / testData$udach_i_neudach_scan

trainData2<-trainData[,-10]

# убираем колонки у которых дисперсия около нуля 
nImportant<-nearZeroVar(trainData2)
names(trainData2)[nImportant]
trainData2<-trainData2[,-nImportant]

# убираем колонки с корреляцией >0.75 
highCor = findCorrelation(cor(trainData2), cutoff = 0.75)
names(trainData2)[highCor]
trainData2<-trainData2[,-highCor]

# убираем колонки с линейной комбинацией 
linCombo <- findLinearCombos(trainData2)
names(trainData2)[linCombo$remove]
trainData2<-trainData2[,-linCombo$remove]

#возвращаем столбец с ответами
trainData2[,"fraud"]<-trainData$fraud

#метод kNN old
knn_old <- train(factor(fraud) ~ ., trainData, method = "knn")
res_old <- predict(knn_old ,testData)

real<-factor(c(realData[,1]))
knn_precision_old<-precision(data=factor(res_old), reference = real)
knn_recall_old<-recall(data=factor(res_old), reference = real)
knn_F_means_old<-F_meas(data=factor(res_old), reference = real)

res3<-as.numeric(c(as.integer(res_old)-1))
res4<-as.numeric(c(realData$fraud))
knn_logLoss_old<-LogLoss(res3, res4)
knn_accuracy_old<-Accuracy(res3, res4)
knn.roc <- roc(res3, res4)
plot(knn.roc, col="black")

#метод kNN new
knn_new<- train(factor(fraud) ~ ., trainData2, method = "knn")
res_new <- predict(knn_new ,testData)

real<-factor(c(realData[,1]))
knn_precision_new<-precision(data=factor(res_new), reference = real)
knn_recall_new<-recall(data=factor(res_new), reference = real)
knn_F_means_new<-F_meas(data=factor(res_new), reference = real)

res3<-as.numeric(c(as.integer(res_new)-1))
res4<-as.numeric(c(realData$fraud))
knn_logLoss_new<-LogLoss(res3, res4)
knn_accuracy_new<-Accuracy(res3, res4)
knn.roc <- roc(res3, res4)
lines(knn.roc, col="darkgray")


#метод SVM Poly old
svmPoly_old <- train(factor(fraud) ~ ., trainData, method = "svmPoly")
res_old <- predict(svmPoly_old ,testData)

real<-factor(c(realData[,1]))
SVM_precision_old<-precision(data=factor(res_old), reference = real)
SVM_recall_old<-recall(data=factor(res_old), reference = real)
SVM_F_means_old<-F_meas(data=factor(res_old), reference = real)

res3<-as.numeric(c(as.integer(res_old)-1))
res4<-as.numeric(c(realData$fraud))
SVM_logLoss_old<-LogLoss(res3, res4)
SVM_accuracy_old<-Accuracy(res3, res4)

SVM.roc <- roc(res3, res4)
lines(SVM.roc, col="red")


#метод SVM Poly new
svmPoly_new <- train(factor(fraud) ~ ., trainData2, method = "svmPoly")
res_new <- predict(svmPoly_new, testData)

real<-factor(c(realData[,1]))
SVM_precision_new<-precision(data=factor(res_new), reference = real)
SVM_recall_new<-recall(data=factor(res_new), reference = real)
SVM_F_means_new<-F_meas(data=factor(res_new), reference = real)

res3<-as.numeric(c(as.integer(res_new)-1))
res4<-as.numeric(c(realData$fraud))
SVM_logLoss_new<-LogLoss(res3, res4)
SVM_accuracy_new<-Accuracy(res3, res4)

SVM.roc <- roc(res3, res4)
lines(SVM.roc, col="darkred")


#метод SVM Linear old
svmLine_old <- train(factor(fraud) ~ ., trainData, method = "svmLinear2")
res_old <- predict(svmLine_old, testData, type="raw")

real<-factor(c(realData[,1]))
SVMl_precision_old<-precision(data=res_old, reference = real)
SVMl_recall_old<-recall(data=res_old, reference = real)

SVMl_F_means_old<-F_meas(data=res_old, reference = real)

res3<-as.numeric(c(as.integer(res_old)-1))
res4<-as.numeric(c(realData$fraud))
SVMl_logLoss_old<-LogLoss(res3, res4)
SVMl_accuracy_old<-Accuracy(res3, res4)

SVMl.roc <- roc(res3, res4)
lines(SVMl.roc, col="green")

#метод SVM Linear new
svmLine_new <- train(factor(fraud) ~ ., trainData2, method = "svmLinear2")
res_new <- predict(svmLine_new, testData)

real<-factor(c(realData[,1]))
SVMl_precision_new<-precision(data=res_new, reference = real)
SVMl_recall_new<-recall(data=res_new, reference = real)

SVMl_F_means_new<-F_meas(data=res_new, reference = real)

res3<-as.numeric(c(as.integer(res_new)-1))
res4<-as.numeric(c(realData$fraud))
SVMl_logLoss_new<-LogLoss(res3, res4)
SVMl_accuracy_new<-Accuracy(res3, res4)

SVMl.roc <- roc(res3, res4)
lines(SVMl.roc, col="darkgreen")

#Комбинации методов
#old
adabag<-train(factor(fraud) ~ ., trainData, method = "AdaBag")
res<- predict(adabag ,testData, type="prob")

roc.plot(realData$fraud, res$'1')
#new
adabag<-train(factor(fraud) ~ ., trainData2, method = "AdaBag")
res<- predict(adabag ,testData, type="raw")

roc.plot(realData$fraud, res$'1')

#композиция

control <- trainControl(method = "repeatedcv", number=10, repeats=3, savePredictions = TRUE, classProbs=TRUE )
algorithmList <- c('svmLinear2','knn')
trainData$fraud<-as.factor(trainData$fraud)
stack_models <- caretList(make.names(fraud) ~ ., data=trainData, methodList = algorithmList, trControl = control)
stackControl<-trainControl(sampling="rose", method="repeatedcv", number=10,repeats=3, savePredictions = T, classProbs = T)
stack.glm <- caretStack(stack_models, method="glm", trControl=stackControl)
print(stack.glm)


res <- predict(stack.glm, newdata=testData, type="prob")
res2<- as.numeric(res)-1

roc.plot(realData$fraud, res2)
roc <- roc(realData$fraud, res2)
plot(roc, col="black")

#------------------Занятие 7--------------------------
#Деревья решений 

library(verification)

C_old <- train(factor(fraud) ~ ., trainData, method = "C5.0")
res_old_C<- predict(C_old, testData, type = "prob")

roc.plot(realData$fraud, res_old_C$`1`)

C_new <- train(factor(fraud) ~ ., trainData2, method = "C5.0")
res_new_C<- predict(C_old, testData, type = "prob")

roc.plot(realData$fraud, res_old_C$`1`)

library(rpart)
library(rpart.plot)

tree_test<-rpart(fraud~ trustLevel+totalScanTimeInSeconds+grandTotal+lineItemVoids+scansWithoutRegistration+ quantityModifications+scannedLineItemsPerSecond+ valuePerSecond+lineItemVoidsPerPosition+ fraud+ne_otm+ otm_i_ne_otm+sec_na_1_udach_scan+ udach_i_neudach_scan+ dolya_neudach_scan+ sec_na_1_scan , data = trainData, minbucket = 1, minsplit=1)
rpart.plot(tree_test)

tree_test<-rpart(fraud~ trustLevel+totalScanTimeInSeconds+grandTotal+lineItemVoids+scansWithoutRegistration+ quantityModifications+scannedLineItemsPerSecond+ valuePerSecond+lineItemVoidsPerPosition+ fraud+ne_otm+ otm_i_ne_otm+sec_na_1_udach_scan+ udach_i_neudach_scan+ dolya_neudach_scan+ sec_na_1_scan , data = trainData2, minbucket = 1, minsplit=1)
rpart.plot(tree_test)

#Naive Bayes
nb<-train(factor(fraud)~., trainData, method = "nb")
res_nb <- predict(nb, testData[1:500,],type = "prob")

roc.plot(realData$fraud[1:500], res_nb$`1`)

nb<-train(factor(fraud)~., trainData2, method = "nb")
res_nb <- predict(nb, testData[1:500,],type = "prob")

roc.plot(realData$fraud[1:500], res_nb$`1`)

#Кросс валидация

train_ctrl = trainControl(
  method = "cv",
  number = 3
  )

nb<- train(factor(fraud)~., trainData, method = "nb", trControl = train_ctrl)
res_nb <- predict(nb, testData[1:500,], type = "prob")
roc.plot(realData$fraud[1:500], res_nb$`1`)

#Нейронная сеть варивнт 1
library(neuralnet)
library(MLmetrics)
library(verification)



#Нейронная сеть вариант 1

library(neuralnet)
library(MLmetrics)
library(verification)

net<-neuralnet(fraud~ ., 
               #trustLevel+totalScanTimeInSeconds+grandTotal+lineItemVoids+scansWithoutRegistration+ quantityModifications+scannedLineItemsPerSecond+ valuePerSecond+lineItemVoidsPerPosition+ fraud+ne_otm+ otm_i_ne_otm+sec_na_1_udach_scan+ udach_i_neudach_scan+ dolya_neudach_scan+ sec_na_1_scan ,
               data=trainData, hidden=c(8,4), linear.output=TRUE,stepmax=1e7)


plot(net)
res<-predict(net,testData)

roc.plot(realData$fraud, res)

net<-neuralnet(fraud~ ., 
               #trustLevel+totalScanTimeInSeconds+grandTotal+lineItemVoids+scansWithoutRegistration+ quantityModifications+scannedLineItemsPerSecond+ valuePerSecond+lineItemVoidsPerPosition+ fraud+ne_otm+ otm_i_ne_otm+sec_na_1_udach_scan+ udach_i_neudach_scan+ dolya_neudach_scan+ sec_na_1_scan ,
               data=trainData2, hidden=c(8,4), linear.output=TRUE,stepmax=1e7)


plot(net)
res<-predict(net,testData2)

roc.plot(realData$fraud, res)



#res_basic<-as.numeric(c(test$y1))-1
#roc.plot(res_basic,res)
#
#result <- round(compute(net,test)$net.result)
#result1 <- result[,2]
#Accuracy(result1, res_basic)



library(keras)
library(tensorflow)
library(reticulate)
model0 <- keras_model_sequential()
model0 %>%
  
  # First hidden layer
  layer_embedding(
    input_dim = 9,
    output_dim = 9) %>%
  
  layer_dropout(rate = 0.1) %>%
  
  # Second hidden layer
  layer_simple_rnn(
    units = 128,
    return_sequences = FALSE) %>%
  
  layer_repeat_vector(1) %>%
  
  # Output layer
  layer_dense(
    units = 1,
    kernel_initializer = "uniform",
    activation = "sigmoid")

summary(model0)

model0 %>% compile(
  loss="binary_crossentropy",
  optimizer = "adam",
  metrics = "accuracy"
)

td<-trainData[,-10]

history0 <- model0 %>% fit(as.matrix(td[1:100,]),
                           as.matrix(trainData$fraud[1:100]),
                           epochs = 100,
                           batch_size = dim(trainData)[1],# 128,
                           validation_split = 0.3,
                           verbose = TRUE)

