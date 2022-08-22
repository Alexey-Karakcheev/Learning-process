library(ggplot2)
library(dplyr)
library(gridExtra)
#library(RColorBrewer)
#library(randomForest)
#library(party)
#Читаем файл
data<-read.csv("C:/Users/karak/Documents/winequality-white.csv", header = T, sep=",", encoding = "UTF-8")
#Избавляемся от NA-значений
data<-na.omit(data)
x <- table(data$quality)

#График количества вина разного качества
barplot(height = x, col = "steelblue", xlab = "Quality", ylab = "Frq", ylim = c(0,2500))

#---------------------------------Графики корреляции переменных-----------------
# Посмотрим на корреляции переменных

library(corrplot)
cor(data, use = "all.obs")
corr_matrix<-cor(data, use = "all.obs")
corrplot(corr_matrix, method = "color")
corrplot(corr_matrix, method = "number")

# Видим следующее:
# density сильно коррелирует с residual.sugar и alcohol
# free.sulfur.dioxide коррелирует с total.sulfur.dioxide



#----------------------------------Распределение переменных --------------------
p1 <- ggplot(data, aes(as.factor(quality),fixed.acidity))+ geom_boxplot() + coord_cartesian(ylim = c(4.5,10.5))
p2 <- ggplot(data, aes(as.factor(quality),volatile.acidity))+ geom_boxplot()+ coord_cartesian(ylim = c(0,0.6))
p3 <- ggplot(data, aes(as.factor(quality),citric.acid))+ geom_boxplot()+ coord_cartesian(ylim = c(0,0.5))
p4 <- ggplot(data, aes(as.factor(quality),residual.sugar))+ geom_boxplot()+ coord_cartesian(ylim = c(0,20))
p5 <- ggplot(data, aes(as.factor(quality),chlorides))+ geom_boxplot()+ coord_cartesian(ylim = c(0,0.07))
p6 <- ggplot(data, aes(as.factor(quality),free.sulfur.dioxide))+ geom_boxplot()+ coord_cartesian(ylim = c(0,70))
p7 <- ggplot(data, aes(as.factor(quality),total.sulfur.dioxide))+ geom_boxplot()+ coord_cartesian(ylim = c(0,220))
p8 <- ggplot(data, aes(as.factor(quality),density))+ geom_boxplot()+ coord_cartesian(ylim = c(0.98,1.0))
p9 <- ggplot(data, aes(as.factor(quality),pH))+ geom_boxplot()+ coord_cartesian(ylim = c(2.8,3.6))
p10 <- ggplot(data, aes(as.factor(quality),sulphates))+ geom_boxplot()+ coord_cartesian(ylim = c(0.3,0.8))
p11 <- ggplot(data, aes(as.factor(quality),alcohol))+ geom_boxplot()+ coord_cartesian(ylim = c(8,13))
grid.arrange(p1,p2, nrow=1)
grid.arrange(p3,p4,p5, nrow=1)
grid.arrange(p6,p7, nrow=1)
grid.arrange(p8,p9, nrow=1)
grid.arrange(p10,p11, nrow=1)




#-------------------------------Разбиваем данные на две выборки-----------------
library(caTools)
set.seed(144)
spl = sample.split(data$quality, 0.7)
train = subset(data, spl == TRUE)
test=subset(data,spl==FALSE)

#------------------------------Строим линейную модель---------------------------

fit <- lm(quality ~ ., data = train)
summary(fit)

# Т.к. p-value переменных citric acid, chlorides и total.sulfur.dioxide > 0.05, 
# уберём из их модели и посмотрим, улучшится ли модель


fit2 <- lm(quality ~ .- citric.acid - chlorides - total.sulfur.dioxide, data = train)
summary(fit2)

#Значение Adjusted R-squared выросло на 0,0006 -> модель улучшилась

#----------------------------Спрогнозируем качество на тестовой выборке---------
predictTest <- predict(fit2, newdata = test)
summary(predictTest)

#Мы столкнулись с ограничениями линейной регрессии - во-первых, мы не можем спрогнозировать целые числа, 
#во-вторых - максимальное предсказанное значение только 7,2, то есть она не предсказывает высокое качество.
#Поэтому мы изменим подход к предсказанию. Вместо предсказания значения quality, мы будем предсказывать классификацию
#новой переменной - рейтинг, которая будет принимать два значения: 1, если quality >7 и 0 в остальных случаях.



data$rating   <- ifelse (as.integer(data$quality) > 7, 1, 0)
#---------------------------Разделение на две выборки---------------------------
set.seed(144)
spl = sample.split(data$rating, 0.7)
train1 = subset(data, spl == TRUE)
test1=subset(data,spl==FALSE)

#-----------------------Построение модели---------------------------------------
mod = glm(formula = rating ~ .- quality, data=train1, family = "binomial")
summary(mod)
#-----------------------Делаем предсказание-------------------------------------
prediction = predict(mod, newdata=test1, type="response")
table(test1$rating, prediction > 0.5)
#-------------------------------------------------------------------------------
print("The maximum value of prediction over testing set is ")
round(max(prediction),3)
#Видим, что <0.5, а значит, что мы снова не можем предсказывать высокое качество.
#Попробуем использовать дргуие методы классификации.






#Посмотреть кнн с Рпабса + найти SVM и их захуярить


# knn_new<- train(factor(rating) ~ ., train1, method = "knn")
# res_new <- predict(knn_new ,test, type="prob")
# summary(knn_new)
# summary(res_new)

