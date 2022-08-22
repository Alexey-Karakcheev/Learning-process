#--------------Занятие 6--------------------
#-----------Кластеризация-------------------
data<-read.csv("C:/Users/karak/Documents/Dataset2_2017_2.csv",header = T, sep = ";", encoding = "UTF-8")
dataMeasures <- c("X1","X2","X3","X4","X5","X6","X7","X8","X9","X10","X11","X12","X13","X14","X15","X16","X17","X18","X19","X20")
trainData2017<-data[dataMeasures]

library(ggpubr)
library(factoextra)

res.km2017 <- kmeans(scale(trainData2017),4,nstart = 25)

res.km2017$cluster

fviz_cluster(res.km2017, data = trainData2017,
             palette = c("red","blue", "green", "purple"),
             geom = "point",
             ellipse.type = "convex"
             )

library(sp)
library(maptools)
library(rgdal)
library(pals)

data[, "cls"] <- res.km2017$cluster
data <- data[order(data$N),]

nc1<-readShapePoly("C:/Users/karak/Documents/gadm36_RUS_shp/gadm36_RUS_1.shp", proj4string=CRS("+proj=longlat +datum=NAD27"))
proj4.str <- sp::CRS("+init=epsg:3413 +lon_0=105")

nc1_2<-spTransform(nc1, proj4.str)

my.Palette <-heat.colors(20, alpha=1)
nc1_2@data$K2017<-data$cls[1:83]

spplot(nc1_2, "K2017", col.regions=my.Palette, main = "Российская Федерация k-means 2017",
        lwd=.4, col="white")
#EM алгоритм
library(EMCluster, quiet = T)

#plot(trainData2017$X15, data$X10, type = "p")

ret2017.em <-init.EM(trainData2017,nclass = 4, method = "em.EM")
ret2017.Rnd <- init.EM(trainData2017,nclass=4, method = "Rnd.EM", EMC = .EMC.Rnd)

emobj2017 <- simple.init(trainData2017, nclass =4)
ret2017.init <- emcluster(trainData2017, emobj2017, assign.class=T)

ret2017.em$class
ret2017.Rnd$class
ret2017.init$class
#Вывод результатов при различных значенях
plotem(ret2017.em, trainData2017, xlab="P_f", ylab = "P_nf")
plotem(ret2017.Rnd, trainData2017, xlab="P_f", ylab = "P_nf")
plotem(ret2017.init, trainData2017, xlab="P_f", ylab = "P_nf")

#рисование результатов кластеризации
 data[, "cls"] <- ret2017.em$class
 data <- data[order(data$N),]

  nc1<-readShapePoly("C:/Users/karak/Documents/gadm36_RUS_shp/gadm36_RUS_1.shp", proj4string=CRS("+proj=longlat +datum=NAD27"))
 proj4.str <- sp::CRS("+init=epsg:3413 +lon_0=105")
nc1_2 <- spTransform(nc1, proj4.str)

my.Palette <-heat.colors(20, alpha=1)
nc1_2@data$K2017<-data$cls[1:83]

spplot(nc1_2, "K2017", col.regions=my.Palette, main = "Российская Федерация k-means 2017",
       lwd=.4, col="white")

#Сеть Кахонена
library(kohonen)
example.sc = scale(trainData2017)

#Сетка 10х10
example.grid = somgrid(xdim = 10, ydim = 10, topo = "hexagonal")
example.som = som(example.sc, grid = example.grid, rlen = 10000, alpha = c(0.05,0.01))

summary(example.som)

res_2017.xyf <- 
  predict(example.som, newdata = scale(trainData2017),
          unit.predictions = getCodes(example.som, 1),
          whatmap = 1)$prediction

groups = 4

example.hc2017 = cutree(hclust(dist(as.matrix(res_2017.xyf[[1]]))), groups)

plot(example.som, type="codes", bgcol=rainbow(groups)[example.hc2017])
add.cluster.boundaries(example.som, example.hc2017)

data[,"cls"]<-as.numeric(example.hc2017)
data <- data[order(data$N),]

nc1<-readShapePoly("C:/Users/karak/Documents/gadm36_RUS_shp/gadm36_RUS_1.shp", proj4string=CRS("+proj=longlat +datum=NAD27"))
proj4.str <- sp::CRS("+init=epsg:3413 +lon_0=105")

nc1_2 <- spTransform(nc1, proj4.str)

my.Palette <- heat.colors(20, alpha = 1)
nc1_2@data$K2017 <- data$cls[1:83]
spplot(nc1_2, "K2017", col.regions = my.Palette, main = "РФ сеть Кохонена 2017", lwd = 4, col="white")

#Ассоциативные правила
#Можно описать как "Кто купил х, также купил у"
#Support -в общем виде это показатель "Частности"
#Confidence - как часто наше правило срабатывает для всего датасета
#lift - это отношение зависимости items к их независимости 


library(arules)
library(arulesViz)

aRulesData <- discretizeDF(trainData2017)
aRulesData <- aRulesData[dataMeasures]


rule <- apriori(aRulesData, parameter = list(support=0.2, confidence=0.5))
inspect(head(sort(rule, by="lift"), 5))
plot(rule)
plot(rule, method = "grouped")

#logit

myLogit <- glm(X44 - X45, trainData, family= binomial(link="logit"))
summary(myLogit)
















