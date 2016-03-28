stocks <- read.csv("StocksCluster.csv")

prop.table(table(stocks$PositiveDec))

cor(stocks)

colMeans(stocks)

library(caTools)
set.seed(144)
spl <- sample.split(stocks$PositiveDec, SplitRatio = 0.7)
stocksTrain <- subset(stocks, spl == TRUE)
stocksTest <- subset(stocks, spl == FALSE)

StocksModel <- glm(PositiveDec ~ ., data = stocksTrain, family = "binomial")
prediction <- predict(StocksModel, type = "response")
prop.table(table(stocksTrain$PositiveDec, prediction >= 0.5))
0.12213175 + 0.44905009

prediction <- predict(StocksModel, newdata = stocksTest, type = "response")
prop.table(table(stocksTest$PositiveDec, prediction >= 0.5))
0.1200345 + 0.4470351

prop.table(table(stocks$PositiveDec))

limitedTrain = stocksTrain
limitedTrain$PositiveDec = NULL
limitedTest = stocksTest
limitedTest$PositiveDec = NULL

library(caret)
preproc <- preProcess(limitedTrain)
normTrain <- predict(preproc, limitedTrain)
normTest <- predict(preproc, limitedTest)
mean(normTrain$ReturnJan)
mean(normTest$ReturnJan)

set.seed(144)
km <- kmeans(normTrain, centers = 3)
splTrain <- split(stocksTrain, km$cluster)
lapply(splTrain, nrow)

library(flexclust)
km.kcca <- as.kcca(km, normTrain)
clusterTrain <- predict(km.kcca)
clusterTest <- predict(km.kcca, newdata=normTest)
splTest <- split(stocksTest, clusterTest)
lapply(splTest, nrow)

stocksTrain1 <- splTrain[[1]]
stocksTrain2 <- splTrain[[2]]
stocksTrain3 <- splTrain[[3]]
stocksTest1 <- splTest[[1]]
stocksTest2 <- splTest[[2]]
stocksTest3 <- splTest[[3]]
mean(stocksTrain1$PositiveDec)
mean(stocksTrain2$PositiveDec)
mean(stocksTrain3$PositiveDec)

StocksModel1 <- glm(PositiveDec ~ ., data = stocksTrain1, family = "binomial")
StocksModel2 <- glm(PositiveDec ~ ., data = stocksTrain2, family = "binomial")
StocksModel3 <- glm(PositiveDec ~ ., data = stocksTrain3, family = "binomial")
summary(StocksModel1)
summary(StocksModel2)
summary(StocksModel3)

PredictTest1 <- predict(StocksModel1, newdata = stocksTest1, type = "response")
PredictTest2 <- predict(StocksModel2, newdata = stocksTest2, type = "response")
PredictTest3 <- predict(StocksModel3, newdata = stocksTest3, type = "response")
prop.table(table(stocksTest1$PositiveDec, PredictTest1 >= 0.5))
0.02311248 + 0.59630200
prop.table(table(stocksTest2$PositiveDec, PredictTest2 >= 0.5))
0.1865385 + 0.3639423
prop.table(table(stocksTest3$PositiveDec, PredictTest3 >= 0.5))
0.5104167 + 0.1354167

AllPredictions = c(PredictTest1, PredictTest2, PredictTest3)
AllOutcomes = c(stocksTest1$PositiveDec, stocksTest2$PositiveDec, stocksTest3$PositiveDec)
prop.table(table(AllOutcomes, AllPredictions >= 0.5))
0.1344272 + 0.4444444
